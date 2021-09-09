module Frame
  use Frame_t
  use mt19937ar
  use nvtx
#ifdef _OPENACC
  use curand
#endif
  implicit none

  real(8), allocatable, dimension(:) :: randBuf
  integer, parameter :: n_randBuf = 32*1024*1024  ! must be even number
  integer :: i_randBuf  ! (0 <= i_randBuf < n_randBuf)
  integer :: h_randBuf  ! (0 or 1)
#ifdef _OPENACC
  !$acc declare create(randBuf)
  !$acc declare create(i_randBuf)
  type(curandGenerator) :: curand_g
  integer :: curand_stat
#endif

contains

  subroutine initRandBuf(seed)
    implicit none
    integer :: seed
    !
    integer :: i
    call nvtxStartRange("initRandBuf", __LINE__)
    allocate(randBuf(0:n_randBuf-1))
#ifdef _OPENACC
    curand_stat = curandCreateGenerator(curand_g, CURAND_RNG_PSEUDO_MTGP32)
    curand_stat = curandSetPseudoRandomGeneratorSeed(curand_g, seed)
    !$acc data copyout(randBuf)
    !$acc host_data use_device(randBuf)
    curand_stat = curandGenerateUniformDouble(curand_g, randBuf, n_randBuf)
    !$acc end host_data
    !$acc end data

!    open(UNIT=999,file="rand-acc.txt")
!    write(999,*) randBuf
!    close(999)
#else
    do i = 0, n_randBuf-1
      randBuf(i) = genrand_real2()  ! mt
    end do

!    open(UNIT=999,file="rand-cpu.txt")
!    write(999,*) randBuf
!    close(999)
#endif
    h_randBuf = 0
    i_randBuf = 0
    !$acc update device(i_randBuf)
    call nvtxEndRange
  end subroutine initRandBuf

  real function getRand(id) result (rnd)
    !$acc routine
    implicit none
    integer :: id  ! (id >= 1)
    !
    integer :: i
    i = mod(i_randBuf + id - 1, n_randBuf)
    rnd = randBuf(i)
  end function getRand

  subroutine updateRandBuf(inc)
    implicit none
    integer :: inc  ! (0 <= inc < n_randBuf/2)
    !
    integer :: i, is, ie
    i_randBuf = mod(i_randBuf + inc, n_randBuf)
    !$acc update device(i_randBuf) async(0)
    if (h_randBuf == 2*i_randBuf/n_randBuf) return

    call nvtxStartRange("updateRandBuf", __LINE__)
    is = h_randBuf * n_randBuf/2
    ie = is + n_randBuf/2 - 1
    ! write(*,*) "# updateRandBuf:", is, ie
#ifdef _OPENACC
    !$acc host_data use_device(randBuf)
    curand_stat = curandGenerateUniformDouble(curand_g, randBuf(is), n_randBuf/2)
    !$acc end host_data
#else
    do i = is, ie
      randBuf(i) = genrand_real2()  ! mt
    end do
#endif
    h_randBuf = 1 - h_randBuf
    call nvtxEndRange
  end subroutine updateRandBuf

  subroutine finishRandBuf()
    i_randBuf = 0
    h_randBuf = 0
    deallocate(randBuf)
#ifdef _OPENACC
    curand_stat = curandDestroyGenerator(curand_g)
#endif
  end subroutine finishRandBuf
  
  type(tmcomp) function timecomp (t)
    integer :: t
    timecomp%step    = modulo(t, steps_per_day)
    timecomp%day     = t / steps_per_day
    timecomp%weekday = modulo(timecomp%day, 7)  ! Sun:0 & Sat:6
    timecomp%hour    = 24.0*dt*real(timecomp%step, kind=8)
  end function

  ! スケジューラ手続きの型:
  ! 【実装上の注意】通常の手続きの形をしているが、実質は、フィールド 
  !  person%mkSched の型を記述するためのものである。
  ! 入力: 
  !   person_: 対象の人の人間
  !   time   : 現在時刻
  ! 出力: 
  !   sched_: スケジュール
!  subroutine scheduler_t(person_, time, sched_)
!    type(person) :: person_ ! type(person)ではNG
!    integer       :: time
!    type(sched)   :: sched_
!  end subroutine

  ! person%belongからカテゴリに一致するものを探す
  type(place_t) function findBelong(xs,kind,doesMatch) result (it)
    type(place_t) :: xs(:)
    integer :: i, kind
    logical, optional :: doesMatch
    do i = 1, size(xs) 
      if (xs(i)%place_k .eq. kind) then
        it = xs(i)
        if (present(doesMatch)) doesMatch = .true.
        return
      end if
    end do
    if (present(doesMatch)) then
      doesMatch = .false.
    else
      !write(*,*) 'findBelong: cannot find place_k:', kind
      !write(*,*) 'findBelong: enforced to the first belonging place'
      it = xs(1)
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! it is indefinit!!
      !stop
    end if
  end function


  subroutine filterBelong(xs,rule,idx,nMatch)
    type(place_t) :: xs(:)
    integer :: i, j 
    integer, intent(out) :: idx(:), nMatch
    interface
      logical function rule(x)
        use Frame_t
        type(place_t) :: x
      end function
    end interface
    nMatch = 0
    do i = 1, size(xs) 
      if (rule(xs(i))) then
        nMatch = nMatch + 1
        idx(nMatch) = i
      end if
    end do
  end subroutine 

  integer function catchTrain(now, src, dst) result (tr_gid)
    !$acc routine
    integer     :: now, src, dst
    !
    integer     :: j, k, pl_gid

    do k = 1, nOnServiceTrains
      tr_gid = onServiceTrainIds(k)
      if (.not. gl_train_onService(tr_gid)) cycle
      pl_gid = gl_train_plGid(tr_gid)
      if (gl_place_idx(pl_gid)%area_t .ne. src) cycle
      do j = gl_train_sked(tr_gid)%i + 1, gl_train_sked(tr_gid)%n
        if (gl_train_sked(tr_gid)%area_t(j) .ne. dst) cycle
        return
      end do
    end do
    tr_gid = 0
    return
  end function

  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  ! 死亡率 (とりあえずThompson et al.(2003)によるものをハードコードする)
  real(8) function deathRate(age)
    !$acc routine
    integer :: age 
    if (age .lt. 1)                   then; deathRate  =   2.2D-5
    else if (1 .ge.age.and.age.lt. 5) then; deathRate  =   1.1D-5
    else if (5 .ge.age.and.age.lt.50) then; deathRate  =   1.5D-5
    else if (50.ge.age.and.age.lt.65) then; deathRate  =  12.5D-5
    else                                  ; deathRate  = 132.5D-5
    end if
  end function


  subroutine updateHealth(pe_gid, now, pl_save_gid)
    !$acc routine
    use Record, only: recordTrip
    integer :: pe_gid
    integer :: now
    integer :: pl_save_gid(:)  ! places where the changes are recorded
    !
    real(8) :: rnd, nu, pTrns
    integer :: j, bit, pl_gid, place_k
    logical :: rec_mask
    interface
      logical function is_nan(x)
        real(8) :: x
      end function
    end interface

    pl_gid = gl_person_visit_gid(pe_gid)

    rnd = getRand(pe_gid)

    if (gl_person_hyposensitized(pe_gid)) then
      nu = gl_person_nu(pe_gid)
    else
      nu = 1.0
    end if

    select case (gl_person_health(pe_gid, 1)%kind)
    case (HL_SUS)
      pTrns = gl_place_pTrns(pl_gid, PTRNS_S2E)
      if (gl_person_nIntervEvt(pe_gid) > 0 .and. now == gl_person_intervEvt(pe_gid, 1)%time) then
        gl_person_nHealth(pe_gid) = gl_person_nHealth(pe_gid) + 1
        gl_person_health(pe_gid, gl_person_nHealth(pe_gid)) = gl_person_health(pe_gid, 1)
        select case (gl_person_intervEvt(pe_gid, 1)%kind)
        case (INTERV_VAC)
          ! S -> V
          gl_person_health(pe_gid, 1) = health(HL_VAC, now)
          !$acc atomic update
          gl_place_nVis(pl_gid, HL_SUS) = gl_place_nVis(pl_gid, HL_SUS) - 1
          !$acc end atomic
          !$acc atomic update
          gl_place_nVis(pl_gid, HL_VAC) = gl_place_nVis(pl_gid, HL_VAC) + 1
          !$acc end atomic
        case (INTERV_INF)
          ! S -> I ==> changed to S -> E
          ! Now, initial infecteds are instriduces as a latent patient.
          gl_person_health(pe_gid, 1) = health(HL_EXP, now)
          !$acc atomic update
          gl_place_nVis(pl_gid, HL_SUS) = gl_place_nVis(pl_gid, HL_SUS) - 1
          !$acc end atomic
          !$acc atomic update
          gl_place_nVis(pl_gid, HL_EXP) = gl_place_nVis(pl_gid, HL_EXP) + 1
          !$acc end atomic
        end select
        gl_person_nIntervEvt(pe_gid) = gl_person_nIntervEvt(pe_gid) - 1
        do j = 1, gl_person_nIntervEvt(pe_gid)
          gl_person_intervEvt(pe_gid, j) = gl_person_intervEvt(pe_gid, j+1)
        end do
      else if (rnd .lt. pTrns) then
        ! S -> E
        gl_person_nHealth(pe_gid) = gl_person_nHealth(pe_gid) + 1
        gl_person_health(pe_gid, gl_person_nHealth(pe_gid)) = gl_person_health(pe_gid, 1)
        gl_person_health(pe_gid, 1) = health(HL_EXP, now)
        !$acc atomic update
        gl_place_nVis(pl_gid, HL_SUS) = gl_place_nVis(pl_gid, HL_SUS) - 1
        !$acc end atomic
        !$acc atomic update
        gl_place_nVis(pl_gid, HL_EXP) = gl_place_nVis(pl_gid, HL_EXP) + 1
        !$acc end atomic
        place_k = gl_place_idx(pl_gid)%place_k
        !$acc atomic update
        frm_cntInf(place_k) = frm_cntInf(place_k) + 1
        !$acc end atomic

        rec_mask = frm_logTrip .and. any(pl_save_gid(:).eq.pl_gid)
        ! 503-05-28
        ! Here we see only 100 places (i.e. game cites). Let's check the
        ! incosistency out ...
        !do j = 1, size(pl_save_gid)
        !  write(*,*) j,":", pl_save_gid(j)
        !end do
        !write(*,*) '---------------------------------------------------------------'
        !stop 9801

        !503-04-21
        ! quick hack for debugging
        !  city_%psumAreaPeople: 
        !  0 575961 753787 893731 1210092 1255852 1305912
        !    1      2      3      4       5       6(tot)

        ! rec_mask = frm_logTrip .and. (pe_gid .gt. 1255852)

        !503-02-02
        ! set checkmark if here(pl_gid) is a home.

        ! 502-12-11:
        ! 移動ではないが、形式的に異動とみなして、新規感染者を記録
        if (rec_mask) call recordTrip(now, pe_gid, pl_gid, pl_gid)
      end if
    case (HL_EXP)
      pTrns = gl_place_pTrns(pl_gid, PTRNS_E2I)
      if (rnd .lt. pTrns) then
        ! E -> I
        gl_person_nHealth(pe_gid) = gl_person_nHealth(pe_gid) + 1
        gl_person_health(pe_gid, gl_person_nHealth(pe_gid)) = gl_person_health(pe_gid, 1)
        gl_person_health(pe_gid, 1) = health(HL_INF, now)
        !$acc atomic update
        gl_place_nVis(pl_gid, HL_EXP) = gl_place_nVis(pl_gid, HL_EXP) - 1
        !$acc end atomic
        !$acc atomic update
        gl_place_nVis(pl_gid, HL_INF) = gl_place_nVis(pl_gid, HL_INF) + 1
        !$acc end atomic
      end if
    case (HL_INF)
      pTrns = gl_place_pTrns(pl_gid, PTRNS_I2R)/nu
      if (rnd .lt. pTrns) then
        ! I -> R or D
        gl_person_nHealth(pe_gid) = gl_person_nHealth(pe_gid) + 1
        gl_person_health(pe_gid, gl_person_nHealth(pe_gid)) = gl_person_health(pe_gid, 1)
        rnd = rnd / pTrns
        if (rnd .lt. deathRate(gl_person_age(pe_gid))) then 
          ! I -> D
          gl_person_health(pe_gid, 1) = health(HL_DIE, now)
          !$acc atomic update
          gl_place_nVis(pl_gid, HL_INF) = gl_place_nVis(pl_gid, HL_INF) - 1
          !$acc end atomic
          !$acc atomic update
          gl_place_nVis(pl_gid, HL_DIE) = gl_place_nVis(pl_gid, HL_DIE) + 1
          !$acc end atomic
        else
          ! I -> R
          gl_person_health(pe_gid, 1) = health(HL_REC, now)
          !$acc atomic update
          gl_place_nVis(pl_gid, HL_INF) = gl_place_nVis(pl_gid, HL_INF) - 1
          !$acc end atomic
          !$acc atomic update
          gl_place_nVis(pl_gid, HL_REC) = gl_place_nVis(pl_gid, HL_REC) + 1
          !$acc end atomic
        end if
      end if
    case (HL_VAC)
      !rateVaccReactを時定数とする指数過程でワクチンは発効する
      if (rnd .lt. rateVaccReact*dt) then
        gl_person_hyposensitized(pe_gid) = .true.
      end if
      !ワクチンが発効するまではSのように振る舞う
      pTrns = gl_place_pTrns(pl_gid, PTRNS_S2E)*nu
      if (rnd .lt. pTrns) then
        gl_person_nHealth(pe_gid) = gl_person_nHealth(pe_gid) + 1
        gl_person_health(pe_gid, gl_person_nHealth(pe_gid)) = gl_person_health(pe_gid, 1)
        gl_person_health(pe_gid, 1) = health(HL_EXP, now)
        !$acc atomic update
        gl_place_nVis(pl_gid, HL_VAC) = gl_place_nVis(pl_gid, HL_VAC) - 1
        !$acc end atomic
        !$acc atomic update
        gl_place_nVis(pl_gid, HL_EXP) = gl_place_nVis(pl_gid, HL_EXP) + 1
        !$acc end atomic
      end if
    end select
  end subroutine updateHealth


  subroutine movePerson(city_, pe_gid)
    type(city), intent(inout) :: city_
    integer :: pe_gid
    !
    integer :: visit_gid
    integer :: dest_gid
    integer :: dest_kind
    integer :: tr_gid, n

    visit_gid = gl_person_visit_gid(pe_gid)  ! current place gid
    dest_gid  = gl_person_dest_gid(pe_gid)   ! current dest  gid
    dest_kind = gl_person_dest_kind(pe_gid)

    if (dest_kind .eq. NONE) then
      n = gl_person_sched(pe_gid)%n
      if (n >= 1 .and. city_%time >= gl_person_sched(pe_gid)%time(n)) then
        dest_gid = gl_person_sched(pe_gid)%pl_gid(n)  ! new dest gid
        gl_person_sched(pe_gid)%n = gl_person_sched(pe_gid)%n - 1

        if (gl_place_idx(visit_gid)%area_t .eq. gl_place_idx(dest_gid)%area_t) then
          ! Arrived at the destination
          gl_person_visit_gid(pe_gid) = dest_gid
          gl_person_dest_kind(pe_gid) = NONE  ! Not necesary?
        else
          ! Move to other area by train
          gl_person_dest_kind(pe_gid) = SOME
          gl_person_dest_gid(pe_gid)  = dest_gid
        end if
      end if
    else if (dest_kind .eq. SOME) then
      if (gl_place_idx(visit_gid)%place_k .eq. PL_TRAIN) then 
        if (gl_place_idx(visit_gid)%area_t .eq. gl_place_idx(dest_gid)%area_t) then
          ! The train arrived at the destination
          gl_person_visit_gid(pe_gid) = dest_gid
          gl_person_dest_kind(pe_gid) = NONE
        endif
      else
        tr_gid = catchTrain(city_%time, &
          gl_place_idx(visit_gid)%area_t, gl_place_idx(dest_gid)%area_t)
        if (tr_gid .ge. 1) then
          ! Get on the train.
          gl_person_visit_gid(pe_gid) = gl_train_plGid(tr_gid)
        end if
      endif
    else
      write(0,*) 'runtime error: bad option kind = ', dest_kind, SOME, NONE
      stop
    endif
  end subroutine movePerson


  subroutine movePersonWithPlace(pe_gid, now, pl_save_gid)
    !$acc routine
    use Record
    integer :: pe_gid  ! person global id
    integer :: now
    integer :: pl_save_gid(:)  ! places where the changes are recorded
    !
    integer :: visit_gid
    integer :: dest_gid
    integer :: dest_kind
    integer :: hl_kind
    integer :: tr_gid, i, j, k, n
    logical, external :: is_nan
    logical :: rec_mask

    visit_gid = gl_person_visit_gid(pe_gid)  ! current place gid
    dest_gid  = gl_person_dest_gid(pe_gid)   ! current dest  gid
    dest_kind = gl_person_dest_kind(pe_gid)
    hl_kind = gl_person_health(pe_gid, 1)%kind
    if (hl_kind < 1 .or. hl_kind > HL_MAX) then
      write(*,*) 'Undefined health state: ', hl_kind
    endif


    ! 詳細つり合いになるように人を動かす
    if (dest_kind == NONE) then
      n = gl_person_sched(pe_gid)%n
      if (n >= 1 .and. now >= gl_person_sched(pe_gid)%time(n)) then
        dest_gid = gl_person_sched(pe_gid)%pl_gid(n)  ! new dest gid
        gl_person_sched(pe_gid)%n = gl_person_sched(pe_gid)%n - 1

        if (gl_place_idx(visit_gid)%area_t == gl_place_idx(dest_gid)%area_t) then
          ! 街内での移動。ここで、滞在者数の調整

          rec_mask = frm_logTrip &
                .and. visit_gid .ne. dest_gid &
                .and. (any(pl_save_gid(:) .eq. visit_gid) .or. any(pl_save_gid(:) .eq. dest_gid))
          if (rec_mask) call recordTrip(now, pe_gid, visit_gid, dest_gid)
          call modifyNVis(visit_gid, hl_kind, -1)
          call modifyNVis(dest_gid,  hl_kind, +1)
          if (hl_kind == HL_INF) call recFirstInfect(dest_gid, now)
          gl_person_visit_gid(pe_gid) = dest_gid
          gl_person_dest_kind(pe_gid) = NONE
        else
          ! Move to other area by train
          gl_person_dest_kind(pe_gid) = SOME
          gl_person_dest_gid(pe_gid)  = dest_gid
        end if
      end if
    else if (dest_kind == SOME) then
      if (gl_place_idx(visit_gid)%place_k == PL_TRAIN) then 
        if (gl_place_idx(visit_gid)%area_t == gl_place_idx(dest_gid)%area_t) then

          ! 下車。ここで、滞在者数の調整
          rec_mask = frm_logTrip &
                .and. visit_gid .ne. dest_gid &
                .and. (any(pl_save_gid(:) .eq. visit_gid) .or. any(pl_save_gid(:) .eq. dest_gid))
          if (rec_mask) call recordTrip(now, pe_gid, visit_gid, dest_gid)
          call modifyNVis(visit_gid, hl_kind, -1)
          call modifyNVis(dest_gid,  hl_kind, +1)
          if (hl_kind == HL_INF) call recFirstInfect(dest_gid, now)
          gl_person_visit_gid(pe_gid) = dest_gid
          gl_person_dest_kind(pe_gid) = NONE
        endif
      else
        tr_gid = catchTrain(now, gl_place_idx(visit_gid)%area_t, gl_place_idx(dest_gid)%area_t)
        if (tr_gid >= 1) then
          ! 乗車。ここで、滞在者数の調整
          dest_gid = gl_train_plGid(tr_gid)  ! train, temporary dest

          rec_mask = frm_logTrip &
                .and. visit_gid .ne. dest_gid &
                .and. (any(pl_save_gid(:) .eq. visit_gid) .or. any(pl_save_gid(:) .eq. dest_gid))
          if (rec_mask) call recordTrip(now, pe_gid, visit_gid, dest_gid)
          call modifyNVis(visit_gid, hl_kind, -1)
          call modifyNVis(dest_gid,  hl_kind, +1)
          if (hl_kind == HL_INF) call recFirstInfect(dest_gid, now)
          gl_person_visit_gid(pe_gid) = dest_gid
        end if
      endif
    else
      write(0,*) 'runtime error: bad option kind = ', dest_kind, SOME, NONE
      stop
    endif

  contains
    subroutine recFirstInfect(pl_gid, itime)
      !$acc routine
      integer :: pl_gid  ! place gid
      integer :: itime
      !
      if (gl_place_idx(pl_gid)%tVis .eq. 0) then
        gl_place_idx(pl_gid)%tVis = itime
      end if
    end subroutine

    subroutine modifyNVis(pl_gid, hl_kind, n)
      !$acc routine
      integer :: pl_gid  ! place gid
      integer :: hl_kind
      integer :: n
      !
      gl_place_updated(pl_gid) = 1
      !!$acc atomic update
      !gl_place_nVis(pl_gid, hl_kind) = gl_place_nVis(pl_gid, hl_kind) + n
      !!$acc end atomic
    end subroutine 

  end subroutine movePersonWithPlace


  subroutine updatePlace_pTrns(pl_gid, nPeak)
    !$acc routine
    integer :: pl_gid, nPeak
    !
    real(8) :: i, nn, beta, betaN, r, sigma, epsilon
    real, parameter :: rCritTrain = 3.0D0
    real, parameter :: rBaseTrain = 3.0D0
    real, parameter :: ddTrain    = 0.63D0
    real, parameter :: norm = exp(-rBaseTrain/rCritTrain)/rBaseTrain

    betaN = gl_place_params(pl_gid, PARAMS_BETAN)
    if (gl_place_idx(pl_gid)%place_k .eq. PL_TRAIN) then
      ! modify betaN, if the concerned place is trains.
      sigma = trainDensity(pl_gid, nPeak)
      r = ddTrain/sqrt(sigma)
      betaN = betaN * exp(-r/rCritTrain)/r/ norm 
    end if

    nn = dble(gl_place_nVis(pl_gid, HL_SUS) &
            + gl_place_nVis(pl_gid, HL_EXP) &
            + gl_place_nVis(pl_gid, HL_INF) &
            + gl_place_nVis(pl_gid, HL_REC) &
            + gl_place_nVis(pl_gid, HL_VAC))
    i = dble(gl_place_nVis(pl_gid, HL_INF))
    beta = 0d0
    if (nn > 0d0) beta = betaN / nn

    gl_place_pTrns(pl_gid, PTRNS_S2E) = beta * i * dt
    gl_place_pTrns(pl_gid, PTRNS_E2I) = alpha * dt
    gl_place_pTrns(pl_gid, PTRNS_I2R) = gamma * dt

    if (gl_place_idx(pl_gid)%place_k .eq. PL_REST) then
      ! Adjust PTRNS_S2E at a restaurant
      epsilon = gl_place_params(pl_gid, PARAMS_EPSILON)

      gl_place_pTrns(pl_gid, PTRNS_S2E_PL) &
        = gl_place_pTrns(pl_gid, PTRNS_S2E_PL) * epsilon &
        + gl_place_pTrns(pl_gid, PTRNS_S2E) * (1.0 - epsilon)

      gl_place_pTrns(pl_gid, PTRNS_S2E) &
        = gl_place_pTrns(pl_gid, PTRNS_S2E) &
        + gl_place_pTrns(pl_gid, PTRNS_S2E_PL) * gl_place_alpha
    end if

  contains
    
    real(8) function trainDensity(pl_gid, nPeak)
      !$acc routine
      integer :: pl_gid, nPeak
      !
      integer :: n
      real(8), parameter :: densityPeak = 2.0D0
      n = gl_place_nVis(pl_gid, HL_SUS) &
        + gl_place_nVis(pl_gid, HL_EXP) &
        + gl_place_nVis(pl_gid, HL_INF) &
        + gl_place_nVis(pl_gid, HL_REC) &
        + gl_place_nVis(pl_gid, HL_VAC)
      trainDensity = densityPeak*dble(n)/dble(nPeak)
    end function trainDensity

  end subroutine updatePlace_pTrns

  
  subroutine evalPeople(city_)
    type(city) :: city_
    !
    integer :: i, j, pe_gid
    type(tmcomp) :: tm
    tm = timecomp(city_%time)
    if (tm%step == 0) then
      do i = 1, size(city_%area)
        do j = 1, size(city_%area(i)%person)
          call mkSched(city_, city_%area(i)%person(j), frm_rndstat)
        end do
      end do
    endif

    do pe_gid = 1, gl_n_people
      call movePerson(city_, pe_gid)
    end do

    do pe_gid = 1, gl_n_people
      call updateHealth(pe_gid, city_%time, city_%pl_save_gid)
    end do
    call updateRandBuf(gl_n_people)
  end subroutine evalPeople


  subroutine evalPeopleWithPlace(city_)
    implicit none
    type(city) :: city_
    !
    integer :: i, j, pe_gid, pl_gid, hl_kind, now, nPeak
    type(tmcomp) :: tm
    call nvtxStartRange("evalPeopleWithPlace", __LINE__)

    now = city_%time
    nPeak = city_%nPeakTrainOcc
    
    tm = timecomp(now)
    if (tm%step == 0) then
      call nvtxStartRange("mkSched", __LINE__)
      !$acc wait

      ! 2020-09-08: Q. Is this loop carried out sequentially?
      do i = 1, size(city_%area)
        do j = 1, size(city_%area(i)%person)
          call mkSched(city_, city_%area(i)%person(j), frm_rndstat)
        end do
      end do

      call nvtxEndRange
    endif

    !$acc kernels async(0)
    !$acc loop gang vector private(pe_gid, i)
    do pe_gid = 1, gl_n_people
      i = now
      call movePersonWithPlace(pe_gid, i, city_%pl_save_gid)
    end do

    !$acc loop gang vector private(pl_gid, hl_kind)
    do pl_gid = 1, gl_n_places
      if (gl_place_updated(pl_gid) == 0) cycle
      !$acc loop seq
      do hl_kind = 1, HL_MAX
        gl_place_nVis(pl_gid, hl_kind) = 0
      end do
    end do

    !$acc loop gang vector private(pe_gid, pl_gid, hl_kind)
    do pe_gid = 1, gl_n_people
      pl_gid = gl_person_visit_gid(pe_gid)
      if (gl_place_updated(pl_gid) == 0) cycle
      hl_kind = gl_person_health(pe_gid, 1)%kind
      !$acc atomic update
      gl_place_nVis(pl_gid, hl_kind) = gl_place_nVis(pl_gid, hl_kind) + 1
      !$acc end atomic
    end do
    
    !$acc loop gang vector private(pl_gid, i)
    do pl_gid = 1, gl_n_places
      if (gl_place_updated(pl_gid) == 0) cycle
      i = nPeak
      call updatePlace_pTrns(pl_gid, i)
      gl_place_updated(pl_gid) = 0
    end do

    !$acc loop gang vector private(pe_gid, i)
    do pe_gid = 1, gl_n_people
      i = now
      call updateHealth(pe_gid, i, city_%pl_save_gid)
    end do
    !$acc end kernels

    call updateRandBuf(gl_n_people)
    !$acc wait

    call nvtxEndRange
  end subroutine evalPeopleWithPlace


  subroutine evalTrain(tr_gid, now)
    !$acc routine
    integer :: tr_gid
    integer :: now
    !
    integer :: time_today
    integer :: sch_time
    integer :: pl_gid, i, n
    !
    ! The train arrives at the station at the set time and
    ! departs that station on the next cycle.
    !
    time_today = modulo(now, steps_per_day)
    pl_gid = gl_train_plGid(tr_gid)

    ! 電車が走っていない適当な時間(3:00)に車庫出しする。
    if (time_today .eq. 180) then
      gl_train_sked(tr_gid)%i = 1
      gl_place_idx(pl_gid)%area_t = 0
    end if

    i = gl_train_sked(tr_gid)%i
    n = gl_train_sked(tr_gid)%n
    if (i > n) return

    sch_time = mod(gl_train_sked(tr_gid)%time(i), steps_per_day)

    if (gl_place_idx(pl_gid)%area_t == gl_train_sked(tr_gid)%area_t(i)) then
      if (time_today > sch_time) then
        ! Train departure
        gl_place_idx(pl_gid)%area_t = 0
        i = i + 1
        if (i > n) then
          gl_train_onService(tr_gid) = .false.
        end if
        gl_train_sked(tr_gid)%i = i
      endif
    else
      if (time_today >= sch_time .and. sch_time > time_today-10) then
        ! Train arrival
        if (i == 1) then
          gl_train_onService(tr_gid) = .true.
        end if
        gl_place_idx(pl_gid)%area_t = gl_train_sked(tr_gid)%area_t(i)
      endif
    endif
  end subroutine 


  subroutine evalTrains(city_)
    type(city) :: city_
    !
    integer :: i, tr_gid, now
    integer :: n_passenger, pl_gid
    call nvtxStartRange("evalTrains", __LINE__)

    now = city_%time
    !**** debug ****
    !write(*,*) "step: ", now, mod(now, steps_per_day) / 60, mod(now, 60)
    !**** debug ****

    nOnServiceTrains = 0
    !$acc update device(nOnServiceTrains) async(0)

    !$acc kernels async(0)
    !$acc loop independent gang vector private(tr_gid, i)
    do tr_gid = 1, gl_n_trains
      i = now
      call evalTrain(tr_gid, i)

      if (gl_train_onService(tr_gid)) then
        !$acc atomic capture
        nOnServiceTrains = nOnServiceTrains + 1
        i = nOnServiceTrains
        !$acc end atomic
        onServiceTrainIds(i) = tr_gid

        !**** debug ****
        !pl_gid = gl_train_plGid(tr_gid)
        !n_passenger = gl_place_nVis(pl_gid, HL_SUS) &
        !            + gl_place_nVis(pl_gid, HL_EXP) &
        !            + gl_place_nVis(pl_gid, HL_INF) &
        !            + gl_place_nVis(pl_gid, HL_VAC) &
        !            + gl_place_nVis(pl_gid, HL_REC) &
        !            + gl_place_nVis(pl_gid, HL_DIE)
        !write(*,*) "  tr_gid: ", tr_gid, n_passenger
        !**** debug ****
      end if
    end do
    !$acc end kernels

    call nvtxEndRange
  end subroutine evalTrains


  ! 次のステップで使う(健康状態の)遷移確率を計算する。
  subroutine estTransit(city_)
    type(city), target :: city_
    integer :: j, k, l, pl_gid
    real(8) :: s, e, i, r, v, n, beta
    !type(place), pointer :: p

    do pl_gid = 1, gl_n_places
      s = gl_place_nVis(pl_gid, HL_SUS)
      e = gl_place_nVis(pl_gid, HL_EXP)
      i = gl_place_nVis(pl_gid, HL_INF)
      r = gl_place_nVis(pl_gid, HL_REC)
      v = gl_place_nVis(pl_gid, HL_VAC)
      n = s + e + i + r + v
      beta = 0d0; if (n > 0d0) beta = gl_place_params(pl_gid, PARAMS_BETAN) / n
      gl_place_pTrns(pl_gid, PTRNS_S2E) = beta * i * dt
      gl_place_pTrns(pl_gid, PTRNS_E2I) = alpha * dt
      gl_place_pTrns(pl_gid, PTRNS_I2R) = gamma * dt
    end do
  end subroutine 


  ! 個人の移動の結果を、場所毎の人口に反映させる
  !  - 各場所の訪問者数を数え上げる
  !  - それに応じて、次のステップで使う(健康状態の)遷移確率を計算する。
  subroutine evalPlace(city_) 
    type(city), target :: city_
    integer :: i, j, k, pl_gid
    !$omp do private(j,k)
    do i = 1, size(city_%area)
      do j = 1, size(city_%area(i)%place)
        do k = 1, size(city_%area(i)%place(j)%o)
          pl_gid = city_%area(i)%place(j)%o(k)%pl_gid
          gl_place_nVis(pl_gid, :) = 0
        end do
      end do
    end do
    !$omp end do
    !$omp barrier

    !$omp do
    do i = 1, size(city_%area)
      do k = 1, size(city_%area(i)%person)
        call addVis(city_%area(i)%person(k))
      end do
    end do
    !$omp end do
    call estTransit(city_)

  contains

    subroutine addVis(p)
      type(person) :: p
      !
      integer :: pl_gid, hl_kind

      pl_gid = gl_person_visit_gid(p%pe_gid)  ! current place
      hl_kind = gl_person_health(p%pe_gid, 1)%kind
      if (1 <= hl_kind .and. hl_kind < HL_MAX) then
        gl_place_nVis(pl_gid, hl_kind) = gl_place_nVis(pl_gid, hl_kind) + 1
      else
        write(*,*) 'evalPlace: Undeined health state: ', gl_person_health(p%pe_gid, 1)
      end if
    end subroutine
  end subroutine

  subroutine advanceTime(city_)
    type(city) :: city_
    integer :: i, j
    integer :: t0
    integer, external :: utime

    integer, save :: person, place
    integer :: n_onService
    
    call evalTrains(city_)

    !$omp barrier
    !$omp master
    t0 = utime()
    !$omp end master
    !$omp barrier

    call evalPeople(city_)

    !$omp barrier
    !$omp master
    person = person + utime() - t0
    !write(*,*) 'Person:', person
    !$omp end master
    !$omp barrier


    !$omp barrier
    !$omp master
    city_%time = city_%time + itimeStride
    !$omp end master
    !$omp flush

    !$omp barrier
    !$omp master
    t0 = utime()
    !$omp end master
    !$omp barrier

    call evalPlace(city_)

    !$omp barrier
    !$omp master
    place = place + utime() - t0
    if (city_%time .eq. 2880) then
      write(*,*) 'Person:', person
      write(*,*) 'Place :', place
    end if
    !$omp end master
    !$omp barrier

    !$omp barrier
  end subroutine

  subroutine advanceTime2(city_)
    implicit none
    type(city) :: city_
    integer :: i, j
    integer :: t0
    integer, external :: utime

    integer, save :: person, place
    integer :: n_onService
    integer :: pe_gid, hl_kind, pl_gid, mismatch  ! debug
    
    call evalTrains(city_)
    
    t0 = utime()

    call evalPeopleWithPlace(city_)

    !***debug***
    if (.false.) then
      !$acc wait
      mismatch = 0
      write(*,*) city_%time
      gl_place_nVis2(:,:) = 0
      do pe_gid = 1, gl_n_people
        hl_kind = gl_person_health(pe_gid, 1)%kind
        pl_gid = gl_person_visit_gid(pe_gid)
        gl_place_nVis2(pl_gid, hl_kind) = gl_place_nVis2(pl_gid, hl_kind) + 1
      end do
      do pl_gid = 1, gl_n_places
        do hl_kind = 1, HL_MAX
          if (gl_place_nVis(pl_gid, hl_kind) == gl_place_nVis2(pl_gid, hl_kind)) cycle
          mismatch = mismatch + 1
          write(*,*) "# Mismatch", pl_gid, gl_place_idx(pl_gid)%place_k, &
            hl_kind, gl_place_nVis(pl_gid, hl_kind), gl_place_nVis2(pl_gid, hl_kind)
        end do
      end do
      if (mismatch > 0) then
        write(0,*) 'unreal_framm.f90:922: mismatch ABEND!'
        stop
      end if
    end if
    !***debug***
    
    person = person + utime() - t0

    city_%time = city_%time + itimeStride

  end subroutine
end module
