module Model1 
  use Frame
  use X_Misc
  use Record
!  use Hook
  implicit none
  ! モデル設定パラメータ:
  ! 実装上の注意: 内容はtenativeかつadhocである。型を立てるのは、記述上の便宜のためであって、
  ! 設計上の理由ではない。

  ! ToDo: fun city で作った町を、ファイルに書き出すことにしたらどう?
  ! そのほうが、まち構成るーるをシェアできてよい。
  !
  !

  ! a back door to see city object from some functions including scheduler 
  type(city), pointer :: city__
contains
  ! fromBase0: 列挙子に対する整数値の起算方法の変換
  ! - 健康状態、地区、場所種別、等の列挙子を表現する0起算の整数値を
  !   1起算に変換する。
  ! - 列挙子を整数値に変換したものを外部ファイルに記録する場合には、
  !   原則として0起算の整数とする。
  ! - 一方、Fortran内で扱うときには、配列の下限にデフォルトに合わせて
  !   1起算とする。
  subroutine fromBase0(idx)
    integer :: idx
    idx = idx + 1
  end subroutine

  integer function roleFromString(str) result(it)
    character(256) :: str
    select case (str)
    case ("Employed"); it = Employed
    case ("Hausfrau"); it = Hausfrau
    case ("Student "); it = Student 
    case ("Doctor"); it = Doctor
    case ("Patient"); it = Patient
    case ("Visitor"); it = Visitor
    case default
      write(*,*) 'unknown place_k name: ', trim(str)
      it = -255
    end select
  end function

!  function place_kToString(str) result(it)
!    integer :: str
!    character(16) :: it
!    select case (str)
!    case (PL_CRAM) ; it = "Cram"
!    case (PL_SCH)  ; it = "Sch"
!    case (PL_CORP) ; it = "Corp"
!    case (PL_HOME) ; it = "Home"
!    case (PL_SUPER); it = "Super"
!    case (PL_PARK) ; it = "Park"
!    case (PL_HOSP) ; it = "Hosp"
!    case (PL_TRAIN); it = "Train"
!    case default; 
!      write(*,*) 'unknown place_k value:', str
!      stop
!    end select
!  end function

  integer function place_kFromString(str) result(it)
    character(256) :: str
    select case (str)
    case ("Cram"); it = PL_CRAM
    case ("Sch"); it = PL_SCH
    case ("Corp"); it = PL_CORP 
    case ("Home"); it = PL_HOME
    case ("Super"); it = PL_SUPER
    case ("Park"); it = PL_PARK 
    case ("Hosp"); it = PL_HOSP
    case ("Train"); it = PL_TRAIN
    case ("Rest") ; it = PL_REST
! -- pseudo place kind, having a negative int
    case ("Game"); it = PL_GAME
    case ("Any");  it = PL_ANY
! --
    case default; 
      write(0,*) 'model1.f90:78:parse error: unknown place_k symbol:', trim(str)
      stop
    end select
  end function

  integer function healthFromString(str) result(it)
    character(256) :: str
    select case (str)
    case ("SUS"); it = HL_SUS
    case ("EXP"); it = HL_EXP 
    case ("INF"); it = HL_INF
    case ("VAC"); it = HL_VAC
    case ("REC"); it = HL_REC 
    case ("DIE"); it = HL_DIE
    case default; 
      write(0,*) 'parse error: unknown place_k symbol:', trim(str)
      stop
    end select
  end function

  subroutine simpleReadCheck(city_)
    type(city) :: city_
    integer ::i, j
    write(*,*) "#areas  = ", size(city_%area)
    do i = 1, size(city_%area)
    write(*,*) "  #persons = ", size(city_%area(i)%person)
    do j = 1, size(city_%area(i)%place)
    write(*,*) "  #place(",j,")", size(city_%area(i)%place(j)%o)
    end do
    end do
    write(*,*) "#trains = ", size(city_%train)
  end subroutine

  subroutine recoverParameters(city_)
    type(city) :: city_
    integer :: i, j, k
    real(8) :: betaN(size(city_%area(1)%place) + 1)
    real(8) :: rr(size(city_%area(1)%place) + 1)
    integer :: cnt(size(city_%area(1)%place) + 1)

    betaN = 0
    cnt = 0

    do i = 1, size(city_%area)
      do j = 1, size(city_%area(i)%place)
        do k = 1, size(city_%area(i)%place(j)%o)
          betaN(j) = betaN(j) + city_%area(i)%place(j)%o(k)%betaN
          cnt(j)  = cnt(j) + 1
        end do
      end do
    end do

    do i = 1, size(city_%train)
      betaN(PL_TRAIN) = betaN(PL_TRAIN) + city_%train(i)%_pl%betaN
      cnt(PL_TRAIN) = cnt(PL_TRAIN) + 1
    end do

    do j = 1, size(city_%area(1)%place) + 1
      rr(j) = betaN(j) / cnt(j) / gamma
      write(*,*) 'rr[',trim(place_kToString(j)),'] = ', rr(j)
    end do
  end subroutine

  subroutine modifyBetaN(city_,betaN,mask)
    type(city) :: city_
    real(8) :: betaN(:)
    logical :: mask(:)
    integer :: i, j, k

    do i = 1, size(city_%area)
      do j = 1, size(city_%area(i)%place)
        if (mask(j)) then
          do k = 1, size(city_%area(i)%place(j)%o)
            city_%area(i)%place(j)%o(k)%betaN = betaN(j)
          end do
        end if
      end do
    end do
  end subroutine

  subroutine modifyRR(city_, rr, mask)
    type(city) :: city_
    real(8) :: rr(:)
    logical :: mask(:)
    real(8) :: betaN(size(rr))
    betaN = rr*gamma
    call modifyBetaN(city_, betaN, mask)
  end subroutine

  subroutine readCity(city_,filename, f_cafe)
    implicit none
    type(city), target :: city_
    character(*)   :: filename, f_cafe
    type(place_t), allocatable, dimension(:) :: plset_save
    
    character(256) :: temp
    integer, parameter :: ist = 8932
    integer :: j, k, l, pe_gid, pl_gid
    logical :: doesMatch
    type(person), pointer :: p
    type(place_t) :: visit
    integer :: ki, n_section_dropRestFreq, n_section_dropRestFreq2
    real(8) :: kr, dropRestFreq, dropRestFreq2

    type(area), pointer :: area_
    type(place), pointer, dimension(:) :: place_
    real(8) :: R0ovw(PL_MAX)

    frm_rndstat = 1
    gl_n_people = 0
    gl_n_places = 0
    gl_n_trains = 0

    open(ist, file=filename)
    call readArea(city_%area)
    call readTrain(city_%train)
    call readTime(city_%time)
   
    ! R0 & Restaurants may be configured by an extra file
    if (trim(f_cafe) .ne. '') then
      close(ist)
      write(*,*) 'restraunts are confired by an extra file'
      open(ist, file=f_cafe)
    endif

    call readOverwriteR0(city_, R0ovw) 

    write(*,*) '1 sizeof(plset_save) = ', size(plset_save)

    call readRest(city_%rest)
    write(*,*) '2 sizeof(plset_save) = ', size(plset_save)
    call setupRest(city_)
    write(*,*) '3 sizeof(plset_save) = ', size(plset_save)

    call setExtraCfg(city_, R0ovw, plset_save)

    close(ist)


    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    ! HERE city obj is modified by shared object user-defined routine
    ! call HookCheckCity(city_, plset_save)  ! alloc & return the contents in plset_save


    write(*,*) 'readCity: ', size(plset_save), ' places will be monitored.'; call flush(5)

    ! immediately convert to global-place-id (pl_gid) system and push them to city
   
    allocate(city_%pl_save_gid(size(plset_save)))
    do J = 1, size(city_%pl_save_gid)
      write(*,*) J, plset_save(J)%area_t, plset_save(J)%place_k, plset_save(J)%id  ; call flush(6)
      area_                => city_%area ( plset_save(J)%area_t  )
      place_               => area_%place( plset_save(J)%place_k )%o
      city_%pl_save_gid(J) =  place_     ( plset_save(J)%id      )%pl_gid
      !write(*,*) j,":", city_%pl_save_gid(j)
    end do
    write(*,*) '---------------------------------------------------------------'
    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    allocate(onServiceTrainIds(size(city_%train)))

    allocate(city_%psumAreaPeople(size(city_%area)+1))
    city_%psumAreaPeople(1) = 0
    do j = 1, size(city_%area)
      city_%psumAreaPeople(j+1) = city_%psumAreaPeople(j) + size(city_%area(j)%person)
    end do
    write(*,*) "city_%psumAreaPeople:", city_%psumAreaPeople

    ! Person
    write(*,*) "gl_n_people:", gl_n_people
    allocate(gl_person_sched(gl_n_people))
    allocate(gl_person_visit_gid(gl_n_people))
    allocate(gl_person_dest_gid(gl_n_people))
    allocate(gl_person_dest_kind(gl_n_people))
    allocate(gl_person_nHealth(gl_n_people))
    allocate(gl_person_health(gl_n_people, HL_MAX))
    allocate(gl_person_age(gl_n_people))
    allocate(gl_person_hyposensitized(gl_n_people))
    allocate(gl_person_nu(gl_n_people))
    allocate(gl_person_nIntervEvt(gl_n_people))
    allocate(gl_person_intervEvt(gl_n_people, 10))
    allocate(gl_person_dropRestFreq(gl_n_people))
    allocate(gl_person_dropRestFreq2(gl_n_people))
    n_section_dropRestFreq  = size(city_%rest%dropRestFreqSet)  - 1
    n_section_dropRestFreq2 = size(city_%rest%dropRestFreqSet2) - 1
    do j = 1, size(city_%area)
      do k = 1, size(city_%area(j)%person)
        p => city_%area(j)%person(k)
        pe_gid = p%pe_gid

        gl_person_sched(pe_gid)%n = 0
        do l = 1, size(p%belong)
          visit = p%belong(l)
          if (visit%place_k .eq. PL_TRAIN) then
            p%belong(l)%pl_gid = city_%train(visit%id)%_pl%pl_gid
          else
            p%belong(l)%pl_gid = city_%area(visit%area_t)%place(visit%place_k)%o(visit%id)%pl_gid
          end if
        enddo
        visit = findBelong(p%belong, PL_HOME, doesMatch)
        if (.not. doesMatch) then
          visit = p%belong(1)
        end if
        gl_person_visit_gid(pe_gid) = visit%pl_gid
        gl_person_dest_kind(pe_gid) = NONE
        gl_person_dest_gid(pe_gid) = 0
        gl_person_nHealth(pe_gid) = 1
        gl_person_health(pe_gid, 1) = p%health
        gl_person_age(pe_gid) = p%age
        !
        kr = rndIn(frm_rndstat, 0D0, 1D0)
        ki = 1
        do while (ki < n_section_dropRestFreq .and. kr >= city_%rest%dropRestFreqSet(ki+1)%idx)
          ki = ki + 1
        end do
        kr = (kr - city_%rest%dropRestFreqSet(ki)%idx) &
           / (city_%rest%dropRestFreqSet(ki+1)%idx - city_%rest%dropRestFreqSet(ki)%idx)
        dropRestFreq = city_%rest%dropRestFreqSet(ki)%val * (1.0 - kr) &
                     + city_%rest%dropRestFreqSet(ki+1)%val * kr
        gl_person_dropRestFreq(pe_gid) = dropRestFreq
        !write(*,*) pe_gid, "dropRestFreq", dropRestFreq
        !
        kr = rndIn(frm_rndstat, 0D0, 1D0)
        ki = 1
        do while (ki < n_section_dropRestFreq2 .and. kr >= city_%rest%dropRestFreqSet2(ki+1)%idx)
          ki = ki + 1
        end do
        kr = (kr - city_%rest%dropRestFreqSet2(ki)%idx) &
           / (city_%rest%dropRestFreqSet2(ki+1)%idx - city_%rest%dropRestFreqSet2(ki)%idx)
        dropRestFreq2 = city_%rest%dropRestFreqSet2(ki)%val * (1.0 - kr) &
                      + city_%rest%dropRestFreqSet2(ki+1)%val * kr
        gl_person_dropRestFreq2(pe_gid) = dropRestFreq2
        !write(*,*) pe_gid, "dropRestFreq2", dropRestFreq2
      end do
    end do
    gl_person_hyposensitized(:) = .false.
    gl_person_nu(:) = 1D0  !減感作率 (とりあえず、1.0にしておく)
    gl_person_nIntervEvt(:) = 0  !介入操作は、まずはなしとする。
    
    ! Place
    write(*,*) "gl_n_places:", gl_n_places
    allocate(gl_place_nVis(gl_n_places, HL_MAX))
    allocate(gl_place_nVis2(gl_n_places, HL_MAX))
    allocate(gl_place_pTrns(gl_n_places, PTRNS_MAX))
    allocate(gl_place_params(gl_n_places, PARAMS_MAX))
    allocate(gl_place_idx(gl_n_places))
    allocate(gl_place_updated(gl_n_places))

    gl_place_nVis(:,:) = 0
    gl_place_nVis2(:,:) = 0
    gl_place_pTrns(:,:) = 0D0
    gl_place_updated(:) = 0
    do j = 1, size(city_%area)
      do k = 1, size(city_%area(j)%place)
        do l = 1, size(city_%area(j)%place(k)%o)
          pl_gid = city_%area(j)%place(k)%o(l)%pl_gid
          gl_place_params(pl_gid, PARAMS_BETAN) = city_%area(j)%place(k)%o(l)%betaN
          gl_place_params(pl_gid, PARAMS_EPSILON) = city_%area(j)%place(k)%o(l)%epsilon

          city_%area(j)%place(k)%o(l)%idx%pl_gid = pl_gid
          gl_place_idx(pl_gid) = city_%area(j)%place(k)%o(l)%idx
        end do
      end do
    end do
    do k = 1, size(city_%train)
      pl_gid = city_%train(k)%_pl%pl_gid
      gl_place_params(pl_gid, PARAMS_BETAN) = city_%train(k)%_pl%betaN

      city_%train(k)%_pl%idx%pl_gid = pl_gid
      gl_place_idx(pl_gid) = city_%train(k)%_pl%idx
    end do

    ! Train
    write(*,*) "gl_n_trains:", gl_n_trains
    allocate(gl_train_plGid(gl_n_trains))
    allocate(gl_train_onService(gl_n_trains))
    allocate(gl_train_sked(gl_n_trains))

    do j = 1, size(city_%train)
      gl_train_plGid(j) = city_%train(j)%_pl%pl_gid
      gl_train_onService(j) = .false.
      gl_train_sked(j)%i = 1
      gl_train_sked(j)%n = city_%train(j)%_sked%n
      do k = 1, gl_train_sked(j)%n
        gl_train_sked(j)%time  (k) = city_%train(j)%_sked%time  (k)
        gl_train_sked(j)%area_t(k) = city_%train(j)%_sked%area_t(k)
      end do
    end do
    
  contains

    subroutine check(get,rule)
      character(*) :: get, rule
      if (trim(get).ne.trim(rule)) then
        write(0,*) 'model1.f90:readCity: parse error: ', trim(get), ' should be ', trim(rule)
        stop
      end if
    end subroutine 

    subroutine readPlace_t(idx)
      type(place_t) :: idx
      read(ist,*) idx%area_t, temp, idx%id
      call fromBase0(idx%area_t)
      idx%place_k = place_kFromString(temp)
      call fromBase0(idx%id)
      idx%tVis = 0
    end subroutine 

    subroutine readHealth(p)
      type(person), target :: p
      read(ist,*,err=999) temp, p%health%time
      p%health%kind = healthFromString(temp)
      !p%nHealth = 1
      return
  999 read(ist,*) temp
      write(0,*) 'parse error in readHealth: ', trim(temp)
      stop
    end subroutine 

    subroutine readPerson(p)
      integer :: i, j, n, m
      type(person), allocatable, dimension(:) :: p
      real(8) :: age
      logical :: doesMatch
      read(ist,*) temp; call check(temp,'person:')
      read(ist,*) n; allocate(p(n))

      do i = 1, n
        read(ist,*,ERR=901) temp,age
        goto 801
  901   read(ist,*) temp   !for older format
        age = 40D0
  801   continue
        p(i)%role = roleFromString(temp)
        p(i)%age  = nint(age)
        select case (p(i)%role)
        case (Employed); p(i)%mkSched = loc(schedEmp)
        case (Hausfrau); p(i)%mkSched = loc(schedHaus)
        case (Student) ; p(i)%mkSched = loc(schedStu)
        case (Patient) ; p(i)%mkSched = loc(schedNul)
        case (Doctor)  ; p(i)%mkSched = loc(schedDoc)
        case (Visitor) ; p(i)%mkSched = loc(schedVisitor)
        end select
        read(ist,*) temp; call check(temp,'belong:')
        read(ist,*) m; allocate(p(i)%belong(m))
        do j = 1, m
          call readPlace_t(p(i)%belong(j))
        end do
        !p(i)%visit = findBelong(p(i)%belong, PL_HOME, doesMatch) 
        !if (.not.doesMatch) then
        !  p(i)%visit = p(i)%belong(1)
        !end if
        !p(i)%dest%kind = NONE
        read(ist,*) temp; call check(temp,'health:')
        call readHealth(p(i))
        !p(i)%sched%n = 0
        if (rndIn(frm_rndstat, 0D0,1D0) .lt. rateUnsafeInf&
            .or. p(i)%health%kind .eq. HL_EXP) then
          p(i)%isUnsafeInf = .true.
        else
          p(i)%isUnsafeInf = .false.
        end if
        !減感作率 (とりあえず、1.0にしておく)
        !p(i)%nu = 1D0
        !p(i)%hyposensitized = .false.
        !この個体への介入操作は、まずはなしとする。
        !p(i)%nIntervEvt = 0

        p(i)%nConference = 0

        gl_n_people = gl_n_people + 1  ! Total number of people
        p(i)%pe_gid = gl_n_people  ! Global person ID
      end do
    end subroutine

    subroutine readPlace(pl)
      type(place) :: pl
      call readPlace_t(pl%idx) 
      read(ist,*) temp; call check(temp,'betaN:')
      read(ist,*) pl%betaN
      pl%epsilon = 0.0

      gl_n_places = gl_n_places + 1  ! Total number of places
      pl%pl_gid = gl_n_places  ! Global place ID
    end subroutine

    subroutine readPlaceGroup(pl, area_id)
      type(place_vector), allocatable, dimension(:) :: pl
      integer :: area_id
      !
      integer :: i, n, idx, j, m
      read(ist,*) temp; call check(temp,'place-group:')
      !read(ist,*) n; allocate(pl(n))
      read(ist,*) n; allocate(pl(PL_MAX))
      do i = 1, n
        read(ist,*) temp; idx = place_kFromString(temp)
        read(ist,*) m; allocate(pl(idx)%o(m))
        do j = 1, m
          call readPlace(pl(idx)%o(j))
        end do
      end do
    end subroutine

    subroutine readArea(area_)
      type(area), allocatable, dimension(:) :: area_
      integer :: i, n
      read(ist,*) temp; call check(temp,'area:')
      read(ist,*) n; allocate(area_(n))
      do i = 1, n
        read(ist,*,err=999) area_(i)%id; call fromBase0(area_(i)%id)
        call readPerson(area_(i)%person)
        call readPlaceGroup(area_(i)%place, area_(i)%id)
      end do
      return
  999 read(ist,'(a256)') temp
      write(0,*) 'model1.f90:484:readArea: syntax error: ', trim(temp)
      stop
    end subroutine

    subroutine readTime(t)
      integer :: t
      read(ist,*) temp; call check(temp,'time:')
      read(ist,*) t
    end subroutine

    ! modifiy some parameters via an extra config file passed as arg(7)
    !   - 503-01-21: under porting from hook.f90 in each directory
    !   - 503-06-01: the resposible rountine for this is split into 
    !       readOverwritR0 and setExtraCfg to avoid reading yet constructed objects
    subroutine readOverwriteR0(city, R0)
      type(city) :: city
      real(8) :: betaN, R0_Game, R0(:), valu
      integer :: nItems, i, jj
      character(256) :: temp

      if (size(R0) .lt. PL_MAX) then
        write(*,*) 'readOverwriteR0: array R0 is short.'
        stop 999
      end if

      R0(:) = sqrt(-1D0) ! NaN

      read(ist,*) temp; call check(temp,'R0-overwritten:')
      read(ist,*) nItems
      write(*,*)  nItems, ' R0s are overwritten'

      do jj = 1,nItems
        read(ist,*) temp, valu
        i = place_kFromString(temp)
        select case (i)
        case (PL_ANY) ; R0(:)   = valu
        case (PL_GAME); R0_Game = valu
        case default  ; R0(i)   = valu
        end select
      end do
    end subroutine

    subroutine setExtraCfg(city, R0, plset_save)
      type(city) :: city
      type(place_t), allocatable, dimension(:) :: plset_save
      real(8) :: R0(:)

      character(256) :: f_conference
      character(256) :: line
      integer :: is_conf, jj, i, nConference, k, cnt
      integer :: cnf_id, start_time, end_time, area_t, place_k, pl_id, attendance
 
      real(8) :: betaN, R0_Game, valu
      integer :: nItems, nCafe

      if (size(R0) .lt. PL_MAX) then
        write(*,*) 'setExtraCfg: array R0 is short.'
        stop 999
      end if

      ! Reflection to Usual places
      do i = 1, size(city%area)
        do j = 1, size(city%area(i)%place)
          if (R0(j) .eq. R0(j)) then  
            ! make sure to compile with '-Kieee'
            write(*,*) 'warning: R0 overwritten in PL',j,': R0 = ',R0(j)
            city%area(i)%place(j)%o(:)%betaN = gamma * R0(j)
          else
            !write(*,*) 'original R0 @ PL',j
          end if
        end do 
      end do

      ! Reflection to Trains
      if (R0(PL_TRAIN) .eq. R0(PL_TRAIN)) then  
        do j = 1, size(city%train) 
          city%train(j)%_pl%betaN = gamma * R0(PL_TRAIN)
        end do
      end if

      ! Reflection to Conference
      !
      !do i = city%nConference ; You CANNOT do this 
      !  ...                   ; because Conferences aren't built YET!
      !end do                  ;
      !
      ! So, you need to read conference file also here
      call getarg(2,f_conference)
      open(is_conf, file=f_conference)
      read(is_conf,*) nConference
      do i = 1, nConference
        read(is_conf,'(a256)') line
        read(line,*) cnf_id, start_time, end_time, area_t, temp, pl_id, attendance
        place_k = place_kFromString(temp) 
        call fromBase0(area_t)
        call fromBase0(pl_id)
        !write(*,*) area_t, trim(temp), place_k, pl_id
        city%area(area_t)%place(place_k)%o(pl_id)%betaN = gamma * R0_Game
      end do
      close(is_conf)


      write(*,*) 'model1.f90:564 warning: R0 overwritten in PL',j,'1-100: R0 = ', R0_Game

      ! >>> tentatively monitor the game sites & restaurants:
      nCafe = size(city%area(1)%place(PL_REST)%o)
      write(*,*) 'nConference = ', nConference, ', nCafe = ', nCafe
      !503-05-28 we see that nCafe is ZERO. .... uum

      allocate(plset_save(nConference + nCafe))

      cnt = 1
      do k = 1, nConference
        plset_save(cnt)%area_t  = 1
        plset_save(cnt)%place_k = PL_HOME
        plset_save(cnt)%id      = k
        cnt = cnt + 1
      end do

      ! the area=1 where games are held.
      do k = 1, nCafe
        plset_save(cnt)%area_t  = 1 
        plset_save(cnt)%place_k = PL_REST
        plset_save(cnt)%id      = k
        cnt = cnt + 1 
      end do
!
    end subroutine 

    subroutine readRest(_rest)
      type(rest) :: _rest
      integer :: i, n
      !
      read(ist,*) temp; call check(temp,'Rest:')
      !
      read(ist,*) temp; call check(temp,'nRest:')
      write(*,*) temp
      read(ist,*) n; allocate(_rest%nRest(n))
      do i = 1, n
        read(ist,*) _rest%nRest(i)
        write(*,*) _rest%nRest(i)
      end do
      !
      read(ist,*) temp; call check(temp,'alpha:')
      write(*,*) temp
      read(ist,*) _rest%alpha
      write(*,*) _rest%alpha
      gl_place_alpha = _rest%alpha
      !$acc update device(gl_place_alpha)
      !
      read(ist,*) temp; call check(temp,'betaNRestSet:')
      write(*,*) temp
      read(ist,*) n; allocate(_rest%betaNRestSet(n))
      do i = 1, n
        read(ist,*) _rest%betaNRestSet(i)%idx, _rest%betaNRestSet(i)%val
        write(*,*) _rest%betaNRestSet(i)%idx, _rest%betaNRestSet(i)%val
      end do
      !
      read(ist,*) temp; call check(temp,'eFoldingTimeSet:')
      write(*,*) temp
      read(ist,*) n; allocate(_rest%eFoldingTimeSet(n))
      do i = 1, n
        read(ist,*) _rest%eFoldingTimeSet(i)%idx, _rest%eFoldingTimeSet(i)%val
        write(*,*) _rest%eFoldingTimeSet(i)%idx, _rest%eFoldingTimeSet(i)%val
      end do
      !
      read(ist,*) temp; call check(temp,'dropRestFreqSet:')
      write(*,*) temp
      read(ist,*) n; allocate(_rest%dropRestFreqSet(n))
      do i = 1, n
        read(ist,*) _rest%dropRestFreqSet(i)%idx, _rest%dropRestFreqSet(i)%val
        write(*,*) _rest%dropRestFreqSet(i)%idx, _rest%dropRestFreqSet(i)%val
      end do
      !
      read(ist,*) temp; call check(temp,'dropRestFreqSet2:')
      write(*,*) temp
      read(ist,*) n; allocate(_rest%dropRestFreqSet2(n))
      do i = 1, n
        read(ist,*) _rest%dropRestFreqSet2(i)%idx, _rest%dropRestFreqSet2(i)%val
        write(*,*) _rest%dropRestFreqSet2(i)%idx, _rest%dropRestFreqSet2(i)%val
      end do
      write(6,*) 'readRest --- done.'; call flush(6)
    end subroutine readRest

    subroutine setupRest(city_)
      type(city) :: city_
      integer :: i, j, m, ierr
      integer :: ki, n_section_betaN, n_section_eFoldingTime
      real(8) :: kr, betaN, eFoldingTime

      n_section_betaN = size(city_%rest%betaNRestSet) - 1
      n_section_eFoldingTime = size(city_%rest%eFoldingTimeSet) - 1
      do i = 1, size(city_%area)
!write(*,*) i
!call flush(6)
        if (i.gt.size(city_%rest%nRest)) then
          m = 0
        else
          m = city_%rest%nRest(i)
        end if
        write(*,*) i, m
        allocate(city_%area(i)%place(PL_REST)%o(m),stat=ierr)
        write(*,*) i, ierr, 'ok'
        call flush(6)
        if (ierr.ne.0) then
          write(*,*) 'malloc fails;', m
          stop 9801
        end if
        do j = 1, m
          ! (*) readPlace_t
          city_%area(i)%place(PL_REST)%o(j)%idx%area_t  = i
          city_%area(i)%place(PL_REST)%o(j)%idx%place_k = PL_REST
          city_%area(i)%place(PL_REST)%o(j)%idx%id      = j
          city_%area(i)%place(PL_REST)%o(j)%idx%tVis    = 0
          !
          kr = rndIn(frm_rndstat, 0D0, 1D0)
          ki = 1
          do while (ki < n_section_betaN .and. kr >= city_%rest%betaNRestSet(ki+1)%idx)
            ki = ki + 1
          end do
          kr = (kr - city_%rest%betaNRestSet(ki)%idx) &
             / (city_%rest%betaNRestSet(ki+1)%idx - city_%rest%betaNRestSet(ki)%idx)
          betaN = city_%rest%betaNRestSet(ki)%val * (1.0 - kr) &
                + city_%rest%betaNRestSet(ki+1)%val * kr
          city_%area(i)%place(PL_REST)%o(j)%betaN = betaN
          !write(*,*) i, j, "betaN", betaN
          !
          kr = rndIn(frm_rndstat, 0D0, 1D0)
          ki = 1
          do while (ki < n_section_eFoldingTime .and. kr >= city_%rest%eFoldingTimeSet(ki+1)%idx)
            ki = ki + 1
          end do
          kr = (kr - city_%rest%eFoldingTimeSet(ki)%idx) &
             / (city_%rest%eFoldingTimeSet(ki+1)%idx - city_%rest%eFoldingTimeSet(ki)%idx)
          eFoldingTime = city_%rest%eFoldingTimeSet(ki)%val * (1.0 - kr) &
                       + city_%rest%eFoldingTimeSet(ki+1)%val * kr
          city_%area(i)%place(PL_REST)%o(j)%epsilon = 1.0 - 1.0 / eFoldingTime
          !write(*,*) i, j, "eFoldingTime", eFoldingTime
          !
          gl_n_places = gl_n_places + 1  ! Total number of places
          city_%area(i)%place(PL_REST)%o(j)%pl_gid = gl_n_places  ! Global place ID
        end do
      end do
      write(6,*) 'setupRest -- done.'; call flush(6)
    end subroutine setupRest

    subroutine readMobSched(sch)
      type(mob_sched) :: sch
      integer :: i, n
      read(ist,*) n; sch%n = n
      read(ist,*) temp; call check(temp,'sked:')
      do i = 1, n
        read(ist,*) sch%time(i), sch%area_t(i)
        call fromBase0(sch%area_t(i))
      end do
    end subroutine

    subroutine readTrain(tr)
      type(mobil), allocatable, dimension(:) :: tr 
      integer :: i, n
      read(ist,*) temp; call check(temp,'train:')
      read(ist,*) n; allocate(tr(n))
      do i = 1, n
        tr(i)%tr_gid = i
        call readPlace(tr(i)%_pl)
        call readMobSched(tr(i)%_sked)
        !tr(i)%iSked = 1
        !tr(i)%onService = .false.
      end do

      gl_n_trains = n
    end subroutine

  end subroutine readCity


  subroutine resetCity(city_)
    type(city), target :: city_
    integer :: i, j, k
    type(person), pointer :: p
    
    city_%time = 0

    gl_place_nVis(:,:) = 0
    gl_place_pTrns(:,:) = 0D0

    do i = 1, size(city_%area)
      do j = 1, size(city_%area(i)%person)
        p => city_%area(i)%person(j)
        gl_person_nHealth(p%pe_gid) = 1
        gl_person_health(p%pe_gid, 1) = health(HL_SUS, 0)
      end do
    end do
  end subroutine

  character(16) function showPlace_k(k) result (it)
    integer :: k
    select case (k)
    case (PL_CRAM ); it = "PL_CRAM"
    case (PL_SCH  ); it = "PL_SCH"
    case (PL_CORP ); it = "PL_CORP"
    case (PL_HOME ); it = "PL_HOME"
    case (PL_SUPER); it = "PL_SUPER"
    case (PL_PARK ); it = "PL_PARK"
    case (PL_HOSP) ; it = "PL_HOSP"
    case (PL_TRAIN); it = "PL_TRAIN"
    end select
  end function

  subroutine writeSched(ost, sch)
    integer :: ost, i
    type(sched)   :: sch
    !
    type(place_t) :: idx
    idx = gl_place_idx(sch%pl_gid(i))
    do i = 1, sch%n
      write(ost,*) sch%time(i), &
        idx%area_t, trim(showPlace_k(idx%place_k)), idx%id
    end do
  end subroutine


  integer(2) function int_comp(i,j)
    integer :: i, j
    int_comp = i - j
  end function

  logical function predCorp(x) result (rule)
    type(place_t) :: x
    rule = x%place_k .eq. PL_CORP
  end function

  integer(2) function idx_comp(i,j)
    type(sched), pointer :: schedEmp__sch
    common /schedEmp__c/ schedEmp__sch
    !$omp threadprivate(/schedEmp__c/)
    integer :: i, j
    idx_comp = schedEmp__sch%time(i) - schedEmp__sch%time(j)
  end function

  subroutine schedNul(p, t, sch, seed)
    type(person) :: p ! implicitly passed pointer to object
    integer      :: t
    type(sched), target :: sch
    integer      :: seed
    sch%n = 0
  end subroutine

  subroutine schedEmp(p, t, sch, seed)
    type(sched), pointer :: schedEmp__sch
    common /schedEmp__c/ schedEmp__sch
    !$omp threadprivate(/schedEmp__c/)
    integer      :: seed
    type(person) :: p ! implicitly passed pointer to object
    integer      :: t, isize, today, i, k, n
    type(sched), target :: sch
    type(tmcomp) :: tm
    integer      :: idx(size(p%belong)), nCorp, nVisCorp
    !integer, allocatable :: idx2(:)
    integer :: idx2(SCHED_MAX)
    integer :: myCorp
    real(8), parameter :: pOut = 0.9D0
    real(8) :: pStop
    integer :: hl_kind
    type(place_t) :: visit, dest

    tm    = timecomp(t)
    today = tm%day * idays
    hl_kind = gl_person_health(p%pe_gid, 1)%kind
    visit = gl_place_idx(gl_person_visit_gid(p%pe_gid))

    if (tm%step .eq. 0) then
      if (.not.p%isUnsafeINF .and. hl_kind.eq.HL_INF) then
        if (visit%place_k .ne. PL_HOME) then
          sch%n = 1
          dest = findBelong(p%belong, PL_HOME)
          sch%time  (1) = t + 1
          sch%pl_gid(1) = dest%pl_gid
        else
          sch%n = 0
        end if
        return
      end if

      if (1 .le. tm%weekday .and. tm%weekday .le. 5) then
        sch%n = 2
        dest = findBelong(p%belong, PL_CORP)
        sch%time  (1) = today + irndIn (seed,  6*ihours, 10*ihours)
        sch%pl_gid(1) = dest%pl_gid
        dest = findBelong(p%belong, PL_HOME)
        sch%time  (2) = today + irndIn (seed, 18*ihours, 22*ihours)
        sch%pl_gid(2) = dest%pl_gid
        myCorp = sch%pl_gid(1)

       if (rndIn(seed, 0.0D0, 1.0D0) .lt. pOut) then
         call filterBelong(p%belong, predCorp, idx, nVisCorp)
         if (nVisCorp .ge. 1) then
           pStop = 1.0D0/dble(nVisCorp)
           do i = 1, nVisCorp
             ! (自社) --> (取引先)
             sch%n = sch%n + 1
             k = irndIn (seed, 1, nVisCorp)
             k = idx(k)
             sch%time  (sch%n) = today + irndIn(seed, 10*ihours, 16*ihours)
             sch%pl_gid(sch%n) = p%belong(k)%pl_gid
             ! (取引先) --> (自社)
             sch%n = sch%n + 1
             sch%time  (sch%n) = sch%time(sch%n-1) + irndIn(seed, 30*iminutes, 2*ihours)
             sch%pl_gid(sch%n) = myCorp
             if (rndIn(seed,0.0D0,1.0D0) .lt. pStop) exit
           end do
           !if (sch%n .gt. 2) write(*,*) sch%n
           !乱数で時刻を決めたので、時間順に並べ替える
           idx2(1:sch%n) = (/(i,i=1,sch%n)/)
           !【注意】inner !functionをqsortに渡すのを避けるために、外部関数+ポインタ
           ! を使っている。そうしないと、OpenMPのもとでは性能がガタ落ちになる。
           schedEmp__sch => sch
           call qsort(idx2, sch%n, 4, idx_comp)
           sch%time  (1:sch%n) = sch%time  (idx2(1:sch%n))
           sch%pl_gid(1:sch%n) = sch%pl_gid(idx2(1:sch%n))
         end if
       end if
      else
        sch%n = irndIn (seed, 1, size(p%belong))
        do i = 1, sch%n
          sch%time(i) = today + irndIn (seed, 10*ihours, 18*ihours)
        end do
        isize = LOC(sch%time(2)) - LOC(sch%time(1))
        call qsort(sch%time, sch%n, isize, int_comp)
        do i = 1, sch%n
          sch%pl_gid(i) = p%belong(irndIn (seed, 1, sch%n))%pl_gid
        end do
        sch%n = sch%n + 1
        dest = findBelong(p%belong, PL_HOME)
        sch%time  (sch%n) = today + 18*ihours
        sch%pl_gid(sch%n) = dest%pl_gid
      end if
    end if

  end subroutine 

  subroutine schedStu(p, t, sch, seed)
    integer      :: seed
    type(person) :: p ! implicitly passed pointer to object
    integer       :: t, isize, today, i
    type(sched)   :: sch
    logical       :: exists
    type(tmcomp)  :: tm
    type(place_t) :: ignore
    integer :: hl_kind
    type(place_t) :: visit, dest

    tm    = timecomp(t)
    today = tm%day * idays
    hl_kind = gl_person_health(p%pe_gid, 1)%kind
    visit = gl_place_idx(gl_person_visit_gid(p%pe_gid))


    if (tm%step .eq. 0) then
      if (.not.p%isUnsafeINF .and. hl_kind.eq.HL_INF) then
        if (visit%place_k .ne. PL_HOME) then
          sch%n = 1
          dest = findBelong(p%belong, PL_HOME)
          sch%time  (1) = t + 1
          sch%pl_gid(1) = dest%pl_gid
        else
          sch%n = 0
        end if
        return
      end if

      if (1 .le. tm%weekday .and. tm%weekday .le. 5) then
        sch%n = 2
        dest = findBelong(p%belong, PL_SCH)
        sch%time  (1) = today + irndIn (seed,  6*ihours, 8*ihours)
        sch%pl_gid(1) = dest%pl_gid
        dest = findBelong(p%belong, PL_HOME)
        sch%time  (2) = today + irndIn (seed, 16*ihours,19*ihours)
        sch%pl_gid(2) = dest%pl_gid
        ignore = findBelong(p%belong, PL_CRAM, exists)
        if (exists) then
          sch%n = sch%n + 2
          dest = findBelong(p%belong, PL_CRAM)
          sch%time  (3) = today + irndIn (seed, 19*ihours, 20*ihours)
          sch%pl_gid(3) = dest%pl_gid
          dest = findBelong(p%belong, PL_HOME)
          sch%time  (4) = today + irndIn (seed, 21*ihours, 22*ihours)
          sch%pl_gid(4) = dest%pl_gid
        end if
      else
        sch%n = irndIn (seed, 1, size(p%belong))
        do i = 1, sch%n
          sch%time(i) = today + irndIn (seed, 10*ihours, 18*ihours)
        end do
        isize = LOC(sch%time(2)) - LOC(sch%time(1))
        call qsort(sch%time, sch%n, isize, int_comp)
        do i = 1, sch%n
          sch%pl_gid(i) = p%belong(irndIn (seed, 1, sch%n))%pl_gid
        end do
        sch%n = sch%n + 1
        dest = findBelong(p%belong, PL_HOME)
        sch%time  (sch%n) = today + 18*ihours
        sch%pl_gid(sch%n) = dest%pl_gid
      end if
    end if
  end subroutine 

  !医療受持者のスケジューラ。会社員のスケジューラのクローンなので改変には注意すること
  subroutine schedDoc(p, t, sch, seed)
    type(sched), pointer :: schedDoc__sch
    common /schedDoc__c/ schedDoc__sch
    !$omp threadprivate(/schedDoc__c/)
    integer      :: seed
    type(person) :: p ! implicitly passed pointer to object
    integer      :: t, isize, today, i, k, n
    type(sched), target :: sch
    type(tmcomp) :: tm
    integer      :: idx(size(p%belong)), nCorp, nVisCorp
    !integer, allocatable :: idx2(:)
    integer :: idx2(SCHED_MAX)
    integer :: myCorp
    real(8) :: pStop
    integer :: hl_kind
    type(place_t) :: visit, dest

    tm    = timecomp(t)
    today = tm%day * idays
    hl_kind = gl_person_health(p%pe_gid, 1)%kind
    visit = gl_place_idx(gl_person_visit_gid(p%pe_gid))

    if (tm%step .eq. 0) then
      if (.not.p%isUnsafeINF .and. hl_kind.eq.HL_INF) then
        if (visit%place_k .ne. PL_HOME) then
          sch%n = 1
          dest = findBelong(p%belong, PL_HOME)
          sch%time  (1) = t + 1
          sch%pl_gid(1) = dest%pl_gid
        else
          sch%n = 0
        end if
        return
      end if

      if (1 .le. tm%weekday .and. tm%weekday .le. 5) then
        sch%n = 2
        dest = findBelong(p%belong, PL_HOSP)
        sch%time  (1) = today + irndIn (seed,  6*ihours, 10*ihours)
        sch%pl_gid(1) = dest%pl_gid
        dest = findBelong(p%belong, PL_HOME)
        sch%time  (2) = today + irndIn (seed, 18*ihours, 22*ihours)
        sch%pl_gid(2) = dest%pl_gid
        myCorp = sch%pl_gid(1)
      else
        sch%n = irndIn (seed, 1, size(p%belong))
        do i = 1, sch%n
          sch%time(i) = today + irndIn (seed, 10*ihours, 18*ihours)
        end do
        isize = LOC(sch%time(2)) - LOC(sch%time(1))
        call qsort(sch%time, sch%n, isize, int_comp)
        do i = 1, sch%n
          sch%pl_gid(i) = p%belong(irndIn (seed, 1, sch%n))%pl_gid
        end do
        sch%n = sch%n + 1
        dest = findBelong(p%belong, PL_HOME)
        sch%time  (sch%n) = today + 18*ihours
        sch%pl_gid(sch%n) = dest%pl_gid
      end if
    end if

  end subroutine 

  subroutine schedVisitor(p, t, sch, seed)
    implicit none
    integer      :: seed
    type(person) :: p ! implicitly passed pointer to object
    integer       :: t, isize, today, i, iConf
    type(sched)   :: sch
    logical       :: exists
    type(tmcomp)  :: tm
    type(place_t) :: ignore, home
    integer :: hl_kind
    type(place_t) :: visit, dest

    integer, parameter :: n_daily_visitors = 1000
    integer :: n_game_visitors
    integer, save ::  dayFstEval = INT4_MIN
    integer :: i_area, n_rest, i_rest, n_visit_games
    logical :: doesMatch


    !call schedHaus(p, t, sch, seed)
    !return

    tm    = timecomp(t)
    today = tm%day * idays
    hl_kind = gl_person_health(p%pe_gid, 1)%kind

    ! get smallest pe_gid among VISITORS (quick hack)
    if (dayFstEval .eq. INT4_MIN) dayFstEval = today

    sch%n = 0

    if (today .eq. dayFstEval) then
      ! not run: 
      !  * You need to get a scope of city object!
      !  * E.g. you have via common pointing it. But it can be done in Fortran?
      if (.not.associated(city__)) then
        write(0,*) 'model1.f90:1031:schedVistor: you need to bind city object to city__ in advance!!. ABEND'
        flush 0
        stop 1234
      end if
      ! assume that * p%pe_gid << n_randBuf = 32GB
      !             * getRand() in [0,1)  (not [0,1])

      home = findBelong(p%belong, PL_HOME, doesMatch)
      if (.not.doesMatch) then 
        write(*,*) size(p%belong)
        do i = 1, size(p%belong)
          write(*,*) place_kToString(p%belong(i)%place_k)
        end do
        !stop 235
        home = p%belong(1) !maybe Hospital
      end if

      n_visit_games = irndIn(seed, 1, 3)

      do i = 1, n_visit_games
        iConf = 1 + int(getRand(p%pe_gid)*city__%nConference)
        sch%n = sch%n + 1

        sch%time  (sch%n) = city__%conference(iConf)%start_time
        sch%pl_gid(sch%n) = city__%conference(iConf)%pl_gid

        ! to do: make sure visit a restaurant every day
        sch%n = sch%n + 1

        i_area = city__%conference(iConf)%area_t
        n_rest = size(city__%area(i_area)%place(PL_REST)%o)

        i_rest = irndIn(seed, 1, n_rest)
        sch%time  (sch%n) = (city__%conference(iConf)%start_time/1440)*1440 + 18*60
        sch%pl_gid(sch%n) = &
          city__%area(i_area)%place(PL_REST)%o(i_rest)%pl_gid  ! REST

        sch%n = sch%n + 1
       
        sch%pl_gid(sch%n) = home%pl_gid
        sch%time  (sch%n) = (city__%conference(iConf)%start_time/1440)*1440 + 23*60 + 55
      end do
    else
      ! to do: resort using qsort
    end if
  end subroutine




  integer(2) function sched2_comp(x, y) result (cmp)
    type sched2
      integer :: tIn, tOut
      type(place_t) :: place_t
    end type
    type (sched2) :: x, y
    if (x%tIn .lt. y%tIn) then
      cmp = -1; return
    else if (x%tIn .gt. y%tIn) then
      cmp =  1; return
    else
      cmp = 0
    end if
  end function

  subroutine schedHaus(p, t, sch, seed)
    integer       :: seed
    type(person)  :: p ! implicitly passed pointer to object
    integer       :: t, isize, today, i, j, idx(size(p%belong)), kind, nOthers, nO
    type(sched)   :: sch
    logical       :: exists
    type(tmcomp)  :: tm
    type(place_t) :: ignore, home
    integer :: tInSuper, tOutSuper
    
    type sched2
      integer :: tIn, tOut
      type(place_t) :: place_t
    end type
    type (sched2), allocatable :: schedO(:)
    type (sched2) :: schedS
    integer :: hl_kind
    type(place_t) :: visit, dest

    tm    = timecomp(t)
    today = tm%day * idays
    hl_kind = gl_person_health(p%pe_gid, 1)%kind
    visit = gl_place_idx(gl_person_visit_gid(p%pe_gid))

    if (tm%step .eq. 0) then
      if (.not.p%isUnsafeINF .and. hl_kind.eq.HL_INF) then
        if (visit%place_k .ne. PL_HOME) then
          sch%n = 1
          dest = findBelong(p%belong, PL_HOME)
          sch%time  (1) = t + 1
          sch%pl_gid(1) = dest%pl_gid
        else
          sch%n = 0
        end if
        return
      end if

      ! (0) スーパー、家、以外の場所を数え上げる。
      j = 0
      do i = 1, size(p%belong) 
        kind = p%belong(i)%place_k
        if (kind .ne. PL_SUPER .and. kind .ne. PL_HOME) then
          j = j + 1
          idx(j) = i
        end if
      end do
      nOthers = j

      ! 注意: nO in [0, nOthers + 1) = [0,nOthers]
      nO = irndIn (seed, 0, nOthers + 1) 
      allocate(schedO(nO + 1))      ! スーパー + その他

      !(* 1. 一日のどこかの時点でたかだか1時間の買い物をする *)
      schedO(1)%tIn     = today         + irndIn (seed, 10*ihours  , 20*ihours)
      schedO(1)%tOut    = schedO(1)%tIn + irndIn (seed, 10*iminutes,  1*ihours)
      schedO(1)%place_t = findBelong(p%belong, PL_SUPER)
      ! 注: 本当はランダムにスーパーを選ぶ

      !(* 2. 別の場所への移動計画 *)
      do i = 2, nO + 1
        schedO(i)%tIn     = today         + irndIn (seed, 10*ihours, 20*ihours)
        schedO(i)%tOut    = schedO(i)%tIn + irndIn (seed,  0*ihours,  2*ihours)
        schedO(i)%place_t = p%belong(idx(irndIn (seed, 1,nOthers+1)))
      end do

      call qsort(schedO, size(schedO), sizeof_sched2(), sched2_comp)

      !(* 3. 場所毎の滞在時刻の間隔から旅程を連結する *)
      home = findBelong(p%belong, PL_HOME)
      sch%n = 0
      call connect (schedO, sch%n) !build outcome of this proc.
    end if

  contains

    integer function sizeof_sched2() result (it)
      type(sched2) :: x(2)
      it = LOC(x(2)) - LOC(x(1))
    end function

    recursive subroutine connect(schedO, n)  
      type(sched2) :: schedO(:)
      integer      :: n
      if (size(schedO) .ge. 2) then
        ! ある訪問先とつぎの訪問先の訪問時間間隔が2時間を超える場合は
        ! 一旦、家に帰る。
        if (schedO(1)%tIn + 2*ihours .lt. schedO(2)%tIn) then !   tb < tb' 
          n = n + 1
          sch%time  (n) = schedO(1)%tIn
          sch%pl_gid(n) = schedO(1)%place_t%pl_gid
          n = n + 1
          sch%time  (n) = schedO(1)%tOut
          sch%pl_gid(n) = home%pl_gid
          call connect(schedO(2:),n)
        else 
          n = n + 1
          sch%time  (n) = schedO(1)%tIn
          sch%pl_gid(n) = schedO(1)%place_t%pl_gid
          call connect(schedO(2:),n)
        end if
      else if (size(schedO) .eq. 1) then
          n = n + 1
          sch%time  (n) = schedO(1)%tIn
          sch%pl_gid(n) = schedO(1)%place_t%pl_gid
          n = n + 1
          sch%time  (n) = schedO(1)%tOut
          sch%pl_gid(n) = home%pl_gid
      else
        continue ! unreachable
      end if
    end subroutine
  end subroutine


  subroutine schedConference(city_, p, sch, seed)
    type(city) :: city_
    type(sched), target :: sch
    type(person) :: p
    integer :: seed
    !
    integer :: t, hl_kind, cnf_id, i
    type(tmcomp) :: tm
    type(place_t) :: visit, home
    integer :: idx2(SCHED_MAX)

    t = city_%time
    tm = timecomp(t)
    hl_kind = gl_person_health(p%pe_gid, 1)%kind
    visit = gl_place_idx(gl_person_visit_gid(p%pe_gid))
    home = findBelong(p%belong, PL_HOME)

    if (tm%step == 0) then
      sch%n = 0
      do i = 1, p%nConference
        cnf_id = p%conference(i)
        if (city_%conference(cnf_id)%cnf_day .ne. tm%day) cycle
        sch%time  (sch%n + 1) = city_%conference(cnf_id)%start_time - irndIn(seed, 0, ihours)
        sch%pl_gid(sch%n + 1) = city_%conference(cnf_id)%pl_gid
        sch%time  (sch%n + 2) = city_%conference(cnf_id)%end_time   + irndIn(seed, 0, ihours)
        sch%pl_gid(sch%n + 2) = home%pl_gid
        sch%n = sch%n + 2
      end do

      !do i = 1, sch%n
      !  write(*,*) "Person", p%pe_gid, "sched", i, sch%time(i), sch%pl_gid(i)
      !end do
    end if
    
  end subroutine schedConference

  
  !情報回収のためのテストラン
  subroutine run0 (city_, iseed)
    type(city), target :: city_
    integer :: iseed
    !
    !common /schedVisitor__c/ city__
    !type(city), pointer :: city__
    integer :: omp_get_num_threads, omp_get_thread_num
    integer :: t0, t1, i, nOcc, iargc, pl_gid, tr_gid
    character(256) :: tmp

    ! make city obj visible in schedVisitor().
    city__ => city_

    open (15, file='train_occ.csv')
    t0 = 1*idays
    t1 = 2*idays
    write(*,*) 'run 0: test run to collect information needed in the simulation'

    city_%time = t0
    nOCC = 0

    !$omp parallel

!$  frm_rndstat = omp_get_thread_num() + 1 + iseed
!$  if (.false.) then
      frm_rndstat = 1 + iseed
!$  end if
    call init_genrand(frm_rndstat)
    call initRandBuf(iseed)
    !$acc update device(frm_logTrip)

    call evalPlace(city_)

    ! city_%nPeakTrainOcc = 0 ! IT CAUSES DIV-BY-ZERO!
    city_%nPeakTrainOcc = 1D0   
    do while (city_%time .lt. t1)
      call advanceTime2(city_)

      !$omp master
      do i = 1, size(city_%train)
        tr_gid = city_%train(i)%tr_gid
        if (gl_train_onService(tr_gid)) then
          pl_gid = gl_train_plGid(tr_gid)
          nOcc = max(nOcc, gl_place_nVis(pl_gid, HL_SUS))
        end if
      end do
      write(15,*) city_%time,',', nOcc
      city_%nPeakTrainOcc = max(city_%nPeakTrainOcc, nOcc)
      !$omp end master
    end do
    call finishRandBuf()

    !$omp end parallel
    write(*,*) 'run0: test run has been done!'
    write(*,*) '  nPeakTrainOcc = ', city_%nPeakTrainOcc
    close(15)
    city_%time = 0
  end subroutine

  subroutine run1 (recstep, tStop, dir, tag, city_, iseed)
    use Record
!!    !$ use omp_lib

    integer :: omp_get_num_threads, omp_get_thread_num
    integer :: recstep, tStop, nstep, step, i, j, k, iargc, iseed
    character(256) :: tmp
    character(*) :: dir, tag
    type(city), target :: city_
    type(place_t), dimension(:) :: plset_save
    integer, parameter :: os  = 2331
    integer, parameter :: os2 = 2332
    integer, parameter :: os3 = 2333
    integer, parameter :: os4 = 2334
    type(nVis) :: pop(size(city_%area))

    !場所毎訪問者数を記録するもののリスト
    integer, parameter :: mask(8) = &
      (/PL_CRAM,PL_SCH,PL_CORP,PL_HOME, PL_SUPER, PL_PARK, PL_HOSP, PL_TRAIN/)
    !integer, parameter :: mask(7) = &
    !  (/PL_CRAM,PL_SCH,PL_CORP,PL_SUPER,PL_PARK,PL_HOSP,PL_TRAIN/)

    logical :: logPP
    logical :: logTrip

    logPP   = .false. ! logflag
    logTrip = .true.  ! logflag


    ! make city obj visible in schedVisitor().
    city__ => city_

    open(os , file=trim(dir)//"/pop_"//trim(tag)//".csv")
    write(0,*) 'pop_XX.csv is opened'
    write(*,*) 'pop_XX.csv is called'

    if (logPP) then 
      open(os2, file=trim(dir)//"/poppw_"//trim(tag)//".csv")
      write(0,*) 'poppw_XX.csv is opened'
      write(*,*) 'poppw_XX.csv is opend'
    end if

    !!!! the core seems to crash before reaching here !!!!!!!
    ! * open(os) seems to fail.

    if (logTrip) then
      frm_logTrip = logTrip
      frm_osTrip  = os3
      open(os3, file=trim(dir)//"/trip_"//trim(tag)//".csv")
    end if


    call showPopTag(os, 5)
  
    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    ! tentatively omit to write headings
    ! to do: impl `writePlacewisePopSubsetTag`
    
    !if (logPP) call writePlacewisePopTag(os2, city_, mask)
    nstep   = tStop / recstep

    !$omp parallel

    !$omp master
    !$  write(*,*) omp_get_num_threads(), 'threads are running.'
    !$omp end master

!$  frm_rndstat = omp_get_thread_num() + 1 + iseed
!$  if (.false.) then
      frm_rndstat = 1 + iseed
!$  end if
    call init_genrand(frm_rndstat)
    call initRandBuf(iseed)
    !$acc update device(frm_logTrip)

    if (logTrip) call initTripLog()
    
    !city_%time = 1439

    allocate(frm_cntInf(PL_MAX))
    frm_cntInf(:) = 0

    call evalPlace(city_)


    do while (city_%time .lt. tStop)
      call advanceTime2(city_)

      if (modulo(city_%time, recstep) .eq. 0) then
      !$omp master
        write(*,*)  'time = ', dble(city_%time)/1440D0
        pop = reducePop(city_)
        call showPop(os, city_%time, pop)
        !if (logPP) call writePlacewisePop(os2, city_, mask)
        if (logPP) call writePlacewisePopSubset(os2, city_, city_%pl_save_gid)
      !$omp end master
      end if
      if (modulo(city_%time, 50*1440).eq.0) then
      !$omp master
        call writeFullPersonState&
          (trim(dir)//"/per_"//trim(tag)//"_time"//trim(sI(city_%time/1440))//".csv", city_)
      !$omp end master
      end if

      if (logTrip .and. mod(city_%time,60) == 0) then
        call outputTripLog(os3, city_%psumAreaPeople)
      end if
    end do

    if (logTrip) call finishTripLog(os3, city_%psumAreaPeople)
    call finishRandBuf()

    !$omp end parallel
    call writeFullPersonState&
      (trim(dir)//"/per_"//trim(tag)//"_time"//trim(sI(city_%time/1440))//".csv", city_)
    write(*,*) 'Done', city_%time

    close(os)
    if (logPP) close(os2)
    if (logTrip) close(os3)

    open(os4, file=trim(dir)//"/cntinf_"//trim(tag)//".csv")
    do k = 1, PL_MAX
      write(os4,'(a,",",I12)') trim(place_kToString(k)), frm_cntinf(k)
    end do
    close(os4)


    open(os4, file=trim(dir)//"/firstinf_"//trim(tag)//".csv")
    call reduceFirstInfect(os4, city_)

    write(*,*) 'Done', city_%time
  end subroutine


  subroutine readConference(city_, filename, iseed)
    implicit none
    type(city), target :: city_
    character(*) :: filename
    integer :: iseed
    !
    integer, parameter :: is = 5532
    integer :: nConference
    character(256) :: line, kind
    integer :: cnf_id, start_time, end_time, area_t, place_k, pl_id, attendance
    integer :: i
    type(conference), pointer :: cnf
    type(tmcomp) :: tm
    integer :: area_id, person_id, pe_gid, irnd
    type(person), pointer :: p

    open(is, file=filename)
    read(is,*) nConference
    allocate(city_%conference(nConference))
    city_%nConference = nConference
    do i = 1, nConference
      cnf => city_%conference(i)
      read(is,'(a256)') line
      read(line,*) cnf_id, start_time, end_time, area_t, kind, pl_id, attendance
      call fromBase0(cnf_id)
      call fromBase0(area_t)
      place_k = place_kFromString(kind)
      call fromBase0(pl_id)
      cnf%cnf_id = cnf_id
      cnf%start_time = start_time
      cnf%end_time = end_time
      cnf%area_t = area_t
      cnf%place_k = place_k
      cnf%pl_id = pl_id
      cnf%attendance = attendance
      !write(*,*) cnf_id, start_time, end_time, area_t, place_k, pl_id, attendance
      cnf%actual_attendance = 0

      tm = timecomp(start_time)
      cnf%cnf_day = tm%day
      cnf%pl_gid = city_%area(area_t)%place(place_k)%o(pl_id)%pl_gid
      !write(*,*) cnf%cnf_id, cnf%cnf_day, cnf%pl_gid
    end do
    close(is)

    ! **** decide who will join which conferences at random ****
    call init_genrand(iseed)
    do area_id = 1, size(city_%area)
      do person_id = 1, size(city_%area(area_id)%person)
        p => city_%area(area_id)%person(person_id)
        pe_gid = p%pe_gid
        !write(*,*) "area ", area_id, "local id ", person_id, "global id", pe_gid
        do i = 1, nConference
          cnf => city_%conference(i)
          irnd = irndIn(0, 0, gl_n_people)
          if (irnd >= cnf%attendance) cycle
          cnf_id = cnf%cnf_id
          p%nConference = p%nConference + 1
          p%conference(p%nConference) = cnf_id
          !write(*,*) "Person ", pe_gid, "will attend Conference ", cnf_id
          cnf%actual_attendance = cnf%actual_attendance + 1
        end do
      end do
    end do

    do i = 1, nConference
      cnf => city_%conference(i)
      write(*,'(a,i4,a,2i5)') "conference", i, ", attendance ", cnf%attendance, cnf%actual_attendance
    end do
    
  end subroutine readConference


  subroutine readInterv(city_, filename)
    type(city), target :: city_
    character(*) :: filename
    !
    integer, parameter :: is = 5533
    integer :: i, nItems
    integer :: time, area_t, person_id, cnf_id
    character(32) :: kind
    character(256) :: line
    real(8) :: eff, sen
    type(person), pointer :: p
    integer :: pe_gid
    integer :: to_add_cnf, j
    open(is, file=filename)
    read(is,*) nItems
    do i = 1, nItems
      read(is,'(a256)') line
      read(line,*) kind
      select case (kind)
      case ("VAC")
        read(line,*) kind, time, area_t, person_id, eff, sen
        call fromBase0(area_t)
        call fromBase0(person_id)
        if (rndIn(frm_rndstat, 0D0, 1D0) .lt. eff) then
          p => city_%area(area_t)%person(person_id)
          pe_gid = p%pe_gid
          gl_person_nIntervEvt(pe_gid) = gl_person_nIntervEvt(pe_gid) + 1
          gl_person_IntervEvt(pe_gid, gl_person_nIntervEvt(pe_gid)) = intervEvt(time,INTERV_VAC,eff)
          gl_person_nu(pe_gid) = sen
          !write(*,*) 'nu = ', gl_person_nu(pe_gid)
        else
          !write(*,*) 'vacc fail!'
        end if
      case ("INF")
        read(line,*) kind, time, area_t, person_id
        call fromBase0(area_t)
        call fromBase0(person_id)
        p => city_%area(area_t)%person(person_id)
        pe_gid = p%pe_gid
        gl_person_nIntervEvt(pe_gid) = gl_person_nIntervEvt(pe_gid) + 1
        gl_person_IntervEvt(pe_gid, gl_person_nIntervEvt(pe_gid)) = intervEvt(time,INTERV_INF,0.0)
      case ("CNF")
        read(line,*) kind, cnf_id, area_t, person_id
        if (cnf_id >= city_%nConference) then
          write(*,*) 'unknow conference id: ', cnf_id
        else
          call fromBase0(cnf_id)
          call fromBase0(area_t)
          call fromBase0(person_id)
          p => city_%area(area_t)%person(person_id)
          ! check current schedule to avoid double booking
          to_add_cnf = 1
          do j = 1, p%nConference
            if (p%conference(j) == cnf_id) then
              to_add_cnf = 0
            end if
          end do
          if (to_add_cnf > 0) then
            p%nConference = p%nConference + 1
            p%conference(p%nConference) = cnf_id
            pe_gid = p%pe_gid
            !write(*,*) "Conference ", cnf_id, "was added to Person", pe_gid
          end if
        end if
      case default
        write(*,*) 'unknown intervension kind: ', trim(kind)
      end select
    end do
    close(is)
  end subroutine
end module
