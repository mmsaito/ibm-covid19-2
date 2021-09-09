module Record
  use Frame_t
  use nvtx

  type tripLog_t
    integer :: itime
    integer :: pe_gid
    integer :: hl_kind
    integer :: visit_gid
    integer :: dest_gid
  end type tripLog_t

  type(tripLog_t), allocatable, dimension(:) :: tripLog
  integer, parameter :: n_tripLog = 1024*1024  ! must be even number
  integer :: i_tripLog  ! (0 <= i_tripLog < n_tripLog)
  integer :: h_tripLog  ! (0 or 1)
  !$acc declare create(tripLog)
  !$acc declare create(i_tripLog)
  
contains

  function reducePop(city_) result (it)
    implicit none
    type(city) :: city_
    !
    type(nVis) :: it(size(city_%area))
    integer    :: idxA, pe_gid, hl_kind
    integer, allocatable, dimension(:,:) :: cnt
    allocate(cnt(HL_MAX, size(city_%area)))
    cnt(:,:) = 0
    do idxA = 1, size(city_%area)
      do pe_gid = city_%psumAreaPeople(idxA)+1, city_%psumAreaPeople(idxA+1)
        hl_kind = gl_person_health(pe_gid, 1)%kind
        cnt(hl_kind, idxA) = cnt(hl_kind, idxA) + 1
      end do
      it(idxA)%s = cnt(HL_SUS, idxA)
      it(idxA)%e = cnt(HL_EXP, idxA)
      it(idxA)%i = cnt(HL_INF, idxA)
      it(idxA)%r = cnt(HL_REC, idxA)
      it(idxA)%v = cnt(HL_VAC, idxA)
      it(idxA)%d = cnt(HL_DIE, idxA)
    end do
    deallocate(cnt)
  end function reducePop

  subroutine  showPopTag(os, n)
    integer :: os, n
    write(os,'("t,",$)')
    do i = 1, n
      write(os,'("s,e,i,r,",$)')
    end do
    write(os,*)
  end subroutine

  !場所ごとの人数についての巨大な表をつくる。扱いはあとでゆっくり考える。
  subroutine writePlacewisePopTag(os,city_,active)
    integer :: os
    type(city), target :: city_
    integer, dimension(:) :: active
    type(area), pointer :: area_
    type(place), pointer, dimension(:) :: place_
    integer :: i, j

    write(os,*) "s,e,i,r,d"
    write(os,*)  size(city_%area), ",nAreas"
    do i = 1, size(city_%area)
      write(os,*) size(city_%area(i)%person), ",nPersons"
      do j = 1, size(city_%area(i)%place)
        write(os,*)  activatedNum(j, size(city_%area(i)%place(j)%o)), ",n" // place_kToString(j)
      end do
      if (i.eq.1) then
        write(os,*) activatedNum(PL_TRAIN, size(city_%train)), ",n" // place_kToString(PL_TRAIN)
      else
        write(os,*) 0, "nTrains"
      endif
      ! <<<<
      write(os,*) "--"
    end do
  contains
    integer function activatedNum(kind,n)
      integer :: kind, j
      activatedNum = 0
      do j = 1, size(active)
        if (active(j).eq.kind) activatedNum = n
      end do
    end function
  end subroutine

  subroutine initTripLog()
    allocate(tripLog(0:n_tripLog-1))
    h_tripLog = 0
    i_tripLog = 0
    !$acc update device(i_tripLog)
  end subroutine initTripLog

  subroutine recordTrip(itime, pe_gid, visit_gid, dest_gid)
    !$acc routine
    implicit none
    integer :: itime, pe_gid, hl_kind, visit_gid, dest_gid
    !
    integer :: i

    !$acc atomic capture
    i = i_tripLog
    i_tripLog = i_tripLog + 1
    !$acc end atomic
    i = mod(i, n_tripLog)
    
    tripLog(i)%itime     = itime
    tripLog(i)%pe_gid    = pe_gid
    tripLog(i)%hl_kind   = gl_person_health(pe_gid, 1)%kind
    tripLog(i)%visit_gid = visit_gid
    tripLog(i)%dest_gid  = dest_gid
  end subroutine recordTrip

  subroutine outputTripLog(os, psumAreaPeople)
    integer :: os
    integer :: psumAreaPeople(*)
    !
    integer :: itime, pe_gid, hl_kind, visit_gid, dest_gid
    integer :: i, is, ie
    !$acc wait
    !$acc update self(i_tripLog)
    if (i_tripLog >= n_tripLog) then
      i_tripLog = mod(i_tripLog, n_tripLog)
      !$acc update device(i_tripLog)
    end if
    if (h_tripLog == 2*i_tripLog/n_tripLog) return

    call nvtxStartRange("outputTripLog", __LINE__)
    is = h_tripLog * n_tripLog/2
    ie = is + n_tripLog/2 - 1
    ! write(*,*) "# outputTripLog:", is, ie
    do i = is, ie
      itime     = tripLog(i)%itime
      pe_gid    = tripLog(i)%pe_gid
      hl_kind   = tripLog(i)%hl_kind
      visit_gid = tripLog(i)%visit_gid
      dest_gid  = tripLog(i)%dest_gid
      call writeTrip(os, itime, psumAreaPeople, pe_gid, hl_kind, visit_gid, dest_gid)
    end do
    h_tripLog = 1 - h_tripLog
    call nvtxEndRange
  end subroutine outputTripLog

  subroutine finishTripLog(os, psumAreaPeople)
    integer :: os
    integer :: psumAreaPeople(*)
    !
    integer :: itime, pe_gid, hl_kind, visit_gid, dest_gid
    integer :: i, is, ie
    call outputTripLog(os, psumAreaPeople)
    
    call nvtxStartRange("finishTripLog", __LINE__)
    is = h_tripLog * n_tripLog/2
    ie = i_tripLog - 1
    write(*,*) "# finishTripLog:", is, ie
    do i = is, ie
      itime     = tripLog(i)%itime
      pe_gid    = tripLog(i)%pe_gid
      hl_kind   = tripLog(i)%hl_kind
      visit_gid = tripLog(i)%visit_gid
      dest_gid  = tripLog(i)%dest_gid
      call writeTrip(os, itime, psumAreaPeople, pe_gid, hl_kind, visit_gid, dest_gid)
    end do
    i_tripLog = 0
    h_tripLog = 0
    deallocate(tripLog)
    call nvtxEndRange
  end subroutine finishTripLog
  
  !個人の移動を記録する【注:性能が極端に低下する】。
  subroutine writeTrip(os, itime, psumAreaPeople, pe_gid, hl_kind, visit_gid, dest_gid)
    integer :: os, itime
    integer :: psumAreaPeople(*)
    integer :: pe_gid, visit_gid, dest_gid
    integer :: hl_kind
    !
    integer :: idxA, idxP
    type(place_t) :: visit, dest

    idxA = 1
    do while (pe_gid > psumAreaPeople(idxA+1))
      idxA = idxA + 1
    end do
    idxP = pe_gid - psumAreaPeople(idxA)
    
    visit = gl_place_idx(visit_gid)
    dest  = gl_place_idx(dest_gid)
    if (visit%place_k == PL_TRAIN) visit%area_t = 0
    if (dest %place_k == PL_TRAIN) dest %area_t = 0
    !$omp critical 
    write(os,'(I8,",",  I4,",",I8,",",  a,",  ",a,",",I4,",",I8,",  "a,",",I4,",",I8)')&
      itime,&  !イベント発生時刻
      idxA, idxP,& !対象者番号[街番号,人番号]
      trim(healthToString(hl_kind)),& !健康状態
      trim(place_kToString(visit%place_k)), visit%area_t, visit%id,& !移動元
      trim(place_kToString(dest%place_k)), dest%area_t, dest%id ! 移動先
    !$omp end critical 
  end subroutine
  
  subroutine writeFullPersonState(file,city_)
    character(*)          :: file
    type(city), target    :: city_
    integer, parameter    :: os = 8943
    type(area), pointer   :: area_
    type(person), pointer :: p
    integer :: i, j, hl_kind

    open(os, FILE=file)
    write(os,'(a4,",",a7,",",a3,",",a12,",",a3,",",a1)')&
      "area", "id     ", "age", "role        ", "hea","i"
    do i = 1, size(city_%area)
      area_ => city_%area(i)
      do j = 1, size(area_%person)
        p => area_%person(j)
        hl_kind = gl_person_health(p%pe_gid, 1)%kind
        write(os,'(I4,",",I7,",",I3,",",a12,",",a3,",",L1)')&
          i, j,&
          p%age,&
          roleToString(p%role),&
          healthToString(hl_kind),&
          infected(gl_person_health(p%pe_gid, :), gl_person_nHealth(p%pe_gid))
      end do
    end do
    close(os)

  contains

    logical function infected(hs,n)
      integer :: n, i
      type (health) :: hs(n)
      infected = .false.
      do i = 1, n
        if (hs(i)%kind .eq. HL_EXP) then
          infected = .true.
          return
        end if
      end do
      !【注意】REC かつ not infected な人: 初期感染者またはワクチン接種者
    end function

  end subroutine writeFullPersonState


  function roleToString(i) result(it)
    character(256) :: it
    integer ::i 
    select case (i)
    case (Employed); it = "Employed"
    case (Hausfrau); it = "Hausfrau"
    case (Student ); it = "Student"
    case (Doctor  ); it = "Doctor"
    case (Patient ); it = "Patient"
    case (Visitor) ; it = "Visitor"
    case default
      !write(0,*) 'unreal_rcord.f90:256:unknown role: ', i
      it = "Unkown" // sI(i)
    end select
  end function

  function place_kToString(str) result(it)
    integer :: str
    character(16) :: it
    select case (str)
    case (PL_CRAM) ; it = "Cram"
    case (PL_SCH)  ; it = "Sch"
    case (PL_CORP) ; it = "Corp"
    case (PL_HOME) ; it = "Home"
    case (PL_SUPER); it = "Super"
    case (PL_PARK) ; it = "Park"
    case (PL_HOSP) ; it = "Hosp"
    case (PL_TRAIN); it = "Train"
    case (PL_REST) ; it = "Rest"
    case default; 
      write(0,*) 'error: unknown place_k value:', str
      it = 'PL_'//sI(str)
      !stop
    end select
  end function

  function healthToString(i) result(it)
    character(256) :: it
    integer :: i
    select case (i)
    case (HL_SUS); it = "SUS"
    case (HL_EXP); it = "EXP" 
    case (HL_INF); it = "INF"
    case (HL_VAC); it = "VAC"
    case (HL_REC); it = "REC"
    case (HL_DIE); it = "DIE"
    case default; 
      write(0,*) 'parse error: unknown health state numberl:', i
      stop
    end select
  end function

  subroutine writePlacewisePop(os,city_,active)
    integer :: os
    type(city), target :: city_
    integer, dimension(:) :: active
    type(area), pointer :: area_
    type(place), pointer, dimension(:) :: place_
    integer :: i, k, l, pl_gid, tr_gid
    call pI(os,city_%time);call pS(os,",")
    do i = 1, size(city_%area)
      area_ => city_%area(i)
      do k = 1, size(area_%place)
        if (activated(k)) then
          place_ => area_%place(k)%o
          do l = 1, size(place_)
            pl_gid = place_(l)%pl_gid
            call pI(os, gl_place_nVis(pl_gid, HL_SUS));call pS(os,",")
            call pI(os, gl_place_nVis(pl_gid, HL_EXP));call pS(os,",")
            call pI(os, gl_place_nVis(pl_gid, HL_INF));call pS(os,",")
            call pI(os, gl_place_nVis(pl_gid, HL_REC));call pS(os,",")
            call pI(os, gl_place_nVis(pl_gid, HL_DIE));call pS(os,",")
          end do
        end if
      end do
      ! experimentally, save # of persons in trains.
      if (i.eq.1 .and. activated(PL_TRAIN)) then
        do l = 1, size(city_%train)
          tr_gid = city_%train(l)%tr_gid
          pl_gid = gl_train_plGid(tr_gid)
          call pI(os, gl_place_nVis(pl_gid, HL_SUS));call pS(os,",")
          call pI(os, gl_place_nVis(pl_gid, HL_EXP));call pS(os,",")
          call pI(os, gl_place_nVis(pl_gid, HL_INF));call pS(os,",")
          call pI(os, gl_place_nVis(pl_gid, HL_REC));call pS(os,",")
          call pI(os, gl_place_nVis(pl_gid, HL_DIE));call pS(os,",")
        end do
      end if
    end do
    write(os,*)
  contains
    logical function activated(kind)
      integer :: kind, j
      activated = .false.
      do j = 1, size(active)
        if (active(j).eq.kind) activated = .true.
      end do
    end function
  end subroutine


  subroutine writePlacewisePopSubset(os, city_, plset)
    integer :: os
    type(city), target :: city_
    integer, dimension(:) :: plset
    type(area), pointer :: area_
    integer :: J, i, k, l, pl_gid, tr_gid
    call pI(os,city_%time);call pS(os,",")

    do J = 1, size(plset)
      pl_gid =  plset(J)
      call pI(os, gl_place_nVis(pl_gid, HL_SUS));call pS(os,",")
      call pI(os, gl_place_nVis(pl_gid, HL_EXP));call pS(os,",")
      call pI(os, gl_place_nVis(pl_gid, HL_INF));call pS(os,",")
      call pI(os, gl_place_nVis(pl_gid, HL_REC));call pS(os,",")
      call pI(os, gl_place_nVis(pl_gid, HL_DIE));call pS(os,",")
    end do
    write(os,*)
  end subroutine


  subroutine reduceFirstInfect(os, city_)
    type (city) :: city_
    integer :: os
    integer, parameter :: tBin = 1440
    integer :: i, j, k, idxT
    integer :: idxLast 
    integer :: nPlaces(PL_MAX - 1, size(city_%area), city_%time / tBin + 1)
    integer :: pl_gid
    nPlaces = 0.0

    idxLast = size(nPlaces, 3)

    write(os,'("t,",$)')
    do k = 1, size(city_%area)
      write(os,'("Cram,School,Corp,Home,Super,Park,",$)')
    end do
    write(os,*)

    do i = 1, size(city_%area)
      do j = 1, size(city_%area(i)%place)
        do k = 1, size(city_%area(i)%place(j)%o)
          pl_gid = city_%area(i)%place(j)%o(k)%pl_gid
          if (gl_place_idx(pl_gid)%tVis .ne. 0) then
            idxT = gl_place_idx(pl_gid)%tVis / tBin + 1
            nPlaces(j, i, idxT) = nPlaces(j, i, idxT) + 1
          end if
        end do
      end do
    end do

    do k = 1, idxLast - 1
      write(os,'(I11,",",$)') tBin*k / 1440
      do i = 1, size(nPlaces, 2)
        if (k.gt.1 .and. k.lt.idxLast) &
          nPlaces(:,i,k) = nPlaces(:,i,k) + nPlaces(:,i,k - 1)
        write(os,'(6(I11,","),$)') nPlaces(:,i,k)
      end do
      write(os,*)
    end do

    write(os,'(I11,",",$)') tBin*idxLast / 1440
    do i = 1, size(city_%area)
      do j = 1, size(city_%area(i)%place)
        write(os,'(I11,",",$)') size(city_%area(i)%place(j)%o)
      end do
    end do
    write(os,*)
  end subroutine

  subroutine  showPop(os, t, pop)
    integer    :: os, t
    type(nVis) :: pop(:)
    write(os,'(I8,",",$)') t
    do i = 1, size(pop)
      write(os,'(4(I6,","),$)') pop(i)%s, pop(i)%e, pop(i)%i, pop(i)%r
    end do
    write(os,*)
  end subroutine
end module
