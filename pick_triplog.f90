module pick_triplog
  implicit none
  ! place_k型定数 (copy from unreal_frame_t)
  integer, parameter :: PL_CRAM  = 1
  integer, parameter :: PL_SCH   = 2
  integer, parameter :: PL_CORP  = 3
  integer, parameter :: PL_HOME  = 4
  integer, parameter :: PL_SUPER = 5
  integer, parameter :: PL_PARK  = 6
  integer, parameter :: PL_HOSP  = 7
  integer, parameter :: PL_TRAIN = 8
  integer, parameter :: PL_REST  = 9
  integer, parameter :: PL_MAX   = PL_REST

  ! health%kind型定数
  integer, parameter :: HL_NA  = 0 !未使用配列要素
  integer, parameter :: HL_SUS = 1
  integer, parameter :: HL_EXP = 2
  integer, parameter :: HL_INF = 3
  integer, parameter :: HL_VAC = 4
  integer, parameter :: HL_REC = 5 
  integer, parameter :: HL_DIE = 6
 
  !integer, parameter :: nGame = 100, nCafe = 50, nDay = 84
  integer, parameter :: nGame = 100, nCafe = 500, nDay = 84
  integer, parameter :: time = 1, town = 2, person = 3, stat = 4, src_k = 5
  integer, parameter :: src_town = 6, src_pl = 7, dst_k = 8, dst_town = 9, dst_pl = 10
  integer :: nrow
  integer :: nEvtGame(nGame), nEvtCafe(nCafe,nDay)
  integer :: nEvtGameMax, nEvtCafeMax
  integer, allocatable :: evtGame(:,:,:)     ! ncol, nevtgamemax, nGame
  integer, allocatable :: evtCafe(:,:,:,:)   ! ncol, nevtcafemax, nCafe, nDay
  !integer(8), allocatable, dimension(:,:) :: table
contains
  function place_kFromString(str) result(it)
    integer :: it
    character(*) :: str
    select case (str)
    case ("Cram" ) ; it = PL_CRAM
    case ("Sch"  ) ; it = PL_SCH
    case ("Corp" ) ; it = PL_CORP
    case ("Home" ) ; it = PL_HOME
    case ("Super") ; it = PL_SUPER
    case ("Park" ) ; it = PL_PARK
    case ("Hosp" ) ; it = PL_HOSP
    case ("Train"); it = PL_TRAIN
    case ("Rest" ) ; it = PL_REST
    case default; 
      write(*,*) 'error: unknown place_k:', str
      stop
    end select
  end function

  function healthFromString(str) result(it)
    character(*) :: str
    integer :: it
    select case (str)
    case ("SUS"); it = HL_SUS
    case ("EXP"); it = HL_EXP 
    case ("INF"); it = HL_INF
    case ("VAC"); it = HL_VAC
    case ("REC"); it = HL_REC
    case ("DIE"); it = HL_DIE
    case default; 
      write(*,*) 'parse error: unknown health state:', str
      stop
    end select
  end function

  subroutine clear()
    deallocate(evtGame)
    deallocate(evtCafe)
  end subroutine

  integer function load(c_filename,n) BIND(C,name='load_')
    integer :: n
    character(len=1) :: c_filename(n)
    character(len=n) :: filename
    character(16) :: xstat, xsrc_k, xdst_k
    integer :: x(10), i, iday, at, ievt, tmp
    logical :: self
    filename = transfer(c_filename, filename)
    open(1234, file=filename, status='old')

    nEvtGame = 0
    nEvtCafe = 0

    ! ---- first count items to know the size of the arrays
    do i = 0, 2147483647
      read(1234,*,END=999) x(1:3), xstat, xsrc_k, x(6:7), xdst_k, x(9:10)
      !write(*,*) x(1:3), stat, src_k, x(6:7), xdst_k, x(9:10)
      iday = x(time) / 1440

      self = xsrc_k .eq. xdst_k .and. x(src_town) .eq. x(dst_town) .and. x(src_pl) .eq. x(dst_pl)

      if (trim(xsrc_k).eq."Home" .and. x(src_town) .eq. 1 .and. x(src_pl) .le. nGame) &
        nEvtGame(x(src_pl)) = nEvtGame(x(src_pl)) + 1

      if (trim(xdst_k).eq."Home" .and. x(dst_town) .eq. 1 .and. x(dst_pl) .le. nGame .and. .not. self) &
        nEvtGame(x(dst_pl)) = nEvtGame(x(dst_pl)) + 1

      if (trim(xsrc_k).eq."Rest".and. x(src_town) .eq. 1) then
        if (iday .le. nDay .and. x(src_pl) .le. nCafe) then 
          nEvtCafe(x(src_pl),iday) = nEvtCafe(x(src_pl),iday) + 1
        else
          write(*,*) 'day ', iday,  'cafe ', x(src_pl),'ignored!'
        end if
      end if

      if (trim(xdst_k).eq."Rest".and. x(dst_town) .eq. 1 .and. .not. self) then
        if (iday .le. nDay .and. x(dst_pl) .le. nCafe) then
          nEvtCafe(x(dst_pl),iday) = nEvtCafe(x(dst_pl),iday) + 1
        else
          write(*,*) 'day ', iday,  'cafe ', x(dst_pl),'ignored!'
        end if
      end if
    end do
999 continue
    !write(*,*) 'nEvtGames = ', nEvtGame
    !write(*,*) 'nEvtCafe  = ', nEvtCafe

    nEvtGameMax = maxval(nEvtGame)
    nEvtCafeMax = maxval(nEvtCafe)
    write(*,*) 'nEvents = ', i
    write(*,*) 'max(nEvtGames) = ', nEvtGameMax
    write(*,*) 'max(nEvtCafe)  = ', nEvtCafeMax

    if (allocated(evtGame)) then
      write(*,*) 'WARINING: loaded tables are overwritten by new ones.'
      call clear()
    end if

    allocate(evtGame(10,nEvtGameMax,nGame))
    allocate(evtCafe(10,nEvtCafeMax,nCafe,nDay))

    nrow = i
    load = nrow
    
    rewind(1234)
    nEvtGame = 0
    nEvtCafe = 0
    ! ---- now it's time to push items to the arrays ----
    do i = 1, nrow
      read(1234,*,END=999) x(1:3), xstat, xsrc_k, x(6:7), xdst_k, x(9:10)
      x(4) = healthFromString(xstat)
      x(5) = place_kFromString(xsrc_k)
      x(8) = place_kFromString(xdst_k)
      iday = x(time) / 1440

      ! one event may be stored more than once when both src and dst are monitored. 
      ! so you CANNOT write as if-elseif- ..--if but as if-if-..
      self = xsrc_k .eq. xdst_k .and. x(src_town) .eq. x(dst_town) .and. x(src_pl) .eq. x(dst_pl)

      ! layout:  allocate(evtGame(10,nEvtGameMax,nGame))
      if (trim(xsrc_k).eq."Home" .and. x(src_town) .eq. 1 .and. x(src_pl) .le. nGame) then
        nEvtGame(x(src_pl)) = nEvtGame(x(src_pl)) + 1
        evtGame(:, nEvtGame(x(src_pl)), x(src_pl) ) = x
      end if

      if (trim(xdst_k).eq."Home" .and. x(dst_town) .eq. 1 .and. x(dst_pl) .le. nGame.and. .not.self) then
        nEvtGame(x(dst_pl)) = nEvtGame(x(dst_pl)) + 1
        evtGame(:, nEvtGame(x(dst_pl)), x(dst_pl) ) = x
      end if

      ! layout:  allocate(evtCafe(10,nEvtCafeMax,nCafe,nDay))
      if (trim(xsrc_k).eq."Rest".and. x(src_town) .eq. 1) then
        if (iday .le. nDay .and. x(src_pl) .le. nCafe) then 
          at = x(src_pl)
          nEvtCafe(at,iday) = nEvtCafe(at,iday) + 1
          ievt = nEvtCafe(at,iday) 
          evtCafe(:,ievt, at, iday) = x
        else
          write(*,*) 'day ', iday,  'cafe ', x(src_pl),'ignored!'
        end if
      end if

      if (trim(xdst_k).eq."Rest".and. x(dst_town) .eq. 1 .and. .not.self) then
        if (iday .le. nDay .and. x(dst_pl) .le. nCafe) then
          at = x(dst_pl)
          nEvtCafe(at,iday) = nEvtCafe(at,iday) + 1
          ievt = nEvtCafe(at,iday) 
          evtCafe(:,ievt, at, iday) = x
        else
          write(*,*) 'day ', iday,  'cafe ', x(dst_pl),'ignored!'
        end if
      end if
    end do
write(*,*) '----'
    close(1234)
  end function

  !integer, allocatable :: evtGame(:,:,:)     ! ncol, nevtgamemax, nGame
  !integer, allocatable :: evtCafe(:,:,:,:)   ! ncol, nevtcafemax, nCafe, nDay

  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  ! I/F to R 
  subroutine dimEvtGame(dimVec)
    integer :: dimVec(3)
    dimVec = ubound(evtGame)
  end subroutine

  subroutine pickNEvtGame(buf,nGame__)
    integer :: nGame__, buf(nGame__)
    integer :: n
    n = min(nGame__, nGame)
    !write(*,*) n, nEvtGame(1:n)
    buf(1:n) = nEvtGame(1:n)
  end subroutine

  subroutine pickEvtGameAll(buf, d1, d2, d3)
    integer :: d1, d2, d3 ! should be equal to ubound(evtGame)
    integer :: buf(d1,d2,d3)
    buf = evtGame
  end subroutine

  subroutine pickEvtGame(buf, d1, d2, i3)
    integer :: d1, d2, i3 ! should be equal to ubound(evtGame)
    integer :: buf(d1,d2)
    d1 = min(d1,ubound(evtGame,1))
    d2 = min(d2,ubound(evtGame,2))
    buf(1:d1,1:d2) = evtGame(1:d1,1:d2,i3)
  end subroutine 

! -- similarly to game
  subroutine dimEvtCafe(dimVec)
    integer :: dimVec(4)
    dimVec = ubound(evtCafe)
  end subroutine

  subroutine pickNEvtCafe(buf, nCafe__, nDay__)
    integer :: nCafe__, nDay__, buf(nCafe__,nDay__)
    integer :: n, m
    n = min(nCafe__, nCafe)
    m = min(nDay__, nDay)
    buf(1:n,1:m) = nEvtCafe(1:n,1:m)
  end subroutine

  subroutine pickEvtCafe(buf, d1, d2,  i3, i4)
  !                         ncol,nevt,icafe,iday
    integer :: d1, d2, i3, i4 ! should be equal to ubound(evtCafe)
    integer :: buf(d1,d2)
    d1 = min(d1,ubound(evtCafe,1))
    d2 = min(d2,ubound(evtCafe,2))
    !write(*,*) i3, i4
    buf(1:d1,1:d2) = evtCafe(1:d1, 1:d2, i3, i4)
  end subroutine 
end module

program main
  use pick_triplog
  !character(80) :: filename = "trip_pgi_201217-125236_0.csv"
  character(80) :: filename = "trip_pgi_210114-165019_1.csv"
  n = load(filename,len(filename))
end program
