!実験スクリプト(共有メモリ版)その1
program JCL1
  use Frame
  use Model1
  type(city),target :: city_
  integer :: seed(256), t0, t1, uptoDate
  integer, external :: time, utime
  character(256) :: f_city, f_conference, f_interv, tag, tmp, f_cafe
  integer :: iseed, iarg
  type(place_t), allocatable, dimension(:) :: plset_save
  logical :: logPP = .false., logTrip = .false.

!  call hookTest()

  !write(*,*) 'rateUnsafeInf = ', rateUnsafeInf
  if (iargc() .lt. 4) then
    write(*,*) 'usage: [this-command] <file-city> <file-interv> <tag> [<seed>]'
    stop
  end if

  call getarg(1,f_city)
  call getarg(2,f_conference)
  call getarg(3,f_interv)
  call getarg(4,tag)

  if (iargc().ge.5) then 
    call getarg(5,tmp); read(tmp,*) iseed
  else
    iseed = 0
  end if

  if (iargc().ge.6) then 
    call getarg(6,tmp); read(tmp,*) uptoDate
    write(*,*) '$6: simulation duration: ',uptoDate,'days'
  else
    uptoDate = 14
  end if

  if (iargc().ge.7) then 
    call getarg(7,f_cafe)
    write(*,*) '$7: extra config file of restraunt:', f_cafe
  else
    f_cafe = ''
  end if

  ! the eight-th and later arguments may come in out of order.
  iarg = 8
  do while (iarg .le. iargc())
    call getarg(iarg, tmp)    
    select case (tmp)
    case ('-logPP')
      logPP = .true.
      write(*,*) 'logPP is activated.'
    case ('-logTrip')
      logTrip = .true.
      write(*,*) 'logTrip is activated.'
    end select
    iarg = iarg + 1
  end do 

  call readCity(city_, f_city, f_cafe)
  call simpleReadCheck(city_)

  call readConference(city_, f_conference, iseed)
  
  call recoverParameters(city_)

  t0 = utime()
  call run0(city_, iseed)

  write(*,*) '*******************1'
  call readInterv(city_, f_interv)
  !stop
  write(*,*) '*******************2'

  !argument format:
  !  subroutine run1 (recstep, tStop, dir, tag, city_, iseed )
  !call run1(180, 1440*180, "./", tag, city_, iseed)
  !call run1(180, 1440*28, "./", tag, city_, iseed)
  !call run1(180, 1440*7, "./", tag, city_, iseed)
  call run1(60, 1440*uptoDate, "./", tag, city_, iseed, logPP, logTrip)
  t1 = utime()
  write(*,*) dble(t1 - t0) / 1000d0, "[sec]"

  call recoverParameters(city_)
end program
