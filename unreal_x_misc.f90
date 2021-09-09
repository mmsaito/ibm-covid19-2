module X_Misc
  use mt19937ar
  implicit none
contains
! 乱数
  real(8) function rndIn (seed,x,y)
    integer :: seed
    real(8) :: x, y, r
    !seed  = ieor(ishft(seed,-1), iand(-iand(seed,1), z'd0000001'))
    !r = abs(dble(seed) / 2147483648D0)
    r = genrand_real2()
    rndIn = x + r *  (y - x)  
  end function
  integer function irndIn (seed,i, j)
    integer :: seed
    integer :: i, j
    irndIn = int(rndIn(seed,dble(i),dble(j)))
  end function

!すきまのない数字の印字
  function sI(num)
    character(len=256) :: sI
    integer :: i, num
    write(sI,'(I12)') num
    do i = 1, len(sI)
      if (sI(i:i) .ne. ' ') exit
    end do
    sI = sI(i:)
  end function

  function sH(num)
    character(len=256) :: sH
    integer :: i, num
    write(sH,'(Z8)') num
    do i = 1, len(sH)
      if (sH(i:i) .ne. ' ') exit
    end do
    sH = sH(i:)
  end function

  ! printing a real number without redundant spaces and zeros.
  function sR(x)
    character(LEN=24) :: sR
    integer :: i, j, k, m
    real(8) :: x
    write(sR,'(g23.15)') x  !g24.16 generates a garbage sometime.

    do k = 1,len(sR)
      if (sR(k:k).eq.'E') exit
    end do

    do i = 1, len(sR)
      if (sR(i:i) .ne. ' ') exit
    end do
    do j = k-1,1,-1
      if (sR(j:j) .ne. ' ' .and. sR(j:j) .ne. '0') exit
    end do
    if (sR(j:j).eq.'.') j=j-1

    do m=len(sR),k+1,-1
      if (sR(m:m) .ne. ' ') exit
    end do

    sR = sR(i:j) // sR(k:m)
  end function

  subroutine pS(os,s)
    integer :: length, os
    character(*) :: s
    character(16) :: sLength, fmt
    length = len_trim(s)
    if (length .ge. 1) then
      sLength = sI(length)
      fmt = "(" // "a" // trim(sLength) // ",$)"
      write(os,fmt) s(1:length)
    end if
  end subroutine

  subroutine pS_nontrim(os,s)
    integer :: length, os
    character(*) :: s
    character(16) :: sLength, fmt
    length = len(s)
    if (length .ge. 1) then
      sLength = sI(length)
      fmt = "(" // "a" // trim(sLength) // ",$)"
      write(os,fmt) s(1:length)
    end if
  end subroutine

  subroutine pI(os,i)
    integer :: os, i
    call pS(os,sI(i))
  end subroutine

  subroutine pR(os,x)
    integer :: os
    real(8) :: x
    call pS(os,sR(x))
  end subroutine
end module
