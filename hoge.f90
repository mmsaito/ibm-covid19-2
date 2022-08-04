module AA
  integer, parameter :: steps_per_day = 1440
  real(8) :: dt = 1d0/dble(steps_per_day)
  !$acc declare copyin(dt)
contains
  subroutine func_with_opt(x,opt)
    integer :: x
    integer, optional, value :: opt
    if (.not.present(opt)) opt = 235
    write(*,*) x, opt
  end subroutine
end module AA

program hoge
  use AA
  call func_with_opt(1,opt=2)
  call func_with_opt(1)
end program
