module AA
  integer, parameter :: steps_per_day = 1440
  real(8) :: dt = 1d0/dble(steps_per_day)
  !$acc declare copyin(dt)
end module AA

program hoge
  use AA
  write(*,*) dt, 1d0/dt
end program
