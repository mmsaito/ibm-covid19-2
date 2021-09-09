module Hook
  use Frame_t
contains
  subroutine HookTest()
    write(*,*) 'hooktest: (default)'
  end subroutine
  subroutine HookCheckCity(city,plset_save)
    type(city) :: city
    type(place_t), allocatable, dimension(:) :: plset_save
    allocate(plset_save(0))
    write(*,*) 'hookcheckcity: (default)'
  end subroutine
end module
