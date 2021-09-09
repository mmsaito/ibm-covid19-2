subroutine swapSched(sch, i, j)
  use Frame_t
  implicit none
  type(sched) :: sch
  integer     :: i, j
  !
  integer :: tmp_time
  integer :: tmp_pl_gid

  tmp_time      = sch%time  (i)
  tmp_pl_gid    = sch%pl_gid(i)
  sch%time  (i) = sch%time  (j)
  sch%pl_gid(i) = sch%pl_gid(j)
  sch%time  (j) = tmp_time
  sch%pl_gid(j) = tmp_pl_gid
end subroutine swapSched


function hasConferenceToday(city_, p)
  use Frame_t
  use Model1
  implicit none
  type(city)   :: city_
  type(person) :: p
  integer      :: hasConferenceToday
  !
  type(tmcomp) :: tm
  integer :: i, cnf_id

  hasConferenceToday = 0
  tm = timecomp(city_%time)
  do i = 1, p%nConference
    cnf_id = p%conference(i)
    if (city_%conference(cnf_id)%cnf_day == tm%day) then
      hasConferenceToday = 1
      !write(*,*) "Person", p%pe_gid, "has the conference", cnf_id, "at day", tm%day
    end if
  end do
end function hasConferenceToday


subroutine mkSched(city_, p, s)
  use Frame_t
  use Model1
  implicit none
  type(city)    :: city_
  type(person)  :: p  ! person
  integer       :: s  ! seed
  !
  integer :: hasConferenceToday
  integer :: i, j, pe_gid, n, pl_gid, time
  integer :: pl_gid_c, pl_gid_h, time_h
  integer :: i_area, n_rest, i_rest

  real(8) :: dropRestFreq
  
  pe_gid = p%pe_gid

  if (hasConferenceToday(city_, p)) then
    call schedConference(city_, p, gl_person_sched(pe_gid), s)
  else
    select case(p%role)
    case (Employed); call schedEmp (p, city_%time, gl_person_sched(pe_gid), s)
    case (Hausfrau); call schedHaus(p, city_%time, gl_person_sched(pe_gid), s)
    case (Student) ; call schedStu (p, city_%time, gl_person_sched(pe_gid), s)
    case (Patient) ; call schedNul (p, city_%time, gl_person_sched(pe_gid), s)
    case (Doctor)  ; call schedDoc (p, city_%time, gl_person_sched(pe_gid), s)
    case (Visitor) ; call schedVisitor(p, city_%time, gl_person_sched(pe_gid), s)
    end select
  end if

  ! Drop by a restaurant nearby.
  n = gl_person_sched(pe_gid)%n
  if ( n >= 2 ) then
    pl_gid_c = gl_person_sched(pe_gid)%pl_gid(n-1)  ! CORP
    pl_gid_h = gl_person_sched(pe_gid)%pl_gid(n)    ! HOME
    time_h = gl_person_sched(pe_gid)%time(n)

    dropRestFreq = 0d0
    if ( gl_place_idx(pl_gid_c)%place_k == PL_CORP ) dropRestFreq = gl_person_dropRestFreq(pe_gid) 
    if ( gl_place_idx(pl_gid_c)%place_k == PL_HOME ) dropRestFreq = gl_person_dropRestFreq2(pe_gid) 

    if ( gl_place_idx(pl_gid_h)%place_k == PL_HOME .and. &
         mod(time_h, 24*60) > 11*60 .and. mod(time_h, 24*60) < 20*60 + 30) then
      if (rndIn(s, 0.0D0, 1.0D0) < dropRestFreq) then
        i_area = gl_place_idx(pl_gid_c)%area_t
        n_rest = size(city_%area(i_area)%place(PL_REST)%o)

        i_rest = irndIn(s, 1, n_rest)
        gl_person_sched(pe_gid)%pl_gid(n) = &
          city_%area(i_area)%place(PL_REST)%o(i_rest)%pl_gid  ! REST
        n = n + 1
        time_h = time_h + irndIn(s, 120, 150)  ! 2.0 -- 2.5 hours
        gl_person_sched(pe_gid)%time(n) = time_h
        gl_person_sched(pe_gid)%pl_gid(n) = pl_gid_h  ! HOME
        gl_person_sched(pe_gid)%n = n

        ! After-party (so-called Niji-kai)
        if (rndIn(s, 0.0D0, 1.0D0) < 0.5 .and. mod(time_h, 24*60) < 21*60 + 30) then
          i_rest = irndIn(s, 1, n_rest)
          gl_person_sched(pe_gid)%pl_gid(n) = &
            city_%area(i_area)%place(PL_REST)%o(i_rest)%pl_gid  ! REST(2)
          n = n + 1
          time_h = time_h + irndIn(s, 90, 120)  ! 1.5 -- 2.0 hours
          gl_person_sched(pe_gid)%time(n) = time_h
          gl_person_sched(pe_gid)%pl_gid(n) = pl_gid_h  ! HOME
          gl_person_sched(pe_gid)%n = n
        end if
        
        ! write(*,'(a,i8,a,i2)') "pe_gid", pe_gid, " drops by restaurant(s) at area", i_area
        ! do i = 1, gl_person_sched(pe_gid)%n
        !   pl_gid = gl_person_sched(pe_gid)%pl_gid(i)
        !   time = gl_person_sched(pe_gid)%time(i)
        !   time = mod(time, 24*60)
        !   write(*,'(2i3,3i6)') time/60, mod(time, 60), &
        !     gl_place_idx(pl_gid)%area_t, gl_place_idx(pl_gid)%place_k, gl_place_idx(pl_gid)%id
        ! end do
      end if
    end if
  end if

  ! Reverse the order of the schedule
  i = 1
  j = gl_person_sched(pe_gid)%n
  do while (i < j)
    call swapSched(gl_person_sched(pe_gid), i, j)
    i = i + 1
    j = j - 1
  end do
  
end subroutine mkSched
