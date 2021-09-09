module Frame_t
  use X_Misc
!!  !$ use omp_lib
  use iso_c_binding
  implicit none

  !固定してもそんなに悪くないパラメータ
  real(8), parameter :: alpha = 1d0/3.5d0
  real(8), parameter :: gamma = 1d0/3.0d0

  integer, parameter :: INT4_MAX =  2147483647
  integer, parameter :: INT4_MIN = -2147483648


  !開発の都合上固定しているもの
  !real(8), parameter :: rateUnsafeInf = 0.5d0      !自重しない感染者の割合
  
  ! R2-9-01: due to make clear the sense of R0 at each place, 
  !   disable self-isolation of infected people ....
  real(8), parameter :: rateUnsafeInf = 1d0      !自重しない感染者の割合


  real(8), parameter :: rateVaccReact = 1d0/14.0d0 !ワクチン効用速度
  !real(8), parameter :: rateVaccReact = 1d0/1.e8 !ワクチン効用速度

  ! 時間刻み
  integer, parameter :: steps_per_day = 1440
  real(8) :: dt = 1d0/dble(steps_per_day)
  !$acc declare copyin(dt)

  ! 時間の単位 (in steps)
  real(8), parameter :: days    = 1440d0
  real(8), parameter :: hours   = 60d0
  real(8), parameter :: minutes = 1d0
  
  integer :: idays   = (days)
  integer :: ihours  = (hours)
  integer :: iminutes= (minutes)

  ! 内部時計の進み幅
  !  - 必要なくなったので、1に固定(parameter属性を付与)している。
  integer, parameter :: itimeStride = 1

  type health
    integer :: kind
    integer :: time
  end type health

  ! health%kind型定数
  integer, parameter :: HL_NA  = 0 !未使用配列要素
  integer, parameter :: HL_SUS = 1
  integer, parameter :: HL_EXP = 2
  integer, parameter :: HL_INF = 3
  integer, parameter :: HL_VAC = 4
  integer, parameter :: HL_REC = 5 
  integer, parameter :: HL_DIE = 6
  integer, parameter :: HL_MAX = HL_DIE

  ! place_k型定数
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

  ! "pseudo" place kind, which cannot be an index for arrays
  integer, parameter :: PL_GAME  = -1   ! a type of conference
  integer, parameter :: PL_ANY   = -256

  type place_t
    sequence
    integer :: pl_gid  ! Global place ID (1 <= gid <= gl_n_places)
    integer :: area_t, place_k, id
    integer :: tVis !使わないかもしれない
  end type

  integer, parameter :: NONE = 0
  integer, parameter :: SOME = 1

  type nVis 
    integer :: s, e, i, r, v, d
  end type
 
  type pTrns 
    real(8) :: s2e, e2i, i2r
  end type

  type place
    integer       :: pl_gid  ! Global place ID (1 <= gid <= gl_n_places)
    type(place_t) :: idx
    integer       :: size  ! Not used?
    real(8)       :: betaN
    real(8)       :: epsilon
  end type

  integer, parameter :: MOB_SCHED_MAX = 10
  type mob_sched
    integer :: i  ! (0 <= i <= n)
    integer :: n  ! (0 <= n <= MOB_SCHED_MAX)
    integer, dimension(MOB_SCHED_MAX) :: time
    integer, dimension(MOB_SCHED_MAX) :: area_t
  end type

  integer, parameter :: SCHED_MAX = 20
  type sched
    sequence
    integer :: n
    integer, dimension(SCHED_MAX) :: time
    integer, dimension(SCHED_MAX) :: pl_gid
  end type

  type mobil
    integer         :: tr_gid  ! Global train ID (1 <= tr_gid <= gl_n_trains)
    type(place)     :: _pl
    type(mob_sched) :: _sked
    !integer         :: iSked
    !logical         :: onService
  end type

  !person%role 定数
  integer, parameter :: Employed = 1
  integer, parameter :: Hausfrau = 2
  integer, parameter :: Student  = 3
  integer, parameter :: Patient  = 4
  integer, parameter :: Doctor   = 5
  integer, parameter :: Visitor  = 6

  type intervEvt
    integer :: time
    integer :: kind ! いまのところvactinationだけなので無視
    real(8) :: eff
  end type
  ! intervEvt%kind 定数
  integer, parameter :: INTERV_INF = 1
  integer, parameter :: INTERV_VAC = 2
  integer, parameter :: INTERV_CNF = 3

  type person
    integer :: pe_gid  ! Global person ID (1 <= gid <= gl_n_people)
    integer :: age, gender, role
    type(place_t), allocatable, dimension(:) :: belong
    type(health) :: health  ! initial health kind
    integer(C_INTPTR_T) :: mkSched
    logical :: isUnsafeINF
    ! ワクチンによる減感作 
    !logical :: hyposensitized !ワクチンが発効したか
    !real(8) :: nu             !(個体)減感作率 ∈ [0,1)
    !シミュレーション中にこの個体が被ることになる介入操作
    !type(intervEvt) :: intervEvt(10)
    !integer :: nIntervEvt
    integer :: conference(10)
    integer :: nConference
  end type

  type place_vector
    type(place), allocatable, dimension(:) :: o
  end type

  type area
    integer :: id
    type(person), allocatable, dimension(:) :: person
    type(place_vector), allocatable, dimension(:) :: place
  end type

  type conference
    ! data to be read from input file (e.g.: "0,600,900,0,Corp,0,300")
    integer :: cnf_id, start_time, end_time
    integer :: area_t, place_k, pl_id, attendance, actual_attendance
    !
    integer :: cnf_day  ! day of conference
    integer :: pl_gid  ! global place ID
  end type conference

  type idxval
    real(8) :: idx
    real(8) :: val
  end type
  
  type rest
    integer, allocatable, dimension(:)      :: nRest
    real                                    :: alpha
    type(idxval), allocatable, dimension(:) :: betaNRestSet
    type(idxval), allocatable, dimension(:) :: eFoldingTimeSet
    type(idxval), allocatable, dimension(:) :: dropRestFreqSet
    type(idxval), allocatable, dimension(:) :: dropRestFreqSet2
  end type
  
  type city
    type(area) , allocatable, dimension(:) :: area
    type(mobil), allocatable, dimension(:) :: train
    integer                                :: time
    integer                                :: nPeakTrainOcc
    integer, allocatable, dimension(:) :: psumAreaPeople
    type(conference), allocatable, dimension(:) :: conference
    integer :: nConference
    type(rest) :: rest
    integer, allocatable, dimension(:) :: pl_save_gid
  end type

  type tmcomp
    integer :: day
    integer :: weekday
    real(8) :: hour
    integer :: step  
  end type
   
  !state variable for random number generator 
  integer :: frm_rndstat
  !$omp threadprivate(frm_rndstat)


  !flags for output control
  logical :: frm_logTrip = .false. !output person trip
  integer :: frm_osTrip = 0
  !$acc declare create(frm_logTrip)

  !transmission count
  integer, allocatable, dimension(:) :: frm_cntInf
  !$acc declare create(frm_cntInf)

  integer, parameter :: PTRNS_S2E = 1
  integer, parameter :: PTRNS_E2I = 2
  integer, parameter :: PTRNS_I2R = 3
  integer, parameter :: PTRNS_S2E_PL = 4
  integer, parameter :: PTRNS_MAX = PTRNS_S2E_PL

  ! Person
  integer :: gl_n_people  ! Total number of people
  type(sched),     allocatable, dimension(:)   :: gl_person_sched
  integer,         allocatable, dimension(:)   :: gl_person_visit_gid
  integer,         allocatable, dimension(:)   :: gl_person_dest_gid
  integer,         allocatable, dimension(:)   :: gl_person_dest_kind
  integer,         allocatable, dimension(:)   :: gl_person_nHealth
  type(health),    allocatable, dimension(:,:) :: gl_person_health  ! (gl_n_people, HL_MAX)
  logical,         allocatable, dimension(:)   :: gl_person_hyposensitized  !ワクチンが発効したか
  real(8),         allocatable, dimension(:)   :: gl_person_nu              !(個体)減感作率 ∈ [0,1)
  integer,         allocatable, dimension(:)   :: gl_person_age
  integer,         allocatable, dimension(:)   :: gl_person_nIntervEvt
  type(intervEvt), allocatable, dimension(:,:) :: gl_person_intervEvt  ! (gl_n_people, 10)
  real(8),         allocatable, dimension(:)   :: gl_person_dropRestFreq
  real(8),         allocatable, dimension(:)   :: gl_person_dropRestFreq2
  !$acc declare create(gl_person_sched)
  !$acc declare create(gl_person_visit_gid)
  !$acc declare create(gl_person_dest_gid)
  !$acc declare create(gl_person_dest_kind)
  !$acc declare create(gl_person_nHealth)
  !$acc declare create(gl_person_health)
  !$acc declare create(gl_person_hyposensitized)
  !$acc declare create(gl_person_nu)
  !$acc declare create(gl_person_age)
  !$acc declare create(gl_person_nIntervEvt)
  !$acc declare create(gl_person_intervEvt)
  !$acc declare create(gl_person_dropRestFreq)
  !$acc declare create(gl_person_dropRestFreq2)
  
  integer, parameter :: PARAMS_BETAN = 1
  integer, parameter :: PARAMS_EPSILON = 2
  integer, parameter :: PARAMS_MAX = PARAMS_EPSILON

  ! Place
  integer :: gl_n_places  ! Total number of places
  real(8) :: gl_place_alpha  ! (*) equal to 'city%rest%alpha'
  integer,       allocatable, dimension(:,:) :: gl_place_nVis   ! (gl_n_place, HL_MAX)
  integer,       allocatable, dimension(:,:) :: gl_place_nVis2  ! debug
  real(8),       allocatable, dimension(:,:) :: gl_place_pTrns  ! (gl_n_place, PTRNS_MAX)
  real(8),       allocatable, dimension(:,:) :: gl_place_params
  type(place_t), allocatable, dimension(:)   :: gl_place_idx
  integer,       allocatable, dimension(:)   :: gl_place_updated
  !$acc declare create(gl_place_alpha)
  !$acc declare create(gl_place_nVis)
  !$acc declare create(gl_place_pTrns)
  !$acc declare create(gl_place_params)
  !$acc declare create(gl_place_idx)
  !$acc declare create(gl_place_updated)

  ! Train
  integer :: gl_n_trains  ! Total number of trains
  integer,         allocatable, dimension(:) :: gl_train_plGid
  logical,         allocatable, dimension(:) :: gl_train_onService
  type(mob_sched), allocatable, dimension(:) :: gl_train_sked
  !$acc declare create(gl_train_plGid)
  !$acc declare create(gl_train_onService)
  !$acc declare create(gl_train_sked)

  integer :: nOnServiceTrains
  integer, allocatable, dimension(:) :: onServiceTrainIds
  !$acc declare create(nOnServiceTrains)
  !$acc declare create(onServiceTrainIds)
  
end module Frame_t
