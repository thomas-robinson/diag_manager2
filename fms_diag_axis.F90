module fms_diag_axis_mod

use fms_diag_data_mod, only: diag_error, fatal, note, warning
use fms2_io


type domain1d
 integer :: filler
end type domain1d
type domain2d
 integer :: filler
end type domain2d
type domainUG
 integer :: filler
end type domainUG

type diag_axis_type
     integer :: id       !< The axis ID
     integer :: start    !< The starting value of the axis
     integer :: ending   !< The ending value of the axis
     integer :: ls       !< The local starting value
     integer :: le       !< the local ending value
     integer :: tile     !< The tile count
     character (len=:), allocatable  :: aname !< The name of the axis
     character (len=:), allocatable  :: units !< The units of the axis
     character (len=1) :: cart !< The "cartesian" axis name
     character (len=:), allocatable :: longname !< The longname of the axis
     integer :: alen !< The length of the axis
     integer :: direction !< The direction of the axis
     character(len=:), allocatable :: set_name !< Set name of the axis
     integer :: set !< set number?
     TYPE(domain1d) :: Domain !<Axis domain
     TYPE(domain2d) :: Domain2 !< Axis domain if 2D
     TYPE(domainUG) :: DomainU !< Axis domain if unstructured
     class(*), allocatable, dimension (:) :: adata !< The axis data
     character (len=:), dimension(:), allocatable :: attributes !< The axis metadata
     logical, allocatable :: initialized

end type diag_axis_type


integer :: UP = 1
integer :: DOWN = -1
integer :: HORIZONTAL = 0
integer :: VOID_AXIS = -999
integer, allocatable :: diag_axis_id_list (:) !< A list of potential axis IDs
integer, allocatable :: diag_axis_id_used (:) !< A list of used axis IDs


! counter of number of axes defined
integer, dimension(:), allocatable :: num_subaxes
integer :: num_def_axes = 0

! storage for axis set names
 character(len=128), dimension(:), allocatable, save :: Axis_sets
 integer :: num_axis_sets = 0

! ---- global storage for all defined axes ----
type(diag_axis_type), allocatable, save :: Axes(:)

! ---- temporary arrays for increasing size ----
character(len=128), allocatable :: Axis_sets_temp(:)
type(diag_axis_type), allocatable :: Axes_temp(:)

public :: diag_axis_type
public :: UP, DOWN, VOID_AXIS, HORIZONTAL

contains

type(diag_axis_type) function fms_diag_axis_init (axis, aname, adata, units, cart, long_name, direction,&
       & set_name, edges, Domain, Domain2, DomainU, aux, req, tile_count, start, ending, attributes)
    type(diag_axis_type), intent(inout)      :: axis !< The axis object
    character(len=*), intent(in)             :: aname !< The name of the axis
    class(*), target, intent(in), dimension(:)    :: adata !< The axis data
    class(*),pointer,        dimension(:)    :: dptr=> NULL() !< A pointer to the data
    character(len=*), intent(in)             :: units !< The axis units
    character(len=*), intent(in)             :: cart !< The cartesian name of the axis 
    character(len=*), intent(in), optional   :: long_name !< Axis long name
    character(len=*), intent(in), optional   :: set_name !< Axis set name?
    integer,          intent(in), optional   :: direction !< Axis direction
    integer,          intent(in), optional   :: edges !< The axis edges
    type(domain1d),   intent(in), optional   :: Domain !<Axis domain
    type(domain2d),   intent(in), optional   :: Domain2 !< Axis domain if 2D
    type(domainUG),   intent(in), optional   :: DomainU !< Axis domain if unstructured
    character(len=*), intent(in), optional   :: aux !< ???
    character(len=*), intent(in), optional   :: req !< Is it required???
    integer,          intent(in), optional   :: tile_count !< The tile count
    integer,          intent(in), optional   :: start !< The starting value of the axis
    integer,          intent(in), optional   :: ending !< The ending value of the axis
    character(len=20), intent(in), dimension(:), optional :: attributes !< attributes?

    type(domain1d) :: domain_x, domain_y
    integer :: ierr, axlen
    integer :: i, set, tile
    integer :: isc, iec, isg, ieg
    character(len=128) :: emsg

    integer :: ls       !< The local starting value
    integer :: le       !< the local ending value

    !Cartesian name error check
    if (len(cart) > 1) call diag_error("fms_diag_axis_init","CARTNAME for "//trim(aname)//&
    " must only be one letter.  You have "//trim(cart),FATAL)
    if (cart .ne. "X" .or. cart .ne. "Y" .or. cart .ne. "Z" .or. &
        cart .ne. "N" .or. cart .ne. "U" .or. cart .ne. "T") &
        call diag_error("fms_diag_axis_init","CARTNAME for "//trim(aname)//" can only be X "//&
        "Y Z U or N.  You have "//trim(cart), FATAL)

    ! Allocate the axes
    if (.not. allocated(Axis_sets)) then 
        allocate(Axis_sets(0:1))
    endif
    if (.not. allocated(Axes)) then 
        allocate(Axes(0:1))
    endif

!---- is there an axis set? ----
    if ( present(set_name) ) then
       set = get_axis_set_num (set_name)
       !Increase size of Axis_sets array by 1
       allocate(Axis_sets_temp(0:size(Axis_sets) + 1))
       Axis_sets_temp(0:size(Axis_sets)) = Axis_sets(0:size(Axis_sets))
       call move_alloc(Axis_sets_temp, Axis_sets)
       !---- add new set name ----
       if (set == 0) then
          num_axis_sets = num_axis_sets + 1
          set = num_axis_sets
          Axis_sets(set) = set_name
       end if
    else
       set = 0
    end if


    !---- see if axis already exists --
    ! if this is time axis, return the ID of a previously defined
    ! if this is spatial axis, FATAL error
    do i = 1, num_def_axes
       if ( trim(aname) == Axes(i)%aname ) then
          if ( trim(aname) == 'Stations' .or. trim(aname) == 'Levels') THEN
             fms_diag_axis_init = Axes(i)
             return
          else if ( set == Axes(i)%set) then
             if ( trim(aname) == 'time' .or.&
                  & trim(cart) == 't' .or.&
                  & trim(aname) == 'nv' .or.&
                  & trim(cart) == 'n' ) then
                fms_diag_axis_init = Axes(i)
                return
             else if ( (cart /= 'x' .and. cart /= 'y')&
                  & .or. tile /= Axes(i)%tile ) then
                ! <ERROR STATUS="FATAL">axis_name <NAME> and axis_set already exist.</ERROR>
                call diag_error('diag_axis_mod::diag_axis_init',&
                     & 'axis_name '//trim(aname)//' and axis_set already exist.', FATAL)
           end if
         end if
      end if
    end do

    !---- register axis ----
    num_def_axes = num_def_axes + 1

    !Increase size of Axes array by 1
    allocate(Axes_temp(0:size(Axes) + 1))
    Axes_temp(0:size(Axes)) = Axes(0:size(Axes))
    call move_alloc(Axes_temp, Axes)

    !---- allocate storage for coordinate values of axis ----
    if ( axis%cart == 'T' ) then
       axlen = 0
    else
       axlen = size(adata(:))
    end if
    !allocate(axis%adata(0:axlen))

     axis%aname = trim(aname)
     axis%adata = adata(1:axlen)
     axis%units = units
     axis%alen = axlen
     axis%cart = cart
     axis%set = set
     if (present(set_name)) then 
          axis%set_name = set_name
     else 
          axis%set_name = ' '
     end if
     if (present(start)) then 
          axis%start = start
     else 
          axis%start = 1
     end if
     if (present(ending)) then 
          axis%ending = ending
     else 
          axis%ending = 1
     end if
     if (present(long_name)) then 
          axis%longname = trim(long_name)
     else
          axis%longname = trim(aname)
     end if
     if (present(direction)) then 
          axis%direction = direction
     else 
          axis%direction = HORIZONTAL
     end if
     if (present(tile_count)) then
         axis%tile = tile_count
     else 
         axis%tile = 1
     end if
     if (present(attributes)) then 
          allocate(character(len=20) :: axis%attributes (size(attributes)))
          do i = 1,size(attributes)
               axis%attributes(i) =  attributes(i)
          end do
     end if

     !---- Handle the DomainU check
    if (present(DomainU) .and. (present(Domain2) .or. present(Domain)) ) then
       ! <ERROR STATUS="FATAL">Presence of DomainU and another Domain at the same time is prohibited</ERROR>
       call diag_error('diag_axis_mod::diag_axis_init',&
            & 'Presence of DomainU and another Domain at the same time is prohibited', FATAL)
    !---- domain2d type ----
    else if ( present(Domain2) .and. present(Domain)) then
       ! <ERROR STATUS="FATAL">Presence of both Domain and Domain2 at the same time is prohibited</ERROR>
       call diag_error('diag_axis_mod::diag_axis_init',&
            & 'Presence of both Domain and Domain2 at the same time is prohibited', FATAL)
    else if ( present(Domain2) .or. present(Domain)) then
       if ( axis%cart /= 'X' .and. axis%cart /= 'Y') then
          ! <ERROR STATUS="FATAL">Domain must not be present for an axis which is not in the X or Y direction.</ERROR>
          call diag_error('diag_axis_mod::diag_axis_init',&
               & 'A Structured Domain must not be present for an axis which is not in the X or Y direction', FATAL)
       end if
    else if (present(DomainU) .and. axis%cart /= 'U') then
          call diag_error('diag_axis_mod::diag_axis_init',&
               & 'In the unstructured domain, the axis cart_name must be U', FATAL)
    end if


    if ( present(Domain2) ) then
       axis%Domain2 = Domain2
       !call mpp_get_domain_components(Domain2, domain_x, domain_y, tile_count=tile_count)
       if ( axis%cart == 'X' ) then
          axis%Domain = domain_x
       end if      
       if ( axis%cart == 'Y' ) then
          axis%Domain = domain_y
          !axis%DomainU = null_domainU 
    else if ( present(Domain)) then
       !---- domain1d type ----
       !axis%Domain2 = null_domain2 ! needed since not 2-D domain
       axis%Domain = Domain
       !axis%DomainU = null_domainU
    else if (present(DomainU)) then
       !axis%Domain2 = null_domain2
       !axis%Domain = null_domain
       axis%DomainU = DomainU
    else
       !axis%Domain2 = null_domain2
       !axis%Domain = null_domain1
       !axis%DomainU = null_domainU
    end if
  end if

     axis%initialized = .true.

     Axes(num_def_axes) = axis
end function fms_diag_axis_init

  

integer function get_axis_set_num(set_name)
    character(len=*), intent(in) :: set_name

    integer :: iset

    get_axis_set_num = 0
    do iset = 1, num_axis_sets
       if ( set_name == Axis_sets(iset) ) then
          get_axis_set_num = iset
          return
       end if
    end do
end function get_axis_set_num

end module fms_diag_axis_mod
