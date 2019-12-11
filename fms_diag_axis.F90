module fms_diag_axis_mod

use fms_diag_data_mod :: only, diag_error
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

public :: diag_axis_type
public :: UP, DOWN, VOID_AXIS, HORIZONTAL

contains

type(diag_axis_type) function fms_diag_axis_init (axis, aname, adata, units, cart, long_name, direction,&
       & set_name, edges, Domain, Domain2, DomainU, aux, req, tile_count)
    type(diag_axis_type), intent(inout)      :: axis !< The axis object
    character(len=*), intent(in)             :: aname !< The name of the axis
    class(*), target, intent(in), dimension(:)    :: adata !< The axis data
    class(*), pointer,          , dimension(:)    :: dptr=> NULL() !< A pointer to the data
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

    type(domain1d) :: domain_x, domain_y
    integer :: ierr, axlen
    integer :: i, set, tile
    integer :: isc, iec, isg, ieg
    character(len=128) :: emsg

    integer :: ls       !< The local starting value
    integer :: le       !< the local ending value

    if (len(cart) > 1) call diag_error("fms_diag_axis_init","CARTNAME for "//trim(aname)//&
    " must only be one letter.  You have "//trim(cart),FATAL)
    if (cart .ne. "X" .or. cart .ne. "Y" .or. cart .ne. "Z" .or. &
        cart .ne. "N" .or. cart .ne. "U" .or. cart .ne. "T") &
         call diag_error("fms_diag_axis_init","CARTNAME for "//trim(aname)//" can only be X "//&
         "Y Z U or N.  You have "//trim(cart), FATAL)

    ! Allocate the axes
    if (.not. allocated(Axis_sets)) allocate(Axis_sets(0:1))
    if (.not. allocated(Axes)) allocate(Axes(0:1))
    end if

!---- is there an axis set? ----
    if ( present(set_name) ) then
       set = get_axis_set_num (set_name)
       !Increase size of Axis_sets array by 1
       integer, allocatable :: tmp(:)
       allocate(tmp(0:size(Axis_sets))
       tmp(0:size(Axis_sets)) = Axis_sets(0:size(Axis_sets))
       call move_alloc(tmp, Axis_sets)
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
       if ( trim(name) == Axes(i)%name ) then
          if ( trim(name) == 'Stations' .or. trim(name) == 'Levels') THEN
             fms_diag_axis_init = Axes(i)
             return
          else if ( set == Axes(i)%set ) then
             if ( trim(lowercase(name)) == 'time' .or.&
                  & trim(lowercase(cart_name)) == 't' .or.&
                  & trim(lowercase(name)) == 'nv' .or.&
                  & trim(lowercase(cart_name)) == 'n' ) then
                fms_diag_axis_init = Axes(i)
                return
             else if ( (lowercase(cart_name) /= 'x' .and. lowercase(cart_name) /= 'y')&
                  & .or. tile /= Axes(i)%tile) then
                ! <ERROR STATUS="FATAL">axis_name <NAME> and axis_set already exist.</ERROR>
                call error_mesg('diag_axis_mod::diag_axis_init',&
                     & 'axis_name '//trim(name)//' and axis_set already exist.', FATAL)
             end if
          end if
       end if
    end do

    !---- register axis ----
    num_def_axes = num_def_axes + 1

    !Increase size of Axes array by 1
    integer, allocatable :: tmp(:)
    allocate(tmp(0:size(Axes))
    tmp(0:size(Axes)) = Axes(0:size(Axes))
    call move_alloc(tmp, Axes)

    !---- allocate storage for coordinate values of axis ----
    if ( Axes(num_def_axes)%cart == 'T' ) then
       axlen = 0
    else
       axlen = SIZE(DATA(:))
    end if
    allocate ( Axes(num_def_axes)%adata(0:axlen) )

     Axes(num_def_axes)%aname = trim(aname)
     Axes(num_def_axes)%adata = adata(1:axlen)
     Axes(num_def_axes)%units = units
     Axes(num_def_axes)%alen = axlen
     Axes(num_def_axes)%cart = cart
     if (present(start)) then 
          Axes(num_def_axes)%start = start
     else 
          Axes(num_def_axes)%start = 1
     end if
     if (present(ending)) then 
          Axes(num_def_axes)%ending = ending
     else 
          Axes(num_def_axes)%ending = 1
     end if
     if (present(longname)) then 
          Axes(num_def_axes)%longname = trim(longname)
     else
          Axes(num_def_axes)%longname = trim(aname)
     end if
     if (present(direction)) then 
          Axes(num_def_axes)%direction = direction
     else 
          Axes(num_def_axes)%direction = HORIZONTAL
     end if
     if (present(tile_count)) then
	  Axes(num_def_axes)%tile = tile_count
     else 
	  Axes(num_def_axes)%tile = 1
     end if
     if (present(attributes)) then 
          alloacte(character(len=20) :: Axes(num_def_axes)%attributes (size(attributes)))
          do i = 1,size(attributes)
               Axes(num_def_axes)%attributes(i) =  attributes
          end do
     end if

     !---- Handle the DomainU check
    if (present(DomainU) .and. (present(Domain2) .or. present(Domain)) ) then
       ! <ERROR STATUS="FATAL">Presence of DomainU and another Domain at the same time is prohibited</ERROR>
       call error_mesg('diag_axis_mod::diag_axis_init',&
            & 'Presence of DomainU and another Domain at the same time is prohibited', FATAL)
    !---- domain2d type ----
    else if ( present(Domain2) .and. present(Domain)) then
       ! <ERROR STATUS="FATAL">Presence of both Domain and Domain2 at the same time is prohibited</ERROR>
       call error_mesg('diag_axis_mod::diag_axis_init',&
            & 'Presence of both Domain and Domain2 at the same time is prohibited', FATAL)
    else if ( present(Domain2) .or. present(Domain)) then
       if ( Axes(num_def_axes)%cart_name /= 'X' .and. Axes(num_def_axes)%cart_name /= 'Y') then
          ! <ERROR STATUS="FATAL">Domain must not be present for an axis which is not in the X or Y direction.</ERROR>
          call error_mesg('diag_axis_mod::diag_axis_init',&
               & 'A Structured Domain must not be present for an axis which is not in the X or Y direction', FATAL)
       end if
    else if (present(DomainU) .and. Axes(num_def_axes)%cart_name /= 'U') then
          call error_mesg('diag_axis_mod::diag_axis_init',&
               & 'In the unstructured domain, the axis cart_name must be U', FATAL)
    end if


    if ( present(Domain2) ) then
       Axes(num_def_axes)%Domain2 = Domain2
       call mpp_get_domain_components(Domain2, domain_x, domain_y, tile_count=tile_count)
       if ( Axes(num_def_axes)%cart_name == 'X' ) Axes(num_def_axes)%Domain = domain_x
       if ( Axes(num_def_axes)%cart_name == 'Y' ) Axes(num_def_axes)%Domain = domain_y
       Axes(num_def_axes)%DomainUG = null_DomainUG
    else if ( PRESENT(Domain)) then
       !---- domain1d type ----
       Axes(num_def_axes)%Domain2 = null_domain2d ! needed since not 2-D domain
       Axes(num_def_axes)%Domain = Domain
       Axes(num_def_axes)%DomainUG = null_DomainUG
    else (present(DomainU)) then
       Axes(num_def_axes)%Domain2 = null_domain2d
       Axes(num_def_axes)%Domain = null_domain1d
       Axes(num_def_axes)%DomainUG = DomainU
    else
       Axes(num_def_axes)%Domain2 = null_domain2d
       Axes(num_def_axes)%Domain = null_domain1d
       Axes(num_def_axes)%DomainUG = null_domainUG
    end if


     Axes(num_def_axes)%initialized = .true.
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
