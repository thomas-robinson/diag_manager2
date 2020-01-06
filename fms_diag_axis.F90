module fms_diag_axis_mod

use fms_diag_data_mod,  only: diag_null, diag_error, fatal, note, warning
use fms2_io_mod

!!TODO:
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



public :: diag_axis_type
public :: UP, DOWN, VOID_AXIS, HORIZONTAL

contains

subroutine fms_diag_axis_init (axis, aname, adata, units, cart, long_name, direction,&
       & set_name, edges, Domain, Domain2, DomainU, aux, req, tile_count, start, ending, attributes)
    type(diag_axis_type), intent(inout)      :: axis !< The axis object
    CHARACTER(len=*), INTENT(in)             :: aname !< The name of the axis
    class(*), target, INTENT(in), DIMENSION(:)    :: adata !< The axis data
    !class(*), pointer,          , DIMENSION(:)    :: dptr=> NULL() !< A pointer to the data
    CHARACTER(len=*), INTENT(in)             :: units !< The axis units
    CHARACTER(len=*), INTENT(in)             :: cart !< The cartesian name of the axis 
    CHARACTER(len=*), INTENT(in), OPTIONAL   :: long_name !< Axis long name
    CHARACTER(len=*), INTENT(in), OPTIONAL   :: set_name !< Axis set name?
    INTEGER,          INTENT(in), OPTIONAL   :: direction !< Axis direction
    INTEGER,          INTENT(in), OPTIONAL   :: edges !< The axis edges
    TYPE(domain1d),   INTENT(in), OPTIONAL   :: Domain !<Axis domain
    TYPE(domain2d),   INTENT(in), OPTIONAL   :: Domain2 !< Axis domain if 2D
    TYPE(domainUG),   INTENT(in), OPTIONAL   :: DomainU !< Axis domain if unstructured
    CHARACTER(len=*), INTENT(in), OPTIONAL   :: aux !< ???
    CHARACTER(len=*), INTENT(in), OPTIONAL   :: req !< Is it required???
    INTEGER,          INTENT(in), OPTIONAL   :: tile_count !< The tile count
    INTEGER,          INTENT(in), OPTIONAL   :: start !!TODO:
    INTEGER,          INTENT(in), OPTIONAL   :: ending !!TODO:
    CHARACTER(len=*), INTENT(in), OPTIONAL   :: attributes(:) !!TODO

    TYPE(domain1d) :: domain_x, domain_y
    INTEGER :: ierr, axlen
    INTEGER :: i, set, tile
    INTEGER :: isc, iec, isg, ieg
    CHARACTER(len=128) :: emsg

     integer :: ls       !< The local starting value
     integer :: le       !< the local ending value

     if (len(cart) > 1) then
        call diag_error("fms_diag_axis_init","CARTNAME for "//trim(aname)// &
            " must only be one letter.  You have " // trim(cart), FATAL)
     end if

     if (cart .ne. "X" .or. cart .ne. "Y" .or. cart .ne. "Z" .or. &
         cart .ne. "N" .or. cart .ne. "U" .or. cart .ne. "T") then
        call diag_error("fms_diag_axis_init","CARTNAME for "//trim(aname)//" can only be X "// &
          "Y Z U or N.  You have "//trim(cart), FATAL)
          end if
     axis%aname = trim(aname)
     axis%cart = cart
     if (present(start)) then 
          axis%start = start
     else
          axis%start = 1
     endif
     if (present(ending)) then 
          axis%ending = ending
     else 
          axis%ending = 1
     endif
     if (present(long_name)) then
          axis%longname = trim(long_name)
     else
          axis%longname = trim(aname)
     endif
     if (present(direction)) then 
          axis%direction = direction
     else 
          axis%direction = HORIZONTAL
     endif
     if (present(attributes)) then 
          allocate(character(len=20) :: axis%attributes (size(attributes)))
          do i = 1,size(attributes)
               axis%attributes(i) =  attributes(i)
          enddo
     endif

     axis%initialized = .true.
end subroutine fms_diag_axis_init

end module fms_diag_axis_mod
