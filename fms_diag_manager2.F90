module fms_diag_manager2_mod

use fms_diag_data_mod, only: diag_files_type, diag_fields_type
use fms_diag_data_mod, only: monthly, daily, diurnal, yearly, no_diag_avergaing, instantaneous, &
     three_hourly, six_hourly, r8, r4, i8, i4, string
use fms_diag_data_mod, only: diag_error,fatal,note,warning

use fms_diag_concur_mod, only: diag_comm_init, fms_write_diag_comm, fms_diag_comm_type
use fms_diag_table_mod !get_diag_table_field
use fms_diag_register_mod

implicit none

logical :: fms_diag_manager_initialized = .false.

logical :: verbose = .false. !< When true, more information is printed out
character(len=20) :: diag_table_name = "diag_yaml           "!< The name of the diagnostic list to be parsed
integer :: diag_pes = 0 !< The number of ranks set aside for running diagnostics
logical :: diag_concurrent = .false. !< When true, run the diagnostics concurrently with diag_pes
integer :: MAX_LEN_VARNAME = 20 !< The size of the string of the varname
integer :: MAX_LEN_META = 30 !< The size of the string of each metadata
integer :: MAX_DIAG_VARS = 1024 !< The maximum number of diag variables
namelist/diag_manager_nml/verbose,diag_table_name,diag_pes,diag_concurrent,MAX_LEN_VARNAME, &
& MAX_LEN_META, MAX_DIAG_VARS

character(len=9) :: read_nml_file="input.nml"
type(fms_diag_comm_type) :: diag_comm !< The diag communicator
integer, allocatable :: diag_var_id_list (:) !< A list of potential diag IDs
integer, allocatable :: diag_var_id_used (:) !< A list of used diag IDs

interface fms_register_diag_field
     module procedure fms_register_diag_field_generic
end interface fms_register_diag_field
interface operator (<)
     procedure obj_lt_int
     procedure int_lt_obj
end interface
interface operator (<=)
     procedure obj_le_int
     procedure int_le_obj
end interface
interface operator (>)
     procedure obj_gt_int
     procedure int_gt_obj
end interface
interface operator (>=)
     procedure obj_ge_int
     procedure int_ge_obj
end interface
interface operator (==)
     procedure obj_eq_int
     procedure int_eq_obj
end interface
interface operator (.ne.)
     procedure obj_ne_int
     procedure int_ne_obj
end interface

!interface fms_send_data
!     module procedure send_data_3d
!end interface fms_send_data

!> \brief Object that holds all variable information
type fms_diag_object
     type (diag_fields_type)                           :: diag_field         !< info from diag_table
     type (diag_files_type),allocatable, dimension(:)  :: diag_file          !< info from diag_table
     integer                                          :: diag_id           !< unique id for varable
!     class (fms_io_obj), allocatable, dimension(:)    :: fms_fileobj        !< fileobjs
     character(len=:), allocatable, dimension(:)      :: metadata          !< metedata for the variable
     logical                                          :: is_static         !< true is this is a static var
     logical, allocatable                             :: is_registered     !< true when registered
     integer, allocatable, dimension(:)               :: frequency         !< specifies the frequency

     integer                                          :: vartype           !< the type of varaible
     character(len=:), allocatable                    :: varname           !< the name of the variable     
     character(len=:), allocatable                    :: longname          !< longname of the variable     
     character(len=:), allocatable                    :: units             !< the units
     character(len=:), allocatable                    :: modname           !< the module
     integer                                          :: missing_value     !< The missing fill value
     integer, allocatable, dimension(:)               :: axis_ids          !< variable axis IDs
!     type (diag_axis), allocatable, dimension(:)      :: axis              !< The axis object 

     contains
     procedure :: register => fms_register_diag_field_obj
!     procedure :: send_data => fms_send_data
     procedure :: diag_id_inq => fms_diag_id_inq
     procedure :: copy => copy_diag_obj
end type fms_diag_object
!> \brief Extends the variable object to work with multiple types of data
type, extends(fms_diag_object) :: fms_diag_object_scalar
     class(*), allocatable :: vardata
end type fms_diag_object_scalar
type, extends(fms_diag_object) :: fms_diag_object_1d
     class(*), allocatable, dimension(:) :: vardata
end type fms_diag_object_1d
type, extends(fms_diag_object) :: fms_diag_object_2d
     class(*), allocatable, dimension(:,:) :: vardata
end type fms_diag_object_2d
type, extends(fms_diag_object) :: fms_diag_object_3d
     class(*), allocatable, dimension(:,:,:) :: vardata
end type fms_diag_object_3d
type, extends(fms_diag_object) :: fms_diag_object_4d
     class(*), allocatable, dimension(:,:,:,:) :: vardata
end type fms_diag_object_4d
type, extends(fms_diag_object) :: fms_diag_object_5d
     class(*), allocatable, dimension(:,:,:,:,:) :: vardata
end type fms_diag_object_5d

type(fms_diag_object_scalar) :: null_sc
type(fms_diag_object_1d) :: null_1d
type(fms_diag_object_2d) :: null_2d
type(fms_diag_object_3d) :: null_3d
type(fms_diag_object_4d) :: null_4d
type(fms_diag_object_5d) :: null_5d



public :: fms_diag_manager_init
public :: fms_diag_object_scalar, fms_diag_object_1d
public :: fms_diag_object_2d, fms_diag_object_3d, fms_diag_object_4d, fms_diag_object_5d
private :: diag_comm, copy_diag_obj, null_sc, null_1d, null_2d, null_3d, null_4d, null_5d


contains
!> \brief Reads the diag namelist and runs all diag_manage initialization routines
subroutine fms_diag_manager_init

integer :: io_error
integer :: i !< For looping
character (len=10000) :: fms_namelist_string
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!! DEAL WITH NAMELIST
open(unit=29,file=read_nml_file,status="old")
read (29, nml=diag_manager_nml, iostat=io_error)
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!> Set up the diag_id lists
allocate (integer :: diag_var_id_list (MAX_DIAG_VARS))
allocate (integer :: diag_var_id_used (MAX_DIAG_VARS))

!OMP PARALLEL DO shared(diag_var_id_list,diag_var_id_used)
do i = 1 , MAX_DIAG_VARS
     diag_var_id_list(i) = i
     diag_var_id_used(i) = 0
enddo

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!> Initialize and check if a diag_comm will be used
if (diag_concurrent) then
   if (diag_pes <= 0) call diag_error ("diag_manager_nml",&
     "If diag_concurrent=.true., then diag_pes must be greater than 0",fatal)
   call diag_comm_init(diag_comm, diag_pes)
   call fms_write_diag_comm(diag_comm)
endif
!> Parse the diag table and store the diag table information
call fms_diag_table_init(diag_table_name, verbose)

!> Initialize the null_d variables
 null_sc%diag_id = -999
 null_1d%diag_id = -999
 null_2d%diag_id = -999
 null_3d%diag_id = -999
 null_4d%diag_id = -999
 null_5d%diag_id = -999

fms_diag_manager_initialized = .true.
end subroutine fms_diag_manager_init
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!> \description A generic routine to register a diagnostic field.  Here we allocate an unallocated 
!! diag object with some default values, and then we call the porcedure register to get the diag_id.
!! This only allocates the file object to fms_diag_object, and will be reallocated on the first send_data
!! call so that it can be set up for the correct type
 type(fms_diag_object) function fms_register_diag_field_generic (modname,varname,axes, time, longname, &
     units, missing_value, metadata) result(diagob)
 character(*)               , intent(in)               :: modname!< The module name
 character(*)               , intent(in)               :: varname!< The variable name
 integer     , dimension(:) , intent(in)               :: axes   !< The axes 
 integer                    , intent(in)               :: time !< Time placeholder 
 character(*)               , intent(in), optional     :: longname!< The variable long name
 character(*)               , intent(in), optional     :: units  !< Units of the variable
 integer                    , intent(in), optional     :: missing_value !< A missing value to be used 
 character(*), dimension(:) , intent(in), optional     :: metadata
 integer :: diag_id
! type(fms_diag_object) :: null_obj
! if (.not. allocated(diagobj)) then 
 diagob%diag_id = -999
! diagob%vartype = -999
!     null_obj%is_static = .false.
!     diagobj = null_obj
! endif
 call diagob%register(modname, varname, axes, time, longname, units, missing_value, metadata) 

end function fms_register_diag_field_generic

subroutine fms_register_diag_field_obj (dobj, modname, varname, axes, time, longname, units, missing_value, metadata)
 class(fms_diag_object)     , intent(inout)            :: dobj
 character(*)               , intent(in)               :: modname!< The module name
 character(*)               , intent(in)               :: varname!< The variable name
 integer     , dimension(:) , intent(in)               :: axes   !< The axes 
 integer                    , intent(in)               :: time !< Time placeholder 
 character(*)               , intent(in), optional     :: longname!< The variable long name
 character(*)               , intent(in), optional     :: units  !< Units of the variable
 integer                    , intent(in)               :: missing_value !< A missing value to be used 
 character(*), dimension(:) , intent(in), optional     :: metadata
 class(*), pointer :: vptr
 integer :: diag_id
 integer :: i
!> Fill in information from the register call
  allocate(character(len=MAX_LEN_VARNAME) :: dobj%varname)
  dobj%varname = trim(varname)
  allocate(character(len=len(modname)) :: dobj%modname)
  dobj%modname = trim(modname)
  if (present(longname)) then
     allocate(character(len=len(longname)) :: dobj%longname)
     dobj%longname = trim(longname)
  endif
  if (present(units)) then
     allocate(character(len=len(units)) :: dobj%units)
     dobj%units = trim(units)
  endif

  if (present(metadata)) then
     allocate(character(len=MAX_LEN_META) :: dobj%metadata(size(metadata)))
     dobj%metadata = metadata
  endif
!> Grab the information from the diag_table
  dobj%diag_field = get_diag_table_field(trim(varname))
     write(6,*)"IKIND for diag_fields(1) is",diag_fields(1)%ikind
     write(6,*)"IKIND for "//trim(varname)//" is ",dobj%diag_field%ikind


!> Get an ID number for the diagnostic 
  do i = 1,max_diag_vars
     if (diag_var_id_used (i) == 0) then 
          diag_var_id_used(i) = diag_var_id_list(i)
          diag_id = diag_var_id_list(i)
          exit
     endif
     if (i == max_diag_vars) then
          call diag_error("fms_register_diag_field_obj","You have registered too many diagnostics."//&
           "Please increase by setting MAX_DIAG_VARS in the diag_manager_nml",FATAL)
     endif
  enddo
  dobj%diag_id = diag_id
!  allocate (logical :: dobj%is_registered )
  dobj%is_registered = .true.
end subroutine fms_register_diag_field_obj
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

!> \descrption The user API for diag_manager is send_data.  Users pass their variable object to 
!! this routine, and magic happens.  Send data 
subroutine fms_send_data (diagobj, var, time, is_in, js_in, ks_in, mask, &
                                   rmask, ie_in, je_in, ke_in, weight, err_msg)
 class (fms_diag_object),target, intent(inout), allocatable :: diagobj !< The diag variable object
 class(*), dimension(:,:,:)   , intent(in) , target         :: var !< The variable
 class(*), dimension(:,:,:)   ,              pointer        :: vptr => NULL() !< A pointer to the data
 integer, optional            , intent(in)                  :: time !< A time place holder
 integer, optional            , intent(in)                  :: is_in !< Start of the first dimension
 integer, optional            , intent(in)                  :: js_in !< Start of the second dimension
 integer, optional            , intent(in)                  :: ks_in !< Start of the third dimension
 integer, optional            , intent(in)                  :: ie_in !< End of the first dimension
 integer, optional            , intent(in)                  :: je_in !< End of the second dimension
 integer, optional            , intent(in)                  :: ke_in !< End of the third dimension
 logical, optional            , intent(in)                  :: mask !< A lask for point to ignore
 real   , optional            , intent(in)                  :: rmask !< I DONT KNOW
 real   , optional            , intent(in)                  :: weight !< Something for averaging?
 CHARACTER(len=*)             , INTENT(out), OPTIONAL       :: err_msg
! local vars
 class (fms_diag_object)      , pointer                     :: dptr => NULL()
 type (fms_diag_object_scalar), allocatable                 :: dsc
 type (fms_diag_object_1d), allocatable                     ::  d1d
 type (fms_diag_object_2d), allocatable                     ::  d2d
! type (fms_diag_object_3d), allocatable                     ::  d3d
 class (fms_diag_object), allocatable                     ::  d3d
 type (fms_diag_object_4d), allocatable                     ::  d4d
 type (fms_diag_object_5d), allocatable                     ::  d5d
!> "Point at diag obj" 
 dptr => diagobj

!> If this is the 
!> 
 select type (dptr)
     type is (fms_diag_object)
!> Switch the diagobj to a 3d
          d3d = null_3d
          call dptr%copy(d3d)
          deallocate(diagobj)
          diagobj = null_3d
          call d3d%copy(diagobj)
 end select
 if (associated(dptr)) nullify(dptr)

  vptr => var
  select type (vptr)
     type is (real(kind=8))
          diagobj%vartype = r8
     type is (real(kind=4))
          diagobj%vartype = r4
     type is (integer(kind=8))
          diagobj%vartype = i8
     type is (integer(kind=4))
          diagobj%vartype = i4
     type is (character(*))
          diagobj%vartype = string
     class default
          call diag_error("fms_register_diag_field_obj","The type of "//trim(diagobj%varname)//&
          " is not supported", FATAL)
  end select

end subroutine fms_send_data
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
subroutine copy_diag_obj(objin , objout)
 class (fms_diag_object)      , intent(in)                :: objin
 class (fms_diag_object)      , intent(inout) , allocatable :: objout
select type (objout)
 class is (fms_diag_object)
!write (6,*) "debug statement",objin%diag_id,objin%diag_id

  if (allocated(objin%is_registered)) then
     objout%is_registered = objin%is_registered
  else
     call diag_error("copy_diag_obj", "You can only copy objects that have been registered",warning)
  endif
!     type (diag_fields_type)                           :: diag_field         !< info from diag_table
!     type (diag_files_type),allocatable, dimension(:)  :: diag_file          !< info from diag_table

     objout%diag_id = objin%diag_id           

!     class (fms_io_obj), allocatable, dimension(:)    :: fms_fileobj        !< fileobjs
     if (allocated(objin%metadata)) objout%metadata = objin%metadata
     objout%is_static = objin%is_static         
     if (allocated(objin%frequency)) objout%frequency = objin%frequency
     if (allocated(objin%varname)) objout%varname = objin%varname
end select
end subroutine copy_diag_obj

!> \brief Returns the diag_id
integer function fms_diag_id_inq (dobj) result(diag_id)
 class(fms_diag_object)     , intent(inout)            :: dobj
! character(*)               , intent(in)               :: varname
 
 if (.not.allocated(dobj%is_registered)) then
     call diag_error ("fms_what_is_my_id","The diag object was not registered", fatal)
 endif
     diag_id = dobj%diag_id
end function fms_diag_id_inq
!> \brief override for checking if object ID is greater than an integer (IDs)
pure logical function obj_gt_int (obj,i) result(ll)
 class (fms_diag_object), intent(in) :: obj
 integer,                 intent(in) :: i
     ll = (obj%diag_id > i)
end function obj_gt_int
!> \brief override for checking if integer (ID) is greater than an object ID
pure logical function int_gt_obj (i,obj) result(ll)
 class (fms_diag_object), intent(in) :: obj
 integer,                 intent(in) :: i
     ll = (i > obj%diag_id)
end function int_gt_obj
!> \brief override for checking if object ID is less than an integer (IDs)
pure logical function obj_lt_int (obj,i) result(ll)
 class (fms_diag_object), intent(in) :: obj
 integer,                 intent(in) :: i
     ll = (obj%diag_id < i)
end function obj_lt_int
!> \brief override for checking if integer (ID) is less than an object ID
pure logical function int_lt_obj (i,obj) result(ll)
 class (fms_diag_object), intent(in) :: obj
 integer,                 intent(in) :: i
     ll = (i < obj%diag_id)
end function int_lt_obj
!> \brief override for checking if object ID is greater than or equal to an integer (IDs)
pure logical function obj_ge_int (obj,i) result(ll)
 class (fms_diag_object), intent(in) :: obj
 integer,                 intent(in) :: i
     ll = (obj%diag_id >= i)
end function obj_ge_int
!> \brief override for checking if integer (ID) is greater than or equal to an object ID
pure logical function int_ge_obj (i,obj) result(ll)
 class (fms_diag_object), intent(in) :: obj
 integer,                 intent(in) :: i
     ll = (i >= obj%diag_id)
end function int_ge_obj
!> \brief override for checking if object ID is less than or equal to an integer (IDs)
pure logical function obj_le_int (obj,i) result(ll)
 class (fms_diag_object), intent(in) :: obj
 integer,                 intent(in) :: i
     ll = (obj%diag_id <= i)
end function obj_le_int
!> \brief override for checking if integer (ID) is less than or equal to an object ID
pure logical function int_le_obj (i,obj) result(ll)
 class (fms_diag_object), intent(in) :: obj
 integer,                 intent(in) :: i
     ll = (i <= obj%diag_id)
end function int_le_obj
!> \brief override for checking if object ID is equal to an integer (IDs)
pure logical function obj_eq_int (obj,i) result(ll)
 class (fms_diag_object), intent(in) :: obj
 integer,                 intent(in) :: i
     ll = (obj%diag_id == i)
end function obj_eq_int
!> \brief override for checking if integer (ID) is equal to an object ID
pure logical function int_eq_obj (i,obj) result(ll)
 class (fms_diag_object), intent(in) :: obj
 integer,                 intent(in) :: i
     ll = (i == obj%diag_id)
end function int_eq_obj
!> \brief override for checking if object ID is not equal to an integer (IDs)
pure logical function obj_ne_int (obj,i) result(ll)
 class (fms_diag_object), intent(in) :: obj
 integer,                 intent(in) :: i
     ll = (obj%diag_id .ne. i)
end function obj_ne_int
!> \brief override for checking if integer (ID) is not equal to an object ID
pure logical function int_ne_obj (i,obj) result(ll)
 class (fms_diag_object), intent(in) :: obj
 integer,                 intent(in) :: i
     ll = (i .ne. obj%diag_id)
end function int_ne_obj

end module fms_diag_manager2_mod
