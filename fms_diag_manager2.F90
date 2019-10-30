module fms_diag_manager2_mod

use fms_diag_data_mod, only: diag_files_type, diag_fields_type
use fms_diag_data_mod, only: monthly, daily, diurnal, yearly, no_diag_avergaing, instantaneous, &
     three_hourly, six_hourly, r8, r4, i8, i4, string
use fms_diag_data_mod, only: diag_error,fatal,note,warning

use fms_diag_concur_mod, only: diag_comm_init, fms_write_diag_comm, fms_diag_comm_type
use fms_diag_table_mod
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
     integer, allocatable, dimension(:)               :: averaging         !< specifies the frequency
     integer                                          :: vartype           !< the type of varaible
     character(len=:), allocatable                    :: varname           !< the name of the variable     
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
 type(fms_diag_object) function fms_register_diag_field_generic (varname,fnames,metadata) &
                       result(diagob)
 character(*)               , intent(in)               :: varname
 character(*), dimension(:) , intent(in)               :: fnames
 character(*), dimension(:) , intent(in), optional     :: metadata
 integer :: diag_id
! type(fms_diag_object) :: null_obj
! if (.not. allocated(diagobj)) then 
 diagob%diag_id = -999
 diagob%vartype = -999
!     null_obj%is_static = .false.
!     diagobj = null_obj
! endif
 call diagob%register(varname,fnames,metadata) 

end function fms_register_diag_field_generic

subroutine fms_register_diag_field_obj (dobj,varname,fnames,metadata,var)
 class(fms_diag_object)     , intent(inout)            :: dobj
 class(*)    ,       target , intent(in), optional     :: var
 character(*)               , intent(in)               :: varname
 character(*), dimension(:) , intent(in)               :: fnames
 character(*), dimension(:) , intent(in), optional     :: metadata
 class(*), pointer :: vptr
 integer :: diag_id
 integer :: i
 
! allocate(type(fms_io_obj) :: dobj%fms_fileobj(size(fnames)))
 if (present(var)) then
  vptr => var
  select type (vptr)
     type is (real(kind=8))
          dobj%vartype = r8
     type is (real(kind=4))
          dobj%vartype = r4
     type is (integer(kind=8))
          dobj%vartype = i8
     type is (integer(kind=4))
          dobj%vartype = i4
     type is (character(*))
          dobj%vartype = string
     class default
          call diag_error("fms_register_diag_field_obj","The type of "//trim(varname)//" is not supported",&
                           FATAL)
  end select
 endif
  allocate(character(len=MAX_LEN_VARNAME) :: dobj%varname)
  dobj%varname = trim(varname)
  if (present(metadata)) then
     allocate(character(len=MAX_LEN_META) :: dobj%metadata(size(metadata)))
     dobj%metadata = metadata
  endif
  
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
 class(*), dimension(:,:,:)   , intent(in)                  :: var !< The variable
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
 
 !local vars
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
     if (allocated(objin%averaging)) objout%averaging = objin%averaging
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
end module fms_diag_manager2_mod
