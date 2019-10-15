module fms_diag_table_mod

use fms_diag_data_mod, only: diag_files_type, diag_fields_type
use iso_c_binding
implicit none

public :: diag_table, fms_diag_table_init, fms_read_diag_table

 character(len=:),allocatable :: diag_table

 character (len=20) :: diag_table_name = "diag_table         "!< This is the filename of the "main" diag table
 logical            :: verbose = .false.
namelist / diag_table_nml / diag_table_name, verbose

integer :: num_diag_files, num_diag_fields
type(diag_files_type),  allocatable, dimension(:) :: diag_files 
type(diag_fields_type), allocatable, dimension(:) :: diag_fields 
logical :: diag_table_init = .false.
integer :: fatal=-143, note=143


!> Interface with C routines wrapping libyaml
interface 
!> Checks the diag_yaml file
 integer function diag_parse_check (fname) bind(C, name="diag_parse_check")
        use iso_c_binding
        character(kind=c_char)  :: fname !< The name of the diag_yaml file
 end function diag_parse_check
!> Outputs the number of files and fields
 subroutine diag_num_files (fname, ifiles_p, ifields_p) bind(C, name="diag_num_files")
        use iso_c_binding
        character(kind=c_char)  :: fname !< The name of the diag_yaml file
        integer(c_int)          :: ifiles_p !< The number of files liste in the diag_yaml
        integer(c_int)          :: ifields_p !< The number of fields liste in the diag_yaml
 end subroutine diag_num_files
!> Fills in the information from the diag_yaml related to the diag_file_type  
 subroutine diag_get_file_info (fname, diag_files_fortran,i) bind(C, name="diag_get_file_info")
        use iso_c_binding
        import
        character(kind=c_char) :: fname !< The name of the diag_yaml file
        type(diag_files_type) :: diag_files_fortran !< Matches the struct filled in with the diag_file_info
!        type(c_ptr) :: diag_files_fortran
        integer(c_int),value :: i !< The index of the diag_file array

 end subroutine diag_get_file_info
end interface


contains
!> Initializaes the diag_table by reading the diag_table_nml, reading in the diag_table, parsing 
!! the diag_table and getting the relevant information by filling in the diag_file and diag_field
!! derived type arrays 
subroutine fms_diag_table_init (nml_file)

character(len=*), intent(in), optional  :: nml_file             !< The name of the namelist file
integer                                 :: io_error             !< Stores the io error code
integer                                 :: a                    !< An integer
character(len=:), allocatable           :: read_nml_file        !< String to store the nml file name
integer                                 :: diag_table_unit      !< Unit number for the diag table
integer                                 :: diag_table_len       !< The length of character of the diag_table
character(len=:),allocatable            :: diag_len_temp        !< The diag_table?
integer                                 :: c_err                !< Error code from C function
character(len=30)                       :: diag_yaml_name       !< The name of the diag_table/yaml file
integer                                 :: i                    !< Indexing integer

!> If already initalized, return
if (diag_table_init) then
     if(verbose)call diag_error("fms_diag_table_mod","diag_table already initialized",NOTE)
     return
endif
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!! DEAL WITH NAMELIST
if(present(nml_file)) then
     a = len(nml_file)
     allocate(character(len=a) :: read_nml_file)
     read_nml_file = nml_file
else
!!!!!! This is for the real code !!!!!!
!     read_nml_file = input_nml_file
     a = 20
     allocate(character(len=a) :: read_nml_file)
     read_nml_file = "diag_table_nml& /"
endif

read (read_nml_file, nml=diag_table_nml, iostat=io_error)
deallocate(read_nml_file)
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!! READ THE DIAG_TABLE
write (6,*) trim(diag_table_name)
diag_table_unit = 290
!> Find the length of the diag_table
open (unit=diag_table_unit, file=trim(diag_table_name), &
     status="old", access="stream")
allocate(character(len=10000000) :: diag_len_temp)
read(diag_table_unit,iostat=io_error) diag_len_temp
inquire (unit=diag_table_unit,pos=diag_table_len) 
close (diag_table_unit)
deallocate(diag_len_temp)
!> Read the diag_table
if (allocated(diag_table)) call diag_error("fms_diag_table_mod","diag_table already allocated",FATAL)
allocate(character(len=diag_table_len) :: diag_table)
open (unit=diag_table_unit, file=trim(diag_table_name), &
     status="old", access="stream")
read(diag_table_unit,iostat=io_error) diag_table
if (verbose) call diag_error("fms_diag_table_mod",trim(diag_table),NOTE)
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!> Check the diag_yaml file
diag_yaml_name = "diag_yaml"
 c_err = diag_parse_check(trim(diag_yaml_name)//c_null_char)
 if (c_err .ne. 0) then
        if (c_err == 1) call diag_error("fms_diag_table_mod","parser error",FATAL)
        if (c_err == 2) call diag_error("fms_diag_table_mod",diag_yaml_name//" error opening",FATAL)        
 endif
!> get the number of files and fields in the diag_yaml 
 call diag_num_files(trim(diag_yaml_name)//c_null_char, num_diag_files, num_diag_fields)
! write(6,'(a,i2,1x,a,i2)')"  Files = ",num_diag_files,"Fields = ",num_diag_fields
 allocate(diag_files(num_diag_files) )
 allocate(diag_fields(num_diag_fields) )
!> get the diag_yaml file info
do i = 1,num_diag_files
  call diag_get_file_info(trim(diag_yaml_name)//c_null_char, diag_files(i), i-1)
  write (6,*) diag_files(i)%freq,",,,", diag_files(i)%fname(1:20),",,,", diag_files(i)%frequnit,",,,", diag_files(i)%unlimdim

enddo
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!> Set the init value to .true.
diag_table_init = .true.
if (verbose) call diag_error("fms_diag_table_mod","Initialized!",NOTE)
end subroutine fms_diag_table_init

subroutine fms_read_diag_table (diag_table_name, diag_table_string)
character(len=*), intent(in)                 :: diag_table_name 
character(len=:), intent(out), allocatable   :: diag_table_string



end subroutine fms_read_diag_table

subroutine diag_error(sub,mess,lev)
character(len=*), intent(in)  :: sub
character(len=*), intent(in)  :: mess
integer, intent(in)           :: lev

write (6,*)sub,":: ",mess
if (lev==fatal) then
     stop
endif

end subroutine diag_error


end module fms_diag_table_mod
