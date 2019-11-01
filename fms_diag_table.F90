module fms_diag_table_mod

use fms_diag_data_mod, only: diag_files_type, diag_fields_type, diag_error,fatal,note,warning
use c_to_fortran_mod, only: fms_c2f_string
use iso_c_binding
implicit none

public :: diag_table, fms_diag_table_init, fms_write_diag_table

 character(len=:),allocatable :: diag_table



integer :: num_diag_files, num_diag_fields
type(diag_files_type),  allocatable, dimension(:) :: diag_files 
type(diag_fields_type), allocatable, dimension(:) :: diag_fields 
type(diag_fields_type)                            :: null_field_type

logical :: diag_table_init = .false.



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
 subroutine diag_get_file_info (fname, diag_files_fortran,diag_fields_fortran,i) bind(C, name="diag_get_file_info")
        use iso_c_binding
        import
        character(kind=c_char) :: fname !< The name of the diag_yaml file
        type(diag_files_type) :: diag_files_fortran !< Matches the struct filled in with the diag_file_info
        type(diag_fields_type) :: diag_fields_fortran !< Matches the struct filled in with the diag_file_info
!        type(c_ptr) :: diag_files_fortran
        integer(c_int),value :: i !< The index of the diag_file array

 end subroutine diag_get_file_info
end interface


contains
!> Initializaes the diag_table by reading the diag_table_nml, reading in the diag_table, parsing 
!! the diag_table and getting the relevant information by filling in the diag_file and diag_field
!! derived type arrays 
subroutine fms_diag_table_init (diag_yaml_name,verb)

character(len=*), intent(in)            :: diag_yaml_name        !< The name of the namelist file
logical         , intent(in), optional  :: verb                  !< If true, more output is printed
logical                                 :: verbose               !< If true, more output is printed
integer                                 :: c_err                 !< Error code from C function
integer                                 :: i                     !< Indexing integer
character(len=:),allocatable            :: char1, char2          !< Utility strings
if (present(verb)) then
     verbose = verb
else
     verbose = .false.
endif
!> If already initalized, return
if (diag_table_init) then
     if(verbose)call diag_error("fms_diag_table_mod","diag_table already initialized",NOTE)
     return
endif
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!> Check the diag_table file
 c_err = diag_parse_check(trim(diag_yaml_name)//c_null_char)
 if (c_err .ne. 0) then
        if (c_err == 1) call diag_error("fms_diag_table_mod","parser error",FATAL)
        if (c_err == 2) call diag_error("fms_diag_table_mod",diag_yaml_name//" error opening",FATAL)        
 endif
!> get the number of files and fields in the diag_yaml 
 call diag_num_files(trim(diag_yaml_name)//c_null_char, num_diag_files, num_diag_fields)

 if (verbose) then
     allocate(character(len=8) :: char1)
     allocate(character(len=8) :: char2)
     write(char1,'(I8)') num_diag_files
     write(char2,'(I8)') num_diag_fields
     call diag_error("diag_table_init","There are "//trim(char2)//" fields in "//trim(char1)//&
                     " diag files.",NOTE)
     deallocate(char1) ; deallocate(char2)
 endif
 allocate(diag_files(num_diag_files) )
 allocate(diag_fields(num_diag_fields) )
!> get the diag_yaml file info
do i = 1,num_diag_files
  call diag_get_file_info(trim(diag_yaml_name)//c_null_char, diag_files(i),diag_fields(i), i-1)
enddo
!> Initialize the NULL types
null_field_type%ikind = -999
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!> Set the init value to .true.
diag_table_init = .true.
if (verbose) call diag_error("fms_diag_table_mod","Initialized!",NOTE)
end subroutine fms_diag_table_init
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
type(diag_fields_type)function get_diag_table_field (field_name) result (field)
 character(len=*), intent(IN) :: field_name
 integer :: i
 do i = 1,size(diag_fields)
  write(6,*) diag_files(i)%fname,fms_c2f_string(diag_fields(i)%fname),diag_fields(i)%fname,i
!  write (6,*) diag_files(i)%fname
     if (trim(field_name) == fms_c2f_string(diag_fields(i)%fname)) then
          field = diag_fields(i)
write (6,*) field_name//" Found"

          return
     endif
 enddo
 field = null_field_type

end function get_diag_table_field

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
subroutine fms_write_diag_table 
!character(len=*), intent(in)                 :: diag_table_name 
!character(len=:), intent(out), allocatable   :: diag_table_string



end subroutine fms_write_diag_table

end module fms_diag_table_mod
