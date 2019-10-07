module fms_diag_table_mod

use iso_c_binding
implicit none

public :: diag_table, fms_diag_table_init, fms_read_diag_table

 character(len=:),allocatable :: diag_table


 character (len=20) :: diag_table_name = "diag_table         "!< This is the filename of the "main" diag table
 logical            :: verbose = .false.
namelist / diag_table_nml / diag_table_name, verbose

logical :: diag_table_init = .false.
integer :: fatal=-143, note=143
contains

subroutine fms_diag_table_init (nml_file)

character(len=*), intent(in), optional  :: nml_file
integer                                 :: io_error
integer                                 :: a
character(len=:), allocatable           :: read_nml_file
integer                                 :: diag_table_unit
integer                                 :: diag_table_len
character(len=:),allocatable            :: diag_len_temp
!> If already initalized, return
if (diag_table_init) then
     if(verbose)call diag_error("fms_diag_table_mod","diag_table already initialized",NOTE)
     return
endif
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
