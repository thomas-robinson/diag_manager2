module fms_diag_data_mod

use iso_c_binding
implicit none

!> Matches C struct 
type, bind(c) :: diag_files_type
     character (c_char) :: fname (20)
     character (c_char) :: frequnit (7)
     integer (c_int)    :: freq
     character (c_char) :: timeunit(7)
     character (c_char) :: unlimdim(8)
     character (c_char) :: key(8)
end type diag_files_type

type, bind(c) :: diag_fields_type
     character (c_char) :: fname
     character (c_char) :: var
     character (c_char) :: files
     integer (c_int)    :: intkind
     character (c_char) :: skind
     character (c_char) :: egion
     character (c_char) :: regcoord
     character (c_char) :: module_location
     character (c_char) :: key
end type diag_fields_type
integer :: fatal=-143, note=143, warning=143*2

public :: diag_files_type, diag_fields_type, diag_error
public :: fatal, note, warning


contains
subroutine diag_error(sub,mess,lev, mpi_on)
character(len=*), intent(in)  :: sub
character(len=*), intent(in)  :: mess
integer, intent(in)           :: lev
logical, intent(in), optional :: mpi_on
character(len=15)             :: slev

if (lev == note) then
     slev = "NOTE: "
elseif (lev == WARNING) then
     slev = "WARNING: "
else
     slev = "FATAL: "
endif
write (6,*)trim(slev)//sub,":: ",mess
if (lev==fatal) then
  if (present(mpi_on) .and. mpi_on) then
     call MPI_ABORT
  else
     stop
  endif
endif

end subroutine diag_error

end module fms_diag_data_mod
