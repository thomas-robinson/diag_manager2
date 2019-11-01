module fms_diag_data_mod

use iso_c_binding
implicit none
include 'mpif.h'

!> Supported averaging intervals
integer, parameter :: monthly = 30
integer, parameter :: daily = 24 
integer, parameter :: diurnal = 2
integer, parameter :: yearly = 12
integer, parameter :: no_diag_avergaing = 0
integer, parameter :: instantaneous = 0
integer, parameter :: three_hourly = 3
integer, parameter :: six_hourly = 6
!integer, parameter :: seasonally = 180
!> Supported type/kind of the variable
!integer, parameter :: r16=16
integer, parameter :: r8 = 8
integer, parameter :: r4 = 4
integer, parameter :: i8 = -8
integer, parameter :: i4 = -4
integer, parameter :: string = 19 !< s is the 19th letter of the alphabet
integer, parameter :: null_type_int = -999
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
     character (c_char) :: fname (20)
     character (c_char) :: var(20)
     character (c_char) :: files(20)
     integer (c_int)    :: ikind
     character (c_char) :: skind(20)
     character (c_char) :: reduction(20)
     character (c_char) :: all_all(4)
     character (c_char) :: region(50)
     character (c_char) :: regcoord(50)
     character (c_char) :: module_location(20)
     character (c_char) :: key(8)
end type diag_fields_type
!> Placeholder for fms2_io file object type.
type fms_io_obj
     character(len=100) :: fname
end type fms_io_obj




integer :: fatal=-143, note=143, warning=143*2
public :: diag_files_type, diag_fields_type
public :: monthly, daily, diurnal, yearly, no_diag_avergaing, instantaneous
public :: three_hourly, six_hourly, r8, r4, i8, i4, string
public :: diag_error, fms_io_obj
public :: fatal, note, warning


contains
subroutine diag_error(sub,mess,lev,mycomm)
character(len=*), intent(in)  :: sub
character(len=*), intent(in)  :: mess
integer, intent(in)           :: lev
integer, intent(in), optional :: mycomm
character(len=15)             :: slev
logical                       :: mpi_on
integer                       :: ierr
integer                       :: comm
integer                       :: rk
mpi_on = .false.
if (lev == note) then
     slev = "NOTE: "
     call mpi_initialized(mpi_on,ierr)
elseif (lev == WARNING) then
     slev = "WARNING: "
else
     slev = "FATAL: "
endif

if (lev == note .and. mpi_on) then
     if (present(mycomm)) then
          comm = mycomm
     else
          comm = MPI_COMM_WORLD
     endif
     call mpi_comm_rank(comm, rk, ierr)
     if (rk == 0) then
          write (6,*)trim(slev)//sub,":: ",mess
     endif
else
     write (6,*)trim(slev)//sub,":: ",mess
endif

if (lev==fatal) then
  if (mpi_on) then
     call MPI_ABORT
  else
     stop
  endif
endif

end subroutine diag_error

end module fms_diag_data_mod
