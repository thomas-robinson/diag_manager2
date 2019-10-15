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


public :: diag_files_type, diag_fields_type

end module fms_diag_data_mod
