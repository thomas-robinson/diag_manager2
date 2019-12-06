module c_to_fortran_mod 
use iso_c_binding

contains

pure function fms_c2f_string (cstring) result(fstring)
 character(c_char), intent(in) :: cstring(*) !< The C string to convert to fortran
 character(len=:), allocatable :: fstring    !< The fortran string returned
 integer :: length !< The string length
 integer :: i
 length = 1
! call c_f_pointer (cstring,
 c_loop: do 
     if (cstring(length) == c_null_char) then
          length = length - 1
          exit c_loop
     endif
     length = length + 1
 enddo c_loop
 allocate(character(len=length) :: fstring)
 do i = 1,length
     fstring(i:i) = cstring(i)
 enddo
 
end function fms_c2f_string


end module c_to_fortran_mod


