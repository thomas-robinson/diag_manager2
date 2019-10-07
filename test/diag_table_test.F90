program diag_table_test

use fms_diag_table_mod, only: fms_diag_table_init

character(len=100) :: input_nml

!call fms_diag_table_init()

open (unit=29, file="/lustre/f2/dev/gfdl/Thomas.Robinson/diag_rewrite/diag_manager2/test/input.nml", &
          status="old", access="stream")
read (29,iostat=i) input_nml
call fms_diag_table_init(input_nml)


end program diag_table_test
