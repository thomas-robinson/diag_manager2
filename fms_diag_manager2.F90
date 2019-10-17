module fms_diag_manager2_mod

use fms_diag_concur_mod, only:fms_write_diag_comm, diag_comm_init, diag_comm_exist, fms_diag_comm_type
use fms_diag_data_mod
use fms_diag_table_mod

implicit none

type(fms_diag_comm_type) :: diag_comm !< The diag communicator
public :: fms_diag_manager_init
private :: diag_comm

contains

subroutine fms_diag_manager_init

!> Initialize and check if a diag_comm will be used
   call diag_comm_init(diag_comm)
   call fms_write_diag_comm(diag_comm)
end subroutine fms_diag_manager_init



end module fms_diag_manager2_mod
