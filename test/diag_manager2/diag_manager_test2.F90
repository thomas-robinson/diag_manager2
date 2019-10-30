program diag_manager_test2

use fms_diag_manager2_mod

integer , parameter :: np = 192
integer, parameter :: nz = 48
integer, parameter :: ntiles = 6
integer, parameter :: time_step = 900 
!integer, parameter :: nt = 4*24*31
integer, parameter :: nt = 96
integer :: is, js, ks
integer :: ierr
real(kind=8) :: tdata(np,np,nz,nt)
 class(fms_diag_object),allocatable  :: id_tdata
integer :: i,j,k,t,tile
write (6,*) "initialize diag manager"
   call fms_diag_manager_init() 
write (6,*) "Diag manager initialized"
!> Register tdata
write (6,*) "Regster tdata"
!     id_tdata = fms_register_diag_field (id_tdata, tdata, "tdata", (/"atmos_daily"/)) 
     id_tdata = fms_register_diag_field ("tdata", (/"atmos_daily"/))
!> Check the type
 select type (id_tdata)
          type is (fms_diag_object_scalar)
               write (6,*) "The type is fms_diag_object_scalar"
          type is (fms_diag_object_2d)
               write (6,*) "The type is fms_diag_object_2d" 
          type is (fms_diag_object_3d)
               write (6,*) "The type is fms_diag_object_3d" 
          type is (fms_diag_object_4d)
               write (6,*) "The type is fms_diag_object_4d" 
          type is (fms_diag_object_5d)
               write (6,*) "The type is fms_diag_object_5d"
          type is (fms_diag_object)
               write (6,*) "The type is fms_diag_object", id_tdata%diag_id
          class default
               write (6,*) "The type is all wrong"
 end select
!> create test data set
tdata = 0.0d0
is=1 ; js=1 ; ks=1
     do t=1,nt
     do k=ks,nz
     do j=js,np
     do i=is,np
          tdata(i,j,k,t) = dble(t)
     enddo
     enddo
     enddo
              if (t==1) then
                select type (id_tdata)
               type is (fms_diag_object)
                    write (6,*) "This should equal 1", id_tdata%diag_id
               class default
                    call diag_error("MAIN","id_tdata was not properly initialized",fatal)
                 end select
              endif
          call fms_send_data(id_tdata, tdata(:,:,:,t), t, is, js, ks)
              if (t==1 .or. t==nt) then
                select type (id_tdata)
               type is (fms_diag_object)
                    call diag_error("MAIN","id_tdata was not properly reallocated",fatal)
               type is (fms_diag_object_3d)
                    write (6,*) "This should equal 1", id_tdata%diag_id
               class default
                    call diag_error("MAIN","id_tdata was not properly reset",fatal)
                 end select          
              endif
     enddo


end program diag_manager_test2


subroutine which_type (obj)
use fms_diag_manager2_mod
 class(fms_diag_object) :: obj
 select type (obj)
          type is (fms_diag_object_scalar)
               write (6,*) "The type is fms_diag_object_scalar"
          type is (fms_diag_object_2d)
               write (6,*) "The type is fms_diag_object_2d" 
          type is (fms_diag_object_3d)
               write (6,*) "The type is fms_diag_object_3d" 
          type is (fms_diag_object_4d)
               write (6,*) "The type is fms_diag_object_4d" 
          type is (fms_diag_object_5d)
               write (6,*) "The type is fms_diag_object_5d"
          type is (fms_diag_object)
               write (6,*) "The type is fms_diag_object"
          class default
               write (6,*) "The type is all wrong"
 end select
end subroutine which_type
