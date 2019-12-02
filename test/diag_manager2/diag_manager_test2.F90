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
real(kind=8) :: t2d(np,np,nt)
 class(fms_diag_object),allocatable  :: id_tdata, id_2d, id_scalar
integer :: i,j,k,t,tile
write (6,*) "initialize diag manager"
   call fms_diag_manager_init() 
write (6,*) "Diag manager initialized"
!> Register tdata
write (6,*) "Regster tdata"
!     id_tdata = fms_register_diag_field (id_tdata, tdata, "tdata", (/"atmos_daily"/)) 
     id_tdata = fms_register_diag_field ("tmod", "tdata", (/1,2,3/), 1)
!     id_2d = fms_register_diag_field ("tmod", "tdata", (/1,2/), 1)

!> create test data set
tdata = 0.0d0
t2d = 1.0d0
is=1 ; js=1 ; ks=1
     do t=1,nt
     do k=ks,nz
     do j=js,np
     do i=is,np
          tdata(i,j,k,t) = dble(t)
          t2d(i,j,t) = (t2d(i,j,t)*dble(i)-dble(j))+dble(t)
     enddo
     enddo
     enddo

          if (id_tdata > 0) then 
               call send_data(id_tdata, tdata(:,:,:,t), t, is, js, ks)
          endif
          if (id_2d > 0) then
               write(6,*) "Calling send data"
               call send_data(id_2d, t2d(:,:,t),t)
          endif
     enddo

 select type (id_tdata)
  type is (fms_diag_object)
   write (6,*) "The type has not been changed"
  type is (fms_diag_object_3d)
   write (6,*) "The type is 3d"
  type is (fms_diag_object_2d)
   write (6,*) "The type is 2d"
  class default
   write (6,*) "Not one of the specified types"
 end select

! call which_type(id_tdata)
 select type (id_2d)
  type is (fms_diag_object)
   write (6,*) "The type has not been changed"
  type is (fms_diag_object_3d)
   write (6,*) "The type is 3d"
  type is (fms_diag_object_2d)
   write (6,*) "The type is 2d"
  class default
   write (6,*) "Not one of the specified types"
 end select




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
