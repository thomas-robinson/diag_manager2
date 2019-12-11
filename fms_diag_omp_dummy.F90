!! \brief Contains miscellanous temporary and experimental routines

! \description Contents of this file are temporary and experimental and
!! may be deleted without notice. This file is used
!! to support the development of module diag_manager_omp_aux module and its test program
!!
module fms_diag_omp_dummy
    use omp_lib
    implicit none

    contains

    ! Calculate the 4D array contaning averages over the rightmost dimension
    ! of the input 5D data array.
    ! CDIM specifies the column (dimension) over which to average.
    ! If num_threads .ne. 0 and iff a (GPU) device is not available
    ! for offloading, then OMP number of threads used are set to num_threads
    subroutine get_average_CMXN( data, CDIM, the_average, num_threads)
        real (kind = 8),  intent(in) :: data (:,:)
        integer (kind=4), intent(in) :: CDIM
        integer (kind=4), intent(in) :: num_threads
        real(kind=8), intent (in out) :: the_average
        integer (kind=4) N, M
        real(kind=8) the_sum
        integer :: i

        N = size (data,1) ! length of one column==number of rows
        M = size (data,2) ! length of one row == number of columns
        the_sum = 0;

        !$omp parallel do reduction(+ : the_sum)
        do i = 1, M
            the_sum = the_sum +  data(CDIM,i)
        end do

        the_average = the_sum / M
    end subroutine get_average_CMXN


    !This is a test subroutine.
    !Returns the average of an input vector.
    subroutine get_average_v1( data, the_average, numThreads)
        real (kind = 8), intent(in) :: data (:)
        integer (kind=4), intent(in) :: numThreads
        real(kind=8), intent (in out) :: the_average
        integer (kind=4) N, iter_chunk
        real(kind=8) the_sum, local_sum, t1, t2;
        real(kind=8) elapsed(0:3)
        integer :: i

        print *, "Entering subroutine getAverage"

        N = size (data)
        the_sum = 0;
        iter_chunk = 5  !TODO: find rational amount and method of determining.

        call omp_set_num_threads( numThreads )

        !compare with simple parallel for and parallel for reduction
        ! each thread gets local copy of local_sum ...
        !$omp parallel private(local_sum, t1, t2) shared(the_sum)
        local_sum = 0
        t1 = omp_get_wtime()

        !the array is distributed statically between threads. Also try dynamic?
        !$omp do schedule(static,iter_chunk)
        do i = 1, N
            local_sum = local_sum +  data(i)
        end do

        t2 = omp_get_wtime();

        !each thread calculated its local_sum. ALl threads have to add to
        !the global sum. It is critical that this operation is atomic.

        !$omp critical
        the_sum = the_sum +  local_sum
        !$omp end critical

        elapsed(omp_get_thread_num()) = t2 - t1
        !$omp end parallel

        do i = 0, numThreads - 1
            print *, "Elapsed time for thread " ,i, " = ",elapsed(i)
        end do

        the_average = the_sum / N
    end subroutine get_average_v1

    !This is a test subroutine.
    !Returns the average of an input vector.
    !The input data is a 2D NxM  array.
    ! CDIM specifies the column (dimension) over which to average.
    ! Asummed column (given fixed CDIM column) is to varry.
    subroutine get_average_MN( data, CDIM,  the_average, numThreads)
        real (kind = 8), intent(in) :: data (:,:)
        integer (kind=4), intent(in) :: CDIM
        integer (kind=4), intent(in) :: numThreads
        real(kind=8), intent (in out) :: the_average
        integer (kind=4) N, M, iter_chunk
        real(kind=8) the_sum, local_sum, t1, t2;
        real(kind=8) elapsed(0:numThreads)
        integer :: i
        logical  (kind = 4) :: initialDevice

        print *, "Entering subroutine getAverage"

        N = size (data,1) ! length of one column==number of rows
        M = size (data,2) ! length of one row == number of columns
        the_sum = 0;
        iter_chunk = 5  !TODO: find rational amount and method of determining.

        !compare with simple parallel for and parallel for reduction
        !!! omp parallel for reduction (+:the_sum)
        ! each thread gets local copy of local_sum ...
        !$omp parallel private(local_sum, t1, t2) shared(the_sum)
        local_sum = 0
        t1 = omp_get_wtime()
        !$omp do schedule(static,10)
        do i = 1, M
            local_sum = local_sum +  data(CDIM,i)
        end do

        t2 = omp_get_wtime();

        !each thread calculated its local_sum. ALl threads have to add to
        !the global sum. It is critical that this operation is atomic.

        !$omp critical
        the_sum = the_sum +  local_sum
        !$omp end critical

        elapsed(omp_get_thread_num()) = t2 - t1
        !$omp end parallel

        do i = 0, numThreads - 1
            print *, "Elapsed time for thread " ,i, " = ",elapsed(i)
        end do

        the_average = the_sum / M
    end subroutine get_average_MN



end module fms_diag_omp_dummy

