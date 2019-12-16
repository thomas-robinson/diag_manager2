!! \brief Contains OpenMP-based averaging routines
!!
!! \description This odule for performssome statistical (e.g. average) operations
!! on data arrays. Typically, With the input array A being N-dimensioanl, an
!! (N-1)-dimensional array is calculated containing the averages for the Nth (righmost) dimension.
!! TODO: 1) Test the actual offloading. See "omp declare target" statement in the
!!         1D version and copy to tothers if works.
!! TODO 2) Test on alternate compiler and GPU
!! TODO 3) Determine loggin error policy (related to array allocation if any)
!! TODO 4) Delete all experimental subroutines

module fms_diag_omp_aux
    use omp_lib
    implicit none
    private get_average_1D, get_average_2D, get_average_3D, get_average_4D, &
    get_average_5D, alloc_subarray_3D, alloc_subarray_4D

    !!Interface for calculating the averages.
    interface get_average
        procedure get_average_1D, get_average_2D, get_average_3D, &
            get_average_4D, get_average_5D
    end interface

    !Interface for allocating an array of one less dimention than the argument array.
    !It is assumed the RHS dimention will is the one that will not be in the newly
    !allocated (sub) array
    interface alloc_subarray
        procedure alloc_subarray_3D, alloc_subarray_4D
    end interface

    contains


    !! Return the average of the data in the entire 1D input data array.
    !! If num_threads .ne. 0 and iff a (GPU) device is not available
    !! for offloading, then OMP number of threads used are set to num_threads
    subroutine get_average_1D( the_data, the_average, num_threads)
        real (kind = 8),  intent(in) :: the_data (:)
        integer (kind=4), intent(in) :: num_threads
        real(kind=8), intent (in out) :: the_average
        integer (kind=8) NT, i
        integer :: gpu_num_threads

        NT = size (the_data,1) !! The number of data points
        the_average = 0.0d0
        !$opm declare target  !!TODO: verify

            gpu_num_threads = num_offloading_threads(num_threads)

            !$omp parallel do reduction(+ : the_average) num_threads(gpu_num_threads)
                do i = 1, NT
                 the_average = the_average +  the_data(i)
                end do

        the_average= the_average/ NT

    end subroutine get_average_1D

    !! Calculate the 1D array contaning averages over the rightmost dimension
    !! of the input 2D data array.
    !! CDIM specifies the column (dimension) over which to average.
    !! If num_threads .ne. 0 and iff a (GPU) device is not available
    !! for offloading, then OMP number of threads used are set to num_threads
    subroutine get_average_2D( the_data, the_average, num_threads)
        real (kind = 8),  intent(in) :: the_data (:,:)
        integer (kind=4), intent(in) :: num_threads
        real(kind=8), intent (in out) :: the_average(:)
        integer (kind=4) NT, i

        NT = size (the_data,2)

        the_average = 0.0d0

        !$opm declare target

            !$omp parallel do reduction(+ : the_average)
            do i = 1, NT
             the_average(:) = the_average(:)+  the_data(:,i)
            end do

       the_average = the_average/ NT

    end subroutine get_average_2D

    !! Calculate the 2D array contaning averages over the rightmost dimension
    !! of the input 3D data array.
    !! CDIM specifies the column (dimension) over which to average.
    !! If num_threads .ne. 0 and iff a (GPU) device is not available
    !! for offloading, then OMP number of threads used are set to num_threads
    subroutine get_average_3D( the_data, the_average, num_threads)
        real (kind = 8),  intent(in) :: the_data (:,:,:)
        integer (kind=4), intent(in) :: num_threads
        real(kind=8), intent(in out) :: the_average(:,:)
        integer :: NT,i

        NT = size (the_data,3)

        the_average = 0.0d0

        !$opm declare target

        !$omp parallel do reduction(+ : the_average)
        do i = 1, NT
            the_average(:,:) = the_average(:,:) + the_data(:,:,i)
        end do

       the_average = the_average/ NT


    end subroutine get_average_3D

    !! Calculate the 3D array contaning averages over the rightmost dimension
    !! of the input 4D data array.
    !! CDIM specifies the column (dimension) over which to average.
    !! If num_threads .ne. 0 and iff a (GPU) device is not available
    !! for offloading, then OMP number of threads used are set to num_threads
    subroutine get_average_4D( the_data, the_average, num_threads)
        real (kind = 8),  intent(in) :: the_data (:,:,:,:)
        integer (kind=4), intent(in) :: num_threads
        real(kind=8), intent(in out) :: the_average(:,:,:)
        integer :: NT,i

        NT = size (the_data,4)

        the_average = 0.0d0

        !$opm declare target

        !$omp parallel do reduction(+ : the_average)
        do i = 1, NT
            the_average(:,:,:) = the_average(:,:,:) + the_data(:,:,:,i)
        end do

        the_average = the_average / NT


    end subroutine get_average_4D

    subroutine get_average_5D( the_data, the_average, num_threads)
        real (kind = 8),  intent(in) :: the_data (:,:,:,:,:)
        integer (kind=4), intent(in) :: num_threads
        real(kind=8), intent(in out) :: the_average(:,:,:,:)
        integer :: NT,i

        NT = size (the_data,5)

        the_average = 0.0d0

        !$opm declare target

        !$omp parallel do reduction(+ : the_average)
        do i = 1, NT
            the_average(:,:,:,:) = the_average(:,:,:,:) + the_data(:,:,:,:,i)
        end do

        the_average = the_average / NT

    end subroutine get_average_5D


!!Determine the number of threads used durring offloading to GPU,
!!If a gpu is not available, return the number of cpu_threads to use
!!WARNIG: This function needs to be called within an "!$omp target ..."
function num_offloading_threads(cpu_num_threads) result (gpu_num_threads)
       integer , intent(in) :: cpu_num_threads
       logical  (kind = 4) :: initial_device
       integer  :: gpu_num_threads
       integer  :: max_gpu_num_threads

       gpu_num_threads = cpu_num_threads

       max_gpu_num_threads = 1024

       initial_device = omp_is_initial_device()
       !!initial_device = .True.

       if ((cpu_num_threads .ne. 0) .and. (initial_device .eqv. .False.)) then
          gpu_num_threads = max_gpu_num_threads
       end if
end function num_offloading_threads


    subroutine alloc_subarray_3D( the_data, the_average)
        real (kind = 8),  intent(in) :: the_data (:,:,:)
        real(kind=8), allocatable, intent(in out) :: the_average(:,:)
        allocate (the_average(1:size(the_data,1), 1:size(the_data,2)))
     !TODO: Determine allocation status
     end subroutine alloc_subarray_3D

      subroutine alloc_subarray_4D( the_data, the_average)
        real (kind = 8),  intent(in) :: the_data (:,:,:,:)
        real(kind=8), allocatable, intent(in out) :: the_average(:,:,:)
         allocate (the_average(1: size(the_data,1), 1: size(the_data,2), 1: size(the_data,3)))
     !TODO: Determine allocation status
     end subroutine alloc_subarray_4D


end module fms_diag_omp_aux

