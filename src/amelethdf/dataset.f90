module dataset_m
    use h5lt
    use amelethdf_m, only : check, hdferr, ELENGTH => ELEMENT_NAME_LENGTH
    use simpletype_m, only : single_t, single_to_string, read_single, &
                             clear_content_single
    use complextype_m, only : read_dataset

    implicit none

    character(len=*), parameter :: MSIG = "[dataset]"
    character(len=*), parameter :: V_DATASET = "dataset"
    integer, parameter :: P_INTEGER = 1, P_REAL = 2, P_COMPLEX = 3

    type dataset_t
        type(single_t) :: single
        integer, dimension(:), allocatable :: dims
        integer, dimension(:), allocatable :: ivalue
        real, dimension(:), allocatable :: rvalue
        complex, dimension(:), allocatable :: cvalue
    end type dataset_t

    contains
        ! read a dataset_t floating type
        subroutine read(file_id, path, dataset)
            implicit none

            integer(hid_t), intent(in) :: file_id
            character(len=*), intent(in) :: path
            type(dataset_t), intent(inout) :: dataset

            integer(hsize_t), dimension(:), allocatable :: dims
            integer :: type_class, rank, length
            integer(size_t) :: type_size

            call clear_content(dataset)

            call read_single(file_id, path, dataset%single)
            ! read the rank
            call h5ltget_dataset_ndims_f(file_id, path, rank, hdferr)
            call check(MSIG//"Can't read rank for "//path)
            ! read the dims
            allocate(dims(rank))
            call h5ltget_dataset_info_f(file_id, path, dims, type_class, &
                                         type_size, hdferr)
            call check(MSIG//"Can't dims rank for "//path)
            allocate(dataset%dims(rank))
            dataset%dims(:) = dims(:)

            length = product(dims)
            if (type_class == H5T_INTEGER_F) then
                allocate(dataset%ivalue(length))
                call h5ltread_dataset_int_f(file_id, path, &
                                            dataset%ivalue, dims, hdferr)
                call check(MSIG//"Can't read integer values for "//path)
            else if (type_class == H5T_FLOAT_F) then
                allocate(dataset%rvalue(length))
                call h5ltread_dataset_float_f(file_id, path, &
                                              dataset%rvalue, dims, hdferr)
                call check(MSIG//"Can't read flaot values for "//path)
            else if (type_class == H5T_COMPOUND_F) then
                allocate(dataset%cvalue(length))
                call read_dataset(file_id, path, dataset%cvalue)
                call check(MSIG//"Can't read complex values for "//path)
            else
                hdferr = -1
                call check(MSIG//"Can't read type for "//path)
            endif
        end subroutine read

        ! Given a shape and indices returns the index the element
        ! in memory
        ! For a (/6, 8, 4, 9/) and (/3, 5, 3, 7/) returns 1275
        function get_index(indices, ishape) result(res)
            implicit none

            integer, dimension(:), intent(in) :: indices, ishape
            integer :: res, i

            res = indices(1)
            do i=2, size(indices)
                res = res + product(ishape(:i-1))*(indices(i)-1)
            enddo
        end function get_index

        ! Check
        subroutine get_check(dataset, indices, datatype)
            implicit none

            type(dataset_t), intent(in) :: dataset
            integer, dimension(:), intent(in) :: indices
            integer :: datatype

            if (datatype == P_INTEGER) then
               if (.not. allocated(dataset%ivalue)) then
                    hdferr = -1
                    call check(MSIG//"Dataset has not integer values")
                endif
            else if (datatype == P_REAL) then
                if (.not. allocated(dataset%rvalue)) then
                    hdferr = -1
                    call check(MSIG//"Dataset has not real values")
                endif
            else if (datatype == P_COMPLEX) then
                if (.not. allocated(dataset%cvalue)) then
                    hdferr = -1
                    call check(MSIG//"Dataset has not complex values")
                endif
            endif

            if (size(indices) /= size(dataset%dims)) then
                hdferr = -1
                call check(MSIG//"Wrong indices length")
            endif
        end subroutine get_check

        ! Given an integer dataset return the value at indices
        function get_integer(dataset, indices) result (res)
            implicit none

            type(dataset_t), intent(in) :: dataset
            integer, dimension(:), intent(in) :: indices
            integer :: res

            call get_check(dataset, indices, P_INTEGER)
            res = dataset%ivalue(get_index(indices, dataset%dims))
        end function get_integer

        ! Given an real dataset return the value at indices
        function get_real(dataset, indices) result (res)
            implicit none

            type(dataset_t), intent(in) :: dataset
            integer, dimension(:), intent(in) :: indices
            real :: res


            call get_check(dataset, indices, P_REAL)
            res = dataset%rvalue(get_index(indices, dataset%dims))
        end function get_real

        ! Given an complex dataset return the value at indices
        function get_complex(dataset, indices) result (res)
            implicit none

            type(dataset_t), intent(in) :: dataset
            integer, dimension(:), intent(in) :: indices
            complex :: res


            call get_check(dataset, indices, P_COMPLEX)
            res = dataset%cvalue(get_index(indices, dataset%dims))
        end function get_complex

        ! clear dataset
        subroutine clear_content(dataset)
            implicit none

            type(dataset_t), intent(inout) :: dataset

            call clear_content_single(dataset%single)
            if (allocated(dataset%dims)) deallocate(dataset%dims)
            if (allocated(dataset%ivalue)) deallocate(dataset%ivalue)
            if (allocated(dataset%rvalue)) deallocate(dataset%rvalue)
            if (allocated(dataset%cvalue)) deallocate(dataset%cvalue)
        end subroutine clear_content

        ! return a string representation of dataset_t
        character(len=250) function to_string(dataset)
            implicit none

            type(dataset_t), intent(in) :: dataset

            to_string = single_to_string(dataset%single)
        end function to_string
end module dataset_m
