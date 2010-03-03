module stringdataset_m
    use h5lt
    use amelethdf_m, only : check, hdferr

    implicit none

    character(len=*), parameter :: MSIG="[stringdataset]"

    contains
        ! Reads a long string dataset
        subroutine read_string_dataset2(file_id, path, buf, l, nb)
            integer(hid_t), intent(in) :: file_id
            character(len=*), intent(in) :: path
            integer, intent(in) :: l, nb
            character(len=(l*nb)), intent(inout) :: buf

            call h5ltread_dataset_string_f(file_id, path, buf, hdferr)
            call check(MSIG//"Can't read string dataset for "//path)
        end subroutine read_string_dataset2

        ! Reads a (m x n) string dataset (rows x columns) of l characters
        subroutine read_string_dataset1(file_id, path, data, l, m, n)
            integer(hid_t), intent(in) :: file_id
            character(len=*), intent(in) :: path
            integer, intent(in) :: l, m, n
            character(len=*), dimension(m, n), intent(inout) :: data

            character(len=(l*m*n)) :: buf
            integer :: i, j, offset

            call read_string_dataset2(file_id, path, buf, l, m*n)
            offset = 1
            do i=1,m
                do j=1,n
                    data(i, j) = buf(offset : offset+l-1)
                    offset = offset + l
                enddo
            enddo
        end subroutine read_string_dataset1

        ! Reads the length of strings contains in a dataset
        ! The dataset must be a string dataset (no check)
        function get_dataset_lmn(file_id, path)
            integer(hid_t), intent(in) :: file_id
            character(len=*), intent(in) :: path

            integer, dimension(3) :: get_dataset_lmn
            integer :: type_class, m = 0, n = 0
            integer(hsize_t), dimension(2) :: dims
            integer(size_t) :: type_size

            dims = 0
            call h5ltget_dataset_info_f(file_id, path, dims, type_class, &
                                        type_size, hdferr)
            call check(MSIG//"Can't read string length for : "//path)
            m = dims(1)
            n = dims(2)
            get_dataset_lmn = (/int(type_size), m, n/)
        end function get_dataset_lmn

        ! Read a 1D string vector
        subroutine read_vector(file_id, path, vector)
            integer(hid_t), intent(in) :: file_id
            character(len=*), intent(in) :: path
            character(len=*), dimension(:), allocatable :: vector

            integer, dimension(3) :: lmn

            lmn = get_dataset_lmn(file_id, path)
            if (lmn(3) /= 0 .and. lmn(3) /= 1) then
                hdferr = -1
                call check(MSIG//" "//trim(path)//" must be a (n x 1) or (n) dataset")
            endif
            if (allocated(vector)) deallocate(vector)
            allocate(vector(lmn(2)))
            call read_string_dataset1(file_id, path, vector, lmn(1), lmn(2), 1)
        end subroutine read_vector

        ! Return the index of an element
        function str_index(arr, aelement)
            character(len=*), dimension(:), intent(in) :: arr
            character(len=*), intent(in) :: aelement

            integer :: i, str_index

            str_index = -1
            do i=1, size(arr)
                if (aelement == arr(i)) then
                    str_index = i
                    cycle
                endif
            enddo
        end function str_index

        ! Write a nd string dataset
        subroutine write_nd_dataset(file_id, path, values, values_shape)
            use h5lt

            integer(hid_t), intent(in) :: file_id
            character(len=*), intent(in) :: path
            integer, dimension(:), intent(in) :: values_shape
            character(len=*), dimension(product(values_shape)), &
                intent(in) :: values

            character(len=*), parameter :: MSIG1 = MSIG//"[WRITE_ND_DATASET] "
            integer :: rank
            integer(hsize_t), dimension(:), allocatable :: dims
            integer(hid_t) :: dataspace_id, dset_id
            integer(size_t) :: type_size
            integer(hid_t) :: type_id

            rank = size(values_shape)
            type_size = len(values)

            allocate(dims(rank))
            dims = values_shape

            call h5tcopy_f(H5T_NATIVE_CHARACTER, type_id, hdferr)
            call H5tset_size_f(type_id, type_size, hdferr);
            call check(MSIG1//"Can't get type size")

            ! Create the dataspace
            call h5screate_simple_f(rank, dims, dataspace_id, hdferr)
            call check(MSIG1//"Can't create data space")

            ! Create the dataset
            call h5dcreate_f(file_id, trim(path), type_id, &
                             dataspace_id, dset_id, hdferr)
            call check(MSIG1//"Can't create dataset")

            ! Write data
            call h5dwrite_f(dset_id, type_id, values, &
                            dims, hdferr, H5P_DEFAULT_F)
            call check(MSIG1//"Can't write dataset")

            call h5tclose_f(type_id, hdferr)
            call h5dclose_f(dset_id, hdferr)
            call h5sclose_f(dataspace_id, hdferr)

            deallocate(dims)
        end subroutine write_nd_dataset
end module stringdataset_m
