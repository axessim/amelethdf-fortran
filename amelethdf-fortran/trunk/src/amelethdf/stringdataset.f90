module stringdataset_m
    use h5lt
    use amelethdf_m, only : check, hdferr

    implicit none

    character(len=*), parameter :: MSIG="[stringdataset]"

    contains
        ! Reads a long string dataset
        subroutine read_string_dataset2(file_id, path, buf, l, nb)
            implicit none

            integer(hid_t), intent(in) :: file_id
            character(len=*), intent(in) :: path
            integer, intent(in) :: l, nb
            character(len=(l*nb)), intent(inout) :: buf


            call h5ltread_dataset_string_f(file_id, path, buf, hdferr)
            call check(MSIG//"Can't read string dataset for "//path)
        end subroutine read_string_dataset2

        ! Reads a (m x n) string dataset (rows x columns) of l characters
        subroutine read_string_dataset1(file_id, path, data, l, m, n)
            implicit none

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
            implicit none

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
            get_dataset_lmn = (/type_size, m, n/)
        end function get_dataset_lmn

        subroutine read_vector(file_id, path, vector)
            implicit none

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
end module stringdataset_m
