module vector_m
    use h5lt
    use amelethdf_m, only : check, hdferr, ELENGTH => ELEMENT_NAME_LENGTH
    use simpletype_m, only : single_t, single_to_string, read_single, &
                             clear_content_single, make_single
    use complextype_m, only : read_dataset, write_dataset

    implicit none

    character(len=*), parameter :: MSIG = "[vector]"
    character(len=*), parameter :: V_VECTOR = "vector"

    integer, parameter :: E_VECTOR = 3

    type vector_t
        type(single_t) :: single
        integer, dimension(:), allocatable :: ivalue
        real, dimension(:), allocatable :: rvalue
        complex, dimension(:), allocatable :: cvalue
    end type vector_t

    contains
        ! read a vector_t floating type
        subroutine read(file_id, path, vector)
            implicit none

            integer(hid_t), intent(in) :: file_id
            character(len=*), intent(in) :: path
            type(vector_t), intent(inout) :: vector

            integer(hsize_t), dimension(1) :: dims
            integer :: type_class
            integer(size_t) :: type_size

            call clear_content(vector)

            call read_single(file_id, path, vector%single)
            call h5ltget_dataset_info_f(file_id, path, dims, type_class, &
                                         type_size, hdferr)
            call check(MSIG//"Can't dims rank for "//path)
            if (type_class == H5T_INTEGER_F) then
                allocate(vector%ivalue(dims(1)))
                call h5ltread_dataset_int_f(file_id, path, &
                                            vector%ivalue, dims, hdferr)
                call check(MSIG//"Can't read integer values for "//path)
            else if (type_class == H5T_FLOAT_F) then
                allocate(vector%rvalue(dims(1)))
                call h5ltread_dataset_float_f(file_id, path, &
                                              vector%rvalue, dims, hdferr)
                call check(MSIG//"Can't read float values for "//path)
            else if (type_class == H5T_COMPOUND_F) then
                allocate(vector%cvalue(dims(1)))
                call read_dataset(file_id, path, vector%cvalue)
                call check(MSIG//"Can't read complex values for "//path)
            else
                hdferr = -1
                call check(MSIG//"Can't read type for "//path)
            endif
        end subroutine read

        ! clear vector
        subroutine clear_content(vector)
            implicit none

            type(vector_t), intent(inout) :: vector

            call clear_content_single(vector%single)
            if (allocated(vector%ivalue)) deallocate(vector%ivalue)
            if (allocated(vector%rvalue)) deallocate(vector%rvalue)
            if (allocated(vector%cvalue)) deallocate(vector%cvalue)
        end subroutine clear_content

        ! return a string representation of vector_t
        character(len=250) function to_string(vector)
            implicit none

            type(vector_t), intent(in) :: vector
!            character(len=200) :: buf = ""

            to_string = single_to_string(vector%single)
!            if (allocated(vector%ivalue)) then
!                write(buf, *) vector%ivalue(:)
!            else if (allocated(vector%rvalue)) then
!                write(buf, *) vector%rvalue(:)
!            else if (allocated(vector%cvalue)) then
!                write(buf, *) vector%cvalue(:)
!            endif
!            to_string = trim(to_string)//"\nValue : "//trim(buf)
        end function to_string

        subroutine write(loc_id, path, vector)
            integer(hid_t), intent(in) :: loc_id
            character(len=*), intent(in) :: path
            type(vector_t), intent(in) :: vector
            integer(hsize_t), dimension(1) :: dims

            if (allocated(vector%ivalue)) then
                dims(1) = size(vector%ivalue)
                call h5ltmake_dataset_int_f(loc_id, path, 1, dims, &
                                            vector%ivalue, hdferr)
                call check(MSIG//"Can't make dataset "//path)
            else if (allocated(vector%rvalue)) then
                dims(1) = size(vector%rvalue)
                call h5ltmake_dataset_float_f(loc_id, path, 1, dims, &
                                              vector%rvalue, hdferr)
                call check(MSIG//"Can't make dataset "//path)
            else if (allocated(vector%cvalue)) then
                dims(1) = size(vector%cvalue)
                call write_dataset(loc_id, path, vector%cvalue)
            endif
            call make_single(loc_id, path, vector%single)
        end subroutine write
end module vector_m
