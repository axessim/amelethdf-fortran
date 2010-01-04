module arrayset_m
    use h5lt
    use amelethdf_m, only : check, hdferr, EL => ELEMENT_NAME_LENGTH, &
                                           AL => ABSOLUTE_PATH_NAME_LENGTH, &
                            read_number_of_children, read_children_name

    use simpletype_m, only : single_t, single_to_string, read_single
    use vector_m, only : vector_t, read_vector => read, &
                         clear_content_vector => clear_content
    use dataset_m, only : dataset_t, read_dataset => read, &
                          clear_content_dataset => clear_content

    implicit none

    integer, parameter :: DIM_LENGTH = 1

    character(len=*), parameter :: MSIG = "[arrayset]"
    character(len=*), parameter :: V_VECTOR = "vector"
    character(len=*), parameter :: P_DATA = "/data"
    character(len=*), parameter :: P_DIMS = "/ds/"
    character(len=*), parameter :: P_DIM = P_DIMS//"dim"
    character(len=*), parameter :: P_DIM1 = P_DIM//"1"

    type arrayset2_t
        type(single_t) :: single
        type(vector_t) :: data
        type(vector_t) :: dim1
    end type arrayset2_t

    type arrayset_t
        type(single_t) :: single
        type(dataset_t) :: data
        type(vector_t), dimension(:), allocatable :: dims
    end type arrayset_t

    contains
        ! reads an arrayset_t
        subroutine read(file_id, path, arr)
            integer(hid_t), intent(in) :: file_id
            character(len=*), intent(in) :: path
            type(arrayset_t), intent(inout) :: arr

            integer :: nb_dims, i
            character(len=DIM_LENGTH) :: c

            call read_single(file_id, path, arr%single)

            call read_dataset(file_id, trim(path)//P_DATA, arr%data)
            nb_dims = read_number_of_children(file_id, trim(path)//P_DIMS)
            allocate(arr%dims(nb_dims))
            do i=1, nb_dims
                write(c, '(i1)') i
                call read_vector(file_id, trim(path)//P_DIM//trim(c), arr%dims(i))
            enddo
        end subroutine read

        ! clear an arrayset_t
        subroutine clear_content(arr)
            type(arrayset_t), intent(inout) :: arr

            integer :: i

            call clear_content_dataset(arr%data)
            if (.not. allocated(arr%dims)) return
            do i=1,size(arr%dims)
                call clear_content_vector(arr%dims(i))
            enddo
        end subroutine clear_content

        ! read an arrayset2_t floating type
        subroutine read2(file_id, path, arr)
            integer(hid_t), intent(in) :: file_id
            character(len=*), intent(in) :: path
            type(arrayset2_t), intent(inout) :: arr

            call read_single(file_id, path, arr%single)
            call read_vector(file_id, trim(path)//P_DATA, arr%data)
            call read_vector(file_id, trim(path)//P_DIM1, arr%dim1)
        end subroutine read2

        ! clear an arrayset2_t
        subroutine clear_content2(arr)
            type(arrayset2_t), intent(inout) :: arr

            call clear_content_vector(arr%data)
            call clear_content_vector(arr%data)
        end subroutine clear_content2

        ! return a string representation of arrayset2_t
        character(len=AL) function to_string2(arr)
            type(arrayset2_t), intent(in) :: arr

            to_string2(:AL) = single_to_string(arr%single)
        end function to_string2

        subroutine write_arrayset_dim(loc_id, path, vector, dim_index)
            integer(hid_t), intent(in) :: loc_id
            character(len=*), intent(in) :: path
            type(vector_t), intent(in) :: vector
            integer, intent(in) :: dim_index
        end subroutine write_arrayset_dim
end module arrayset_m
