!
! This module provide utilities to handle multi file capabilities
! /externalElement reading.
!
module externalelement_m
    use h5lt
    use amelethdf_m, only : check, hdferr, EL => ELEMENT_NAME_LENGTH, &
                                           AL => ABSOLUTE_PATH_NAME_LENGTH
    use stringdataset_m, only : get_dataset_lmn, read_string_dataset1
    use set_m, only : set_t, in, add, str_index => get_index, clean

    implicit none

    character(len=*), parameter :: MSIG="[externalelement]"
    integer(hid_t) :: main_file_id
    integer :: current_index

    type externalelement_t
        character(len=AL) :: internal_name, file_name, external_name
        integer(hid_t) :: file_id
    end type externalelement_t

    contains
        subroutine read(loc_id, path, external_elements)
            integer(hid_t), intent(in) :: loc_id
            character(len=*), intent(in) :: path
            type(externalelement_t), dimension(:), allocatable, &
                intent(inout) :: external_elements

            character(len=AL), dimension(:,:), allocatable :: buf
            integer, dimension(3) :: lmn
            integer :: i

            lmn = get_dataset_lmn(loc_id, path)
            print *, "lmn : ", lmn(:)
            allocate(buf(lmn(3), lmn(2)))
            call read_string_dataset1(loc_id, path, buf, lmn(1), lmn(3), lmn(2))
            allocate(external_elements(lmn(3)))
            do i=1,lmn(3)
                external_elements(i)%internal_name = buf(i, 1)
                external_elements(i)%file_name = buf(i, 2)
                external_elements(i)%external_name = buf(i, 3)
            enddo
        end subroutine read

        ! Find out the path in external_elements and return the index
        ! Set the global current_index to the found value
        function get_index(external_elements, path)
            type(externalelement_t), dimension(:), intent(in) :: external_elements
            character(len=*), intent(in) :: path
            integer :: i, get_index

            get_index = -1
            do i=1, size(external_elements)
                if (path==external_elements(i)%internal_name) then
                    get_index = i
                endif
            enddo
            current_index = get_index
        end function get_index

        function get_file(external_elements, apath) result(afile)
            type(externalelement_t), dimension(:), intent(in) :: external_elements
            character(len=*), intent(in) :: apath
            character(len=AL) :: afile
            integer :: id

            afile = ""
            id = get_index(external_elements, apath)
            if (id>0) afile = trim(external_elements(id)%file_name)
        end function

        function get_file_id(external_elements, apath) result(file_id)
            type(externalelement_t), dimension(:), intent(in) :: external_elements
            character(len=*), intent(in) :: apath
            integer(hid_t) :: file_id
            integer :: id

            file_id = main_file_id
            id = get_index(external_elements, apath)
            if (id>0) file_id = external_elements(id)%file_id
        end function

        function get_path(external_elements, path) result(apath)
            type(externalelement_t), dimension(:), intent(in) :: external_elements
            character(len=*), intent(in) :: path
            character(len=AL) :: apath
            integer :: id

            apath = ""
            id = get_index(external_elements, path)
            if (id>0) apath = trim(external_elements(id)%external_name)
        end function

        ! Open all external files and store the id
        subroutine open_external_files(external_elements)
            type(externalelement_t), dimension(:), intent(inout) :: external_elements
            character(AL) :: name
            integer :: i, j
            integer(hid_t) :: file_id
            integer(hid_t), dimension(size(external_elements)) :: buf_id
            type(set_t) :: buf
            allocate(buf%array(size(external_elements)))
            call clean(buf)

            do i=1,size(external_elements)
                name = external_elements(i)%file_name
                if (.not. in(buf, name)) then
                    call h5fopen_f(trim(name), &
                        H5F_ACC_RDONLY_F, file_id, hdferr, H5P_DEFAULT_F)
                    call check("Can't open "//trim(name))
                    call add(buf, name)
                    external_elements(i)%file_id = file_id
                    buf_id(buf%number_of_element) = file_id
                else
                    j = str_index(buf, name)
                    external_elements(i)%file_id = buf_id(j)
                endif
            enddo
        end subroutine open_external_files

        ! Close all external files
        subroutine close_external_files(external_elements)
            type(externalelement_t), dimension(:), intent(in) :: external_elements

            character(AL) :: name
            integer(hid_t) :: file_id
            integer :: i
            type(set_t) :: buf

            allocate(buf%array(size(external_elements)))
            call clean(buf)
            do i=1,size(external_elements)
                name = external_elements(i)%file_name
                if (.not. in(buf, name)) then
                    file_id = external_elements(i)%file_id
                    call h5fclose_f(file_id, hdferr)
                    call check("Can't close "//trim(name))
                    call add(buf, name)
                endif
            enddo
        end subroutine close_external_files
end module externalelement_m


