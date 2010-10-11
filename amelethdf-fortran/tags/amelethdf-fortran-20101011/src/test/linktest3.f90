module tools
    use h5lt

    implicit none

    character(len=*), parameter :: MSIG = "[tools]"
    integer, parameter :: ELEMENT_NAME_LENGTH = 30, &
                          ABSOLUTE_PATH_NAME_LENGTH = 100
    ! error flag
    integer :: hdferr

    contains
        subroutine check(message, must_stop)
            character(len=*) :: message
            logical, intent(in), optional :: must_stop
            logical :: must_stop1

            must_stop1 = .true.
            if (present(must_stop)) must_stop1 = must_stop

            if (hdferr < 0) then
                print *, message
                if (must_stop1) then
                    stop
                endif
            endif
        end subroutine check

        ! Read the number of children of a group
        function read_number_of_children(file_id, path)
            implicit none

            integer(hid_t), intent(in) :: file_id
            character(len=*), intent(in) :: path
            integer :: read_number_of_children

            call h5gn_members_f(file_id, trim(path), &
                                read_number_of_children, hdferr)
            call check(MSIG//"Can't read the number of children of "//trim(path))
        end function read_number_of_children

        ! Read the children's name of a group
        subroutine read_children_name(file_id, path, children)
            integer(hid_t), intent(in) :: file_id
            character(len=*), intent(in) :: path
            character(len=ELEMENT_NAME_LENGTH), &
                dimension(:), allocatable :: children

            integer :: i, obj_type, nb_children

            nb_children = read_number_of_children(file_id, path)
            allocate(children(nb_children))
            children = ""
            do i=1, nb_children
                call h5gget_obj_info_idx_f(file_id, trim(path), i-1, &
                                           children(i), obj_type, hdferr)
                call check(MSIG//"\nCan't read the name of children of "//path)
            enddo
        end subroutine read_children_name


        ! Reads a string attribute, it can be mandatory
        function read_attribute(file_id, path, attr, buf, mandatory) result(here)
            implicit none

            integer(hid_t), intent(in) :: file_id
            character(len=*), intent(in) :: path, attr
            character(len=*), intent(inout) :: buf
            logical, intent(in), optional :: mandatory

            character(len=ABSOLUTE_PATH_NAME_LENGTH) :: buf1
            logical :: here, mandatory1

            mandatory1 = .false.
            if (present(mandatory)) mandatory1 = mandatory

            here = .false.
            buf1 = ""
            buf = ""
            call h5aexists_by_name_f(file_id, path, attr, &
                                     here, hdferr, H5P_DEFAULT_F)
            if (mandatory1 .and. .not. here) then
                hdferr = -1
                call check(MSIG//attr//" does not exist for : "//path)
            endif
            if (here) then
                call h5ltget_attribute_string_f(file_id, path, attr, &
                                                buf1, hdferr)
                call check(MSIG//"Can't read attribute for "//path//"@"//attr)
                buf = trim(buf1)
            endif
        end function read_attribute

        ! Reads a float attribute, it can be mandatory
        function read_float_attribute(file_id, path, attr, buf, mandatory) result(here)
            implicit none

            integer(hid_t), intent(in) :: file_id
            character(len=*), intent(in) :: path, attr
            real, intent(inout) :: buf
            logical, intent(in), optional :: mandatory

            real, dimension(1) :: buf1
            logical :: here, mandatory1

            mandatory1 = .false.
            if (present(mandatory)) mandatory1 = mandatory

            here = .true.
            call h5aexists_by_name_f(file_id, path, attr, &
                                     here, hdferr, H5P_DEFAULT_F)
            if (mandatory1 .and. .not. here) then
                hdferr = -1
                call check(MSIG//attr//" does not exist for : "//path)
            endif
            if (here) then
                call h5ltget_attribute_float_f(file_id, path, attr, buf1, hdferr)
                call check(MSIG//"Can't read attribute for "//path//"@"//attr)
                buf = buf1(1)
            endif
        end function read_float_attribute
end module tools


program linktest
    use hdf5

    use tools, only : check, hdferr, read_children_name, &
                            EL => ELEMENT_NAME_LENGTH, &
                            AL => ABSOLUTE_PATH_NAME_LENGTH

    integer(hid_t) :: file_id1
    integer(hid_t) :: file_id2
    character(len=*), parameter :: file_name1 = "file1.h5"
    character(len=*), parameter :: file_name2 = "file2.h5"
    character(len=EL) :: node1, node2, node3
    character(len=EL), dimension(:), allocatable :: children1, children2, children3
    integer :: i, j, k

    ! HDF5 library initialization
    hdferr = 0
    call h5open_f(hdferr)
    call check("Can't initialize HDF5")
    print *, "HDF5 library initialized"

    ! Reopens files
    print *, "Opens ", file_name1, " ..."
    call h5fopen_f(file_name1, H5F_ACC_RDWR_F, file_id1, hdferr)
    call check("Can't open "//file_name1)

    node1 = "/mesh"
    if (allocated(children1)) deallocate(children1)
    call read_children_name(file_id1, node1, children1)
    print *, "Number of children : ", size(children1)
    print *, node1
    do i=1, size(children1)
        print *, "  ", trim(children1(i))
        node2 = trim(node1)//"/"//trim(children1(i))
        if (allocated(children2)) deallocate(children2)
        children2 = ""
        call read_children_name(file_id1, node2, children2)
        print *, "    Number of children : ", size(children2)
        do j=1, size(children2)
            print *, "    ", trim(children2(j))
            node3 = trim(node2)//"/"//trim(children2(j))
            if (allocated(children3)) deallocate(children3)
            children3 = ""
            call read_children_name(file_id1, node3, children3)
            print *, "      Number of children : ", size(children3)
            do k=1, size(children3)
                print *, "      ", trim(children3(k))
            enddo
        enddo
    enddo

    if (allocated(children1)) deallocate(children1)
    if (allocated(children2)) deallocate(children2)
    if (allocated(children3)) deallocate(children3)

    call h5fclose_f(file_id1, hdferr)
    call h5close_f(hdferr)
    print *, "HDF5 closed"
end program linktest
