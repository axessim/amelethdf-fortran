module amelethdf_m
    use h5lt

    implicit none

    character(len=*), parameter :: MSIG = "[amelethdf]"
    integer, parameter :: ELEMENT_NAME_LENGTH = 50, &
                          ABSOLUTE_PATH_NAME_LENGTH = 100
    ! error flag
    integer :: hdferr

    contains
        subroutine check(message, must_stop)
            ! the printed message
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

        ! Can remove the null character at the end of a string
        ! c is an optional character. It is used to fill
        ! the rest of the string
        elemental subroutine trim_null_char(string, c)
            character(len=*), intent(inout) :: string
            character, intent(in), optional :: c
            character :: c1
            integer :: ind

            c1 = ""
            if (present(c)) c1 = c

            ind = scan(string, char(0))
            if (ind/=0) string(ind:) = c1
        end subroutine trim_null_char

        ! Read the number of children of a group
        function read_number_of_children(file_id, path)
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
                call trim_null_char(children(i))
                call check(MSIG//"\nCan't read the name of children of "//path)
            enddo
        end subroutine read_children_name


        ! Reads a string attribute, it can be mandatory
        function read_attribute(file_id, path, attr, buf, mandatory) result(here)
            integer(hid_t), intent(in) :: file_id
            character(len=*), intent(in) :: path
            ! the name of the attribute
            character(len=*), intent(in) :: attr
            ! the returned value
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
                call trim_null_char(buf1)
                buf = trim(buf1)
            endif
        end function read_attribute

        ! Reads a float attribute, it can be mandatory
        function read_float_attribute(file_id, path, attr, buf, mandatory) &
        result(here)
            integer(hid_t), intent(in) :: file_id
            character(len=*), intent(in) :: path
            ! the name of the attribute
            character(len=*), intent(in) :: attr
            ! the value of the attribute
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

        ! Reads a integer attribute, it can be mandatory
        function read_int_attribute(file_id, path, attr, buf, mandatory) &
            result(here)

            integer(hid_t), intent(in) :: file_id
            ! the name of the attribute
            character(len=*), intent(in) :: path
            ! the value of the attribute
            character(len=*), intent(in) :: attr
            integer, intent(inout) :: buf
            logical, intent(in), optional :: mandatory

            integer, dimension(1) :: buf1
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
                call h5ltget_attribute_int_f(file_id, path, attr, buf1, hdferr)
                call check(MSIG//"Can't read attribute for "//path//"@"//attr)
                buf = buf1(1)
            endif
        end function read_int_attribute
end module amelethdf_m
