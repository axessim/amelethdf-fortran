module tools
    use h5lt

    implicit none

    character(len=*), parameter :: MSIG = "[amelethdf]"
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
end module tools

program string_attribute_test
    use hdf5
    use h5lt
    use tools, only : check, hdferr, &
                            EL => ELEMENT_NAME_LENGTH, &
                            AL => ABSOLUTE_PATH_NAME_LENGTH

    character(len=AL) :: command_line_buffer = ""
    character(len=AL) :: filename = ""
    character(len=AL) :: the_string

    integer(hid_t) :: file_id
    integer :: slen


    call getarg(1, command_line_buffer)
    if (len(command_line_buffer) == 0) then
        call check("Can't read the input file name")
    endif
    filename = trim(command_line_buffer)

    ! HDF5 library initialization
    hdferr = 0
    call h5open_f(hdferr)
    print *, "HDF5 library initialized"

    ! File creation
    print *, "Create ", trim(filename), " ..."
    call h5fcreate_f(filename, H5F_ACC_TRUNC_F, file_id, hdferr, &
                       H5P_DEFAULT_F, H5P_DEFAULT_F)
    call check("Can't create "//trim(filename))

    ! Write string attribute
    the_string = "first_attribute"
    call h5ltset_attribute_string_f(file_id, "/", "first_attribute", &
                                    "first_attribute", hdferr)
    the_string = "second attribute"
    call h5ltset_attribute_string_f(file_id, "/", "second_attribute", &
                                    trim(the_string), hdferr)
    the_string = "une autre histoire"
    call h5ltset_attribute_string_f(file_id, "/", "une autre histoire", &
                                    trim(the_string), hdferr)
    the_string = "même avec un accent"
    call h5ltset_attribute_string_f(file_id, "/", "même avec un accent", &
                                    trim(the_string), hdferr)

    call h5fclose_f(file_id, hdferr)
    call check("Can't close "//trim(filename))

    call h5close_F(hdferr)
    call check("Can't close hdf5")
    print *, "End"
end program string_attribute_test
