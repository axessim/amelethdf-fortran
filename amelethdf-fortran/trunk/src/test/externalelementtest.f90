program externalelement
    use hdf5
    use amelethdf_m, only : check, hdferr, &
                            EL => ELEMENT_NAME_LENGTH, &
                            AL => ABSOLUTE_PATH_NAME_LENGTH
    use externalelement_m, only : externalelement_t, read, open_external_files, &
                                  close_external_files

    character(len=AL) :: command_line_buffer = ""
    character(len=AL) :: filename = "", buf

    character(len=*), parameter :: MSIG = "[main]"

    ! file identifier
    integer(hid_t) :: file_id, new_file_id
    integer :: buf_len

    ! Amelet types
    type(externalelement_t), dimension(:), allocatable :: external_elements

    call getarg(1, command_line_buffer)
    if (len(command_line_buffer) == 0) then
        call check(MSIG//"Can't read the input file name")
    endif
    filename = trim(command_line_buffer)
    print *, "File name : ", trim(filename)

    ! HDF5 library initialization
    hdferr = 0
    call h5open_f(hdferr)
    print *, "HDF5 library initialized"

    call h5fopen_f(filename, H5F_ACC_RDONLY_F, file_id, hdferr, H5P_DEFAULT_F)
    call check("Can't open "//trim(filename))

    print *, "Going to read external element ..."
    call read(file_id, "/externalElement/glist/external", external_elements)
    do i=1,size(external_elements)
        print *, "internal : ", trim(external_elements(i)%internal_name), &
                 ", file name  : ", trim(external_elements(i)%file_name), &
                 ", external : ", trim(external_elements(i)%external_name)
    enddo

    write(*, "(12a)", advance="no"), "Open test : "
    call open_external_files(external_elements)
    print *, "OK"

    do i=1,size(external_elements)
        new_file_id = 0
        call h5freopen_f(external_elements(i)%file_id, new_file_id, hdferr)
        write(*, "(11a)", advance="no") "Open test name :"
        if (new_file_id > 0) print *, trim(external_elements(i)%file_name), " OK"
    enddo

    write(*, "(12a)", advance="no"), "Close test : "
    call close_external_files(external_elements)
    print *, "OK"

    do i=1,size(external_elements)
        new_file_id = 0
        call h5freopen_f(external_elements(i)%file_id, new_file_id, hdferr)
        write(*, "(11a)", advance="no") "Close test name :"
        if (new_file_id < 0) print *, trim(external_elements(i)%file_name), " OK"
    enddo

    call h5fclose_f(file_id, hdferr)
    print *, "End"
end program externalelement
