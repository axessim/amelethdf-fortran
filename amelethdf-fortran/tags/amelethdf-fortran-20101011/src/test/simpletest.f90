program simpletest
    use hdf5
    use amelethdf_m, only : check, hdferr, &
                            EL => ELEMENT_NAME_LENGTH, &
                            AL => ABSOLUTE_PATH_NAME_LENGTH
    use simpletype_m, only : single_t, make_single

    character(len=AL) :: command_line_buffer = ""
    character(len=AL) :: filename = ""

    character(len=*), parameter :: MSIG = "[main]"

    ! file identifier
    integer(hid_t) :: file_id

    ! Amelet types
    type(single_t) :: single

    call getarg(1, command_line_buffer)
    if (len(command_line_buffer) == 0) then
        call check(MSIG//"Can't read the input file name")
    endif
    filename = trim(command_line_buffer)

    ! HDF5 library initialization
    hdferr = 0
    call h5open_f(hdferr)
    print *, "HDF5 library initialized"

    ! Write in the HDF5 file
    print *, "Write ", trim(filename), " ..."
    single%label = "A single label"
    call h5fopen_f(filename, H5F_ACC_RDWR_F, file_id, hdferr, H5P_DEFAULT_F)
    call check("Can't open "//trim(filename))
    call h5fclose_f(file_id, hdferr)
    call check("Can't close "//trim(filename))

    ! Read from the HDF5 file
    print *, "Reading ", trim(filename), " ..."
    call h5fopen_f(filename, H5F_ACC_RDONLY_F, file_id, hdferr, H5P_DEFAULT_F)
    call check("Can't open "//trim(filename))
    call h5fclose_f(file_id, hdferr)
    call check("Can't close "//trim(filename))

    print *, "End"
end program simpletest
