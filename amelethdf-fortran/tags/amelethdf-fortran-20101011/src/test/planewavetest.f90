program planewavetest
    use hdf5
    use amelethdf_m, only : check, hdferr, &
                            EL => ELEMENT_NAME_LENGTH, &
                            AL => ABSOLUTE_PATH_NAME_LENGTH
    use planewave_m, only : planewave_t, clear_content, read_pw => read, &
                            islinear, iselliptic

    character(len=AL) :: command_line_buffer = ""
    character(len=AL) :: filename = "", path = ""

    character(len=*), parameter :: MSIG = "[main]"

    ! file identifier
    integer(hid_t) :: file_id

    ! Amelet types
    type(planewave_t) :: pw

    call getarg(1, command_line_buffer)
    if (len(command_line_buffer) == 0) then
        call check(MSIG//"Can't read the input file name")
    endif
    filename = trim(command_line_buffer)

    ! HDF5 library initialization
    hdferr = 0
    call h5open_f(hdferr)
    print *, "HDF5 library initialized"

    print *, "Reading ", trim(filename), " ..."
    call h5fopen_f(filename, H5F_ACC_RDONLY_F, file_id, hdferr, H5P_DEFAULT_F)
    call check("Can't open "//trim(filename))

    print *, "\nReading plane wave :"
    path = "/electromagneticSource/planeWave/x_plane_wave"
    call read_pw(file_id, path, pw)
    print *, "Theta : ", pw%theta
    print *, "Phi : ", pw%phi
    print *, "Polarization : ", pw%polarization
    print *, "Polarization is linear : ", islinear(pw)
    print *, "Polarization is elliptic : ", iselliptic(pw)
    print *, "Magnitude : ", pw%magnitude%singlecomplex%value

    call h5fclose_f(file_id, hdferr)

    print *, "End"
end program planewavetest
