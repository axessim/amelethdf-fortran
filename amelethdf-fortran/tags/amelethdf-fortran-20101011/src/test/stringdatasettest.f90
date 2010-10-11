program complextypetest
    use hdf5
    use amelethdf_m, only : check, hdferr, &
                            EL => ELEMENT_NAME_LENGTH, &
                            AL => ABSOLUTE_PATH_NAME_LENGTH
    use stringdataset_m, only : write_nd_dataset

    character(len=AL) :: command_line_buffer = ""
    character(len=AL) :: filename = ""

    character(len=*), parameter :: MSIG = "[main]"

    character(len=EL), dimension(3, 10) :: values
    character(len=3) :: si
    integer :: i

    ! file identifier
    integer(hid_t) :: file_id

    do i=1,size(values, 2)
        write(si, "(i3)") i
        values(1, i) = si
        write(si, "(i3)") 2*i
        values(2, i) = si
        write(si, "(i3)") 3*i
        values(3, i) = si
    enddo

    call getarg(1, command_line_buffer)
    if (len(command_line_buffer) == 0) then
        call check(MSIG//"Can't read the input file name")
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

    call write_nd_dataset(file_id, "string_dataset", values, shape(values))

    call h5fclose_f(file_id, hdferr)

    print *, "End"
end program complextypetest
