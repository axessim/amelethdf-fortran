program networktest
    use hdf5
    use amelethdf_m, only : check, hdferr, &
                            EL => ELEMENT_NAME_LENGTH, &
                            AL => ABSOLUTE_PATH_NAME_LENGTH
    use network_m, only : connection_t, read_connections, &
                          tube_t, read_tubes

    character(len=AL) :: command_line_buffer = ""
    character(len=AL) :: filename = "", path = ""

    character(len=*), parameter :: MSIG = "[main]"
    integer :: i

    ! file identifier
    integer(hid_t) :: file_id

    ! Amelet types
    type(connection_t) :: connections
    type(tube_t) :: tubes

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

    print *
    print *, "Reading connections :"
    path = "/network/net1/connections"
    call read_connections(file_id, path, connections)
    do i=1, size(connections%id_junction)
        print *, "id_junction : ", trim(connections%id_junction(i))
    enddo
    print *, "id_port : ", connections%id_port
    do i=1, size(connections%id_tube)
        print *, "id_tube : ", trim(connections%id_tube(i))
    enddo
    print *, "id_wire : ", connections%id_wire


    print *
    print *, "Reading tubes :"
    path = "/network/net1/tubes"
    call read_tubes(file_id, path, tubes)
    do i=1, size(tubes%id)
        print *, "id : ", trim(tubes%id(i))
    enddo
    do i=1, size(tubes%extremity1)
        print *, "extremity 1 : ", trim(tubes%extremity1(i))
    enddo
    do i=1, size(tubes%extremity2)
        print *, "extremity 2 : ", trim(tubes%extremity2(i))
    enddo
    do i=1, size(tubes%transmission_line)
        print *, "transmission line : ", trim(tubes%transmission_line(i))
    enddo

    call h5fclose_f(file_id, hdferr)

    print *, "End"
end program networktest
