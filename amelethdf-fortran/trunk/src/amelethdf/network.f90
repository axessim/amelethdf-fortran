module network_m
    use h5tb
    use amelethdf_m, only : hdferr, check, &
                            trim_null_char, &
                            EL => ELEMENT_NAME_LENGTH, &
                            AL => ABSOLUTE_PATH_NAME_LENGTH

    implicit none

    character(len=*), parameter :: TC_ID = "id"
    character(len=*), parameter :: TC_EXTREMITY1 = "extremity1"
    character(len=*), parameter :: TC_EXTREMITY2 = "extremity2"
    character(len=*), parameter :: TC_TRANS_LINE = "transmissionLine"

    character(len=*), parameter :: MSIG = "[network]"
    character(len=*), parameter :: TC_ID_JUNCTION = "idJunction"
    character(len=*), parameter :: TC_ID_PORT = "idPort"
    character(len=*), parameter :: TC_ID_TUBE = "idTube"
    character(len=*), parameter :: TC_ID_WIRE = "idWire"

    type tube_t
        character(len=EL), dimension(:), allocatable  :: id
        character(len=EL), dimension(:), allocatable  :: extremity1
        character(len=EL), dimension(:), allocatable  :: extremity2
        character(len=AL), dimension(:), allocatable  :: transmission_line
    end type tube_t

    type connection_t
        character(len=AL), dimension(:), allocatable  :: id_junction
        integer, dimension(:), allocatable  :: id_port
        character(len=AL), dimension(:), allocatable  :: id_tube
        integer, dimension(:), allocatable  :: id_wire
    end type connection_t

contains
    ! Read the /network/$network/connections tables
    subroutine read_connections(file_id, path, connections)
        integer(hid_t), intent(in) :: file_id
        character(len=*), intent(in) :: path
        type(connection_t), intent(inout) :: connections

        integer(hsize_t) :: nrecords, nfields, start
        integer(size_t) :: type_size

        start = 0

        call connections_clear_content(connections)

        call h5tbget_table_info_f(file_id, path, nfields, nrecords, hdferr)
        call check(MSIG//"Can't read table info for"//path)

        ! Read idJunction column
        allocate(connections%id_junction(nrecords))
        type_size = AL
        call h5tbread_field_name_f(file_id, path, TC_ID_JUNCTION, &
                                   start, nrecords, type_size, &
                                   connections%id_junction, hdferr)
        call check(MSIG//"Can't field values for"//path//"#"//TC_ID_JUNCTION)
        call trim_null_char(connections%id_junction)

        ! Read idPort column
        allocate(connections%id_port(nrecords))
        call h5tget_size_f(H5T_NATIVE_INTEGER, type_size, hdferr)
        call h5tbread_field_name_f(file_id, path, TC_ID_PORT, &
                                   start, nrecords, type_size, &
                                   connections%id_port, hdferr)
        call check(MSIG//"Can't field values for"//path//"#"//TC_ID_PORT)

        ! Read idTube column
        allocate(connections%id_tube(nrecords))
        type_size = AL
        call h5tbread_field_name_f(file_id, path, TC_ID_TUBE, &
                                   start, nrecords, type_size, &
                                   connections%id_tube, hdferr)
        call check(MSIG//"Can't field values for"//path//"#"//TC_ID_TUBE)
        call trim_null_char(connections%id_tube)

        ! Read idPort column
        allocate(connections%id_wire(nrecords))
        call h5tget_size_f(H5T_NATIVE_INTEGER, type_size, hdferr)
        call h5tbread_field_name_f(file_id, path, TC_ID_WIRE, &
                                   start, nrecords, type_size, &
                                   connections%id_wire, hdferr)
        call check(MSIG//"Can't field values for"//path//"#"//TC_ID_WIRE)
    end subroutine read_connections

    ! Read the /network/$network/tubes tables
    subroutine read_tubes(file_id, path, tubes)
        integer(hid_t), intent(in) :: file_id
        character(len=*), intent(in) :: path
        type(tube_t), intent(inout) :: tubes

        integer(hsize_t) :: nrecords, nfields, start
        integer(size_t) :: type_size

        start = 0
        type_size = EL

        call tubes_clear_content(tubes)

        call h5tbget_table_info_f(file_id, path, nfields, nrecords, hdferr)
        call check(MSIG//"Can't read table info for"//path)

        ! Read id column
        allocate(tubes%id(nrecords))
        call h5tbread_field_name_f(file_id, path, TC_ID, &
                                   start, nrecords, type_size, &
                                   tubes%id, hdferr)
        call check(MSIG//"Can't field values for"//path//"#"//TC_ID)
        call trim_null_char(tubes%id)

        ! Read extremity1 column
        allocate(tubes%extremity1(nrecords))
        call h5tbread_field_name_f(file_id, path, TC_EXTREMITY1, &
                                   start, nrecords, type_size, &
                                   tubes%extremity1, hdferr)
        call check(MSIG//"Can't field values for"//path//"#"//TC_EXTREMITY1)
        call trim_null_char(tubes%extremity1)

        ! Read extremity2 column
        allocate(tubes%extremity2(nrecords))
        call h5tbread_field_name_f(file_id, path, TC_EXTREMITY2, &
                                   start, nrecords, type_size, &
                                   tubes%extremity2, hdferr)
        call check(MSIG//"Can't field values for"//path//"#"//TC_EXTREMITY2)
        call trim_null_char(tubes%extremity2)

        ! Read transmissionLine column
        type_size = AL
        allocate(tubes%transmission_line(nrecords))
        call h5tbread_field_name_f(file_id, path, TC_TRANS_LINE, &
                                   start, nrecords, type_size, &
                                   tubes%transmission_line, hdferr)
        call check(MSIG//"Can't field values for"//path//"#"//TC_TRANS_LINE)
        call trim_null_char(tubes%transmission_line)
    end subroutine read_tubes

    ! Clear a tube_t type
    subroutine tubes_clear_content(tubes)
        type(tube_t), intent(inout) :: tubes

        if (allocated(tubes%id)) then
            deallocate(tubes%id)
        endif
        if (allocated(tubes%extremity2)) deallocate(tubes%extremity2)
        if (allocated(tubes%extremity2)) deallocate(tubes%extremity2)
        if (allocated(tubes%transmission_line)) then
            deallocate(tubes%transmission_line)
        endif
    end subroutine tubes_clear_content

    ! Clear a connection_t type
    subroutine connections_clear_content(connections)
        type(connection_t), intent(inout) :: connections

        if (allocated(connections%id_junction)) then
            deallocate(connections%id_junction)
        endif
        if (allocated(connections%id_port)) deallocate(connections%id_port)
        if (allocated(connections%id_tube)) deallocate(connections%id_tube)
        if (allocated(connections%id_wire)) deallocate(connections%id_wire)
    end subroutine connections_clear_content
end module network_m
