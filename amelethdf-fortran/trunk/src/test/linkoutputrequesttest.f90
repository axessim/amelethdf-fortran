program linkoutputrequesttest
    use hdf5
    use amelethdf_m, only : check, hdferr, &
                            EL => ELEMENT_NAME_LENGTH, &
                            AL => ABSOLUTE_PATH_NAME_LENGTH, &
                            read_children_name

    use linkoutputrequest_m, only : link_t, read_link => read, C_LINK, &
                                    get_type, isdataonmesh

    character(len=AL) :: command_line_buffer = "", filename = ""

    integer(hid_t) :: file_id
    type(link_t) :: link
    character(len=AL) :: path = "", path2 = ""
    character(len=EL), dimension(:), allocatable :: starr, starr2

    ! Reads command line arguments
    call getarg(1, command_line_buffer)
    if (len(command_line_buffer) == 0) then
        call check("Can't read the input file name")
    endif
    filename = trim(command_line_buffer)

    ! HDF5 library initialization
    hdferr = 0
    call h5open_f(hdferr)
    print *, "HDF5 library initialized"

    ! Opens the file
    print *, "Reading ", trim(filename), " ..."
    call h5fopen_f(filename, H5F_ACC_RDONLY_F, file_id, hdferr, H5P_DEFAULT_F)
    call check("Can't open "//trim(filename))

    ! Link test
    print *, "\nLink test ..."
    if (allocated(starr)) deallocate(starr)
    call read_children_name(file_id, C_LINK, starr)
    do i=1, size(starr)
        path = trim(C_LINK)//trim(starr(i))
        print *, "Link group : ", trim(path)
        if (allocated(starr2)) deallocate(starr2)
        call read_children_name(file_id, trim(path), starr2)
        do j=1, size(starr2)
            path2 = trim(path)//"/"//trim(starr2(j))
            print *, "Link : ", path2
            call read_link(file_id, path2, link)
            print *, "Subject : ", link%subject
            print *, "Object : ", link%object
            print *, "Is dataonmesh : ", isdataonmesh(link)
        enddo
    enddo

    print *, "End"
    call h5fclose_f(file_id, hdferr)
end program linkoutputrequesttest
