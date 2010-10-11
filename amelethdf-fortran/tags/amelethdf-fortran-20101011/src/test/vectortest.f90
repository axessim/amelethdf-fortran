program vectortest
    use hdf5
    use amelethdf_m, only : check, hdferr, &
                            EL => ELEMENT_NAME_LENGTH, &
                            AL => ABSOLUTE_PATH_NAME_LENGTH
    use vector_m, only : vector_t, read_vector => read, &
                         vector_to_string => to_string, &
                         vector_clear_content => clear_content, &
                         vector_write => write

    ! file name
    character(len=AL) :: buffer = ""
    character(len=AL) :: filename = ""

    character(len=*), parameter :: MSIG = "[main]"
    character(len=AL) :: working_directory

    ! file identifier
    integer(hid_t) :: file_id

    ! Amelet types
    type(vector_t) :: vector

    ! Write working directory
    buffer = ""
!    call get_environment_variable("PWD", buffer)
!    print *, "Working directory : ", buffer
    call getcwd(working_directory)
    print *, "Working directory : ", working_directory

    call get_command(buffer)
    print *, "Command : ", trim(buffer)

    buffer = ""
    call get_command_argument(1, buffer)
    if (len_trim(buffer) == 0) then
        call check(MSIG//"Can't read the input file name")
    endif
    filename = trim(buffer)

    ! HDF5 library initialization
    hdferr = 0
    call h5open_f(hdferr)
    print *, "HDF5 library initialized"

    print *, "Reading ", trim(filename), " ..."
    call h5fopen_f(filename, H5F_ACC_RDONLY_F, file_id, hdferr, H5P_DEFAULT_F)
    call check("Can't open "//trim(filename))


    print *, "\nReading vector ..."
    ! integer vector
    print *, "/int_vector ..."
    call read_vector(file_id, "/int_vector", vector)
    print *, "Vector : ", vector_to_string(vector)
    print *, "vector values : ", vector%ivalue
    ! float vector
    call vector_clear_content(vector)
    print *, "/float_vector ..."
    call read_vector(file_id, "/float_vector", vector)
    print *, "vector : ", vector_to_string(vector)
    print *, "vector values : ", vector%rvalue
    ! Complex vector
    call vector_clear_content(vector)
    print *, "/comp_vector ..."
    call read_vector(file_id, "/comp_vector", vector)
    print *, "vector : ", vector_to_string(vector)
    print *, "vector values : ", vector%cvalue

    call h5fclose_f(file_id, hdferr)
    call check("Can't close "//trim(filename))


    print *, "Open ", trim(filename), " in RW mode"
    call h5fopen_f(filename, H5F_ACC_RDWR_F, file_id, hdferr, H5P_DEFAULT_F)
    call check("Can't open "//trim(filename))
    call vector_clear_content(vector)
    vector%single%label = "a new vector"
    vector%single%physical_nature = "electricField"
    vector%single%unit = "voltPerMeter"
    print *, "len trim : ", len_trim(vector%single%unit)
    print *, "len trim : ", len(trim(vector%single%unit))
    vector%single%comment = "the field in the sphere"
    allocate(vector%cvalue(5))
    vector%cvalue(1) = (1,1)
    vector%cvalue(2) = (1,2)
    vector%cvalue(3) = (1,3)
    vector%cvalue(4) = (1,4)
    vector%cvalue(5) = (1,5)
    call vector_write(file_id, "/written_vector", vector)

    call h5fclose_f(file_id, hdferr)
    call check("Can't close "//trim(filename))

    print *, "Reading ", trim(filename), " to verify..."
    call h5fopen_f(filename, H5F_ACC_RDONLY_F, file_id, hdferr, H5P_DEFAULT_F)
    call check("Can't open "//trim(filename))
    call vector_clear_content(vector)
    call read_vector(file_id, "/int_vector", vector)

    ! close the library
    call h5close_f(hdferr)
    call check("Can't close HDF5 library")
    print *, "End"
end program vectortest
