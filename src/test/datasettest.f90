program datasettest
    use hdf5
    use amelethdf_m, only : check, hdferr, &
                            EL => ELEMENT_NAME_LENGTH, &
                            AL => ABSOLUTE_PATH_NAME_LENGTH
    use dataset_m, only : dataset_t, read_dataset => read, &
                         dataset_to_string => to_string, &
                         dataset_clear_content => clear_content, &
                         get_complex, get_real, get_integer

    ! file name
    character(len=AL) :: command_line_buffer = ""
    character(len=AL) :: filename = ""

    character(len=*), parameter :: MSIG = "[main]"
    character(len=AL) :: working_directory

    ! file identifier
    integer(hid_t) :: file_id

    ! Amelet types
    type(dataset_t) :: dataset

    ! Write working directory
    call getcwd(working_directory)
    print *, "Working directory : ", working_directory

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


    print *, "\nReading dataset ..."
    ! integer dataset
    print *, "/int_dataset ..."
    call read_dataset(file_id, "/int_dataset", dataset)
    print *, "Dataset : ", dataset_to_string(dataset)
    print *, "Dataset rank : ", size(dataset%dims)
    print *, "Dataset dims : ", dataset%dims(:)
    print *, "Dataset values : ", dataset%ivalue
    print *, " data (2, 2) : ", get_integer(dataset, (/2, 2/))
    ! float dataset
    call dataset_clear_content(dataset)
    print *, "/float_dataset ..."
    call read_dataset(file_id, "/float_dataset", dataset)
    print *, "Dataset : ", dataset_to_string(dataset)
    print *, "Dataset rank : ", size(dataset%dims)
    print *, "Dataset dims : ", dataset%dims(:)
    print *, "Dataset values : ", dataset%rvalue
    print *, " data (2, 2) : ", get_real(dataset, (/2, 2/))
    ! Complex dataset
    call dataset_clear_content(dataset)
    print *, "/comp_dataset ..."
    call read_dataset(file_id, "/comp_dataset", dataset)
    print *, "Dataset : ", dataset_to_string(dataset)
    print *, "Dataset rank : ", size(dataset%dims)
    print *, "Dataset dims : ", dataset%dims(:)
    print *, "Dataset values : ", dataset%cvalue
    print *, " data (2, 2) : ", get_complex(dataset, (/2, 2/))

    call h5fclose_f(file_id, hdferr)

    print *, "End"
end program datasettest
