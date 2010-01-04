program arraysettest
    use hdf5
    use amelethdf_m, only : check, hdferr, &
                            EL => ELEMENT_NAME_LENGTH, &
                            AL => ABSOLUTE_PATH_NAME_LENGTH
    use simpletype_m, only : single_to_string
    use arrayset_m, only : arrayset_t, arrayset2_t, &
                           read_arrayset => read, &
                           read_arrayset2 => read2, &
                           clear_content_arrayset => clear_content, &
                           clear_content_arrayset2 => clear_content2

    character(len=AL) :: command_line_buffer = ""
    character(len=AL) :: filename = ""

    character(len=*), parameter :: MSIG = "[main]"

    ! file identifier
    integer(hid_t) :: file_id

    ! Amelet types
    type(arrayset_t) :: arr
    type(arrayset2_t) :: arr2

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

    ! a real arrayset2_t
    call read_arrayset2(file_id, "/a_real_arrayset2", arr2)
    print *, "\n/a_real_arrayset2"
    print *, "floatingType : ", trim(single_to_string(arr2%single))
    print *, "data : ", arr2%data%rvalue(:)
    print *, "dim : ", arr2%dim1%rvalue(:)

    ! a complex arrayset2_t
    call clear_content_arrayset2(arr2)
    call read_arrayset2(file_id, "/a_complex_arrayset2", arr2)
    print *, "\n/a_complex_arrayset2"
    print *, "floatingType : ", trim(single_to_string(arr2%single))
    print *, "data : ", arr2%data%cvalue(:)
    print *, "dim : ", arr2%dim1%rvalue(:)

    ! a real arrayset_t
    call read_arrayset(file_id, "/a_real_arrayset", arr)
    print *, "\n/a_complex_arrayset"
    print *, "floatingType : ", trim(single_to_string(arr%single))
    print *, "data : ", arr%data%rvalue(:)
    print *, "dim 1 : ", arr%dims(1)%rvalue(:)
    print *, "dim 2 : ", arr%dims(2)%rvalue(:)
    call clear_content_arrayset(arr)

    call h5fclose_f(file_id, hdferr)

    print *, "End"
end program arraysettest
