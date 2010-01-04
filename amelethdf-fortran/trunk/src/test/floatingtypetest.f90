program datasettest
    use hdf5
    use amelethdf_m, only : check, hdferr, &
                            EL => ELEMENT_NAME_LENGTH, &
                            AL => ABSOLUTE_PATH_NAME_LENGTH
    use floatingtype_m, only : floatingtype_t, get_type, read, &
                               issinglereal, issinglecomplex, isvector, &
                               isdataset, isarrayset, clear_content

    ! file name
    character(len=AL) :: command_line_buffer = ""
    character(len=AL) :: filename = ""

    character(len=*), parameter :: MSIG = "[main]"

    integer(hid_t) :: file_id
    type(floatingtype_t) :: ft
    character(len=AL) :: path


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

    path = "/a_singleReal"
    call clear_content(ft)
    call read(file_id, path, ft)
    print * ,"\n/a_singleReal is a singleReal : ", issinglereal(ft)
    print *, "Value : ", ft%singlereal%value

    path = "/a_singleComplex"
    call clear_content(ft)
    call read(file_id, path, ft)
    print * ,"\n/a_singleComplex is a singleComplex : ", issinglecomplex(ft)
    print *, "Value : ", ft%singlecomplex%value

    path = "/a_vector"
    call clear_content(ft)
    call read(file_id, path, ft)
    print * ,"\n/a_vector is a vector : ", isvector(ft)
    print *, "Value : ", ft%vector%cvalue


    path = "/a_dataset"
    call clear_content(ft)
    call read(file_id, path, ft)
    print * ,"\n/a_dataset is a dataset : ", isdataset(ft)
    print *, "Value : ", ft%dataset%cvalue

    path = "/a_real_arrayset"
    call clear_content(ft)
    call read(file_id, path, ft)
    print * ,"\n/a_real_arrayset is an arrayset : ", isarrayset(ft)
    print *, "Value : ", ft%arrayset%data%rvalue
    print *, "Dim1 : ", ft%arrayset%dims(1)%rvalue

    call h5fclose_f(file_id, hdferr)

    print *, "End"
end program datasettest
