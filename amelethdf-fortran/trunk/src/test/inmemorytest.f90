program inmemorytest
    use h5lt
    use inmemory, only : create_inmemory_file

    ! File name
    character(len=*), parameter :: name = "inmemory.h5"
    ! File id
    integer(hid_t) :: file_id
    ! Property list access id
    integer(hid_t) :: faplist_id
    ! Error flag
    integer :: hdferr
    ! file block size in bytes.
    integer(size_t), parameter :: increment = 1024**2
    logical, parameter :: backing_store = .false.
    character(len=10) :: format_attr

    ! HDF5 library initialization
    call h5open_f(hdferr)

    call h5pcreate_f(H5P_FILE_ACCESS_F, faplist_id, hdferr)
    if (hdferr < 0) then
        print *
        print *, "Can't create property list"
        call exit()
    endif

    call h5pset_fapl_core_f(faplist_id, increment, backing_store, hdferr)
    if (hdferr < 0) then
        print *
        print *, "Can't set core driver"
        call exit()
    endif


    call h5fcreate_f(name, H5F_ACC_TRUNC_F, file_id, hdferr, &
                     H5P_DEFAULT_F, faplist_id)

    ! Amelet attributes
    print *, "Write FORMAT attribute..."
    call h5ltset_attribute_string_f(file_id, "/", &
                                    "FORMAT", "AMELET-HDF", hdferr)

    call h5fflush_f(file_id, H5F_SCOPE_GLOBAL_F, hdferr)

    ! Read with
    call h5ltget_attribute_string_f(file_id, "/", &
                                    "FORMAT", format_attr, hdferr)
    print *, "Format attribute is : ", trim(format_attr)

    call h5fclose_f(file_id, hdferr)

    ! Again with amelethdf inmemory module
    print *
    print *, "With the inmemory module"
    call create_inmemory_file("inmemory.h5", file_id)
    ! Amelet attributes
    print *, "Write FORMAT attribute..."
    call h5ltset_attribute_string_f(file_id, "/", &
                                    "FORMAT", "AMELET-HDF", hdferr)
    call h5fflush_f(file_id, H5F_SCOPE_GLOBAL_F, hdferr)

    ! Read with
    call h5ltget_attribute_string_f(file_id, "/", &
                                    "FORMAT", format_attr, hdferr)
    print *, "Format attribute is : ", trim(format_attr)

    call h5fclose_f(file_id, hdferr)
end program inmemorytest

