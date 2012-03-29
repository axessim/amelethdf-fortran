program WriteComplexType
    use hdf5
    use amelethdf_m, only : hdferr, check
    use complextype_m, only : write_complex_type, write_attribute, &
                              write_dataset

    ! file name
    character(len=*), parameter :: filename = "test/input.h5"

    character(len=100) :: working_directory = ""

    ! file identifier
    integer(hid_t) :: file_id
    complex, dimension(13) :: values = (14, 15)

    ! Write working directory
    call getcwd(working_directory)
    print *, "Working directory : ", working_directory

    ! HDF5 library initialization
    call h5open_f(hdferr)
    print *, "HDF5 library initialized"

    print *, "Reading ", filename, " ..."
    call h5fopen_f(filename, H5F_ACC_RDWR_F, file_id, hdferr, H5P_DEFAULT_F)
    call check("Can't open "//filename)

!    call write_complex_type(file_id)
!    call create_attribute(file_id, &
!        "/physicalModel/volume/diel1/relativePermittivity", &
!        "value1", complex(2,3))

    call write_dataset(file_id, "/physicalModel/volume/values", values)

    call h5fclose_f(file_id, hdferr)

    print *, "End"
end program
