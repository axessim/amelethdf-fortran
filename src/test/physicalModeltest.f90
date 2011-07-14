program physicalmodeltest
    use hdf5
    use amelethdf_m, only : check, hdferr, &
                            EL => ELEMENT_NAME_LENGTH, &
                            AL => ABSOLUTE_PATH_NAME_LENGTH
    use physicalmodel_m, only : physicalvolume_t, read, clear_content
    use floatingtype_m, only : floatingtype_t

    character(len=AL) :: command_line_buffer = ""
    character(len=AL) :: filename = ""

    character(len=*), parameter :: MSIG = "[main]"

    ! file identifier
    integer(hid_t) :: file_id

    ! Amelet types
    type(physicalvolume_t) :: pv
    type(floatingtype_t) :: ft
    integer :: i

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
    call read(file_id, "/physicalModel/volume/dielectric", pv)
    print *, "Dielectric"
    print *, "  relative permittivity : ", pv%relative_permittivity%floatingtype
    print *, "     ",pv%relative_permittivity%singlecomplex%value
    print *, "  relative permeability : ", pv%relative_permeability%floatingtype
    print *, "     ",pv%relative_permeability%singlecomplex%value
    print *, "  magnetic conductivity : ", pv%magnetic_conductivity%floatingtype
    print *, "     ",pv%magnetic_conductivity%singlereal%value
    print *, "  electric conductivity : ", pv%electric_conductivity%floatingtype
    
    print *, "    data = ",pv%electric_conductivity%arrayset%data%cvalue(:)
    print *, "    frequencies =  ",pv%electric_conductivity%arrayset%dims(1)%rvalue(:)
    !print *, "data : ", arr2%data%rvalue(:)
    !print *, "dim : ", arr2%dim1%rvalue(:)

    call clear_content(pv)

    call h5fclose_f(file_id, hdferr)

    print *, "End"
end program physicalmodeltest
