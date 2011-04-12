module physicalmodel_m
    use h5lt
    use h5tb
    use amelethdf_m, only : check, hdferr, &
                            EL => ELEMENT_NAME_LENGTH, &
                            AL => ABSOLUTE_PATH_NAME_LENGTH, &
                            trim_null_char
    use floatingtype_m, only : floatingtype_t, floatingtype_read => read, &
                               floatingtype_clear_content => clear_content

    implicit none

    character(len=*), parameter :: MSIG = "[physicalmodel]"

    character(len=*), parameter :: C_PHYSICAL_MODEL = "/physicalModel/"
    character(len=*), parameter :: C_MULTILAYER = C_PHYSICAL_MODEL//"multilayer/"
    character(len=*), parameter :: C_PHYSICAL_VOLUME = C_PHYSICAL_MODEL//"volume/"
    character(len=*), parameter :: RELATIVE_PERMITTIVITY = "/relativePermittivity/"
    character(len=*), parameter :: RELATIVE_PERMEABILITY = "/relativePermeability/"
    character(len=*), parameter :: ELECTRIC_CONDUCTIVITY = "/electricConductivity/"
    character(len=*), parameter :: MAGNETIC_CONDUCTIVITY = "/magneticConductivity/"
    character(len=*), parameter :: TC_PHYSICALMODEL = "physicalModel"
    character(len=*), parameter :: TC_THICKNESS = "thickness"

    type physicalvolume_t
        type(floatingtype_t) :: relative_permittivity
        type(floatingtype_t) :: relative_permeability
        type(floatingtype_t) :: electric_conductivity
        type(floatingtype_t) :: magnetic_conductivity
    end type physicalvolume_t

    type multilayer_t
        character(len=AL), dimension(:), allocatable :: physicalModel
        real(kind=4), dimension(:), allocatable :: thickness
        integer(kind=4) :: nblayers
    end type multilayer_t

    contains
        subroutine read(file_id, path, pv)
            implicit none

            integer(hid_t), intent(in) :: file_id
            character(len=*), intent(in) :: path
            type(physicalvolume_t), intent(inout) :: pv

            call floatingtype_read(file_id, trim(path)//RELATIVE_PERMITTIVITY, &
                                   pv%relative_permittivity)
            call floatingtype_read(file_id, trim(path)//RELATIVE_PERMEABILITY, &
                                   pv%relative_permeability)
            call floatingtype_read(file_id, trim(path)//ELECTRIC_CONDUCTIVITY, &
                                   pv%electric_conductivity)
            call floatingtype_read(file_id, trim(path)//MAGNETIC_CONDUCTIVITY, &
                                   pv%magnetic_conductivity)
        end subroutine read

        ! Clear the content of a physicalvolume_t
        subroutine clear_content(pv)
            type(physicalvolume_t), intent(inout) :: pv

            call floatingtype_clear_content(pv%relative_permittivity)
            call floatingtype_clear_content(pv%relative_permeability)
            call floatingtype_clear_content(pv%electric_conductivity)
            call floatingtype_clear_content(pv%magnetic_conductivity)
        end subroutine clear_content

        subroutine multilayer_read(file_id, path, ml)
            implicit none

            integer(hid_t), intent(in) :: file_id
            character(len=*), intent(in) :: path
            type(multilayer_t), intent(inout) :: ml

            integer(hsize_t) :: nrecords, nfields, start
            integer(size_t) :: type_size
            character(len=AL), dimension(:), allocatable :: cbuf
            character(len=AL), dimension(:), allocatable :: field_names
            integer(size_t), dimension(:), allocatable :: field_sizes
            integer(size_t), dimension(:), allocatable :: field_offsets
            integer :: i


            call h5tbget_table_info_f(file_id, path, nfields, nrecords, hdferr)
            call check(MSIG//"Can't read table info for"//path)


            allocate(field_names(nfields))
            allocate(field_sizes(nfields))
            allocate(field_offsets(nfields))
            call check(MSIG//"Can't read field info for"//path)

            ml%nblayers=nrecords
            if (allocated(ml%physicalModel)) deallocate(ml%physicalModel)
            allocate(ml%physicalModel(nrecords))
            allocate(cbuf(nrecords))
            start = 0
            type_size = AL
            call h5tbread_field_name_f(file_id, path, TC_PHYSICALMODEL, &
                                       start, nrecords, type_size, &
                                       cbuf, hdferr)
            call check(MSIG//"Can't field values for"//path//"#"//TC_PHYSICALMODEL)
            do i=1,size(cbuf)
                call trim_null_char(cbuf(i))
            enddo
            ml%physicalModel(:) = cbuf(:)

 
            if (allocated(ml%thickness)) deallocate(ml%thickness)
            allocate(ml%thickness(nrecords))
            start = 0
            call h5tget_size_f(H5T_NATIVE_REAL_4, type_size, hdferr)
            call h5tbread_field_name_f(file_id, path, TC_THICKNESS, &
                                       start, nrecords, type_size, &
                                       ml%thickness, hdferr)
            call check(MSIG//"Can't field values for"//path//"#"//TC_THICKNESS)


            deallocate(field_names, field_sizes, field_offsets, cbuf)

        end subroutine multilayer_read
end module physicalmodel_m
