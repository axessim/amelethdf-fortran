module physicalmodel_m
    use h5lt
    use amelethdf_m, only : check, hdferr
    use floatingtype_m, only : floatingtype_t, floatingtype_read => read, &
                               floatingtype_clear_content => clear_content

    implicit none

    character(len=*), parameter :: MSIG = "[physicalmodel]"

    character(len=*), parameter :: C_PHYSICAL_MODEL = "/physicalModel/"
    character(len=*), parameter :: C_PHYSICAL_VOLUME = C_PHYSICAL_MODEL//"volume/"
    character(len=*), parameter :: RELATIVE_PERMITTIVITY = "/relativePermittivity/"
    character(len=*), parameter :: RELATIVE_PERMEABILITY = "/relativePermeability/"
    character(len=*), parameter :: ELECTRIC_CONDUCTIVITY = "/electricConductivity/"
    character(len=*), parameter :: MAGNETIC_CONDUCTIVITY = "/magneticConductivity/"

    type physicalvolume_t
        type(floatingtype_t) :: relative_permittivity
        type(floatingtype_t) :: relative_permeability
        type(floatingtype_t) :: electric_conductivity
        type(floatingtype_t) :: magnetic_conductivity
    end type physicalvolume_t

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
end module physicalmodel_m
