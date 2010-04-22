module generator_m
    use h5lt
    use amelethdf_m, only : check, hdferr, EL => ELEMENT_NAME_LENGTH,&
                            read_attribute
    use floatingtype_m, only : floatingtype_t, floatingtype_read => read, &
                               floatingtype_clear_content => clear_content

    implicit none

    character(len=*), parameter :: MSIG = "[generator]"
    character(len=*), parameter :: C_GENERATOR = "generator"

    character(len=*), parameter :: F_MAGNITUDE = "magnitude"
    character(len=*), parameter :: F_INNER_IMPEDANCE = "innerImpedance"
    character(len=*), parameter :: A_TYPE = "type"

    type generator_t
        character(len=EL) :: type
        type(floatingtype_t) :: inner_impedance
        type(floatingtype_t) :: magnitude
    end type generator_t

    contains
        ! read a generator
        subroutine read(file_id, path, g)
            integer(hid_t), intent(in) :: file_id
            character(len=*), intent(in) :: path
            type(generator_t), intent(inout) :: g

            logical :: ok

            ok = read_attribute(file_id, path, A_TYPE, g%type, .true.)

            call floatingtype_read(file_id, trim(path)//"/"//F_MAGNITUDE, &
                                   g%magnitude)
            call floatingtype_read(file_id, trim(path)//"/"//F_INNER_IMPEDANCE, &
                                   g%inner_impedance)
        end subroutine read

        subroutine clear_content(file_id, path, g)
            integer(hid_t), intent(in) :: file_id
            character(len=*), intent(in) :: path
            type(generator_t), intent(inout) :: g

            g%type = ""
            call floatingtype_clear_content(g%inner_impedance)
            call floatingtype_clear_content(g%magnitude)
        end subroutine clear_content
end module generator_m
