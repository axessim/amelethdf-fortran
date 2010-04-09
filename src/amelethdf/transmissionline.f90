module tranmission_line_m
    use hdf5
    use amelethdf_m, only : hdferr, check, &
                            read_attribute, &
                            read_int_attribute, &
                            EL => ELEMENT_NAME_LENGTH, &
                            AL => ABSOLUTE_PATH_NAME_LENGTH

    implicit none

    character(len=*), parameter :: A_TYPE = "type"
    character(len=*), parameter :: A_DOMAIN = "domain"
    character(len=*), parameter :: A_RANK = "rank"
    character(len=*), parameter :: A_REF_ELT = "referenceElement"
    character(len=*), parameter :: A_INT_LINE = "internalLine"

    type element_t
        character(len=EL) :: type
        integer :: domain
        integer :: rank
        character(len=AL) :: reference_element
        character(len=AL) :: internal_line
    end type element_t

contains
    ! Read the /transmissionLine/$tl/element children
    subroutine read_element(file_id, path, element)
        integer(hid_t), intent(in) :: file_id
        character(len=*), intent(in) :: path
        type(element_t), intent(inout) :: element

        logical :: here

        here = read_attribute(file_id, path, A_TYPE, element%type, .true.)
        here = read_int_attribute(file_id, path, A_DOMAIN, &
                                  element%domain, .true.)
        here = read_int_attribute(file_id, path, A_RANK, &
                                  element%rank, .true.)
        here = read_attribute(file_id, path, A_REF_ELT, &
                              element%reference_element, .false.)
        here = read_attribute(file_id, path, A_INT_LINE, &
                              element%internal_line, .false.)
    end subroutine read_element
end module tranmission_line_m
