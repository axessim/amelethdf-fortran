module simpletype_m
    use h5lt
!    use h5p
    use amelethdf_m, only : check, hdferr, EL => ELEMENT_NAME_LENGTH, &
                                           AL => ABSOLUTE_PATH_NAME_LENGTH, &
                                           read_string_attr => read_attribute
    use complextype_m, only : read_attribute

    implicit none

    integer, parameter :: S_SINGLE_LENGTH = 200
    character(len=*), parameter :: MSIG = "simpletype"
    character(len=*), parameter :: A_FLOATING_POINT = "floatingPoint"
    character(len=*), parameter :: V_SINGLE_REAL = "singleReal"
    character(len=*), parameter :: V_SINGLE_COMPLEX = "singleComplex"
    character(len=*), parameter :: V_DATASET = "dataset"
    character(len=*), parameter :: V_ARRAYSET = "arraySet"

    integer, parameter :: E_SINGLE_REAL = 1
    integer, parameter :: E_SINGLE_COMPLEX = 2
    integer, parameter :: E_VECTOR = 3
    integer, parameter :: E_DATASET = 4
    integer, parameter :: E_ARRAYSET = 5

    character(len=*), parameter :: A_LABEL = "label"
    character(len=*), parameter :: A_PHYSICAL_NATURE = "physicalNature"
    character(len=*), parameter :: A_UNIT = "unit"
    character(len=*), parameter :: A_COMMENT = "comment"
    character(len=*), parameter :: A_VALUE = "value"

    ! Types definition

    ! Base type, common with all floating types
    type single_t
        character(len=EL) :: label = ""
        character(len=EL) :: physical_nature = ""
        character(len=EL) :: unit = ""
        character(len=EL) :: comment = ""
    end type single_t

    type singlereal_t
        type(single_t) :: single
        real :: value
    end type singlereal_t

    type singlecomplex_t
        type(single_t) :: single
        complex :: value
    end type singlecomplex_t

    type dataset_t
        type(single_t) :: single
        integer, dimension(:), allocatable :: ivalue
        real, dimension(:), allocatable :: rvalue
        complex, dimension(:), allocatable :: cvalue
    end type dataset_t

    type linearlistofreal1_t
        real :: first
        real :: last
        integer :: number_of_values
    end type linearlistofreal1_t

    type linearlistofreal2_t
        real :: first
        real :: step
        integer :: number_of_values
    end type linearlistofreal2_t

    type logarithmlistofreal_t
        real :: first
        real :: last
        integer :: number_of_values
    end type logarithmlistofreal_t

    type perdecadelistofreal_t
        real :: first
        integer :: number_of_decades
        integer :: number_of_values_per_decade
    end type perdecadelistofreal_t

    type linearlistofinteger2_t
        integer :: first
        integer :: step
        integer :: number_of_values
    end type linearlistofinteger2_t

    contains
        integer function get_type(file_id, path)
            integer(hid_t), intent(in) :: file_id
            character(len=*), intent(in) :: path

            print *, "File : ", file_id
            print *, "Path : ", path

            get_type = 0
        end function

        ! read a single_t floating type
        subroutine read_single(file_id, path, single)
            integer(hid_t), intent(in) :: file_id
            character(len=*), intent(in) :: path
            type(single_t), intent(out) :: single

            character(len=EL) :: buf
            logical :: ok

            buf = ""
            ok = read_string_attr(file_id, path, A_LABEL, buf)
            single%label = ""
            single%label = trim(buf)

            buf = ""
            ok = read_string_attr(file_id, path, A_PHYSICAL_NATURE, buf)
            single%physical_nature = ""
            single%physical_nature = trim(buf)

            buf = ""
            ok = read_string_attr(file_id, path, A_UNIT, buf)
            single%unit = ""
            single%unit = trim(buf)

            buf = ""
            ok = read_string_attr(file_id, path, A_COMMENT, buf)
            single%comment = ""
            single%comment = trim(buf)
        end subroutine read_single

        subroutine clear_content_single(single)
            type(single_t), intent(inout) :: single

            single%label = ""
            single%physical_nature = ""
            single%unit = ""
            single%comment = ""
        end subroutine clear_content_single

        subroutine clear_content_singlereal(singlereal)
            type(singlereal_t), intent(inout) :: singlereal

            call clear_content_single(singlereal%single)
            singlereal%value = 0
        end subroutine clear_content_singlereal

        subroutine clear_content_singlecomplex(singlecomplex)
            type(singlecomplex_t), intent(inout) :: singlecomplex

            call clear_content_single(singlecomplex%single)
            singlecomplex%value = 0
        end subroutine clear_content_singlecomplex

        ! read a singlereal_t floating type
        subroutine read_singlereal(file_id, path, singlereal)
            integer(hid_t), intent(in) :: file_id
            character(len=*), intent(in) :: path
            type(singlereal_t), intent(out) :: singlereal

            character(len=*), parameter :: PREFIX = "["//V_SINGLE_REAL//"]"
            real, dimension(1) :: buf

            call read_single(file_id, path, singlereal%single)
            call h5ltget_attribute_float_f(file_id, path, A_VALUE, &
                                           buf, hdferr)
            call check(PREFIX//"Can't read real value attribute for "//path)
            singlereal%value = buf(1)
        end subroutine read_singlereal

        ! read a singlecomplex_t floating type
        subroutine read_singlecomplex(file_id, path, singlecomplex)
            integer(hid_t), intent(in) :: file_id
            character(len=*), intent(in) :: path
            type(singlecomplex_t), intent(out) :: singlecomplex

            character(len=*), parameter :: PREFIX = "["//V_SINGLE_COMPLEX//"]"
            logical :: ok

            call read_single(file_id, path, singlecomplex%single)
            ok = read_attribute(file_id, path, A_VALUE, singlecomplex%value)
            call check(PREFIX//"Can't read complex value attribute for "//path)
        end subroutine read_singlecomplex

        !
        ! deallocation subroutines
        !

        !
        ! Print subroutines
        !

        ! return a string representation of single_t
        character(len=S_SINGLE_LENGTH) function single_to_string(single)
            type(single_t), intent(in) :: single

            character(len=S_SINGLE_LENGTH) :: s

            s = ""
            s = "Label : "//trim(single%label)//&
                ", Physical nature : "//trim(single%physical_nature)//&
                ", Unit : "//trim(single%unit)//&
                ", Comment : "//trim(single%comment)
            single_to_string = trim(s)
        end function single_to_string

        ! return a string representation of singlereal_t
        character(len=S_SINGLE_LENGTH) function singlereal_to_string(singlereal)
            type(singlereal_t), intent(inout) :: singlereal
            character(len=20) :: buf

            buf = ""
            singlereal_to_string = single_to_string(singlereal%single)
            write(buf, *) singlereal%value
            singlereal_to_string = &
                trim(singlereal_to_string)//", Value : "//trim(buf)
        end function singlereal_to_string

        ! return a string representation of singlecomplex_t
        character(len=S_SINGLE_LENGTH) function singlecomplex_to_string(singlecomplex)
            type(singlecomplex_t), intent(inout) :: singlecomplex
            character(len=40) :: buf

            buf = ""
            singlecomplex_to_string = single_to_string(singlecomplex%single)
            write(buf, *) singlecomplex%value
            singlecomplex_to_string = &
                trim(singlecomplex_to_string)//", Value : "//trim(buf)
        end function singlecomplex_to_string

        !
        ! Creation subroutine
        !
        subroutine make_single(file_id, path, single)
            integer(hid_t), intent(in) :: file_id
            character(len=*), intent(in) :: path
            type(single_t), intent(in) :: single
            integer :: slen

            call h5ltset_attribute_string_f(file_id, trim(path), &
                                            A_LABEL, trim(single%label), hdferr)
            call check(MSIG//"Can't write label for "//path)
            call h5ltset_attribute_string_f(file_id, trim(path), &
                                            A_PHYSICAL_NATURE, &
                                            trim(single%physical_nature), hdferr)
            call check(MSIG//"Can't write physical nature for "//path)
            call h5ltset_attribute_string_f(file_id, trim(path), &
                                            A_UNIT, trim(single%unit), hdferr)
            call check(MSIG//"Can't write unit for "//path)
            call h5ltset_attribute_string_f(file_id, trim(path), &
                                            A_COMMENT, trim(single%comment), hdferr)
            call check(MSIG//"Can't write comment for "//path)
        end subroutine make_single
end module simpletype_m
