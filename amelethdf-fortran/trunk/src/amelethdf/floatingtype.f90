module floatingtype_m
    use h5lt
    use amelethdf_m, only : check, hdferr, EL => ELEMENT_NAME_LENGTH, &
                            read_attribute
    use simpletype_m, only : singlereal_t, singlecomplex_t, &
                             read_singlereal, clear_content_singlereal, &
                             read_singlecomplex, clear_content_singlecomplex
    use vector_m, only : vector_t, read_vector => read, &
                         clear_content_vector => clear_content
    use dataset_m, only : dataset_t, read_dataset => read, &
                          clear_content_dataset => clear_content
    use arrayset_m, only : arrayset_t, read_arrayset => read, &
                           clear_content_arrayset => clear_content

    implicit none

    integer, parameter :: S_SINGLE_LENGTH = 200
    character(len=*), parameter :: MSIG = "[floatingtype]"
    character(len=*), parameter :: A_FLOATING_TYPE = "floatingType"
    character(len=*), parameter :: V_SINGLE_REAL = "singleReal"
    character(len=*), parameter :: V_SINGLE_COMPLEX = "singleComplex"
    character(len=*), parameter :: V_VECTOR = "vector"
    character(len=*), parameter :: V_DATA_SET = "dataSet"
    character(len=*), parameter :: V_ARRAY_SET = "arraySet"

    integer, parameter :: E_SINGLE_REAL = 1
    integer, parameter :: E_SINGLE_COMPLEX = 2
    integer, parameter :: E_VECTOR = 3
    integer, parameter :: E_DATA_SET = 4
    integer, parameter :: E_ARRAY_SET = 5

    type floatingtype_t
        integer :: floatingtype
        type(singlereal_t) :: singlereal
        type(singlecomplex_t) :: singlecomplex
        type(vector_t) :: vector
        type(dataset_t) :: dataset
        type(arrayset_t) :: arrayset
    end type floatingtype_t

    contains
        ! Reads a floatingtype_t structure
        subroutine read(file_id, path, ft)
            integer(hid_t), intent(in) :: file_id
            character(len=*), intent(in) :: path
            type(floatingtype_t), intent(inout) :: ft

            ft%floatingtype = get_type(file_id, path)
            select case (ft%floatingtype)
                case (E_SINGLE_REAL)
                    call read_singlereal(file_id, path, ft%singlereal)
                case (E_SINGLE_COMPLEX)
                    call read_singlecomplex(file_id, path, ft%singlecomplex)
                case (E_VECTOR)
                    call read_vector(file_id, path, ft%vector)
                case (E_DATA_SET)
                    call read_dataset(file_id, path, ft%dataset)
                case (E_ARRAY_SET)
                    call read_arrayset(file_id, path, ft%arrayset)
            end select
        end subroutine read

        ! Returns the type (as an integer) of a floatingType
        integer function get_type(file_id, path)
            integer(hid_t), intent(in) :: file_id
            character(len=*), intent(in) :: path

            character(LEN=EL) :: buf
            logical :: ok

            ok = read_attribute(file_id, path, A_FLOATING_TYPE, buf, .true.)
            get_type = -1
            if (trim(buf) == V_SINGLE_REAL) then
                get_type = E_SINGLE_REAL
            else if (trim(buf) == V_SINGLE_COMPLEX) then
                get_type = E_SINGLE_COMPLEX
            else if (trim(buf) == V_VECTOR) then
                get_type = E_VECTOR
            else if (trim(buf) == V_DATA_SET) then
                get_type = E_DATA_SET
            else if (trim(buf) == V_ARRAY_SET) then
                get_type = E_ARRAY_SET
            else
                hdferr = -1
                call check(MSIG//"Can't get floating type for "//path)
            end if
        end function

        subroutine clear_content(ft)
            type(floatingtype_t), intent(inout) :: ft

            ft%floatingtype = 0
            call clear_content_singlereal(ft%singlereal)
            call clear_content_singlecomplex(ft%singlecomplex)
            call clear_content_vector(ft%vector)
            call clear_content_dataset(ft%dataset)
            call clear_content_arrayset(ft%arrayset)
        end subroutine clear_content

        ! Series of test functions
        logical function issinglereal(ft)
            type(floatingtype_t), intent(in) :: ft

            issinglereal = (ft%floatingtype == E_SINGLE_REAL)
        end function issinglereal

        logical function issinglecomplex(ft)
            type(floatingtype_t), intent(in) :: ft

            issinglecomplex = (ft%floatingtype == E_SINGLE_COMPLEX)
        end function issinglecomplex

        logical function isvector(ft)
            type(floatingtype_t), intent(in) :: ft

            isvector = (ft%floatingtype == E_VECTOR)
        end function isvector

        logical function isdataset(ft)
            type(floatingtype_t), intent(in) :: ft

            isdataset = (ft%floatingtype == E_DATA_SET)
        end function isdataset

        logical function isarrayset(ft)
            type(floatingtype_t), intent(in) :: ft

            isarrayset = (ft%floatingtype == E_ARRAY_SET)
        end function isarrayset
end module floatingtype_m
