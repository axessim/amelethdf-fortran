module mesh_m
    use h5lt
    use amelethdf_m, only : check, hdferr, &
                            EL => ELEMENT_NAME_LENGTH, &
                            AL => ABSOLUTE_PATH_NAME_LENGTH
    use hdfpath_m, only : like
    use stringdataset_m, only : read_string_dataset1, get_dataset_lmn

    implicit none

    character(len=*), parameter :: MSIG = "[mesh]"

    character(len=*), parameter :: C_MESH = "/mesh/"
    character(len=*), parameter :: ATTRIBUTE_TYPE = "type"
    character(len=*), parameter :: ATTRIBUTE_STRUCTURED = "structured"
    character(len=*), parameter :: ATTRIBUTE_UNSTRUCTURED = "unstructured"
    integer, parameter :: STRUCTURED = 0, UNSTRUCTURED = 1
    character(len=EL), dimension(:), allocatable :: mesh_names

    type groupgroup_t
        character(len=AL) :: name = ""
        character(len=EL), dimension(:), allocatable :: elements
    end type groupgroup_t

    type meshlink_t
    end type meshlink_t

    contains
        ! Return the type of a mesh
        function mtype(file_id, path)
            integer(hid_t), intent(in) :: file_id
            character(len=*), intent(in) :: path
            integer :: mtype
            character(len=len(ATTRIBUTE_UNSTRUCTURED)) :: buf

            call h5ltget_attribute_string_f(file_id, trim(path), &
                                            ATTRIBUTE_TYPE, &
                                            buf, hdferr)
            call check("Can't read type attribute for "//path)
            mtype = STRUCTURED
            if (buf == ATTRIBUTE_UNSTRUCTURED) then
                mtype = UNSTRUCTURED
            else if (buf == ATTRIBUTE_STRUCTURED) then
                mtype = STRUCTURED
            else
                call check("Can't read @type for "//path//"->"//buf)
            end if
        end function mtype

        ! Returns true is path is a structured mesh
        function isstructured(file_id, path)
            integer(hid_t), intent(in) :: file_id
            character(len=*), intent(in) :: path

            logical :: isStructured

            isStructured = .false.
            if (mtype(file_id, path) == STRUCTURED) then
                isStructured = .true.
            endif
        end function isstructured

        ! Reads a groupgroup
        subroutine read_groupgroup(file_id, path, group)
            integer(hid_t), intent(in) :: file_id
            character(len=*), intent(in) :: path
            type(groupgroup_t), intent(inout) :: group

            integer, dimension(3) :: lmn

            group%name = ""
            group%name = trim(path)

            lmn = get_dataset_lmn(file_id, trim(path))
            allocate(group%elements(lmn(2)))
            group%elements = ""
            call read_string_dataset1(file_id, trim(path), &
                                      group%elements, lmn(1), lmn(2), 1)
        end subroutine read_groupgroup

        ! Prints a groupgroup to the console
        subroutine print_groupgroup(group)
            type(groupgroup_t), intent(in) :: group

            integer :: i

            print *, "groupGroup :"
            print *, "\nname : ", group%name
            do i=1,size(group%elements)
                print *, "\ni :", i, group%elements(i)
            enddo
        end subroutine print_groupgroup

        ! Helper functions
        logical function isgroup(path)
            character(len=*), intent(in) :: path

            isgroup = like(path, "/mesh/*/*/group/*")
        end function isgroup

        logical function isgroupgroup(path)
            character(len=*), intent(in) :: path

            isgroupgroup = like(path, "/mesh/*/*/groupgroup/*")
        end function isgroupgroup
end module mesh_m
