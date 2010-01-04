module linkoutputrequest_m
    use h5lt
    use amelethdf_m, only : check, hdferr, &
                            AL => ABSOLUTE_PATH_NAME_LENGTH, &
                            read_attribute
    use hdfpath_m, only : like
    implicit none

    character(len=*), parameter :: MSIG = "[link]"
    character(len=*), parameter :: C_LINK = "/link/"
    character(len=*), parameter :: C_OUTPUT_REQUEST = "/outputRequest/"

    character(len=*), parameter :: A_SUBJECT = "subject"
    character(len=*), parameter :: A_OBJECT = "object"

    integer, parameter :: DATA_ON_MESH = 1

    type link_t
        character(len=AL) :: name = ""
        character(len=AL) :: subject = "", object = ""
    end type link_t

    contains
        ! Reads a (subject/object) of link/outputRequest
        subroutine read(loc_id, path, link)
            implicit none

            integer(hid_t), intent(in) :: loc_id
            character(len=*), intent(in) :: path
            type(link_t), intent(inout) :: link

            logical :: ok

            ok = read_attribute(loc_id, path, A_SUBJECT, link%subject, .true.)
            ok = read_attribute(loc_id, path, A_OBJECT, link%object, .true.)
        end subroutine read

        ! Return the type of link
        integer function get_type(link)
            implicit none

            type(link_t), intent(in) :: link

            if (like(link%subject, "/physicalModel/conductingMaterial") .or. &
                like(link%subject, "/physicalModel/*/*") .or. &
                like(link%subject, "/electromagneticSource/*/*")) then
                if (like(link%object, "/mesh/*/*/group/*/") .or. &
                    like(link%object, "/mesh/*/*/groupgroup/*/") .or. &
                    like(link%object, "/mesh/*/*")) then
                    get_type = DATA_ON_MESH
                endif
            else
                get_type = -1
            endif
        end function get_type

        ! Helper functions
        logical function isdataonmesh(link)
            implicit none

            type(link_t), intent(in) :: link

            isdataonmesh = (get_type(link) == DATA_ON_MESH)
        end function isdataonmesh
end module linkoutputrequest_m
