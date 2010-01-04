module physicalmodel_m
    use h5lt
    use amelethdf_m, only : check, hdferr

    implicit none

    character(len=*), parameter :: MSIG = "[physicalmodel]"

    character(len=*), parameter :: C_PHYSICAL_MODEL = "/physicalModel/"

    contains
        subroutine read(file_id, path)
            implicit none

            integer(hid_t), intent(in) :: file_id
            character(len=*), intent(in) :: path

            print *, "File : ", file_id
            print *, "Path : ", path
        end subroutine read
end module physicalmodel_m
