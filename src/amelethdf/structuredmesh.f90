module structuredmesh_m
    use h5lt
    use amelethdf_m, only : check, hdferr, &
                            EL => ELEMENT_NAME_LENGTH, &
                            AL => ABSOLUTE_PATH_NAME_LENGTH, &
                            read_children_name
    use stringdataset_m, only : get_dataset_lmn, read_string_dataset1
    use mesh_m, only : groupgroup_t, print_groupgroup, read_groupgroup

    implicit none

    character(len=*), parameter :: MSIG = "[structuredmesh]"

    character(len=*), parameter :: G_CARTESIAN_GRID = "/cartesianGrid"
    character(len=*), parameter :: G_NORMAL = "/normal"
    character(len=*), parameter :: D_X = "/x"
    character(len=*), parameter :: D_Y = "/y"
    character(len=*), parameter :: D_Z = "/z"

    character(len=*), parameter :: GROUP = "/group"
    character(len=*), parameter :: GROUPGROUP = "/groupGroup"

    type group_t
        character(len=AL) :: name = ""
        character(len=EL) :: type = ""
        character(len=EL) :: entityType = ""
        integer, dimension(:, :), allocatable :: elements
    end type group_t

    type structured_mesh_t
        character(len=AL) :: name
        real, dimension(:), allocatable :: x, y, z
        type(group_t), dimension(:), allocatable :: groups
        type(groupgroup_t), dimension(:), allocatable :: groupgroups
    end type structured_mesh_t

    contains
        ! Reads a structured mesh and store the result in smesh
        subroutine read(file_id, mesh_path, smesh)
            integer(hid_t), intent(in) :: file_id
            character(len=*), intent(in) :: mesh_path
            type(structured_mesh_t), intent(inout) :: smesh

            integer :: i, n
            character(len=AL) :: path = "", group_path = ""
            character(len=EL), dimension(:), allocatable :: group_name

            smesh%name = ""
            smesh%name = mesh_path

            ! X Axis
            if (allocated(smesh%x)) deallocate(smesh%x)
            call read_axis(file_id, trim(mesh_path), D_X, smesh%x)

            ! Y Axis
            if (allocated(smesh%y)) deallocate(smesh%y)
            call read_axis(file_id, trim(mesh_path), D_Y, smesh%y)

            ! Z Axis
            if (allocated(smesh%z)) deallocate(smesh%z)
            call read_axis(file_id, trim(mesh_path), D_Z, smesh%z)

            ! groups
            path = ""
            path = trim(mesh_path)//GROUP
            if (allocated(group_name)) deallocate(group_name)
            call read_children_name(file_id, path, group_name)
            n = size(group_name)
            if (allocated(smesh%groups)) deallocate(smesh%groups)
            allocate(smesh%groups(n))
            do i=1,n
                group_path = trim(path)//"/"//trim(group_name(i))
                call read_group(file_id, trim(group_path), smesh%groups(i))
            end do

            ! groupGroups
            path = ""
            path = trim(mesh_path)//GROUPGROUP
            if (allocated(group_name)) deallocate(group_name)
            call read_children_name(file_id, path, group_name)
            if (allocated(smesh%groupgroups)) deallocate(smesh%groupgroups)
            n = size(group_name)
            allocate(smesh%groupgroups(n))
            do i=1,n
                group_path = trim(path)//"/"//trim(group_name(i))
                call read_groupgroup(file_id, trim(group_path), &
                                     smesh%groupgroups(i))
            enddo
        end subroutine read

        ! Read an axis of a structured mesh
        subroutine read_axis(file_id, parent, xyz, buf)
            integer(hid_t), intent(in) :: file_id
            character(len=*), intent(in) :: xyz
            character(len=*), intent(in) :: parent
            real, dimension(:), allocatable :: buf

            integer :: type_class
            integer(size_t) :: type_size
            integer(hsize_t), dimension(1) :: one_dims

            integer :: i
            character(len=100) :: path = ""

            ! Axis
            path = trim(parent)//G_CARTESIAN_GRID//xyz
            print *, "path : ...", path, "..."
            call h5ltget_dataset_info_f(file_id, path, one_dims, type_class, &
                                        type_size, hdferr)
            call check(MSIG//"Can't read the values number of "//xyz)
            print *, "\nNumber of", xyz, " values : ", one_dims(1)

            ! values
            allocate(buf(one_dims(1)))
            call h5ltread_dataset_float_f(file_id, path, buf, &
                                          one_dims, hdferr)
            call check(MSIG//"Can't read "//xyz//" values")
            do i=1, one_dims(1)
                print *, xyz, " n°", i, " :", buf( i)
            enddo
        end subroutine read_axis

        ! Print to the console smesh characteristics
        subroutine printt(smesh)
            type(structured_mesh_t), intent(in) :: smesh

            integer :: i

            print *, "Structured mesh"
            print *, "Name : ", smesh%name
            print *, "Groups :"
            do i=1,size(smesh%groups)
                print *, "Name : ", smesh%groups(i)%name, &
                         ", type : ", smesh%groups(i)%type
            enddo

            print *, "GroupGroups :"
            do i=1,size(smesh%groupgroups)
                print *, "Name : ", smesh%groupgroups(i)%name
            enddo
        end subroutine printt

        ! Reads a structured group
        subroutine read_group(file_id, path, group)
            integer(hid_t), intent(in) :: file_id
            character(len=*), intent(in) :: path
            type(group_t), intent(inout) :: group

            integer(hsize_t), dimension(2) :: dims
            integer :: type_class
            integer(size_t) :: type_size

            ! Name setting
            group%name = ""
            group%name = trim(path)

            ! type attribute
            group%type = ""
            call h5ltget_attribute_string_f(file_id, path, "type", &
                                            group%type, hdferr)
            call check(MSIG//"Can't read attribute : "//path//"@"//"type")

            call h5ltget_dataset_info_f(file_id, path, dims, type_class, &
                                        type_size, hdferr)
            call check(MSIG//"Can't read info for : "//path)

            ! values
            allocate(group%elements(dims(1), dims(2)))
            call h5ltread_dataset_int_f(file_id, path, group%elements, &
                                        dims, hdferr)
            call check(MSIG//"Can't read values for : "//path)
        end subroutine read_group
end module structuredmesh_m
