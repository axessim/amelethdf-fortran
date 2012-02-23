module structuredmesh_m
    use h5lt
    use h5tb
    use amelethdf_m, only : check, hdferr, &
                            EL => ELEMENT_NAME_LENGTH, &
                            AL => ABSOLUTE_PATH_NAME_LENGTH, &
                            read_children_name, trim_null_char, &
                            read_attribute
    use stringdataset_m, only : get_dataset_lmn, read_string_dataset1
    use mesh_m, only : groupgroup_t, print_groupgroup, read_groupgroup, &
                       groupgroup_clear_content
                       
    use hdfpath_m, only : exists
    
    implicit none

    character(len=*), parameter :: MSIG = "[structuredmesh]"

    character(len=*), parameter :: G_CARTESIAN_GRID = "/cartesianGrid"
    character(len=*), parameter :: G_NORMAL = "/normal"
    character(len=*), parameter :: D_X = "/x"
    character(len=*), parameter :: D_Y = "/y"
    character(len=*), parameter :: D_Z = "/z"
    character(len=*), parameter :: TC_IMIN = "imin"
    character(len=*), parameter :: TC_JMIN = "jmin"
    character(len=*), parameter :: TC_KMIN = "kmin"
    character(len=*), parameter :: TC_IMAX = "imax"
    character(len=*), parameter :: TC_JMAX = "jmax"
    character(len=*), parameter :: TC_KMAX = "kmax"
    character(len=*), parameter :: TC_V1 = "v1"
    character(len=*), parameter :: TC_V2 = "v2"
    character(len=*), parameter :: TC_V3 = "v3"
    character(len=*), parameter :: GROUP = "/group"
    character(len=*), parameter :: GROUPGROUP = "/groupGroup"
    character(len=*), parameter :: SELECTOR_ON_MESH = "/selectorOnMesh"
    character(len=*), parameter :: A_POINT_IN_ELEMENT = "pointInElement"
    
    type group_t
        character(len=AL) :: name = ""
        character(len=EL) :: type = ""
        character(len=EL) :: entity_type = ""
        integer, dimension(:, :), allocatable :: elements
    end type group_t

    type selector_on_mesh_ptinelt_t
        character(len=EL)  :: name
        integer, dimension(:), allocatable  :: imin,jmin,kmin,imax,jmax,kmax
        real, dimension(:), allocatable  :: v1, v2, v3
    end type selector_on_mesh_ptinelt_t

    type structured_mesh_t
        character(len=AL) :: name
        real, dimension(:), allocatable :: x, y, z
        type(group_t), dimension(:), allocatable :: groups
        type(groupgroup_t), dimension(:), allocatable :: groupgroups
        type(selector_on_mesh_ptinelt_t), dimension(:), allocatable :: som_ptinelt
    end type structured_mesh_t

    
    contains
        ! Reads a structured mesh and store the result in smesh
        subroutine read(file_id, mesh_path, smesh)
            integer(hid_t), intent(in) :: file_id
            character(len=*), intent(in) :: mesh_path
            type(structured_mesh_t), intent(inout) :: smesh

            integer :: i, n, j, nb
            character(len=AL) :: path = "", group_path = ""
            character(len=EL), dimension(:), allocatable :: group_name
            character(len=EL), dimension(:), allocatable :: children_name
            character(len=EL) :: sem_type =""
            logical :: here
            
            call clear_content(smesh)

            smesh%name = ""
            smesh%name = mesh_path

            ! X Axis
            call read_axis(file_id, trim(mesh_path), D_X, smesh%x)

            ! Y Axis
            call read_axis(file_id, trim(mesh_path), D_Y, smesh%y)

            ! Z Axis
            call read_axis(file_id, trim(mesh_path), D_Z, smesh%z)

            ! groups
            path = ""
            path = trim(mesh_path)//GROUP
            if (allocated(group_name)) deallocate(group_name)
            call read_children_name(file_id, path, group_name)
            n = size(group_name)
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
            n = size(group_name)
            allocate(smesh%groupgroups(n))
            do i=1,n
                group_path = trim(path)//"/"//trim(group_name(i))
                call read_groupgroup(file_id, trim(group_path), &
                                     smesh%groupgroups(i))
            enddo
            path = ""
            path = trim(mesh_path)//SELECTOR_ON_MESH
            if (exists(file_id, path)) then
                !selectorOnMesh/point in element
                path = ""
                path = trim(mesh_path)//SELECTOR_ON_MESH
                if(allocated(children_name)) deallocate(children_name)
                call read_children_name(file_id, path, children_name)
                n = size(children_name)
                nb = 0
                do i=1,n
                    path = ""
                    path = trim(mesh_path)//SELECTOR_ON_MESH//"/"//children_name(i)
                    sem_type = ""
                    here = read_attribute(file_id, path, "type", sem_type)
                    if(sem_type == A_POINT_IN_ELEMENT) nb=nb+1
                enddo
                if(nb>0) then
                    if(allocated(smesh%som_ptinelt)) deallocate(smesh%som_ptinelt)
                    allocate(smesh%som_ptinelt(nb))
                    j = 0
                    do i = 1,n
                        path = ""
                        path = trim(mesh_path)//SELECTOR_ON_MESH//"/"//children_name(i)
                        sem_type = ""
                        here = read_attribute(file_id, path, "type", sem_type)
                        if(sem_type == A_POINT_IN_ELEMENT) then
                            j = j+1
                            smesh%som_ptinelt(j)%name = children_name(i)
                            call read_selector_on_mesh_ptinelt(file_id, path, &
                                                       smesh%som_ptinelt(j))
                        endif
                    enddo
                endif
                 
            endif
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

            integer :: i, nb, j, nb2

            print *, "Structured mesh"
            print *, "Name : ", smesh%name
            print *, "Groups :"
            nb = 0
            if (allocated(smesh%groups)) nb = size(smesh%groups)
            do i=1,nb
                print *, "Name : ", smesh%groups(i)%name, &
                         ", type : ", trim(smesh%groups(i)%type)
            enddo

            print *, "GroupGroups :"
            nb = 0
            if (allocated(smesh%groupgroups)) nb = size(smesh%groupgroups)
            do i=1,nb
                print *, "Name : ", smesh%groupgroups(i)%name
            enddo
            
            nb = 0
            if (allocated(smesh%som_ptinelt)) &
                nb = size(smesh%som_ptinelt)
            print *
            print *, "Selector on mesh / point in elements (structured) ..."
            
            do i=1,nb
                print *, "name : ", trim(smesh%som_ptinelt(i)%name)
                nb2 = size(smesh%som_ptinelt(i)%imin)
                do j=1,nb2
                    print *, " imin : ", smesh%som_ptinelt(i)%imin(j), &
                         ", jmin : ", smesh%som_ptinelt(i)%jmin(j), &
                         ", kmin : ", smesh%som_ptinelt(i)%kmin(j), &
                         ", imax : ", smesh%som_ptinelt(i)%imax(j), &
                         ", jmax : ", smesh%som_ptinelt(i)%jmax(j), &
                         ", kmax : ", smesh%som_ptinelt(i)%kmax(j), &
                         ", v1 : ", smesh%som_ptinelt(i)%v1(j), &
                         ", v2 : ", smesh%som_ptinelt(i)%v2(j), &
                         ", v3 : ", smesh%som_ptinelt(i)%v3(j)
                enddo
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

        ! read selector on mesh / point in element
        ! the selector on mesh element is a table (imin, jmin, kmin, imax, jmax, kmax, v1, v2, v3)
        ! imin, jmin, kmin, imax, jmax, kmax are  integer
        ! v1, v2, v3 are real
        subroutine read_selector_on_mesh_ptinelt(file_id, path, somptinelt)
            integer(hid_t), intent(in) :: file_id
            character(len=*), intent(in) :: path
            type(selector_on_mesh_ptinelt_t), intent(inout) :: somptinelt

            integer(hsize_t) :: nrecords, nfields, start
            integer(size_t) :: type_size
            integer(size_t), dimension(:), allocatable :: field_sizes
            integer(size_t), dimension(:), allocatable :: field_offsets

            integer :: i
            character(len=EL), dimension(:), allocatable :: cbuf

            call h5tbget_table_info_f(file_id, path, nfields, nrecords, hdferr)
            call check(MSIG//"Can't read table info for"//path)
            print *, "path = ", path
            allocate(field_sizes(nfields))
            allocate(field_offsets(nfields))

            allocate(cbuf(nrecords))
            start = 0
            type_size = EL

            if (allocated(somptinelt%imin)) deallocate(somptinelt%imin)
            allocate(somptinelt%imin(nrecords))
            start = 0
            call h5tget_size_f(H5T_NATIVE_INTEGER, type_size, hdferr)
            call h5tbread_field_index_f(file_id, path, 1, &
                                       start, nrecords, type_size, &
                                       somptinelt%imin, hdferr)
            call check(MSIG//"Can't field values for"//path//"#"//TC_IMIN)

            if (allocated(somptinelt%jmin)) deallocate(somptinelt%jmin)
            allocate(somptinelt%jmin(nrecords))
            start = 0
            call h5tget_size_f(H5T_NATIVE_INTEGER, type_size, hdferr)
            call h5tbread_field_index_f(file_id, path, 2, &
                                       start, nrecords, type_size, &
                                       somptinelt%jmin, hdferr)
            call check(MSIG//"Can't field values for"//path//"#"//TC_JMIN)

            if (allocated(somptinelt%kmin)) deallocate(somptinelt%kmin)
            allocate(somptinelt%kmin(nrecords))
            start = 0
            call h5tget_size_f(H5T_NATIVE_INTEGER, type_size, hdferr)
            call h5tbread_field_index_f(file_id, path, 3, &
                                       start, nrecords, type_size, &
                                       somptinelt%kmin, hdferr)
            call check(MSIG//"Can't field values for"//path//"#"//TC_KMIN)

            if (allocated(somptinelt%imax)) deallocate(somptinelt%imax)
            allocate(somptinelt%imax(nrecords))
            start = 0
            call h5tget_size_f(H5T_NATIVE_INTEGER, type_size, hdferr)
            call h5tbread_field_index_f(file_id, path, 4, &
                                       start, nrecords, type_size, &
                                       somptinelt%imax, hdferr)
            call check(MSIG//"Can't field values for"//path//"#"//TC_IMAX)

            if (allocated(somptinelt%jmax)) deallocate(somptinelt%jmax)
            allocate(somptinelt%jmax(nrecords))
            start = 0
            call h5tget_size_f(H5T_NATIVE_INTEGER, type_size, hdferr)
            call h5tbread_field_index_f(file_id, path, 5, &
                                       start, nrecords, type_size, &
                                       somptinelt%jmax, hdferr)
            call check(MSIG//"Can't field values for"//path//"#"//TC_JMAX)

            if (allocated(somptinelt%kmax)) deallocate(somptinelt%kmax)
            allocate(somptinelt%kmax(nrecords))
            start = 0
            call h5tget_size_f(H5T_NATIVE_INTEGER, type_size, hdferr)
            call h5tbread_field_index_f(file_id, path, 6, &
                                       start, nrecords, type_size, &
                                       somptinelt%kmax, hdferr)
            call check(MSIG//"Can't field values for"//path//"#"//TC_KMAX)

            if (allocated(somptinelt%v1)) deallocate(somptinelt%v1)
            allocate(somptinelt%v1(nrecords))
            start = 0
            call h5tget_size_f(H5T_NATIVE_REAL, type_size, hdferr)
            call h5tbread_field_index_f(file_id, path, 7, &
                                       start, nrecords, type_size, &
                                       somptinelt%v1, hdferr)
            call check(MSIG//"Can't field values for"//path//"#"//TC_V1)

            if (allocated(somptinelt%v2)) deallocate(somptinelt%v2)
            allocate(somptinelt%v2(nrecords))
            start = 0
            call h5tget_size_f(H5T_NATIVE_REAL, type_size, hdferr)
            call h5tbread_field_index_f(file_id, path, 8, &
                                       start, nrecords, type_size, &
                                       somptinelt%v2, hdferr)
            call check(MSIG//"Can't field values for"//path//"#"//TC_V2)

            if (allocated(somptinelt%v3)) deallocate(somptinelt%v3)
            allocate(somptinelt%v3(nrecords))
            start = 0
            call h5tget_size_f(H5T_NATIVE_REAL, type_size, hdferr)
            call h5tbread_field_index_f(file_id, path, 9, &
                                       start, nrecords, type_size, &
                                       somptinelt%v3, hdferr)
            call check(MSIG//"Can't field values for"//path//"#"//TC_V3)
            deallocate( field_sizes, field_offsets, cbuf)
        end subroutine read_selector_on_mesh_ptinelt

        subroutine group_clear_content(group)
            type(group_t), intent(inout) :: group

            group%name = ""
            group%type = ""
            group%entity_type = ""
            if (allocated(group%elements)) deallocate(group%elements)
        end subroutine group_clear_content
        
        subroutine selector_on_mesh_ptinelt_clear_content(somptinelt)
            type(selector_on_mesh_ptinelt_t), intent(inout) :: somptinelt
           
            
            somptinelt%name=""
            if(allocated(somptinelt%imin)) deallocate(somptinelt%imin)
            if(allocated(somptinelt%jmin)) deallocate(somptinelt%jmin)
            if(allocated(somptinelt%kmin)) deallocate(somptinelt%kmin)
            if(allocated(somptinelt%imax)) deallocate(somptinelt%imax)
            if(allocated(somptinelt%jmax)) deallocate(somptinelt%jmax)
            if(allocated(somptinelt%kmax)) deallocate(somptinelt%kmax)
            if(allocated(somptinelt%v1)) deallocate(somptinelt%v1)
            if(allocated(somptinelt%v2)) deallocate(somptinelt%v2)
            if(allocated(somptinelt%v3)) deallocate(somptinelt%v3)

        end subroutine selector_on_mesh_ptinelt_clear_content

        ! Clear content subroutine
        subroutine clear_content(smesh)
            type(structured_mesh_t), intent(inout) :: smesh

            integer :: i

            smesh%name = ""
            if (allocated(smesh%x)) deallocate(smesh%x)
            if (allocated(smesh%y)) deallocate(smesh%y)
            if (allocated(smesh%z)) deallocate(smesh%z)

            if (allocated(smesh%groups)) then
                do i=1,size(smesh%groups)
                    call group_clear_content(smesh%groups(i))
                enddo
                deallocate(smesh%groups)
            endif

            if (allocated(smesh%groupgroups)) then
                do i=1,size(smesh%groupgroups)
                    call groupgroup_clear_content(smesh%groupgroups(i))
                enddo
                deallocate(smesh%groupgroups)
            endif
        end subroutine clear_content
end module structuredmesh_m
