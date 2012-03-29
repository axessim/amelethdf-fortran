program meshreader
    use amelethdf

    implicit none

    ! file name
    character(len=AL) :: command_line_buffer = ""
    character(len=AL) :: filename = ""

    character(len=AL) :: working_directory, path, path2, path3, elt
    character(len=EL), dimension(:), allocatable :: children_name, &
                                                    children_name2, &
                                                    children_name3
    integer :: i, j, k

    ! file identifier
    integer(hid_t) :: file_id

    ! Amelet types
    type(simulation_t) :: sim
    type(structured_mesh_t) :: smesh
    type(unstructured_mesh_t) :: umesh
    type(multilayer_t) :: ml
    type(physicalvolume_t) :: pv
    type(planewave_t) :: pw
    type(generator_t) :: g
    type(floatingtype_t) :: ft
    type(link_t) :: link
    type(globalenvironment_t) :: ge
    type(grid_t) :: grid
    logical :: link_exists
    type(vector_t) :: mydim



    ! Write working directory
    call getcwd(working_directory)
    print *, "Working directory : ", working_directory

    call getarg(1, command_line_buffer)
    if (len(command_line_buffer) == 0) then
        call check("Can't read the input file name")
    endif
    filename = trim(command_line_buffer)

    ! HDF5 library initialization
    hdferr = 0
    call h5open_f(hdferr)
    print *, "HDF5 library initialized"

    print *, "Reading ", trim(filename), " ..."
    call h5fopen_f(filename, H5F_ACC_RDONLY_F, file_id, hdferr, H5P_DEFAULT_F)
    call check("Can't open "//trim(filename))

    ! All categories
    print *
    print *, "Reading categories"
    if (allocated(children_name)) deallocate(children_name)
    call read_children_name(file_id, "/", children_name)
    do i=1, size(children_name)
        print *, "Category : ", children_name(i)
    enddo


    ! Meshes
    print *
    print *, "Reading Mesh ..."
    call allocate_children_name(file_id, C_MESH, children_name)
    print *, "  Number of children : ", size(children_name)
    do i=1, size(children_name)
        path = ""
        path = trim(C_MESH//children_name(i))
        print *, "  Mesh group : ", trim(path), " ..."
        if (allocated(children_name2)) deallocate(children_name2)
        call read_children_name(file_id, path, children_name2)
        print *, "    Number of children : ", size(children_name2)
        do j=1, size(children_name2)
            path2 = ""
            path2 = trim(path)//"/"//trim(children_name2(j))
            print *, "    Mesh n°: ", j, ", ", trim(path2), " ..."
            if (isStructured(file_id, trim(path2))) then
                call smesh_read(file_id, trim(path2), smesh)
                call smesh_print(smesh)
                call smesh_clear_content(smesh)
            else
                call umesh_read(file_id, trim(path2), umesh)
                call umesh_print(umesh)
                call umesh_clear_content(umesh)
            endif
        enddo
    enddo


    call h5fclose_f(file_id, hdferr)

    print *, "End"

contains
    subroutine allocate_children_name(file_id, path, children_name)
        integer(hid_t) :: file_id
        character(len=*) :: path
        character(len=*), dimension(:), allocatable :: children_name

        if (allocated(children_name)) deallocate(children_name)
        if (exists(file_id, path)) then
            call read_children_name(file_id, path, children_name)
        else
            allocate(children_name(0))
        endif
    end subroutine allocate_children_name
end program meshreader
