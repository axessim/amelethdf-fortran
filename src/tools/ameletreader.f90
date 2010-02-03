program ameletreader
    use amelethdf

    implicit none

    ! file name
    character(len=AL) :: command_line_buffer = ""
    character(len=AL) :: filename = ""

    character(len=AL) :: working_directory, path, path2, path3
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
    type(planewave_t) :: pw
    type(floatingtype_t) :: ft
    type(link_t) :: link

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

    ! Simulations
    print *
    print *, "Reading simulation ..."
    if (allocated(children_name)) deallocate(children_name)
    call read_children_name(file_id, C_SIMULATION, children_name)
    do i=1, size(children_name)
        call readSimulation(file_id, C_SIMULATION//trim(children_name(i)), sim)
        call printSimulation(sim)
    enddo

    ! Meshes
    print *, "\nReading Mesh ..."
    if (allocated(children_name)) deallocate(children_name)
    call read_children_name(file_id, C_MESH, children_name)
    do i=1, size(children_name)
        path = trim(C_MESH//children_name(i))
        print *, "Mesh group : ", path
        if (allocated(children_name2)) deallocate(children_name2)
        call read_children_name(file_id, path, children_name2)
        do j=1, size(children_name2)
            path2 = path//"/"//trim(children_name2(i))
            print *, "\nReading ", trim(path2), " ..."
            if (allocated(children_name3)) deallocate(children_name3)
            call read_children_name(file_id, path2, children_name3)
            do k=1, size(children_name3)
                path3 = trim(path2)//"/"//trim(children_name3(i))
                print *, "\nReading ", trim(path3), " ..."
                if (isStructured(file_id, trim(path3))) then
                    call readStructuredMesh(file_id, trim(path3), smesh)
                    call printStructuredMesh(smesh)
                else
                    call readUnstructuredMesh(file_id, trim(path3), umesh)
                    call printUnstructuredMesh(umesh)
                endif
            enddo
        enddo
    enddo

    ! Physical Models
    print *
    print *, "Reading Physical models ..."
    if (allocated(children_name)) deallocate(children_name)
    call read_children_name(file_id, C_PHYSICAL_MODEL, children_name)
    do i=1, size(children_name)
        print *, "Physical models : ", children_name(i)
    enddo

    ! Electromagnetic sources
    print *
    print *, "Reading Electromagnetic Sources ..."
    if (allocated(children_name)) deallocate(children_name)
    call read_children_name(file_id, C_ELECTROMAGNETIC_SOURCE, children_name)
    do i=1, size(children_name)
        path = trim(C_ELECTROMAGNETIC_SOURCE)//trim(children_name(i))
        print *, "Electromagnetic Sources : ", trim(path)
        if (allocated(children_name2)) deallocate(children_name2)
        call read_children_name(file_id, trim(path), children_name2)
        if (like(path, C_ELECTROMAGNETIC_SOURCE//C_PLANE_WAVE)) then
            do j=1, size(children_name2)
                path2 = trim(path)//"/"//trim(children_name2(j))
                print *, path2
                call read_planewave(file_id, path2, pw)
                print *, "Theta : ", pw%theta
                print *, "Phi : ", pw%phi
                print *, "Polarization : ", pw%polarization
                print *, "Polarization is linear : ", islinear(pw)
                print *, "Polarization is elliptic : ", iselliptic(pw)
                print *, "Magnitude : ", pw%magnitude%singlecomplex%value
            enddo
        endif
    enddo

    ! Global environment
    print *
    print *, "Reading global environment ..."
    if (allocated(children_name)) deallocate(children_name)
    call read_children_name(file_id, C_GLOBAL_ENVIRONMENT, children_name)
    do i=1, size(children_name)
        path = trim(C_GLOBAL_ENVIRONMENT)//trim(children_name(i))
        print *, "Global environment : ", trim(path)
        call read_floatingtype(file_id, trim(path)//"/"//"frequency", ft)
        print *, "Value : ", ft%vector%rvalue
    enddo

    ! Label
    print *
    print *, "Reading label ..."
    if (allocated(children_name)) deallocate(children_name)
    call read_children_name(file_id, C_LABEL, children_name)
    do i=1, size(children_name)
        path = trim(C_LABEL)//trim(children_name(i))
        if (allocated(children_name2)) deallocate(children_name2)
        call read_string_vector(file_id, path, children_name2)
        print *, "Label : ", children_name2(:)
    enddo

    ! Link
    print *
    print *, "Reading links ..."
    if (allocated(children_name)) deallocate(children_name)
    call read_children_name(file_id, C_LINK, children_name)
    do i=1, size(children_name)
        path = trim(C_LINK)//trim(children_name(i))
        print *, "Link group : ", trim(path)
        if (allocated(children_name2)) deallocate(children_name2)
        call read_children_name(file_id, trim(path), children_name2)
        do j=1, size(children_name2)
            path2 = trim(path)//"/"//trim(children_name2(i))
            print *, "Link : ", trim(path2)
            call read_link(file_id, path2, link)
            print *, "Subject : ", trim(link%subject)
            print *, "Object : ", trim(link%object)
            print *, "Is dataonmesh : ", isdataonmesh(link)
        enddo
    enddo

    ! Output request
    print *
    print *, "Reading output request ..."
    if (allocated(children_name)) deallocate(children_name)
    call read_children_name(file_id, C_OUTPUT_REQUEST, children_name)
    do i=1, size(children_name)
        path = trim(C_OUTPUT_REQUEST)//trim(children_name(i))
        print *, "Output request group : ", trim(path)
        if (allocated(children_name2)) deallocate(children_name2)
        call read_children_name(file_id, trim(path), children_name2)
        do j=1, size(children_name2)
            path2 = trim(path)//"/"//trim(children_name2(i))
            print *, "Output request : ", trim(path2)
            call read_link(file_id, path2, link)
            print *, "Subject : ", trim(link%subject)
            print *, "Object : ", trim(link%object)
        enddo
    enddo

    call h5fclose_f(file_id, hdferr)

    print *, "End"
end program ameletreader