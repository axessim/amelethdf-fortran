program ameletreader
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

    ! Simulations
    print *
    print *, "Reading simulation ..."
    call allocate_children_name(file_id, C_SIMULATION, children_name)
    do i=1, size(children_name)
        call read_simulation(file_id, C_SIMULATION//trim(children_name(i)), sim)
        call print_simulation(sim)
        call simulation_clear_content(sim)
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

    ! Physical Models
    print *
    print *, "Reading Physical models ..."
    call h5lexists_f(file_id, C_MULTILAYER, link_exists, hdferr, H5P_DEFAULT_F)
    if(link_exists .eqv. .TRUE.) then
        print *, "Reading Multi-Layers ..."
        call allocate_children_name(file_id, C_MULTILAYER, children_name)
        do i=1, size(children_name)
            path = trim(C_MULTILAYER)//trim(children_name(i))
            print *, " Multi-layer : ", children_name(i)
            call multilayer_read(file_id, trim(path),ml)
            print *, "    number of layers = ", ml%nblayers
            do j=1, ml%nblayers
                print *,"  Layer number : ",j
                print *,"    Physical model : ", trim(ml%physicalModel(j))
                elt = ""
                elt = read_element_path(trim(ml%physicalModel(j)), 2)
                if(elt == "grid") then
                    print *,"        It is a grid material"
                else if(elt == "volume") then
                    print *,"        It is a classical volumic material"
                endif
                print *,"    Thickness : ", ml%thickness(j)
            enddo
        enddo
    endif
    call h5lexists_f(file_id, C_GRID, link_exists, hdferr, H5P_DEFAULT_F)
    if(link_exists .eqv. .TRUE.) then
        print *, "Reading Grids ..."
        call allocate_children_name(file_id, C_GRID, children_name)
        do i=1, size(children_name)
            path = trim(C_GRID)//trim(children_name(i))
            print *, " Grid : ", children_name(i)
            call grid_read(file_id, trim(path), grid)
            print *, "    texture type = ", grid%textureType
            if(grid%textureType == "woven") then
                print *, "        surrounding material = ", &
                            grid%wovengrid%surroundingmaterial
                print *, "        grid material = ", &
                            grid%wovengrid%gridmaterial
                print *, "        pitch fiber = ", &
                            grid%wovengrid%pitchFiber
                print *, "        fiberPerPitch = ", &
                            grid%wovengrid%fiberPerPitch
                print *, "        relative height = ", &
                            grid%wovengrid%relativeHeight
                print *, "        angle = ", &
                            grid%wovengrid%angle
                do j=1, grid%wovengrid%nbcombs
                    print *, "            wire number ",j
                    if(grid%wovengrid%wireSectionType(j) == "circular") then
                        print *, "            diameter = ", &
                                  grid%wovengrid%diameterWire(j)
                    else if(grid%wovengrid%wireSectionType(j) == "rectangular") then
                        print *, "            thickness = ", &
                                  grid%wovengrid%thicknessWire(j)
                        print *, "            width = ", &
                                  grid%wovengrid%widthWire(j)
                    endif
                enddo
            else if(grid%textureType == "comb") then
                print *, "        surrounding material = ", &
                            grid%combgrid%surroundingmaterial
                print *, "        grid material = ", &
                            grid%combgrid%gridmaterial
                print *, "        shift = ", &
                            grid%combgrid%shift
                print *,"        number of combs = ", &
                           grid%combgrid%nbcombs
                do j=1, grid%combgrid%nbcombs
                    print *, "            comb number ",j
                    print *, "                relative height = ", &
                            grid%combgrid%relativeHeight(j)
                    print *, "                angle = ", &
                            grid%combgrid%angle(j)
                    print *, "        wire section type = ", &
                            grid%combgrid%wireSectionType

                    if(grid%combgrid%wireSectionType(j) == "circular") then
                        print *, "            diameter = ", &
                                  grid%combgrid%diameterWire(j)
                    else if(grid%combgrid%wireSectionType(j) == "rectangular") then
                        print *, "            thickness = ", &
                                  grid%combgrid%thicknessWire(j)
                        print *, "            width = ", &
                                  grid%combgrid%widthWire(j)
                endif

                enddo
            else if(grid%textureType == "random") then
                print *, "        surrounding material = ", &
                           grid%randomgrid%surroundingmaterial
                print *,"        number of random type = ", &
                           grid%randomgrid%numberFillerType
                do j=1, grid%randomgrid%numberFillerType
                    print *, "            filler number ",j
                    print *, "                grid material = ", &
                            grid%randomgrid%gridmaterial(i)
                    print *, "                vol fraction filler = ", &
                            grid%randomgrid%volFractioFiller(j)
                    print *, "                diameter wire = ", &
                            grid%randomgrid%diameterWire(j)
                    print *, "                length wire = ", &
                            grid%randomgrid%lengthWire(j)
                    print *, "                scale filler = ", &
                            grid%randomgrid%scaleFiller(j)
                enddo

            endif
        enddo
    endif
    print *, "Reading physical volume models ..."
    call allocate_children_name(file_id, C_PHYSICAL_VOLUME, children_name)
    do i=1, size(children_name)
        path = trim(C_PHYSICAL_VOLUME)//trim(children_name(i))
        print *, "Physical volume models : ", children_name(i)
        call physicalvolume_read(file_id, trim(path), pv)
        if (issinglecomplex(pv%relative_permittivity)) then
            print *, "  Relative permittivity :", trim( &
                singlecomplex_to_string(pv%relative_permittivity%singlecomplex))
        endif
        if (issinglecomplex(pv%relative_permeability)) then
            print *, "  Relative permittivity :", trim( &
                singlecomplex_to_string(pv%relative_permeability%singlecomplex))
        endif
        if (isarrayset(pv%electric_conductivity)) then
            print *, "  Electric conductivy  : "
            print *, "         label : ", &
                 pv%electric_conductivity%arrayset%single%label
            print *, "         physical nature : ", &
                 pv%electric_conductivity%arrayset%single%physical_nature
            print *, "         nb dims : ",&
                 size(pv%electric_conductivity%arrayset%dims)
            do j=1, size(pv%electric_conductivity%arrayset%dims)
                print *,"          dim",j,":"
                print *,"          physical nature :",&
                pv%electric_conductivity%arrayset%dims(j)%single%physical_nature
                if(allocated(pv%electric_conductivity%arrayset%dims(j)%rvalue))then
                 print *, pv%electric_conductivity%arrayset%dims(j)%rvalue(:)
                endif
                print *,"data :", pv%electric_conductivity%arrayset%data%cvalue(:)
                
            enddo
        endif
        if (issinglereal(pv%electric_conductivity)) then
            print *, "  Electric conductivy  : ", trim( &
                singlereal_to_string(pv%electric_conductivity%singlereal))
        endif
        if (issinglereal(pv%magnetic_conductivity)) then
            print *, "  Magnetic conductivy  : ", trim( &
                singlereal_to_string(pv%magnetic_conductivity%singlereal))
        endif
        call physicalvolume_clear_content(pv)
    enddo

    ! Electromagnetic sources
    print *
    print *, "Reading Electromagnetic Sources ..."
    call allocate_children_name(file_id, C_ELECTROMAGNETIC_SOURCE, children_name)
    do i=1, size(children_name)
        path = trim(C_ELECTROMAGNETIC_SOURCE)//trim(children_name(i))
        print *, "Electromagnetic Sources : ", trim(path)
        if (allocated(children_name2)) deallocate(children_name2)
        call read_children_name(file_id, trim(path), children_name2)
        if (like(path, C_ELECTROMAGNETIC_SOURCE//C_PLANE_WAVE)) then
            do j=1, size(children_name2)
                path2 = trim(path)//"/"//trim(children_name2(j))
                print *, "Plane Wave  : ", trim(path)
                print *, path2
                call read_planewave(file_id, path2, pw)
                print *, "Theta : ", pw%theta
                print *, "Phi : ", pw%phi
                print *, "Polarization : ", pw%polarization
                print *, "Polarization is linear : ", islinear(pw)
                print *, "Polarization is elliptic : ", iselliptic(pw)
                if (issinglecomplex(pw%magnitude)) then
                    print *, "  Magnitude :", trim( &
                        singlecomplex_to_string(pw%magnitude%singlecomplex))
                endif
                call planewave_clear_content(pw)
            enddo
        else if (like(path, C_ELECTROMAGNETIC_SOURCE//C_GENERATOR)) then
            do j=1, size(children_name2)
                path2 = trim(path)//"/"//trim(children_name2(j))
                print *, "Generator : ", trim(path)
                print *, path2
                call generator_read(file_id, path2, g)
                print *, "Type : ", g%type
                if (issinglecomplex(g%inner_impedance)) then
                    print *, "  Inner impedance :", trim( &
                        singlecomplex_to_string(g%inner_impedance%singlecomplex))
                endif
                if (issinglecomplex(g%magnitude)) then
                    print *, "  Magnitude :", trim( &
                        singlecomplex_to_string(g%magnitude%singlecomplex))
                endif
                call generator_clear_content(file_id, path2, g)
            enddo
        endif
    enddo

    ! Global environment
    print *
    print *, "Reading global environment ..."
    call allocate_children_name(file_id, C_GLOBAL_ENVIRONMENT, children_name)
    do i=1, size(children_name)
        path = trim(C_GLOBAL_ENVIRONMENT)//trim(children_name(i))
        print *, "Global environment : "
        call allocate_children_name(file_id,path,children_name2)
        do j=1, size(children_name2)
            path2 = trim(path)//"/"//trim(children_name2(j))
            print *, "  Path :",trim(path2)
            call read_ge(file_id,path2,ge)
            print *, "    nb values :", ge%nbvalues
            print *, "    values :"
            do k=1,ge%nbvalues
                print *,"          ", ge%values(k)
            enddo
            print *, "    domain : " , trim(ge%domain)
            print *, "    scaleType : ", trim(ge%scaleType)
            print *, "    min : ", ge%minimum
            print *, "    max : ", ge%maximum
               
        enddo
        !call read_floatingtype(file_id, trim(path)//"/"//"frequency", ft)
        
        !print *, "Value : ", ft%vector%rvalue
    enddo

    ! Label
    print *
    print *, "Reading label ..."
    call allocate_children_name(file_id, C_LABEL, children_name)
    do i=1, size(children_name)
        path = trim(C_LABEL)//trim(children_name(i))
        if (allocated(children_name2)) deallocate(children_name2)
        call read_string_vector(file_id, path, children_name2)
        print *, "Label : ", children_name2(:)
    enddo

    ! Link
    print *
    print *, "Reading links ..."
    call allocate_children_name(file_id, C_LINK, children_name)
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
    call allocate_children_name(file_id, C_OUTPUT_REQUEST, children_name)
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
end program ameletreader
