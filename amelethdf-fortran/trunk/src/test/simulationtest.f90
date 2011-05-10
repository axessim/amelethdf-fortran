! This program read an HDF5 file with a simulation group
! and write this group inside another HDF5 file (output.h5)
!
program simulationtest
    use hdf5
    use h5lt
    !use tools
    use amelethdf

    implicit none

    character(len=AL) :: command_line_buffer = ""

    integer(hid_t) :: file_id, grp_id, fileout_id
    character(len=AL) :: filename = ""
    character(len=AL) :: path = ""
    integer(hsize_t), dimension(:), allocatable :: dims

    real, dimension(10, 10) :: data
    real, dimension(10) :: dim1, dim2
    character(len=AL) :: label, physical_nature, unit, comment

    type(simulation_t) :: sim
    logical :: here
    character(len=AL) :: entry_point = ""
    character(len=EL) :: amelethdfversion = ""
    character(len=EL) :: formatfile = ""
    character(len=EL), dimension(:), allocatable :: children_name
    integer :: i

    ! HDF5 library initialization
    call h5open_f(hdferr)
    print *, "HDF5 library initialized"


    ! Command line reading
    call getarg(1, command_line_buffer)
    if (len(command_line_buffer) == 0) then
        call check("Can't read the input file name")
    endif
    filename = trim(command_line_buffer)
    
    print *, "Reading ", trim(filename), "..."
    call h5fopen_f(filename, H5F_ACC_RDONLY_F, file_id, hdferr, H5P_DEFAULT_F)
    call check("Can't open "//trim(filename))
    ! File creation
    print *, "Create ", "output.h5", " ..."
    call h5fcreate_f("output.h5", H5F_ACC_TRUNC_F, fileout_id, hdferr, &
                     H5P_DEFAULT_F, H5P_DEFAULT_F)
    call check("Can't create output.h5")
    call h5gcreate_f(fileout_id,C_SIMULATION, grp_id, hdferr)
    call check("Can't create /simulation group")
    ! Simulations
    print *
    print *, "Reading entry_point attribute ..."
    here = read_string_attribute(file_id,"/","entryPoint",entry_point,.true.)
    call h5ltset_attribute_string_f(fileout_id,"/","entryPoint",&
                                              trim(entry_point), hdferr)
    here = read_string_attribute(file_id,"/","AMELETHDF_FORMAT_VERSION", &
                                   amelethdfversion,.true.)
    call h5ltset_attribute_string_f(fileout_id,"/","AMELETHDF_FORMAT_VERSION",&
                                              trim(amelethdfversion), hdferr)
    here = read_string_attribute(file_id,"/","FORMAT",formatfile,.true.)
    call h5ltset_attribute_string_f(fileout_id,"/","FORMAT",&
                                              trim(formatfile), hdferr)
    
    print *, "Reading simulation ..."
    call allocate_children_name(file_id, C_SIMULATION, children_name)
    do i=1, size(children_name)
        call read_simulation(file_id, C_SIMULATION//trim(children_name(i)), sim)
        call print_simulation(sim)
        call write_simulation(fileout_id, sim)
        call simulation_clear_content(sim)
    enddo
    deallocate(children_name)

    call h5fclose_f(file_id, hdferr)
    call h5fclose_f(fileout_id, hdferr)

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

end program simulationtest
