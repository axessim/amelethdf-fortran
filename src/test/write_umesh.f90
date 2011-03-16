module tools
    use hdf5
    use h5lt

    implicit none

    integer, parameter :: AL=100
    integer :: hdferr

  contains
	subroutine check(message, must_stop)
	    character(len=*) :: message
	    logical, intent(in), optional :: must_stop
	    logical :: must_stop1

	    must_stop1 = .true.
	    if (present(must_stop)) must_stop1 = must_stop

	    if (hdferr < 0) then
	        print *, message
	        if (must_stop1) then
	            stop
	        endif
	    endif
	end subroutine check
        subroutine set_mesh_type(file_id, path, meshtype)
            integer(hid_t), intent(in) :: file_id
            character(len=*), intent(in) :: path
            character(len=*), intent(in) :: meshtype

            
            print *, "Writes /", trim(path)//"@type ..."
            call h5ltset_attribute_string_f(file_id, trim(path), "type", &
                                            trim(meshtype), hdferr)
            call check("Can't write type for "//path)
        end subroutine set_mesh_type
    

end module tools

! This program write an HDF5 file with the following arraySet structure
!
! data.h5
! `-- mesh
!     `-- gmesh1
!         `-- an_umesh [@type=unstructured]
!             |-- nodes
!             |-- elementTypes
!             |-- elementNodes
!             |-- group
!             |   |-- a_group
!             |   `-- another_group
!             `-- groupGroup
!                 `-- a_groupGroup

program write_an_umesh
    use hdf5
    use h5lt
    use tools

    implicit none

    integer(hid_t) :: file_id, grp_id, space, dset
    character(len=AL) :: command_line_buffer
    character(len=AL) :: filename, path
    integer(hsize_t), dimension(:), allocatable :: dims
    real, dimension(3, 5) :: nodes
    integer(kind=4), dimension(13) :: elementNodes
    integer(kind=4), dimension(4):: elementTypes
    integer(kind=4), dimension(3):: a_group
    integer(kind=4), dimension(1) :: another_group
    character(len=AL) :: meshtype
    character(len=14), dimension(2) :: a_groupGroup
    integer(hid_t) :: type_id
    integer(size_t) :: type_size


    nodes(1,1)=0.
    nodes(2,1)=0.
    nodes(3,1)=0.

    nodes(1,2)=0.1
    nodes(2,2)=0.
    nodes(3,2)=0.

    nodes(1,3)=0.
    nodes(2,3)=0.1
    nodes(3,3)=0.

    nodes(1,4)=0.1
    nodes(2,4)=0.2
    nodes(3,4)=0.

    nodes(1,5)=0.2
    nodes(2,5)=0.1
    nodes(3,5)=0.

    elementTypes(1)=11
    elementTypes(2)=13
    elementTypes(3)=11
    elementTypes(4)=11

    elementNodes(1)=0
    elementNodes(2)=1
    elementNodes(3)=2
    elementNodes(4)=2
    elementNodes(5)=3
    elementNodes(6)=4
    elementNodes(7)=1
    elementNodes(8)=1
    elementNodes(9)=2
    elementNodes(10)=4
    elementNodes(11)=2
    elementNodes(12)=3
    elementNodes(13)=4

    a_group(1)=0
    a_group(2)=2
    a_group(3)=3
    another_group=1

    a_groupGroup(1)="a_group"
    a_groupGroup(2)="another_group"

    type_size = len(a_groupGroup)
    ! HDF5 library initialization
    call h5open_f(hdferr)
    print *, "HDF5 library initialized"


    ! Command line reading
    call getarg(1, command_line_buffer)
    if (len(command_line_buffer) == 0) then
        call check("Can't read the input file name")
    endif
    filename = trim(command_line_buffer)
    ! File creation
    print *, "Create ", trim(filename), " ..."
    call h5fcreate_f(filename, H5F_ACC_TRUNC_F, file_id, hdferr, &
                     H5P_DEFAULT_F, H5P_DEFAULT_F)
    call check("Can't create "//trim(filename))

    ! Writes FORMAT attribute
    print *, "Writes /@FORMAT ..."
    call h5ltset_attribute_string_f(file_id, "/", &
                                    "FORMAT", "AMELETHDF", hdferr)
    call check("Can't write type attribute for /")

    ! Writes AMELETHDF_FORMAT_VERSION attribute
    print *, "Writes /@AMELETHDF_FORMAT_VERSION ..."
    call h5ltset_attribute_string_f(file_id, "/", &
                                    "AMELETHDF_FORMAT_VERSION", "1.5.0", hdferr)
    call check("Can't write type attribute for /")

    ! floatingType group creation
    print *, "Creates /mesh group ..."
    call h5gcreate_f(file_id, "mesh", grp_id, hdferr)
    call check("Can't create /mesh group")
    call h5gclose_f(grp_id, hdferr)


    ! gmesh1 group creation
    print *, "Creates /mesh/an_umesh group ..."
    call h5gcreate_f(file_id, "mesh/gmesh1", grp_id, hdferr)
    call check("Can't create /mesh/gmesh1 group")
    call h5gclose_f(grp_id, hdferr)

    ! arraySet group creation
    print *, "Creates /mesh/gmesh1/an_umesh group ..."
    call h5gcreate_f(file_id, "mesh/gmesh1/an_umesh", grp_id, hdferr)
    call check("Can't create /mesh/gmesh1/an_umesh group")
    call h5gclose_f(grp_id, hdferr)

    
    ! Writes type attribute
    print *, "Writes /mesh/an_umesh/@type ..."
    call h5ltset_attribute_string_f(file_id, "mesh/gmesh1/an_umesh", &
                                    "type", "unstructured", hdferr)
    call check("Can't write type attribute for mesh/gmesh1/an_umesh")

    ! Writes /mesh/gmesh1/an_umesh/nodes
    print *, "Writes /mesh/gmesh1/an_umesh/nodes ..."
    allocate(dims(2))
    dims = (/3, 5/)
    call h5ltmake_dataset_float_f(file_id, "mesh/gmesh1/an_umesh/nodes", 2, &
                                  dims, nodes, hdferr)
    call check("Can't write nodes for "//path)
    deallocate(dims)

    ! Writes /mesh/gmesh1/an_umesh/elementNodes
    path = "mesh/gmesh1/an_umesh/elementNodes"
    print *, "Writes ", trim(path), " ..."
    allocate(dims(1))
    dims = (/13/)
    call h5ltmake_dataset_int_f(file_id, trim(path), 1, dims, elementNodes, hdferr)
    call check("Can't write dataset for "//path)
    deallocate(dims)


    ! Writes /mesh/gmesh1/an_umesh/elementTypes
    path = "mesh/gmesh1/an_umesh/elementTypes"
    print *, "Writes ", trim(path), " ..."
    allocate(dims(1))
    dims = (/4/)
    call h5screate_simple_f(1, dims, space, hdferr)
    call check("Can't create space for "//path)
    call h5dcreate_f(file_id, path, H5T_STD_U8BE, space, dset, hdferr)
    call check("Can't create dataset for "//path)
    call h5dwrite_f(dset, H5T_NATIVE_INTEGER, elementTypes, dims, hdferr, file_space_id=space)
    !call h5ltmake_dataset_f(file_id, trim(path), 1, dims, H5T_NATIVE_UINT8, elementTypes, hdferr)
    call check("Can't write dataset for "//path)

    call h5dclose_f(dset,hdferr)
    call h5sclose_f(space,hdferr)

    deallocate(dims)


    ! group create /mesh/gmesh1/an_umesh/group
    print *, "Creates /mesh/gmesh1/an_umesh/group group ..."
    call h5gcreate_f(file_id, "mesh/gmesh1/an_umesh/group", grp_id, hdferr)
    call check("Can't create /mesh/gmesh1/an_umesh/group group")
    call h5gclose_f(grp_id, hdferr)

    ! Writes /mesh/gmesh1/an_umesh/group/a_group
    path = "mesh/gmesh1/an_umesh/group/a_group"
    print *, "Writes ", trim(path), " ..."
    allocate(dims(1))
    dims = (/3/)
    call h5ltmake_dataset_int_f(file_id, trim(path), 1, dims, a_group, hdferr)
    call check("Can't write dataset for "//path)
    deallocate(dims)

    ! Writes type attribute
    print *, "Writes mesh/gmesh1/an_umesh/group/a_group@type ..."
    call h5ltset_attribute_string_f(file_id, "mesh/gmesh1/an_umesh/group/a_group", &
                                    "type", "element", hdferr)
    call check("Can't write type attribute for mesh/gmesh1/an_umesh/group/a_group")

    ! Writes entityType attribute
    print *, "Writes mesh/gmesh1/an_umesh/group/a_group@entityType ..."
    call h5ltset_attribute_string_f(file_id, "mesh/gmesh1/an_umesh/group/a_group", &
                                    "entityType", "face", hdferr)
    call check("Can't write type attribute for mesh/gmesh1/an_umesh/group/a_group")


    ! Writes /mesh/gmesh1/an_umesh/group/another_group
    path = "mesh/gmesh1/an_umesh/group/another_group"
    print *, "Writes ", trim(path), " ..."
    allocate(dims(1))
    dims = (/1/)
    call h5ltmake_dataset_int_f(file_id, trim(path), 1, dims, another_group, hdferr)
    call check("Can't write dataset for "//path)
    deallocate(dims)

    ! Writes type attribute
    print *, "Writes mesh/gmesh1/an_umesh/group/another_group@type ..."
    call h5ltset_attribute_string_f(file_id, "mesh/gmesh1/an_umesh/group/another_group", &
                                    "type", "element", hdferr)
    call check("Can't write type attribute for mesh/gmesh1/an_umesh/group/another_group")

    ! Writes entityType attribute
    print *, "Writes mesh/gmesh1/an_umesh/group/another_group@entityType ..."
    call h5ltset_attribute_string_f(file_id, "mesh/gmesh1/an_umesh/group/another_group", &
                                    "entityType", "face", hdferr)
    call check("Can't write type attribute for mesh/gmesh1/an_umesh/group/another_group")
    
    ! group create /mesh/gmesh1/an_umesh/groupGroup
    print *, "Creates /mesh/gmesh1/an_umesh/groupGroup group ..."
    call h5gcreate_f(file_id, "mesh/gmesh1/an_umesh/groupGroup", grp_id, hdferr)
    call check("Can't create /mesh/gmesh1/an_umesh/groupGroup group")
    call h5gclose_f(grp_id, hdferr)

    ! Writes /mesh/gmesh1/an_umesh/group/a_groupGroup
    path = "mesh/gmesh1/an_umesh/groupGroup/a_groupGroup"
    print *, "Writes ", trim(path), " ..."
    allocate(dims(1))
    dims = (/2/)
    call h5tcopy_f(H5T_NATIVE_CHARACTER, type_id, hdferr)
    call h5tset_size_f(type_id, type_size, hdferr)
    call check("Can't get type size")

    call h5screate_simple_f(1, dims, space, hdferr)
    call check("Can't create data space")

    call h5dcreate_f(file_id, trim(path), type_id, space, dset, hdferr)
    call check("Can't create dataset")

    call h5dwrite_f(dset, type_id, a_groupGroup, dims, hdferr, H5P_DEFAULT_F)
    call check("Can't write dataset for "//path)

    call h5tclose_f(type_id,hdferr)
    call h5dclose_f(dset,hdferr)
    call h5sclose_f(space,hdferr)

    deallocate(dims)

    ! close filename
    call h5fclose_f(file_id, hdferr)
    call check("Can't close "//trim(filename))

    ! close HDF5 library
    call h5close_f(hdferr)
    call check("Can't close HFD5")
end program write_an_umesh
