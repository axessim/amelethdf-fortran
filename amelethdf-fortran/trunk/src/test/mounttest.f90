program mounttest
    use hdf5
    use amelethdf_m, only : check, hdferr, read_children_name, &
                            EL => ELEMENT_NAME_LENGTH, &
                            AL => ABSOLUTE_PATH_NAME_LENGTH

    integer(hid_t) :: file_id1, mesh_id1, my_mesh_id1
    integer(hid_t) :: file_id2, mesh_id2, mesh_id3
    character(len=*), parameter :: file_name1 = "file1.h5", file_name2 = "file2.h5"
    character(len=EL) :: node1, node2, node3
    character(len=EL), dimension(:), allocatable :: children1, children2, children3
    integer :: i, j, k

    ! HDF5 library initialization
    hdferr = 0
    call h5open_f(hdferr)
    call check("Can't initialize HDF5")
    print *, "HDF5 library initialized"

    ! File 1
    print *, "Creates ", file_name1, " ..."
    call h5fcreate_f(file_name1, H5F_ACC_TRUNC_F, file_id1, hdferr)
    call check("Can't create "//file_name1)

    print *, "Creates /mesh group in ", file_name1
    call h5gcreate_f(file_id1, "mesh", mesh_id, hdferr)
    call check("Can't create /mesh in "//file_name1)

    print *, "Creates /mesh/mesh1 group in ", file_name1
    call h5gcreate_f(mesh_id, "mesh1", my_mesh_id, hdferr)
    call check("Can't create /mesh/mesh1 in "//file_name1)

    call h5gclose_f(my_mesh_id, hdferr)
    call check("Can't close /mesh/my_mesh in "//file_name1)
    call h5gclose_f(mesh_id, hdferr)
    call check("Can't close /mesh in "//file_name1)
    call h5fclose_f(file_id1, hdferr)
    call check("Can't close "//file_name1)

    ! File 2
    print *, "Creates ", file_name2, " ..."
    call h5fcreate_f(file_name2, H5F_ACC_TRUNC_F, file_id2, hdferr)
    call check("Can't create "//file_name2)

    print *, "Creates /mesh2 group in ", file_name2
    call h5gcreate_f(file_id2, "mesh2", mesh_id2, hdferr)
    call check("Can't create /mesh2 in "//file_name2)

    print *, "Creates /mesh3 group in ", file_name2
    call h5gcreate_f(file_id2, "mesh3", mesh_id3, hdferr)
    call check("Can't create /mesh3 in "//file_name2)

    call h5gclose_f(mesh_id3, hdferr)
    call check("Can't close /mesh3 in "//file_name2)
    call h5gclose_f(mesh_id2, hdferr)
    call check("Can't close /mesh2 in "//file_name2)
    call h5fclose_f(file_id2, hdferr)
    call check("Can't close "//file_name2)



    ! Reopens files
    print *, "Opens ", file_name1, " ..."
    call h5fopen_f(file_name1, H5F_ACC_RDWR_F, file_id1, hdferr)
    call check("Can't open "//file_name1)

    print *, "Opens ", file_name2, " ..."
    call h5fopen_f(file_name2, H5F_ACC_RDWR_F, file_id2, hdferr)
    call check("Can't open "//file_name2)

    node1 = "/mesh"
    call h5fmount_f(file_id1, node1, file_id2, hdferr)
    call check("Can't mount file 2")
    if (allocated(children1)) deallocate(children1)
    call read_children_name(file_id1, node1, children1)
    print *, "Number of children : ", size(children1)
    print *, node1
    do i=1, size(children1)
        print *, "  ", children1(i)
        node2 = trim(node1)//"/"//trim(children1(i))
        if (allocated(children2)) deallocate(children2)
        call read_children_name(file_id1, node2, children2)
        do j=1, size(children2)
            print *, "    ", children2(i)
            node3 = trim(node2)//"/"//trim(children2(i))
            if (allocated(children3)) deallocate(children3)
            call read_children_name(file_id1, node3, children3)
            do k=1, size(children3)
                print *, "      ", children3(i)
            enddo
        enddo
    enddo

    if (allocated(children1)) deallocate(children1)
    if (allocated(children2)) deallocate(children2)
    if (allocated(children3)) deallocate(children3)
    call h5funmount_f(file_id1, node1, hdferr)
    call h5close_f(hdferr)
    print *, "HDF5 closed"
end program mounttest
