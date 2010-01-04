program mounttest
    use hdf5
    use amelethdf_m, only : check, hdferr, read_children_name, &
                            EL => ELEMENT_NAME_LENGTH, &
                            AL => ABSOLUTE_PATH_NAME_LENGTH

    integer(hid_t) :: file_id1
    integer(hid_t) :: file_id2
    character(len=*), parameter :: file_name1 = "file1.h5"
    character(len=*), parameter :: file_name2 = "file2.h5"
    character(len=EL) :: node1, node2, node3
    character(len=EL), dimension(:), allocatable :: children1, children2, children3
    integer :: i, j, k

    ! HDF5 library initialization
    hdferr = 0
    call h5open_f(hdferr)
    call check("Can't initialize HDF5")
    print *, "HDF5 library initialized"

    ! Reopens files
    print *, "Opens ", file_name1, " ..."
    call h5fopen_f(file_name1, H5F_ACC_RDWR_F, file_id1, hdferr)
    call check("Can't open "//file_name1)

    node1 = "/mesh"
    if (allocated(children1)) deallocate(children1)
    call read_children_name(file_id1, node1, children1)
    print *, "Number of children : ", size(children1)
    print *, node1
    do i=1, size(children1)
        print *, "  ", trim(children1(i))
        node2 = trim(node1)//"/"//trim(children1(i))
        if (allocated(children2)) deallocate(children2)
        children2 = ""
        call read_children_name(file_id1, node2, children2)
        print *, "    Number of children : ", size(children2)
        do j=1, size(children2)
            print *, "    ", trim(children2(j))
            node3 = trim(node2)//"/"//trim(children2(j))
            if (allocated(children3)) deallocate(children3)
            children3 = ""
            call read_children_name(file_id1, node3, children3)
            print *, "      Number of children : ", size(children3)
            do k=1, size(children3)
                print *, "      ", trim(children3(k))
            enddo
        enddo
    enddo

    if (allocated(children1)) deallocate(children1)
    if (allocated(children2)) deallocate(children2)
    if (allocated(children3)) deallocate(children3)

    call h5fclose_f(file_id1, hdferr)
    call h5close_f(hdferr)
    print *, "HDF5 closed"
end program mounttest
