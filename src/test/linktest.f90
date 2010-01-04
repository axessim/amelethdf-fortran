program mounttest
    use hdf5
    use amelethdf_m, only : check, hdferr, read_children_name, &
                            EL => ELEMENT_NAME_LENGTH, &
                            AL => ABSOLUTE_PATH_NAME_LENGTH

    integer(hid_t) :: file_id1, mesh_id1, my_mesh_id1, grp_id
    integer(hid_t) :: file_id2, mesh_id2, mesh_id3, mesh_id4, mesh_d5
    character(len=*), parameter :: file_name1 = "file1.h5"
    character(len=*), parameter :: file_name2 = "file2.h5"

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

    ! File 2
    print *, "Creates ", file_name2, " ..."
    call h5fcreate_f(file_name2, H5F_ACC_TRUNC_F, file_id2, hdferr)
    call check("Can't create "//file_name2)

    print *, "Creates /mesh group in ", file_name2
    call h5gcreate_f(file_id2, "mesh", mesh_id2, hdferr)
    call check("Can't create /mesh in "//file_name2)

    print *, "Creates /mesh/mesh1 group in ", file_name2
    call h5gcreate_f(mesh_id2, "mesh1", mesh_id3, hdferr)
    call check("Can't create /mesh/mesh1 in "//file_name2)

    print *, "Creates /mesh/mesh1/mesh21 group in ", file_name2
    call h5gcreate_f(mesh_id3, "mesh21", mesh_id4, hdferr)
    call check("Can't create /mesh/mesh1/mesh21 in "//file_name2)
    print *, "Creates /mesh/mesh1/mesh22 group in ", file_name2
    call h5gcreate_f(mesh_id3, "mesh22", mesh_id5, hdferr)
    call check("Can't create /mesh/mesh1/mesh22 in "//file_name2)

    call h5gclose_f(mesh_id5, hdferr)
    call check("Can't close /mesh/mesh1/mesh22 in "//file_name2)
    call h5gclose_f(mesh_id4, hdferr)
    call check("Can't close /mesh/mesh1/mesh21 in "//file_name2)
    call h5gclose_f(mesh_id3, hdferr)
    call check("Can't close /mesh/mesh1 in "//file_name2)
    call h5gclose_f(mesh_id2, hdferr)
    call check("Can't close /mesh in "//file_name2)
    call h5fclose_f(file_id2, hdferr)
    call check("Can't close "//file_name2)

    call h5gopen_f(file_id1, "/mesh", grp_id, hdferr)
    call h5lcreate_external_f("file2.h5", "/mesh/mesh1", grp_id, "/mesh/mesh2", hdferr)
    call h5gclose_f(grp_id, hdferr)
    call h5fclose_f(file_id1, hdferr)
    call h5close_f(hdferr)
    print *, "HDF5 closed"
end program mounttest
