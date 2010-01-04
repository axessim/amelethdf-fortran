program hdfpathtest
    use hdf5
    use amelethdf_m, only : check, hdferr, read_children_name, &
                            EL => ELEMENT_NAME_LENGTH, &
                            AL => ABSOLUTE_PATH_NAME_LENGTH
    use hdfpath_m, only : rsplit, lsplit, dirname, basename, isleaf, isgroup, &
                          join, isabs, contain, element, remove_sep, head, &
                          like, exists, isleaf, isgroup

    character(len=AL) :: path = ""
    character(len=AL), dimension(:), allocatable :: path_array
    integer :: l = 0

    ! file name
    character(len=AL) :: command_line_buffer = ""
    character(len=AL) :: filename = ""

    character(len=*), parameter :: MSIG = "[main]"
    character(len=AL) :: working_directory

    ! file identifier
    integer(hid_t) :: file_id

    ! Path operations
    print *, "\nhdfpathtest"
    print *, "Path handling ..."

    print *, "\n\nRemove_sep ... "
    path = "/foo/bar/baz/"
    l = scan(path, "/", .true.)
    print *, "Path : ", path
    print *, trim(path), " : ", trim(remove_sep(path))
    print *, trim(path), " with true : ", trim(remove_sep(path, .true.))
    print *, trim(path(2:)), " : ", trim(remove_sep(path(2:)))
    print *, trim(path(2:)), " with true : ", trim(remove_sep(path(2:), .true.))
    print *, trim(path(:l-1)), " : ", trim(remove_sep(path(:l-1)))
    print *, trim(path(:l-1)), " with true : ", trim(remove_sep(path(:l-1), .true.))
    print *, "foo", " : ", trim(remove_sep("foo"))
    print *, trim("foo"), " with true : ", trim(remove_sep("foo", .true.))
    print *, "/foo", " : ", trim(remove_sep("/foo"))
    print *, trim("/foo"), " with true : ", trim(remove_sep("/foo", .true.))
    print *, "foo/", " : ", trim(remove_sep("foo/"))
    print *, trim("foo/"), " with true : ", trim(remove_sep("foo/", .true.))
    print *, "/foo/", " : ", trim(remove_sep("/foo/"))
    print *, trim("/foo/"), " with true : ", trim(remove_sep("/foo/", .true.))

    print *, "\n\nHead"
    path = "/foo/bar/baz/"
    l = scan(path, "/", .true.)
    print *, "Path : ", path
    print *, trim(path), " : ", trim(head(path))
    print *, trim(path(2:)), " : ", trim(head(path(2:)))
    print *, trim(path(:l-1)), " : ", trim(head(path(:l-1)))
    print *, "foo", " : ", trim(head("foo"))
    print *, "/foo", " : ", trim(head("/foo"))
    print *, "foo/", " : ", trim(head("foo/"))
    print *, "/foo/", " : ", trim(head("/foo/"))


    print *, "\n\nLsplit"
    allocate(path_array(2))
    path = "/foo/bar/baz/"
    path_array = lsplit(path)
    print *, "Path : ", trim(path), ", split : ", trim(path_array(1)), ", ", trim(path_array(2))
    path = "/foo/bar/baz"
    path_array = lsplit(path)
    print *, "Path : ", trim(path), ", split : ", trim(path_array(1)), ", ", trim(path_array(2))
    path = "foo/bar/baz"
    path_array = lsplit(path)
    print *, "Path : ", trim(path), ", split : ", trim(path_array(1)), ", ", trim(path_array(2))
    path = "/foo/bar/baz"
    path_array = lsplit(path)
    print *, "Path : ", trim(path), ", split : ", trim(path_array(1)), ", ", trim(path_array(2))
    path = "/foo/"
    path_array = lsplit(path)
    print *, "Path : ", trim(path), ", split : ", trim(path_array(1)), ", ", trim(path_array(2))
    path = "/foo"
    path_array = lsplit(path)
    print *, "Path : ", trim(path), ", split : ", trim(path_array(1)), ", ", trim(path_array(2))
    path = "foo/"
    path_array = lsplit(path)
    print *, "Path : ", trim(path), ", split : ", trim(path_array(1)), ", ", trim(path_array(2))
    path = "foo"
    path_array = lsplit(path)
    print *, "Path : ", trim(path), ", split : ", trim(path_array(1)), ", ", trim(path_array(2))


    print *, "\n\nRsplit"
    path = "/foo/bar/baz/"
    path_array = rsplit(path)
    print *, "Path : ", trim(path), ", split : ", trim(path_array(1)), ", ", trim(path_array(2))
    path = "/foo/bar/baz"
    path_array = rsplit(path)
    print *, "Path : ", trim(path), ", split : ", trim(path_array(1)), ", ", trim(path_array(2))
    path = "foo/bar/baz"
    path_array = rsplit(path)
    print *, "Path : ", trim(path), ", split : ", trim(path_array(1)), ", ", trim(path_array(2))
    path = "/foo/bar/baz"
    path_array = rsplit(path)
    print *, "Path : ", trim(path), ", split : ", trim(path_array(1)), ", ", trim(path_array(2))
    path = "/foo/"
    path_array = rsplit(path)
    print *, "Path : ", trim(path), ", split : ", trim(path_array(1)), ", ", trim(path_array(2))
    path = "/foo"
    path_array = rsplit(path)
    print *, "Path : ", trim(path), ", split : ", trim(path_array(1)), ", ", trim(path_array(2))
    path = "foo/"
    path_array = rsplit(path)
    print *, "Path : ", trim(path), ", split : ", trim(path_array(1)), ", ", trim(path_array(2))
    path = "foo"
    path_array = rsplit(path)
    print *, "Path : ", trim(path), ", split : ", trim(path_array(1)), ", ", trim(path_array(2))


    print *, "\n\nDirname Basename"
    path = "/foo/bar/baz/"
    print *, trim(path), ", dirname path : ", trim(dirname(path)), ", basename : ", trim(basename(path))
    path = "foo/bar/baz/"
    print *, trim(path), ", dirname path : ", trim(dirname(path)), ", basename : ", trim(basename(path))
    path = "/foo"
    print *, trim(path), ", dirname path : ", trim(dirname(path)), ", basename : ", trim(basename(path))
    path = "foo"
    print *, trim(path), ", dirname path : ", trim(dirname(path)), ", basename : ", trim(basename(path))



    print *, "\n\njoin"
    path_array = (/"foo/bar/", "/baz"/)
    print *, "join : ", trim(path_array(1)), " & ", trim(path_array(2)), " : " , trim(join(path_array))
    path_array = (/"foo/bar/", "/baz/"/)
    print *, "join : ", trim(path_array(1)), " & ", trim(path_array(2)), " : " , trim(join(path_array))
    path_array = (/"/foo/bar/", "/baz/"/)
    print *, "join : ", trim(path_array(1)), " & ", trim(path_array(2)), " : " , trim(join(path_array))

    print *, "\n\nisabs"
    path = "/foo/bar/baz/"
    print *, trim(path), ",  absolute : ", isabs(path)
    path = "bar/baz/"
    print *, trim(path), ",  absolute : ", isabs(path)

    print *, "\n\ncontain"
    print *, "/foo/bar/baz contains /foo/bar : ", contain("/foo/bar/baz",&
                                                          "/foo/bar")
    print *, "\n\nElement :"
    print *, "\nFrom the beginning :"
    path = "/foo/bar/baz/"
    print *, "Path : ", trim(path)
    print *, "first element : ", element(path, 1)
    print *, "second element : ", element(path, 2)
    print *, "third element : ", element(path, 3)
    path = "foo/bar/baz"
    print *, "Path : ", trim(path)
    print *, "first element : ", element(path, 1)
    print *, "second element : ", element(path, 2)
    print *, "third element : ", element(path, 3)
    print *, "\nFrom the end :"
    path = "/foo/bar/baz/"
    print *, "Path : ", trim(path)
    print *, "first element : ", element(path, 1, .true.)
    print *, "second element : ", element(path, 2, .true.)
    print *, "third element : ", element(path, 3, .true.)
    path = "foo/bar/baz"
    print *, "Path : ", trim(path)
    print *, "first element : ", element(path, 1, .true.)
    print *, "second element : ", element(path, 2, .true.)
    print *, "third element : ", element(path, 3, .true.)

    print *, "\n\nLike :"
    path_array = (/"foo/bar/", "/foo/bar"/)
    print *, "Paths : ", trim(path_array(1)), ", ", &
             trim(path_array(2)), " :", like(path_array(1), path_array(2))
    path_array = (/"foo/bar/", "/foo/*"/)
    print *, "Paths : ", trim(path_array(1)), ", ", &
             trim(path_array(2)), " :", like(path_array(1), path_array(2))
    path_array = (/"foo/bar/baz", "/foo/*/baz"/)
    print *, "Paths : ", trim(path_array(1)), ", ", &
             trim(path_array(2)), " :", like(path_array(1), path_array(2))
    deallocate(path_array)


    print *, "\nFile operations"
    ! Write working directory
    call getcwd(working_directory)
    print *, "Working directory : ", working_directory

    call getarg(1, command_line_buffer)
    if (len(command_line_buffer) == 0) then
        call check(MSIG//"Can't read the input file name")
    endif
    filename = trim(command_line_buffer)

    ! HDF5 library initialization
    hdferr = 0
    call h5open_f(hdferr)
    print *, "HDF5 library initialized"

    print *, "Reading ", trim(filename), " ..."
    call h5fopen_f(filename, H5F_ACC_RDONLY_F, file_id, hdferr, H5P_DEFAULT_F)
    call check("Can't open "//trim(filename))

    print *, "/a_group exists : ", exists(file_id, "/a_group")
    print *, "/an_array exists : ", exists(file_id, "/an_array")
    print *, "/another_array exists : ", exists(file_id, "/another_array")
    print *, "/a_group is a group : ", isgroup(file_id, "/a_group")
    print *, "/an_array is a leaf : ", isleaf(file_id, "/an_array")

    call h5fclose_f(file_id, hdferr)

    print *, "End"
end program hdfpathtest
