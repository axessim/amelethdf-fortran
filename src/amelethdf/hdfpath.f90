module hdfpath_m
    use h5lt
    use amelethdf_m, only : check, hdferr, &
                            EL => ELEMENT_NAME_LENGTH, &
                            AL => ABSOLUTE_PATH_NAME_LENGTH

    implicit none

    character(len=*), parameter :: MSIG = "[hdfpath]"


    contains
        function basename(path)
            implicit none
            
            character(len=*), intent(in) :: path
            character(len=len(path)) :: basename
            character(len=len(path)), dimension(2) :: buf
            
            buf = rsplit(path)
            basename = buf(2)
        end function basename

        function dirname(path)
            implicit none
            
            character(len=*), intent(in) :: path
            character(len=len(path)) :: dirname
            character(len=len(path)), dimension(2) :: buf

            buf = rsplit(path)
            dirname = buf(1)
        end function dirname

        ! Returns true if path exists in loc_id
        function exists(loc_id, path) result(link_exists)
            implicit none
            
            integer(hid_t), intent(in) :: loc_id
            character(len=*), intent(in) :: path
            logical :: link_exists

            call h5lexists_f(loc_id, path, link_exists, hdferr, H5P_DEFAULT_F)
            call check(MSIG//"No node "//path)
        end function exists

        ! returns true if path is obsolute
        logical function isabs(path)
            implicit none
            
            character(len=*), intent(in) :: path

            isabs = .false.
            if (path(1:1) == '/') isabs = .true.
        end function isabs

        ! returns the type of the path object
        integer function gettype(loc_id, path)
            implicit none

            integer(hid_t), intent(in) :: loc_id
            character(len=*), intent(in) :: path

            integer(hid_t) :: obj_id

            call h5oopen_f(loc_id, path, obj_id, hdferr, H5P_DEFAULT_F)
            call check(MSIG//"Can't open "//path)
            call h5iget_type_f(obj_id, gettype, hdferr)
            call check(MSIG//"Can't get type for "//path)
            if (gettype == H5I_BADID_F) then
                call check(MSIG//"Not valid type for "//path)
            endif
            call closetype(obj_id)
            call check(MSIG//"Can't close type id for"//path)
        end function gettype

        ! closes an opened object
        subroutine closetype(object_id)
            implicit none

            integer(hid_t), intent(in) :: object_id
            integer :: object_type

            character(len=*), parameter :: FSIG = "[closetype]"
            call h5iget_type_f(object_id, object_type, hdferr)
            call check(MSIG//FSIG//"Can't get type")
            hdferr = -1
            if (object_type == H5I_DATASET_F) then
                call h5dclose_f(object_id, hdferr)
            else if (object_type == H5I_GROUP_F) then
                call h5gclose_f(object_id, hdferr)
            endif
            call check(MSIG//FSIG//"Can't close object")
        end subroutine closetype

        ! returns true if path is a leaf (table, dataset)
        logical function isleaf(loc_id, path)
            implicit none
            
            integer(hid_t), intent(in) :: loc_id
            character(len=*), intent(in) :: path
            
            isleaf = .false.
            if (gettype(loc_id, path) == H5I_DATASET_F) isleaf = .true.
        end function isleaf

        ! returns true if path is a group
        logical function isgroup(loc_id, path)
            implicit none

            integer(hid_t), intent(in) :: loc_id
            character(len=*), intent(in) :: path

            isgroup = .false.
            if (gettype(loc_id, path) == H5I_GROUP_F) isgroup = .true.
        end function isgroup

        ! remove separator '/' from the start and the end of string
        ! for "foo/bar/" returns "foo/bar"
        function remove_sep(path, begin)
            implicit none

            character(len=*), intent(in) :: path
            logical, optional, intent(in) :: begin
            character(len=len(path)) :: remove_sep

            integer :: start, end, l
            logical :: begin1

            begin1 = .false.
            if (present(begin)) begin1 = begin

            if (path == "") then
                remove_sep = ""
                return
            endif

            start = 1
            if (.not. begin1) then
                start = scan(path, "/")
                if (start == 0) then
                    start = 1
                else if (start == 1) then
                    start = 2
                else if (start > 1) then
                    start = 1
                endif
            endif

            l = len_trim(path)
            end = scan(path, "/", .true.)
            if (end == l) then
                end = l - 1
            else
                end = l
            endif

            remove_sep = trim(path(start:end))
        end function remove_sep

        ! joins paths of an array of path
        ! for ["/foo", "bar"] returns "/foo/bar"
        function join(paths)
            implicit none
            
            character(len=*), dimension(:), intent(in) :: paths
            character(len=size(paths)*len(paths)) :: join
            
            integer :: i
            character :: sep = "/"

            join = trim(remove_sep(paths(1), .true.))
            do i=2,size(paths)
                join = trim(join)//sep//trim(remove_sep(paths(i)))
            enddo
        end function join

        ! rsplits a path and return the dirname and the basename
        ! for "/foo/bar/baz" returns "/foo/bar" and "baz"
        function rsplit(path)
            implicit none
            
            character(len=*), intent(in) :: path
            character(len=len(path)), dimension(2) :: rsplit
            
            integer :: i

            i = scan(trim(remove_sep(path, .true.)), "/", .true.)
            rsplit(1) = trim(path(:i-1))
            rsplit(2) = trim(remove_sep(trim(path(i+1:))))
        end function rsplit

        ! lsplits a path and return the dirname and the basename
        ! for "/foo/bar/baz" returns "/foo" and "bar/baz"
        function lsplit(path)
            implicit none

            character(len=*), intent(in) :: path
            character(len=len(path)), dimension(2) :: lsplit

            character(len=len(path)) :: buf
            integer :: i

            buf = remove_sep(path)
            i = scan(buf, "/")
            if (i==0) then
                lsplit(1) = trim(buf)
                lsplit(2) = ""
            else
                lsplit(1) = trim(buf(:i-1))
                lsplit(2) = trim(buf(i+1:))
            endif
        end function lsplit

        ! splits a path and return the first element path
        ! for "/foo/bar/" returns "foo"
        function head(path)
            implicit none

            character(len=*), intent(in) :: path
            character(len=len(path)) :: head

            character(len=len(path)), dimension(2) :: buf

            buf = lsplit(path)
            head(:) = trim(buf(1)(:))
        end function head

        ! returns true is subpath is in path
        ! for "/foo/bar" & "/foo" returns true
        logical function contain(path, subpath)
            implicit none

            character(len=*), intent(in) :: path, subpath

            integer :: i = 0

            contain = .false.
            i = index(path, subpath, .false.)
            if (i > 0) contain = .true.
        end function contain

        ! returns the nth element of path
        ! for ("/foo/bar/baz", 2) returns bar
        recursive function element(path, ind, back) result (res)
            implicit none

            character(len=*), intent(in) :: path
            integer, intent(in) :: ind
            logical, optional, intent(in) :: back

            character(len=len(path)) :: res, buf
            character(len=len(path)), dimension(2) :: buf2
            logical :: back1

            if (path == "") return

            back1 = .false.
            if (present(back)) back1 = back

            buf = remove_sep(path)

            if (.not. back1) then
                buf2 = lsplit(buf)
                if (ind > 1) then
                    res = element(buf2(2), ind-1, back)
                else
                    res = trim(buf2(1))
                endif
            else
                buf2 = rsplit(buf)
                if (ind > 1) then
                    res = element(buf2(1), ind-1, back)
                else
                    res = trim(buf2(2))
                endif
            endif
        end function element

        ! Return true if a path looks like a patter
        ! For "/foo/bar/baz" & "/foo/*/baz" return true
        recursive function like(path, pattern) result (res)
            implicit none

            character(len=*), intent(in) :: path
            character(len=*), intent(in) :: pattern

            logical :: res
            character(len=len(path)), dimension(2) :: buf1
            character(len=len(pattern)), dimension(2) :: buf2

            buf1 = lsplit(path)
            buf2 = lsplit(pattern)
            if (buf1(1) == buf2(1) .or. buf2(1) == "*") then
                if (buf1(2) == "" .and. buf2(2) == "") then
                    res = .true.
                else
                    res = like(buf1(2), buf2(2))
                endif
            else
                res = .false.
            endif
        end function like
end module hdfpath_m
