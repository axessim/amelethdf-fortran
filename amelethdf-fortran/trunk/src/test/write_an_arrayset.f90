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

    ! print floatingType attributes in path
	subroutine set_floating_type(file_id, path, label, physical_nature, unit, comment)
	    integer(hid_t), intent(in) :: file_id
	    character(len=*), intent(in) :: path
        character(len=*), intent(in) :: label, physical_nature, unit, comment

	    ! Writes /floatingType/an_arrayset/data attributes
	    print *, "Writes /", trim(path)//"@label ..."
	    call h5ltset_attribute_string_f(file_id, trim(path), "label", &
	                                    trim(label), hdferr)
	    call check("Can't write label for "//path)
	    print *, "Writes /", trim(path)//"@physicalNature ..."
	    call h5ltset_attribute_string_f(file_id, trim(path), "physicalNature", &
	                                    trim(physical_nature), hdferr)
	    call check("Can't write physical nature for "//path)
	    print *, "Writes /", trim(path)//"@unit ..."
	    call h5ltset_attribute_string_f(file_id, trim(path), "unit", &
	                                    trim(unit), hdferr)
	    call check("Can't write unit for "//path)
	    print *, "Writes /", trim(path)//"@comment ..."
	    call h5ltset_attribute_string_f(file_id, trim(path), "comment", &
	                                    trim(comment), hdferr)
	    call check("Can't write comment for "//path)
	end subroutine set_floating_type
end module tools

! This program write an HDF5 file with the following arraySet structure
!
! data.h5
! `-- floatingType
!     `-- an_arrayset
!         |-- data[@physicaNature=electricField
!         |        @unit=voltPerMeter]
!         `-- ds
!             |-- dim1[@physicalNature=length
!             |        @unit=meter]
!             `-- dim2[@physicalNature=length
!                      @unit=meter]
!
!    real, dimension(10, 10) :: data
!    real, dimension(10) :: dim1, dim2
!
program write_an_arrayset
    use hdf5
    use h5lt
    use tools

    implicit none

    integer(hid_t) :: file_id, grp_id
    character(len=AL) :: command_line_buffer
    character(len=AL) :: filename, path
    integer(hsize_t), dimension(:), allocatable :: dims

    real, dimension(10, 10) :: data
    real, dimension(10) :: dim1, dim2
    character(len=AL) :: label, physical_nature, unit, comment

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

    ! floatingType group creation
    print *, "Creates /floatingType group creation ..."
    call h5gcreate_f(file_id, "floatingType", grp_id, hdferr)
    call check("Can't create /floatingType group")
    call h5gclose_f(grp_id, hdferr)

    ! arraySet group creation
    print *, "Creates /floatingType/an_arrayset group ..."
    call h5gcreate_f(file_id, "floatingType/an_arrayset", grp_id, hdferr)
    call check("Can't create /floatingType/an_arrayset group")
    call h5gclose_f(grp_id, hdferr)

    ! arraySet group creation
    print *, "Creates /floatingType/an_arrayset/ds group ..."
    call h5gcreate_f(file_id, "floatingType/an_arrayset/ds", grp_id, hdferr)
    call check("Can't create /floatingType/arrayset/ds group")
    call h5gclose_f(grp_id, hdferr)

    ! Writes floatingType attribute
    print *, "Writes /floatingType/an_arrayset/@floatingType ..."
    call h5ltset_attribute_string_f(file_id, "floatingType/an_arrayset", &
                                    "floatingType", "arraySet", hdferr)
    call check("Can't write floatingType attribute for floatingType/an_arrayset")

    ! Writes /floatingType/an_arrayset/data
    print *, "Writes /floatingType/an_arrayset/data ..."
    allocate(dims(2))
    dims = (/10, 10/)
    call h5ltmake_dataset_float_f(file_id, "floatingType/an_arrayset/data", 2, &
                                  dims, data, hdferr)
    call check("Can't write dataset for "//path)
    deallocate(dims)

    ! Writes /floatingType/an_arrayset/data attributes
    path = "floatingType/an_arrayset/data"
    call set_floating_type(file_id, path, "Electric Field", "electricField", &
                           "voltPerMeter", "electric field")

    ! Writes /floatingType/an_arrayset/ds/dim1
    path = "floatingType/an_arrayset/ds/dim1"
    print *, "Writes ", trim(path), " ..."
    allocate(dims(1))
    dims = (/10/)
    call h5ltmake_dataset_float_f(file_id, trim(path), 1, dims, dim1, hdferr)
    call check("Can't write dataset for "//path)
    deallocate(dims)
    ! Writes /floatingType/an_arrayset/ds/dim1 floatingType attributes ...
    call set_floating_type(file_id, trim(path), "x", "length", &
                           "meter", "The X axis")

    ! Writes /floatingType/an_arrayset/ds/dim2
    path = "floatingType/an_arrayset/ds/dim2"
    print *, "Writes ",  trim(path), " ..."
    allocate(dims(1))
    dims = (/10/)
    call h5ltmake_dataset_float_f(file_id, trim(path), 1, dims, dim2, hdferr)
    call check("Can't write dataset for "//path)
    deallocate(dims)
    ! Writes /floatingType/an_arrayset/ds/dim1 floatingType attributes ...
    call set_floating_type(file_id, trim(path), "x", "length", &
                           "meter", "The Y axis")

    ! close filename
    call h5fclose_f(file_id, hdferr)
    call check("Can't close "//trim(filename))

    ! close HDF5 library
    call h5close_f(hdferr)
    call check("Can't close HFD5")
end program write_an_arrayset
