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

	subroutine write_string_dataset(file_id, path, buf)
	    integer(hid_t), intent(in) :: file_id
        character(len=*), intent(in) :: path
        character(len=*), dimension(:), intent(in) :: buf

        integer :: rank
        integer(hsize_t), dimension(1) :: dims
        integer(hid_t) :: space_id, dset_id, str_type_id
        integer(size_t) :: buf_size

        rank = 1
        dims = size(buf)
        buf_size = len(buf)
        call h5screate_simple_f(rank, dims, space_id, hdferr)
        call h5tcopy_f(H5T_NATIVE_CHARACTER, str_type_id, hdferr)
        call H5tset_size_f(str_type_id, buf_size, hdferr);
        call h5dcreate_f(file_id, path, str_type_id, space_id, dset_id, hdferr)
        call h5dwrite_f(dset_id, str_type_id, buf, dims, hdferr)

        call h5dclose_f(dset_id, hdferr)
        call h5sclose_f(space_id, hdferr)
        call h5tclose_f(str_type_id, hdferr)
	end subroutine write_string_dataset
end module tools

! This program write an HDF5 file with the following arraySet structure
!
! data.h5
! |-- simulation
! |   `-- outputs
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
program write_an_output_file
    use hdf5
    use h5lt
    use tools

    implicit none

    integer :: i, j
    integer(hid_t) :: file_id, grp_id, grp2_id
    character(len=AL) :: command_line_buffer
    character(len=AL) :: filename, path
    integer(hsize_t), dimension(:), allocatable :: dims

    real, dimension(10, 10) :: data
    real, dimension(10) :: dim1, dim2
    character(len=AL) :: label, physical_nature, unit, comment
    character(len=AL), dimension(:), allocatable :: outputs

    ! Data initialization
    do i=1, size(dim1)
        dim1(i) = 10.0*i
        do j=1, size(dim2)
            dim2 = 12.0*j
            data = i**2 + j**2
        enddo
    enddo

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

    ! Amelet attributes
    print *, "Write FORMAT attribute..."
    call h5ltset_attribute_string_f(file_id, "/", &
                                    "FORMAT", "AMELET-HDF", hdferr)
    call check("Can't write FORMAT attribute for "//path)

    print *, "Write AMELETHDF_FORMAT_VERSION attribute..."
    call h5ltset_attribute_string_f(file_id, "/", &
                                    "AMELETHDF_FORMAT_VERSION", &
                                    "1.0.0", hdferr)
    call check("Can't write AMELETHDF_FORMAT_VERSION attribute for "//path)

    print *, "Write entryPoint attribute..."
    call h5ltset_attribute_string_f(file_id, "/", &
                                    "entryPoint", &
                                    "/simulation/this_simulation", hdferr)
    call check("Can't write entryPoint attribute for "//path)


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


    ! Writes /simulation group
    print *, "Creates /simulation ..."
    path = "simulation"
    call h5gcreate_f(file_id, trim(path), grp_id, hdferr)
    call check("Can't create /simulation")
    call h5gclose_f(grp_id, hdferr)

    ! Writes /simulation/this_simulation group
    print *, "Creates /simulation/this_simulation ..."
    path = "simulation/this_simulation"
    call h5gcreate_f(file_id, trim(path), grp_id, hdferr)
    call check("Can't create /simulation")
    call h5gclose_f(grp_id, hdferr)

    ! Adds this_simulation attributes
    print *, "Writes /", trim(path)//"@module ..."
    call h5ltset_attribute_string_f(file_id, trim(path), &
                                    "module", "a_module", hdferr)
    call check("Can't write module attribute for "//path)
    print *, "Writes /", trim(path)//"@version ..."
    call h5ltset_attribute_string_f(file_id, trim(path), &
                                    "version", "1.0.0", hdferr)
    call check("Can't write version attribute for "//path)


    ! Writes simulation/this_simulation/outputs
    allocate(outputs(1))
    outputs(1) = "/floatingType/an_arrayset"
    path = "simulation/this_simulation/outputs"
    print *, "Writes /", trim(path) //" ..."
    call write_string_dataset(file_id, trim(path), outputs)
    call check("Can't create "//path)


    ! close filename
    call h5fclose_f(file_id, hdferr)
    call check("Can't close "//trim(filename))

    ! close HDF5 library
    call h5close_f(hdferr)
    call check("Can't close HFD5")
end program write_an_output_file
