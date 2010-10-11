module complextype_m
    use hdf5
    use amelethdf_m, only : check, hdferr

    implicit none

    character(len=*), parameter :: MSIG = "[COMPLEXTYPE]"
    integer(hid_t) :: real_type_id = 0
    integer(hid_t) :: double_type_id = 0

    contains
        ! Create the real datatype
        subroutine create_type_id(real_or_double, type_id)
            integer(hid_t), intent(in) :: real_or_double
            integer(hid_t), intent(out) :: type_id

            integer(size_t) :: type_size, two_type_size, offset

            offset = 0

            call h5tget_size_f(real_or_double, type_size, hdferr)
            call check(MSIG//"Can 't get size")
            two_type_size = type_size*2
            call h5tcreate_f(H5T_COMPOUND_F, two_type_size, &
                             type_id, hdferr)
            call check(MSIG//"Can 't create type id")
            call h5tinsert_f(type_id, "r", offset, real_or_double, hdferr)
            call check(MSIG//"Can 't insert 'r' type id")
            call h5tinsert_f(type_id, "i", type_size, real_or_double, hdferr)
            call check(MSIG//"Can 't insert 'i' type id")
        end subroutine create_type_id

        ! Create the real datatype
        subroutine create_real_type_id()
            call create_type_id(H5T_NATIVE_REAL, real_type_id)
        end subroutine create_real_type_id

        ! Create the double datatype
        subroutine create_double_type_id()
            call create_type_id(H5T_NATIVE_DOUBLE, double_type_id)
        end subroutine create_double_type_id

        ! Create complex type ids
        subroutine create_type_ids()
            if (real_type_id == 0) call create_real_type_id()
            if (double_type_id == 0) call create_double_type_id()
        end subroutine create_type_ids

        ! write the named complex type in a file (at the root)
        subroutine write_complex_type(file_id)
            integer(hid_t), intent(in) :: file_id

            call create_type_ids()
            call h5tcommit_f(file_id, "/real_complex_type", real_type_id, hdferr)
            call check(MSIG//"Can 't commit real type id")
            call h5tcommit_f(file_id, "/double_complex_type", double_type_id, hdferr)
            call check(MSIG//"Can 't commit complex type id")
        end subroutine write_complex_type

        ! Writes a complex attribute at path
        subroutine write_attribute(file_id, path, name, value)
            integer(hid_t), intent(in) :: file_id
            character(len=*), intent(in) :: path
            character(len=*), intent(in) :: name
            complex, intent(in) :: value

            integer(hid_t) :: space_id, attr_id
            integer(hsize_t), dimension(1) :: dims = (/1/)
            real, dimension(2) :: buf

            call create_type_ids()
            call h5screate_simple_f(1, dims, space_id, hdferr)
            call check(MSIG//"Can 't create data space")
            call h5acreate_by_name_f(file_id, path, name, real_type_id, &
                                     space_id, attr_id, hdferr)
            call check(MSIG//"Can 't create attribute name")
            buf(1) = real(value)
            buf(2) = aimag(value)
            call h5awrite_f(attr_id, real_type_id,  buf, dims, hdferr)
            call check(MSIG//"Can 't write attribute values")
        end subroutine

        ! Read a complex attribute at path
        function read_attribute(file_id, path, name, value, mandatory) result(here)
            integer(hid_t), intent(in) :: file_id
            character(len=*), intent(in) :: path
            character(len=*), intent(in) :: name
            complex, intent(out) :: value
            logical, intent(in), optional :: mandatory

            integer(hid_t) :: attr_id
            integer(hsize_t), dimension(1) :: dims
            real, dimension(2) :: buf
            logical :: here, mandatory1

            mandatory1 = .false.
            if (present(mandatory)) mandatory1 = mandatory

            dims = (/1/)
            here = .true.
            call h5aexists_by_name_f(file_id, path, name, &
                                     here, hdferr, H5P_DEFAULT_F)
            if (mandatory1 .and. .not. here) then
                hdferr = -1
                call check(MSIG//name//" does not exist for : "//path)
            endif
            if (here) then
                call create_type_ids()
                call h5aopen_by_name_f(file_id, path, name, attr_id, hdferr)
                call check(MSIG//"Can 't open attribute name")
                call h5aread_f(attr_id, real_type_id, buf, dims, hdferr)
                call check(MSIG//"Can't read attribute")
                value = cmplx(buf(1), buf(2))
                call h5aclose_f(attr_id, hdferr)
                call check(MSIG//"Can't close attribute")
            endif
        end function read_attribute

        ! Read a complex dataset
        subroutine read_dataset(file_id, path, values)
            use h5lt

            integer(hid_t), intent(in) :: file_id
            character(len=*), intent(in) :: path
            complex, dimension(:), allocatable, intent(out) :: values

            integer :: rank, i
            integer(hsize_t), dimension(:), allocatable :: dims
            integer :: type_class, length
            integer(size_t) :: type_size
            real, dimension(:, :), allocatable :: buf

            call create_type_ids()
            call h5ltget_dataset_ndims_f(file_id, path, rank, hdferr)
            call check(MSIG//"Can't read dataset rank")
            allocate(dims(rank))
            dims = 0
            call h5ltget_dataset_info_f(file_id, path, dims, type_class, &
                                        type_size, hdferr)
            call check(MSIG//"Can't read dataset info")

            length = product(dims)
            allocate(buf(2, length))
            call h5ltread_dataset_f(file_id, path, real_type_id, &
                                    buf, dims, hdferr)
            call check(MSIG//"Can't read dataset values")

            allocate(values(length))
            do i=1, length
                values(i) = cmplx(buf(1, i), buf(2, i))
            enddo
            deallocate(dims, buf)
        end subroutine read_dataset

        ! Write a 1D complex dataset
        subroutine write_dataset(file_id, path, values)
            use h5lt

            integer(hid_t), intent(in) :: file_id
            character(len=*), intent(in) :: path
            complex, dimension(:), intent(in) :: values

            character(len=*), parameter :: MSIG1 = MSIG//"[WRITE_DATASET] "
            integer :: rank = 1
            integer(hsize_t), dimension(1) :: dims
            integer(hid_t) :: dataspace_id, dset_id, dtr_id, dti_id
            integer(size_t) :: type_size, offset
            integer(hid_t) :: type_id

            offset = 0

            call create_type_ids()
            type_id = real_type_id

            dims(1) = size(values)

            ! Create the dataspace
            call h5screate_simple_f(rank, dims, dataspace_id, hdferr)
            call check(MSIG1//"Can't create data space")

            ! Create the dataset
            call h5dcreate_f(file_id, path, type_id, &
                             dataspace_id, dset_id, hdferr)
            call check(MSIG1//"Can't create dataset")

            ! Create datatype for writing
            call h5tget_size_f(H5T_NATIVE_REAL, type_size, hdferr)
            call check(MSIG1//"Can't get size of type_id")

            call h5tcreate_f(H5T_COMPOUND_F, type_size, dtr_id, hdferr)
            call check(MSIG1//"Can't create real data type")
!            call h5tinsert_f(dtr_id, "r", 0, type_id, hdferr)
            call h5tinsert_f(dtr_id, "r", offset, H5T_NATIVE_REAL, hdferr)
            call check(MSIG1//"Can't insert real part")

            call h5tcreate_f(H5T_COMPOUND_F, type_size, dti_id, hdferr)
            call check(MSIG1//"Can't create imaginary data type")
!            call h5tinsert_f(dti_id, "i", 0, type_id, hdferr)
            call h5tinsert_f(dti_id, "i", offset, H5T_NATIVE_REAL, hdferr)
             call check(MSIG1//"Can't insert imaginary part")

            ! Write data
            call h5dwrite_f(dset_id, dtr_id, real(values), &
                            dims, hdferr, H5P_DEFAULT_F)
            call check(MSIG1//"Can't write real part")
            call h5dwrite_f(dset_id, dti_id, aimag(values), &
                            dims, hdferr, H5P_DEFAULT_F)
            call check(MSIG1//"Can't write imaginary part")

            call h5tclose_f(dtr_id, hdferr)
            call h5tclose_f(dti_id, hdferr)
            call h5dclose_f(dset_id, hdferr)
            call h5sclose_f(dataspace_id, hdferr)
        end subroutine write_dataset

        ! Write a nD complex dataset
        subroutine write_nd_dataset(file_id, path, values, values_shape)
            use h5lt

            integer(hid_t), intent(in) :: file_id
            character(len=*), intent(in) :: path
            integer, dimension(:), intent(in) :: values_shape
            complex, dimension(product(values_shape)), intent(in) :: values

            character(len=*), parameter :: MSIG1 = MSIG//"[WRITE_ND_DATASET] "
            integer :: rank
            integer(hsize_t), dimension(:), allocatable :: dims
            integer(hid_t) :: dataspace_id, dset_id, dtr_id, dti_id
            integer(size_t) :: offset, type_size
            integer(hid_t) :: type_id

            offset = 0

            call create_type_ids()
            type_id = real_type_id

            rank = size(values_shape)

            allocate(dims(rank))
            dims = values_shape

            ! Create the dataspace
            call h5screate_simple_f(rank, dims, dataspace_id, hdferr)
            call check(MSIG1//"Can't create data space")

            ! Create the dataset
            call h5dcreate_f(file_id, trim(path), type_id, &
                             dataspace_id, dset_id, hdferr)
            call check(MSIG1//"Can't create dataset")

            ! Create datatype for writing
            call h5tget_size_f(H5T_NATIVE_REAL, type_size, hdferr)
            call check(MSIG1//"Can't get size of type_id")

            call h5tcreate_f(H5T_COMPOUND_F, type_size, dtr_id, hdferr)
            call check(MSIG1//"Can't create real data type")
            call h5tinsert_f(dtr_id, "r", offset, H5T_NATIVE_REAL, hdferr)
            call check(MSIG1//"Can't insert real part")

            call h5tcreate_f(H5T_COMPOUND_F, type_size, dti_id, hdferr)
            call check(MSIG1//"Can't create imaginary data type")
            call h5tinsert_f(dti_id, "i", offset, H5T_NATIVE_REAL, hdferr)
            call check(MSIG1//"Can't insert imaginary part")

            ! Write data
            call h5dwrite_f(dset_id, dtr_id, real(values), &
                            dims, hdferr, H5P_DEFAULT_F)
            call check(MSIG1//"Can't write real part")
            call h5dwrite_f(dset_id, dti_id, aimag(values), &
                            dims, hdferr, H5P_DEFAULT_F)
            call check(MSIG1//"Can't write imaginary part")

            call h5tclose_f(dtr_id, hdferr)
            call h5tclose_f(dti_id, hdferr)
            call h5dclose_f(dset_id, hdferr)
            call h5sclose_f(dataspace_id, hdferr)

            deallocate(dims)
        end subroutine write_nd_dataset


        subroutine read_cattribute(file_id, path, name, value)
            use h5lt

            integer(hid_t), intent(in) :: file_id
            character(len=*), intent(in) :: path
            character(len=*), intent(in) :: name
            complex, intent(out) :: value

            real, dimension(2) :: buf

            call h5ltget_attribute_float_f(file_id, path, name, &
                                           buf, hdferr)
            call check(MSIG//"Can't read attribute "//trim(path)//"@"//name)
            value = cmplx(buf(1), buf(2))
        end subroutine read_cattribute
end module complextype_m

