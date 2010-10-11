! Test module to use a in-memory HDF5 file.
! In-memory file can be useful for unit test.

module inmemory
    use hdf5
    use amelethdf, only : check, hdferr

    implicit none

    ! String signature
    character(len=*), parameter :: MSIG = "[IN-MEMORY] "
    ! file block size in bytes.
    integer(size_t), parameter :: increment = 1024**2
    ! Backing storage strategy
    logical, parameter :: backing_store = .false.

contains
    ! Create an in-memory file
    subroutine create_inmemory_file(name, file_id)
        ! file name
        character(len=*), intent(in) :: name
	    ! File id
	    integer(hid_t), intent(inout) :: file_id
	    ! Property list access id
	    integer(hid_t) :: faplist_id
	    ! Error flag

	    ! HDF5 library initialization
	    call h5open_f(hdferr)

        ! Create a property list for file access
	    call h5pcreate_f(H5P_FILE_ACCESS_F, faplist_id, hdferr)
	    call check(MSIG//"Can't create property list")

        ! Set the driver to "core" with the property list
	    call h5pset_fapl_core_f(faplist_id, increment, backing_store, hdferr)
        call check(MSIG//"Can't set core driver")

        ! Create the file with the property list
	    call h5fcreate_f(name, H5F_ACC_TRUNC_F, file_id, hdferr, &
	                     H5P_DEFAULT_F, faplist_id)
    end subroutine create_inmemory_file
end module inmemory

