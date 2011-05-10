module simulation_m
    use h5lt
    use amelethdf_m, only : check, hdferr, EL => ELEMENT_NAME_LENGTH, &
                                           AL => ABSOLUTE_PATH_NAME_LENGTH, &
                                           trim_null_char, read_attribute
    use stringdataset_m, only : read_string_dataset1

    implicit none

    character(len=*), parameter :: MSIG = "[simulation]"
    character(len=*), parameter :: C_SIMULATION = "/simulation/"
    character(len=*), parameter :: INPUTS = "/inputs"
    character(len=*), parameter :: OUTPUTS = "/outputs"
    character(len=*), parameter :: PARAMETERS = "/parameters"
    character(len=*), parameter :: A_MODULE_NAME = "module"
    character(len=*), parameter :: A_MODULE_VERSION = "version"

    type simulation_t
        character(len=AL) :: name = ""
        character(len=AL), dimension(:), allocatable :: inputs, outputs
        character(len=EL) :: modulename = ""
        character(len=EL) :: moduleversion = ""
    end type simulation_t

    contains
!        subroutine read(file_id)
!            implicit none
!
!            integer(hid_t), intent(in) :: file_id
!
!            character(len=EL) :: name = "UNKNOWN"
!            integer :: number_of_simulations, obj_type, i
!
!            call h5gn_members_f(file_id, C_SIMULATION, number_of_simulations, hdferr)
!            call check("\nCan't read the number of simulations")
!            print *, "\tNumber of simulations : ", number_of_simulations
!
!            do i=1,number_of_simulations
!                call h5gget_obj_info_idx_f(file_id, C_SIMULATION, i-1, name, &
!                                           obj_type, hdferr)
!                call check("\nCan't read the name of simulations")
!
!                print *, "\t\tSimulation's name : ", name
!                call read_at(file_id, name)
!            end do
!        end subroutine read

        ! Reads a simulation
        subroutine read(file_id, sim_path, sim)
            integer(hid_t), intent(in) :: file_id
            character(len=*), intent(in) :: sim_path
            type(simulation_t), intent(inout) :: sim

            character(len=AL) :: path = ""
            logical :: here

            sim%name(:) = trim(sim_path)
            here = read_attribute(file_id,sim_path, A_MODULE_NAME, sim%modulename, .true.)
            here = read_attribute(file_id,sim_path, A_MODULE_VERSION, sim%moduleversion, .true.)
            path = trim(sim_path)//INPUTS
            if (h5ltfind_dataset_f(file_id, path) == 0) then
                call read_dataset(file_id, path, sim%inputs)
            endif
            path = trim(sim_path)//OUTPUTS
            if (h5ltfind_dataset_f(file_id, path) == 0) then
                call read_dataset(file_id, path, sim%outputs)
            endif
            
        end subroutine read

        ! Read n x 1 string dataset
        subroutine read_dataset(file_id, path, dataset)
            integer(hid_t), intent(in) :: file_id
            character(len=*), intent(in) :: path
            character(len=*), dimension(:), allocatable, intent(inout) :: dataset

            integer :: type_class, m, n
            
            integer(hsize_t), dimension(1) :: dims = 0
            integer(size_t) :: type_size

            call h5ltget_dataset_info_f(file_id, path, dims, type_class, &
                                        type_size, hdferr)
            call check("\nCan't read the number of inputs for "//path)
            m = dims(1); n= 1
            allocate(dataset(m))
            call read_string_dataset1(file_id, path, dataset, &
                                      int(type_size), m, 1)
            call trim_null_char(dataset)
        end subroutine read_dataset

        ! Print a simulation
        subroutine printt(sim)
            type(simulation_t), intent(in) :: sim

            integer :: i

            print *, "Name : ", trim(sim%name)
            print *, "  Module : ", trim(sim%modulename)
            print *, "  Implementation version : ", trim(sim%moduleversion)
            if (allocated(sim%inputs)) then
                do i=1, size(sim%inputs)
                    print *, "Inputs : ", trim(sim%inputs(i))
                enddo
            endif
            if (allocated(sim%outputs)) then
                do i=1, size(sim%outputs)
                    print *, "Outputs : ", trim(sim%outputs(i))
                enddo
            endif
        end subroutine printt

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


        ! Clear content
        subroutine clear_content(sim)
            type(simulation_t), intent(inout) :: sim

            sim%name = ""
            if (allocated(sim%inputs)) deallocate(sim%inputs)
            if (allocated(sim%outputs)) deallocate(sim%outputs)
        end subroutine clear_content

        ! Write a simulation
        subroutine write(file_id, sim)
            type(simulation_t), intent(in) :: sim
            integer(hid_t), intent(in) :: file_id
            integer(hid_t) :: grp_id

            character(len=AL) :: path = ""
            integer i

            path = trim(sim%name)
            !call h5gcreate_f(file_id,C_SIMULATION, grp_id, hdferr)
            !call check("Can't create /simulation group")
            call h5gcreate_f(file_id,path, grp_id, hdferr)
            call check("Can't create group" //path)
            call h5ltset_attribute_string_f(file_id,path,A_MODULE_NAME,&
                                              sim%modulename, hdferr)
            call check("Can't create attibute for " //A_MODULE_NAME)        
            call h5ltset_attribute_string_f(file_id,path,A_MODULE_VERSION,&
                                              sim%moduleversion, hdferr)
            call check("Can't create attibute for " //A_MODULE_VERSION)
            path = trim(sim%name)//INPUTS
            call write_string_dataset(file_id, path, sim%inputs)
            path = trim(sim%name)//OUTPUTS
            call write_string_dataset(file_id, path, sim%outputs)
        end subroutine write

end module simulation_m
