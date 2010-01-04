module simulation_m
    use h5lt
    use amelethdf_m, only : check, hdferr, EL => ELEMENT_NAME_LENGTH, &
                                           AL => ABSOLUTE_PATH_NAME_LENGTH
    use stringdataset_m, only : read_string_dataset1

    implicit none

    character(len=*), parameter :: MSIG = "[simulation]"
    character(len=*), parameter :: C_SIMULATION = "/simulation/"
    character(len=*), parameter :: INPUTS = "/inputs"
    character(len=*), parameter :: OUTPUTS = "/outputs"
    character(len=*), parameter :: PARAMETERS = "/parameters"

    type simulation_t
        character(len=AL) :: name = ""
        character(len=AL), dimension(:), allocatable :: inputs, outputs
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

        subroutine read(file_id, sim_path, sim)
            implicit none

            integer(hid_t), intent(in) :: file_id
            character(len=*), intent(in) :: sim_path
            type(simulation_t), intent(inout) :: sim

            character(len=AL) :: path = ""

            sim%name(:) = trim(sim_path)
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
            implicit none

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
            call read_string_dataset1(file_id, path, dataset, type_size, m, 1)
        end subroutine read_dataset

        ! Print a simulation
        subroutine printt(sim)
            implicit none

            type(simulation_t), intent(inout) :: sim

            integer :: i

            print *, "Name : ", trim(sim%name)
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
end module simulation_m
