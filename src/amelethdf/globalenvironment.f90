module globalenvironment_m
    use h5lt
    use h5tb
    use amelethdf_m, only : check, hdferr, &
                            EL => ELEMENT_NAME_LENGTH, &
                            AL => ABSOLUTE_PATH_NAME_LENGTH, &
                            read_string_attr => read_attribute
   use simpletype_m, only : A_PHYSICAL_NATURE

    implicit none

    character(len=*), parameter :: MSIG = "[globalenvironment]"


    type globalenvironment_t
        real(kind=4) :: minimum
        real(kind=4) :: maximum
        integer(kind=4) :: nbvalues
        character(len=10) :: domain = ""
        character(len=6) :: scaleType = ""
        real(kind=4), dimension(:), allocatable :: values
    end type globalenvironment_t

    contains
        subroutine read(file_id, path, ge)
            implicit none

            integer(hid_t), intent(in) :: file_id
            character(len=*), intent(in) :: path
            type(globalenvironment_t), intent(inout) :: ge

            integer(hsize_t), dimension(1) :: dims
            integer :: type_class
            integer(size_t) :: type_size
            character(len=EL) :: buf
            logical :: ok,oklin,oklog
            real(kind=4) :: step, steplog, test
            integer :: i
            
            oklin = .TRUE.
            oklog = .TRUE.
            call clear_content(ge)
            call h5ltget_dataset_info_f(file_id, path, dims, type_class, &
                                        type_size, hdferr)
            call check(MSIG//"Can't dims rank for "//path)
            ge%nbvalues = dims(1) 
            if (type_class == H5T_FLOAT_F) then
                allocate(ge%values(dims(1)))
                call h5ltread_dataset_float_f(file_id, path, &
                                              ge%values, dims, hdferr)
                call check(MSIG//"Can't read float values for "//path)
                ge%minimum = minval(ge%values)
                ge%maximum = maxval(ge%values)
                stepLog = (log10(ge%maximum) - log10(ge%minimum))/(ge%nbvalues-1)
                step = (ge%maximum - ge%minimum)/(ge%nbvalues-1)
                if(dims(1)>1) then
                    do i=2,dims(1)
                        if(oklin .eqv. .TRUE.) then
                            test = abs((step - (ge%values(i) - ge%values(i-1)))/step)
                            if(test < 0.01) then
                                oklin = .TRUE.
                            else
                                oklin = .FALSE.
                            endif
                        endif
                        if(oklog .eqv. .TRUE.) then
                            test = abs((steplog - (log10(ge%values(i)) - log10(ge%values(i-1))))/steplog)
                            if(test < 0.01) then
                                oklog = .TRUE.
                            else
                                oklog = .FALSE.
                            endif
                        endif
                   enddo
               endif
               if(oklin .eqv. .FALSE.) then
                   if(oklog .eqv. .FALSE.) then
                       ge%scaleType="other"
                   else
                       ge%scaleType="log"
                   endif
               else
                   ge%scaleType="lin"
               endif
                
            else
                hdferr = -1
                call check(MSIG//"no float values in "//path)
            endif
            
            buf = ""
            ok = read_string_attr(file_id, path, A_PHYSICAL_NATURE, buf)
            ge%domain = ""
            ge%domain = trim(buf)
           

       end subroutine read

       subroutine clear_content(ge)
           type(globalenvironment_t), intent(inout) :: ge

           if(allocated(ge%values)) deallocate(ge%values)
       end subroutine clear_content

end module globalenvironment_m
