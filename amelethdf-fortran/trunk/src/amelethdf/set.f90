!
! Sort of set module
! A character string can be only once in a set
!
module set_m
    use amelethdf_m, only : AL => ABSOLUTE_PATH_NAME_LENGTH

    implicit none

    character(len=*), parameter :: MSIG="[set]"

    type set_t
        character(len=AL), dimension(:), allocatable :: array
        integer :: number_of_element = 0
    end type set_t

    contains
        ! Return true if aelement is in aset
        function in(aset, aelement)
            type(set_t), intent(inout) :: aset
            character(len=*), intent(in) :: aelement
            logical :: in

            integer :: i

            in = .false.
            do i=1, aset%number_of_element
                if (aelement == aset%array(i)) in = .true.
            enddo
        end function in

        ! Add aelement to aset
        subroutine add(aset, aelement)
            type(set_t), intent(inout) :: aset
            character(len=*), intent(in) :: aelement

            if (.not. in(aset, aelement)) then
                aset%number_of_element = aset%number_of_element + 1
                aset%array(aset%number_of_element) = aelement
            endif
        end subroutine

        ! Get  ith from aset
        function get(aset, id) result(element)
            type(set_t), intent(inout) :: aset
            integer, intent(in) :: id
            character(len=len(aset%array)) :: element

            element = aset%array(id)
        end function get

        ! Return the index of an element
        function get_index(aset, aelement)
            type(set_t), intent(inout) :: aset
            character(len=*), intent(in) :: aelement

            integer :: i, get_index

            get_index = -1
            do i=1, aset%number_of_element
                if (aelement == aset%array(i)) then
                    get_index = i
                    cycle
                endif
            enddo
        end function

        subroutine clean(aset)
            type(set_t), intent(inout) :: aset

            aset%array = ""
        end subroutine clean
end module set_m
