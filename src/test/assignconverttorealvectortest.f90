program converttorealvectortest
    use hdf5
    use amelethdf_m, only : check, hdferr, &
                            EL => ELEMENT_NAME_LENGTH, &
                            AL => ABSOLUTE_PATH_NAME_LENGTH
    use floatingtype_m, only : floatingtype_t, vector_t, get_type, read, &
                               issinglereal, issinglecomplex, isvector, &
                               isdataset, isarrayset, clear_content, &
                               convert_to_real_vector, E_VECTOR

    type(floatingtype_t), target :: ft
    type(vector_t), pointer :: vp
    type(vector_t) :: vp1
    real, dimension(:), pointer :: vd
    real, dimension(:), allocatable :: vd1

    ft%floatingtype = E_VECTOR
    allocate(ft%vector%rvalue(10))
    ft%vector%rvalue = 11.5

    vp => ft%vector
    if (vp%rvalue(1) /= 11.5 ) then
        print *, "Error, vp%irvalue /= 11.5)"
    endif
    print *, "Ok, pointer test."

    vp1 = ft%vector
    if (vp1%rvalue(1) /= 11.5 ) then
        print *, "Error, vp1%irvalue /= 11.5)"
    endif
    print *, "Ok, assignment test."


    vd => convert_to_real_vector(ft)
    if (vd(1) /= 11.5 ) then
        print *, "Error, vd /= 11.5"
    endif
    print *, "Ok, convert_to_real_vector by pointer test."


    if (allocated(vd1)) then
        print *, "Error, vd1 should not be allocated"
        stop
    endif
    allocate(vd1(10))
    vd1 = convert_to_real_vector(ft)
    if (vd1(1) /= 11.5 ) then
        print *, "Error, vd1 /= 11.5"
    endif
    if (.not. allocated(vd1)) then
        print *, "Error, vd1 should be allocated"
    endif
    print *, "Ok, convert_to_real_vector by assignment test."

    print *, "Ok"
    print *, "End"
end program converttorealvectortest
