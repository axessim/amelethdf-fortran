program settest
    use set_m, only : set_t, add, get_index, in

    ! Amelet types
    type(set_t) :: set

    allocate(set%array(10))
    set%array(1) = "one"
    set%array(2) = "two"
    set%array(3) = "three"
    set%number_of_element = 3

    write(*, "(3a)", advance="no") "1 : "
    if (in(set, "two")) write(*, *) "OK"

    call add(set, "four")
    write(*, "(3a)", advance="no") "2 : "
    if (set%number_of_element == 4) print *, "OK"

    write(*, "(3a)", advance="no") "3 : "
    if (get_index(set, "three") == 3) print *, "OK"

    write(*, "(3a)", advance="no") "4 : "
    if (get_index(set, "three     ") == 3) print *, "OK"
    print *, "End"
end program settest
