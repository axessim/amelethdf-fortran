module planewave_m
    use h5lt
    use amelethdf_m, only : check, hdferr, ELENGTH => ELEMENT_NAME_LENGTH,&
                            read_attribute, read_float_attribute
    use complextype_m, only: read_complex_attribute => read_attribute
    use floatingtype_m, only : floatingtype_t, read_floatingtype => read, &
                               clear_content_floatingtype => clear_content

    implicit none

    character(len=*), parameter :: MSIG = "[planewave]"
    character(len=*), parameter :: C_PLANE_WAVE = "planeWave"

    character(len=*), parameter :: F_MAGNITUDE = "magnitude"
    character(len=*), parameter :: A_THETA = "theta"
    character(len=*), parameter :: A_PHI = "phi"
    character(len=*), parameter :: A_LINEAR_POLARIZATION = "linearPolarization"
    character(len=*), parameter :: A_ELLIPTICAL_POLARIZATION_ETHETA = &
                                   "ellipticalPolarizationETheta"
    character(len=*), parameter :: A_ELLIPTICAL_POLARIZATION_EPHI = &
                                   "ellipticalPolarizationEPhi"

    integer, parameter :: LINEAR = 1
    integer, parameter :: ELLIPTIC = 2

    type planewave_t
        real :: theta, phi, linear_polarization
        integer :: polarization
        complex :: elliptical_Polarization_etheta, elliptical_polarization_ephi
        type(floatingtype_t) :: magnitude
    end type planewave_t

    contains
        ! read a planewave_t
        subroutine read(file_id, path, pw)
            implicit none

            integer(hid_t), intent(in) :: file_id
            character(len=*), intent(in) :: path
            type(planewave_t), intent(inout) :: pw

            logical :: ok

            ok = read_float_attribute(file_id, path, A_THETA, pw%theta, .true.)
            ok = read_float_attribute(file_id, path, A_PHI, pw%phi, .true.)

            pw%polarization = LINEAR
            ok = read_float_attribute(file_id, path, A_LINEAR_POLARIZATION, &
                                      pw%linear_polarization)
            if (.not. ok) then
                ok = read_complex_attribute(file_id, path, &
                    A_ELLIPTICAL_POLARIZATION_ETHETA, &
                    pw%elliptical_Polarization_etheta)
                hdferr = -1
                if (.not. ok) then
                    call check(MSIG//A_ELLIPTICAL_POLARIZATION_ETHETA// &
                               " should be present for "//path)
                endif
                ok = read_complex_attribute(file_id, path, &
                    A_ELLIPTICAL_POLARIZATION_EPHI, &
                    pw%elliptical_polarization_ephi)
                if (.not. ok) then
                    call check(MSIG//A_ELLIPTICAL_POLARIZATION_EPHI// &
                               " should be present for "//path)
                endif
                pw%polarization = ELLIPTIC
            endif

            call read_floatingtype(file_id, trim(path)//"/"//F_MAGNITUDE, &
                                   pw%magnitude)
        end subroutine read

        subroutine clear_content(planewave)
            implicit none

            type(planewave_t), intent(inout) :: planewave

            planewave%theta = 0
            planewave%phi = 0
            planewave%linear_polarization = 0
            planewave%elliptical_Polarization_etheta = 0
            planewave%elliptical_polarization_ephi = 0
            call clear_content_floatingtype(planewave%magnitude)
        end subroutine clear_content

        ! helper functions
        logical function islinear(planewave)
            implicit none

            type(planewave_t), intent(in) :: planewave

            islinear = (planewave%polarization == LINEAR)
        end function islinear

        logical function iselliptic(planewave)
            implicit none

            type(planewave_t), intent(in) :: planewave

            iselliptic = (planewave%polarization == ELLIPTIC)
        end function iselliptic
end module planewave_m
