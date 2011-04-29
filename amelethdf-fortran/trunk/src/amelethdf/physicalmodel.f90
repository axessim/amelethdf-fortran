module physicalmodel_m
    use h5lt
    use h5tb
    use amelethdf_m, only : check, hdferr, &
                            EL => ELEMENT_NAME_LENGTH, &
                            AL => ABSOLUTE_PATH_NAME_LENGTH, &
                            trim_null_char, &
                            read_string_attr => read_attribute, &
                            read_float_attribute, read_int_attribute, &
                            read_children_name
    use floatingtype_m, only : floatingtype_t, floatingtype_read => read, &
                               floatingtype_clear_content => clear_content
    use hdfpath_m, only : exists

    implicit none

    character(len=*), parameter :: MSIG = "[physicalmodel]"

    character(len=*), parameter :: C_PHYSICAL_MODEL = "/physicalModel/"
    character(len=*), parameter :: C_MULTILAYER = C_PHYSICAL_MODEL//"multilayer/"
    character(len=*), parameter :: C_GRID = C_PHYSICAL_MODEL//"grid/"
    character(len=*), parameter :: C_PHYSICAL_VOLUME = C_PHYSICAL_MODEL//"volume/"
    character(len=*), parameter :: RELATIVE_PERMITTIVITY = "/relativePermittivity/"
    character(len=*), parameter :: RELATIVE_PERMEABILITY = "/relativePermeability/"
    character(len=*), parameter :: ELECTRIC_CONDUCTIVITY = "/electricConductivity/"
    character(len=*), parameter :: MAGNETIC_CONDUCTIVITY = "/magneticConductivity/"
    character(len=*), parameter :: TC_PHYSICALMODEL = "physicalModel"
    character(len=*), parameter :: TC_THICKNESS = "thickness"
    character(len=*), parameter :: A_TEXTURE_TYPE = "textureType"
    character(len=*), parameter :: A_SURROUNDING_MATERIAL = "surroundingMaterial"
    character(len=*), parameter :: A_GRID_MATERIAL = "gridMaterial"
    character(len=*), parameter :: A_PITCH_FIBER = "pitchFiber"
    character(len=*), parameter :: A_FIBER_PER_PITCH = "fiberPerPitch"
    character(len=*), parameter :: A_WIRE_SECTION_TYPE = "wireSectionType"
    character(len=*), parameter :: A_DIAMETER_WIRE = "diameterWire"
    character(len=*), parameter :: A_SHIFT = "shift"
    character(len=*), parameter :: A_THICKNESS_WIRE = "thicknessWire"
    character(len=*), parameter :: A_WIDTH_WIRE = "widthWire"
    character(len=*), parameter :: A_RELATIVE_HEIGHT = "relativeHeight"
    character(len=*), parameter :: A_ANGLE = "angle"
    character(len=*), parameter :: A_PITCH = "pitch"
    character(len=*), parameter :: A_VOL_FRACTION_FILLER = "volFractioFiller"
    character(len=*), parameter :: A_LENGTH_WIRE = "lengthWire"
    character(len=*), parameter :: A_SCALE_FILLER = "scaleFiller"
    character(len=*), parameter :: A_TYPE_FILLER = "typeFiller"
    character(len=EL), dimension(:), allocatable :: children_name

    type physicalvolume_t
        type(floatingtype_t) :: relative_permittivity
        type(floatingtype_t) :: relative_permeability
        type(floatingtype_t) :: electric_conductivity
        type(floatingtype_t) :: magnetic_conductivity
    end type physicalvolume_t

    type multilayer_t
        character(len=AL), dimension(:), allocatable :: physicalModel
        real(kind=4), dimension(:), allocatable :: thickness
        integer(kind=4) :: nblayers
    end type multilayer_t


    type combgrid_t
        integer(kind=4) :: nbcombs
        real(kind=4), dimension(:), allocatable :: relativeHeight
        real(kind=4), dimension(:), allocatable :: angle
        real(kind=4), dimension(:), allocatable :: pitch
        character(len=AL) :: surroundingMaterial = ""
        character(len=AL) :: gridMaterial = ""
        character(len=12) :: wireSectionType = ""
        real(kind=4)  :: shift
        real(kind=4)  :: thicknessWire
        real(kind=4)  :: widthWire
        real(kind=4)  :: diameterWire
    end type combgrid_t

    type randomgrid_t
        character(len=AL) :: surroundingMaterial = ""
        character(len=AL), dimension(:), allocatable :: gridMaterial
        real(kind=4), dimension(:), allocatable :: volFractioFiller
        real(kind=4), dimension(:), allocatable :: diameterWire
        real(kind=4), dimension(:), allocatable :: lengthWire
        character(len=6), dimension(:), allocatable :: scaleFiller
        character(len=7), dimension(:), allocatable :: typeFiller
        integer(kind=4) :: numberFillerType
    end type randomgrid_t

    type wovengrid_t
        real(kind=4) :: relativeHeight
        real(kind=4) :: angle
        character(len=AL) :: surroundingMaterial = ""
        character(len=AL) :: gridMaterial = ""
        character(len=12) :: wireSectionType = ""
        real(kind=4)  :: thicknessWire
        real(kind=4)  :: widthWire
        real(kind=4)  :: diameterWire
        real(kind=4)  :: pitchFiber
        integer(kind=4) :: fiberPerPitch
    end type wovengrid_t

    type grid_t
        character(len=10) :: textureType = ""
        type(combgrid_t) :: combgrid
        type(randomgrid_t) :: randomgrid
        type(wovengrid_t) :: wovengrid
    end type grid_t


    contains

        subroutine read(file_id, path, pv)
            implicit none

            integer(hid_t), intent(in) :: file_id
            character(len=*), intent(in) :: path
            type(physicalvolume_t), intent(inout) :: pv

            call floatingtype_read(file_id, trim(path)//RELATIVE_PERMITTIVITY, &
                                   pv%relative_permittivity)
            call floatingtype_read(file_id, trim(path)//RELATIVE_PERMEABILITY, &
                                   pv%relative_permeability)
            call floatingtype_read(file_id, trim(path)//ELECTRIC_CONDUCTIVITY, &
                                   pv%electric_conductivity)
            call floatingtype_read(file_id, trim(path)//MAGNETIC_CONDUCTIVITY, &
                                   pv%magnetic_conductivity)
        end subroutine read

        ! Clear the content of a physicalvolume_t
        subroutine clear_content(pv)
            type(physicalvolume_t), intent(inout) :: pv

            call floatingtype_clear_content(pv%relative_permittivity)
            call floatingtype_clear_content(pv%relative_permeability)
            call floatingtype_clear_content(pv%electric_conductivity)
            call floatingtype_clear_content(pv%magnetic_conductivity)
        end subroutine clear_content

        subroutine multilayer_read(file_id, path, ml)
            implicit none

            integer(hid_t), intent(in) :: file_id
            character(len=*), intent(in) :: path
            type(multilayer_t), intent(inout) :: ml

            integer(hsize_t) :: nrecords, nfields, start
            integer(size_t) :: type_size
            character(len=AL), dimension(:), allocatable :: cbuf
            character(len=AL), dimension(:), allocatable :: field_names
            integer(size_t), dimension(:), allocatable :: field_sizes
            integer(size_t), dimension(:), allocatable :: field_offsets
            integer :: i


            call h5tbget_table_info_f(file_id, path, nfields, nrecords, hdferr)
            call check(MSIG//"Can't read table info for"//path)


            allocate(field_names(nfields))
            allocate(field_sizes(nfields))
            allocate(field_offsets(nfields))
            call check(MSIG//"Can't read field info for"//path)

            ml%nblayers=nrecords
            if (allocated(ml%physicalModel)) deallocate(ml%physicalModel)
            allocate(ml%physicalModel(nrecords))
            allocate(cbuf(nrecords))
            start = 0
            type_size = AL
            call h5tbread_field_name_f(file_id, path, TC_PHYSICALMODEL, &
                                       start, nrecords, type_size, &
                                       cbuf, hdferr)
            call check(MSIG//"Can't field values for"//path//"#"//TC_PHYSICALMODEL)
            do i=1,size(cbuf)
                call trim_null_char(cbuf(i))
            enddo
            ml%physicalModel(:) = cbuf(:)

 
            if (allocated(ml%thickness)) deallocate(ml%thickness)
            allocate(ml%thickness(nrecords))
            start = 0
            call h5tget_size_f(H5T_NATIVE_REAL_4, type_size, hdferr)
            call h5tbread_field_name_f(file_id, path, TC_THICKNESS, &
                                       start, nrecords, type_size, &
                                       ml%thickness, hdferr)
            call check(MSIG//"Can't field values for"//path//"#"//TC_THICKNESS)


            deallocate(field_names, field_sizes, field_offsets, cbuf)

        end subroutine multilayer_read

        subroutine woven_read(file_id, path, wovengrid)
            implicit none

            integer(hid_t), intent(in) :: file_id
            character(len=*), intent(in) :: path
            type(wovengrid_t), intent(inout) :: wovengrid
            character(len=AL) :: buf
            logical :: ok
            integer :: i
            character(len=AL) :: path2 = ""

            buf = ""
            ok = read_string_attr(file_id, path, A_SURROUNDING_MATERIAL, buf)
            wovengrid%surroundingMaterial = ""
            wovengrid%surroundingMaterial = trim(buf)

            buf = ""
            ok = read_string_attr(file_id, path, A_GRID_MATERIAL, buf)
            wovengrid%gridMaterial = ""
            wovengrid%gridMaterial = trim(buf)

            buf = ""
            ok = read_string_attr(file_id, path, A_WIRE_SECTION_TYPE, buf)
            wovengrid%wireSectionType = ""
            wovengrid%wireSectionType = trim(buf)
            if(wovengrid%wireSectionType == "rectangular") then
                ok = read_float_attribute(file_id, path, A_THICKNESS_WIRE, &
                                          wovengrid%thicknessWire, .true.)
                ok = read_float_attribute(file_id, path, A_WIDTH_WIRE, &
                                          wovengrid%widthWire, .true.)
            else if(wovengrid%wireSectionType == "circular") then
                ok = read_float_attribute(file_id, path, A_DIAMETER_WIRE, &
                                          wovengrid%diameterWire, .true.)
            endif

            ok = read_float_attribute(file_id, path, A_PITCH_FIBER, &
                                      wovengrid%pitchFiber, .true.)

            ok = read_int_attribute(file_id, path, A_FIBER_PER_PITCH, &
                                      wovengrid%fiberPerPitch, .true.)
            if (allocated(children_name)) deallocate(children_name)
            call read_children_name(file_id, path, children_name)
            do i=1, size(children_name)
                path2 = trim(path)//"/"//trim(children_name(i))
                ok = read_float_attribute(file_id, path2, &
                            A_RELATIVE_HEIGHT, wovengrid%relativeHeight, &
                            .true.)
                ok = read_float_attribute(file_id, path2, &
                            A_ANGLE, wovengrid%angle, &
                            .true.)
            enddo


        end subroutine woven_read


        subroutine comb_read(file_id, path, combgrid)
            implicit none

            integer(hid_t), intent(in) :: file_id
            character(len=*), intent(in) :: path
            type(combgrid_t), intent(inout) :: combgrid
            character(len=AL) :: buf 
            logical :: ok
            integer :: i
            character(len=AL) :: path2 = ""

            buf = ""
            ok = read_string_attr(file_id, path, A_SURROUNDING_MATERIAL, buf)
            combgrid%surroundingMaterial = ""
            combgrid%surroundingMaterial = trim(buf)

            buf = ""
            ok = read_string_attr(file_id, path, A_GRID_MATERIAL, buf)
            combgrid%gridMaterial = ""
            combgrid%gridMaterial = trim(buf)

            buf = ""
            ok = read_string_attr(file_id, path, A_WIRE_SECTION_TYPE, buf)
            combgrid%wireSectionType = ""
            combgrid%wireSectionType = trim(buf)
            if(combgrid%wireSectionType == "rectangular") then
                ok = read_float_attribute(file_id, path, A_THICKNESS_WIRE, &
                                          combgrid%thicknessWire, .true.)
                ok = read_float_attribute(file_id, path, A_WIDTH_WIRE, &
                                          combgrid%widthWire, .true.)
            else if(combgrid%wireSectionType == "circular") then
                ok = read_float_attribute(file_id, path, A_DIAMETER_WIRE, &
                                          combgrid%diameterWire, .true.)
            endif

            ok = read_float_attribute(file_id, path, A_SHIFT, &
                                      combgrid%shift, .true.)

            if (allocated(children_name)) deallocate(children_name)
            call read_children_name(file_id, path, children_name)
            combgrid%nbcombs = size(children_name)
            if (allocated(combgrid%relativeHeight)) deallocate(combgrid%relativeHeight)
            allocate(combgrid%relativeHeight(combgrid%nbcombs))
            if (allocated(combgrid%angle)) deallocate(combgrid%angle)
            allocate(combgrid%angle(combgrid%nbcombs))
            if (allocated(combgrid%pitch)) deallocate(combgrid%pitch)
            allocate(combgrid%pitch(combgrid%nbcombs))
            do i=1, size(children_name)
                path2 = trim(path)//"/"//trim(children_name(i))
                ok = read_float_attribute(file_id, path2, &
                            A_RELATIVE_HEIGHT, combgrid%relativeHeight(i), &
                            .true.)
                ok = read_float_attribute(file_id, path2, &
                            A_ANGLE, combgrid%angle(i), &
                            .true.)
                ok = read_float_attribute(file_id, path2, &
                            A_PITCH, combgrid%pitch(i), &
                            .true.)
            enddo


        end subroutine comb_read

        subroutine random_read(file_id, path, randomgrid)
            implicit none

            integer(hid_t), intent(in) :: file_id
            character(len=*), intent(in) :: path
            type(randomgrid_t), intent(inout) :: randomgrid
            character(len=AL) :: buf
            logical :: ok
            integer :: i
            character(len=AL) :: path2 = ""

            buf = ""
            ok = read_string_attr(file_id, path, A_SURROUNDING_MATERIAL, buf)
            randomgrid%surroundingMaterial = ""
            randomgrid%surroundingMaterial = trim(buf)

            if (allocated(children_name)) deallocate(children_name)
            call read_children_name(file_id, path, children_name)
            randomgrid%numberFillerType = size(children_name)
            if (allocated(randomgrid%volFractioFiller)) &
                    deallocate(randomgrid%volFractioFiller)
            allocate(randomgrid%volFractioFiller(randomgrid%numberFillerType))
            if (allocated(randomgrid%diameterWire)) &
                    deallocate(randomgrid%diameterWire)
            allocate(randomgrid%diameterWire(randomgrid%numberFillerType))
            if (allocated(randomgrid%lengthWire)) &
                    deallocate(randomgrid%lengthWire)
            allocate(randomgrid%lengthWire(randomgrid%numberFillerType))
            if (allocated(randomgrid%scaleFiller)) &
                    deallocate(randomgrid%scaleFiller)
            allocate(randomgrid%scaleFiller(randomgrid%numberFillerType))
            if (allocated(randomgrid%gridMaterial)) &
                    deallocate(randomgrid%gridMaterial)
            allocate(randomgrid%gridMaterial(randomgrid%numberFillerType))
            if (allocated(randomgrid%typeFiller)) &
                    deallocate(randomgrid%typeFiller)
            allocate(randomgrid%typeFiller(randomgrid%numberFillerType))
            
            

            do i=1, size(children_name)
                path2 = trim(path)//"/"//trim(children_name(i))
                ok = read_float_attribute(file_id, path2, &
                            A_VOL_FRACTION_FILLER,randomgrid%volFractioFiller(i), &
                            .true.)
                ok = read_float_attribute(file_id, path2, &
                            A_DIAMETER_WIRE, randomgrid%diameterWire(i), &
                            .true.)
                ok = read_float_attribute(file_id, path2, &
                            A_LENGTH_WIRE, randomgrid%lengthWire(i), &
                            .true.)
                buf = ""
                ok = read_string_attr(file_id, path2, A_SCALE_FILLER, buf)
                randomgrid%scaleFiller(i) = ""
                randomgrid%scaleFiller(i) = trim(buf)

                buf = ""
                ok = read_string_attr(file_id, path2, A_GRID_MATERIAL, buf)
                randomgrid%gridMaterial(i) = ""
                randomgrid%gridMaterial(i) = trim(buf)
                
                buf = ""
                ok = read_string_attr(file_id, path2, A_TYPE_FILLER, buf)
                randomgrid%typeFiller(i) = ""
                randomgrid%typeFiller(i) = trim(buf)

            enddo


        end subroutine random_read

        subroutine grid_read(file_id, path, grid)
            implicit none

            integer(hid_t), intent(in) :: file_id
            character(len=*), intent(in) :: path
            type(grid_t), intent(inout) :: grid
            character(len=EL) :: buf
            character(len=AL) :: path2
            logical :: ok

            buf = ""
            ok = read_string_attr(file_id, path, A_TEXTURE_TYPE, buf)
            grid%textureType = ""
            grid%textureType = trim(buf)
            if((grid%textureType == "woven") .or. &
               (grid%textureType == "unilateral")) then
                call woven_read(file_id, path, grid%wovengrid)
            else if(grid%textureType == "comb") then
                call comb_read(file_id, path, grid%combgrid)
            else if(grid%textureType == "random") then
                call random_read(file_id, path, grid%randomgrid)
            endif

        end subroutine grid_read

end module physicalmodel_m
