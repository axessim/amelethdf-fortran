module category_m
    use h5lt
    use amelethdf_m, only : check, hdferr
    use mesh_m, only : C_MESH
    use physicalmodel_m, only : C_PHYSICAL_MODEL

    implicit none

    character(len=*), parameter :: C_LABEL = "/label/"
    character(len=*), parameter :: C_ELECTROMAGNETIC_SOURCE = &
                                   "/electromagneticSource/"
    character(len=*), parameter :: C_GLOBAL_ENVIRONMENT = &
                                   "/globalEnvironment/"
end module category_m
