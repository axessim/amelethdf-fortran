module amelethdf
    use hdf5
    use amelethdf_m, only : check, hdferr, read_children_name, &
                            EL => ELEMENT_NAME_LENGTH, &
                            AL => ABSOLUTE_PATH_NAME_LENGTH, &
                            read_string_attribute => read_attribute, &
                            read_float_attribute, &
                            read_int_attribute
    use category_m, only : C_ELECTROMAGNETIC_SOURCE, C_GLOBAL_ENVIRONMENT, &
                           C_LABEL
    use mesh_m, only : C_MESH, mesh_names, isStructured
    use unstructuredmesh_m, only : umesh_read => read, &
                                   umesh_print => printt, &
                                   unstructured_mesh_t, &
                                   umesh_group_t => group_t, &
                                   umesh_generate_node_numbers => &
                                   generate_node_numbers, &
                                   umesh_number_of_nodes => number_of_nodes, &
                                   umesh_get_group_by_name => get_group_by_name, &
                                   umesh_get_index_by_short_name_in_some => &
                                   get_index_by_short_name_in_some, &
                                   umesh_generate_offsets => generate_offsets
    use structuredmesh_m, only : smesh_read => read, &
                                 smesh_print => printt, &
                                 structured_mesh_t
    use simulation_m, only : read_simulation => read, &
                             print_simulation => printt, &
                             C_SIMULATION, &
                             simulation_t
    use simpletype_m, only : singlereal_t, read_singlereal, &
                             singlereal_to_string, &
                             singlecomplex_t, read_singlecomplex, &
                             singlecomplex_to_string
    use vector_m, only : vector_t, read_vector => read, &
                         vector_to_string => to_string, &
                         vector_clear_content => clear_content
    use arrayset_m, only : arrayset_t, read_arrayset => read, &
                           arrayset_to_string => to_string2
    use physicalmodel_m, only : C_PHYSICAL_MODEL
    use complextype_m, only : create_attribute, read_cattribute, &
                            write_complex_type, read_attribute, &
                            write_complex_dataset => write_nd_dataset
    use hdfpath_m, only : basename, dirname, exists, isleaf, isgroup, like, join
    use planewave_m, only : planewave_t, read_planewave => read, &
                            islinear, iselliptic, C_PLANE_WAVE
    use floatingtype_m, only : floatingtype_t, read_floatingtype => read, &
                               issinglereal, isvector, convert_to_real_vector, &
                               set_floating_type
    use stringdataset_m, only : get_dataset_lmn, read_string_dataset1, &
                                read_string_vector => read_vector, &
                                write_string_dataset => write_nd_dataset
    use linkoutputrequest_m, only : link_t, read_link => read, isdataonmesh, &
                                    C_LINK, C_OUTPUT_REQUEST

end module amelethdf
