# tools CMakeList.txt file

#ADD_EXECUTABLE(ameletreader ameletreader.f90)
#TARGET_LINK_LIBRARIES(ameletreader ${tools_lib_dep})

#INSTALL(TARGETS ameletreader DESTINATION bin)


file(GLOB test_src *.f90)
FOREACH(test_file ${test_src})
    file(RELATIVE_PATH rel_test_file ${CMAKE_CURRENT_SOURCE_DIR} ${test_file})
    STRING(REPLACE ".f90"  "" test_exec ${rel_test_file})
    ADD_EXECUTABLE(${test_exec} ${test_file})
    TARGET_LINK_LIBRARIES(${test_exec} ${amelethdf_so} ${tools_lib_dep})
ENDFOREACH(test_file)

INSTALL(TARGETS ${test_exec} DESTINATION bin)
