# tools CMakeList.txt file

#ADD_EXECUTABLE(ameletreader ameletreader.f90)
#TARGET_LINK_LIBRARIES(ameletreader ${tools_lib_dep})

#INSTALL(TARGETS ameletreader DESTINATION bin)


file(GLOB exe_src *.f90)
FOREACH(exe_file ${exe_src})
    file(RELATIVE_PATH rel_exe_file ${CMAKE_CURRENT_SOURCE_DIR} ${exe_file})
    STRING(REPLACE ".f90"  "" exe_exec ${rel_exe_file})
    ADD_EXECUTABLE(${exe_exec} ${exe_file})
    TARGET_LINK_LIBRARIES(${exe_exec}  ${tools_lib_dep})
    INSTALL(TARGETS ${exe_exec} DESTINATION bin)
    MESSAGE("executable : " ${exe_exec}  ${tools_lib_dep})
ENDFOREACH(exe_file)

