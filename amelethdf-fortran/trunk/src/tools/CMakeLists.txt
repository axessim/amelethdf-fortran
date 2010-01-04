# tools CMakeList.txt file

ADD_EXECUTABLE(ameletreader ameletreader.f90)
TARGET_LINK_LIBRARIES(ameletreader ${tools_lib_dep})

INSTALL(TARGETS ameletreader DESTINATION bin)
