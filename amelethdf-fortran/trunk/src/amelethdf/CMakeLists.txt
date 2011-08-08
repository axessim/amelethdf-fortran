# Amelet-hdf CMakeList.txt file
	
FILE(GLOB amelethdf_src *.f90)

ADD_LIBRARY(amelethdf_so SHARED ${amelethdf_src})
SET_TARGET_PROPERTIES( amelethdf_so  PROPERTIES OUTPUT_NAME "amelethdff")
TARGET_LINK_LIBRARIES(amelethdf_so ${amelethdf_lib_dep})
INSTALL(TARGETS amelethdf_so
  EXPORT        AmeletHDFFLibraryDepends
  RUNTIME       DESTINATION "${INSTALL_BIN_DIR}" COMPONENT bin
  LIBRARY       DESTINATION "${INSTALL_LIB_DIR}" COMPONENT lib
  ARCHIVE       DESTINATION "${INSTALL_LIB_DIR}" COMPONENT lib
  PUBLIC_HEADER DESTINATION "${INSTALL_INCLUDE_DIR}"
  COMPONENT dev
  )

ADD_LIBRARY(amelethdf STATIC ${amelethdf_src})
SET_TARGET_PROPERTIES( amelethdf  PROPERTIES OUTPUT_NAME "amelethdff")
INSTALL(TARGETS amelethdf
  EXPORT        AmeletHDFFLibraryDepends
  RUNTIME       DESTINATION "${INSTALL_BIN_DIR}" COMPONENT bin
  LIBRARY       DESTINATION "${INSTALL_LIB_DIR}" COMPONENT lib
  ARCHIVE       DESTINATION "${INSTALL_LIB_DIR}" COMPONENT lib
  COMPONENT dev
  )

FILE(GLOB amelethdf_module ${CMAKE_CURRENT_BINARY_DIR}/*.mod)
INSTALL(FILES ${amelethdf_module} DESTINATION "${INSTALL_INCLUDE_DIR}")

