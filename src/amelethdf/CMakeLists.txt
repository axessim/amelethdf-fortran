# Amelet-hdf CMakeList.txt file
	
FILE(GLOB amelethdf_src *.f90)

ADD_LIBRARY(amelethdf_so SHARED ${amelethdf_src})
SET_TARGET_PROPERTIES( amelethdf_so  PROPERTIES OUTPUT_NAME "amelethdff")
INSTALL(TARGETS amelethdf_so LIBRARY DESTINATION "lib")

ADD_LIBRARY(amelethdf STATIC ${amelethdf_src})
SET_TARGET_PROPERTIES( amelethdf  PROPERTIES OUTPUT_NAME "amelethdff")
INSTALL(TARGETS amelethdf ARCHIVE DESTINATION "lib")

FILE(GLOB amelethdf_module ${CMAKE_CURRENT_BINARY_DIR}/*.mod)
INSTALL(FILES ${amelethdf_module} DESTINATION "include")

