# Top level CMakeList.txt file

cmake_minimum_required (VERSION 2.6 FATAL_ERROR)
enable_language(Fortran)

PROJECT(AmeletHDFF)

MESSAGE( "--------------------------------------------------" )
MESSAGE( STATUS "Configuring project Amelet-HDF for Fortran" )
MESSAGE( STATUS "Host system is ${CMAKE_SYSTEM_NAME}"  )
MESSAGE( STATUS "Configuration is ${CMAKE_BUILD_TYPE}"  )


#-------------------------------------------------------------
# Configu install-tree
#-------------------------------------------------------------
# Offer the user the choice of overriding the installation directories
set(INSTALL_LIB_DIR lib CACHE PATH "Installation directory for libraries")
IF (WIN32)
	set(INSTALL_ARCHIVE_DIR lib/static CACHE PATH "Installation directory for libraries")
ELSE ()
    set(INSTALL_ARCHIVE_DIR lib CACHE PATH "Installation directory for libraries")
ENDIF ()
set(INSTALL_BIN_DIR bin CACHE PATH "Installation directory for executables")
set(INSTALL_INCLUDE_DIR include CACHE PATH
  "Installation directory for header files")
set(INSTALL_DATA_DIR share CACHE PATH
  "Installation directory for data files")
 
# Make relative paths absolute (needed later on)
foreach(p LIB ARCHIVE BIN INCLUDE DATA)
  set(var INSTALL_${p}_DIR)
  if(NOT IS_ABSOLUTE "${${var}}")
    set(${var} "${CMAKE_INSTALL_PREFIX}/${${var}}")
  endif()
endforeach()


#-------------------------------------------------------------
# Found all dependaces.
#-------------------------------------------------------------
# Only HDF5 for Frotran and dep are nedeed!
FIND_PACKAGE(HDF5 REQUIRED NO_DEFAULT_PATH)
IF (NOT HDF5_FOUND)
  MESSAGE( FATAL_ERROR "HDF5 not found." )
ELSE ()
  IF (NOT HDF5_BUILD_FORTRAN)
    MESSAGE( FATAL_ERROR "HDF5 not build for Fortran." )
  ENDIF ()
ENDIF ()

INCLUDE_DIRECTORIES(${HDF5_INCLUDE_DIR_FORTRAN}
                    ${CMAKE_CURRENT_BINARY_DIR}/src/amelethdf)
SET(amelethdf_lib_dep 
  hdf5_hl_fortran hdf5_hl_f90cstub 
  hdf5_hl hdf5_fortran hdf5_f90cstub 
  hdf5)
SET(tools_lib_dep amelethdf_so ${amelethdf_lib_dep})


#-------------------------------------------------------------
# Walk sub dir
#-------------------------------------------------------------
ADD_SUBDIRECTORY(src)

#-------------------------------------------------------------
# create a AmeletHDFFConfig.cmake file and co.
#-------------------------------------------------------------

## CMAKE CONFIGURE FILE into build-tree
set(AMELET_HDF_F_INCLUDE_DIRS 
  "${PROJECT_SOURCE_DIR}/src" 
  "${PROJECT_BINARY_DIR}/src/amelethdf")
set(AMELET_HDF_F_CMAKE_DIR 
  "${PROJECT_BINARY_DIR}")
# Add all targets to the build-tree export set
export(TARGETS amelethdf amelethdf_so
  FILE "${AMELET_HDF_F_CMAKE_DIR}/AmeletHDFFLibraryDepends.cmake")
# Export the package for use from the build-tree
# (this registers the build-tree with a global CMake-registry)
export(PACKAGE AmeletHDFF)
# Create a AxesSignConfig.cmake file for the use from the build tree
configure_file(resources/AmeletHDFFConfig.cmake.in
  "${AMELET_HDF_F_CMAKE_DIR}/AmeletHDFFConfig.cmake" @ONLY)
configure_file(resources/AmeletHDFFConfigVersion.cmake.in
  "${AMELET_HDF_F_CMAKE_DIR}/AmeletHDFFConfigVersion.cmake" @ONLY)



## CMAKE CONFIGURE FILE into install-tree
set(AMELET_HDF_F_INCLUDE_DIRS "${INSTALL_INCLUDE_DIR}")
#set(AMELET_HDF_F_LIB_DIR "${INSTALL_LIB_DIR}")
set(AMELET_HDF_F_CMAKE_DIR "${INSTALL_LIB_DIR}/amelethdff/cmake")
# Install the export set for use with the install-tree
install(EXPORT AmeletHDFFLibraryDepends
  DESTINATION "${AMELET_HDF_F_CMAKE_DIR}"
  COMPONENT dev)
# Create a AxesSignConfig.cmake file for the use from the install tree
# and install it
configure_file(resources/AmeletHDFFConfig.cmake.in
  "${PROJECT_BINARY_DIR}/InstallFiles/AmeletHDFFConfig.cmake" @ONLY)
configure_file(resources/AmeletHDFFConfigVersion.cmake.in
  "${PROJECT_BINARY_DIR}/InstallFiles/AmeletHDFFConfigVersion.cmake" @ONLY)
install(FILES
  "${PROJECT_BINARY_DIR}/InstallFiles/AmeletHDFFConfig.cmake"
  "${PROJECT_BINARY_DIR}/InstallFiles/AmeletHDFFConfigVersion.cmake"
  DESTINATION "${AMELET_HDF_F_CMAKE_DIR}" 
  COMPONENT dev)

