# Top level CMakeList.txt file

cmake_minimum_required (VERSION 2.6 FATAL_ERROR)
enable_language(Fortran)

PROJECT(amelet)

MESSAGE( "--------------------------------------------------" )
MESSAGE( STATUS "Configuring project Amelet-HDF"         )
MESSAGE( STATUS "Host system is ${CMAKE_SYSTEM_NAME}"   )
MESSAGE( STATUS "Configuration is ${CMAKE_BUILD_TYPE}"  )


# Only HDF5 for Frotran and dep are nedeed!
FIND_PACKAGE(HDF5 REQUIRED NO_DEFAULT_PATH)
IF (NOT HDF5_FOUND)
  MESSAGE( FATAL_ERROR "HDF5 not found." )
ELSE ()
  IF (NOT HDF5_BUILD_FORTRAN)
    MESSAGE( FATAL_ERROR "HDF5 not build for Fortran." )
  ENDIF ()
ENDIF ()

INCLUDE_DIRECTORIES(${HDF5_INCLUDE_DIR}/fortran
                    ${CMAKE_CURRENT_BINARY_DIR}/src/amelethdf)

SET(tools_lib_dep amelethdf hdf5_hl_fortran hdf5_fortran hdf5_hl_f90cstub)

ADD_SUBDIRECTORY(src)

