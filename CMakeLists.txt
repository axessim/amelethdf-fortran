# Top level CMakeList.txt file

cmake_minimum_required (VERSION 2.6 FATAL_ERROR)
enable_language(Fortran)

PROJECT(amelet)

MESSAGE( "--------------------------------------------------" )
MESSAGE( STATUS "Configuring project Amelet-HDF"         )
MESSAGE( STATUS "Host system is ${CMAKE_SYSTEM_NAME}"   )
MESSAGE( STATUS "Configuration is ${CMAKE_BUILD_TYPE}"  )

SET(LIBHDF5_DIR /usr CACHE PATH "Path to HDF5 library")

INCLUDE_DIRECTORIES(${LIBHDF5_DIR}/include ${LIBHDF5_DIR}/modsdll
                    ${LIBHDF5_DIR}/include/fortran
                    ${CMAKE_CURRENT_BINARY_DIR}/src/amelethdf)

FIND_PACKAGE(ZLIB REQUIRED)
get_filename_component (Fortran_COMPILER_NAME ${CMAKE_Fortran_COMPILER} NAME)
MESSAGE(FORTRAN COMPILER = "${Fortran_COMPILER_NAME}")
IF (${CMAKE_HOST_WIN32})
    SET(amelethdf_lib_dep ${LIBHDF5_DIR}/lib/hdf5_hl_fortrandll.lib
	                      ${LIBHDF5_DIR}/lib/hdf5_hl_f90cstubdll.lib
                          ${LIBHDF5_DIR}/lib/hdf5_hldll.lib
                          ${LIBHDF5_DIR}/lib/hdf5_fortrandll.lib
                          ${LIBHDF5_DIR}/lib/hdf5_f90cstubdll.lib
                          ${LIBHDF5_DIR}/lib/hdf5dll.lib
                          ${LIBHDF5_DIR}/lib/szip.lib
                          ${ZLIB_LIBRARY})
	SET(amelethdf_lib_dep_so ${amelethdf_lib_dep})
ELSE (${CMAKE_HOST_UNIX})
  IF (Fortran_COMPILER_NAME STREQUAL "gfortran")
    MESSAGE(GFORTRAN)
    SET(amelethdf_lib_dep ${LIBHDF5_DIR}/lib/libhdf5hl_fortran.a
                          ${LIBHDF5_DIR}/lib/libhdf5_hl.a
                          ${LIBHDF5_DIR}/lib/libhdf5_fortran.a
                          ${LIBHDF5_DIR}/lib/libhdf5.a
                          ${ZLIB_LIBRARY})
    SET(amelethdf_lib_dep_so ${LIBHDF5_DIR}/lib/libhdf5hl_fortran.so
                          ${LIBHDF5_DIR}/lib/libhdf5_hl.so
                          ${LIBHDF5_DIR}/lib/libhdf5_fortran.so
                          ${LIBHDF5_DIR}/lib/libhdf5.so
                          ${ZLIB_LIBRARY})
  ELSE (Fortran_COMPILER_NAME STREQUAL "gfortran")
    MESSAGE(IFORT)
    SET(amelethdf_lib_dep ${LIBHDF5_DIR}/lib/libhdf5hl_fortran.a
                          ${LIBHDF5_DIR}/lib/libhdf5_hl_f90cstub.a
                          ${LIBHDF5_DIR}/lib/libhdf5_hl.a
                          ${LIBHDF5_DIR}/lib/libhdf5_fortran.a
                          ${LIBHDF5_DIR}/lib/libhdf5_f90cstub.a
                          ${LIBHDF5_DIR}/lib/libhdf5.a
                          ${ZLIB_LIBRARY})
    SET(amelethdf_lib_dep_so ${LIBHDF5_DIR}/lib/libhdf5hl_fortran.so
                          ${LIBHDF5_DIR}/lib/libhdf5_hl_f90cstub.so
                          ${LIBHDF5_DIR}/lib/libhdf5_hl.so
                          ${LIBHDF5_DIR}/lib/libhdf5_fortran.so
                          ${LIBHDF5_DIR}/lib/libhdf5_f90cstub.so
                          ${LIBHDF5_DIR}/lib/libhdf5.so
                          ${ZLIB_LIBRARY})
  ENDIF (Fortran_COMPILER_NAME STREQUAL "gfortran")
ENDIF (${CMAKE_HOST_WIN32})

SET(tools_lib_dep amelethdf ${amelethdf_lib_dep})

ADD_SUBDIRECTORY(src)

