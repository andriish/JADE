CMAKE_MINIMUM_REQUIRED(VERSION 3.5.0) #Because of ROOT
PROJECT(JTUPLE Fortran C)
set(VERSION 2020.11)
include("GNUInstallDirs")
set(CMAKE_MODULE_PATH "${CMAKE_SOURCE_DIR}/../cmake/Modules/")
set(CMAKE_VERBOSE_MAKEFILE OFF)
option(JADE_FORCE_32                   "Force build of 32 bit libraries." OFF)
option(JADE_USE_CERNLIB                "Attempt to use CERNLIB instead of buildin code (picocernlib)"     ON)
if (JADE_FORCE_32)
  list(PREPEND CMAKE_SYSTEM_LIBRARY_PATH "/usr/lib")
  message(STATUS "${CMAKE_SYSTEM_LIBRARY_PATH}")
  set( FIND_LIBRARY_USE_LIB32_PATHS  ON)
  set( FIND_LIBRARY_USE_LIB64_PATHS  OFF)
  set(CMAKE_SIZEOF_VOID_P 4)
  set(LIB_SUFFIX )
endif()
########################################################################
include(JADE_compiler_setup)
set(CMAKE_POSITION_INDEPENDENT_CODE FALSE)
########################################################################
find_package(BLAS REQUIRED)
message(STATUS "JADE FPTOBOS: BLAS_LIBRARIES=${BLAS_LIBRARIES}")
find_package(LAPACK REQUIRED)
message(STATUS "JADE FPTOBOS: LAPACK_LIBRARIES=${LAPACK_LIBRARIES}")
########################################################################
if (JADE_USE_CERNLIB)
  find_package(CERNLIB  REQUIRED)
  message(STATUS "JADE FPTOBOS: CERNLIB_KERNLIB_STATIC=${CERNLIB_KERNLIB_STATIC}")
  message(STATUS "JADE FPTOBOS: CERNLIB_PACKLIB_STATIC=${CERNLIB_PACKLIB_STATIC}")
  message(STATUS "JADE FPTOBOS: CERNLIB_MATHLIB_STATIC=${CERNLIB_MATHLIB_STATIC}")
  set(PICOCERNLIB "${CERNLIB_PACKLIB_STATIC};${CERNLIB_KERNLIB_STATIC};${CERNLIB_MATHLIB_STATIC}")
endif()
set(JEXTERN_LIBRARIES  ${PICOCERNLIB} )
message(STATUS "JEXTERN libraries       : ${JEXTERN_LIBRARIES}")
########################################################################
find_package(JADESOFT REQUIRED)
message(STATUS "JADE FPTOBOS: JADESOFT_LIBRARIES_STATIC=${JADESOFT_LIBRARIES_STATIC}")
########################################################################
set(FPTOBOSEXE fptobos)
set(UNPJADEXE unpjad)
########################################################################
# Define some directories
include_directories( ${CMAKE_SOURCE_DIR}/src )

set(FPTOBOS_src 
 fptobos.f unpjad.f  unpaff.f  bytswp.f  chasci.f  pripatr.f prihead.f 
 prizvtx.f prizehd.f prize4v.f prilgcl.f prigvtx.f primur1.f primur2.f 
 pritpev.f pritptr.f pritpvx.f pritagg.f twoin1.f cnvibm3e.f chebcd.f )
list(TRANSFORM FPTOBOS_src PREPEND "${CMAKE_SOURCE_DIR}/src/")

set(UNPJAD_src 
unpjad1.f unpaff.f bytswp.f chasci.f pripatr.f prihead.f prizvtx.f prize4v.f 
twoin1.f cnvibm3e.f 
# prizehd.f prilgcl.f prigvtx.f primur1.f primur2.f pritpev.f pritptr.f 
# pritpvx.f pritagg.f chebcd.f 
 mvcl.f   #ISNT IT IN BLAS?
)
list(TRANSFORM UNPJAD_src PREPEND "${CMAKE_SOURCE_DIR}/src/")

add_executable(${FPTOBOSEXE} ${FPTOBOS_src})
add_executable(${UNPJADEXE} ${UNPJAD_src})

if(${CMAKE_SYSTEM_NAME} MATCHES "Darwin")
  target_link_libraries(${FPTOBOSEXE}  ${JEXTERN_LIBRARIES}  ${JADESOFT_LIBRARIES_STATIC} ${LAPACK_LIBRARIES} )
  target_link_libraries(${UNPJADEXE}   ${JEXTERN_LIBRARIES}  ${JADESOFT_LIBRARIES_STATIC} ${LAPACK_LIBRARIES} )
else()
  target_link_libraries(${FPTOBOSEXE} -Wl,--start-group ${JEXTERN_LIBRARIES}  ${JADESOFT_LIBRARIES_STATIC} ${LAPACK_LIBRARIES} -Wl,--end-group)
  target_link_libraries(${UNPJADEXE}  -Wl,--start-group ${JEXTERN_LIBRARIES}  ${JADESOFT_LIBRARIES_STATIC} ${LAPACK_LIBRARIES} -Wl,--end-group)
endif()

########################################################################
INSTALL(TARGETS ${FPTOBOSEXE}  RUNTIME DESTINATION ${CMAKE_INSTALL_BINDIR})
INSTALL(TARGETS ${UNPJADEXE}  RUNTIME DESTINATION ${CMAKE_INSTALL_BINDIR})
########################################################################
