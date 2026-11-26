set(VERSION 2024.04)
include("GNUInstallDirs")
list(APPEND CMAKE_MODULE_PATH "${CMAKE_CURRENT_SOURCE_DIR}/../cmake/Modules")
set(CMAKE_VERBOSE_MAKEFILE OFF)
option(JADE_FORCE_32                   "Force build of 32 bit libraries." OFF)
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
find_package(CERNLIB  REQUIRED)
message(STATUS "JADE FPTOBOS: CERNLIB_KERNLIB_STATIC=${CERNLIB_KERNLIB_STATIC}")
message(STATUS "JADE FPTOBOS: CERNLIB_PACKLIB_STATIC=${CERNLIB_PACKLIB_STATIC}")
message(STATUS "JADE FPTOBOS: CERNLIB_MATHLIB_STATIC=${CERNLIB_MATHLIB_STATIC}")
set(JEXTERN_LIBRARIES  "${CERNLIB_PACKLIB_STATIC};${CERNLIB_KERNLIB_STATIC};${CERNLIB_MATHLIB_STATIC}" )
message(STATUS "JEXTERN libraries       : ${JEXTERN_LIBRARIES}")
########################################################################
if(TARGET interface AND TARGET jadegs AND TARGET boslib AND TARGET zlib)
  # built together with jadesoft (top-level CMakeLists.txt): the libraries of this build
  set(JADESOFT_LIBRARIES_STATIC interface jadegs boslib zlib)
else()
  find_package(JADESOFT REQUIRED)
endif()
message(STATUS "JADE FPTOBOS: JADESOFT_LIBRARIES_STATIC=${JADESOFT_LIBRARIES_STATIC}")
########################################################################
set(FPTOBOSEXE fptobos)
set(UNPJADEXE unpjad)
########################################################################
# Define some directories
include_directories( ${CMAKE_CURRENT_SOURCE_DIR}/src )

set(FPTOBOS_src 
 fptobos.f unpjad.f  unpaff.f  bytswp.f  chasci.f  pripatr.f prihead.f 
 prizvtx.f prizehd.f prize4v.f prilgcl.f prigvtx.f primur1.f primur2.f 
 pritpev.f pritptr.f pritpvx.f pritagg.f twoin1.f cnvibm3e.f chebcd.f )
list(TRANSFORM FPTOBOS_src PREPEND "${CMAKE_CURRENT_SOURCE_DIR}/src/")

set(UNPJAD_src 
unpjad1.f unpaff.f bytswp.f chasci.f pripatr.f prihead.f prizvtx.f prize4v.f 
twoin1.f cnvibm3e.f 
# prizehd.f prilgcl.f prigvtx.f primur1.f primur2.f pritpev.f pritptr.f 
# pritpvx.f pritagg.f chebcd.f 
 mvcl.f   #ISNT IT IN BLAS?
)
list(TRANSFORM UNPJAD_src PREPEND "${CMAKE_CURRENT_SOURCE_DIR}/src/")

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
INSTALL(TARGETS ${FPTOBOSEXE} EXPORT JADETargets  RUNTIME DESTINATION ${CMAKE_INSTALL_BINDIR})
INSTALL(TARGETS ${UNPJADEXE} EXPORT JADETargets  RUNTIME DESTINATION ${CMAKE_INSTALL_BINDIR})
########################################################################
