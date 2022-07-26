CMAKE_MINIMUM_REQUIRED(VERSION 3.5.0) #Because of ROOT
PROJECT(JTUPLE Fortran CXX C)
SET(VERSION 2020.11)
include("GNUInstallDirs")
SET(CMAKE_MODULE_PATH "${CMAKE_SOURCE_DIR}/../cmake/Modules/")
set(CMAKE_VERBOSE_MAKEFILE OFF)
option(JADE_FORCE_32                   "Force build of 32 bit libraries." OFF)
if (JADE_FORCE_32)
  list(PREPEND CMAKE_SYSTEM_LIBRARY_PATH "/usr/lib")
  message(STATUS "${CMAKE_SYSTEM_LIBRARY_PATH}")
  set( FIND_LIBRARY_USE_LIB32_PATHS  ON)
  set( FIND_LIBRARY_USE_LIB64_PATHS  OFF)
  set(CMAKE_SIZEOF_VOID_P 4)
  #set (CMAKE_EXE_LINKER_FLAGS  "-m32 ${CMAKE_EXE_LINKER_FLAGS}")
endif()
########################################################################
include(JADE_compiler_setup)
########################################################################
  find_package(BLAS REQUIRED)
  find_package(LAPACK REQUIRED)
########################################################################
if (JADE_USE_CERNLIB)
  find_package(CERNLIB  REQUIRED HINTS /Volumes/Work/INSTALL/share/cernlib/cmake)
  message(STATUS "JADE: CERNLIB_PACKLIB=${CERNLIB_PACKLIB}")
  message(STATUS "JADE: CERNLIB_PACKLIB_STATIC=${CERNLIB_PACKLIB_STATIC}")
  set(PICOCERNLIB "${MOTIF_LIBRARIES};${CERNLIB_PACKLIB_STATIC};${CERNLIB_KERNLIB_STATIC};${CERNLIB_MATHLIB_STATIC};${CERNLIB_GRAFLIB_STATIC};${CERNLIB_GRAFX11_STATIC}")
  if(${CMAKE_SYSTEM_NAME} MATCHES "Darwin")
    set(Motif_DIR /usr/local/Cellar/openmotif/2.3.8_1)
  endif()
  find_package(Motif REQUIRED)
  message(STATUS "JADE: MOTIF_LIBRARIES=${MOTIF_LIBRARIES}")
else()
  include(JADE_ROOT_setup)
  find_library(LIBPICOKERNLIB REQUIRED NAMES libpicokernlib.a  PATHS ${PICOCERNLIBPREFIX}/lib ${PICOCERNLIBPREFIX}/lib64 NO_DEFAULT_PATH)
  find_library(LIBPICOGRAFLIB REQUIRED NAMES libpicograflib.a  PATHS ${PICOCERNLIBPREFIX}/lib ${PICOCERNLIBPREFIX}/lib64 NO_DEFAULT_PATH)
  set(PICOCERNLIB ${LIBPICOKERNLIB} ${LIBPICOGRAFLIB} ${ROOT_LIBRARIES})
endif()
set(JEXTERN_LIBRARIES  ${PICOCERNLIB} )
set(CMAKE_CXX_FLAGS     "${CMAKE_CXX_FLAGS}     -DJEXTERNISPICO")
set(CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} -DJEXTERNISPICO")
########################################################################
  if(${CMAKE_SYSTEM_NAME} MATCHES "Darwin")
     set(JEXTERN_GRAPH_LIBRARIES -lc++)
  else()
    find_package(X11  REQUIRED)
    set(JEXTERN_GRAPH_LIBRARIES ${X11_LIBRARIES})
  endif()
message(STATUS "JEXTERN libraries       : ${JEXTERN_LIBRARIES}")
message(STATUS "JEXTERN_GRAPH libraries : ${JEXTERN_GRAPH_LIBRARIES}")
########################################################################
SET(FPTOBOSEXE fptobos)
SET(UNPJADEXE unpjad)

# Define the library name
FIND_PATH(JADELIB_LIBRARY_DIR NAMES libinterface.a libjadegs.a libboslib.a libzlib.a PATHS
  ${JADELIB_ROOT_DIR}/lib
  ${JADELIB_ROOT_DIR}/lib64
  ../jadesoft/lib
  ../jadesoft/
    ${CMAKE_CURRENT_BINARY_DIR}/../jadesoft/
  NO_DEFAULT_PATH
) 
set(jadelibs interface jadegs boslib zlib)  
set(JADE_LIBRARIES)
foreach(_cpt ${jadelibs})
  find_library(JADE_${_cpt}_LIBRARY lib${_cpt}.a ${_cpt} HINTS ${JADELIB_LIBRARY_DIR})
  if(JADE_${_cpt}_LIBRARY)
    mark_as_advanced(JADE_${_cpt}_LIBRARY)
    list(APPEND JADE_LIBRARIES ${JADE_${_cpt}_LIBRARY})
  endif()
endforeach()
  

# Define some directories
SET(SRC ${CMAKE_SOURCE_DIR}/src)
SET(LIB ${CMAKE_SOURCE_DIR}/lib)
SET(BIN ${CMAKE_SOURCE_DIR}/bin)
include_directories( ${SRC} )

SET(FPTOBOS_src ${SRC}/fptobos.f 
${SRC}/unpjad.f 
${SRC}/unpaff.f 
${SRC}/bytswp.f 
${SRC}/chasci.f 
${SRC}/pripatr.f 
${SRC}/prihead.f 
${SRC}/prizvtx.f 
${SRC}/prizehd.f 
${SRC}/prize4v.f 
${SRC}/prilgcl.f 
${SRC}/prigvtx.f
 ${SRC}/primur1.f 
 ${SRC}/primur2.f 
 ${SRC}/pritpev.f 
 ${SRC}/pritptr.f 
 ${SRC}/pritpvx.f 
 ${SRC}/pritagg.f 
 ${SRC}/twoin1.f  
 ${SRC}/cnvibm3e.f 
 ${SRC}/chebcd.f )
 
 
 SET(UNPJAD_src
${SRC}/unpjad1.f 
${SRC}/unpaff.f 
${SRC}/bytswp.f 
${SRC}/chasci.f 
${SRC}/pripatr.f 
${SRC}/prihead.f 
${SRC}/prizvtx.f 
${SRC}/prize4v.f 
${SRC}/twoin1.f  
${SRC}/cnvibm3e.f 
#${SRC}/prizehd.f 
#${SRC}/prilgcl.f 
#${SRC}/prigvtx.f 
#${SRC}/primur1.f 
#${SRC}/primur2.f 
#${SRC}/pritpev.f 
#${SRC}/pritptr.f 
#${SRC}/pritpvx.f 
#${SRC}/pritagg.f 
#${SRC}/chebcd.f 

${SRC}/mvcl.f   #ISNT IT IN BLAS?
)

ADD_EXECUTABLE(${FPTOBOSEXE} ${FPTOBOS_src})
ADD_EXECUTABLE(${UNPJADEXE} ${UNPJAD_src})

if(${CMAKE_SYSTEM_NAME} MATCHES "Darwin")
  TARGET_LINK_LIBRARIES(${FPTOBOSEXE}  ${JEXTERN_LIBRARIES}  ${JADE_LIBRARIES} ${LAPACK_LIBRARIES} )
  TARGET_LINK_LIBRARIES(${UNPJADEXE}  ${JEXTERN_LIBRARIES}  ${JADE_LIBRARIES} ${LAPACK_LIBRARIES} )
else()
  TARGET_LINK_LIBRARIES(${FPTOBOSEXE} -Wl,--start-group ${JEXTERN_LIBRARIES}  ${JADE_LIBRARIES} ${LAPACK_LIBRARIES} -Wl,--end-group)
  TARGET_LINK_LIBRARIES(${UNPJADEXE} -Wl,--start-group ${JEXTERN_LIBRARIES}  ${JADE_LIBRARIES} ${LAPACK_LIBRARIES} -Wl,--end-group)
endif()

########################################################################
INSTALL(TARGETS ${FPTOBOSEXE}  RUNTIME DESTINATION ${CMAKE_INSTALL_BINDIR})
INSTALL(TARGETS ${UNPJADEXE}  RUNTIME DESTINATION ${CMAKE_INSTALL_BINDIR})
SET(CMAKE_Fortran_MODULE_DIRECTORY ${LIB})
########################################################################
