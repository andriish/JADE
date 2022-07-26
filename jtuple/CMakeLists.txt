########################################################################
CMAKE_MINIMUM_REQUIRED(VERSION 3.5.0) #Because of ROOT
PROJECT(JTUPLE Fortran CXX C)
SET(VERSION 2020.11)
include("GNUInstallDirs")
SET(CMAKE_MODULE_PATH "${CMAKE_SOURCE_DIR}/../cmake/Modules/")
set(CMAKE_VERBOSE_MAKEFILE OFF)
option(JADE_USE_CERNLIB "Attempt to use CERNLIB instead of buildin code (picocernlib)"     OFF)
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
set(JEXTERN_LIBRARIES  ${PICOCERNLIB})
set(CMAKE_CXX_FLAGS     "${CMAKE_CXX_FLAGS}     -DJEXTERNISPICO")
set(CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} -DJEXTERNISPICO")
########################################################################
set(JEXTERN_GRAPH_LIBRARIES X11)
message(STATUS "JEXTERN libraries       : ${JEXTERN_LIBRARIES}")
message(STATUS "JEXTERN_GRAPH libraries : ${JEXTERN_GRAPH_LIBRARIES}")
########################################################################

SET(JTJOBEXE jtjob)
SET(JZREADEXE jzread)
SET(TAKLUMIEXE taklumi)

# Define the library name
SET(JTLIB jtlib)
SET(NTUPLE          ntuple)
SET(ZREAD         zread)

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
message(STATUS "JADE_LIBRARIES : ${JADE_LIBRARIES}")

# Define some directories
SET(SRC ${CMAKE_SOURCE_DIR}/src)
SET(LIB ${CMAKE_SOURCE_DIR}/lib)
SET(BIN ${CMAKE_SOURCE_DIR}/bin)
include_directories( ${SRC} )

SET(JTLIB_src 
${SRC}/jadev.F ${SRC}/jadepr.F ${SRC}/endian.f ${SRC}/jzsetr.F ${SRC}/jzinit.F ${SRC}/jzevnt.F ${SRC}/jzfnsh.F
)
ADD_LIBRARY(${JTLIB} ${JTLIB_src})

SET(NTUPLE_src 
${SRC}/jtbknt.F ${SRC}/jtevsl.F ${SRC}/casso.f ${SRC}/cinfo.f ${SRC}/cnass.f ${SRC}/cnone.f ${SRC}/cyjet.f
                ${SRC}/qqcone.f ${SRC}/qqjets.f ${SRC}/caxes.f ${SRC}/ckern.f 
                ${SRC}/cncnt.f ${SRC}/cwrit.f ${SRC}/qqangl.f 
                ${SRC}/qqevsh.f ${SRC}/cdble.f ${SRC}/cksord.f ${SRC}/cnjet.f ${SRC}/cyget.f 
                ${SRC}/qqckbk.f ${SRC}/qqform.f ${SRC}/px116.f
)
ADD_LIBRARY(${NTUPLE} ${NTUPLE_src})

SET(ZREAD_src  ${SRC}/jtevnt.F ${SRC}/jtfnsh.F ${SRC}/jtinit.F ${SRC}/jtsetr.F)
ADD_LIBRARY(${ZREAD} ${ZREAD_src})

SET(TAKLUMIEXE_src ${SRC}/taklumi.f)
ADD_EXECUTABLE(${TAKLUMIEXE} ${TAKLUMIEXE_src})

SET(JZREADEXE_src ${SRC}/jzread.F)
ADD_EXECUTABLE(${JZREADEXE} ${JZREADEXE_src})

SET(JTJOBEXE_src ${SRC}/jtjob.F  ${SRC}/showb.f)
ADD_EXECUTABLE(${JTJOBEXE} ${JTJOBEXE_src})

if(${CMAKE_SYSTEM_NAME} MATCHES "Darwin")
  TARGET_LINK_LIBRARIES(${TAKLUMIEXE}   ${JEXTERN_LIBRARIES} )
  TARGET_LINK_LIBRARIES(${JZREADEXE}   ${JEXTERN_LIBRARIES} ${ZREAD} ${NTUPLE} ${JTLIB} ${JADE_LIBRARIES} ${LAPACK_LIBRARIES} )
  TARGET_LINK_LIBRARIES(${JTJOBEXE}  ${JEXTERN_LIBRARIES} ${ZREAD} ${NTUPLE} ${BOSLIB} ${JADE_LIBRARIES} ${LAPACK_LIBRARIES} )
else()
  TARGET_LINK_LIBRARIES(${TAKLUMIEXE} -Wl,--start-group ${JEXTERN_LIBRARIES} -Wl,--end-group)
  TARGET_LINK_LIBRARIES(${JZREADEXE} -Wl,--start-group ${JEXTERN_LIBRARIES} ${ZREAD} ${NTUPLE} ${JTLIB} ${JADE_LIBRARIES} ${LAPACK_LIBRARIES} -Wl,--end-group)
  TARGET_LINK_LIBRARIES(${JTJOBEXE} -Wl,--start-group ${JEXTERN_LIBRARIES} ${ZREAD} ${NTUPLE} ${JADE_LIBRARIES} ${LAPACK_LIBRARIES} -Wl,--end-group)
endif()

########################################################################
INSTALL(TARGETS ${JTJOBEXE} ${JZREADEXE} ${TAKLUMIEXE} RUNTIME DESTINATION ${CMAKE_INSTALL_BINDIR})

INSTALL( TARGETS ${BOSLIB} 
${ZREAD}
${NTUPLE}
ARCHIVE DESTINATION ${CMAKE_INSTALL_LIBDIR} LIBRARY DESTINATION ${CMAKE_INSTALL_LIBDIR} COMPONENT library )

SET(CMAKE_Fortran_MODULE_DIRECTORY ${LIB})
########################################################################
