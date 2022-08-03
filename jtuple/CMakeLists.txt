########################################################################
CMAKE_MINIMUM_REQUIRED(VERSION 3.5.0) #Because of ROOT
PROJECT(JTUPLE Fortran CXX C)
set(VERSION 2022.08)
include("GNUInstallDirs")
set(CMAKE_MODULE_PATH "${CMAKE_SOURCE_DIR}/../cmake/Modules/")
set(CMAKE_VERBOSE_MAKEFILE OFF)
option(JADE_USE_CERNLIB "Attempt to use CERNLIB instead of buildin code (picocernlib)"     ON)
option(JADE_FORCE_32                   "Force build of 32 bit libraries." OFF)
if (JADE_FORCE_32)
  list(PREPEND CMAKE_SYSTEM_LIBRARY_PATH "/usr/lib")
  message(STATUS "${CMAKE_SYSTEM_LIBRARY_PATH}")
  set( FIND_LIBRARY_USE_LIB32_PATHS  ON)
  set( FIND_LIBRARY_USE_LIB64_PATHS  OFF)
  set(CMAKE_SIZEOF_VOID_P 4)
endif()
########################################################################
include(JADE_compiler_setup)
set(CMAKE_POSITION_INDEPENDENT_CODE FALSE)
########################################################################
find_package(BLAS REQUIRED)
message(STATUS "JADE JTUPLE: BLAS_LIBRARIES=${BLAS_LIBRARIES}")
find_package(LAPACK REQUIRED)
message(STATUS "JADE JTUPLE: LAPACK_LIBRARIES=${LAPACK_LIBRARIES}")
########################################################################
if (JADE_USE_CERNLIB)
  find_package(CERNLIB  REQUIRED)
  message(STATUS "JADE JTUPLE: CERNLIB_KERNLIB_STATIC=${CERNLIB_KERNLIB_STATIC}")
  message(STATUS "JADE JTUPLE: CERNLIB_PACKLIB_STATIC=${CERNLIB_PACKLIB_STATIC}")
  message(STATUS "JADE JTUPLE: CERNLIB_MATHLIB_STATIC=${CERNLIB_MATHLIB_STATIC}")
  if(${CMAKE_SYSTEM_NAME} MATCHES "Darwin")
    set(Motif_DIR /usr/local/Cellar/openmotif/2.3.8_1)
  endif()
  find_package(Motif REQUIRED)
  message(STATUS "JADE  JTUPLE: MOTIF_LIBRARIES=${MOTIF_LIBRARIES}")
  set(PICOCERNLIB "${MOTIF_LIBRARIES};${CERNLIB_PACKLIB_STATIC};${CERNLIB_KERNLIB_STATIC};${CERNLIB_MATHLIB_STATIC};${CERNLIB_GRAFLIB_STATIC};${CERNLIB_GRAFX11_STATIC}")
endif()
set(JEXTERN_LIBRARIES  ${PICOCERNLIB})
########################################################################
message(STATUS "JEXTERN libraries       : ${JEXTERN_LIBRARIES}")
########################################################################
find_package(JADESOFT REQUIRED)
message(STATUS "JADE JTUPLE: JADESOFT_LIBRARIES_STATIC=${JADESOFT_LIBRARIES_STATIC}")
########################################################################


set(JTJOBEXE jtjob)
set(JZREADEXE jzread)
set(TAKLUMIEXE taklumi)

# Define the library name
set(JTLIB jtlib)
set(NTUPLE          ntuple)
set(ZREAD         zread)

# Define some directories
set(SRC ${CMAKE_SOURCE_DIR}/src)
set(LIB ${CMAKE_SOURCE_DIR}/lib)
set(BIN ${CMAKE_SOURCE_DIR}/bin)
include_directories( ${SRC} )

set(JTLIB_src 
${SRC}/jadev.F ${SRC}/jadepr.F ${SRC}/endian.f ${SRC}/jzsetr.F ${SRC}/jzinit.F ${SRC}/jzevnt.F ${SRC}/jzfnsh.F
)
ADD_LIBRARY(${JTLIB} ${JTLIB_src})

set(NTUPLE_src 
${SRC}/jtbknt.F ${SRC}/jtevsl.F ${SRC}/casso.f ${SRC}/cinfo.f ${SRC}/cnass.f ${SRC}/cnone.f ${SRC}/cyjet.f
                ${SRC}/qqcone.f ${SRC}/qqjets.f ${SRC}/caxes.f ${SRC}/ckern.f 
                ${SRC}/cncnt.f ${SRC}/cwrit.f ${SRC}/qqangl.f 
                ${SRC}/qqevsh.f ${SRC}/cdble.f ${SRC}/cksord.f ${SRC}/cnjet.f ${SRC}/cyget.f 
                ${SRC}/qqckbk.f ${SRC}/qqform.f ${SRC}/px116.f
)
ADD_LIBRARY(${NTUPLE} ${NTUPLE_src})

set(ZREAD_src  ${SRC}/jtevnt.F ${SRC}/jtfnsh.F ${SRC}/jtinit.F ${SRC}/jtsetr.F)
ADD_LIBRARY(${ZREAD} ${ZREAD_src})

set(TAKLUMIEXE_src ${SRC}/taklumi.f)
add_executable(${TAKLUMIEXE} ${TAKLUMIEXE_src})

set(JZREADEXE_src ${SRC}/jzread.F)
add_executable(${JZREADEXE} ${JZREADEXE_src})

set(JTJOBEXE_src ${SRC}/jtjob.F  ${SRC}/showb.f)
add_executable(${JTJOBEXE} ${JTJOBEXE_src})

if(${CMAKE_SYSTEM_NAME} MATCHES "Darwin")
  target_link_libraries(${TAKLUMIEXE}   ${JEXTERN_LIBRARIES} )
  target_link_libraries(${JZREADEXE}   ${JEXTERN_LIBRARIES} ${ZREAD} ${NTUPLE} ${JTLIB} ${JADESOFT_LIBRARIES_STATIC} ${LAPACK_LIBRARIES} )
  target_link_libraries(${JTJOBEXE}  ${JEXTERN_LIBRARIES} ${ZREAD} ${NTUPLE} ${BOSLIB} ${JADESOFT_LIBRARIES_STATIC} ${LAPACK_LIBRARIES} )
else()
  target_link_libraries(${TAKLUMIEXE} -Wl,--start-group ${JEXTERN_LIBRARIES} -Wl,--end-group)
  target_link_libraries(${JZREADEXE} -Wl,--start-group ${JEXTERN_LIBRARIES} ${ZREAD} ${NTUPLE} ${JTLIB} ${JADESOFT_LIBRARIES_STATIC} ${LAPACK_LIBRARIES} -Wl,--end-group)
  target_link_libraries(${JTJOBEXE} -Wl,--start-group ${JEXTERN_LIBRARIES} ${ZREAD} ${NTUPLE} ${JADESOFT_LIBRARIES_STATIC} ${LAPACK_LIBRARIES} -Wl,--end-group)
endif()

########################################################################
INSTALL(TARGETS ${JTJOBEXE} ${JZREADEXE} ${TAKLUMIEXE} RUNTIME DESTINATION ${CMAKE_INSTALL_BINDIR})
INSTALL( TARGETS ${BOSLIB}  ${ZREAD}  ${NTUPLE} ARCHIVE DESTINATION ${CMAKE_INSTALL_LIBDIR} LIBRARY DESTINATION ${CMAKE_INSTALL_LIBDIR} COMPONENT library )
########################################################################
