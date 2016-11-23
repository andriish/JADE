#https://github.com/SethMMorton/cmake_fortran_template
##################################################
# Define the project and the depencies that it has
##################################################

CMAKE_MINIMUM_REQUIRED(VERSION 2.8.5)
PROJECT(JTUPLE Fortran)
include("GNUInstallDirs")
# Set the FOO version
SET(VERSION 2.0)

# Add our local modlues to the module path
SET(CMAKE_MODULE_PATH "${CMAKE_SOURCE_DIR}/../cmake/Modules/")

set(CMAKE_VERBOSE_MAKEFILE ON)

find_package(CERNLIB)

########################################################################

set (ROOT_FIND_COMPONENTS Gui Core Cint RIO Net Hist Graf Graf3d Gpad Tree Rint Postscript Matrix Physics MathCore Thread)
  find_file(ROOT_CONFIG ROOTConfig.cmake HINTS $ENV{ROOTSYS} ${ROOT_DIR} NO_DEFAULT_PATH PATH_SUFFIXES cmake)
  if (ROOT_CONFIG)
     message(STATUS "ROOTConfig.cmake found and will be used: ${ROOT_CONFIG}")
     include(${ROOT_CONFIG})
     message(STATUS "ROOT use file: ${ROOT_USE_FILE}")
     include(${ROOT_USE_FILE})
     if(ROOT_LIBRARIES AND ROOT_INCLUDE_DIRS)
       set(ROOT_FOUND TRUE)
     endif()
  else()
     message(STATUS "ROOTConfig.cmake not found, trying to use FindROOT.cmake...")
     find_package(ROOT QUIET REQUIRED COMPONENTS Gui Core Cint RIO Net Hist Graf Graf3d Gpad Tree Rint Postscript Matrix Physics MathCore Thread)
  endif()

  message(STATUS "ROOT includes : ${ROOT_INCLUDE_DIRS}")
  message(STATUS "ROOT libraries: ${ROOT_LIBRARIES}")
  message(STATUS "ROOT_FOUND    : ${ROOT_FOUND}")

  if(ROOT_FOUND)
    include_directories(${ROOT_INCLUDE_DIRS})
#    add_definitions(-DHEPMC_ROOTIO)
  else()
    message(STATUS "JADESOFT warning: ROOT not found")
  endif()


set(JEXTERN_LIBRARIES  ${PICOCERNLIB} ${ROOT_LIBRARIES} -lstdc++)
set(CMAKE_CXX_FLAGS     "${CMAKE_CXX_FLAGS}     -DJEXTERNISPICO")
set(CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} -DJEXTERNISPICO")

#############USE IT WITH CERNLIB->########################################
#set(JEXTERN_LIBRARIES   ${CERN_STATIC_LIBRARIES} )
#set(JEXTERN_LIBRARIES   ${CERN_DYNAMIC_LIBRARIES} ) 
#set(CMAKE_CXX_FLAGS     "${CMAKE_CXX_FLAGS}     -DJEXTERNISCERNLIB")
#set(CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} -DJEXTERNISCERNLIB")
#############<-USE IT WITH CERNLIB########################################


set(JEXTERN_GRAPH_LIBRARIES X11)
########################################################################


get_filename_component (Fortran_COMPILER_NAME ${CMAKE_Fortran_COMPILER} NAME)
if (Fortran_COMPILER_NAME MATCHES "gfortran.*")
   set(CMAKE_Fortran_FLAGS            "${CMAKE_Fortran_FLAGS} -fno-range-check -std=legacy -g -ffixed-line-length-none -finit-local-zero   -fno-automatic -O0   -fno-align-commons ")
endif()

if (Fortran_COMPILER_NAME MATCHES "ifort.*")
#NOT USED
   set(CMAKE_Fortran_FLAGS            "${CMAKE_Fortran_FLAGS}  -fno-automatic -fno-backslash -extend-source 132")	
endif()

if (Fortran_COMPILER_NAME MATCHES "xlf.*")
#PPC64 only
   set(CMAKE_Fortran_FLAGS           "${CMAKE_Fortran_FLAGS}  -qfixed=256  -qsigtrap -g -qsave -qrndsngl -qmaxmem=-1 -qextname -qfloat=fltint:hsflt:hssngl:nans:rndsngl -qcharlen=32767 -qxlf77=leadzero -qfullpath -qctyplss -qintlog ")	
#-q32  -melf32ppc
endif()

if (Fortran_COMPILER_NAME MATCHES "sun.*")
#NOT WELL TESTED
#Local variables are static by default. Alternate is STATIC statement. 
   set(CMAKE_Fortran_FLAGS           "${CMAKE_Fortran_FLAGS}   -ftrap=%none -f77 -e  -g  ")
#-m32 
endif()


message(STATUS "Fortran compiler        : ${Fortran_COMPILER_NAME}")
message(STATUS "Fortran compiler flags  : ${CMAKE_Fortran_FLAGS}")
message(STATUS "C++ compiler            : ${CXX_COMPILER_NAME}")
message(STATUS "C++ compiler flags      : ${CMAKE_CXX_FLAGS}")
message(STATUS "JEXTERN libraries       : ${JEXTERN_LIBRARIES}")
message(STATUS "JEXTERN_GRAPH libraries : ${JEXTERN_GRAPH_LIBRARIES}")



############################################################
# Define the actual files and folders that make up the build
############################################################



# Define the executable name
SET(FPTOBOSEXE fptobos)
SET(UNPJADEXE unpjad)


# Define the library name

FIND_PATH(JADELIB_LIBRARY_DIR NAMES libinterface.a libjadegs.a libboslib.a libzlib.a PATHS
  ${JADELIB_ROOT_DIR}/lib
  ${JADELIB_ROOT_DIR}/lib64
  /usr/lib/jadesoft
  ../jadesoft/lib
  ../jadesoft/
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
)
 
 
#${SRC}/mvcl.f 
ADD_EXECUTABLE(${FPTOBOSEXE} ${FPTOBOS_src})
ADD_EXECUTABLE(${UNPJADEXE} ${UNPJAD_src})


TARGET_LINK_LIBRARIES(${FPTOBOSEXE} -Wl,--start-group ${JEXTERN_LIBRARIES}  ${JADE_LIBRARIES} lapack -Wl,--end-group)
TARGET_LINK_LIBRARIES(${UNPJADEXE} -Wl,--start-group ${JEXTERN_LIBRARIES}  ${JADE_LIBRARIES} lapack -Wl,--end-group)



INSTALL(TARGETS ${FPTOBOSEXE}  RUNTIME DESTINATION ${CMAKE_INSTALL_BINDIR})
INSTALL(TARGETS ${UNPJADEXE}  RUNTIME DESTINATION ${CMAKE_INSTALL_BINDIR})



SET(CMAKE_Fortran_MODULE_DIRECTORY ${LIB})
# Add a distclean target to the Makefile
ADD_CUSTOM_TARGET(distclean 
    COMMAND ${CMAKE_COMMAND} -P ${CMAKE_SOURCE_DIR}/distclean.cmake
)
