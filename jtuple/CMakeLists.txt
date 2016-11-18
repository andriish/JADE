#https://github.com/SethMMorton/cmake_fortran_template
##################################################
# Define the project and the depencies that it has
##################################################

CMAKE_MINIMUM_REQUIRED(VERSION 2.8.5)
PROJECT(JTUPLE Fortran)

# Set the FOO version
SET(VERSION 2.0)

# Add our local modlues to the module path
SET(CMAKE_MODULE_PATH "${CMAKE_SOURCE_DIR}/../cmake/Modules/")

set(CMAKE_VERBOSE_MAKEFILE ON)

set(JEXTERN_LIBRARIES  ${CMAKE_SOURCE_DIR}/../picocernlib/libpicocernlib.a ${ROOT_LIBRARIES} -lstdc++)
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
   set(CMAKE_Fortran_FLAGS            "${CMAKE_Fortran_FLAGS}  -std=legacy -g -ffixed-line-length-none -finit-local-zero   -fno-automatic -O0   -fno-align-commons ")
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
SET(JTJOBEXE jtjob)
SET(JZREADEXE jzread)
SET(TAKLUMIEXE taklumi)


# Define the library name
SET(JTLIB jtlib)
SET(NTUPLE          ntuple)
SET(ZREAD         zread)

FIND_PATH(JADELIB_LIBRARY_DIR NAMES libinterface.a libjadegs.a libboslib.a libzlib.a PATHS
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

SET(ZREAD_src 
${SRC}/jtevnt.F ${SRC}/jtfnsh.F ${SRC}/jtinit.F ${SRC}/jtsetr.F
)
ADD_LIBRARY(${ZREAD} ${ZREAD_src})





SET(TAKLUMIEXE_src ${SRC}/taklumi.f)
ADD_EXECUTABLE(${TAKLUMIEXE} ${TAKLUMIEXE_src})
TARGET_LINK_LIBRARIES(${TAKLUMIEXE} -Wl,--start-group ${JEXTERN_LIBRARIES} -Wl,--end-group)


SET(JZREADEXE_src ${SRC}/jzread.F)
ADD_EXECUTABLE(${JZREADEXE} ${JZREADEXE_src})
TARGET_LINK_LIBRARIES(${JZREADEXE} -Wl,--start-group ${JEXTERN_LIBRARIES} ${ZREAD} ${NTUPLE} ${JTLIB} ${JADE_LIBRARIES} lapack -Wl,--end-group)

SET(JTJOBEXE_src ${SRC}/jtjob.F  ${SRC}/showb.f)
ADD_EXECUTABLE(${JTJOBEXE} ${JTJOBEXE_src})
TARGET_LINK_LIBRARIES(${JTJOBEXE} -Wl,--start-group ${JEXTERN_LIBRARIES} ${ZREAD} ${NTUPLE} ${JADE_LIBRARIES} lapack -Wl,--end-group)






SET(CMAKE_Fortran_MODULE_DIRECTORY ${LIB})
# Add a distclean target to the Makefile
ADD_CUSTOM_TARGET(distclean 
    COMMAND ${CMAKE_COMMAND} -P ${CMAKE_SOURCE_DIR}/distclean.cmake
)
