# CMake project file for FOO from 

##################################################
# Define the project and the depencies that it has
##################################################

CMAKE_MINIMUM_REQUIRED(VERSION 2.8.5)
PROJECT(JADESOFT Fortran)

SET(VERSION 0.0.1)

# Add our local modlues to the module path
SET(CMAKE_MODULE_PATH "${CMAKE_SOURCE_DIR}/cmake/Modules/")

set(CMAKE_VERBOSE_MAKEFILE ON)
set(CERNLIB_FIND_REQUIRED ON)


 ################################################################################
 #    Copyright (C) 2014 GSI Helmholtzzentrum fuer Schwerionenforschung GmbH    #
 #                                                                              #
 #              This software is distributed under the terms of the             # 
 #         GNU Lesser General Public Licence version 3 (LGPL) version 3,        #  
 #                  copied verbatim in the file "LICENSE"                       #
 ################################################################################
# - Try to find CERNLIB
# Once done this will define
#
#  CERNLIB_FOUND - system has CERNLIB
#### (not needed)  CERNLIB_INCLUDE_DIR - the CERNLIB include directory
#  CERNLIB_LIBRARIES - The libraries needed to use CERNLIB
#### (not needed)  CERNLIB_DEFINITIONS - Compiler switches required for using CERNLIB
#

if (CERNLIB_INCLUDE_DIR AND CERNLIB_LIBRARY_DIR)
  SET (CERNLIB_INCLUDE_DIR CERNLIB_INCLUDE_DIR-NOTFOUND)
  SET (CERNLIB_LIB_DIR CERNLIB_LIB_DIR-NOTFOUND)
  SET (CERNLIB_PLISTS_LIB_DIR CERNLIB_PLISTS_LIB_DIR-NOTFOUND)
endif (CERNLIB_INCLUDE_DIR AND CERNLIB_LIBRARY_DIR)

MESSAGE(STATUS "Looking for CERNLIB...")

FIND_PATH(CERNLIB_INCLUDE_DIR NAMES CERNLIB PATHS
  ${SIMPATH}/cern/include
  $ENV{CERN_ROOT}/include
#CentOS
  /usr/include/cernlib/2006
  NO_DEFAULT_PATH
)

FIND_PATH(CERNLIB_LIBRARY_DIR NAMES libgeant.a libmathlib.a PATHS
  ${SIMPATH}/cern/lib
  $ENV{CERN_ROOT}/lib
#CentOS
  /usr/lib/cernlib/2006/lib
  NO_DEFAULT_PATH
)

if (CERNLIB_LIBRARY_DIR)
   set(CERNLIB_FOUND TRUE)
endif (CERNLIB_LIBRARY_DIR)
set(cernlibs kernlib packlib mathlib graflib grafX11)  
if (CERNLIB_FOUND)
  if (NOT CERNLIB_FIND_QUIETLY)
    MESSAGE(STATUS "Looking for CERNLIB... - found ${CERNLIB_LIBRARY_DIR}")
  endif (NOT CERNLIB_FIND_QUIETLY)
  
  SET(LD_LIBRARY_PATH ${LD_LIBRARY_PATH} ${CERNLIB_LIBRARY_DIR})
set(CERN_LIBRARIES)
foreach(_cpt ${cernlibs})
#one can link with static
#  find_library(CERN_${_cpt}_LIBRARY lib${_cpt}.a ${_cpt} HINTS ${CERNLIB_LIBRARY_DIR})
#or dynamic cernlib
  find_library(CERN_${_cpt}_LIBRARY lib${_cpt}.so ${_cpt} HINTS ${CERNLIB_LIBRARY_DIR})
  if(CERN_${_cpt}_LIBRARY)
    mark_as_advanced(CERN_${_cpt}_LIBRARY)
    list(APPEND CERN_LIBRARIES ${CERN_${_cpt}_LIBRARY})
  endif()
endforeach()
list(REMOVE_DUPLICATES CERN_LIBRARIES)
  
else (CERNLIB_FOUND)
  if (CERNLIB_FIND_REQUIRED)
    message(FATAL_ERROR "Looking for CERNLIB... - Not found")
  endif (CERNLIB_FIND_REQUIRED)
endif (CERNLIB_FOUND)
####################################################################################################


#INCLUDE(${CMAKE_MODULE_PATH}/SetUpLAPACK.cmake)



get_filename_component (Fortran_COMPILER_NAME ${CMAKE_Fortran_COMPILER} NAME)

if (Fortran_COMPILER_NAME MATCHES "gfortran.*")
   set(CMAKE_Fortran_FLAGS            "-m32 -std=legacy -m32 -g -ffixed-line-length-none -finit-local-zero   -fno-automatic -O0   -fno-align-commons ")
endif()

if (Fortran_COMPILER_NAME MATCHES "ifort.*")
#NOT USED
   set(CMAKE_Fortran_FLAGS            "-fno-automatic -fno-backslash -extend-source 132")	
endif()

if (Fortran_COMPILER_NAME MATCHES "xlf.*")
#PPC64 only
   set(CMAKE_Fortran_FLAGS           " -qfixed=256 -q32  -melf32ppc -qsigtrap -g -qsave -qrndsngl -qmaxmem=-1 -qextname -qfloat=fltint:hsflt:hssngl:nans:rndsngl -qcharlen=32767 -qxlf77=leadzero -qfullpath -qctyplss -qintlog ")	
   find_library(GFORTRAN_LIBRARY NAMES libgfortran.so.3  gfortran HINTS /usr/lib )
   list(APPEND CERN_LIBRARIES  ${GFORTRAN_LIBRARY})
endif()

if (Fortran_COMPILER_NAME MATCHES "sun.*")
#NOT WELL TESTED
#Local variables are static by default. Alternate is STATIC statement. 
   set(CMAKE_Fortran_FLAGS           "  -ftrap=%none -f77 -e -m32  -g  ")	
   list(APPEND CERN_LIBRARIES  /usr/lib/libgfortran.so.3)
   list(APPEND CERN_LIBRARIES  /lib/libgcc_s.so.1)
endif()

#message(STATUS "Fortran compiler : ${Fortran_COMPILER_NAME}")
#elseif()

#endif()
message(STATUS "Fortran compiler        : ${Fortran_COMPILER_NAME}")
message(STATUS "Fortran compiler flags  : ${CMAKE_Fortran_FLAGS}")
message(STATUS "CERN libraries          : ${CERN_LIBRARIES}")

############################################################
# Define the actual files and folders that make up the build
############################################################

# Define the executable name
#SET(FOOEXE foo)
SET(MCJADEEXE mcjade)
SET(SUPERVEXE superv)
SET(JADEZEXE jadez)
SET(ZE4VEXE ze4v)

# Define the library name
SET(BOSLIB boslib)
SET(GRAFIX          grafix)
SET(INTERFACE         interface)
SET(JADEGS         jadegs)
SET(JADEMUS         jademus)
#SET(JADESR         jadesr)
SET(JMC         jmc)
SET(PATRECSR         patrecsr)
SET(SOURCE         source)
SET(TAGG         tagg)
SET(TOFLIB         toflib)
SET(VERTEX         vertex)
SET(WERTEX         wertex)
SET(ZLIB         zlib)



# Define some directories
SET(SRC ${CMAKE_SOURCE_DIR}/src2016)
SET(SRCMAIN ${CMAKE_SOURCE_DIR}/main2016)
SET(LIB ${CMAKE_SOURCE_DIR}/lib)
SET(BIN ${CMAKE_SOURCE_DIR}/bin)

include_directories( ${SRC}/boslib ${SRC}/grafix ${SRC}/interface ${SRC}/jadegs ${SRC}/jademus ${SRC}/jadesr ${SRC}/jmc ${SRC}/patrecsr ${SRC}/source ${SRC}/tagg  ${SRC}/toflib ${SRC}/vertex ${SRC}/wertex ${SRC}/zlib  )

SET(SRCMCJADE ${SRCMAIN}/mcjade)
SET(SRCSUPERV ${SRCMAIN}/superv)
SET(SRCJADEZ ${SRCMAIN}/jadez)
SET(SRCZE4V ${SRCMAIN}/ze4v)



SET(SRCBOSLIB ${SRC}/boslib)
SET(SRCGRAFIX          ${SRC}/grafix)
SET(SRCINTERFACE         ${SRC}/interface)
SET(SRCJADEGS         ${SRC}/jadegs)
SET(SRCJADEMUS         ${SRC}/jademus)
SET(SRCJMC         ${SRC}/jmc)
SET(SRCPATRECSR         ${SRC}/patrecsr)
SET(SRCSOURCE         ${SRC}/source)
SET(SRCTAGG         ${SRC}/tagg)
SET(SRCTOFLIB         ${SRC}/toflib)
SET(SRCVERTEX         ${SRC}/vertex)
SET(SRCWERTEX         ${SRC}/wertex)
SET(SRCZLIB         ${SRC}/zlib)
# Have the .mod files placed in the lib folder
SET(CMAKE_Fortran_MODULE_DIRECTORY ${LIB})


if (Fortran_COMPILER_NAME MATCHES "xlf.*")
   file(GLOB myfortag ${SRCTAGG}/*.for)
   set_property(SOURCE ${myfortag} PROPERTY COMPILE_FLAGS -qsuffix=f=for)
endif()

# The source for the BAR library and have it placed in the lib folder
ADD_SUBDIRECTORY(${SRCBOSLIB} )
ADD_SUBDIRECTORY(${SRCGRAFIX}      )
ADD_SUBDIRECTORY(${SRCINTERFACE}         )
ADD_SUBDIRECTORY(${SRCJADEGS}          )
ADD_SUBDIRECTORY(${SRCJADEMUS}         )
#ADD_SUBDIRECTORY(${SRCJADESR}         )
ADD_SUBDIRECTORY(${SRCJMC}          )
ADD_SUBDIRECTORY(${SRCPATRECSR}          )
ADD_SUBDIRECTORY(${SRCSOURCE}          )
ADD_SUBDIRECTORY(${SRCTAGG}         )
ADD_SUBDIRECTORY(${SRCTOFLIB}         )
ADD_SUBDIRECTORY(${SRCVERTEX}         )
ADD_SUBDIRECTORY(${SRCWERTEX}         )
ADD_SUBDIRECTORY(${SRCZLIB}         )
# The source for the FOO binary and have it placed in the bin folder
#ADD_SUBDIRECTORY(${SRCFOO} ${BIN})

ADD_SUBDIRECTORY(${SRCMCJADE} )
ADD_SUBDIRECTORY(${SRCSUPERV} )
ADD_SUBDIRECTORY(${SRCJADEZ} )
ADD_SUBDIRECTORY(${SRCZE4V} )

# Add a distclean target to the Makefile
ADD_CUSTOM_TARGET(distclean 
    COMMAND ${CMAKE_COMMAND} -P ${CMAKE_SOURCE_DIR}/distclean.cmake
)
