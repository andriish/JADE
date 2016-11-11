# CMake project file for FOO

##################################################
# Define the project and the depencies that it has
##################################################

CMAKE_MINIMUM_REQUIRED(VERSION 2.8.5)
PROJECT(JTUPLE Fortran)

# Set the FOO version
SET(VERSION 1.2.3)

# Add our local modlues to the module path
SET(CMAKE_MODULE_PATH "${CMAKE_SOURCE_DIR}/cmake/Modules/")

# Uncomment if it is required that Fortran 90 is supported
#IF(NOT CMAKE_Fortran_COMPILER_SUPPORTS_F90)
#    MESSAGE(FATAL_ERROR "Fortran compiler does not support F90")
#ENDIF(NOT CMAKE_Fortran_COMPILER_SUPPORTS_F90)

# Set some options the user may choose
# Uncomment the below if you want the user to choose a parallelization library
#OPTION(USE_MPI "Use the MPI library for parallelization" OFF)
#OPTION(USE_OPENMP "Use OpenMP for parallelization" OFF)

# This INCLUDE statement executes code that sets the compile flags for DEBUG,
# RELEASE, and TESTING.  You should  review this file and make sure the flags 
# are to your liking.

#INCLUDE(${CMAKE_MODULE_PATH}/SetCompileFlag.cmake)
#set(CERNLIB_FIND_QUIETLY FALSE)
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


set(CERN_LIBRARIES ${CMAKE_SOURCE_DIR}/../picocernlib/libpicocernlib.a -lc  -L${CMAKE_SOURCE_DIR}/../ROOTLIBS 
-lCore -lCint -lRIO -lNet -lHist -lGraf -lGraf3d -lGpad -lTree -lRint -lPostscript -lMatrix -lPhysics -lMathCore -lThread -lstdc++)



get_filename_component (Fortran_COMPILER_NAME ${CMAKE_Fortran_COMPILER} NAME)
#if ("${CMAKE_Fortran_FLAGS}" STREQUAL "" )
if (Fortran_COMPILER_NAME MATCHES "gfortran.*")
   set(CMAKE_Fortran_FLAGS            "-m32 -std=legacy -m32 -g -ffixed-line-length-none -finit-local-zero -fbacktrace -fcheck=all  -fautomatic -O0   -fno-align-commons ")
endif()

if (Fortran_COMPILER_NAME MATCHES "ifort.*")
   set(CMAKE_Fortran_FLAGS            "-fno-automatic -fno-backslash -extend-source 132")	
endif()
if (Fortran_COMPILER_NAME MATCHES "xlf.*")
   set(CMAKE_Fortran_FLAGS           " -qfixed=256 -q32  -melf32ppc -qsigtrap -g -qsave -qrndsngl -qmaxmem=-1 -qextname -qfloat=fltint:hsflt:hssngl:nans:rndsngl -qcharlen=32767 -qxlf77=leadzero -qfullpath -qctyplss -qintlog ")	
   find_library(GFORTRAN_LIBRARY NAMES libgfortran.so.3  gfortran HINTS /usr/lib )
   list(APPEND CERN_LIBRARIES  ${GFORTRAN_LIBRARY})
endif()
#message(STATUS "Fortran compiler : ${Fortran_COMPILER_NAME}")
#elseif()

#endif()
message(STATUS "Fortran compiler        : ${Fortran_COMPILER_NAME}")
message(STATUS "Fortran compiler flags  : ${CMAKE_Fortran_FLAGS}")
message(STATUS "CERN libraries          : ${CERN_LIBRARIES}")


#if (Fortran_COMPILER_NAME MATCHES "xlf.*")
#   file(GLOB myfortag ${SRCTAGG}/*.for)
#      set_property(SOURCE ${myfortag} PROPERTY COMPILE_FLAGS -qsuffix=f=for)
#       endif()


############################################################
# Define the actual files and folders that make up the build
############################################################



# Define the executable name
#SET(FOOEXE foo)
SET(JTJOBEXE jtjob)
SET(JZREADEXE jzread)
SET(TAKLUMIEXE taklumi)


# Define the library name
SET(JTLIB jtlib)
SET(NTUPLE          ntuple)
SET(ZREAD         zread)


#libjtlib.a : libjtlib.a( $(LIBJADEOBJS) )
#libntuple.a : libntuple.a( $(LIBNTUPLEOBJS) )
#libzread.a : libzread.a( $(LIBZREADOBJS) )


#jtjob : jtjob.o libjtlib.a libntuple.a
	#$(LD) $(LDFLAGS) -o $@ $^ $(JADE)/main/superv/showb.o \
	#-L$(JADE)/lib $(patsubst %,-l%,$(JADELIBS)) \
	#-L$(CERNLIB) $(patsubst %,-l%,$(CERNLIBS)) >> MAIN.map

## Link ze4v -> ntuple:
#jzread : jzread.o libzread.a libntuple.a
	#$(LD) $(LDFLAGS) -o $@ $^ \
	#-L$(CERNLIB) $(patsubst %,-l%,$(CERNLIBS))

## Link lumi analysis program:
#taklumi : taklumi.o
	#$(LD) $(LDFLAGS) -o $@ $< \
	#-L$(CERNLIB) $(patsubst %,-l%,$(CERNLIBS))

 #LIBZREADOBJS = jadev.o jadepr.o endian.o jzsetr.o jzinit.o jzevnt.o jzfnsh.o
  #LIBJADEOBJS = jtevnt.o jtfnsh.o jtinit.o jtsetr.o
#LIBNTUPLEOBJS = jtbknt.o jtevsl.o casso.o cinfo.o cnass.o cnone.o cyjet.o \
                #qqcone.o qqjets.o caxes.o ckern.o \
                #cncnt.o cwrit.o qqangl.o \
                #qqevsh.o cdble.o cksord.o cnjet.o cyget.o \
                #qqckbk.o qqform.o px116.o

 #EXEOBJS = jtjob.o jzread.o
FIND_PATH(JADELIB_LIBRARY_DIR NAMES libinterface.a libjadegs.a libboslib.a libzlib.a PATHS
  /usr/lib/jadesoft
  ../jadesoft/lib
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
TARGET_LINK_LIBRARIES(${TAKLUMIEXE} -Wl,--start-group ${CERN_LIBRARIES} -Wl,--end-group)


SET(JZREADEXE_src ${SRC}/jzread.F)
ADD_EXECUTABLE(${JZREADEXE} ${JZREADEXE_src})
TARGET_LINK_LIBRARIES(${JZREADEXE} -Wl,--start-group ${CERN_LIBRARIES} ${ZREAD} ${NTUPLE} ${JTLIB} ${JADE_LIBRARIES} lapack -Wl,--end-group)

SET(JTJOBEXE_src ${SRC}/jtjob.F  ${SRC}/showb.f)
ADD_EXECUTABLE(${JTJOBEXE} ${JTJOBEXE_src})
TARGET_LINK_LIBRARIES(${JTJOBEXE} -Wl,--start-group ${CERN_LIBRARIES} ${ZREAD} ${NTUPLE} ${JADE_LIBRARIES} lapack -Wl,--end-group)






SET(CMAKE_Fortran_MODULE_DIRECTORY ${LIB})
# Add a distclean target to the Makefile
ADD_CUSTOM_TARGET(distclean 
    COMMAND ${CMAKE_COMMAND} -P ${CMAKE_SOURCE_DIR}/distclean.cmake
)
