# CMake project file for FOO

##################################################
# Define the project and the depencies that it has
##################################################

CMAKE_MINIMUM_REQUIRED(VERSION 2.8.5)
PROJECT(JADESOFT Fortran)

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
INCLUDE(${CMAKE_MODULE_PATH}/SetFortranFlags.cmake) 
# Locate and set parallelization libraries.  There are some CMake peculiarities
# taken care of here, such as the fact that the FindOpenMP routine doesn't know
# about Fortran.
#INCLUDE(${CMAKE_MODULE_PATH}/SetParallelizationLibrary.cmake)
# Setup the LAPACK libraries.  This also takes care of peculiarities, such as
# the fact the searching for MKL requires a C compiler, and that the results
# are not stored in the cache. 
#INCLUDE(${CMAKE_MODULE_PATH}/SetUpLAPACK.cmake)

# There is an error in CMAKE with this flag for pgf90.  Unset it
GET_FILENAME_COMPONENT(FCNAME ${CMAKE_Fortran_COMPILER} NAME)
IF(FCNAME STREQUAL "pgf90")
    UNSET(CMAKE_SHARED_LIBRARY_LINK_Fortran_FLAGS)
ENDIF(FCNAME STREQUAL "pgf90")

SET_COMPILE_FLAG(CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS}" Fortran 
"-m32 -std=legacy -m32 -g -ffixed-line-length-none -finit-local-zero   -fno-automatic -O0   -fno-align-commons")

############################################################
# Define the actual files and folders that make up the build
############################################################

# Define the executable name
#SET(FOOEXE foo)

# Define the library name
SET(BOSLIB boslib)
SET(GRAFIX          grafix)
SET(INTERFACE         interface)
SET(JADEGS         jadegs)
SET(JADEMUS         jademus)
SET(JADESR         jadesr)
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
SET(LIB ${CMAKE_SOURCE_DIR}/lib)
SET(BIN ${CMAKE_SOURCE_DIR}/bin)
#SET(SRCFOO ${SRC}/foo)
SET(SRCBOSLIB ${SRC}/boslib)
SET(SRCGRAFIX          ${SRC}/grafix)
SET(SRCINTERFACE         ${SRC}/interface)
SET(SRCJADEGS         ${SRC}/jadegs)
SET(SRCJADEMUS         ${SRC}/jademus)
SET(SRCJADESR         ${SRC}/jadesr)
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

# The source for the BAR library and have it placed in the lib folder
ADD_SUBDIRECTORY(${SRCBOSLIB} ${LIB})
ADD_SUBDIRECTORY(${GRAFIX}      ${LIB})
ADD_SUBDIRECTORY(${INTERFACE}         ${LIB})
ADD_SUBDIRECTORY(${JADEGS}          ${LIB})
ADD_SUBDIRECTORY(${JADEMUS}         ${LIB})
ADD_SUBDIRECTORY(${JADESR}         ${LIB})
ADD_SUBDIRECTORY(${JMC}          ${LIB})
ADD_SUBDIRECTORY(${PATRECSR}          ${LIB})
ADD_SUBDIRECTORY(${SOURCE}          ${LIB})
ADD_SUBDIRECTORY(${TAGG}         ${LIB})
ADD_SUBDIRECTORY(${TOFLIB}         ${LIB})
ADD_SUBDIRECTORY(${VERTEX}         ${LIB})
ADD_SUBDIRECTORY(${WERTEX}         ${LIB})
ADD_SUBDIRECTORY(${ZLIB}         ${LIB})
# The source for the FOO binary and have it placed in the bin folder
#ADD_SUBDIRECTORY(${SRCFOO} ${BIN})

# Add a distclean target to the Makefile
ADD_CUSTOM_TARGET(distclean 
    COMMAND ${CMAKE_COMMAND} -P ${CMAKE_SOURCE_DIR}/distclean.cmake
)
