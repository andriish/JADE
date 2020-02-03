# - Locate HepMC library
# in a directory defined via  HEPMC3_ROOT_DIR or HEPMC3_DIR environment variable
# Defines:
#
#  HEPMC3_FOUND
#  HEPMC3_INCLUDE_DIR
#  HEPMC3_INCLUDE_DIRS (not cached)
#  HEPMC3_LIBRARIES
#  HEPMC3_FIO_LIBRARIES

find_path(HEPMC3_INCLUDE_DIR HepMC/GenEvent.h
          HINTS 
          $ENV{HEPMC3_ROOT_DIR}/include 
          ${HEPMC3_ROOT_DIR}/include
          $ENV{HEPMC3_DIR}/include 
          ${HEPMC3_DIR}/include
          /usr/include/HepMC
          )

find_library(HEPMC3_DYNAMIC_LIBRARIES NAMES  libHepMC3.so libHepMC3rootIO.so
             HINTS 
             $ENV{HEPMC3_ROOT_DIR}/lib 
             $ENV{HEPMC3_ROOT_DIR}/lib64 
             ${HEPMC3_ROOT_DIR}/lib
             ${HEPMC3_ROOT_DIR}/lib64
             HINTS 
             $ENV{HEPMC3_DIR}/lib 
             $ENV{HEPMC3_DIR}/lib64 
             ${HEPMC3_DIR}/lib
             ${HEPMC3_DIR}/lib64
             /usr/lib
             /usr/lib64
             )

find_library(HEPMC3_STATIC_LIBRARIES NAMES libHepMC3.a 
             HINTS 
             $ENV{HEPMC3_ROOT_DIR}/lib 
             $ENV{HEPMC3_ROOT_DIR}/lib64 
             ${HEPMC3_ROOT_DIR}/lib
             ${HEPMC3_ROOT_DIR}/lib64
             HINTS 
             $ENV{HEPMC3_DIR}/lib 
             $ENV{HEPMC3_DIR}/lib64 
             ${HEPMC3_DIR}/lib
             ${HEPMC3_DIR}/lib64
             /usr/lib
             /usr/lib64
             )


get_filename_component(HEPMC3_STATIC_LIBRARY_DIR ${HEPMC3_STATIC_LIBRARIES} PATH)
get_filename_component(HEPMC3_DYMANIC_LIBRARY_DIR ${HEPMC3_DYNAMIC_LIBRARIES} PATH)
set(HEPMC3_LIBRARIES "${HEPMC3_STATIC_LIBRARY_DIR}/libHepMC3.so")
set(HEPMC3_ROOTIO_LIBRARIES "-L${HEPMC3_DYNAMIC_LIBRARY_DIR} -lHepMC3 -lHepMC3rootIO")

set(HEPMC3_INCLUDE_DIRS ${HEPMC3_INCLUDE_DIR})

# handle the QUIETLY and REQUIRED arguments and set HEPMC3_FOUND to TRUE if
# all listed variables are TRUE

INCLUDE(FindPackageHandleStandardArgs)

FIND_PACKAGE_HANDLE_STANDARD_ARGS(HepMC3 FOUND_VAR HEPMC3_FOUND REQUIRED_VARS HEPMC3_INCLUDE_DIR HEPMC3_STATIC_LIBRARIES HEPMC3_DYNAMIC_LIBRARIES)

mark_as_advanced(HEPMC3_FOUND HEPMC3_INCLUDE_DIRS HEPMC3_STATIC_LIBRARIES HEPMC3_DYNAMIC_LIBRARIES)
