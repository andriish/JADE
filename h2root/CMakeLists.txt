########################################################################
# jade_h2root: ROOT's h2root (HBOOK -> ROOT converter) with the 64 bit
# address fix, see README.md. Build:
#   cmake -S h2root -B buildGNU64/h2root [-DJADE_FORCE_32=ON] && cmake --build buildGNU64/h2root
########################################################################
CMAKE_MINIMUM_REQUIRED(VERSION 3.10) #Because of ROOT
PROJECT(JADEH2ROOT Fortran CXX C)
include("GNUInstallDirs")
option(JADE_FORCE_32 "Force build of 32 bit executables." OFF)
if (JADE_FORCE_32)
  list(PREPEND CMAKE_SYSTEM_LIBRARY_PATH "/usr/lib")
  set(FIND_LIBRARY_USE_LIB32_PATHS  ON)
  set(FIND_LIBRARY_USE_LIB64_PATHS  OFF)
  set(CMAKE_SIZEOF_VOID_P 4)
  set(CMAKE_C_FLAGS "-m32 ${CMAKE_C_FLAGS}")
  set(CMAKE_CXX_FLAGS "-m32 ${CMAKE_CXX_FLAGS}")
  set(CMAKE_Fortran_FLAGS "-m32 ${CMAKE_Fortran_FLAGS}")
  set(CMAKE_EXE_LINKER_FLAGS "-m32 ${CMAKE_EXE_LINKER_FLAGS}")
  if ("${LIB_SUFFIX}" STREQUAL "64")
    set(LIB_SUFFIX)
  endif()
endif()
########################################################################
find_package(ROOT REQUIRED COMPONENTS Core RIO Net Hist Graf Graf3d Gpad Tree Matrix MathCore Thread)
message(STATUS "JADE h2root: ROOT ${ROOT_VERSION}")
########################################################################
# The minimal HBOOK/ZEBRA/KERNLIB subset needed by h2root. Like ROOT does, compile
# the Fortran code without optimisation and without warnings.
add_library(minicern_jade STATIC
  src/hbook.f src/kernlib.f src/zebra.f src/cernlib.c)
set_target_properties(minicern_jade PROPERTIES POSITION_INDEPENDENT_CODE ON)
# Options for the old (non-standard, argument-mismatching) minicern sources, per compiler
if (CMAKE_Fortran_COMPILER_ID STREQUAL "GNU")
  set(MINICERN_FFLAGS -w -O0 -fallow-argument-mismatch -std=legacy)
elseif (CMAKE_Fortran_COMPILER_ID MATCHES "Intel")
  set(MINICERN_FFLAGS -w -O0)
elseif (CMAKE_Fortran_COMPILER_ID MATCHES "NVHPC|PGI")
  set(MINICERN_FFLAGS -w -O0)
elseif (CMAKE_Fortran_COMPILER_ID STREQUAL "NAG")
  set(MINICERN_FFLAGS -w -O0 -mismatch -dusty)
else()
  set(MINICERN_FFLAGS -O0)
endif()
target_compile_options(minicern_jade PRIVATE "$<$<COMPILE_LANGUAGE:Fortran>:${MINICERN_FFLAGS}>")
target_compile_options(minicern_jade PUBLIC "$<$<COMPILE_LANGUAGE:Fortran>:-O0>")
target_link_libraries(minicern_jade ${CMAKE_Fortran_IMPLICIT_LINK_LIBRARIES})

add_executable(jade_h2root src/h2root.cxx)
target_compile_options(jade_h2root PRIVATE -w)
target_link_libraries(jade_h2root PRIVATE
  ROOT::Core ROOT::RIO ROOT::Net ROOT::Hist ROOT::Graf ROOT::Graf3d ROOT::Gpad
  ROOT::Tree ROOT::Matrix ROOT::MathCore ROOT::Thread minicern_jade)
install(TARGETS jade_h2root DESTINATION ${CMAKE_INSTALL_BINDIR})
