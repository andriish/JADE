CMAKE_MINIMUM_REQUIRED(VERSION 3.5.0) #Because of ROOT
PROJECT(JADECONVERT Fortran C CXX)

SET(VERSION 0.99)

# Add our local modlues to the module path
SET(CMAKE_MODULE_PATH "${CMAKE_SOURCE_DIR}/../cmake/Modules/")
include("GNUInstallDirs")
set(CMAKE_VERBOSE_MAKEFILE ON)


find_package(LAPACK REQUIRED)
find_package(HepMC3)
find_package(ROOT)


if(CMAKE_SYSTEM_NAME MATCHES Darwin)
set(CMAKE_C_FLAGS   "${CMAKE_C_FLAGS} -O0 -g ")
set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -O0 -g  ")
else()
set(CMAKE_C_FLAGS   "${CMAKE_C_FLAGS} -O0 -g  ")
set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -O0 -g  ")
endif()



include(CheckCXXCompilerFlag)
CHECK_CXX_COMPILER_FLAG("-std=c++1y" COMPILER_SUPPORTS_CXX1y)
CHECK_CXX_COMPILER_FLAG("-std=c++11" COMPILER_SUPPORTS_CXX11)
CHECK_CXX_COMPILER_FLAG("-std=c++0x" COMPILER_SUPPORTS_CXX0X)
if(COMPILER_SUPPORTS_CXX1y)
   set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -std=c++1y")
else()
if(COMPILER_SUPPORTS_CXX11)
   set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -std=c++11")
elseif(COMPILER_SUPPORTS_CXX0X)
  set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -std=c++0x")
else()
message(STATUS "The compiler ${CMAKE_CXX_COMPILER} has no C++11 support.")
endif()
endif()

########################################################################

set (ROOT_FIND_COMPONENTS Gui Core RIO Net Hist Graf Graf3d Gpad Tree Rint Postscript Matrix Physics MathCore Thread)
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
#     find_package(ROOT QUIET REQUIRED COMPONENTS Gui Core Cint RIO Net Hist Graf Graf3d Gpad Tree Rint Postscript Matrix Physics MathCore Thread)
     find_package(ROOT QUIET REQUIRED COMPONENTS Gui Core RIO Net Hist Graf Graf3d Gpad Tree Rint Postscript Matrix Physics MathCore Thread)
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

include_directories(${CMAKE_CURRENT_SOURCE_DIR}/include ${CMAKE_CURRENT_SOURCE_DIR}  ${ROOT_INCLUDE_DIRS}  ${HEPMC3_INCLUDE_DIRS})



message(STATUS "Fortran compiler        : ${Fortran_COMPILER_NAME}")
message(STATUS "Fortran compiler flags  : ${CMAKE_Fortran_FLAGS}")
message(STATUS "C++ compiler            : ${CXX_COMPILER_NAME}")
message(STATUS "C++ compiler flags      : ${CMAKE_CXX_FLAGS}")
message(STATUS "HEPMC3 libraries        : ${HEPMC3_LIBRARIES} ${HEPMC3_ROOTIO_LIBRARIES}")
message(STATUS "LAPACK libraries        : ${LAPACK_LIBRARIES_DIR}")


find_program(GENGETOPT gengetopt)
if( NOT GENGETOPT )
  message( STATUS  "JADESOFT:  gengetopt not found. Old scripts for options parsing will be used." )
else()
  message( STATUS  "JADESOFT:  gengetopt found in ${GENGETOPT}. New scripts for options parsing can be generated." )
  add_custom_command(
    OUTPUT  ${CMAKE_CURRENT_SOURCE_DIR}/cmdline.c ${CMAKE_CURRENT_SOURCE_DIR}/cmdline.h
    COMMAND ${GENGETOPT} -N  --input=${CMAKE_CURRENT_SOURCE_DIR}/cmdline.ggo --file-name cmdline --output-dir=${CMAKE_CURRENT_SOURCE_DIR} 
                        --include-getopt --show-required -u'inputfile outputfile'
    DEPENDS ${CMAKE_CURRENT_SOURCE_DIR}/cmdline.ggo
    COMMENT        "JADESOFT:  Generating getopt parser code..."
    VERBATIM
  )

endif()
add_custom_target( cmdline DEPENDS ${CMAKE_CURRENT_SOURCE_DIR}/cmdline.c ${CMAKE_CURRENT_SOURCE_DIR}/cmdline.h)

add_executable(convert_example_JADE.exe
        ${CMAKE_CURRENT_SOURCE_DIR}/cmdline.c
        ${CMAKE_CURRENT_SOURCE_DIR}/convert_example.cc
        ${CMAKE_CURRENT_SOURCE_DIR}/src/WriterJADE.cc 
        ${CMAKE_CURRENT_SOURCE_DIR}/src/jadeio.f
        )

target_include_directories(convert_example_JADE.exe ${HEPMC3_INCLUDE_DIR})
target_link_libraries(convert_example_JADE.exe ${HEPMC3_LIBRARIES}   ${LAPACK_LIBRARIES})
add_dependencies( convert_example_JADE.exe cmdline  )

# installs 
install(TARGETS convert_example_JADE.exe RUNTIME DESTINATION bin)
