CMAKE_MINIMUM_REQUIRED(VERSION 3.5.0) #Because of ROOT
PROJECT(JADECONVERT Fortran C CXX)
SET(VERSION 2020.11)
include("GNUInstallDirs")
SET(CMAKE_MODULE_PATH "${CMAKE_SOURCE_DIR}/../cmake/Modules/")
set(CMAKE_VERBOSE_MAKEFILE OFF)
########################################################################
include(JADE_compiler_setup)
########################################################################
  find_package(BLAS REQUIRED)
  find_package(LAPACK REQUIRED)
message(STATUS "LAPACK libraries        : ${LAPACK_LIBRARIES_DIR}")
########################################################################
find_package(HepMC3  HINTS /u/andriish/Z/share/HepMC3/cmake/)
message(STATUS "HEPMC3 libraries        : ${HEPMC3_LIBRARIES} ${HEPMC3_ROOTIO_LIBRARIES}")
########################################################################
include(JADE_ROOT_setup)
########################################################################
find_program(GENGETOPT gengetopt)
if( NOT GENGETOPT )
  message( STATUS  "JADESOFT:  gengetopt not found. Old scripts for options parsing will be used." )
else()
  message( STATUS  "JADESOFT:  gengetopt found in ${GENGETOPT}. New scripts for options parsing can be generated." )
  add_custom_command(
    OUTPUT  ${CMAKE_CURRENT_SOURCE_DIR}/cmdline.c ${CMAKE_CURRENT_SOURCE_DIR}/cmdline.h
    COMMAND ${GENGETOPT} -N  --input=${CMAKE_CURRENT_SOURCE_DIR}/cmdline.ggo --file-name cmdline --output-dir=${CMAKE_CURRENT_SOURCE_DIR} 
                        --include-getopt --show-required -u\'inputfile\ outputfile\'
    DEPENDS ${CMAKE_CURRENT_SOURCE_DIR}/cmdline.ggo
    COMMENT        "JADESOFT:  Generating getopt parser code..."
    VERBATIM
  )
endif()
add_custom_target( cmdline DEPENDS ${CMAKE_CURRENT_SOURCE_DIR}/cmdline.c ${CMAKE_CURRENT_SOURCE_DIR}/cmdline.h)
########################################################################
include_directories(${CMAKE_CURRENT_SOURCE_DIR}/include ${CMAKE_CURRENT_SOURCE_DIR}  ${ROOT_INCLUDE_DIRS}  ${HEPMC3_INCLUDE_DIRS})
add_executable(convert_example_JADE.exe
        ${CMAKE_CURRENT_SOURCE_DIR}/cmdline.c
        ${CMAKE_CURRENT_SOURCE_DIR}/convert_example.cc
        ${CMAKE_CURRENT_SOURCE_DIR}/src/WriterJADE.cc 
        ${CMAKE_CURRENT_SOURCE_DIR}/src/jadeio.f
        )
target_compile_definitions( convert_example_JADE.exe PUBLIC HEPMCCONVERT_EXTENSION_JADE  )
target_include_directories(convert_example_JADE.exe PUBLIC ${HEPMC3_INCLUDE_DIR})
target_link_libraries(convert_example_JADE.exe ${HEPMC3_LIBRARIES}   ${LAPACK_LIBRARIES})
add_dependencies( convert_example_JADE.exe cmdline  )
########################################################################
# installs 
install(TARGETS convert_example_JADE.exe RUNTIME DESTINATION bin)
########################################################################
