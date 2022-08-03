CMAKE_MINIMUM_REQUIRED(VERSION 3.5.0) #Because of ROOT
PROJECT(JADECONVERT Fortran C CXX)
set(VERSION 2022.08)
include("GNUInstallDirs")
set(CMAKE_MODULE_PATH "${CMAKE_SOURCE_DIR}/../cmake/Modules/")
set(CMAKE_VERBOSE_MAKEFILE OFF)
########################################################################
include(JADE_compiler_setup)
set(CMAKE_POSITION_INDEPENDENT_CODE FALSE)
########################################################################
find_package(BLAS REQUIRED)
message(STATUS "JADE convert: BLAS_LIBRARIES=${BLAS_LIBRARIES}")
find_package(LAPACK REQUIRED)
message(STATUS "JADE convert: LAPACK_LIBRARIES=${LAPACK_LIBRARIES}")
########################################################################
find_package(HepMC3  REQUIRED) #The later is to test JADE with NAG on Cobra
message(STATUS "JADE convert: HEPMC3_LIBRARIES=${HEPMC3_LIBRARIES}")
########################################################################
include(JADE_ROOT_setup)
########################################################################
find_program(GENGETOPT gengetopt)
if( NOT GENGETOPT )
  message( STATUS  "JADE convert:  gengetopt not found. Old scripts for options parsing will be used." )
else()
  message( STATUS  "JADE convert:  GENGETOPT=${GENGETOPT}. New scripts for options parsing can be generated." )
  add_custom_command(
    OUTPUT  ${CMAKE_CURRENT_SOURCE_DIR}/cmdline.c ${CMAKE_CURRENT_SOURCE_DIR}/cmdline.h
    COMMAND ${GENGETOPT} -N  --input=${CMAKE_CURRENT_SOURCE_DIR}/cmdline.ggo --file-name cmdline --output-dir=${CMAKE_CURRENT_SOURCE_DIR} 
                        --include-getopt --show-required -u\'inputfile\ outputfile\'
    DEPENDS ${CMAKE_CURRENT_SOURCE_DIR}/cmdline.ggo
    COMMENT        "JADE convert:  Generating getopt parser code..."
    VERBATIM
  )
endif()
add_custom_target( cmdline DEPENDS ${CMAKE_CURRENT_SOURCE_DIR}/cmdline.c ${CMAKE_CURRENT_SOURCE_DIR}/cmdline.h)
########################################################################
include_directories(${CMAKE_CURRENT_SOURCE_DIR}/include ${CMAKE_CURRENT_SOURCE_DIR}  ${ROOT_INCLUDE_DIRS}  ${HEPMC3_INCLUDE_DIRS})
add_executable(convert_hepmc_to_jade
        ${CMAKE_CURRENT_SOURCE_DIR}/cmdline.c
        ${CMAKE_CURRENT_SOURCE_DIR}/convert_mc.cc
        ${CMAKE_CURRENT_SOURCE_DIR}/src/WriterJADE.cc 
        ${CMAKE_CURRENT_SOURCE_DIR}/src/jadeio.f
        )
target_compile_definitions( convert_hepmc_to_jade PUBLIC HEPMCCONVERT_EXTENSION_JADE  )
target_include_directories(convert_hepmc_to_jade PUBLIC ${HEPMC3_INCLUDE_DIR})
target_link_libraries(convert_hepmc_to_jade ${HEPMC3_LIBRARIES} ${HEPMC3_SEARCH_LIB}  ${LAPACK_LIBRARIES})
set_target_properties(convert_hepmc_to_jade PROPERTIES LINKER_LANGUAGE Fortran) 
add_dependencies( convert_hepmc_to_jade cmdline  )
########################################################################
# installs 
install(TARGETS convert_hepmc_to_jade RUNTIME DESTINATION bin)
########################################################################
