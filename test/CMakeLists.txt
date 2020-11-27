CMAKE_MINIMUM_REQUIRED(VERSION 3.5.0) #Because of ROOT
PROJECT(TEST Fortran CXX C)
SET(VERSION 2020.11)
include("GNUInstallDirs")
SET(CMAKE_MODULE_PATH "${CMAKE_SOURCE_DIR}/../cmake/Modules/")
set(CMAKE_VERBOSE_MAKEFILE OFF)
########################################################################
include(CheckCXXCompilerFlag)
CHECK_CXX_COMPILER_FLAG("-std=c++1y" COMPILER_SUPPORTS_CXX1y)
CHECK_CXX_COMPILER_FLAG("-std=c++11" COMPILER_SUPPORTS_CXX11)
if(COMPILER_SUPPORTS_CXX1y)
  set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -std=c++1y")
else()
  if(COMPILER_SUPPORTS_CXX11)
    set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -std=c++11")
  else()
    message(STATUS "The compiler ${CMAKE_CXX_COMPILER} has no C++11 support.")
  endif()
endif()
########################################################################
find_package(HepMC3 REQUIRED)
find_package(Pythia8 8.220)
ENABLE_TESTING()
if(NOT PYTHIA8_FOUND)
  message(STATUS "JADE test: Pythia8 package not found, was disabled or too old. The minimal supported Pythia8 version is 8.220. Please check if PYTHIA8_ROOT_DIR is set properly.") 
else()
  message(STATUS "JADE test: Pythia8 package found. ${PYTHIA8_LIBRARIES} ${PYTHIA8_VERSION}") 
  add_executable(pythia8_JADE.exe  pythia8_JADE.cc)
  target_link_libraries(pythia8_JADE.exe  ${PYTHIA8_LIBRARIES}  ${HEPMC3_LIBRARIES}  )
  target_include_directories(pythia8_JADE.exe PRIVATE ${HEPMC3_INCLUDE_DIR} ${PYTHIA8_INCLUDE_DIR}/Pythia8Plugins/ ${HEPMC3_INTERFACES_DIR}/pythia8/include/Pythia8/)
  add_test(NAME pythia8_HepMC3_1 COMMAND pythia8_JADE.exe 
                                         ${CMAKE_CURRENT_SOURCE_DIR}/testPythia1.input 
                                         ${CMAKE_CURRENT_BINARY_DIR}/testPythia1.output )
 
  add_test(NAME HepMC3_CPROD_1 COMMAND     ${JADEPREFIX}/bin/convert_example_JADE.exe -i hepmc3 -o jade 
                                         ${CMAKE_CURRENT_BINARY_DIR}/testPythia1.output 
                                         ${CMAKE_CURRENT_BINARY_DIR}/testConvert1.cprod
                                         -e Mode=0 )
  set_tests_properties(HepMC3_CPROD_1 PROPERTIES DEPENDS pythia8_HepMC3_1)   
  
  add_test(NAME CPROD_BOS_1 COMMAND sh ${CMAKE_CURRENT_SOURCE_DIR}/wrapper.sh mcjade1.card  ${JADEPREFIX}/bin/mcjade )

  set_tests_properties(CPROD_BOS_1 PROPERTIES DEPENDS HepMC3_CPROD_1)   

endif()

########################################################################
find_program( MEMORYCHECK_COMMAND valgrind )
if(MEMORYCHECK_COMMAND)
  message(STATUS "JADE test: valgrind package found in ${MEMORYCHECK_COMMAND}.  memory check tests enabled.") 
  set( MEMORYCHECK_COMMAND_OPTIONS "--trace-children=yes --leak-check=full --error-exitcode=1" )
elseif()
  message(STATUS "JADE test: valgrind package not found or disabled.  memory check tests disabled.") 
endif()
########################################################################
