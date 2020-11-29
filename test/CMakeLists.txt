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
find_package(ROOT)
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
  find_package(ROOT QUIET REQUIRED COMPONENTS Gui Core RIO Net Hist Graf Graf3d Gpad Tree Rint Postscript Matrix Physics MathCore Thread)
endif()
message(STATUS "ROOT includes : ${ROOT_INCLUDE_DIRS}")
message(STATUS "ROOT libraries: ${ROOT_LIBRARIES}")
message(STATUS "ROOT_FOUND    : ${ROOT_FOUND}")
if(ROOT_FOUND)
  include_directories(${ROOT_INCLUDE_DIRS})
else()
  message(STATUS "JADESOFT warning: ROOT not found")
endif()
########################################################################
find_package(HepMC3 REQUIRED)
find_package(Pythia8 8.220)
ENABLE_TESTING()

configure_file(${CMAKE_CURRENT_SOURCE_DIR}/wrapper.sh.in      ${CMAKE_CURRENT_BINARY_DIR}/wrapper.sh)
configure_file(${CMAKE_CURRENT_SOURCE_DIR}/old/aupdat1      ${CMAKE_CURRENT_BINARY_DIR}/aupdat1 COPYONLY)
configure_file(${CMAKE_CURRENT_SOURCE_DIR}/old/aupdat1.b    ${CMAKE_CURRENT_BINARY_DIR}/aupdat1.b COPYONLY)
configure_file(${CMAKE_CURRENT_SOURCE_DIR}/old/bupdat0      ${CMAKE_CURRENT_BINARY_DIR}/bupdat0 COPYONLY)
configure_file(${CMAKE_CURRENT_SOURCE_DIR}/old/bupdat0.b    ${CMAKE_CURRENT_BINARY_DIR}/bupdat0.b COPYONLY)
configure_file(${CMAKE_CURRENT_SOURCE_DIR}/old/bupdat1      ${CMAKE_CURRENT_BINARY_DIR}/bupdat1 COPYONLY)
configure_file(${CMAKE_CURRENT_SOURCE_DIR}/old/bupdat1.b    ${CMAKE_CURRENT_BINARY_DIR}/bupdat1.b COPYONLY)
configure_file(${CMAKE_CURRENT_SOURCE_DIR}/jadetapes/JAD073/file006    ${CMAKE_CURRENT_BINARY_DIR}/jadetapes/JAD073/file006 COPYONLY)



add_test(NAME FPTOBOS_1 COMMAND sh ${CMAKE_CURRENT_BINARY_DIR}/wrapper.sh ${CMAKE_CURRENT_SOURCE_DIR}/fptobos1.card  ${JADEPREFIX}/bin/fptobos)
set_property( TEST  FPTOBOS_1 PROPERTY ENVIRONMENT "GFORTRAN_CONVERT_UNIT=native") 

if(NOT PYTHIA8_FOUND)
  message(STATUS "JADE test: Pythia8 package not found, was disabled or too old. The minimal supported Pythia8 version is 8.220. Please check if PYTHIA8_ROOT_DIR is set properly.") 

else()
  message(STATUS "JADE test: Pythia8 package found. ${PYTHIA8_LIBRARIES} ${PYTHIA8_VERSION}") 
  add_executable(pythia8_JADE.exe  pythia8_JADE.cc)
  target_link_libraries(pythia8_JADE.exe  ${PYTHIA8_LIBRARIES}  ${HEPMC3_LIBRARIES}  )
  target_include_directories(pythia8_JADE.exe PRIVATE ${HEPMC3_INCLUDE_DIR} ${PYTHIA8_INCLUDE_DIR}/Pythia8Plugins/ ${HEPMC3_INTERFACES_DIR}/pythia8/include/Pythia8/)
  add_test(NAME pythia8_produce_HepMC3_MC1 COMMAND pythia8_JADE.exe 
                                         ${CMAKE_CURRENT_SOURCE_DIR}/testPythia1.input 
                                         ${CMAKE_CURRENT_BINARY_DIR}/testPythia1.output )
 
  add_test(NAME HepMC3_to_CPROD_MC1 COMMAND ${JADEPREFIX}/bin/convert_example_JADE.exe -i hepmc3 -o jade 
                                          ${CMAKE_CURRENT_BINARY_DIR}/testPythia1.output 
                                          ${CMAKE_CURRENT_BINARY_DIR}/testConvert1.cprod
                                          -e Mode=0 )
  set_property( TEST HepMC3_to_CPROD_MC1 PROPERTY DEPENDS pythia8_produce_HepMC3_MC1)   
  set_property( TEST HepMC3_to_CPROD_MC1 PROPERTY ENVIRONMENT "LD_LIBRARY_PATH=${ROOT_DIR}/../lib:${ROOT_DIR}/../lib64:$ENV{LD_LIBRARY_PATH}")   
  
  add_test(NAME MCJADE_MC1 COMMAND sh ${CMAKE_CURRENT_BINARY_DIR}/wrapper.sh ${CMAKE_CURRENT_SOURCE_DIR}/mcjade1.card  ${JADEPREFIX}/bin/mcjade )
  set_property( TEST MCJADE_MC1 PROPERTY DEPENDS HepMC3_to_CPROD_MC1)   
  set_property( TEST MCJADE_MC1 PROPERTY ENVIRONMENT "GFORTRAN_CONVERT_UNIT=native")

  add_test(NAME SUPERV_MC1 COMMAND sh ${CMAKE_CURRENT_BINARY_DIR}/wrapper.sh ${CMAKE_CURRENT_SOURCE_DIR}/superv1.card  ${JADEPREFIX}/bin/superv ) 
  set_property( TEST SUPERV_MC1 PROPERTY  DEPENDS CPROD_BOS_MC1) 
  set_property( TEST SUPERV_MC1 PROPERTY ENVIRONMENT "GFORTRAN_CONVERT_UNIT=big_endian\;native:2") 
  #NOTE: superv.stat needs a random number generator seeds. Otherwise it will use the same each time
   
  add_test(NAME ZE4V_MC1 COMMAND sh ${CMAKE_CURRENT_BINARY_DIR}/wrapper.sh ${CMAKE_CURRENT_SOURCE_DIR}/ze4v1.card  ${JADEPREFIX}/bin/ze4v)
  set_property( TEST ZE4V_MC1 PROPERTY  DEPENDS SUPERV_MC1) 
  set_property( TEST ZE4V_MC1 PROPERTY ENVIRONMENT "GFORTRAN_CONVERT_UNIT=big_endian") 

  add_test(NAME JZREAD_MC1 COMMAND  sh ${CMAKE_CURRENT_BINARY_DIR}/wrapper.sh ${CMAKE_CURRENT_SOURCE_DIR}/jzread1.card  ${JADEPREFIX}/bin/jzread )
  set_property( TEST JZREAD_MC1 PROPERTY  DEPENDS ZE4V_MC1) 
  set_property( TEST JZREAD_MC1 PROPERTY ENVIRONMENT "GFORTRAN_CONVERT_UNIT=native") 

  add_test(NAME JTJOB_MC1 COMMAND sh ${CMAKE_CURRENT_BINARY_DIR}/wrapper.sh ${CMAKE_CURRENT_SOURCE_DIR}/jtjob1.card  ${JADEPREFIX}/bin/jtjob)
  set_property( TEST JTJOB_MC1 PROPERTY  DEPENDS JZREAD_MC1) 
  set_property( TEST JTJOB_MC1 PROPERTY ENVIRONMENT "GFORTRAN_CONVERT_UNIT=native\;big_endian:2,22") 

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
