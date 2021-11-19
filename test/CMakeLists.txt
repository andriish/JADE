CMAKE_MINIMUM_REQUIRED(VERSION 3.5.0) #Because of ROOT
PROJECT(TEST Fortran CXX C)
SET(VERSION 2020.11)
include("GNUInstallDirs")
SET(CMAKE_MODULE_PATH "${CMAKE_SOURCE_DIR}/../cmake/Modules/")
set(CMAKE_VERBOSE_MAKEFILE OFF)
########################################################################
include(JADE_compiler_setup)
########################################################################
include(JADE_ROOT_setup)
########################################################################
macro( jade_fortran_test testname gnuenv  intelenv nagenv)
  if (${CMAKE_Fortran_COMPILER_ID}  MATCHES  "GNU")
     set_property( TEST ${testname} PROPERTY ENVIRONMENT "${gnuenv}") 
  endif()
  if (${CMAKE_Fortran_COMPILER_ID}  MATCHES  "Intel")
     set_property( TEST ${testname} PROPERTY ENVIRONMENT "${intelenv}") 
  endif()
  if (${CMAKE_Fortran_COMPILER_ID}  MATCHES  "NAG")
     set_property( TEST ${testname} PROPERTY ENVIRONMENT "${nagenv}") 
  endif()
endmacro( jade_fortran_test )

find_package(HepMC3  REQUIRED HINTS /u/andriish/Z/share/HepMC3/cmake/) #The later is to test JADE with NAG on Cobra
find_package(Pythia8 8.220)
ENABLE_TESTING()

configure_file(${CMAKE_CURRENT_SOURCE_DIR}/wrapper.sh.in      ${CMAKE_CURRENT_BINARY_DIR}/wrapper.sh)
configure_file(${CMAKE_CURRENT_SOURCE_DIR}/../jadesoft/cal/aupdat1      ${CMAKE_CURRENT_BINARY_DIR}/aupdat1 COPYONLY)
configure_file(${CMAKE_CURRENT_SOURCE_DIR}/../jadesoft/cal/aupdat1.b    ${CMAKE_CURRENT_BINARY_DIR}/aupdat1.b COPYONLY)
configure_file(${CMAKE_CURRENT_SOURCE_DIR}/../jadesoft/cal/bupdat0      ${CMAKE_CURRENT_BINARY_DIR}/bupdat0 COPYONLY)
configure_file(${CMAKE_CURRENT_SOURCE_DIR}/../jadesoft/cal/bupdat0.b    ${CMAKE_CURRENT_BINARY_DIR}/bupdat0.b COPYONLY)
configure_file(${CMAKE_CURRENT_SOURCE_DIR}/../jadesoft/cal/bupdat1      ${CMAKE_CURRENT_BINARY_DIR}/bupdat1 COPYONLY)
configure_file(${CMAKE_CURRENT_SOURCE_DIR}/../jadesoft/cal/bupdat1.b    ${CMAKE_CURRENT_BINARY_DIR}/bupdat1.b COPYONLY)
configure_file(${CMAKE_CURRENT_SOURCE_DIR}/jadetapes/JAD073/file006    ${CMAKE_CURRENT_BINARY_DIR}/jadetapes/JAD073/file006 COPYONLY)


add_test(NAME FPTOBOS_1_CLEANUP COMMAND rm -f ${CMAKE_CURRENT_BINARY_DIR}/file006.bos)
add_test(NAME FPTOBOS_1 COMMAND sh ${CMAKE_CURRENT_BINARY_DIR}/wrapper.sh ${CMAKE_CURRENT_SOURCE_DIR}/cards/fptobos1.card  ${JADEPREFIX}/bin/fptobos)
jade_fortran_test(FPTOBOS_1 "GFORTRAN_CONVERT_UNIT=native" "F_UFMTENDIAN=" " ") 
set_property( TEST FPTOBOS_1 PROPERTY  DEPENDS FPTOBOS_1_CLEANUP)

#add_test(NAME SUPERV_DATA1 COMMAND sh ${CMAKE_CURRENT_BINARY_DIR}/wrapper.sh ${CMAKE_CURRENT_SOURCE_DIR}/cards/supervfile006.card  ${JADEPREFIX}/bin/superv ) 
#set_property( TEST SUPERV_DATA1 PROPERTY  DEPENDS FPTOBOS_1) 
#jade_fortran_test(SUPERV_DATA1 "GFORTRAN_CONVERT_UNIT=big_endian\;native:2" "F_UFMTENDIAN=big:1,3-100" " ") 
#NOTE: superv.stat needs a random number generator seeds. Otherwise it will use the same each time


if(NOT PYTHIA8_FOUND)
  message(STATUS "JADE test: Pythia8 package not found, was disabled or too old. The minimal supported Pythia8 version is 8.220. Please check if PYTHIA8_ROOT_DIR is set properly.") 

else()
  message(STATUS "JADE test: Pythia8 package found. ${PYTHIA8_LIBRARIES} ${PYTHIA8_VERSION}") 
  add_executable(pythia8_JADE.exe  ${CMAKE_CURRENT_SOURCE_DIR}/src/pythia8_JADE.cc)
  target_link_libraries(pythia8_JADE.exe  ${PYTHIA8_LIBRARIES}  ${HEPMC3_LIB} ${HEPMC3_SEARCH_LIB}  )
  target_include_directories(pythia8_JADE.exe PRIVATE ${HEPMC3_INCLUDE_DIR} ${PYTHIA8_INCLUDE_DIR}/Pythia8Plugins/ ${HEPMC3_INTERFACES_DIR}/pythia8/include/Pythia8/)
  add_test(NAME pythia8_produce_HepMC3_MC1 COMMAND pythia8_JADE.exe 
                                         ${CMAKE_CURRENT_SOURCE_DIR}/cards/testPythia1.input 
                                         ${CMAKE_CURRENT_BINARY_DIR}/testPythia1.output )
 
  add_test(NAME HepMC3_to_CPROD_MC1 COMMAND ${JADEPREFIX}/bin/convert_mc_JADE.exe -i hepmc3 -o jade 
                                          ${CMAKE_CURRENT_BINARY_DIR}/testPythia1.output 
                                          ${CMAKE_CURRENT_BINARY_DIR}/testConvert1.cprod
                                          -e Mode=0 )
  set_property( TEST HepMC3_to_CPROD_MC1 PROPERTY DEPENDS pythia8_produce_HepMC3_MC1)   
  set_property( TEST HepMC3_to_CPROD_MC1 PROPERTY ENVIRONMENT "LD_LIBRARY_PATH=${ROOT_DIR}/../lib:${ROOT_DIR}/../lib64:$ENV{LD_LIBRARY_PATH}")   
  
  add_test(NAME MCJADE_MC1 COMMAND sh ${CMAKE_CURRENT_BINARY_DIR}/wrapper.sh ${CMAKE_CURRENT_SOURCE_DIR}/cards/mcjade1.card  ${JADEPREFIX}/bin/mcjade )
  set_property( TEST MCJADE_MC1 PROPERTY DEPENDS HepMC3_to_CPROD_MC1)   
  jade_fortran_test(MCJADE_MC1 "GFORTRAN_CONVERT_UNIT=native" "F_UFMTENDIAN=" " " )

  add_test(NAME SUPERV_MC1 COMMAND sh ${CMAKE_CURRENT_BINARY_DIR}/wrapper.sh ${CMAKE_CURRENT_SOURCE_DIR}/cards/superv1.card  ${JADEPREFIX}/bin/superv ) 
  set_property( TEST SUPERV_MC1 PROPERTY  DEPENDS CPROD_BOS_MC1) 
  jade_fortran_test(SUPERV_MC1 "GFORTRAN_CONVERT_UNIT=big_endian\;native:2" "F_UFMTENDIAN=big:1,3-100" " " ) 
  #NOTE: superv.stat needs a random number generator seeds. Otherwise it will use the same each time
   
  add_test(NAME ZE4V_MC1 COMMAND sh ${CMAKE_CURRENT_BINARY_DIR}/wrapper.sh ${CMAKE_CURRENT_SOURCE_DIR}/cards/ze4v1.card  ${JADEPREFIX}/bin/ze4v)
  set_property( TEST ZE4V_MC1 PROPERTY  DEPENDS SUPERV_MC1) 
  jade_fortran_test(ZE4V_MC1 "GFORTRAN_CONVERT_UNIT=big_endian" "F_UFMTENDIAN=big" " ") 

  add_test(NAME JZREAD_MC1 COMMAND  sh ${CMAKE_CURRENT_BINARY_DIR}/wrapper.sh ${CMAKE_CURRENT_SOURCE_DIR}/cards/jzread1.card  ${JADEPREFIX}/bin/jzread )
  set_property( TEST JZREAD_MC1 PROPERTY  DEPENDS ZE4V_MC1) 
  jade_fortran_test(JZREAD_MC1 PROPERTY ENVIRONMENT "GFORTRAN_CONVERT_UNIT=native" "F_UFMTENDIAN=" " ") 

  add_test(NAME JTJOB_MC1 COMMAND sh ${CMAKE_CURRENT_BINARY_DIR}/wrapper.sh ${CMAKE_CURRENT_SOURCE_DIR}/cards/jtjob1.card  ${JADEPREFIX}/bin/jtjob)
  set_property( TEST JTJOB_MC1 PROPERTY  DEPENDS JZREAD_MC1) 
  jade_fortran_test(JTJOB_MC1 "GFORTRAN_CONVERT_UNIT=native\;big_endian:2,22" "F_UFMTENDIAN=big:2,22" "FORT_CONVERT2=big_ieee FORT_CONVERT22=big_ieee") 

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
