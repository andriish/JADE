CMAKE_MINIMUM_REQUIRED(VERSION 3.5.0) #Because of ROOT
PROJECT(JADETEST Fortran CXX C)
set(VERSION 2022.08)
include("GNUInstallDirs")
set(CMAKE_MODULE_PATH "${CMAKE_SOURCE_DIR}/../cmake/Modules/")
set(CMAKE_VERBOSE_MAKEFILE OFF)
########################################################################
set(JADE_FORCE_32 OFF)
include(JADE_compiler_setup)
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
########################################################################
find_package(JADESOFT REQUIRED)
message(STATUS "JADE test: JADESOFT_CAL_DIR=${JADESOFT_CAL_DIR}")
########################################################################
find_package(HepMC3  REQUIRED)
message(STATUS "JADE test: HEPMC3_LIBRARIES=${HEPMC3_LIBRARIES}")
########################################################################
find_package(Pythia8 8.220)
message(STATUS "JADE test: PYTHIA8_LIBRARIES=${PYTHIA8_LIBRARIES} PYTHIA8_VERSION=${PYTHIA8_VERSION}") 
########################################################################
ENABLE_TESTING()

configure_file(${CMAKE_CURRENT_SOURCE_DIR}/wrapper.sh.in      ${CMAKE_CURRENT_BINARY_DIR}/wrapper.sh)
configure_file(${JADESOFT_CAL_DIR}/aupdat1      ${CMAKE_CURRENT_BINARY_DIR}/aupdat1 COPYONLY)
configure_file(${JADESOFT_CAL_DIR}/aupdat1.b    ${CMAKE_CURRENT_BINARY_DIR}/aupdat1.b COPYONLY)
configure_file(${JADESOFT_CAL_DIR}/bupdat0      ${CMAKE_CURRENT_BINARY_DIR}/bupdat0 COPYONLY)
configure_file(${JADESOFT_CAL_DIR}/bupdat0.b    ${CMAKE_CURRENT_BINARY_DIR}/bupdat0.b COPYONLY)
configure_file(${JADESOFT_CAL_DIR}/bupdat1      ${CMAKE_CURRENT_BINARY_DIR}/bupdat1 COPYONLY)
configure_file(${JADESOFT_CAL_DIR}/bupdat1.b    ${CMAKE_CURRENT_BINARY_DIR}/bupdat1.b COPYONLY)
configure_file(${CMAKE_CURRENT_SOURCE_DIR}/jadetapes/JAD073/file006    ${CMAKE_CURRENT_BINARY_DIR}/jadetapes/JAD073/file006 COPYONLY)


add_TEST(NAME FPTOBOS_DATA_1_CLEANUP COMMAND rm -f ${CMAKE_CURRENT_BINARY_DIR}/file006.bos)

add_test(NAME FPTOBOS_DATA_1 COMMAND sh ${CMAKE_CURRENT_BINARY_DIR}/wrapper.sh ${CMAKE_CURRENT_SOURCE_DIR}/cards/fptobos1.card  ${JADESOFT_BIN_DIR}/fptobos)
jade_fortran_test(FPTOBOS_DATA_1 "GFORTRAN_CONVERT_UNIT=native" "F_UFMTENDIAN=" " ") 
set_property( TEST FPTOBOS_DATA_1 PROPERTY  DEPENDS FPTOBOS_DATA_1_CLEANUP)

add_test(NAME SUPERV_DATA_1 COMMAND sh ${CMAKE_CURRENT_BINARY_DIR}/wrapper.sh ${CMAKE_CURRENT_SOURCE_DIR}/cards/supervfile006.card  ${JADESOFT_BIN_DIR}/superv ) 
set_property( TEST SUPERV_DATA_1 PROPERTY  DEPENDS FPTOBOS_DATA_1) 
jade_fortran_test(SUPERV_DATA_1 "GFORTRAN_CONVERT_UNIT=big_endian\;native:2" "F_UFMTENDIAN=big:1,3-100" " ") 
#NOTE: superv.stat needs a random number generator seeds. Otherwise it will use the same each time

add_test(NAME ZE4V_DATA_1 COMMAND sh ${CMAKE_CURRENT_BINARY_DIR}/wrapper.sh ${CMAKE_CURRENT_SOURCE_DIR}/cards/ze4vfile006.card  ${JADESOFT_BIN_DIR}/ze4v)
set_property( TEST ZE4V_DATA_1 PROPERTY  DEPENDS SUPERV_DATA_1) 
jade_fortran_test(ZE4V_DATA_1 "GFORTRAN_CONVERT_UNIT=big_endian" "F_UFMTENDIAN=big" " ") 

add_test(NAME JZREAD_DATA_1 COMMAND  sh ${CMAKE_CURRENT_BINARY_DIR}/wrapper.sh ${CMAKE_CURRENT_SOURCE_DIR}/cards/jzreadfile006.card  ${JADESOFT_BIN_DIR}/jzread )
set_property( TEST JZREAD_DATA_1 PROPERTY  DEPENDS ZE4V_DATA_1) 
jade_fortran_test(JZREAD_DATA_1 PROPERTY ENVIRONMENT "GFORTRAN_CONVERT_UNIT=native" "F_UFMTENDIAN=" " ") 

add_test(NAME JTJOB_DATA_1 COMMAND sh ${CMAKE_CURRENT_BINARY_DIR}/wrapper.sh ${CMAKE_CURRENT_SOURCE_DIR}/cards/jtjobfile006.card  ${JADESOFT_BIN_DIR}/jtjob)
set_property( TEST JTJOB_DATA_1 PROPERTY  DEPENDS JZREAD_DATA_1) 
jade_fortran_test(JTJOB_DATA_1 "GFORTRAN_CONVERT_UNIT=native\;big_endian:2,22" "F_UFMTENDIAN=big:2,22" "FORT_CONVERT2=big_ieee FORT_CONVERT22=big_ieee") 




find_program(H2ROOT NAMES h2root)
find_package(ROOT COMPONENTS Core RIO Tree)
if (H2ROOT AND ROOT_FOUND)
  message(STATUS "JADE test: ROOT_INCLUDE_DIRS=${ROOT_INCLUDE_DIRS} ROOT_LIBRARIES=${ROOT_LIBRARIES}")
  add_executable(ROOT_analysis_1  ${CMAKE_CURRENT_SOURCE_DIR}/src/ROOT_analysis_1.cc)
  target_link_libraries(ROOT_analysis_1  ${ROOT_LIBRARIES}  )
  target_include_directories(ROOT_analysis_1 PRIVATE ${ROOT_INCLUDE_DIRS} )
  add_executable(ROOT_analysis_2  ${CMAKE_CURRENT_SOURCE_DIR}/src/ROOT_analysis_2.cc ${CMAKE_CURRENT_SOURCE_DIR}/src/fjcore/fjcore.cc)
  target_link_libraries(ROOT_analysis_2  ${ROOT_LIBRARIES}  )
  target_include_directories(ROOT_analysis_2 PRIVATE ${ROOT_INCLUDE_DIRS} )

else()
  message(STATUS "JADE test: h2root utility form the ROOT package or the ROOT package was not found. The utility is needed to convert the HBOOK files to ROOT. ROOT analyses will be disabled.") 
endif()

if (ROOT_FOUND AND H2ROOT)
  add_test(NAME H2ROOT_DATA_1 COMMAND ${H2ROOT}  jtjobfile006.hbook  jtjobfile006.root)
  set_property( TEST H2ROOT_DATA_1 PROPERTY  DEPENDS JTJOB_DATA_1)   
  add_test(NAME ROOT_ANALYSIS_DATA_1 COMMAND ${CMAKE_CURRENT_BINARY_DIR}/ROOT_analysis_1 jtjobfile006.root jtjobfile006_1.pdf)
  set_property( TEST ROOT_ANALYSIS_DATA_1 PROPERTY  DEPENDS H2ROOT_DATA_1) 
  add_test(NAME ROOT_ANALYSIS_DATA_2 COMMAND ${CMAKE_CURRENT_BINARY_DIR}/ROOT_analysis_2 jtjobfile006.root jtjobfile006_2.pdf)
  set_property( TEST ROOT_ANALYSIS_DATA_2 PROPERTY  DEPENDS H2ROOT_DATA_1) 
endif()

if((NOT PYTHIA8_FOUND) OR (NOT HepMC3_FOUND))
  message(STATUS "JADE test: HepMC3 and/or Pythia8 package not found, was disabled or too old. The minimal supported Pythia8 version is 8.220. Please check if PYTHIA8_ROOT_DIR is set properly.") 
else()
  add_executable(pythia8_JADE  ${CMAKE_CURRENT_SOURCE_DIR}/src/pythia8_JADE.cc)
  target_link_libraries(pythia8_JADE  ${PYTHIA8_LIBRARIES}  ${HEPMC3_LIB} ${HEPMC3_SEARCH_LIB}  )
  target_include_directories(pythia8_JADE PRIVATE ${HEPMC3_INCLUDE_DIR}  ${PYTHIA8_INCLUDE_DIR} ${PYTHIA8_INCLUDE_DIR}/Pythia8Plugins/ ${HEPMC3_INTERFACES_DIR}/pythia8/include/Pythia8/)
  add_test(NAME PYTHIA8_PRODUCE_HEPMC3_MC_1 COMMAND pythia8_JADE 
                                         ${CMAKE_CURRENT_SOURCE_DIR}/cards/testPythia1.input 
                                         ${CMAKE_CURRENT_BINARY_DIR}/testPythia1.output )
 
  add_test(NAME HEPMC3_TO_CPROD_MC_1 COMMAND ${JADESOFT_BIN_DIR}/convert_hepmc_to_jade -i hepmc3 -o jade 
                                          ${CMAKE_CURRENT_BINARY_DIR}/testPythia1.output 
                                          ${CMAKE_CURRENT_BINARY_DIR}/testConvert1.cprod
                                          -e Mode=0 )
  set_property( TEST HEPMC3_TO_CPROD_MC_1 PROPERTY DEPENDS PYTHIA8_PRODUCE_HEPMC3_MC_1)   
  set_property( TEST HEPMC3_TO_CPROD_MC_1 PROPERTY ENVIRONMENT "LD_LIBRARY_PATH=${ROOT_DIR}/../lib:${ROOT_DIR}/../lib64:$ENV{LD_LIBRARY_PATH}")   
  
  add_test(NAME MCJADE_MC_1 COMMAND sh ${CMAKE_CURRENT_BINARY_DIR}/wrapper.sh ${CMAKE_CURRENT_SOURCE_DIR}/cards/mcjade1.card  ${JADESOFT_BIN_DIR}/mcjade )
  set_property( TEST MCJADE_MC_1 PROPERTY DEPENDS HEPMC3_TO_CPROD_MC_1)   
  jade_fortran_test(MCJADE_MC_1 "GFORTRAN_CONVERT_UNIT=native" "F_UFMTENDIAN=" " " )

  add_test(NAME SUPERV_MC_1 COMMAND sh ${CMAKE_CURRENT_BINARY_DIR}/wrapper.sh ${CMAKE_CURRENT_SOURCE_DIR}/cards/superv1.card  ${JADESOFT_BIN_DIR}/superv ) 
  set_property( TEST SUPERV_MC_1 PROPERTY  DEPENDS HEPMC3_TO_CPROD_MC_1) 
  jade_fortran_test(SUPERV_MC_1 "GFORTRAN_CONVERT_UNIT=big_endian\;native:2" "F_UFMTENDIAN=big:1,3-100" " " ) 
  #NOTE: superv.stat needs a random number generator seeds. Otherwise it will use the same each time
   
  add_test(NAME ZE4V_MC_1 COMMAND sh ${CMAKE_CURRENT_BINARY_DIR}/wrapper.sh ${CMAKE_CURRENT_SOURCE_DIR}/cards/ze4v1.card  ${JADESOFT_BIN_DIR}/ze4v)
  set_property( TEST ZE4V_MC_1 PROPERTY  DEPENDS SUPERV_MC_1) 
  jade_fortran_test(ZE4V_MC_1 "GFORTRAN_CONVERT_UNIT=big_endian" "F_UFMTENDIAN=big" " ") 

  add_test(NAME JZREAD_MC_1 COMMAND  sh ${CMAKE_CURRENT_BINARY_DIR}/wrapper.sh ${CMAKE_CURRENT_SOURCE_DIR}/cards/jzread1.card  ${JADESOFT_BIN_DIR}/jzread )
  set_property( TEST JZREAD_MC_1 PROPERTY  DEPENDS ZE4V_MC_1) 
  jade_fortran_test(JZREAD_MC_1 PROPERTY ENVIRONMENT "GFORTRAN_CONVERT_UNIT=native" "F_UFMTENDIAN=" " ") 

  add_test(NAME JTJOB_MC_1 COMMAND sh ${CMAKE_CURRENT_BINARY_DIR}/wrapper.sh ${CMAKE_CURRENT_SOURCE_DIR}/cards/jtjob1.card  ${JADESOFT_BIN_DIR}/jtjob)
  set_property( TEST JTJOB_MC_1 PROPERTY  DEPENDS JZREAD_MC_1) 
  jade_fortran_test(JTJOB_MC_1 "GFORTRAN_CONVERT_UNIT=native\;big_endian:2,22" "F_UFMTENDIAN=big:2,22" "FORT_CONVERT2=big_ieee FORT_CONVERT22=big_ieee") 



  add_test(NAME PY57_MC_1_CLEANUP COMMAND rm -f ${CMAKE_CURRENT_BINARY_DIR}/py57.cprod ${CMAKE_CURRENT_BINARY_DIR}/py57.hist  ${CMAKE_CURRENT_BINARY_DIR}/mcrnset.dat  ${CMAKE_CURRENT_BINARY_DIR}/mcrncnt.dat)

  add_test(NAME PY57_MC_1 COMMAND sh ${CMAKE_CURRENT_BINARY_DIR}/wrapper.sh ${CMAKE_CURRENT_SOURCE_DIR}/cards/py57.card  ${JADESOFT_BIN_DIR}/py57)
  set_property(TEST PY57_MC_1 PROPERTY  DEPENDS PY57_MC_1_CLEANUP) 
  jade_fortran_test(PY57_MC_1 "GFORTRAN_CONVERT_UNIT=native" "F_UFMTENDIAN=" " ") 

  add_test(NAME MCJADE_MC_2 COMMAND sh ${CMAKE_CURRENT_BINARY_DIR}/wrapper.sh ${CMAKE_CURRENT_SOURCE_DIR}/cards/mcjade2.card  ${JADESOFT_BIN_DIR}/mcjade )
  set_property( TEST MCJADE_MC_2 PROPERTY DEPENDS PY57_MC_1)   
  jade_fortran_test(MCJADE_MC_2 "GFORTRAN_CONVERT_UNIT=native" "F_UFMTENDIAN=" " " )

  add_test(NAME SUPERV_MC_2 COMMAND sh ${CMAKE_CURRENT_BINARY_DIR}/wrapper.sh ${CMAKE_CURRENT_SOURCE_DIR}/cards/superv2.card  ${JADESOFT_BIN_DIR}/superv ) 
  set_property( TEST SUPERV_MC_2 PROPERTY  DEPENDS MCJADE_MC_2) 
  jade_fortran_test(SUPERV_MC_2 "GFORTRAN_CONVERT_UNIT=big_endian\;native:2" "F_UFMTENDIAN=big:1,3-100" " " ) 
  #NOTE: superv.stat needs a random number generator seeds. Otherwise it will use the same each time
   
  add_test(NAME ZE4V_MC_2 COMMAND sh ${CMAKE_CURRENT_BINARY_DIR}/wrapper.sh ${CMAKE_CURRENT_SOURCE_DIR}/cards/ze4v2.card  ${JADESOFT_BIN_DIR}/ze4v)
  set_property( TEST ZE4V_MC_2 PROPERTY  DEPENDS SUPERV_MC_2) 
  jade_fortran_test(ZE4V_MC_2 "GFORTRAN_CONVERT_UNIT=big_endian" "F_UFMTENDIAN=big" " ") 

  add_test(NAME JZREAD_MC_2 COMMAND  sh ${CMAKE_CURRENT_BINARY_DIR}/wrapper.sh ${CMAKE_CURRENT_SOURCE_DIR}/cards/jzread2.card  ${JADESOFT_BIN_DIR}/jzread )
  set_property( TEST JZREAD_MC_2 PROPERTY  DEPENDS ZE4V_MC_2) 
  jade_fortran_test(JZREAD_MC_2 PROPERTY ENVIRONMENT "GFORTRAN_CONVERT_UNIT=native" "F_UFMTENDIAN=" " ") 

  add_test(NAME JTJOB_MC_2 COMMAND sh ${CMAKE_CURRENT_BINARY_DIR}/wrapper.sh ${CMAKE_CURRENT_SOURCE_DIR}/cards/jtjob2.card  ${JADESOFT_BIN_DIR}/jtjob)
  set_property( TEST JTJOB_MC_2 PROPERTY  DEPENDS JZREAD_MC_2) 
  jade_fortran_test(JTJOB_MC_2 "GFORTRAN_CONVERT_UNIT=native\;big_endian:2,22" "F_UFMTENDIAN=big:2,22" "FORT_CONVERT2=big_ieee FORT_CONVERT22=big_ieee") 





  if (ROOT_FOUND AND H2ROOT)
    add_test(NAME H2ROOT_MC_1 COMMAND ${H2ROOT}  jtjob1.hbook  jtjob1.root)
    set_property( TEST H2ROOT_MC_1 PROPERTY  DEPENDS JTJOB_MC_1)   

    add_test(NAME H2ROOT_MC_2 COMMAND ${H2ROOT}  jtjob2.hbook  jtjob2.root)
    set_property( TEST H2ROOT_MC_2 PROPERTY  DEPENDS JTJOB_MC_2)   

    
    add_test(NAME ROOT_ANALYSIS1_MC_1 COMMAND ${CMAKE_CURRENT_BINARY_DIR}/ROOT_analysis_1 jtjob1.root jtjob11.pdf)    
    set_property( TEST ROOT_ANALYSIS1_MC_1 PROPERTY  DEPENDS H2ROOT_MC_1) 
    
    add_test(NAME ROOT_ANALYSIS2_MC_1 COMMAND ${CMAKE_CURRENT_BINARY_DIR}/ROOT_analysis_2 jtjob1.root jtjob21.pdf)
    set_property( TEST ROOT_ANALYSIS2_MC_1 PROPERTY  DEPENDS H2ROOT_MC_1) 

    add_test(NAME ROOT_ANALYSIS1_MC_2 COMMAND ${CMAKE_CURRENT_BINARY_DIR}/ROOT_analysis_1 jtjob2.root jtjob12.pdf)
    set_property( TEST ROOT_ANALYSIS1_MC_2 PROPERTY  DEPENDS H2ROOT_MC_2) 

    add_test(NAME ROOT_ANALYSIS2_MC_2 COMMAND ${CMAKE_CURRENT_BINARY_DIR}/ROOT_analysis_2 jtjob2.root jtjob22.pdf)
    set_property( TEST ROOT_ANALYSIS2_MC_2 PROPERTY  DEPENDS H2ROOT_MC_2) 


  endif()
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
