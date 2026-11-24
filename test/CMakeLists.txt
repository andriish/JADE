CMAKE_MINIMUM_REQUIRED(VERSION 3.10) #Because of ROOT
# CMP0219 (newer CMake than 4.3.0): macro arguments keep backslashes. The tests pass lists such as
# "GFORTRAN_CONVERT_UNIT=big_endian\;native:2" through jade_fortran_test and rely on the old
# behaviour (the backslash escapes the semicolon). Older CMake has no such policy and behaves as OLD.
if(POLICY CMP0219)
  cmake_policy(SET CMP0219 OLD)
endif()
PROJECT(JADETEST Fortran CXX C)
set(VERSION 2024.04)
include("GNUInstallDirs")
set(CMAKE_MODULE_PATH "${CMAKE_SOURCE_DIR}/../cmake/Modules/")
set(CMAKE_VERBOSE_MAKEFILE OFF)
########################################################################
#set(JADE_FORCE_32 OFF)
if (JADE_FORCE_32)
  list(PREPEND CMAKE_SYSTEM_LIBRARY_PATH "/usr/lib")
  message(STATUS "${CMAKE_SYSTEM_LIBRARY_PATH}")
  set( FIND_LIBRARY_USE_LIB32_PATHS  ON)
  set( FIND_LIBRARY_USE_LIB64_PATHS  OFF)
  set(CMAKE_SIZEOF_VOID_P 4)
  set (CMAKE_EXE_LINKER_FLAGS  "-m32 ${CMAKE_EXE_LINKER_FLAGS}")
  #To prevent eraling the suffix in case it is 32
  if (LIB_SUFFIX)
    if ( ${LIB_SUFFIX}  STREQUAL "64" )
      set (LIB_SUFFIX  )
    endif()
  endif()
endif()

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
  if (${CMAKE_Fortran_COMPILER_ID}  MATCHES  "PGI|NVHPC")
     # the NVIDIA runtime uses FORT_CONVERT_UNIT with the same syntax as GFORTRAN_CONVERT_UNIT
     string(REPLACE "GFORTRAN_CONVERT_UNIT" "FORT_CONVERT_UNIT" nvenv "${gnuenv}")
     set_property( TEST ${testname} PROPERTY ENVIRONMENT "${nvenv}")
  endif()
endmacro( jade_fortran_test )
########################################################################
find_package(JADESOFT 2000.0.0 REQUIRED)
find_package(CERNLIB REQUIRED)
find_package(LAPACK REQUIRED)
find_package(BLAS REQUIRED)
message(STATUS "JADE test: JADESOFT_CAL_DIR=${JADESOFT_CAL_DIR} JADESOFT_DATA_DIR=${JADESOFT_DATA_DIR}")
########################################################################
find_package(HepMC3  REQUIRED)
message(STATUS "JADE test: HEPMC3_LIBRARIES=${HEPMC3_LIBRARIES}")
########################################################################
find_package(Pythia8 8.300)
message(STATUS "JADE test: PYTHIA8_FOUND=${PYTHIA8_FOUND} PYTHIA8_VERSION=${PYTHIA8_VERSION}") 
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


# The bank names of the binary calibration files must have the byte order of the host, otherwise KLREAD
# ignores every record and no calibration constants are applied (see jadesoft/CMakeLists.txt, calcheck.f).
add_executable(calcheck ${CMAKE_CURRENT_SOURCE_DIR}/src/calcheck.f)
foreach(calfile aupdat1 bupdat0 bupdat1)
  add_test(NAME CALIB_NAMES_${calfile} COMMAND calcheck ${CMAKE_CURRENT_BINARY_DIR}/${calfile}.b)
  jade_fortran_test(CALIB_NAMES_${calfile} "GFORTRAN_CONVERT_UNIT=big_endian" "F_UFMTENDIAN=big" " ")
endforeach()

# Example analysis in Fortran: K0S -> pi+ pi- from the pairs of the tracks of opposite charges with close helices
# (src/k0s.f). It reads the BOS file of superv, so that it needs the libraries of the JADE software and CERNLIB.
add_executable(k0s ${CMAKE_CURRENT_SOURCE_DIR}/src/k0s.f)
set(K0S_LIBS ${JADESOFT_LIBRARIES_STATIC} ${CERNLIB_PACKLIB_STATIC} ${CERNLIB_KERNLIB_STATIC} ${CERNLIB_MATHLIB_STATIC} ${LAPACK_LIBRARIES} ${BLAS_LIBRARIES})
if(${CMAKE_SYSTEM_NAME} MATCHES "Darwin")
  target_link_libraries(k0s ${K0S_LIBS})
else()
  target_link_libraries(k0s -Wl,--start-group ${K0S_LIBS} -Wl,--end-group)
endif()

add_TEST(NAME FPTOBOS_DATA_1_CLEANUP COMMAND rm -f ${CMAKE_CURRENT_BINARY_DIR}/file006.bos)

add_test(NAME FPTOBOS_DATA_1 COMMAND sh ${CMAKE_CURRENT_BINARY_DIR}/wrapper.sh ${CMAKE_CURRENT_SOURCE_DIR}/cards/fptobos1.card  ${JADESOFT_BIN_DIR}/fptobos)
jade_fortran_test(FPTOBOS_DATA_1 "GFORTRAN_CONVERT_UNIT=native" "F_UFMTENDIAN=" " ") 
set_property( TEST FPTOBOS_DATA_1 PROPERTY  DEPENDS FPTOBOS_DATA_1_CLEANUP)

add_test(NAME SUPERV_DATA_1 COMMAND sh ${CMAKE_CURRENT_BINARY_DIR}/wrapper.sh ${CMAKE_CURRENT_SOURCE_DIR}/cards/supervfile006.card  ${JADESOFT_BIN_DIR}/superv ) 
set_property( TEST SUPERV_DATA_1 PROPERTY  DEPENDS FPTOBOS_DATA_1) 
jade_fortran_test(SUPERV_DATA_1 "GFORTRAN_CONVERT_UNIT=big_endian\;native:2" "F_UFMTENDIAN=big:1,3-100" " ") 
#NOTE: superv.stat needs a random number generator seeds. Otherwise it will use the same each time

# Prints the runs, the numbers of events and the CM energy of a BOS file (src/bosinfo.f)
set(BOSINFO_LIBS ${JADESOFT_LIBRARIES_STATIC} ${CERNLIB_PACKLIB_STATIC} ${CERNLIB_KERNLIB_STATIC} ${CERNLIB_MATHLIB_STATIC})
add_executable(bosinfo ${CMAKE_CURRENT_SOURCE_DIR}/src/bosinfo.f)
if (APPLE)
  target_link_libraries(bosinfo ${BOSINFO_LIBS})
else()
  target_link_libraries(bosinfo -Wl,--start-group ${BOSINFO_LIBS} -Wl,--end-group)
endif()
add_test(NAME BOSINFO_DATA_1 COMMAND bosinfo ${CMAKE_CURRENT_BINARY_DIR}/file006.bos)
set_property( TEST BOSINFO_DATA_1 PROPERTY  DEPENDS FPTOBOS_DATA_1)
set_property( TEST BOSINFO_DATA_1 PROPERTY  PASS_REGULAR_EXPRESSION "mean CM energy")
jade_fortran_test(BOSINFO_DATA_1 "GFORTRAN_CONVERT_UNIT=big_endian\;native:2" "F_UFMTENDIAN=big:2" "FORT_CONVERT2=big_ieee")

# The same for the raw FPACK file, read directly with the unpacking routines of fptobos (src/fpinfo.f)
set(FPINFO_UNP unpjad.f unpaff.f bytswp.f chasci.f cnvibm3e.f chebcd.f twoin1.f pripatr.f prihead.f prizvtx.f prizehd.f prize4v.f prilgcl.f prigvtx.f primur1.f primur2.f pritpev.f pritptr.f pritpvx.f pritagg.f)
list(TRANSFORM FPINFO_UNP PREPEND "${CMAKE_CURRENT_SOURCE_DIR}/../fptobos/src/")
add_executable(fpinfo ${CMAKE_CURRENT_SOURCE_DIR}/src/fpinfo.f ${FPINFO_UNP})
if (APPLE)
  target_link_libraries(fpinfo ${BOSINFO_LIBS})
else()
  target_link_libraries(fpinfo -Wl,--start-group ${BOSINFO_LIBS} -Wl,--end-group)
endif()
add_test(NAME FPINFO_DATA_1 COMMAND fpinfo ${CMAKE_CURRENT_SOURCE_DIR}/jadetapes/JAD073/file006)
set_property( TEST FPINFO_DATA_1 PROPERTY PASS_REGULAR_EXPRESSION "mean CM energy")
jade_fortran_test(FPINFO_DATA_1 "GFORTRAN_CONVERT_UNIT=native" "F_UFMTENDIAN=" " ")

# the same chain: the output of fpinfo (FPACK file) and bosinfo (BOS file made from it) must be equal
add_test(NAME INFO_COMPARE_DATA_1 COMMAND sh ${CMAKE_CURRENT_SOURCE_DIR}/info_compare.sh $<TARGET_FILE:fpinfo> ${CMAKE_CURRENT_SOURCE_DIR}/jadetapes/JAD073/file006 $<TARGET_FILE:bosinfo> ${CMAKE_CURRENT_BINARY_DIR}/file006.bos ${CMAKE_CURRENT_BINARY_DIR})
set_property( TEST INFO_COMPARE_DATA_1 PROPERTY DEPENDS FPTOBOS_DATA_1)

# Event display. Needs a virtual X server (xvfb-run, or JADEZ_TEST_DISPLAY set); skipped otherwise.
if (EXISTS ${JADESOFT_BIN_DIR}/jadez)
  add_test(NAME JADEZ_DATA_1 COMMAND bash ${CMAKE_CURRENT_SOURCE_DIR}/jadez_test.sh ${JADESOFT_BIN_DIR}/jadez
           ${JADESOFT_DATA_DIR}/F22BOW.HELP.DATASET ${CMAKE_CURRENT_BINARY_DIR})
  set_property( TEST JADEZ_DATA_1 PROPERTY DEPENDS FPTOBOS_DATA_1)
  set_property( TEST JADEZ_DATA_1 PROPERTY SKIP_RETURN_CODE 77)
  set_property( TEST JADEZ_DATA_1 PROPERTY TIMEOUT 700)
  jade_fortran_test(JADEZ_DATA_1 "GFORTRAN_CONVERT_UNIT=big_endian\;native:2" "F_UFMTENDIAN=big:1,3-100" " ")
endif()

add_test(NAME K0S_DATA_1 COMMAND k0s ${CMAKE_CURRENT_BINARY_DIR}/supervfile006.bos ${CMAKE_CURRENT_BINARY_DIR}/k0s_data_1.txt ${CMAKE_CURRENT_BINARY_DIR}/k0s_data_1_dist.txt ${CMAKE_CURRENT_SOURCE_DIR}/cards/k0s.card)
set_property( TEST K0S_DATA_1 PROPERTY  DEPENDS SUPERV_DATA_1)
jade_fortran_test(K0S_DATA_1 "GFORTRAN_CONVERT_UNIT=native\;big_endian:2" "F_UFMTENDIAN=big:2" "FORT_CONVERT2=big_ieee")

add_test(NAME ZE4V_DATA_1 COMMAND sh ${CMAKE_CURRENT_BINARY_DIR}/wrapper.sh ${CMAKE_CURRENT_SOURCE_DIR}/cards/ze4vfile006.card  ${JADESOFT_BIN_DIR}/ze4v)
set_property( TEST ZE4V_DATA_1 PROPERTY  DEPENDS SUPERV_DATA_1) 
jade_fortran_test(ZE4V_DATA_1 "GFORTRAN_CONVERT_UNIT=big_endian" "F_UFMTENDIAN=big" " ") 

add_test(NAME JZREAD_DATA_1 COMMAND  sh ${CMAKE_CURRENT_BINARY_DIR}/wrapper.sh ${CMAKE_CURRENT_SOURCE_DIR}/cards/jzreadfile006.card  ${JADESOFT_BIN_DIR}/jzread )
set_property( TEST JZREAD_DATA_1 PROPERTY  DEPENDS ZE4V_DATA_1) 
jade_fortran_test(JZREAD_DATA_1 PROPERTY ENVIRONMENT "GFORTRAN_CONVERT_UNIT=native" "F_UFMTENDIAN=" " ") 

# the table of the luminosity: the installed file lumi.dat (next to the calibration files, installed by jtuple)
unset(JADE_LUMI_DAT CACHE)
find_file(JADE_LUMI_DAT lumi.dat PATHS ${JADESOFT_DATA_DIR} NO_DEFAULT_PATH)
message(STATUS "JADE test: JADE_LUMI_DAT=${JADE_LUMI_DAT}")
if (EXISTS ${JADESOFT_BIN_DIR}/taklumi AND JADE_LUMI_DAT)
  add_test(NAME TAKLUMI_LUMITABLE_1 COMMAND bash ${CMAKE_CURRENT_SOURCE_DIR}/taklumi_test.sh ${JADESOFT_BIN_DIR}/taklumi ${JADE_LUMI_DAT} ${CMAKE_CURRENT_BINARY_DIR})
endif()
add_test(NAME JTJOB_DATA_1 COMMAND sh ${CMAKE_CURRENT_BINARY_DIR}/wrapper.sh ${CMAKE_CURRENT_SOURCE_DIR}/cards/jtjobfile006.card  ${JADESOFT_BIN_DIR}/jtjob)
set_property( TEST JTJOB_DATA_1 PROPERTY  DEPENDS JZREAD_DATA_1) 
jade_fortran_test(JTJOB_DATA_1 "GFORTRAN_CONVERT_UNIT=native\;big_endian:2,22" "F_UFMTENDIAN=big:2,22" "FORT_CONVERT2=big_ieee FORT_CONVERT22=big_ieee") 


# jade_h2root (installed with JADESOFT, see h2root/README.md) is preferred: the h2root of
# ROOT 6.40 crashes on 64 bit machines
find_program(H2ROOT NAMES jade_h2root h2root HINTS ${JADESOFT_BIN_DIR})
message(STATUS "JADE test: H2ROOT=${H2ROOT}")
find_package(ROOT COMPONENTS Core RIO Tree)
if (H2ROOT AND ROOT_FOUND)
  message(STATUS "JADE test: ROOT_INCLUDE_DIRS=${ROOT_INCLUDE_DIRS} ROOT_LIBRARIES=${ROOT_LIBRARIES}")
  add_executable(ROOT_analysis_1  ${CMAKE_CURRENT_SOURCE_DIR}/src/ROOT_analysis_1.cc)
  target_link_libraries(ROOT_analysis_1  ROOT::Core ROOT::RIO ROOT::Hist ROOT::Tree ROOT::Gui)
  add_executable(ROOT_analysis_2  ${CMAKE_CURRENT_SOURCE_DIR}/src/ROOT_analysis_2.cc ${CMAKE_CURRENT_SOURCE_DIR}/src/fjcore/fjcore.cc)
  target_link_libraries(ROOT_analysis_2  ROOT::Core ROOT::RIO ROOT::Hist ROOT::Tree ROOT::Gui)
else()
  message(STATUS "JADE test: h2root utility form the ROOT package or the ROOT package was not found. The utility is needed to convert the HBOOK files to ROOT. ROOT analyses will be disabled.") 
endif()

if (ROOT_FOUND AND H2ROOT)
  add_test(NAME H2ROOT_DATA_1 COMMAND ${H2ROOT}  jtjobfile006.hbook  jtjobfile006.root)
  set_property( TEST H2ROOT_DATA_1 PROPERTY  DEPENDS JTJOB_DATA_1)   
  add_test(NAME ROOT_ANALYSIS_DATA_1 COMMAND ${CMAKE_CURRENT_BINARY_DIR}/ROOT_analysis_1 jtjobfile006.root jtjobfile006_1.pdf "real data")
  set_property( TEST ROOT_ANALYSIS_DATA_1 PROPERTY  DEPENDS H2ROOT_DATA_1) 
  add_test(NAME ROOT_ANALYSIS_DATA_2 COMMAND ${CMAKE_CURRENT_BINARY_DIR}/ROOT_analysis_2 jtjobfile006.root jtjobfile006_2.pdf)
  set_property( TEST ROOT_ANALYSIS_DATA_2 PROPERTY  DEPENDS H2ROOT_DATA_1) 
endif()

if((NOT PYTHIA8_FOUND) OR (NOT HepMC3_FOUND))
  message(STATUS "JADE test: HepMC3 and/or Pythia8 package not found, was disabled or too old. The minimal supported Pythia8 version is 8.220. Please check if PYTHIA8_ROOT_DIR is set properly.") 
else()
  add_executable(pythia8_JADE  ${CMAKE_CURRENT_SOURCE_DIR}/src/pythia8_JADE.cc)
  target_link_libraries(pythia8_JADE  Pythia8::Pythia8  ${HEPMC3_LIB} ${HEPMC3_SEARCH_LIB}  )
  target_include_directories(pythia8_JADE PRIVATE ${HEPMC3_INCLUDE_DIR} ${PYTHIA8_INCLUDE_DIR}/Pythia8Plugins/ ${HEPMC3_INTERFACES_DIR}/pythia8/include/Pythia8/)
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
   
  add_test(NAME K0S_MC_1 COMMAND k0s ${CMAKE_CURRENT_BINARY_DIR}/superv1.bos ${CMAKE_CURRENT_BINARY_DIR}/k0s_mc_1.txt ${CMAKE_CURRENT_BINARY_DIR}/k0s_mc_1_dist.txt ${CMAKE_CURRENT_SOURCE_DIR}/cards/k0s.card)
  set_property( TEST K0S_MC_1 PROPERTY  DEPENDS SUPERV_MC_1)
  jade_fortran_test(K0S_MC_1 "GFORTRAN_CONVERT_UNIT=native\;big_endian:2" "F_UFMTENDIAN=big:2" "FORT_CONVERT2=big_ieee")

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

  # The same chain at another energy: Pythia8 events at 43.8 GeV (card testPythia2.input), the detector
  # configuration of 1985 (card mcjade3.card), second example of the analysis of simulated events
  add_test(NAME PYTHIA8_PRODUCE_HEPMC3_MC_3 COMMAND pythia8_JADE
                                         ${CMAKE_CURRENT_SOURCE_DIR}/cards/testPythia2.input
                                         ${CMAKE_CURRENT_BINARY_DIR}/testPythia2.output )

  add_test(NAME HEPMC3_TO_CPROD_MC_3 COMMAND ${JADESOFT_BIN_DIR}/convert_hepmc_to_jade -i hepmc3 -o jade
                                          ${CMAKE_CURRENT_BINARY_DIR}/testPythia2.output
                                          ${CMAKE_CURRENT_BINARY_DIR}/testConvert2.cprod
                                          -e Mode=0 )
  set_property( TEST HEPMC3_TO_CPROD_MC_3 PROPERTY DEPENDS PYTHIA8_PRODUCE_HEPMC3_MC_3)
  set_property( TEST HEPMC3_TO_CPROD_MC_3 PROPERTY ENVIRONMENT "LD_LIBRARY_PATH=${ROOT_DIR}/../lib:${ROOT_DIR}/../lib64:$ENV{LD_LIBRARY_PATH}")

  add_test(NAME MCJADE_MC_3 COMMAND sh ${CMAKE_CURRENT_BINARY_DIR}/wrapper.sh ${CMAKE_CURRENT_SOURCE_DIR}/cards/mcjade3.card  ${JADESOFT_BIN_DIR}/mcjade )
  set_property( TEST MCJADE_MC_3 PROPERTY DEPENDS HEPMC3_TO_CPROD_MC_3)
  jade_fortran_test(MCJADE_MC_3 "GFORTRAN_CONVERT_UNIT=native" "F_UFMTENDIAN=" " " )

  add_test(NAME SUPERV_MC_3 COMMAND sh ${CMAKE_CURRENT_BINARY_DIR}/wrapper.sh ${CMAKE_CURRENT_SOURCE_DIR}/cards/superv3.card  ${JADESOFT_BIN_DIR}/superv )
  set_property( TEST SUPERV_MC_3 PROPERTY  DEPENDS MCJADE_MC_3)
  jade_fortran_test(SUPERV_MC_3 "GFORTRAN_CONVERT_UNIT=big_endian\;native:2" "F_UFMTENDIAN=big:1,3-100" " " )

  add_test(NAME K0S_MC_3 COMMAND k0s ${CMAKE_CURRENT_BINARY_DIR}/superv3.bos ${CMAKE_CURRENT_BINARY_DIR}/k0s_mc_3.txt ${CMAKE_CURRENT_BINARY_DIR}/k0s_mc_3_dist.txt ${CMAKE_CURRENT_SOURCE_DIR}/cards/k0s.card)
  set_property( TEST K0S_MC_3 PROPERTY  DEPENDS SUPERV_MC_3)
  jade_fortran_test(K0S_MC_3 "GFORTRAN_CONVERT_UNIT=native\;big_endian:2" "F_UFMTENDIAN=big:2" "FORT_CONVERT2=big_ieee")

  add_test(NAME ZE4V_MC_3 COMMAND sh ${CMAKE_CURRENT_BINARY_DIR}/wrapper.sh ${CMAKE_CURRENT_SOURCE_DIR}/cards/ze4v3.card  ${JADESOFT_BIN_DIR}/ze4v)
  set_property( TEST ZE4V_MC_3 PROPERTY  DEPENDS SUPERV_MC_3)
  jade_fortran_test(ZE4V_MC_3 "GFORTRAN_CONVERT_UNIT=big_endian" "F_UFMTENDIAN=big" " ")

  add_test(NAME JZREAD_MC_3 COMMAND  sh ${CMAKE_CURRENT_BINARY_DIR}/wrapper.sh ${CMAKE_CURRENT_SOURCE_DIR}/cards/jzread3.card  ${JADESOFT_BIN_DIR}/jzread )
  set_property( TEST JZREAD_MC_3 PROPERTY  DEPENDS ZE4V_MC_3)
  jade_fortran_test(JZREAD_MC_3 PROPERTY ENVIRONMENT "GFORTRAN_CONVERT_UNIT=native" "F_UFMTENDIAN=" " ")

  add_test(NAME JTJOB_MC_3 COMMAND sh ${CMAKE_CURRENT_BINARY_DIR}/wrapper.sh ${CMAKE_CURRENT_SOURCE_DIR}/cards/jtjob3.card  ${JADESOFT_BIN_DIR}/jtjob)
  set_property( TEST JTJOB_MC_3 PROPERTY  DEPENDS JZREAD_MC_3)
  jade_fortran_test(JTJOB_MC_3 "GFORTRAN_CONVERT_UNIT=native\;big_endian:2,22" "F_UFMTENDIAN=big:2,22" "FORT_CONVERT2=big_ieee FORT_CONVERT22=big_ieee")

  if (ROOT_FOUND AND H2ROOT)
    add_test(NAME H2ROOT_MC_1 COMMAND ${H2ROOT}  jtjob1.hbook  jtjob1.root)
    set_property( TEST H2ROOT_MC_1 PROPERTY  DEPENDS JTJOB_MC_1)   

    add_test(NAME H2ROOT_MC_2 COMMAND ${H2ROOT}  jtjob2.hbook  jtjob2.root)
    set_property( TEST H2ROOT_MC_2 PROPERTY  DEPENDS JTJOB_MC_2)   

    add_test(NAME ROOT_ANALYSIS1_MC_1 COMMAND ${CMAKE_CURRENT_BINARY_DIR}/ROOT_analysis_1 jtjob1.root jtjob11.pdf "Pythia8 hadronic events")    
    set_property( TEST ROOT_ANALYSIS1_MC_1 PROPERTY  DEPENDS H2ROOT_MC_1) 
    
    add_test(NAME ROOT_ANALYSIS2_MC_1 COMMAND ${CMAKE_CURRENT_BINARY_DIR}/ROOT_analysis_2 jtjob1.root jtjob21.pdf)
    set_property( TEST ROOT_ANALYSIS2_MC_1 PROPERTY  DEPENDS H2ROOT_MC_1) 

    add_test(NAME ROOT_ANALYSIS1_MC_2 COMMAND ${CMAKE_CURRENT_BINARY_DIR}/ROOT_analysis_1 jtjob2.root jtjob12.pdf "PYTHIA 5.7 hadronic events")
    set_property( TEST ROOT_ANALYSIS1_MC_2 PROPERTY  DEPENDS H2ROOT_MC_2) 

    add_test(NAME ROOT_ANALYSIS2_MC_2 COMMAND ${CMAKE_CURRENT_BINARY_DIR}/ROOT_analysis_2 jtjob2.root jtjob22.pdf)
    set_property( TEST ROOT_ANALYSIS2_MC_2 PROPERTY  DEPENDS H2ROOT_MC_2) 


    add_test(NAME H2ROOT_MC_3 COMMAND ${H2ROOT}  jtjob3.hbook  jtjob3.root)
    set_property( TEST H2ROOT_MC_3 PROPERTY  DEPENDS JTJOB_MC_3)

    add_test(NAME ROOT_ANALYSIS1_MC_3 COMMAND ${CMAKE_CURRENT_BINARY_DIR}/ROOT_analysis_1 jtjob3.root jtjob13.pdf "Pythia8 hadronic events")
    set_property( TEST ROOT_ANALYSIS1_MC_3 PROPERTY  DEPENDS H2ROOT_MC_3)

    add_test(NAME ROOT_ANALYSIS2_MC_3 COMMAND ${CMAKE_CURRENT_BINARY_DIR}/ROOT_analysis_2 jtjob3.root jtjob23.pdf)
    set_property( TEST ROOT_ANALYSIS2_MC_3 PROPERTY  DEPENDS H2ROOT_MC_3)
  endif()
endif()

########################################################################
find_program( MEMORYCHECK_COMMAND valgrind )
if(MEMORYCHECK_COMMAND)
  message(STATUS "JADE test: valgrind package found in ${MEMORYCHECK_COMMAND}.  memory check tests enabled.") 
  set( MEMORYCHECK_COMMAND_OPTIONS "--trace-children=yes --leak-check=full --error-exitcode=1" )
else()
  message(STATUS "JADE test: valgrind package not found or disabled.  memory check tests disabled.") 
endif()
########################################################################

# Dummy generator of HepMC3 events e+ e- -> pi+ pi- (src/dummy_gen.cc: two particles back to back, the momentum is the beam
# energy), through the same chain as the other Monte Carlo samples: a simple sample for debugging (the momentum scale and the
# magnetic field of the reconstruction; k0s prints the mean momentum of the back-to-back pairs of tracks). It needs HepMC3 only.
if (HepMC3_FOUND)
  add_executable(dummy_gen ${CMAKE_CURRENT_SOURCE_DIR}/src/dummy_gen.cc)
  target_link_libraries(dummy_gen ${HEPMC3_LIB})
  target_include_directories(dummy_gen PRIVATE ${HEPMC3_INCLUDE_DIR})
  add_test(NAME DUMMYGEN_PRODUCE_HEPMC3_MC_5 COMMAND dummy_gen ${CMAKE_CURRENT_BINARY_DIR}/testDummy5.output 1000 34.6 211)
  set_property( TEST DUMMYGEN_PRODUCE_HEPMC3_MC_5 PROPERTY ENVIRONMENT "LD_LIBRARY_PATH=${ROOT_DIR}/../lib:${ROOT_DIR}/../lib64:$ENV{LD_LIBRARY_PATH}")

  add_test(NAME HEPMC3_TO_CPROD_MC_5 COMMAND ${JADESOFT_BIN_DIR}/convert_hepmc_to_jade -i hepmc3 -o jade
                                          ${CMAKE_CURRENT_BINARY_DIR}/testDummy5.output
                                          ${CMAKE_CURRENT_BINARY_DIR}/testConvert5.cprod
                                          -e Mode=0 )
  set_property( TEST HEPMC3_TO_CPROD_MC_5 PROPERTY DEPENDS DUMMYGEN_PRODUCE_HEPMC3_MC_5)
  set_property( TEST HEPMC3_TO_CPROD_MC_5 PROPERTY ENVIRONMENT "LD_LIBRARY_PATH=${ROOT_DIR}/../lib:${ROOT_DIR}/../lib64:$ENV{LD_LIBRARY_PATH}")

  add_test(NAME MCJADE_MC_5 COMMAND sh ${CMAKE_CURRENT_BINARY_DIR}/wrapper.sh ${CMAKE_CURRENT_SOURCE_DIR}/cards/mcjade5.card  ${JADESOFT_BIN_DIR}/mcjade )
  set_property( TEST MCJADE_MC_5 PROPERTY DEPENDS HEPMC3_TO_CPROD_MC_5)
  jade_fortran_test(MCJADE_MC_5 "GFORTRAN_CONVERT_UNIT=native" "F_UFMTENDIAN=" " " )

  add_test(NAME SUPERV_MC_5 COMMAND sh ${CMAKE_CURRENT_BINARY_DIR}/wrapper.sh ${CMAKE_CURRENT_SOURCE_DIR}/cards/superv5.card  ${JADESOFT_BIN_DIR}/superv )
  set_property( TEST SUPERV_MC_5 PROPERTY  DEPENDS MCJADE_MC_5)
  jade_fortran_test(SUPERV_MC_5 "GFORTRAN_CONVERT_UNIT=big_endian\;native:2" "F_UFMTENDIAN=big:1,3-100" " " )

  add_test(NAME K0S_MC_5 COMMAND k0s ${CMAKE_CURRENT_BINARY_DIR}/superv5.bos ${CMAKE_CURRENT_BINARY_DIR}/k0s_mc_5.txt ${CMAKE_CURRENT_BINARY_DIR}/k0s_mc_5_dist.txt ${CMAKE_CURRENT_SOURCE_DIR}/cards/k0s.card)
  set_property( TEST K0S_MC_5 PROPERTY DEPENDS SUPERV_MC_5)
  set_property( TEST K0S_MC_5 PROPERTY PASS_REGULAR_EXPRESSION "back-to-back pairs of two tracks")
  jade_fortran_test(K0S_MC_5 "GFORTRAN_CONVERT_UNIT=native\;big_endian:2" "F_UFMTENDIAN=big:2" "FORT_CONVERT2=big_ieee")

  # e+ e- -> K0S K0S at 14 GeV (a K0S decays to pi+ pi- after its flight, with the lifetime): the K0S analysis finds the peak at
  # the mass of the K0S (at 34.6 GeV the momentum of the K0S is high and the resolution of the mass of the pair is poor)
  add_test(NAME DUMMYGEN_PRODUCE_HEPMC3_MC_6 COMMAND dummy_gen ${CMAKE_CURRENT_BINARY_DIR}/testDummy6.output 1000 14 310)
  set_property( TEST DUMMYGEN_PRODUCE_HEPMC3_MC_6 PROPERTY ENVIRONMENT "LD_LIBRARY_PATH=${ROOT_DIR}/../lib:${ROOT_DIR}/../lib64:$ENV{LD_LIBRARY_PATH}")

  add_test(NAME HEPMC3_TO_CPROD_MC_6 COMMAND ${JADESOFT_BIN_DIR}/convert_hepmc_to_jade -i hepmc3 -o jade
                                          ${CMAKE_CURRENT_BINARY_DIR}/testDummy6.output
                                          ${CMAKE_CURRENT_BINARY_DIR}/testConvert6.cprod
                                          -e Mode=0 )
  set_property( TEST HEPMC3_TO_CPROD_MC_6 PROPERTY DEPENDS DUMMYGEN_PRODUCE_HEPMC3_MC_6)
  set_property( TEST HEPMC3_TO_CPROD_MC_6 PROPERTY ENVIRONMENT "LD_LIBRARY_PATH=${ROOT_DIR}/../lib:${ROOT_DIR}/../lib64:$ENV{LD_LIBRARY_PATH}")

  add_test(NAME MCJADE_MC_6 COMMAND sh ${CMAKE_CURRENT_BINARY_DIR}/wrapper.sh ${CMAKE_CURRENT_SOURCE_DIR}/cards/mcjade6.card  ${JADESOFT_BIN_DIR}/mcjade )
  set_property( TEST MCJADE_MC_6 PROPERTY DEPENDS HEPMC3_TO_CPROD_MC_6)
  jade_fortran_test(MCJADE_MC_6 "GFORTRAN_CONVERT_UNIT=native" "F_UFMTENDIAN=" " " )

  add_test(NAME SUPERV_MC_6 COMMAND sh ${CMAKE_CURRENT_BINARY_DIR}/wrapper.sh ${CMAKE_CURRENT_SOURCE_DIR}/cards/superv6.card  ${JADESOFT_BIN_DIR}/superv )
  set_property( TEST SUPERV_MC_6 PROPERTY  DEPENDS MCJADE_MC_6)
  jade_fortran_test(SUPERV_MC_6 "GFORTRAN_CONVERT_UNIT=big_endian\;native:2" "F_UFMTENDIAN=big:1,3-100" " " )

  add_test(NAME K0S_MC_6 COMMAND k0s ${CMAKE_CURRENT_BINARY_DIR}/superv6.bos ${CMAKE_CURRENT_BINARY_DIR}/k0s_mc_6.txt ${CMAKE_CURRENT_BINARY_DIR}/k0s_mc_6_dist.txt ${CMAKE_CURRENT_SOURCE_DIR}/cards/k0s.card)
  set_property( TEST K0S_MC_6 PROPERTY DEPENDS SUPERV_MC_6)
  set_property( TEST K0S_MC_6 PROPERTY PASS_REGULAR_EXPRESSION "pairs with 0.47 < m < 0.52 GeV")
  jade_fortran_test(K0S_MC_6 "GFORTRAN_CONVERT_UNIT=native\;big_endian:2" "F_UFMTENDIAN=big:2" "FORT_CONVERT2=big_ieee")
endif()

# Event display with the detector hardware (chamber boundary lines), result tables and
# orthogonal projections ON, on the Monte Carlo event reconstructed by SUPERV_MC_1.
if (TEST SUPERV_MC_1 AND EXISTS ${JADESOFT_BIN_DIR}/jadez)
  add_test(NAME JADEZ_MC_1 COMMAND bash ${CMAKE_CURRENT_SOURCE_DIR}/jadez_mc_test.sh ${JADESOFT_BIN_DIR}/jadez
           ${JADESOFT_DATA_DIR}/F22BOW.HELP.DATASET ${CMAKE_CURRENT_BINARY_DIR})
  set_property( TEST JADEZ_MC_1 PROPERTY DEPENDS SUPERV_MC_1)
  set_property( TEST JADEZ_MC_1 PROPERTY SKIP_RETURN_CODE 77)
  set_property( TEST JADEZ_MC_1 PROPERTY TIMEOUT 400)
endif()

# Event display matching the reference R-phi picture (doc/eventdisplayreferences/): hardware, tables,
# insets and the small dE/dx plot, without bank listing.
if (TEST SUPERV_MC_1 AND EXISTS ${JADESOFT_BIN_DIR}/jadez)
  add_test(NAME JADEZ_RPHI_1 COMMAND bash ${CMAKE_CURRENT_SOURCE_DIR}/jadez_rphi_test.sh ${JADESOFT_BIN_DIR}/jadez
           ${JADESOFT_DATA_DIR}/F22BOW.HELP.DATASET ${CMAKE_CURRENT_BINARY_DIR})
  set_property( TEST JADEZ_RPHI_1 PROPERTY DEPENDS SUPERV_MC_1)
  set_property( TEST JADEZ_RPHI_1 PROPERTY SKIP_RETURN_CODE 77)
  set_property( TEST JADEZ_RPHI_1 PROPERTY TIMEOUT 300)
endif()
