# Example: the chain fptobos -> superv -> ze4v -> jzread -> jtjob -> h2root -> ROOT_analysis_1 on a JADE data tape.
#   fptobos converts the tape to the BOS format, superv reconstructs the events, ze4v and jzread compress and read them,
#   jtjob makes an HBOOK ntuple, jade_h2root converts it to a ROOT file, and ROOT_analysis_1 (ROOT_analysis_1.cc, a
#   TSelector, jadeanalysis.h) makes the pictures of the thrust (1-T) and of the energies of the tracks and of the lead
#   glass clusters. The programs read their cards from the standard input, in the directory <build>/run.
#
#   cmake -S examples/root_analysis -B build -DCMAKE_PREFIX_PATH=<install prefix> [-DJADE_EXAMPLE_DATA=<directory with the tapes>]
#   cmake --build build            # ROOT_analysis_1
#   ctest --test-dir build         # the chain, the results are in build/run
#
# The data are not part of the example: JADE_EXAMPLE_DATA is the directory with the FPACK tapes (default: the sample
# data of the installed test project, JADE_TEST_DIR/jadetapes), JADE_EXAMPLE_TAPE the tape in it. The package JADE is
# required: the compilers, the flags and the number of bits are those of the installation (JADEConfig.cmake). Fortran is
# enabled, although only C++ is compiled: the environment of the Fortran programs of the chain depends on the compiler.
cmake_minimum_required(VERSION 3.24)
set(JADE_NO_TARGETS ON)
find_package(JADE 2000.0.0 CONFIG REQUIRED)
unset(JADE_NO_TARGETS)
jade_use_toolchain()
project(JADEROOTEXAMPLE Fortran C CXX)
jade_use_flags()
find_package(JADE 2000.0.0 CONFIG REQUIRED)
find_package(ROOT REQUIRED COMPONENTS Core RIO Tree)
enable_testing()

# the data: a directory, not part of the example
set(JADE_EXAMPLE_DATA "${JADE_TEST_DIR}/jadetapes" CACHE PATH "Directory with the data (FPACK tapes)")
set(JADE_EXAMPLE_TAPE "JAD073/file006" CACHE STRING "The tape, relative to JADE_EXAMPLE_DATA")
set(JADE_EXAMPLE_TAPE_PATH "${JADE_EXAMPLE_DATA}/${JADE_EXAMPLE_TAPE}")
if(NOT EXISTS "${JADE_EXAMPLE_TAPE_PATH}")
  message(FATAL_ERROR "The data tape ${JADE_EXAMPLE_TAPE_PATH} does not exist: give the directory with "
                      "-DJADE_EXAMPLE_DATA=<directory> (and -DJADE_EXAMPLE_TAPE=<file in it>)")
endif()
message(STATUS "JADE example root_analysis: data ${JADE_EXAMPLE_TAPE_PATH}")

# The first field of the card of fptobos (IBYTSW) switches the byte swapping of the words of the FPACK file (big-endian
# words) on: right for a little-endian host; on a big-endian host the words must not be swapped.
if(JADE_BIG_ENDIAN)
  set(JADE_IBYTSW_FIELD "         0")
else()
  set(JADE_IBYTSW_FIELD "         1")
endif()

# the analysis program
add_executable(ROOT_analysis_1 ROOT_analysis_1.cc)
target_link_libraries(ROOT_analysis_1 ROOT::Core ROOT::RIO ROOT::Hist ROOT::Tree ROOT::Gui)

# the working directory of the chain: the calibration files of the installation and the cards
set(workdir ${CMAKE_CURRENT_BINARY_DIR}/run)
file(MAKE_DIRECTORY ${workdir})
foreach(f aupdat1 aupdat1.b bupdat0 bupdat0.b bupdat1 bupdat1.b)
  configure_file(${JADE_CAL_DIR}/${f} ${workdir}/${f} COPYONLY)
endforeach()
configure_file(fptobos.card.in ${workdir}/fptobos.card @ONLY)
foreach(c superv ze4v jzread jtjob)
  configure_file(${c}.card ${workdir}/${c}.card COPYONLY)
endforeach()

# A step of the chain: the program reads its card from the standard input.
function(jade_example_step name program card depends)
  add_test(NAME ${name} WORKING_DIRECTORY ${workdir} COMMAND sh -c "\"$0\" < \"$1\"" ${program} ${card})
  if(depends)
    set_property(TEST ${name} PROPERTY DEPENDS ${depends})
  endif()
endfunction()

jade_example_step(ROOT_FPTOBOS ${JADE_fptobos_EXECUTABLE} fptobos.card "")
jade_fortran_test(ROOT_FPTOBOS "GFORTRAN_CONVERT_UNIT=native" "F_UFMTENDIAN=" " ")
jade_example_step(ROOT_SUPERV ${JADE_superv_EXECUTABLE} superv.card ROOT_FPTOBOS)
jade_fortran_test(ROOT_SUPERV "GFORTRAN_CONVERT_UNIT=big_endian\;native:2" "F_UFMTENDIAN=big:1,3-100" " ")
jade_example_step(ROOT_ZE4V ${JADE_ze4v_EXECUTABLE} ze4v.card ROOT_SUPERV)
jade_fortran_test(ROOT_ZE4V "GFORTRAN_CONVERT_UNIT=big_endian" "F_UFMTENDIAN=big" " ")
jade_example_step(ROOT_JZREAD ${JADE_jzread_EXECUTABLE} jzread.card ROOT_ZE4V)
jade_fortran_test(ROOT_JZREAD "GFORTRAN_CONVERT_UNIT=native" "F_UFMTENDIAN=" " ")
jade_example_step(ROOT_JTJOB ${JADE_jtjob_EXECUTABLE} jtjob.card ROOT_JZREAD)
jade_fortran_test(ROOT_JTJOB "GFORTRAN_CONVERT_UNIT=native\;big_endian:2,22" "F_UFMTENDIAN=big:2,22" "FORT_CONVERT2=big_ieee FORT_CONVERT22=big_ieee")

add_test(NAME ROOT_H2ROOT WORKING_DIRECTORY ${workdir} COMMAND ${JADE_H2ROOT} jtjobfile006.hbook jtjobfile006.root)
set_property(TEST ROOT_H2ROOT PROPERTY DEPENDS ROOT_JTJOB)
add_test(NAME ROOT_ANALYSIS WORKING_DIRECTORY ${workdir}
         COMMAND $<TARGET_FILE:ROOT_analysis_1> jtjobfile006.root jtjobfile006_1.pdf "real data")
set_property(TEST ROOT_ANALYSIS PROPERTY DEPENDS ROOT_H2ROOT)
