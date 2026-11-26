# Example: the K0S chain fptobos -> superv -> k0s on a JADE data tape.
#   fptobos converts the tape to the BOS format, superv reconstructs the events, k0s (k0s.f, Fortran) looks for K0S -> pi+ pi-
#   in pairs of tracks of opposite charges with close helices (the cuts are in k0s.card): the mass distribution and the
#   distances of the vertices. The programs read their cards from the standard input, in the directory <build>/run.
#
#   cmake -S examples/k0s -B build -DCMAKE_PREFIX_PATH=<install prefix> [-DJADE_EXAMPLE_DATA=<directory with the tapes>]
#   cmake --build build            # k0s
#   ctest --test-dir build         # the chain, the results are in build/run
#
# The data are not part of the example: JADE_EXAMPLE_DATA is the directory with the FPACK tapes (default: the sample
# data of the installed test project, JADE_TEST_DIR/jadetapes), JADE_EXAMPLE_TAPE the tape in it. The package JADE is
# required: the compilers, the flags and the number of bits are those of the installation (JADEConfig.cmake).
cmake_minimum_required(VERSION 3.24)
set(JADE_NO_TARGETS ON)
find_package(JADE 2000.0.0 CONFIG REQUIRED)
unset(JADE_NO_TARGETS)
jade_use_toolchain()
project(JADEK0SEXAMPLE Fortran C)
jade_use_flags()
find_package(JADE 2000.0.0 CONFIG REQUIRED)
enable_testing()

# the data: a directory, not part of the example
set(JADE_EXAMPLE_DATA "${JADE_TEST_DIR}/jadetapes" CACHE PATH "Directory with the data (FPACK tapes)")
set(JADE_EXAMPLE_TAPE "JAD073/file006" CACHE STRING "The tape, relative to JADE_EXAMPLE_DATA")
set(JADE_EXAMPLE_TAPE_PATH "${JADE_EXAMPLE_DATA}/${JADE_EXAMPLE_TAPE}")
if(NOT EXISTS "${JADE_EXAMPLE_TAPE_PATH}")
  message(FATAL_ERROR "The data tape ${JADE_EXAMPLE_TAPE_PATH} does not exist: give the directory with "
                      "-DJADE_EXAMPLE_DATA=<directory> (and -DJADE_EXAMPLE_TAPE=<file in it>)")
endif()
message(STATUS "JADE example k0s: data ${JADE_EXAMPLE_TAPE_PATH}")

# The first field of the card of fptobos (IBYTSW) switches the byte swapping of the words of the FPACK file (big-endian
# words) on: right for a little-endian host; on a big-endian host the words must not be swapped.
if(JADE_BIG_ENDIAN)
  set(JADE_IBYTSW_FIELD "         0")
else()
  set(JADE_IBYTSW_FIELD "         1")
endif()

# the analysis program
add_executable(k0s k0s.f)
jade_link_group(k0s ${JADE_CORE_TARGETS} ${JADE_CERNLIB_TARGETS})
target_link_libraries(k0s PRIVATE LAPACK::LAPACK BLAS::BLAS)

# the working directory of the chain: the calibration files of the installation and the cards
set(workdir ${CMAKE_CURRENT_BINARY_DIR}/run)
file(MAKE_DIRECTORY ${workdir})
foreach(f aupdat1 aupdat1.b bupdat0 bupdat0.b bupdat1 bupdat1.b)
  configure_file(${JADE_CAL_DIR}/${f} ${workdir}/${f} COPYONLY)
endforeach()
configure_file(fptobos.card.in ${workdir}/fptobos.card @ONLY)
configure_file(superv.card ${workdir}/superv.card COPYONLY)
configure_file(k0s.card ${workdir}/k0s.card COPYONLY)

# A step of the chain: the program reads its card from the standard input.
function(jade_example_step name program card depends)
  add_test(NAME ${name} WORKING_DIRECTORY ${workdir} COMMAND sh -c "\"$0\" < \"$1\"" ${program} ${card})
  if(depends)
    set_property(TEST ${name} PROPERTY DEPENDS ${depends})
  endif()
endfunction()

jade_example_step(K0S_FPTOBOS ${JADE_fptobos_EXECUTABLE} fptobos.card "")
jade_fortran_test(K0S_FPTOBOS "GFORTRAN_CONVERT_UNIT=native" "F_UFMTENDIAN=" " ")
jade_example_step(K0S_SUPERV ${JADE_superv_EXECUTABLE} superv.card K0S_FPTOBOS)
jade_fortran_test(K0S_SUPERV "GFORTRAN_CONVERT_UNIT=big_endian\;native:2" "F_UFMTENDIAN=big:1,3-100" " ")
add_test(NAME K0S_ANALYSIS WORKING_DIRECTORY ${workdir}
         COMMAND $<TARGET_FILE:k0s> supervfile006.bos k0s.txt k0s_dist.txt k0s.card)
set_property(TEST K0S_ANALYSIS PROPERTY DEPENDS K0S_SUPERV)
jade_fortran_test(K0S_ANALYSIS "GFORTRAN_CONVERT_UNIT=native\;big_endian:2" "F_UFMTENDIAN=big:2" "FORT_CONVERT2=big_ieee")
