# The JADE software: one CMake project with the parts jadesoft, convert, jtuple, fptobos and h2root as
# ordinary subdirectories. It consolidates the build and the installation of all libraries and executables
# and installs the package configuration JADEConfig.cmake (imported targets JADE::<name> of all installed
# executables and libraries, the directories, the toolchain and the compilers).
#
#   cmake -S . -B buildGNU64 [-DJADE_TOOLCHAIN=GNU] [-DJADE_BITS=64] [-DCMAKE_INSTALL_PREFIX=/abs/path]
#   cmake --build buildGNU64
#   cmake --install buildGNU64
#
# The tests are the project test/ (its own CMakeLists.txt): they use the installed software through JADEConfig.cmake.
#
# Options (cache variables):
#   JADE_TOOLCHAIN       GNU (default), GNU-11 ... GNU-16, Intel, XL, NAG, PGI, SUN: the compilers and their
#                        environment, see cmake/Modules/JADE_toolchain.cmake
#   JADE_BITS            32 or 64 (default: the host); 32 sets JADE_FORCE_32
#   CMAKE_INSTALL_PREFIX default: <source dir>/installed<toolchain><bits>
#   JADE_PREFIX_BASE     if given, the prefix is <JADE_PREFIX_BASE><toolchain><bits>
#   JADE_BUILD_CONVERT, JADE_BUILD_JTUPLE, JADE_BUILD_FPTOBOS, JADE_BUILD_H2ROOT   parts to build (ON)
#   JADE_OLD_MC, JADE_FPE, JADE_CAL_DIR_NAME   options of jadesoft
cmake_minimum_required(VERSION 3.10)
list(PREPEND CMAKE_MODULE_PATH "${CMAKE_CURRENT_SOURCE_DIR}/cmake/Modules")

# toolchain and bits: the compilers are selected before project()
include(JADE_toolchain)
foreach(JADE_L Fortran C CXX)
  if(JADE_${JADE_L})
    set(CMAKE_${JADE_L}_COMPILER ${JADE_${JADE_L}})
  endif()
endforeach()
# the compilers need the environment of the toolchain (setvars.sh of oneAPI, ...) also when they are called
if(JADE_TOOLCHAIN_ENV)
  foreach(JADE_L Fortran C CXX)
    set(CMAKE_${JADE_L}_COMPILER_LAUNCHER ${CMAKE_COMMAND} -E env ${JADE_TOOLCHAIN_ENV})
    set(CMAKE_${JADE_L}_LINKER_LAUNCHER   ${CMAKE_COMMAND} -E env ${JADE_TOOLCHAIN_ENV})
  endforeach()
endif()

set(JADE_VERSION_MAJOR 2026)
set(JADE_VERSION_MINOR 11)
set(JADE_VERSION_PATCH 0)
project(JADE VERSION ${JADE_VERSION_MAJOR}.${JADE_VERSION_MINOR}.${JADE_VERSION_PATCH} LANGUAGES Fortran C CXX)
include(GNUInstallDirs)

# the byte order of the host (the calibration files of jadesoft, the byte swapping of the tests): in JADEConfig.cmake
include(TestBigEndian)
if(DEFINED CMAKE_C_BYTE_ORDER)
  if(CMAKE_C_BYTE_ORDER STREQUAL "BIG_ENDIAN")
    set(JADE_BIG_ENDIAN TRUE)
  else()
    set(JADE_BIG_ENDIAN FALSE)
  endif()
else()
  test_big_endian(JADE_BIG_ENDIAN)
endif()

# installed<toolchain><bits> in the source directory unless a prefix is given; with JADE_PREFIX_BASE
# the prefix is <JADE_PREFIX_BASE><toolchain><bits> (the meaning of --prefix of jadeinstall.sh)
if(JADE_PREFIX_BASE)
  set(CMAKE_INSTALL_PREFIX "${JADE_PREFIX_BASE}${JADE_TOOLCHAIN}${JADE_BITS}" CACHE PATH "Install prefix" FORCE)
elseif(CMAKE_INSTALL_PREFIX_INITIALIZED_TO_DEFAULT)
  set(CMAKE_INSTALL_PREFIX "${CMAKE_CURRENT_SOURCE_DIR}/installed${JADE_TOOLCHAIN}${JADE_BITS}" CACHE PATH "Install prefix" FORCE)
endif()
message(STATUS "JADE install prefix: ${CMAKE_INSTALL_PREFIX}")

option(JADE_BUILD_CONVERT "Build convert (HepMC3 -> JADE)" ON)
option(JADE_BUILD_JTUPLE  "Build jtuple" ON)
option(JADE_BUILD_FPTOBOS "Build fptobos and unpjad" ON)
option(JADE_BUILD_H2ROOT  "Build jade_h2root (h2root with the 64 bit fix)" ON)

########################################################################
# the parts, in the order of the dependencies: jtuple and fptobos link the libraries of jadesoft
add_subdirectory(jadesoft)
if(JADE_BUILD_CONVERT)
  add_subdirectory(convert)
endif()
if(JADE_BUILD_JTUPLE)
  add_subdirectory(jtuple)
endif()
if(JADE_BUILD_FPTOBOS)
  add_subdirectory(fptobos)
endif()
if(JADE_BUILD_H2ROOT)
  add_subdirectory(h2root)
endif()

########################################################################
# the package configuration: every executable and library that the parts install (install(TARGETS ... EXPORT JADETargets))
# as the imported target JADE::<name>, and JADEConfig.cmake with the rest of the information
include(CMakePackageConfigHelpers)
set(JADE_CONFIG_INSTALL_DIR ${CMAKE_INSTALL_DATADIR}/JADE/cmake)
install(EXPORT JADETargets NAMESPACE JADE:: DESTINATION ${JADE_CONFIG_INSTALL_DIR})

# what the configuration records: toolchain, compilers, environment, options
set(JADE_CONFIG_INFO "")
foreach(JADE_L Fortran C CXX)
  string(APPEND JADE_CONFIG_INFO
    "set(JADE_${JADE_L}_COMPILER [=[${CMAKE_${JADE_L}_COMPILER}]=])\n"
    "set(JADE_${JADE_L}_COMPILER_ID [=[${CMAKE_${JADE_L}_COMPILER_ID}]=])\n"
    "set(JADE_${JADE_L}_COMPILER_VERSION [=[${CMAKE_${JADE_L}_COMPILER_VERSION}]=])\n")
endforeach()
foreach(JADE_O JADE_OLD_MC JADE_FPE JADE_CAL_DIR_NAME)
  string(APPEND JADE_CONFIG_INFO "set(${JADE_O} [=[${${JADE_O}}]=])\n")
endforeach()
set(JADE_PARTS jadesoft)
foreach(JADE_P convert jtuple fptobos h2root)
  string(TOUPPER ${JADE_P} JADE_PU)
  if(JADE_BUILD_${JADE_PU})
    list(APPEND JADE_PARTS ${JADE_P})
  endif()
endforeach()
# the external libraries (CERNLIB, Motif, X11, BLAS, LAPACK, HepMC3, ROOT) that the parts link, as found by the build
set(JADE_EXTERNAL_LIBRARIES "")
foreach(JADE_X CERNLIB MOTIF X11 BLAS LAPACK HEPMC3 ROOT)
  string(APPEND JADE_CONFIG_INFO "set(JADE_${JADE_X}_LIBRARIES [=[${JADE_${JADE_X}_LIBRARIES}]=])\n")
  list(APPEND JADE_EXTERNAL_LIBRARIES ${JADE_${JADE_X}_LIBRARIES})
endforeach()
list(REMOVE_DUPLICATES JADE_EXTERNAL_LIBRARIES)
string(APPEND JADE_CONFIG_INFO "set(JADE_EXTERNAL_LIBRARIES [=[${JADE_EXTERNAL_LIBRARIES}]=])\n")
string(APPEND JADE_CONFIG_INFO "set(JADE_EXE_LINKER_FLAGS [=[${JADE_EXE_LINKER_FLAGS}]=])\n")
string(APPEND JADE_CONFIG_INFO "set(JADE_PARTS ${JADE_PARTS})\n")
string(APPEND JADE_CONFIG_INFO "set(JADE_TOOLCHAIN ${JADE_TOOLCHAIN})\n")
string(APPEND JADE_CONFIG_INFO "set(JADE_BITS ${JADE_BITS})\n")
string(APPEND JADE_CONFIG_INFO "set(JADE_BIG_ENDIAN ${JADE_BIG_ENDIAN})\n")
string(APPEND JADE_CONFIG_INFO "set(JADE_FORCE_32 ${JADE_FORCE_32})\n")
string(APPEND JADE_CONFIG_INFO "set(JADE_TOOLCHAIN_ENV [=[${JADE_TOOLCHAIN_ENV}]=])\n")

configure_package_config_file(cmake/Templates/JADEConfig.cmake.in ${CMAKE_CURRENT_BINARY_DIR}/cmake/JADEConfig.cmake
                              INSTALL_DESTINATION ${JADE_CONFIG_INSTALL_DIR}
                              PATH_VARS CMAKE_INSTALL_BINDIR CMAKE_INSTALL_LIBDIR CMAKE_INSTALL_DATADIR)
write_basic_package_version_file(${CMAKE_CURRENT_BINARY_DIR}/cmake/JADEConfig-version.cmake
                                 VERSION ${PROJECT_VERSION} COMPATIBILITY AnyNewerVersion)
install(FILES ${CMAKE_CURRENT_BINARY_DIR}/cmake/JADEConfig.cmake
              ${CMAKE_CURRENT_BINARY_DIR}/cmake/JADEConfig-version.cmake
        DESTINATION ${JADE_CONFIG_INSTALL_DIR})
