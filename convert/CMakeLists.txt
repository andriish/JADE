CMAKE_MINIMUM_REQUIRED(VERSION 3.5.0) #Because of ROOT
PROJECT(JADECONVERT Fortran C CXX)
SET(VERSION 2020.11)
include("GNUInstallDirs")
SET(CMAKE_MODULE_PATH "${CMAKE_SOURCE_DIR}/../cmake/Modules/")
set(CMAKE_VERBOSE_MAKEFILE OFF)
########################################################################
#Setup Fortran compiller
if (${CMAKE_Fortran_COMPILER_ID}  MATCHES "GNU")
  if(${CMAKE_SYSTEM_NAME} MATCHES "Darwin")
    set(CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} -std=legacy -g -O0 -fallow-invalid-boz  -cpp -fno-range-check -ffixed-line-length-none  -finit-local-zero -fno-automatic -fno-align-commons ")
  else()
    set(CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} -std=legacy -g -O0                      -cpp -fno-range-check -ffixed-line-length-none  -finit-local-zero -fno-automatic -fno-align-commons ")
  endif()
endif()
if (${CMAKE_Fortran_COMPILER_ID}  MATCHES  "Intel")
  set(CMAKE_Fortran_FLAGS   "${CMAKE_Fortran_FLAGS} -g -O0  -debug extended -traceback  -fpe0  -fpp  -diag-disable=6371 -diag-disable=6375  -diag-disable=7784 -diag-disable=7713 -diag-disable=8291 -noalign -noauto -zero -nbs -extend-source 132")
endif()
if (${CMAKE_Fortran_COMPILER_ID}  MATCHES  "XL")
  set(CMAKE_Fortran_FLAGS   "${CMAKE_Fortran_FLAGS} -g -qfixed=256 -qsigtrap -qsave -qrndsngl -qmaxmem=-1 -qextname -qfloat=fltint:hsflt:hssngl:nans:rndsngl -qcharlen=32767 -qxlf77=leadzero -qfullpath -qctyplss -qintlog ")
endif()
message(STATUS "Fortran compiler        : ${CMAKE_Fortran_COMPILER_ID}")
message(STATUS "Fortran compiler flags  : ${CMAKE_Fortran_FLAGS}")
########################################################################
#Setup C compiller
if (${CMAKE_C_COMPILER_ID}  MATCHES "Clang")
  set(CMAKE_C_FLAGS "${CMAKE_C_FLAGS} -g -O1 -Wno-conditional-uninitialized -Wno-incompatible-library-redeclaration -Wno-implicit-function-declaration -Wno-implicit-int -Wno-return-type")
endif()
if (${CMAKE_C_COMPILER_ID}  MATCHES "GNU")
  set(CMAKE_C_FLAGS "${CMAKE_C_FLAGS} -g -O1")
endif()
if (${CMAKE_C_COMPILER_ID}  MATCHES  "Intel")
  set(CMAKE_C_FLAGS   "${CMAKE_C_FLAGS} -g -O1  -debug extended -traceback   -diag-disable=2196  -diag-disable=161  -diag-disable=303   ")
endif()
if (${CMAKE_C_COMPILER_ID}  MATCHES  "XL")
  set(CMAKE_C_FLAGS   "${CMAKE_C_FLAGS} -g")
endif()
message(STATUS "C compiler            : ${CMAKE_C_COMPILER_ID}")
message(STATUS "C compiler flags      : ${CMAKE_C_FLAGS}")
########################################################################
#Setup C++ compiller
if (${CMAKE_CXX_COMPILER_ID}  MATCHES "Clang")
  set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -g -O1  -Wno-conditional-uninitialized -Wno-incompatible-library-redeclaration -Wno-implicit-function-declaration -Wno-implicit-int -Wno-return-type")
endif()
if (${CMAKE_CXX_COMPILER_ID}  MATCHES "GNU")
  set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -g -O1")
endif()
if (${CMAKE_CXX_COMPILER_ID}  MATCHES  "Intel")
  set(CMAKE_CXX_FLAGS   "${CMAKE_CXX_FLAGS} -g -O1  -debug extended -traceback   -diag-disable=2196  -diag-disable=161  -diag-disable=303   ")
endif()
if (${CMAKE_CXX_COMPILER_ID}  MATCHES  "XL")
  set(CMAKE_CXX_FLAGS   "${CMAKE_CXX_FLAGS} -g")
endif()
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
message(STATUS "C++ compiler            : ${CMAKE_CXX_COMPILER_ID}")
message(STATUS "C++ compiler flags      : ${CMAKE_CXX_FLAGS}")
########################################################################
  find_package(BLAS REQUIRED)
  find_package(LAPACK REQUIRED)
message(STATUS "LAPACK libraries        : ${LAPACK_LIBRARIES_DIR}")
########################################################################
find_package(HepMC3)
message(STATUS "HEPMC3 libraries        : ${HEPMC3_LIBRARIES} ${HEPMC3_ROOTIO_LIBRARIES}")
########################################################################
set(CMAKE_CXX_FLAGS_BEFORE_ROOT "${CMAKE_CXX_FLAGS}")
set(CMAKE_C_FLAGS_BEFORE_ROOT "${CMAKE_C_FLAGS}")
set(CMAKE_Fortran_FLAGS_BEFORE_ROOT "${CMAKE_Fortran_FLAGS}")
find_package(ROOT REQUIRED  Gui Core RIO Net Hist Graf Graf3d Gpad Tree Rint Postscript Matrix Physics MathCore Thread)
message(STATUS "ROOT use file: ${ROOT_USE_FILE}")
include(${ROOT_USE_FILE})
set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS_BEFORE_ROOT}")
set(CMAKE_C_FLAGS "${CMAKE_C_FLAGS_BEFORE_ROOT}")
set(CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS_BEFORE_ROOT}")
string(REPLACE " " ";" ROOT_CXX_FLAGS_LIST ${ROOT_CXX_FLAGS})
#foreach(fl ${ROOT_CXX_FLAGS_LIST})
#  CHECK_CXX_COMPILER_FLAG(${fl} COMPILER_SUPPORTS_${fl})
#  if(COMPILER_SUPPORTS_${fl})
#    set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} ${fl}")
#  endif()
#endforeach(fl ${ROOT_CXX_FLAGS_LIST})
include_directories(${ROOT_INCLUDE_DIRS})
message(STATUS "ROOT includes : ${ROOT_INCLUDE_DIRS}")
message(STATUS "ROOT libraries: ${ROOT_LIBRARIES}")
message(STATUS "ROOT_FOUND    : ${ROOT_FOUND}")
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
