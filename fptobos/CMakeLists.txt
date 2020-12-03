CMAKE_MINIMUM_REQUIRED(VERSION 3.5.0) #Because of ROOT
PROJECT(JTUPLE Fortran CXX C)
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
option(JADE_USE_CERNLIB      OFF)
if (JADE_USE_CERNLIB)
  find_library (LIBGRAFLIB NAMES libgraflib.a HINTS /usr/lib64/cernlib/2006/lib/ /usr/lib64/cernlib/2006/lib/)
  find_library (LIBMATHLIB NAMES libmathlib.a HINTS /usr/lib64/cernlib/2006/lib/ /usr/lib64/cernlib/2006/lib/)
  find_library (LIBPACKLIB NAMES libpacklib.a HINTS /usr/lib64/cernlib/2006/lib/ /usr/lib64/cernlib/2006/lib/)
  find_library (LIBKERNLIB NAMES libkernlib.a HINTS /usr/lib64/cernlib/2006/lib/ /usr/lib64/cernlib/2006/lib/)
  set(PICOCERNLIB "${LIBPACKLIB};${LIBKERNLIB};${LIBMATHLIB};${LIBGRAFLIB}")
else()
  find_library(LIBPICOKERNLIB REQUIRED NAMES libpicokernlib.a  PATHS ${PICOCERNLIBPREFIX}/lib ${PICOCERNLIBPREFIX}/lib64 NO_DEFAULT_PATH)
  find_library(LIBPICOGRAFLIB REQUIRED NAMES libpicograflib.a  PATHS ${PICOCERNLIBPREFIX}/lib ${PICOCERNLIBPREFIX}/lib64 NO_DEFAULT_PATH)
  if(${CMAKE_SYSTEM_NAME} MATCHES "Darwin")
     set(PICOCERNLIB ${LIBPICOKERNLIB} ${LIBPICOGRAFLIB} ${ROOT_LIBRARIES} -lc++)
  else()
    set(PICOCERNLIB ${LIBPICOKERNLIB} ${LIBPICOGRAFLIB} ${ROOT_LIBRARIES} -lstdc++)
  endif()
endif()
set(JEXTERN_LIBRARIES  ${PICOCERNLIB} )
set(CMAKE_CXX_FLAGS     "${CMAKE_CXX_FLAGS}     -DJEXTERNISPICO")
set(CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} -DJEXTERNISPICO")
########################################################################
  if(${CMAKE_SYSTEM_NAME} MATCHES "Darwin")
     set(JEXTERN_GRAPH_LIBRARIES -lc++)
  else()
    find_package(X11  REQUIRED)
    set(JEXTERN_GRAPH_LIBRARIES ${X11_LIBRARIES})
  endif()
message(STATUS "JEXTERN libraries       : ${JEXTERN_LIBRARIES}")
message(STATUS "JEXTERN_GRAPH libraries : ${JEXTERN_GRAPH_LIBRARIES}")
########################################################################
SET(FPTOBOSEXE fptobos)
SET(UNPJADEXE unpjad)

# Define the library name
FIND_PATH(JADELIB_LIBRARY_DIR NAMES libinterface.a libjadegs.a libboslib.a libzlib.a PATHS
  ${JADELIB_ROOT_DIR}/lib
  ${JADELIB_ROOT_DIR}/lib64
  /usr/lib/jadesoft
  ../jadesoft/lib
  ../jadesoft/
  NO_DEFAULT_PATH
) 
set(jadelibs interface jadegs boslib zlib)  
set(JADE_LIBRARIES)
foreach(_cpt ${jadelibs})
  find_library(JADE_${_cpt}_LIBRARY lib${_cpt}.a ${_cpt} HINTS ${JADELIB_LIBRARY_DIR})
  if(JADE_${_cpt}_LIBRARY)
    mark_as_advanced(JADE_${_cpt}_LIBRARY)
    list(APPEND JADE_LIBRARIES ${JADE_${_cpt}_LIBRARY})
  endif()
endforeach()
  

# Define some directories
SET(SRC ${CMAKE_SOURCE_DIR}/src)
SET(LIB ${CMAKE_SOURCE_DIR}/lib)
SET(BIN ${CMAKE_SOURCE_DIR}/bin)
include_directories( ${SRC} )

SET(FPTOBOS_src ${SRC}/fptobos.f 
${SRC}/unpjad.f 
${SRC}/unpaff.f 
${SRC}/bytswp.f 
${SRC}/chasci.f 
${SRC}/pripatr.f 
${SRC}/prihead.f 
${SRC}/prizvtx.f 
${SRC}/prizehd.f 
${SRC}/prize4v.f 
${SRC}/prilgcl.f 
${SRC}/prigvtx.f
 ${SRC}/primur1.f 
 ${SRC}/primur2.f 
 ${SRC}/pritpev.f 
 ${SRC}/pritptr.f 
 ${SRC}/pritpvx.f 
 ${SRC}/pritagg.f 
 ${SRC}/twoin1.f  
 ${SRC}/cnvibm3e.f 
 ${SRC}/chebcd.f )
 
 
 SET(UNPJAD_src
${SRC}/unpjad1.f 
${SRC}/unpaff.f 
${SRC}/bytswp.f 
${SRC}/chasci.f 
${SRC}/pripatr.f 
${SRC}/prihead.f 
${SRC}/prizvtx.f 
${SRC}/prize4v.f 
${SRC}/twoin1.f  
${SRC}/cnvibm3e.f 
#${SRC}/prizehd.f 
#${SRC}/prilgcl.f 
#${SRC}/prigvtx.f 
#${SRC}/primur1.f 
#${SRC}/primur2.f 
#${SRC}/pritpev.f 
#${SRC}/pritptr.f 
#${SRC}/pritpvx.f 
#${SRC}/pritagg.f 
#${SRC}/chebcd.f 

${SRC}/mvcl.f   #ISNT IT IN BLAS?
)

ADD_EXECUTABLE(${FPTOBOSEXE} ${FPTOBOS_src})
ADD_EXECUTABLE(${UNPJADEXE} ${UNPJAD_src})

if(${CMAKE_SYSTEM_NAME} MATCHES "Darwin")
  TARGET_LINK_LIBRARIES(${FPTOBOSEXE} -mmacosx-version-min=10.12 ${JEXTERN_LIBRARIES}  ${JADE_LIBRARIES} ${LAPACK_LIBRARIES} )
  TARGET_LINK_LIBRARIES(${UNPJADEXE} -mmacosx-version-min=10.12 ${JEXTERN_LIBRARIES}  ${JADE_LIBRARIES} ${LAPACK_LIBRARIES} )
else()
  TARGET_LINK_LIBRARIES(${FPTOBOSEXE} -Wl,--start-group ${JEXTERN_LIBRARIES}  ${JADE_LIBRARIES} ${LAPACK_LIBRARIES} -Wl,--end-group)
  TARGET_LINK_LIBRARIES(${UNPJADEXE} -Wl,--start-group ${JEXTERN_LIBRARIES}  ${JADE_LIBRARIES} ${LAPACK_LIBRARIES} -Wl,--end-group)
endif()

########################################################################
INSTALL(TARGETS ${FPTOBOSEXE}  RUNTIME DESTINATION ${CMAKE_INSTALL_BINDIR})
INSTALL(TARGETS ${UNPJADEXE}  RUNTIME DESTINATION ${CMAKE_INSTALL_BINDIR})
SET(CMAKE_Fortran_MODULE_DIRECTORY ${LIB})
########################################################################
