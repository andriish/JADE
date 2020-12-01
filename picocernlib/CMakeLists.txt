CMAKE_MINIMUM_REQUIRED(VERSION 3.5.0) #Because of ROOT
PROJECT(PICOCERNLIB Fortran C CXX)
SET(VERSION 2020.11)
include("GNUInstallDirs")
SET(CMAKE_MODULE_PATH "${CMAKE_SOURCE_DIR}/../cmake/Modules/")
set(CMAKE_VERBOSE_MAKEFILE OFF)
########################################################################
set(CMAKE_C_FLAGS   "${CMAKE_C_FLAGS} -O0 -g -Wno-incompatible-library-redeclaration -Wno-implicit-function-declaration -Wno-implicit-int  -Wno-return-type  -DCERNLIB_QMLXIA64 -DCERNLIB_X11 -DCERNLIB_UNIX -DCERNLIB_LINUX  -DCERNLIB_GFORTRAN -DCERNLIB_QX_SC ")
set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -O0 -g  -Wno-return-type  -DCERNLIB_QMLXIA64 -DCERNLIB_X11 -DCERNLIB_UNIX -DCERNLIB_LINUX  -DCERNLIB_GFORTRAN -DCERNLIB_QX_SC ")
set(CMAKE_Fortran_FLAGS            "${CMAKE_Fortran_FLAGS}  -DCERNLIB_QMLXIA64 -DCERNLIB_X11 -DCERNLIB_UNIX -DCERNLIB_LINUX  -DCERNLIB_GFORTRAN  -DCERNLIB_QX_SC ")
########################################################################
#Setup Fortran compiller
if (${CMAKE_Fortran_COMPILER_ID}  MATCHES "GNU")
  if(${CMAKE_SYSTEM_NAME} MATCHES "Darwin")
    set(CMAKE_Fortran_FLAGS            "${CMAKE_Fortran_FLAGS}  -fallow-invalid-boz  -cpp -std=legacy -g -ffixed-line-length-none -finit-local-zero   -fno-automatic -O0   -fno-align-commons ")
  else()
    set(CMAKE_Fortran_FLAGS            "${CMAKE_Fortran_FLAGS}  -std=legacy -g -ffixed-line-length-none -finit-local-zero   -fno-automatic -O0   -fno-align-commons ")
  endif()
endif()
if (${CMAKE_Fortran_COMPILER_ID}  MATCHES  "Intel")
#NOT USED
  set(CMAKE_Fortran_FLAGS            "${CMAKE_Fortran_FLAGS} -g -O0  -diag-disable=6371 -diag-disable=7713 -diag-disable=8291 -noalign -noauto -zero     -nbs -extend-source 132")
endif()
if (${CMAKE_Fortran_COMPILER_ID}  MATCHES  "XL")
#PPC64 only
  set(CMAKE_Fortran_FLAGS           "${CMAKE_Fortran_FLAGS}  -qfixed=256  -qsigtrap -g -qsave -qrndsngl -qmaxmem=-1 -qextname -qfloat=fltint:hsflt:hssngl:nans:rndsngl -qcharlen=32767 -qxlf77=leadzero -qfullpath -qctyplss -qintlog ")	
#-q32  -melf32ppc
endif()
if (${CMAKE_Fortran_COMPILER_ID}  MATCHES  "SunPro")
#NOT WELL TESTED
#Local variables are static by default. Alternate is STATIC statement. 
  set(CMAKE_Fortran_FLAGS           "${CMAKE_Fortran_FLAGS}   -ftrap=%none -f77 -e  -g  ")
#-m32 
endif()
message(STATUS "Fortran compiler        : ${CMAKE_Fortran_COMPILER_ID}")
message(STATUS "Fortran compiler flags  : ${CMAKE_Fortran_FLAGS}")
########################################################################
#Setup C++ compiller
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
set (ROOT_FIND_COMPONENTS Core RIO Tree Graf)
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
    find_package(ROOT QUIET REQUIRED COMPONENTS Core RIO Tree  Graf)
  endif()
  message(STATUS "ROOT includes: ${ROOT_INCLUDE_DIRS}")
  message(STATUS "ROOT libraries: ${ROOT_LIBRARIES}")
  message(STATUS "ROOT_FOUND: ${ROOT_FOUND}")
  if(ROOT_FOUND)
    include_directories(${ROOT_INCLUDE_DIRS})
  else()
    message(STATUS "PICOCERNLIB warning: ROOT needed for ROOTIO but not found")
    message(STATUS "PICOCERNLIB: Skipping HepMC ROOT IO libraries")
  endif()
########################################################################



SET(PICOKERNLIB picokernlib)
SET(PICOGRAFLIB picograflib)
# Define some directories
SET(TOP ${CMAKE_SOURCE_DIR}/)
SET(SRC ${CMAKE_SOURCE_DIR}/src)
SET(LIB ${CMAKE_SOURCE_DIR}/lib)
SET(BIN ${CMAKE_SOURCE_DIR}/bin)

include_directories( ${TOP}/include  ${TOP}/ffread  ${TOP}/include/kerngen)

SET(CMAKE_Fortran_MODULE_DIRECTORY ${LIB})

if (${CMAKE_Fortran_COMPILER_ID}  MATCHES  "XL")
   file(GLOB myfortag ${SRCTAGG}/*.for)
   set_property(SOURCE ${myfortag} PROPERTY COMPILE_FLAGS -qsuffix=f=for)
endif()

SET(PICOKERNLIB_src
${TOP}/src/cbyt.F
${TOP}/src/prob.F
${TOP}/src/ucopy2.F
${TOP}/src/cltou.F
${TOP}/src/sbyt.F
${TOP}/src/ublank.F
#${TOP}/src/datimh.F
${TOP}/src/datimh.c
${TOP}/src/lenocc.F
${TOP}/src/msbit0.F
${TOP}/src/uhtoc.F
${TOP}/src/vzero.F
${TOP}/src/sbit1.F
${TOP}/src/locf.F
${TOP}/src/cdmon.F
${TOP}/src/cleap.F
${TOP}/src/ubunch.F
${TOP}/src/timest.F
${TOP}/src/sbit0.F
${TOP}/src/iucomp.F
${TOP}/src/mtlprt.F
${TOP}/src/cyear4.F
${TOP}/src/ublow.F
${TOP}/src/uctoh.F
${TOP}/src/msbit1.F
${TOP}/src/mbytet.F
${TOP}/src/jbit.F
#${TOP}/src/datime.F
${TOP}/src/datime.c
${TOP}/src/timel.F
${TOP}/src/vblank.F
${TOP}/src/cydiy.F
${TOP}/src/ucopy.F
${TOP}/src/caldat.F
${TOP}/src/rnorml.F
${TOP}/src/abend.F
${TOP}/src/vfill.F
${TOP}/src/uzero.F
${TOP}/src/mbytor.F
${TOP}/ffread/sffread/ffset.F
${TOP}/ffread/sffread/ffread.F
${TOP}/ffread/sffread/ffskip.F
${TOP}/ffread/sffread/fffind.F
${TOP}/ffread/sffread/ffupca.F
${TOP}/ffread/sffread/ffinit.F
${TOP}/ffread/sffread/ffcard.F
${TOP}/ffread/sffread/ffuset.F
${TOP}/ffread/sffread/ffrset.F
${TOP}/ffread/sffread/ffkey.F
${TOP}/ffread/sffread/ffget.F
${TOP}/ffread/sffread/ffgo.F
${TOP}/ffread/sffread/ffuser.F
${TOP}/src/jumpxn.c
${TOP}/src/cfget.c
${TOP}/src/cfopen.F
${TOP}/src/mtlset.F
${TOP}/src/noarg.F
${TOP}/src/cutol.F
${TOP}/src/cfclos.c
${TOP}/src/vxinvb.c
${TOP}/src/cfopei.c
${TOP}/src/fchtak.c
${TOP}/src/spaces.F
${TOP}/src/vdot.F
${TOP}/src/blow.F
${TOP}/src/jbytet.F
${TOP}/src/chtoi.F
${TOP}/src/itoch.F
${TOP}/src/upkch.F
${TOP}/src/indexc.F

${TOP}/src/ckrack.F
)
SET(PICOGRAFLIB_src
#The C++ code with ROOT
${TOP}/src/JBOOK.cxx
${TOP}/src/JINTERFACE.F
${TOP}/src/JHIGZ.cxx
#APP mode
${TOP}/src/Frame.cxx
#${TOP}/src/Frame_Dict.cxx
)


ADD_LIBRARY(${PICOKERNLIB} STATIC  ${PICOKERNLIB_src})
ADD_LIBRARY(${PICOGRAFLIB} STATIC  ${PICOGRAFLIB_src})

# Add a distclean target to the Makefile
ADD_CUSTOM_TARGET(distclean COMMAND ${CMAKE_COMMAND} -P ${CMAKE_SOURCE_DIR}/distclean.cmake)

########################################################################
install(
TARGETS ${PICOKERNLIB} ${PICOGRAFLIB} 
ARCHIVE DESTINATION ${CMAKE_INSTALL_LIBDIR} 
LIBRARY DESTINATION ${CMAKE_INSTALL_LIBDIR} 
COMPONENT library 
)
########################################################################
