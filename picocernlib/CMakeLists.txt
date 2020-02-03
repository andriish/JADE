CMAKE_MINIMUM_REQUIRED(VERSION 2.8.5)
PROJECT(PICOCERNLIB Fortran C CXX)

set(CMAKE_CXX_STANDARD 17)
set(CMAKE_CXX_EXTENSIONS OFF)
SET(VERSION 2.99)
# Add our local modlues to the module path
SET(CMAKE_MODULE_PATH "${CMAKE_SOURCE_DIR}/../cmake/Modules/")
include("GNUInstallDirs")
set(CMAKE_VERBOSE_MAKEFILE OFF)


set(CMAKE_C_FLAGS   "${CMAKE_C_FLAGS} -O0 -g -Wno-incompatible-library-redeclaration -Wno-implicit-function-declaration -Wno-implicit-int  -Wno-return-type  -DCERNLIB_QMLXIA64 -DCERNLIB_X11 -DCERNLIB_UNIX -DCERNLIB_LINUX  -DCERNLIB_GFORTRAN -DCERNLIB_QX_SC ")
set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -O0 -g  -Wno-return-type  -DCERNLIB_QMLXIA64 -DCERNLIB_X11 -DCERNLIB_UNIX -DCERNLIB_LINUX  -DCERNLIB_GFORTRAN -DCERNLIB_QX_SC ")


get_filename_component (Fortran_COMPILER_NAME ${CMAKE_Fortran_COMPILER} NAME)

if (Fortran_COMPILER_NAME MATCHES "gfortran.*")
   set(CMAKE_Fortran_FLAGS            "   -DCERNLIB_QMLXIA64 -DCERNLIB_X11 -DCERNLIB_UNIX -DCERNLIB_LINUX  -DCERNLIB_GFORTRAN  -DCERNLIB_QX_SC         -std=legacy -g -ffixed-line-length-none -finit-local-zero   -fno-automatic -O0   -fno-align-commons ")
endif()

if (Fortran_COMPILER_NAME MATCHES "ifort.*")
#NOT USED
   set(CMAKE_Fortran_FLAGS            "-fno-automatic -fno-backslash -extend-source 132")
endif()

if (Fortran_COMPILER_NAME MATCHES "xlf.*")
#PPC64 only
   set(CMAKE_Fortran_FLAGS           " -qfixed=256 -q32  -melf32ppc -qsigtrap -g -qsave -qrndsngl -qmaxmem=-1 -qextname -qfloat=fltint:hsflt:hssngl:nans:rndsngl -qcharlen=32767 -qxlf77=leadzero -qfullpath -qctyplss -qintlog ")	
endif()

if (Fortran_COMPILER_NAME MATCHES "sun.*")
#Local variables are static by default. Alternate is STATIC statement. 
   set(CMAKE_Fortran_FLAGS           "  -ftrap=%none -f77 -e -m32  -g  ")
endif()


message(STATUS "Fortran compiler        : ${Fortran_COMPILER_NAME}")
message(STATUS "Fortran compiler flags  : ${CMAKE_Fortran_FLAGS}")
message(STATUS "CERN libraries          : ${CERN_LIBRARIES}")


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




SET(PICOCERNLIB picocernlib)
# Define some directories
SET(TOP ${CMAKE_SOURCE_DIR}/)
SET(SRC ${CMAKE_SOURCE_DIR}/src)
SET(LIB ${CMAKE_SOURCE_DIR}/lib)
SET(BIN ${CMAKE_SOURCE_DIR}/bin)

include_directories( ${TOP}/include  ${TOP}/ffread )



SET(CMAKE_Fortran_MODULE_DIRECTORY ${LIB})

if (Fortran_COMPILER_NAME MATCHES "xlf.*")
   file(GLOB myfortag ${SRCTAGG}/*.for)
   set_property(SOURCE ${myfortag} PROPERTY COMPILE_FLAGS -qsuffix=f=for)
endif()

SET(PICOCERNLIB_src
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
#The C++ code with ROOT
${TOP}/src/JBOOK.cxx
${TOP}/src/JINTERFACE.F
${TOP}/src/JHIGZ.cxx
#APP mode
${TOP}/src/Frame.cxx
#${TOP}/src/Frame_Dict.cxx
)


ADD_LIBRARY(${PICOCERNLIB} STATIC  ${PICOCERNLIB_src})

# Add a distclean target to the Makefile
ADD_CUSTOM_TARGET(distclean COMMAND ${CMAKE_COMMAND} -P ${CMAKE_SOURCE_DIR}/distclean.cmake)


install(
TARGETS ${PICOCERNLIB} 
ARCHIVE DESTINATION ${CMAKE_INSTALL_LIBDIR} 
LIBRARY DESTINATION ${CMAKE_INSTALL_LIBDIR} 
COMPONENT library 
)


