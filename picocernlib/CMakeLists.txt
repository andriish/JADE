CMAKE_MINIMUM_REQUIRED(VERSION 3.5.0) #Because of ROOT
PROJECT(PICOCERNLIB Fortran C CXX)
SET(VERSION 2020.11)
include("GNUInstallDirs")
SET(CMAKE_MODULE_PATH "${CMAKE_SOURCE_DIR}/../cmake/Modules/")
set(CMAKE_VERBOSE_MAKEFILE OFF)
########################################################################
include(JADE_compiler_setup)
########################################################################
include(JADE_ROOT_setup)
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
##${TOP}/src/ranmar.F
${TOP}/src/eisrs1.F
${TOP}/src/tql2.F
${TOP}/src/tred2.F
${TOP}/src/gamma.F #SUNGAMMA
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

target_compile_definitions(${PICOKERNLIB} PRIVATE -DCERNLIB_QMLXIA64 -DCERNLIB_X11 -DCERNLIB_UNIX -DCERNLIB_LINUX  -DCERNLIB_GFORTRAN -DCERNLIB_QX_SC )
target_compile_definitions(${PICOGRAFLIB} PRIVATE -DCERNLIB_QMLXIA64 -DCERNLIB_X11 -DCERNLIB_UNIX -DCERNLIB_LINUX  -DCERNLIB_GFORTRAN -DCERNLIB_QX_SC )

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
