########################################################################
#Setup Fortran compiller
include(CheckFortranCompilerFlag)
if (${CMAKE_Fortran_COMPILER_ID}  MATCHES "GNU")
  set(Fortran_FLAGS_TO_CHECK "-fallow-invalid-boz" "-fallow-argument-mismatch"  "-Wno-argument-mismatch" )
  foreach(fl ${Fortran_FLAGS_TO_CHECK})
    CHECK_Fortran_COMPILER_FLAG(${fl} Fortran_COMPILER_SUPPORTS_${fl})
    if(Fortran_COMPILER_SUPPORTS_${fl})
      set(CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} ${fl}")
    endif()
  endforeach(fl ${Fortran_FLAGS_TO_CHECK})
  set(CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} -std=legacy -g -O0 -fbacktrace -cpp -fno-range-check -ffixed-line-length-none  -finit-local-zero -fno-automatic -fno-align-commons ")
  if (JADE_FPE)
    set(CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} -ffpe-trap=invalid,zero,overflow ")
  endif()
endif()
if (${CMAKE_Fortran_COMPILER_ID}  MATCHES  "Intel")
  set(CMAKE_Fortran_FLAGS   "${CMAKE_Fortran_FLAGS} -g -O0  -debug extended -traceback  -fpe0  -fpp  -diag-disable=6371 -diag-disable=6375  -diag-disable=7784 -diag-disable=7713 -diag-disable=8291 -noalign -noauto -zero -nbs -extend-source 132")
endif()
if (${CMAKE_Fortran_COMPILER_ID}  MATCHES  "NAG")
  set(CMAKE_Fortran_FLAGS   "${CMAKE_Fortran_FLAGS} -g -fpp -f90 -dusty  -save -mismatch_all    -w=unused -w=uda -w=x77 -w=ques -w=obs")
endif()
if (${CMAKE_Fortran_COMPILER_ID}  MATCHES  "XL")
  set(CMAKE_Fortran_FLAGS   "${CMAKE_Fortran_FLAGS} -g -O0 -qfixed=72 -qsigtrap -qsave  -qmaxmem=-1 -qextname -qfloat=hsflt:hssngl:nans -qcharlen=32767 -qxlf77=leadzero -qfullpath -qctyplss -qintlog  -qsuppress=1520-065  ")
endif()
if (${CMAKE_Fortran_COMPILER_ID}  MATCHES  "PGI")
  set(CMAKE_Fortran_FLAGS   "${CMAKE_Fortran_FLAGS} -g -O0 -traceback -Msave -Mextend -Ktrap=fp -Mpreprocess  ")
endif()
if (${CMAKE_Fortran_COMPILER_ID}  MATCHES  "SunPro")
  set(CMAKE_Fortran_FLAGS   "${CMAKE_Fortran_FLAGS} -g -O0 -f77 -fixed -ftrap=%none -DGAMMA=SUNGAMMA -xpp=cpp")
endif()
message(STATUS "Fortran compiler        : ${CMAKE_Fortran_COMPILER_ID}")
message(STATUS "Fortran compiler flags  : ${CMAKE_Fortran_FLAGS}")
########################################################################
#Setup C compiller
include(CheckCCompilerFlag)
set(C_FLAGS_TO_CHECK  "-Wno-implicit-function-declaration" "-Wno-implicit-int" "-Wno-return-type" )
foreach(fl ${C_FLAGS_TO_CHECK})
  CHECK_C_COMPILER_FLAG(${fl} C_COMPILER_SUPPORTS_${fl})
  if(C_COMPILER_SUPPORTS_${fl})
    set(CMAKE_C_FLAGS "${CMAKE_C_FLAGS} ${fl}")
  endif()
endforeach(fl ${C_FLAGS_TO_CHECK})
if (${CMAKE_C_COMPILER_ID}  MATCHES "Clang")
  set(CMAKE_C_FLAGS "${CMAKE_C_FLAGS} -g -O1 -Wno-conditional-uninitialized -Wno-incompatible-library-redeclaration ")
endif()
if (${CMAKE_C_COMPILER_ID}  MATCHES "GNU")
  set(CMAKE_C_FLAGS "${CMAKE_C_FLAGS} -g -O1")
endif()
if (${CMAKE_C_COMPILER_ID}  MATCHES  "Intel")
  set(CMAKE_C_FLAGS   "${CMAKE_C_FLAGS} -g -O1  -debug extended -traceback   -diag-disable=2196  -diag-disable=161  -diag-disable=303   ")
endif()
if (${CMAKE_C_COMPILER_ID}  MATCHES  "XL")
  set(CMAKE_C_FLAGS   "${CMAKE_C_FLAGS} -g -O1 ")
endif()
if (${CMAKE_C_COMPILER_ID}  MATCHES  "PGI")
  set(CMAKE_C_FLAGS   "${CMAKE_C_FLAGS} -g -O1 ")
endif()
if (${CMAKE_C_COMPILER_ID}  MATCHES  "SunPro")
  set(CMAKE_C_FLAGS   "${CMAKE_C_FLAGS} -g -O1")
endif()
message(STATUS "C compiler            : ${CMAKE_C_COMPILER_ID}")
message(STATUS "C compiler flags      : ${CMAKE_C_FLAGS}")
########################################################################
#Setup C++ compiller
include(CheckCXXCompilerFlag)
set(CXX_FLAGS_TO_CHECK "-std=c++11" "-std=c++1y" "-Wno-implicit-int" )
foreach(fl ${CXX_FLAGS_TO_CHECK})
  CHECK_CXX_COMPILER_FLAG(${fl} CXX_COMPILER_SUPPORTS_${fl})
  if(CXX_COMPILER_SUPPORTS_${fl})
    set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} ${fl}")
  endif()
endforeach(fl ${CXX_FLAGS_TO_CHECK})

if (${CMAKE_CXX_COMPILER_ID}  MATCHES "Clang")
  set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -g -O1  -Wno-conditional-uninitialized -Wno-incompatible-library-redeclaration -Wno-implicit-function-declaration -Wno-implicit-int -Wno-return-type")
  set(CMAKE_EXE_LINKER_FLAGS "${CMAKE_EXE_LINKER_FLAGS} -lc++ ")
endif()
if (${CMAKE_CXX_COMPILER_ID}  MATCHES "GNU")
  set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -g -O1")
  set(CMAKE_EXE_LINKER_FLAGS "${CMAKE_EXE_LINKER_FLAGS} -lstdc++ ")
endif()
if (${CMAKE_CXX_COMPILER_ID}  MATCHES  "Intel")
  set(CMAKE_CXX_FLAGS   "${CMAKE_CXX_FLAGS} -g -O1  -debug extended -traceback   -diag-disable=2196  -diag-disable=161  -diag-disable=303   ")
endif()
if (${CMAKE_CXX_COMPILER_ID}  MATCHES  "XL")
  set(CMAKE_CXX_FLAGS   "${CMAKE_CXX_FLAGS} -g -O1")
  set(CMAKE_EXE_LINKER_FLAGS "${CMAKE_EXE_LINKER_FLAGS} -L/opt/ibm/lib -L//opt/ibm/xlC/16.1.1/lib/ -libmc++")
endif()
if (${CMAKE_CXX_COMPILER_ID}  MATCHES  "PGI")
  set(CMAKE_CXX_FLAGS   "${CMAKE_CXX_FLAGS} -g -O1")
  set(CMAKE_EXE_LINKER_FLAGS "${CMAKE_EXE_LINKER_FLAGS} -L/opt/nvidia/hpc_sdk/Linux_x86_64/20.9/compilers/lib/  -lstdc++ ")
endif()
if (${CMAKE_CXX_COMPILER_ID}  MATCHES  "SunPro")
  set(CMAKE_CXX_FLAGS   "${CMAKE_CXX_FLAGS} -g -O1")
  #The -lstdc++ is for ROOT
  set(CMAKE_EXE_LINKER_FLAGS "${CMAKE_EXE_LINKER_FLAGS} -L/opt/oracle/developerstudio12.6/lib/compilers/CC-gcc/lib/amd64/  -lgcc_s -lstdc++ ")
  set(CMAKE_EXE_LINKER_FLAGS "${CMAKE_EXE_LINKER_FLAGS} -L/opt/oracle/developerstudio12.6/prod/lib/compilers/rtlibs/usr/lib/amd64/ -lCrunG3")
endif()
message(STATUS "C++ compiler            : ${CMAKE_CXX_COMPILER_ID}")
message(STATUS "C++ compiler flags      : ${CMAKE_CXX_FLAGS}")
########################################################################
message(STATUS "EXE linker flags        : ${CMAKE_EXE_LINKER_FLAGS}")
########################################################################
