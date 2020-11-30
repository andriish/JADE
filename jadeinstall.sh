#!/bin/bash
if [ "$(uname)" == "Darwin" ]; then
  export PATH=/usr/local/bin:$PATH
  export MACOSX_DEPLOYMENT_TARGET=10.12
  if [ -z ${CMAKE+x} ]; then
    CMAKE=cmake
    CTEST=ctest
  fi
else
#If the name $CMAKE is not set, the cmake name will default to cmake.
#Unless cmake programm has version 2 and cmake3 program with version 3 exists. 
  if [ -z ${CMAKE+x} ]; then
    CMAKE=cmake
    CTEST=ctest
    which $CMAKE
    if [ "$?" = "0" ]; then 
      cmake_version=$( $CMAKE --version | head -n 1 | cut -f 3 -d' ' | cut -f1 -d. )
    else
      cmake_version="2"
    fi
    which cmake3
    if [ "$?" = "0" ]; then 
      cmake3_version=$( cmake3 --version | head -n 1 | cut -f 3 -d' ' | cut -f1 -d. )
    else
      cmake3_version="2"
    fi
    if [ "$cmake_version" = "2" ] && [ "$cmake3_version" = "3" ] ; then
      CMAKE=cmake3
      CTEST=ctest3
    fi
 fi
fi
which $CMAKE
if [ "$?" != "0" ]; then 
  echo 'cannot locate cmake' ; exit 1; 
fi


tmpprefix=$( echo "$*" | egrep -- '--\<prefix\>' | cut -f2 -d=)
if test -n "$tmpprefix"; then
 if [ "$(uname)" == "Darwin" ]; then
 export TOP=$(greadlink -f $tmp)
 else
 export TOP=$(readlink -f $tmp)
 fi
else
 export TOP=$(pwd)/installed
fi
########################################################################
tmptoolchain=$( echo "$*" | egrep -- '--\<toolchain\>' | cut -f2 -d=)
toolchain=GNU
if test -z "$tmptoolchain"; then
 tmptoolchain="GNU"
fi
if  [ "$tmptoolchain" != "GNU" ] && [ "$tmptoolchain" !=  "Intel" ]; then
 echo "Unknown toolchain "$tmptoolchain" using GNU instead."
 echo "Possible values for the toolchain are Intel, GNU, and XL"
else 
 toolchain=$tmptoolchain
fi
echo "Used toolchain="$toolchain
##This is for Intel on Linux
if [ "$(uname)" = "Linux" ] && [ "$toolchain" = "Intel" ]; then
 .  /opt/intel/oneapi/setvars.sh
 export CC=icc
 export CXX=icpc
 export FC=ifort
fi
##This is for GNU on Linux
if [ "$(uname)" = "Linux" ] && [ "$toolchain" = "GNU" ]; then
 export CC=gcc
 export CXX=g++
 export FC=gfortran
fi
##This is for XL on Linux
if [ "$(uname)" = "Linux" ] && [ "$toolchain" = "XL" ]; then
 export CC=xlc
 export CXX=xlC
 export FC=xlf
fi
##This is for Intel on MacOSX
if [ "$(uname)" = "Darwin" ] && [ "$toolchain" = "Intel" ]; then
 .  /opt/intel/oneapi/setvars.sh
 export CC=icc
 export CXX=icpc
 export FC=ifort
fi
##This is for GNU/Clang on MacOSX
if [ "$(uname)" = "Darwin" ] && [ "$toolchain" = "GNU" ]; then
 export CC=clang
 export CXX=clang++
 export FC=gfortran
fi
########################################################################
mkdir -p build/picocernlib
cd build/picocernlib
rm -rf outputs CMakeFiles CMakeCache.txt
$CMAKE -H../../picocernlib -B. -DCMAKE_Fortran_COMPILER=$FC  -DCMAKE_CXX_COMPILER=$CXX  -DCMAKE_C_COMPILER=$CC -DCMAKE_INSTALL_PREFIX=$TOP
make -f Makefile clean
make -f Makefile -j 2 || { echo 'make failed' ; exit 1; }
make install
cd ../..
########################################################################
mkdir -p build/jadesoft
cd build/jadesoft
rm -rf outputs CMakeFiles CMakeCache.txt
$CMAKE -H../../jadesoft -B. -DCMAKE_Fortran_COMPILER=$FC  -DCMAKE_CXX_COMPILER=$CXX  -DCMAKE_C_COMPILER=$CC -DCMAKE_INSTALL_PREFIX=$TOP -DPICOCERNLIBPREFIX=$TOP  -DJADE_USE_CERNLIB:BOOL=OFF
make -f Makefile clean
make -f Makefile -j 2 || { echo 'make failed' ; exit 1; }
make install
cd ../..
########################################################################
mkdir -p build/convert
cd build/convert
rm -rf outputs CMakeFiles CMakeCache.txt
$CMAKE -H../../convert -B. -DCMAKE_Fortran_COMPILER=$FC  -DCMAKE_CXX_COMPILER=$CXX  -DCMAKE_C_COMPILER=$CC -DCMAKE_INSTALL_PREFIX=$TOP 
make -f Makefile clean
make -f Makefile -j 2 || { echo 'make failed' ; exit 1; }
make install
cd ../..
########################################################################
mkdir -p build/jtuple
cd build/jtuple
rm -rf outputs CMakeFiles CMakeCache.txt
$CMAKE -H../../jtuple -B. -DCMAKE_Fortran_COMPILER=$FC  -DCMAKE_CXX_COMPILER=$CXX  -DCMAKE_C_COMPILER=$CC -DCMAKE_INSTALL_PREFIX=$TOP -DPICOCERNLIBPREFIX=$TOP -DJADE_USE_CERNLIB:BOOL=OFF
make -f Makefile clean
make -f Makefile -j 2 || { echo 'make failed' ; exit 1; }
make install
cd ../..
########################################################################
mkdir -p build/fptobos
cd build/fptobos
rm -rf outputs CMakeFiles CMakeCache.txt
$CMAKE -H../../fptobos -B. -DCMAKE_Fortran_COMPILER=$FC  -DCMAKE_CXX_COMPILER=$CXX  -DCMAKE_C_COMPILER=$CC -DCMAKE_INSTALL_PREFIX=$TOP -DPICOCERNLIBPREFIX=$TOP -DJADE_USE_CERNLIB:BOOL=OFF
make -f Makefile clean
make -f Makefile -j 2 || { echo 'make failed' ; exit 1; }
make install
cd ../..
