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
set -x

tmp=$( echo "$*" | egrep -- '--\<prefix\>' | cut -f2 -d=)
if test -n "$tmp"; then
 if [ "$(uname)" == "Darwin" ]; then
 export TOP=$(greadlink -f $tmp)
 else
 export TOP=$(readlink -f $tmp)
 fi
else
 export TOP=$(pwd)/installed
fi

if [ "$(uname)" == "Darwin" ]; then
  export PYTHIA8_ROOT_DIR=/usr/local/Cellar/pythia/8.243
fi
########################################################################
mkdir -p build/test
cd build/test
#rm -rf outputs CMakeFiles CMakeCache.txt
$CMAKE -H../../test -B. -DCMAKE_Fortran_COMPILER=gfortran  -DJADEPREFIX=$TOP
#make -f Makefile clean
make -f Makefile -j 2 || { echo 'make failed' ; exit 1; }
$CTEST -H../../test -B.
$CTEST .
cd ../..