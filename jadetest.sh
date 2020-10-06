#!/bin/bash
if [ "$(uname)" == "Darwin" ]; then
 export PATH=/usr/local/bin:$PATH
 export MACOSX_DEPLOYMENT_TARGET=10.12
 if [ -z ${CMAKE+x} ]; then
 CMAKE=cmake
 fi
else
#If the name $CMAKE is not set, the cmake name will default to cmake.
#Unless cmake programm has version 2 and cmake3 program with version 3 exists. 
 if [ -z ${CMAKE+x} ]; then
 CMAKE=cmake
 cmake_version=$( $CMAKE --version | head -n 1 | cut -f 3 -d' ' | cut -f1 -d. )
 cmake3_version=$( cmake3 --version | head -n 1 | cut -f 3 -d' ' | cut -f1 -d. )
 if [ "$cmake_version" = "2" ] && [ "$cmake3_version" = "3" ] ; then
 CMAKE=cmake3
 fi
 fi
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



########################################################################
mkdir -p build/test
cd build/test
#rm -rf outputs CMakeFiles CMakeCache.txt
$CMAKE -S ../../test -B . -DCMAKE_Fortran_COMPILER=gfortran  -DJADEPREFIX=$TOP
#make -f Makefile clean
make -f Makefile -j 2 || { echo 'make failed' ; exit 1; }
ctest3 -S ../../test -B .
ctest3 .
cd ../..
