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


########################################################################
mkdir -p build/root
cd build/root
#rm -rf outputs CMakeFiles CMakeCache.txt
$CMAKE -H../../externals/root -B. -DCMAKE_Fortran_COMPILER=gfortran \
 -DCMAKE_INSTALL_PREFIX=$TOP \
 -Dminimal:BOOL=ON \
 -Dpyroot:BOOL=OFF \
 -Dpyroot_legacy:BOOL=OFF \
 -Dbuiltin_lz4:BOOL=OFF \
 -Dbuiltin_lzma:BOOL=OFF \
 -Dbuiltin_freetype:BOOL=OFF \
 -Dbuiltin_nlohmannjson:BOOL=OFF \
 -Dbuiltin_pcre:BOOL=OFF \
 -Dbuiltin_xxhash:BOOL=ON \
 -Dbuiltin_zstd:BOOL=OFF \
 -DCMAKE_CXX_FLAGS=-m32 \
 -DCMAKE_C_FLAGS=-m32
#make -f Makefile clean
make -f Makefile -j 4 || { echo 'make failed' ; exit 1; }
make install
cd ../..
########################################################################
mkdir -p build/HepMC3
cd build/HepMC3
#rm -rf outputs CMakeFiles CMakeCache.txt
$CMAKE -H../../externals/HepMC3 -B. -DCMAKE_Fortran_COMPILER=gfortran \
 -DCMAKE_INSTALL_PREFIX=$TOP \
 -DCMAKE_CXX_FLAGS=-m32 \
 -DCMAKE_C_FLAGS=-m32
#make -f Makefile clean
make -f Makefile -j 4 || { echo 'make failed' ; exit 1; }
make install
cd ../..
########################################################################