#!/bin/bash
#Options: --bits=32|64 --toolchain=NAME --prefix=/abs/path, in any order
tmpprefix=""; tmptoolchain=""; tmpbits=""
for arg in "$@"; do
  case "$arg" in
    --prefix=*) tmpprefix="${arg#*=}" ;;
    --toolchain=*) tmptoolchain="${arg#*=}" ;;
    --bits=*) tmpbits="${arg#*=}" ;;
  esac
done
if [ "$(uname)" == "Darwin" ]; then
  export PATH=/usr/local/bin:$PATH
  export MACOSX_DEPLOYMENT_TARGET=12.0
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
    which cmake3 >/dev/null 2>&1
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


tmp="$tmpprefix"
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
toolchain=GNU
if test -z "$tmptoolchain"; then
 tmptoolchain="GNU"
fi
if  [ "$tmptoolchain" != "GNU" ] && [ "$tmptoolchain" != "GNU-11" ]&& [ "$tmptoolchain" != "GNU-12" ] && [ "$tmptoolchain" !=  "Intel" ] && [ "$tmptoolchain" != "NAG" ] && [ "$tmptoolchain" !=  "XL" ] && [ "$tmptoolchain" !=  "PGI" ] && [ "$tmptoolchain" !=  "SUN" ]; then
 echo "Unknown toolchain "$tmptoolchain" using GNU instead."
 echo "Possible values for the toolchain are Intel, GNU, and XL"
else 
 toolchain=$tmptoolchain
fi
echo "Used toolchain="$toolchain
##This is for Intel on Linux
if [ "$(uname)" = "Linux" ] && [ "$toolchain" = "Intel" ]; then
 . /opt/intel/oneapi/setvars.sh --force
 export CC=icx
 export CXX=icpx
 export FC=ifx
  which ifx icx icpx >/dev/null 2>&1 || { echo "Intel compilers (ifx, icx, icpx) not found after setvars.sh; install intel-oneapi-compiler-fortran and intel-oneapi-compiler-dpcpp-cpp"; exit 1; }
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
##This is for NAG on Linux
if [ "$(uname)" = "Linux" ] && [ "$toolchain" = "NAG" ]; then
  export LD_LIBRARY_PATH=$LD_LIBRARY_PATH:$(dirname $(which nagfor))/../lib/NAG_Fortran/
  export CC=gcc
  export CXX=g++
  export FC=nagfor
fi
##This is for PGI on Linux
if [ "$(uname)" = "Linux" ] && [ "$toolchain" = "PGI" ]; then
  # NVIDIA HPC SDK: the newest installed version
  NVHPC_BIN=$(ls -d /opt/nvidia/hpc_sdk/Linux_x86_64/[0-9]*.[0-9]*/compilers/bin 2>/dev/null | sort -V | tail -1)
  if [ ! -x "$NVHPC_BIN/nvfortran" ]; then echo "nvfortran not found in /opt/nvidia/hpc_sdk/Linux_x86_64/*/compilers/bin"; exit 1; fi
  export PATH=$NVHPC_BIN:$PATH
  export CC=nvc
  export CXX=nvc++
  export FC=nvfortran
fi
##This is for SUN on Linux
if [ "$(uname)" = "Linux" ] && [ "$toolchain" = "SUN" ]; then
  export PATH=/opt/oracle/developerstudio12.6/bin:$PATH
  export LD_LIBRARY_PATH=/opt/oracle/developerstudio12.6/lib:$LD_LIBRARY_PATH
  export CC=suncc
  export CXX=sunCC
  export FC=sunf77
fi
##This is for Intel on MacOSX
if [ "$(uname)" = "Darwin" ] && [ "$toolchain" = "Intel" ]; then
  . /opt/intel/oneapi/setvars.sh --force
  export CC=icx
  export CXX=icpx
  export FC=ifx
fi
##This is for GNU/Clang on MacOSX
if [ "$(uname)" = "Darwin" ] && [ "$toolchain" = "GNU-11" ]; then
  export CC=gcc-11
  export CXX=clang++
  export FC=gfortran-11
fi
if [ "$(uname)" = "Darwin" ] && [ "$toolchain" = "GNU-12" ]; then
  export CC=gcc-12
  export CXX=clang++
  export FC=gfortran-12
fi
if [ "$(uname)" == "Darwin" ]; then
  bits=64
else
 if [ "$(uname -m)" == "i686" ]; then
   bits=32
 else
   bits=64
 fi
fi
if test -z "$tmpbits"; then
  tmpbits=$bits
fi
if  [ "$tmpbits" != "32" ] && [ "$tmpbits" != "64" ] ; then
  echo "Unknown bits "$tmpbits" using 64 instead."
  echo "Possible values for the bits are 32 and 64"
else 
  bits=$tmpbits
fi

if  [ "$tmpbits" == "32" ]; then
  bit_arguments="-DJADE_FORCE_32:BOOL=ON"
else
  bit_arguments=" "
fi

########################################################################
mkdir -p build/test
cd build/test
rm -rf outputs CMakeFiles CMakeCache.txt
$CMAKE -S ../../test -B . -DCMAKE_Fortran_COMPILER=$FC  -DCMAKE_CXX_COMPILER=$CXX  -DCMAKE_C_COMPILER=$CC  -DJADESOFT_DIR=$TOP$toolchain$bits/share/JADESOFT/cmake $bit_arguments
$CMAKE --build  . -j 2 || { echo 'cmake build failed' ; exit 1; }
$CMAKE --install .
$CTEST .  --timeout 10 --output-on-failure
ctest_status=$?
#$CTEST -H../../test -B.  --timeout 10 --rerun-failed
cd ../..
exit $ctest_status
