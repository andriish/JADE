#!/bin/bash
# Tests of the installed JADE software. The work is done by the CMake project test/ (it reads the
# JADEConfig.cmake of the installation: toolchain, bits, compilers); this script only translates the options.
# Without it:
#   cmake -S test -B buildGNU64/test -DCMAKE_PREFIX_PATH=<install prefix> && cmake --build buildGNU64/test && (cd buildGNU64/test && ctest --timeout 200 --output-on-failure)
#Options: --bits=32|64 --toolchain=NAME --prefix=/abs/path --timeout-factor=N, in any order
# (--timeout-factor=N multiplies the time limits of the tests, 20 for slow machines, e.g. s390x under QEMU;
#  --prefix as for jadeinstall.sh; JADE_FAIL_ON_SKIP=1 (used by CI): skipped tests count as failures)
tmpprefix=""; tmptoolchain=""; tmpbits=""; timeout_factor=1
for arg in "$@"; do
  case "$arg" in
    --prefix=*) tmpprefix="${arg#*=}" ;;
    --toolchain=*) tmptoolchain="${arg#*=}" ;;
    --bits=*) tmpbits="${arg#*=}" ;;
    --timeout-factor=*) timeout_factor="${arg#*=}" ;;
  esac
done
if [ "$(uname)" == "Darwin" ]; then
  export PATH=/usr/local/bin:$PATH
  if [ -z ${CMAKE+x} ]; then CMAKE=cmake; CTEST=ctest; fi
else
  if [ -z ${CMAKE+x} ]; then
    CMAKE=cmake; CTEST=ctest
    cmake_version=2; cmake3_version=2
    which cmake >/dev/null 2>&1 && cmake_version=$( cmake --version | head -n 1 | cut -f 3 -d' ' | cut -f1 -d. )
    which cmake3 >/dev/null 2>&1 && cmake3_version=$( cmake3 --version | head -n 1 | cut -f 3 -d' ' | cut -f1 -d. )
    if [ "$cmake_version" = "2" ] && [ "$cmake3_version" = "3" ] ; then CMAKE=cmake3; CTEST=ctest3; fi
  fi
fi
which $CMAKE >/dev/null 2>&1 || { echo 'cannot locate cmake' ; exit 1; }

toolchain=${tmptoolchain:-GNU}
if [ "$(uname)" == "Darwin" ]; then bits=64; elif [ "$(uname -m)" == "i686" ]; then bits=32; else bits=64; fi
bits=${tmpbits:-$bits}
if test -n "$tmpprefix"; then
  if [ "$(uname)" == "Darwin" ]; then TOP=$(greadlink -f $tmpprefix); else TOP=$(readlink -f $tmpprefix); fi
else
  TOP=$(pwd)/installed
fi
SRC=$(cd "$(dirname "$0")" && pwd)
build=build$toolchain$bits/test
mkdir -p $build
rm -rf $build/outputs $build/CMakeFiles $build/CMakeCache.txt
$CMAKE -S "$SRC/test" -B $build -DCMAKE_PREFIX_PATH=$TOP$toolchain$bits -DJADE_TEST_TIMEOUT_FACTOR=$timeout_factor || { echo 'cmake configuration failed' ; exit 1; }
$CMAKE --build $build -j 2 || { echo 'cmake build failed' ; exit 1; }
cd $build
$CTEST . --timeout 200 --output-on-failure
exit $?
