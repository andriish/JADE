#!/bin/bash
# Build and install of the JADE software. The work is done by the top-level CMakeLists.txt (toolchains,
# compilers, the order jadesoft -> convert -> jtuple -> fptobos -> h2root, the installation and the file
# JADEConfig.cmake); this script only translates the options. Without it:
#   cmake -S . -B buildGNU64 -DJADE_TOOLCHAIN=GNU -DJADE_BITS=64 -DCMAKE_INSTALL_PREFIX=/abs/path && cmake --build buildGNU64
#Options: --bits=32|64 --toolchain=NAME --prefix=/abs/path, in any order
# (--prefix=P installs into P<toolchain><bits>, e.g. P/intel64 needs --prefix=P/ ; default: ./installed<toolchain><bits>)
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
  if [ -z ${CMAKE+x} ]; then CMAKE=cmake; fi
else
  #If the name $CMAKE is not set, the cmake name will default to cmake, unless cmake has version 2 and cmake3 exists.
  if [ -z ${CMAKE+x} ]; then
    CMAKE=cmake
    cmake_version=2; cmake3_version=2
    which cmake >/dev/null 2>&1 && cmake_version=$( cmake --version | head -n 1 | cut -f 3 -d' ' | cut -f1 -d. )
    which cmake3 >/dev/null 2>&1 && cmake3_version=$( cmake3 --version | head -n 1 | cut -f 3 -d' ' | cut -f1 -d. )
    if [ "$cmake_version" = "2" ] && [ "$cmake3_version" = "3" ] ; then CMAKE=cmake3; fi
  fi
fi
which $CMAKE >/dev/null 2>&1 || { echo 'cannot locate cmake' ; exit 1; }

# names of the build directory (as before: build<toolchain><bits>)
toolchain=${tmptoolchain:-GNU}
if [ "$(uname)" == "Darwin" ]; then bits=64; elif [ "$(uname -m)" == "i686" ]; then bits=32; else bits=64; fi
bits=${tmpbits:-$bits}
args="-DJADE_TOOLCHAIN=$toolchain -DJADE_BITS=$bits"
if test -n "$tmpprefix"; then
  if [ "$(uname)" == "Darwin" ]; then TOP=$(greadlink -f $tmpprefix); else TOP=$(readlink -f $tmpprefix); fi
  args="$args -DJADE_PREFIX_BASE=$TOP"
fi
SRC=$(cd "$(dirname "$0")" && pwd)
$CMAKE -S "$SRC" -B build$toolchain$bits $args || { echo 'cmake configuration failed' ; exit 1; }
$CMAKE --build build$toolchain$bits || { echo 'cmake build failed' ; exit 1; }
$CMAKE --install build$toolchain$bits || { echo "cmake install failed" ; exit 1; }
