#!/bin/bash
#######################MAC setup###################################
#export ROOTSYS=/Applications/root_v6.09.02/
. /Applications/root_v6.09.02/bin/thisroot.sh
export PATH=/opt/subversion/bin:$PATH
export MACOSX_DEPLOYMENT_TARGET=10.12
#sudo /Applications/CMake.app/Contents/bin/cmake-gui --install=/usr/local/bin
mkdir -p jadesoft/bin

#F_UFMTENDIAN="little;big:10,20" should work with intel

#######################Compile HepMC3###################################
set -x
export TOP=$(pwd)/installed
mkdir -p $TOP
cd HepMC3/
rm -rf outputs CMakeFiles  CMakeCache.txt
cmake CMakeLists.txt -DCMAKE_Fortran_COMPILER=gfortran  -DHEPMC_ENABLE_ROOTIO=ON -DCMAKE_INSTALL_PREFIX=$TOP
make -f Makefile clean
make -f Makefile -j 8
make -f Makefile  install
cd ..
########################################################################
cd picocernlib
rm -rf outputs CMakeFiles  CMakeCache.txt
cmake CMakeLists.txt -DCMAKE_Fortran_COMPILER=gfortran -DCMAKE_INSTALL_PREFIX=$TOP
make -f Makefile clean
make -f Makefile -j 8
make install
cd ..

########################################################################
cd jadesoft
rm -rf outputs CMakeFiles  CMakeCache.txt
cmake -Wno-dev CMakeLists.txt -DCMAKE_Fortran_COMPILER=gfortran  -DCMAKE_INSTALL_PREFIX=$TOP  -DPICOCERNLIB=$TOP/lib/libpicocernlib.a
make -f Makefile clean
make -f Makefile -j 8
make install
cd ..
########################################################################
cd convert
rm -rf outputs CMakeFiles  CMakeCache.txt
cmake CMakeLists.txt -DCMAKE_Fortran_COMPILER=gfortran  -DCMAKE_INSTALL_PREFIX=$TOP  -DHEPMC3_ROOT_DIR=$TOP 
make -f Makefile clean
make -f Makefile -j 8
make install
cd ..

########################################################################
cd jtuple
rm -rf outputs CMakeFiles  CMakeCache.txt
cmake CMakeLists.txt -DCMAKE_Fortran_COMPILER=gfortran  -DCMAKE_INSTALL_PREFIX=$TOP  -DPICOCERNLIB=$TOP/lib/libpicocernlib.a -DJADELIB_ROOT_DIR=$TOP
make -f Makefile clean
make -f Makefile -j 8
make install
cd ..
########################################################################
cd fptobos
rm -rf outputs CMakeFiles  CMakeCache.txt
cmake CMakeLists.txt -DCMAKE_Fortran_COMPILER=gfortran  -DCMAKE_INSTALL_PREFIX=$TOP  -DPICOCERNLIB=$TOP/lib/libpicocernlib.a -DJADELIB_ROOT_DIR=$TOP
make -f Makefile clean
make -f Makefile -j 8
make install
cd ..
