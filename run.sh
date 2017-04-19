#!/bin/bash
#######################Compile HepMC3###################################
set -x
mkdir -p jadesoft/bin
export TOP=$(pwd)/installed
mkdir -p $TOP
cd HepMC3/
rm -rf outputs CMakeFiles  CMakeCache.txt
cmake CMakeLists.txt -DCMAKE_Fortran_COMPILER=gfortran  -DHEPMC_ENABLE_ROOTIO=ON -DCMAKE_INSTALL_PREFIX=$TOP
make -f Makefile clean
make -f Makefile 
make -f Makefile  install
cd ..
########################################################################
cd picocernlib
rm -rf outputs CMakeFiles  CMakeCache.txt
cmake CMakeLists.txt -DCMAKE_Fortran_COMPILER=gfortran -DCMAKE_INSTALL_PREFIX=$TOP
make -f Makefile clean
make -f Makefile 
make install
cd ..

########################################################################
cd jadesoft
rm -rf outputs CMakeFiles  CMakeCache.txt
cmake CMakeLists.txt -DCMAKE_Fortran_COMPILER=gfortran  -DCMAKE_INSTALL_PREFIX=$TOP  -DPICOCERNLIB=$TOP/lib64/libpicocernlib.a
make -f Makefile clean
make -f Makefile 
make install
cd ..
########################################################################
cd convert
rm -rf outputs CMakeFiles  CMakeCache.txt
cmake CMakeLists.txt -DCMAKE_Fortran_COMPILER=gfortran  -DCMAKE_INSTALL_PREFIX=$TOP  -DHEPMC3_ROOT_DIR=$TOP 
make -f Makefile clean
make -f Makefile 
make install
cd ..

########################################################################
cd jtuple
rm -rf outputs CMakeFiles  CMakeCache.txt
cmake CMakeLists.txt -DCMAKE_Fortran_COMPILER=gfortran  -DCMAKE_INSTALL_PREFIX=$TOP  -DPICOCERNLIB=$TOP/lib64/libpicocernlib.a -DJADELIB_ROOT_DIR=$TOP
make -f Makefile clean
make -f Makefile
make install
cd ..
########################################################################
cd fptobos
rm -rf outputs CMakeFiles  CMakeCache.txt
cmake CMakeLists.txt -DCMAKE_Fortran_COMPILER=gfortran  -DCMAKE_INSTALL_PREFIX=$TOP  -DPICOCERNLIB=$TOP/lib64/libpicocernlib.a -DJADELIB_ROOT_DIR=$TOP
make -f Makefile clean
make -f Makefile
make install
cd ..
