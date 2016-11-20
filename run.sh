#!/bin/bash
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
cmake CMakeLists.txt -DCMAKE_Fortran_COMPILER=gfortran  -DCMAKE_INSTALL_PREFIX=$TOP
make -f Makefile clean
make -f Makefile -j 8
make install
cd ..
exit
########################################################################
cd jtuple
rm -rf outputs CMakeFiles  CMakeCache.txt
cmake CMakeLists.txt -DCMAKE_Fortran_COMPILER=gfortran  -DCMAKE_INSTALL_PREFIX=$TOP
make -f Makefile clean
make -f Makefile -j 8
make install
cd ..
