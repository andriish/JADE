#!/bin/bash
export LD_LIBRARY_PATH=/usr/lib64/SHERPA-MC:$LD_LIBRARY_PATH
export PATH=$PATH:$(pwd)/../jadesoft/bin:./
export BLAS_LIBRARY_PATH=/usr/lib64
export BLAS_SEARCH_LIBS=/usr/lib64/libblas.so
#######################Compile HepMC3###################################
cd ../HepMC3/
rm -rf outputs CMakeFiles
cmake CMakeLists.txt -DCMAKE_Fortran_COMPILER=gfortran  -DHEPMC_ENABLE_ROOTIO=ON -DHEPMC_BUILD_EXAMPLES=ON 
make -f Makefile clean
make -f Makefile -j 8
########################################################################
cd ../picocernlib
cmake CMakeLists.txt -DCMAKE_Fortran_COMPILER=gfortran
make -f Makefile clean
make -f Makefile -j 8
########################################################################
cd ../jadesoft
cmake CMakeLists.txt -DCMAKE_Fortran_COMPILER=gfortran 
make -f Makefile clean
make -f Makefile -j 8
########################################################################
cd ../jtuple
cmake CMakeLists.txt -DCMAKE_Fortran_COMPILER=gfortran
make -f Makefile clean
make -f Makefile -j 8
########################################################################
cd  ../Tests
Sherpa -f Runeesherpa_0.12.dat EVENT_OUTPUT=HepMC_GenEvent[out.hepmc2] HEPMC_TREE_LIKE=1  EVENTS=100
mv out.hepmc2.hepmc2g   sherpa34gev.hepmc2
########################################################################
../HepMC3/outputs/bin/convert_example_JADE.exe hepmc2_jade  sherpa34gev.hepmc2   bsherpa34gev.jade Mode=0:events_limit=100
../HepMC3/outputs/bin/convert_example_JADE.exe hepmc2_jade  sherpa34gev.hepmc2   asherpa34gev.jade Mode=1:events_limit=100
########################################################################
export GFORTRAN_CONVERT_UNIT='native'
cat mcjadecard.txt   | mcjade
########################################################################
export GFORTRAN_CONVERT_UNIT='big_endian;native:2'
cat supervcard.txt   | superv
########################################################################
export GFORTRAN_CONVERT_UNIT='native;big_endian:2,22'
cat ze4vcard.txt     | ze4v
########################################################################
export GFORTRAN_CONVERT_UNIT='native'
cat  jzreadcard.txt  | jzread 
########################################################################
export GFORTRAN_CONVERT_UNIT='native;big_endian:2,22'
cat  jtjobcard.txt   | jtjob
########################################################################

exit
#TO run jadez do
# export big endian and run on the sv_sherpa
export GFORTRAN_CONVERT_UNIT='big_endian;native:50'

