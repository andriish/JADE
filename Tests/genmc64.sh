#!/bin/bash
export LD_LIBRARY_PATH=//usr/lib64/SHERPA-MC/://usr/lib64/SHERPA-MC/:$LD_LIBRARY_PATH:$(pwd)/../ROOTLIBS
export PATH=$PATH:$(pwd)/../jadesoft/bin
Sherpa -f Runeesherpa_0.12.dat EVENT_OUTPUT=HepMC_GenEvent[out.hepmc2] HEPMC_TREE_LIKE=1  EVENTS=100
mv out.hepmc2.hepmc2g   sherpa34gev.hepmc2
cd ../HepMC3/

export GFORTRAN_CONVERT_UNIT='native'
export BLAS_LIBRARY_PATH=/usr/lib64
export LD_LIBRARY_PATH=$LD_LIBRARY_PATH:$BLAS_LIBRARY_PATH
export BLAS_SEARCH_LIBS=/usr/lib64/libblas.so
rm -rf outputs CMakeFiles
cmake CMakeLists.txt -DCMAKE_Fortran_COMPILER=gfortran  -DHEPMC_ENABLE_ROOTIO=OFF -DHEPMC_BUILD_EXAMPLES=ON -DCMAKE_CXX_FLAGS=-m64  -DCMAKE_Fortran_FLAGS=" -m32 -fno-automatic -fno-backslash -ffixed-line-length-132"
make clean
make -j 8

#####
cd ../picocernlib
cmake CMakeLists.txt -DCMAKE_Fortran_COMPILER=gfortran
make -f Makefile
####
cd ../Tests


../HepMC3/outputs/bin/convert_example_JADE.exe hepmc2_jade  sherpa34gev.hepmc2   bsherpa34gev.jade Mode=0:events_limit=100
#../HepMC3/outputs/bin/convert_example_JADE.exe hepmc2_jade  sherpa34gev.hepmc2   asherpa34gev.jade Mode=1:events_limit=100
cd ../jadesoft
cmake CMakeLists.txt -DCMAKE_Fortran_COMPILER=gfortran
make -f Makefile
cd  ../Tests
cat mcjadecard.txt | mcjade

#exit

#h2root sherpa34gev.hist
export GFORTRAN_CONVERT_UNIT='big_endian;native:2'
cat supervcard.txt | superv
export GFORTRAN_CONVERT_UNIT='native;big_endian:2,22'
cat ze4vcard.txt | ze4v


#exit
cd ../jtuple
cmake CMakeLists.txt -DCMAKE_Fortran_COMPILER=gfortran
make -f Makefile
export PATH=$PATH:$(pwd)
cd ../Tests
export GFORTRAN_CONVERT_UNIT='native'
cat  jzreadcard.txt   | jzread 
#h2root jz_sherpa34gev_final.hbook
export GFORTRAN_CONVERT_UNIT='native;big_endian:2,22'
cat  jtjobcard.txt   | jtjob
#h2root jt_sherpa34gev_final.hbook



exit
#TO run jadez do
# export big endian and run on the sv_sherpa
export GFORTRAN_CONVERT_UNIT='big_endian;native:50'

