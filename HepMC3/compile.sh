export BLAS_LIBRARY_PATH=/usr/lib
export LD_LIBRARY_PATH=$LD_LIBRARY_PATH:$BLAS_LIBRARY_PATH
export BLAS_SEARCH_LIBS=/usr/lib/libblas.so
cmake CMakeLists.txt  -DHEPMC_ENABLE_ROOTIO=OFF -DHEPMC_BUILD_EXAMPLES=ON -DCMAKE_CXX_FLAGS=-m32  -DCMAKE_Fortran_FLAGS=" -m32 -fno-automatic -fno-backslash -ffixed-line-length-132"
