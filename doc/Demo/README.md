# JADE

mkdir -p DEMO
export PATH=$PATH:$(pwd)/installedGNU64/bin
cd DEMO
cp ../jadesoft/job2016/superv/mc_t86-05-17.bos ./
cp ../installedGNU64/share/JADESOFT/cal/* ./
cp  bupdat0.b F11LHO.BUPDAT0
cp  aupdat1.b F11LHO.AUPDAT1
export GFORTRAN_CONVERT_UNIT=big_endian\;native:2
jadez

