#!/bin/bash
set -x
if [ ! -f $1 ]; then
echo "File ->"$1"<- does not exist."
exit
fi

top=$(pwd)
cd ../../jadesoft
make
make install
cd $top


cp  $1 ./fpackdatafile
########################################################################
export GFORTRAN_CONVERT_UNIT='big_endian'
rm -rf datajade.bos
cat fptobos.card   | fptobos > fptobos.log
rm -rf datajade.bos
export GFORTRAN_CONVERT_UNIT='native'
cat fptobos.card   | unpjad > unpjad.log

#exit
########################################################################
#exit
export GFORTRAN_CONVERT_UNIT='big_endian;native:2'
cat superv.card   | superv > superv.log
########################################################################
rm -rf ze4v.ze4v ze4v.bnk ze4v.bos
export GFORTRAN_CONVERT_UNIT='big_endian;big_endian:2,22'
cat ze4v.card     | ze4v > ze4v.log
########################################################################
export GFORTRAN_CONVERT_UNIT='native'
cat  jzread.card  | jzread >jzread.log
########################################################################
#export GFORTRAN_CONVERT_UNIT='native;big_endian:2,22'
#cat  jtjob.card   | jtjob >jtjob.log
########################################################################
#mkdir -p output
#mv *.stat *.bos *.ze4v *.root *.histo *.cprod *.log *.bnk output


