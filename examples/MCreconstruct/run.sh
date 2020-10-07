#!/bin/bash
export GFORTRAN_CONVERT_UNIT='native'
convert_example_JADE.exe hepmc2_jade  $1   temp.jade Mode=0 > convert.log
if [ ! -f temp.jade ]; then
echo "Conversion on ->"$1"<-has failed. See convert.log"
exit
fi
mv  temp.jade mcjadeinput.cprod
########################################################################
export GFORTRAN_CONVERT_UNIT='native'
cat mcjade.card   | mcjade > mcjade.log
########################################################################
export GFORTRAN_CONVERT_UNIT='big_endian;native:2'
cat superv.card   | superv > superv.log
########################################################################
export GFORTRAN_CONVERT_UNIT='native;big_endian:2,22'
cat ze4v.card     | ze4v > ze4v.log
########################################################################
export GFORTRAN_CONVERT_UNIT='native'
cat  jzread.card  | jzread >jzread.log
########################################################################
export GFORTRAN_CONVERT_UNIT='native;big_endian:2,22'
cat  jtjob.card   | jtjob >jtjob.log
########################################################################
mkdir -p output
mv *.stat *.bos *.ze4v *.root *.histo *.cprod *.log *.bnk output 


