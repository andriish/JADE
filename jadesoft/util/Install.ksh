#!/bin/ksh
#
# Shell script to compile all utility programs
#
# 19.06.2003  Pedro A. Movilla Fernandez
#

. ../Init_jade_env.ksh

env=`uname`

# Compile the MC libraries
cd ${JADE_UTIL}/mcgen/src/.
${JADE_UTIL}/mcgen/src/Inst.ksh 2>&1 | tee  ${JADE_UTIL}/mcgen/src/Inst.${env}.log

cd ${JADE_UTIL}/mcgen/mc/.
${JADE_UTIL}/mcgen/mc/mc105j_lib.ksh 2>&1 | tee ${JADE_UTIL}/mcgen/mc/Inst.mc105j_lib.${env}.log

# Compile the QCD generator programs
cd ${JADE_UTIL}/mcgen/pythia/.
${JADE_UTIL}/mcgen/pythia/py57.ksh comp 2>&1 | tee ${JADE_UTIL}/mcgen/pythia/Inst.py57.${env}.log

cd ${JADE_UTIL}/mcgen/jetset/.
${JADE_UTIL}/mcgen/jetset/jt63.ksh comp 2>&1 | tee ${JADE_UTIL}/mcgen/jetset/Inst.jt63.${env}.log

${JADE_UTIL}/mcgen/jetset/jt74.ksh comp 2>&1 | tee ${JADE_UTIL}/mcgen/jetset/Inst.jt74.${env}.log

cd ${JADE_UTIL}/mcgen/herwig/.
${JADE_UTIL}/mcgen/herwig/hw58d.ksh comp 2>&1 | tee ${JADE_UTIL}/mcgen/herwig/Inst.hw58d.${env}.log
${JADE_UTIL}/mcgen/herwig/hw59.ksh comp 2>&1 | tee ${JADE_UTIL}/mcgen/herwig/Inst.hw59.${env}.log

cd ${JADE_UTIL}/mcgen/ariadne/.
${JADE_UTIL}/mcgen/ariadne/ar48p.ksh comp 2>&1 | tee ${JADE_UTIL}/mcgen/ariadne/Inst.ar48p.${env}.log

cd ${JADE_UTIL}/mcgen/cojets/.
${JADE_UTIL}/mcgen/cojets/cj623.ksh comp 2>&1 | tee ${JADE_UTIL}/mcgen/cojets/Inst.cj623.${env}.log

cd ${JADE_UTIL}/mcgen/lepton/.
${JADE_UTIL}/mcgen/lepton/lepton.ksh comp 2>&1 | tee ${JADE_UTIL}/mcgen/lepton/Inst.lepton.${env}.log

# Compile the ZE4V reading program
cd ${JADE_UTIL}/zread/.
${JADE_UTIL}/zread/zread.ksh  2>&1 | tee ${JADE_UTIL}/zread/Inst.zread.${env}.log

# Compile the calibration file conversion programs
cd ${JADE_UTIL}/ccal/.
${JADE_UTIL}/ccal/ccal.ksh  2>&1 | tee ${JADE_UTIL}/ccal/Inst.ccal.${env}.log

echo "***"
echo "*** Installation of ${JADE_UTIL} finished."
echo "***"
