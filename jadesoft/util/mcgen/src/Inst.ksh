#!/bin/ksh
#
# Shell script to compile some event generator code 
# and analyses libraries from OPAL
#
# 19.06.2003  Pedro A. Movilla Fernandez
#

. ../../../Init_jade_env.ksh

# basenames of car files to install
genl="ariadne408 coj623d herwig58d herwig59 jt63jade jt74opal pythia5722 ckern104 ckern105 px113 px114"

for car in $genl; do
 echo "--------------------------------------"
 echo "... installing $car"
 echo "--------------------------------------"

 ytofort ${car}.cra 
 Stat=$?
 if [ $Stat -ne 0 ]; then
    exit 9
 fi
 ftolib ${car}.f
 Stat=$?
 if [ $Stat -ne 0 ]; then
    exit 9
 fi
 rm -rf ${car}
 #rm -f ${car}.f 
 mkdir -p ${JADE_MCLIB}
 mv -f ${car}.a ${JADE_MCLIB}/lib${car}.a

 echo "***"
 echo "*** library ${JADE_MCLIB}/lib${car}.a created."
 echo "***"

done

exit 0
