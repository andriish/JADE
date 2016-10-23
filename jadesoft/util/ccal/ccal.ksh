#!/bin/ksh
#
# Shell script to compile the ZE4V data reading routine
#
# 19.06.2003  Pedro A. Movilla Fernandez
#

#
. ../../Init_jade_env.ksh

env=`uname`

# compile conversion routine
fort conv.f
Stat=$?
if [ $Stat -ne 0 ]; then
    exit 9
fi
link conv.o
Stat=$?
if [ $Stat -ne 0 ]; then
    exit 9
fi
rm -f conv.o
mv -f conv.run  conv.${env}.run

# compile read routine (for tests)
fort rconv.f
Stat=$?
if [ $Stat -ne 0 ]; then
    exit 9
fi
link rconv.o
Stat=$?
if [ $Stat -ne 0 ]; then
    exit 9
fi
rm -f rconv.o
mv -f rconv.run  rconv.${env}.run

# demo calibration file
rm -f aupdat1
ln -s ${JADE_CAL}/aupdat1

echo ""
echo "conv.${env}.run, rconv.${env}.run created."
echo ""

exit 0
