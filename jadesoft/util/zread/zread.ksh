#!/bin/ksh
#
# Shell script to compile the ZE4V data reading routine
#
# 19.06.2003  Pedro A. Movilla Fernandez
#


#
. ../../Init_jade_env.ksh


fort zread.f
Stat=$?
if [ $Stat -ne 0 ]; then
    exit 9
fi
link zread.o
Stat=$?
if [ $Stat -ne 0 ]; then
    exit 9
fi

rm -f zread.o

env=`uname`
mv -f zread.run  zread.${env}.run

echo ""
echo "zread.${env}.run created."
echo ""

exit 0
