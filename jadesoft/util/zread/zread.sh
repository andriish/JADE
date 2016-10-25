#!/bin/bash
#
# Shell script to compile the ZE4V data reading routine
#
# 19.06.2003  Pedro A. Movilla Fernandez
#
# 25.10.2016  AV, bash version

#
. ../../Init_jade_env.sh


gfortran -m32  -c zread.f
Stat=$?
if [ $Stat -ne 0 ]; then
    exit 9
fi
gfortran -m32 -o zread.run  zread.o  $(cernlib-static )
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
