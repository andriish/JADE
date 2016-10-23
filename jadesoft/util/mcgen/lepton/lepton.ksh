#!/usr/bin/ksh
# Job to generate lepton pairs w/o any underlying physics
# 09.11.00, P. Movilla Fernandez
#
# 19.06.03, PMF: modified for use with a minimal JADE environment
#                + further modifications
#

# Time stamp:
echo
echo 'Start job at: '`date`
echo

#
. ../../../Init_jade_env.ksh

# Switch for making of executable:
MAKEEXE=OFF
if [ x"$1" = 'xcomp' ]; then MAKEEXE=ON; fi

# Number of events to generate
EVENTS=1000

# Center-of-mass energy
ECM=12.0

# Lepton flavour: 1=electron, 2=muon
LEPTON=1

# Set the name of the .cprod (and .evki) file for this run:
NAME=lepton.${ECM}.${LEPTON}

# Go to scratch directory:
# cd ~/mc/lepton

# Make the executable if demanded:
if [ $MAKEEXE = ON ] ; then

# Compose cradle file:
cat > lepton.cra <<EOF
+USE,P=LEPTON.
+EXE.
+PAM,13,T=A,C. lepton.car
+QUIT.
EOF

# Run patchy to get the code:
ytofort lepton
rc=$?
if [ $rc -ne 0 ] ; then
     echo 'lepton: patchy return code' $rc
     echo `whoami` -m 'py57_qq: patchy wrong'
     exit $rc
fi
# Compile, output goes in subdirectory:
fort lepton.f
rc=$?
if [ $rc -ne 0 ] ; then
     echo 'lepton: compilation return code' $rc
     echo `whoami` -m 'lepton: compilation wrong'
     exit $rc
fi

# Link it:
if [ $HARDWARE = "RS6000" ]; then
    ulink=""
else
    ulink="-uludata_ -upydata_ -umcevbl_ -umcpobl_"
fi
link -m $ulink lepton.o

rc=$?
if [ $rc -ne 0 ] ; then
  echo 'lepton: bad news, link return code' $rc
  echo `whoami` -m 'mc-comp_link: no executable made'
  exit $rc
fi

mkdir -p ${JADE_MCBIN}
if [ -x lepton.run ]; then
    mv -f lepton.run ${JADE_MCBIN}/.
    echo "lepton.run moved to ${JADE_MCBIN}/."
else
    echo "Compilation error!"
    exit 9
fi

# Delete temporary files:
rm -f lepton.cra
rm -f lepton.lis
#rm -f lepton.f
rm -f lepton.o
exit
# End of optional making of executable:
fi

# Delete files if neccessary:
echo Will try to remove $NAME.cprod
rm -f $NAME.cprod

# Execute:
${JADE_MCBIN}/lepton.run << EOF
LIST
C
C Cards for MC package:

C Define the name of the 4-vector output file:
MCEVFILE '$NAME.cprod'
C Format of the 4-vector output file:
C 'A': ASCII
C 'B': Binary
FORMAT 'B'

C Number of events generate
NEVENT $EVENTS

C CENTER OF MASS ENERGY
CMS    $ECM

C LEPTON FLAVOUR TO GENERATE
C TYPE=1: ELECTRON
C     =2: MUON
TYPE ${LEPTON}

STOP
EOF

# Time stamp:
echo
echo 'Finish job at: '`date`
echo

# The End
exit
