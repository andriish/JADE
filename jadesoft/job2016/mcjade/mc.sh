#!/usr/bin/ksh
##########################################
#
# Steering job for MCJADE.
#
#      28/05/00  Pedro Movilla Fernandez
#      24/06/03  PMF last mod.
#      26/10/16  AV
#
##########################################

# Check current platform
if [ `uname` != 'Linux' ] ; then
   echo Current platform is not supported.
   exit
fi
. ../../Init_jade_env.sh
set -x
# Time stamp:
echo
echo 'Start MCJADE job at: '`date`
echo

# Initialise random number seed:
MCSTART=ON

# Set JADE detector configuration:
YEAR=86; MONTH=05; DAY=17
#YEAR=81; MONTH=07; DAY=17

# Set name of the 4-vector input file (no more than 78 characters)
#VECT=`pwd`/demo.py57isr_35.aix.cprod
VECT=`pwd`/demo.py57isr_35.linux.cprod

# Set the name of the .hist, .bos and .bnk files for this run
NAME=mc_t$YEAR-$MONTH-$DAY

# Go to scratch directory:
job=$$
out=out
mkdir -p $out
cd $out

# Delete files if neccessary:
if [ $MCSTART = 'ON' ] ; then
    echo Will remove mcjade.stat.
    rm -f mcjade.stat
fi

# Execute:
time mcjade << EOF
LIST

C Initialise random number generator
MCSTART $MCSTART

C Configuration of the JADE detector
YEAR    $YEAR
MONTH   $MONTH
DAY     $DAY

C Tracking options
C --- Smear gamma and electron energies
SMEAR    ON
C --- Gamma conversion in outer tank and coil
CONVERT  ON
C --- Absorption losses
ABSORB   ON
C --- 3 dim shower profile fit to eggs code
SHOW3D   ON
C --- Vertex chamber tracking:
C     Normally, this option is set automatically in MCJADE
C     VERTEX = ON : Vertex chamber tracking is performed
C            = OFF: Old beam pipe geometry and beam pipe counters
C                   is considered (before Mai 84)
VERTEX   ON

C First and last event to be tracked
C     LAST=0: process until end of 4-vector 'CPROD' file
FIRST   1
LAST    100
C 4-Vector print out
MCNPRI  100

C Name of the 4-vector input file
MCVECT
'$VECT'
C Name of the bos banks output file
MCBOS '$NAME.bos'

C Print out of BOS banks in readable ASCII format
C (memory consuming, only for tests!!!)
BANKS   OFF
MCBANK '$NAME.bnk'

C Control histograms
HISTO   ON
MCHIST '$NAME.hist'
C
STOP
EOF

# Time stamp:
echo
echo 'Finish MCJADE job at: '`date`
echo

# The End
exit
