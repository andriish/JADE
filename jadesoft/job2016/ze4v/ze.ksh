#!/usr/bin/ksh
##########################################
#
# Steering job for the ZE4V job.
#
#      05/06/00  Pedro Movilla Fernandez
#      24/06/03  PMF last mod.
#
##########################################

# Check current platform
if [ `uname` != 'AIX' ] ; then
   echo Current platform is not supported.
   exit
fi
. ../../Init_jade_env.ksh

# Time stamp:
echo
echo 'Start ZE4V job at: '`date`
echo

# Initialise random number seed for dEdx MC
ZSTART=ON

# Set input BOS file
SVBOS=`pwd`/sv_t86-05-17.bos

# Set calibration file: Use either AUPDAT1 or ( BUPDAT0 and BUPDAT1 )
AUPDAT0=${JADE_CAL}/aupdat1.b
#BUPDAT0=${JADE_CAL}/bupdat0.b
#BUPDAT1=${JADE_CAL}/bupdat1.b

# Set the name of the .hist, .bos and .bnk ouput files for this run
NAME=ze_t86-05-17

# Go to scratch directory:
out=out
mkdir -p $out
cd $out

# Delete files if neccessary:
if [ $ZSTART = 'ON' ] ; then
    echo Will remove ze4v.stat.
    rm -f ze4v.stat
fi

# Execute:
time ze4v << EOF
LIST

C Initialise random number generator
ZSTART $ZSTART

C Input file with the JADE events in BOS format
SVBOS
'$SVBOS'

C Max number of events to read from this file
C EVTREAD=0: process until end of input file
EVTREAD  0

C Initial event skip
EVTSKP   0

C Output file with compressed JADE BOS events in ZE4V format
ZE4V  '$NAME.ze4v'

C Max number of compressed BOS events to write out into this file 
C  ZE4VOUT=0: write out until end of reading input file
ZE4VOUT 0

C Max number of ZE4V-bank prints
ZE4VPRT 30

C Max number of full JADE BOS events to write out
C  EVTOUT=0: write out until end of reading input file
EVTOUT  0

C TP/PK-flag (1=TP/2=PK)
TPPK 2

C Selection (0:enab/1:disab)
SELECT 1
C Muon quality selection
MSELCT 0
C Elektron selection
ESELCT 0

C Output file with the JADE events in BOS format
ZBOS
'$NAME.bos'

C Print out of event BOS banks in readable ASCII format
C (memory consuming, only for tests!!!)
BANKS   ON
ZBANK  '$NAME.bnk'

C If BANKS=ON, specify the BOS banks whose contents should
C be printed out in readable form
CONT 'HEAD ALGN LGCL PATR ZVTX'

C Name of the calibration file
AUPDAT0 '$AUPDAT0'
BUPDAT0 '$BUPDAT0'
BUPDAT1 '$BUPDAT1'

C
STOP
EOF

# Time stamp:
echo
echo 'Finish ZE4V job at: '`date`
echo

# The End
exit
