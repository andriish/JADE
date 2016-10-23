#!/usr/bin/ksh
##########################################
#
# Steering job for the JADE Supervisor
#
#      28/05/00  Pedro Movilla Fernandez
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
echo 'Start SUPERV job at: '`date`
echo

# Initialise random number seed for smearing MC
SVSTART=ON

# Set input BOS file (no more than 78 characters)
MCBOS=`pwd`/mc_t86-05-17.bos

# Set calibration file: Use either AUPDAT1 or ( BUPDAT0 and BUPDAT1 )
AUPDAT0=${JADE_CAL}/aupdat1.b
#BUPDAT0=${JADE_CAL}/bupdat0.b
#BUPDAT1=${JADE_CAL}/bupdat1.b

# Set the name of the .hist, .bos and .bnk ouput files for this run
NAME=sv_t86-05-17

# Go to scratch directory:
out=out
mkdir -p $out
cd $out

# Delete files if neccessary:
if [ $SVSTART = 'ON' ] ; then
    echo Will remove superv.stat.
    rm -f superv.stat
fi

# Execute:
time superv << EOF
LIST

C Initialise random number generator
SVSTART $SVSTART

C First and last event to be reconstructed
C      LAST=0: process until end of input file
FIRST  1
LAST   0

C Name of the input BOS file
MCBOS
'$MCBOS'

C Reconstruction options
ZSRFTV OFF
VTXFIT OFF

C Calibration constants
C - Name of the calibration file
AUPDAT0 
'$AUPDAT0'
BUPDAT0 
'$BUPDAT0'
BUPDAT1 
'$BUPDAT1'

C - Calibration constants reading flags
C (corresponds to COMMON /CMCCAL/ LBMC(1...15))
MUCA ON
LGMA OFF
TAGS OFF
TOFC OFF
LGST OFF
DEDX OFF
SPTG OFF
RVTX OFF
ZCAL OFF
TAGF OFF
IDJS OFF
VTXC OFF
VTXR OFF
VTXB OFF
VTXF OFF

C Name of the BOS banks output file
SVBOS '$NAME.bos'

C Print out of BOS banks in readable ASCII format
C (memory consuming, only for tests!!!)
BANKS   ON
SVBANK '$NAME.bnk'

C If BANKS=ON, specify the BOS banks whose contents should
C be printed out in readable form for each supervisor level (1-7)
C   Some BOS banks of interest:
C   HEAD ALGN JETC ZVTX PATR JHTL LGCL LATC 
C   ATOF VECT PALL HITL VTXC HTSL TRIG

C LEVEL1 'HEAD LATC ATOF ALGN JETC PATR'
C LEVEL2 'LATC ATOF ALGN JETC PATR HTSL JHTL'
LEVEL2 'HEAD'
LEVEL3 'ALGN'
LEVEL4 'ZVTX'
C LEVEL5 'PATR'
C LEVEL6 'LGCL'
LEVEL7 'PATR LGCL'

C Control histograms
HISTO   ON
SVHIST '$NAME.hist'
C
STOP
EOF

# Time stamp:
echo
echo 'Finish SUPERV job at: '`date`
echo

# The End
exit
