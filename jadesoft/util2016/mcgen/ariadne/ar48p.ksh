#!/usr/bin/ksh
# Job to generate 4-vector MC with PYTHIA/ARIADNE 4.08
# Modifications:
# 19.09.96, STK: remove symbolic links to *BINS.DAT with MC101,
# 20.09.96, STK: copy from hw58_qq.scr, replace hw58 by py57(21)
# 24.09.96, STK: mods for PYTHIA/JETSET version differences
#
# 11.05.00, PMF: adapted for use on the Linux/AIX platforms at the MPI
#                + some further mods
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

# Switches for 4-vector writing:
MC4VEC=OFF
MCSTART=ON

# Set the name of the .hist, .dat, .cprod (and .evki) file for this run:
NAME=ar48p

# Set the version of MC package:
VERSION=105j
echo The version of the MC package is: mc$VERSION

# Go to scratch directory:
# cd ~/mc/ariadne

# Make the executable if demanded:
if [ $MAKEEXE = ON ] ; then

# Compose cradle file:
cat > mcar48p_$VERSION.cra <<EOF
+SELF.+USE,HPUX,ERRTRP.
+USE,TYPE.
+USE,PYJTAR.
+USE,LUCMMN.
+USE,PYCMMN.
+USE,MCCDES.
+EXE.
+USE,P=MCAR48P.
+USE,P=MCUSER.
+PAM,11,R=LUCMMN,T=A,C. jt74opal.car
+PAM,12,R=PYCMMN,T=A,C. pythia5722.car
+PAM,13,T=A,C. mc$VERSION.car
+QUIT.
EOF

# Run patchy to get the code:
ytofort mcar48p_$VERSION
rc=$?
if [ $rc -ne 0 ] ; then
     echo 'ar48p_qq: patchy return code' $rc
     echo `whoami` -m 'ar48p_qq: patchy wrong'
     exit $rc
fi
# Compile, output goes in subdirectory:
fort mcar48p_$VERSION.f
rc=$?
if [ $rc -ne 0 ] ; then
     echo 'ar48p_qq: compilation return code' $rc
     echo `whoami` -m 'ar48p_qq: compilation wrong'
     exit $rc
fi

# Link it:
if [ $HARDWARE = "RS6000" ]; then
    ulink=""
else
    ulink="-uardata_ -uludata_ -upydata_ -umcevbl_ -umcpobl_"
fi
link -m -D $ulink mcar48p_$VERSION.o \
	libmc$VERSION libpythia5722 \
	libariadne408 libjt74opal \
	libckern105 libpx114
rc=$?
if [ $rc -ne 0 ] ; then
  echo 'ar48p_qq: bad news, link return code' $rc
  echo `whoami` -m 'mc-comp_link: no executable made'
  exit $rc
fi

mkdir -p ${JADE_MCBIN}
if [ -x mcar48p_$VERSION.run ]; then
    mv -f mcar48p_$VERSION.run ${JADE_MCBIN}/.
    echo "mcar48p_$VERSION.run moved to ${JADE_MCBIN}/."
else
    echo "Compilation error\!"
    exit 9
fi
# Delete temporary files:
rm -f mcar48p_$VERSION.cra
rm -f mcar48p_$VERSION.lis
#rm -f mcar48p_$VERSION.f
rm -f mcar48p_$VERSION.o
exit
# End of optional making of executable:
fi

# Delete files if neccessary:
if [ $MC4VEC = 'ON' ] ; then
  echo Will try to remove $NAME.evki
  rm -f $NAME.evki
  echo Will try to remove $NAME.cprod
  rm -f $NAME.cprod
  if [ $MCSTART = 'ON' ] ; then
    echo Will try to remove mcrnset.dat and mcrncnt.dat
    rm -f mcrnset.dat
    rm -f mcrncnt.dat
  fi
fi

# Execute:
time ${JADE_MCBIN}/mcar48p_$VERSION.run << EOF
LIST
C
C Cards for MC package:
MCIRUN 6666
MCSTART $MCSTART
MC4VEC  $MC4VEC
C
C Define the name of the 4-vector output file:
C   - extension .cprod forces binary format (JADE 'CPROD' format)
C   - extension .nt forces a HBOOK NTuple format (not yet supported!!!)
C   - other extensions yield to a readable ascii format (for test purposes)
C
MCEVFILE '$NAME.cprod'
MCNEVT 100
MCECMS 35.0
MCNPRI 100
MCTAB  OFF
MCISR  OFF
MCFSR  ON
C List of PDG IDs for particles to be declared as stable if MCSTBL = ON
MCSTBL OFF
MCIDST  3112 3122 3222 3312 3322 3334 310
C        411 421 431
C        511 513 521 523 531 533 541 543 5122
MCBOOS OFF
MCRDM  101234567
C
C QUARK TYPE TO ANALYSE:
C
C MCUDSC = ON: Only UDSC flavours are filled at parton level AND hadron level
C MCBQRK = ON: Only B quark flavour is analysed for partons/hadrons
C MCFLAV = I : I=0: all flavours analysed, I=1-5: Flavour I is analysed
C
MCUDSC OFF
MCBQRK OFF
MCFLAV 0
C
C JETSET 7.4 OPAL standard parameters: b=        0.52  +/- 0.04   PARJ(42)
C                                      sigma_Q=  0.40  +/- 0.03   PARJ(21)
C                                      Q_0=      1.90  +/- 0.50   PARJ(82)
C                                      eps_c=   -0.031 +/- 0.011  PARJ(54)
C                                      eps_b=   -0.0038+/- 0.0010 PARJ(55)
C                                      a=        0.11             PARJ(41)
C                                      Lamb_QCD= 0.250 +/- 0.006  PARJ(81)
JTPB    0.52
JTPSQ   0.40
JTPQ0   1.90
JTPEC  -0.031
JTPEB  -0.0038
JTPA    0.11
JTPLAMB 0.25
C
C
C Jetrates (time saving option) calculation
C
C MCJTEV: Number of events to analyse for jet finding, -1: all
C MCDURH = ON: Analyse using the Durham jet finding
C MCJDE0 = ON: Analyse using the JADE E0 jet finding
C MCCONE = ON: Analyse using the Cone jet finders
C MCCAMJ = ON: Analyse using the Cambridge Jet finder
C MCCKRN = ON: Analyse using the CKERN Cambridge
C
MCJTEV 0
MCJDE0 OFF
MCDURH OFF
MCCONE OFF
MCCAMJ OFF
MCCKRN OFF
C
C User cards for MCUSxx routines:
OUTFILE '$NAME.hist'
FSRRM OFF
ISRRM OFF
STOP
EOF

# Time stamp:
echo
echo 'Finish job at: '`date`
echo

# The End
exit
