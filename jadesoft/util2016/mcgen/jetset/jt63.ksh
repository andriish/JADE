#!/usr/bin/ksh
# Job to generate 4-vector MC with JETSET6.3.
# 01.06.01 P.A.Movilla Ferandez
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
NAME=jt63

# Set the version of MC package:
VERSION=105j
echo The version of the MC package is: mc$VERSION

# Go to scratch directory:
# cd ~/mc/jetset

# Make the executable if demanded:
if [ $MAKEEXE = ON ] ; then

# Compose cradle file:
cat > mcjt63_$VERSION.cra <<EOF
+SELF.+USE,HPUX,ERRTRP.
+USE,TYPE.
+USE,PYJTAR.
+USE,JT63.
+USE,LUCMMN.
+USE,LU63MN.
+USE,MCCDES.
+EXE.
+USE,P=MCJT63.
+USE,P=MCUSER.
+PAM,11,R=LUCMMN,T=A,C. jt74opal.car
+PAM,12,R=LU63MN,T=A,C. jt63jade.car
+SELF.+PAM,12,R=LU63MN,T=A,C. jt63jade.car
+PAM,13,T=A,C. mc$VERSION.car
+QUIT.
EOF

# Run patchy to get the code:
ytofort mcjt63_$VERSION
rc=$?
if [ $rc -ne 0 ] ; then
     echo 'jt63_qq: patchy return code' $rc
     echo `whoami` -m 'jt63_qq: patchy wrong'
     exit $rc
fi
# Compile, output goes in subdirectory:
fort mcjt63_$VERSION.f
rc=$?
if [ $rc -ne 0 ] ; then
     echo 'jt63_qq: compilation return code' $rc
     echo `whoami` -m 'jt63_qq: compilation wrong'
     exit $rc
fi

# Link it:
if [ $HARDWARE = "RS6000" ]; then
    ulink=""
else
    ulink="-uludata_ -uluda63_ -uluedat_ -uluhdat_ -umcevbl_ -umcpobl_"
fi
link -m -D $ulink mcjt63_$VERSION.o  \
	libmc$VERSION libjt63jade \
	libckern105 libpx114
rc=$?
if [ $rc -ne 0 ] ; then
  echo 'jt63_qq: bad news, link return code' $rc
  echo `whoami` -m 'mc-comp_link: no executable made'
  exit $rc
fi

mkdir -p ${JADE_MCBIN}
if [ -x mcjt63_$VERSION.run ]; then
    mv -f mcjt63_$VERSION.run ${JADE_MCBIN}/.
    echo "mcjt63_$VERSION.run moved to ${JADE_MCBIN}/."
else
    echo "Compilation error!"
    exit 9
fi

# Delete temporary files:
rm -f mcjt63_$VERSION.cra
rm -f mcjt63_$VERSION.lis
#rm -f mcjt63_$VERSION.f
rm -f mcjt63_$VERSION.o
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
time ${JADE_MCBIN}/mcjt63_$VERSION.run << EOF
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
MCECMS 14.0
MCNPRI 100
MCTAB  OFF
MCISR  OFF
C FSR not supported in Jetset6.3!
MCFSR  ON
C List of PDG IDs for particles to be declared as stable if MCSTBL = ON
MCSTBL OFF
MCIDST  511 513 521 523 531 533 541 543 5122
C        411 421 431
C  3112 3122 3222 3312 3322 3334 310

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
C JETSET 6.3 JADE standard parameters: b=        0.90   PAR(32)
C                                      sigma_Q=  0.30   PAR(12)
C                                      Q_0=      1.0    PARE(22)
C                                      eps_c=   -0.050  PAR(44)
C                                      eps_b=   -0.010  PAR(45)
C                                      a=        0.50   PAR(31)
C                                      Lamb_QCD= 0.400  PARE(21)
JTPB    0.90
JTPSQ   0.30
JTPQ0   1.0
JTPEC  -0.050
JTPEB  -0.010
JTPA    0.50
JTPLAMB 0.40
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
