#!/usr/bin/ksh 
# Shell script to compile and create 4-vector MC generation library:
# Modifications:
# 29.07.96, STK: add -z flag to ftolib to get debugable library 
# 18.09.96, STK: introduce variable for version of MC package, set to 101,
#                use HERWIG 5.9 cards
# 19.09.96, STK: Mods for use with version 101 of MC
#
# 11.05.00, PMF: adapted for use on the Linux/AIX platforms at the MPI
#                + some further mods
# 13.05.00, PMF: introduce an additional patchy step to choose
#                the event record write out routine.
#                The cradle file must contain
#                 '+USE,OPAL,EVWRTO.'   for OPAL format
#                 '+USE,JADE,EVWRTO.'   for JADE format
#
# 19.06.03, PMF: modified for use with a minimal JADE environment + further modifications
#

# Time stamp:
echo
echo 'Start at:'`date`
echo

#
. ../../../Init_jade_env.ksh

# Set version number:
VERSION=105j
echo
echo 'The MC code version number is:' $VERSION
echo

# Compose cradle files:
cat > mc$VERSION.cra <<EOF
+SELF.+USE,HPUX.
+USE,JADE,EVWRTO.
+USE,TYPE.
+USE,LUCMMN.
+USE,LU63MN.
+USE,MCCDES.
+EXE.
+USE,P=MCKERN.
+USE,P=PXMOD.
+USE,P=JADEEV.
+PAM,11,R=LUCMMN,T=A,C. jt74opal.car
+PAM,12,T=A,C. jt63jade.car
+PAM,13,T=A,C. mc$VERSION.car
+QUIT.
EOF

# Run ypatchy:
ytofort mc$VERSION

rm -f mc$VERSION.cra
rm -f mc$VERSION.lis

# Compile the code and make library:
ftolib mc$VERSION.f

rm -f mc$VERSION.f
rm -rf mc$VERSION
rm -f ftolib.log

mkdir -p ${JADE_MCLIB}
mv -f mc$VERSION.a ${JADE_MCLIB}/libmc$VERSION.a

echo "library ${JADE_MCLIB}/libmc$VERSION.a created"

# The End:
exit










