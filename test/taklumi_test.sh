#!/bin/bash
# Test of taklumi: prints the table of the JADE luminosity (the integrated barrel luminosity per
# run range of equal beam energy and per range of the centre-of-mass energy).
# usage: taklumi_test.sh <taklumi executable> <lumi.dat> <work dir>
TAKLUMI=$1
LUMIDAT=$2
WORK=$3
cd "$WORK" || exit 1
cp "$LUMIDAT" lumi.dat || { echo "FAIL: cannot copy $LUMIDAT"; exit 1; }
# first and last run number: all JADE runs (the program accepts at most 30397 runs)
echo "1 30397" | "$TAKLUMI" > taklumi.log 2>&1
rc=$?
fail=0
[ $rc -eq 0 ] || { echo "FAIL: exit code $rc"; fail=1; }
grep -q "Fortran runtime error" taklumi.log && { echo "FAIL: Fortran runtime error"; fail=1; }
grep -q "INITIALIZATION FINISHED" taklumi.log || { echo "FAIL: luminosity file not read"; fail=1; }
# the table per range of the centre-of-mass energy is the last part of the output
sed -n '/CMS RANGE/,$p' taklumi.log > lumitable.txt
echo "=== Luminosity table (taklumi, runs 1 - 30397; the complete output is taklumi.log) ==="
cat lumitable.txt
echo "==="
NROWS=$(grep -c "^ *[0-9][0-9]*\.[0-9]* - *[0-9][0-9]*\.[0-9]* " lumitable.txt)
[ "$NROWS" -eq 6 ] || { echo "FAIL: $NROWS rows in the table of the CMS ranges, 6 expected"; fail=1; }
# every range has a non-zero luminosity
[ "$(grep -c "0.000000E+00 +-" lumitable.txt)" -eq 0 ] || { echo "FAIL: a CMS range without luminosity"; fail=1; }
[ $fail -eq 0 ] && echo "PASS" || tail -30 taklumi.log
exit $fail
