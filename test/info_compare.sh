#!/bin/sh
# Compares the run table of fpinfo (reads the FPACK file directly) and of bosinfo (reads the BOS file
# that fptobos made from it): the runs, the numbers of events and the CM energy must be equal.
# usage: info_compare.sh <fpinfo> <FPACK file> <bosinfo> <BOS file> <work dir>
FPINFO=$1; FPFILE=$2; BOSINFO=$3; BOSFILE=$4; WORK=$5
cd "$WORK" || exit 1
# byte order settings of the Fortran runtimes (gfortran, Intel, NVIDIA) for each program
GFORTRAN_CONVERT_UNIT=native F_UFMTENDIAN= FORT_CONVERT_UNIT=native "$FPINFO" "$FPFILE" > fpinfo.out 2>&1 || { echo "FAIL: fpinfo"; exit 1; }
GFORTRAN_CONVERT_UNIT='big_endian;native:2' F_UFMTENDIAN=big:2 FORT_CONVERT_UNIT='big_endian;native:2' "$BOSINFO" "$BOSFILE" > bosinfo.out 2>&1 || { echo "FAIL: bosinfo"; exit 1; }
# the lines with the numbers: from the table header to the mean CM energy, and the number of events
for f in fpinfo bosinfo; do
  grep -E "events with bank HEAD|number of runs|^ +[0-9]+ +[0-9]+ +[0-9]+\.[0-9]+ +[0-9]+\.[0-9]+ *$|mean CM energy" $f.out > $f.cmp
done
cat fpinfo.cmp
if diff fpinfo.cmp bosinfo.cmp; then echo "PASS: the same runs, numbers of events and energy"; else echo "FAIL: fpinfo and bosinfo differ"; exit 1; fi
