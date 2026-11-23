#!/bin/bash
# Event display test with a short sequence (5-6) of display commands, in one jadez session.
# usage: jadez_seq_test.sh <jadez executable> <help dataset> <work dir> <name> <data|mc> <draw|text> <command>...
#   data : input file006.bos (FPTOBOS_DATA_1),  mc : input superv1.bos (SUPERV_MC_1)
#   draw : every command is followed by H (hardcopy); the number of pictures and their colours are checked
#   text : the commands only print text (no hardcopy), only the exit code and the log are checked
# The commands are run in the given order in the same session, so the display state (OPT flags, ...)
# is carried from one to the next. Commands that ask for more input are not suitable.
# Needs a virtual X display (xvfb-run or JADEZ_TEST_DISPLAY), otherwise skipped (77).
SCRIPTDIR=$(cd "$(dirname "$0")" && pwd)
JADEZ=$1; HELP=$2; WORK=$3; NAME=$4; INPUT=$5; MODE=$6
shift 6
cd "$WORK" || exit 1
export HOME="$(pwd)/home_higz"; mkdir -p "$HOME"
if [ "$INPUT" = "data" ]; then
  BOS=file006.bos
  export GFORTRAN_CONVERT_UNIT="big_endian;native:2"
  export F_UFMTENDIAN="big:1,3-100"
  export FORT_CONVERT_UNIT="big_endian;native:2"
else
  BOS=superv1.bos
  export GFORTRAN_CONVERT_UNIT=big_endian
  export F_UFMTENDIAN=big
  export FORT_CONVERT_UNIT=big_endian
fi
for f in $BOS aupdat1.b; do
  [ -f "$f" ] || { echo "missing input $f (the test producing it must run first)"; exit 1; }
done
RUN=""
if command -v xvfb-run >/dev/null 2>&1; then
  XVFB_ERR="$(pwd)/xvfb_err.log"; RUN="xvfb-run -a -e $XVFB_ERR"
elif [ -n "$JADEZ_TEST_DISPLAY" ]; then
  export DISPLAY=$JADEZ_TEST_DISPLAY
else
  echo "SKIP: no xvfb-run and JADEZ_TEST_DISPLAY not set"
  exit 77
fi
DIR=seq_$NAME
mkdir -p "$DIR" && cd "$DIR" && rm -f JADE.* fort.77
cp ../$BOS .
cp ../aupdat1.b F11LHO.AUPDAT1
cp "$HELP" F22BOW.HELP.DATASET
LOG=../jadez_$NAME.log
NCMD=$#
# macOS has no `timeout` unless coreutils' gnubin is on PATH; Homebrew coreutils gives `gtimeout`
TMO=timeout; command -v timeout >/dev/null 2>&1 || TMO=gtimeout
( printf '%s\n\n' "$BOS"; sleep 2
  for c in "$@"; do
    echo "$c"; sleep 1.5
    [ "$MODE" = draw ] && { echo H; sleep 1.5; }
  done
  echo END; sleep 2 ) | $TMO 300 $RUN "$JADEZ" > "$LOG" 2>&1
rc=$?
echo "jadez exit code $rc, $NCMD commands ($MODE): $*"
fail=0
[ $rc -eq 0 ] || { echo "FAIL: non-zero exit code"; fail=1; }
grep -q "Fortran runtime error" "$LOG" && { echo "FAIL: Fortran runtime error"; fail=1; }
grep -q "Floating-point exception\|SIGFPE" "$LOG" && { echo "FAIL: floating point exception"; fail=1; }
grep -q "Invalid command string" "$LOG" && { echo "FAIL: display command rejected"; fail=1; }
if [ "$MODE" = draw ]; then
  # Intel/NVIDIA Fortran: HIGZ writes the PostScript through libgfortran's unit 77 (fort.77), split it
  if [ -s fort.77 ] && [ -z "$(find . -maxdepth 1 -name 'JADE.*.ps' -size +0)" ]; then
    csplit -s -z -f JADE.fort77. -b '%03d.ps' fort.77 '/^%!PS-Adobe/' '{*}' && rm -f fort.77
  fi
  NF=$(find . -maxdepth 1 \( -name 'JADE.*.png' -o -name 'JADE.*.ps' \) -size +0 | wc -l)
  echo "$NF hardcopy files for $NCMD H commands"
  [ "$NF" -ge "$NCMD" ] || { echo "FAIL: too few hardcopies"; fail=1; }
  python3 "$SCRIPTDIR/check_colours.py" JADE.*.png JADE.*.ps 2>/dev/null \
    || { echo "FAIL: the pictures are not coloured / lead glass numbers missing"; fail=1; }
  if command -v gs >/dev/null 2>&1; then
    for f in JADE.*.ps; do
      [ -s "$f" ] && gs -q -dNOPAUSE -dBATCH -dSAFER -sDEVICE=png16m -r100 -dEPSCrop -sOutputFile="$f.png" "$f" >/dev/null 2>&1
    done
  fi
  echo "pictures are in $WORK/$DIR: JADE.nnn.ps(.png)"
fi
[ $fail -eq 0 ] && echo "PASS" || { tail -30 "$LOG"; [ -s "$XVFB_ERR" ] && tail -10 "$XVFB_ERR"; }
exit $fail
