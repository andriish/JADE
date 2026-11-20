#!/bin/bash
# Event display test with the detector hardware (chamber boundary lines), the
# result tables and the orthogonal projections switched on, on the Monte Carlo
# event reconstructed by SUPERV_MC_1 (superv1.bos).
# usage: jadez_mc_test.sh <jadez executable> <help dataset> <work dir>
# Needs a virtual X display (xvfb-run or JADEZ_TEST_DISPLAY), otherwise skipped (77).
SCRIPTDIR=$(cd "$(dirname "$0")" && pwd)
JADEZ=$1
HELP=$2
WORK=$3
cd "$WORK" || exit 1
for f in superv1.bos aupdat1.b; do
  [ -f "$f" ] || { echo "missing input $f (SUPERV_MC_1 must run first)"; exit 1; }
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
mkdir -p mcdisplay && cd mcdisplay && rm -f JADE.*
cp ../superv1.bos .
cp ../aupdat1.b F11LHO.AUPDAT1
cp "$HELP" F22BOW.HELP.DATASET
export JADE_PHIGS_SCREENSHOT=1
# the superv output is big-endian on all units
export GFORTRAN_CONVERT_UNIT=big_endian

# OPT 16: automatic result tables, OPT 17: orthogonal projections (insets).
# A non-zero argument draws the detector hardware (chamber boundary lines).
VIEWS="RB1 RC1 ZXB1 ZYB1 ZXC1 ZYC1 FW1 RU1 CYL1 DEDX-2"
NV=$(echo $VIEWS | wc -w)
( printf 'superv1.bos\n\n'; sleep 2; echo "OPT 16"; sleep 1; echo "OPT 17"; sleep 1
  for c in $VIEWS; do echo "$c"; sleep 1; echo H; sleep 1.5; done
  echo END; sleep 2 ) | timeout 600 $RUN "$JADEZ" > ../jadez_mc_test.log 2>&1
rc=$?
if [ $rc -ne 0 ]; then
  echo "--- diagnostics (jadez failed) ---"
  uname -a; ulimit -a | grep -E "open files|core|virtual|stack"
  echo "DISPLAY=$DISPLAY HOME=$HOME"; ls -la /tmp/.X11-unix 2>&1
  echo "--- first lines of the jadez output ---"; head -40 ../jadez_mc_test.log
  echo "--- X connection check (python ctypes, same xvfb-run) ---"
  $RUN python3 -c "import ctypes,os;x=ctypes.CDLL('libX11.so.6');x.XOpenDisplay.restype=ctypes.c_void_p;d=x.XOpenDisplay(None);print('DISPLAY',os.environ.get('DISPLAY'),'XOpenDisplay ->',d)" 2>&1 | tail -5
  if command -v gdb >/dev/null 2>&1; then
    echo "--- gdb backtrace of jadez at start-up ---"
    $RUN gdb -batch -ex run -ex bt -ex "info sharedlibrary" --args "$JADEZ" < /dev/null 2>&1 | grep -v "^\[" | tail -50
  fi
  echo "--- Xvfb errors ---"; cat "$XVFB_ERR" 2>&1 | tail -30
  echo "--- end diagnostics ---"
fi
echo "jadez exit code $rc, $NV hardware views with OPT 16 and OPT 17"
fail=0
[ $rc -eq 0 ] || { echo "FAIL: non-zero exit code"; fail=1; }
grep -q "Fortran runtime error" ../jadez_mc_test.log && { echo "FAIL: Fortran runtime error"; fail=1; }
grep -q "Invalid command string" ../jadez_mc_test.log && { echo "FAIL: display command rejected"; fail=1; }
NF=$(find . -maxdepth 1 \( -name 'JADE.*.png' -o -name 'JADE.*.ps' \) -size +0 | wc -l)
echo "$NF hardcopy files for $NV H commands"
[ "$NF" -ge "$NV" ] || { echo "FAIL: too few hardcopies"; fail=1; }
# coloured, orange lead glass numbers, thin grey chamber boundary lines
python3 "$SCRIPTDIR/check_colours.py" --grey JADE.*.png JADE.*.ps 2>/dev/null \
  || { echo "FAIL: colours / lead glass numbers / grey detector boundary lines missing"; fail=1; }
if command -v gs >/dev/null 2>&1; then
  for f in JADE.*.ps; do
    [ -s "$f" ] && gs -q -dNOPAUSE -dBATCH -dSAFER -sDEVICE=png16m -r100 -dEPSCrop -sOutputFile="$f.png" "$f" >/dev/null 2>&1
  done
fi
echo "pictures are in $WORK/mcdisplay: JADE.nnn.ps(.png) (HIGZ) or JADE.nnn.png / JADE.nnn.screen.png (OpenPHIGS)"
[ $fail -eq 0 ] && echo "PASS" || tail -30 ../jadez_mc_test.log
exit $fail
