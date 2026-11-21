#!/bin/bash
# Event display test matching the reference R-phi picture (doc/eventdisplayreferences/): detector hardware
# (chamber boundaries), result tables, orthogonal projections and the small dE/dx plot,
# no bank listing, on the Monte Carlo event reconstructed by SUPERV_MC_1 (superv1.bos).
# usage: jadez_mc_test.sh <jadez executable> <help dataset> <work dir>
# Needs a virtual X display (xvfb-run or JADEZ_TEST_DISPLAY), otherwise skipped (77).
SCRIPTDIR=$(cd "$(dirname "$0")" && pwd)
JADEZ=$1
HELP=$2
WORK=$3
cd "$WORK" || exit 1
# HIGZ reads (and creates) $HOME/higz_windows.dat; use a private HOME so the result does not
# depend on the user's home directory (and nothing is left in it)
export HOME="$(pwd)/home_higz"; mkdir -p "$HOME"
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
mkdir -p rphidisplay && cd rphidisplay && rm -f JADE.* fort.77
cp ../superv1.bos .
cp ../aupdat1.b F11LHO.AUPDAT1
cp "$HELP" F22BOW.HELP.DATASET
# the superv output is big-endian on all units
export GFORTRAN_CONVERT_UNIT=big_endian
export F_UFMTENDIAN=big   # the same for Intel Fortran

# OPT 13 OFF: no bank listing, OPT 14 OFF: results header lines also after a command with an
# argument (blue PATR and orange LGCL header lines), OPT 16: automatic result tables, OPT 17: orthogonal
# projections (insets). RB1: R-phi view with the detector hardware (chamber boundary
# lines). DEDX -4: dE/dx versus momentum plot drawn small on the current view.
( printf 'superv1.bos\n\n'; sleep 2
  for c in "OPT 13" "OPT 14" "OPT 16" "OPT 17" "RB1" "DEDX -4"; do echo "$c"; sleep 1.5; done
  echo H; sleep 2
  echo END; sleep 2 ) | timeout 600 $RUN "$JADEZ" > ../jadez_rphi_test.log 2>&1
rc=$?
if [ $rc -ne 0 ]; then
  echo "--- diagnostics (jadez failed) ---"
  uname -a; ulimit -a | grep -E "open files|core|virtual|stack"
  echo "DISPLAY=$DISPLAY HOME=$HOME"; ls -la /tmp/.X11-unix 2>&1
  echo "--- first lines of the jadez output ---"; head -40 ../jadez_rphi_test.log
  echo "--- X connection check (python ctypes, same xvfb-run) ---"
  $RUN python3 -c "import ctypes,os;x=ctypes.CDLL('libX11.so.6');x.XOpenDisplay.restype=ctypes.c_void_p;d=x.XOpenDisplay(None);print('DISPLAY',os.environ.get('DISPLAY'),'XOpenDisplay ->',d)" 2>&1 | tail -5
  echo "--- X environment seen by a client under xvfb-run ---"
  $RUN sh -c 'env | grep -E "^(DISPLAY|XAUTHORITY|HOME)="; xauth list 2>&1 | head -3; hostname; cat /etc/hosts; ls -la /tmp/.X11-unix' 2>&1 | head -20
  if command -v strace >/dev/null 2>&1; then
    echo "--- strace of the display connection of jadez ---"
    $RUN strace -f -e trace=%file,%network -o "$(pwd)/jadez_strace.log" "$JADEZ" < /dev/null > /dev/null 2>&1
    grep -v -E "ld.so|/lib/|/usr/lib|/proc/|/sys/|/dev/" "$(pwd)/jadez_strace.log" | tail -40
    echo "--- HIGZ window file (HIGZ reads \$HOME/higz_windows.dat, display host name) ---"
    ls -la "$HOME" 2>&1 | head; cat -A "$HOME/higz_windows.dat" 2>&1 | head -12
  fi
  if command -v gdb >/dev/null 2>&1; then
    echo "--- gdb backtrace of jadez at start-up ---"
    $RUN gdb -batch -ex run -ex bt -ex "info sharedlibrary" --args "$JADEZ" < /dev/null 2>&1 | grep -v "^\[" | tail -50
  fi
  echo "--- Xvfb errors ---"; cat "$XVFB_ERR" 2>&1 | tail -30
  echo "--- end diagnostics ---"
fi
echo "jadez exit code $rc (R-phi reference picture)"
fail=0
[ $rc -eq 0 ] || { echo "FAIL: non-zero exit code"; fail=1; }
grep -q "Fortran runtime error" ../jadez_rphi_test.log && { echo "FAIL: Fortran runtime error"; fail=1; }
grep -q "Invalid command string" ../jadez_rphi_test.log && { echo "FAIL: display command rejected"; fail=1; }
grep -q "NAME: " ../jadez_rphi_test.log && { echo "FAIL: bank listing printed (OPT 13 not off?)"; fail=1; }
NV=1
# Intel Fortran: HIGZ (in the gfortran-built CERNLIB) writes the PostScript through libgfortran's
# unit 77, which is not the unit that jadez opened as JADE.nnn.ps, so the pictures end up in
# fort.77 and the JADE.nnn.ps stay empty. Split fort.77 into one file per picture.
if [ -s fort.77 ] && [ -z "$(find . -maxdepth 1 -name 'JADE.*.ps' -size +0)" ]; then
  csplit -s -z -f JADE.fort77. -b '%03d.ps' fort.77 '/^%!PS-Adobe/' '{*}' && rm -f fort.77
fi
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
echo "pictures are in $WORK/rphidisplay: JADE.nnn.ps(.png)"
[ $fail -eq 0 ] && echo "PASS" || tail -30 ../jadez_rphi_test.log
exit $fail
