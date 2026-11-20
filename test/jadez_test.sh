#!/bin/bash
# Smoke test of the JADE event display (jadez) on the sample data file.
# usage: jadez_test.sh <jadez executable> <help dataset> <work dir>
#
# The display needs an X server. To avoid opening windows on a desktop the test
# only runs on a virtual display: xvfb-run if available, or the display given in
# JADEZ_TEST_DISPLAY (e.g. a headless weston/Xwayland). Otherwise it is skipped
# (exit code 77, see SKIP_RETURN_CODE in CMakeLists.txt).
SCRIPTDIR=$(cd "$(dirname "$0")" && pwd)
JADEZ=$1
HELP=$2
WORK=$3
cd "$WORK" || exit 1
for f in file006.bos aupdat1.b; do
  [ -f "$f" ] || { echo "missing input $f (FPTOBOS_DATA_1 must run first)"; exit 1; }
done
cp aupdat1.b F11LHO.AUPDAT1
cp "$HELP" F22BOW.HELP.DATASET

RUN=""
if command -v xvfb-run >/dev/null 2>&1; then
  XVFB_ERR="$(pwd)/xvfb_err.log"; RUN="xvfb-run -a -e $XVFB_ERR"
elif [ -n "$JADEZ_TEST_DISPLAY" ]; then
  export DISPLAY=$JADEZ_TEST_DISPLAY
else
  echo "SKIP: no xvfb-run and JADEZ_TEST_DISPLAY not set"
  exit 77
fi

# Display commands that run without further prompts. Commands that ask for
# more input or arguments (COM, OPT, BL, DAX, ZV, AX, PRINT, ZOOM, ...) are not used.
#  DRAW : commands drawing a picture (detector views, hit/track displays, muon filter
#         views RC1/ZXC1/.. and MUPT/MUONS; the muon chambers themselves are only drawn
#         if the event has muon hits and muon calibration, which this sample lacks);
#         each is followed by H (hardcopy), which must give a non-empty picture.
#  TEXT : commands that only print or draw nothing for this event.
DRAW="RA RB RC ZXA ZXB ZXC ZXD ZYA ZYB ZYC ZYD FW RU CYL FWMU ZC VRX VRZX VRZY VC RB1 DET TRLG FADC RC1 ZXC1 ZYC1 ZXD1 ZYD1 FWMU1 MUPT MUONS"
TEXT="TRUE STAT VX EC FC CLUS DEDX TOF MASS MUTYP MUR2"
NDRAW=$(echo $DRAW | wc -w)
NTEXT=$(echo $TEXT | wc -w)
rm -f JADE.*.png JADE.*.ps
# OpenPHIGS backend: also save what is shown on the screen (JADE.nnn.screen.png)
export JADE_PHIGS_SCREENSHOT=1

# dataset name, <ENTER> for the calibration file, then the commands, then END
( printf 'file006.bos\n\n'; sleep 2
  for c in $DRAW; do echo "$c"; sleep 0.7; echo H; sleep 1; done
  for c in $TEXT; do echo "$c"; sleep 0.7; done
  echo END; sleep 2 ) | timeout 600 $RUN "$JADEZ" > jadez_test.log 2>&1
rc=$?
if [ $rc -ne 0 ]; then
  echo "--- diagnostics (jadez failed) ---"
  uname -a; ulimit -a | grep -E "open files|core|virtual|stack"
  echo "DISPLAY=$DISPLAY HOME=$HOME"; ls -la /tmp/.X11-unix 2>&1
  echo "--- first lines of the jadez output ---"; head -40 jadez_test.log
  echo "--- X connection check (python ctypes, same xvfb-run) ---"
  $RUN python3 -c "import ctypes,os;x=ctypes.CDLL('libX11.so.6');x.XOpenDisplay.restype=ctypes.c_void_p;d=x.XOpenDisplay(None);print('DISPLAY',os.environ.get('DISPLAY'),'XOpenDisplay ->',d)" 2>&1 | tail -5
  echo "--- X environment seen by a client under xvfb-run ---"
  $RUN sh -c 'env | grep -E "^(DISPLAY|XAUTHORITY|HOME)="; xauth list 2>&1 | head -3; hostname; cat /etc/hosts; ls -la /tmp/.X11-unix' 2>&1 | head -20
  if command -v strace >/dev/null 2>&1; then
    echo "--- strace of the display connection of jadez ---"
    $RUN strace -f -e trace=connect,socket,access -o "$(pwd)/jadez_strace.log" "$JADEZ" < /dev/null > /dev/null 2>&1
    grep -E "X11|connect|ENOENT|EACCES|EPERM|ECONN" "$(pwd)/jadez_strace.log" | tail -20
  fi
  if command -v gdb >/dev/null 2>&1; then
    echo "--- gdb backtrace of jadez at start-up ---"
    $RUN gdb -batch -ex run -ex bt -ex "info sharedlibrary" --args "$JADEZ" < /dev/null 2>&1 | grep -v "^\[" | tail -50
  fi
  echo "--- Xvfb errors ---"; cat "$XVFB_ERR" 2>&1 | tail -30
  echo "--- end diagnostics ---"
fi
echo "jadez exit code $rc, $NDRAW drawing + $NTEXT text commands"
fail=0
[ $rc -eq 0 ] || { echo "FAIL: non-zero exit code"; fail=1; }
grep -q "Fortran runtime error" jadez_test.log && { echo "FAIL: Fortran runtime error"; fail=1; }
grep -q "NR OF EVENTS READ:     1 " jadez_test.log || { echo "FAIL: event not read"; fail=1; }
grep -q "Invalid command string" jadez_test.log && { echo "FAIL: display command rejected"; fail=1; }

# hardcopies: HIGZ writes JADE.nnn.ps, OpenPHIGS JADE.nnn.png (or .ps)
NFILES=$(find . -maxdepth 1 \( -name 'JADE.*.png' -o -name 'JADE.*.ps' \) -size +0 | wc -l)
echo "$NFILES hardcopy files for $NDRAW H commands"
[ "$NFILES" -ge "$NDRAW" ] || { echo "FAIL: too few hardcopies"; fail=1; }

# the pictures must be coloured
python3 "$SCRIPTDIR/check_colours.py" JADE.*.png JADE.*.ps 2>/dev/null \
  || { echo "FAIL: hardcopies are not coloured"; fail=1; }
# make the HIGZ PostScript pictures viewable (JADE.nnn.ps -> JADE.nnn.ps.png)
if command -v gs >/dev/null 2>&1; then
  for f in JADE.*.ps; do
    [ -s "$f" ] && gs -q -dNOPAUSE -dBATCH -dSAFER -sDEVICE=png16m -r100 -dEPSCrop -sOutputFile="$f.png" "$f" >/dev/null 2>&1
  done
fi
echo "pictures are in $WORK: JADE.nnn.ps(.png) (HIGZ) or JADE.nnn.png / JADE.nnn.screen.png (OpenPHIGS)"
[ $fail -eq 0 ] && echo "PASS" || tail -30 jadez_test.log
exit $fail
