#!/bin/bash
# Runs fpinfo on all FPACK files of the JADE tapes and writes a table with one line per file and run:
#   tape  file  run  events  Ebeam/GeV  Ecm/GeV
# The tape files are only read. Files for which fpinfo fails are listed in <output>.failed.
#
# usage: scan_tapes.sh [-t <jadetapes directory>] [-f <fpinfo executable>] [-o <output file>] [-j <jobs>]
#   -t  directory with the tapes JADnnn/fileNNN   (default: the volume under /run/media/$USER, or ./jadetapes)
#   -f  fpinfo executable (default: $FPINFO, then buildGNU64/test/fpinfo, then fpinfo on the PATH)
#   -o  output table (default: jadetapes_runs.txt)
#   -j  number of files processed in parallel (default: 4)
# Example (the volume mounted by the desktop):
#   sh test/scan_tapes.sh -t /run/media/$USER/036d2b78-dc5a-436c-a201-1df255b45844/jade/jadetapes -j 8
# The totals per run and per energy can be made from the table, e.g.
#   awk '$1!~/^#/{n[$3]+=$4; e[$3]=$6} END{for(r in n) print r, n[r], e[r]}' jadetapes_runs.txt | sort -n

TAPES=""; FPINFO_EXE="${FPINFO:-}"; OUT=jadetapes_runs.txt; JOBS=4
while getopts "t:f:o:j:" o; do
  case $o in t) TAPES=$OPTARG;; f) FPINFO_EXE=$OPTARG;; o) OUT=$OPTARG;; j) JOBS=$OPTARG;; *) exit 2;; esac
done
if [ -z "$TAPES" ]; then
  for d in /run/media/"$USER"/*/jade/jadetapes ./jadetapes; do [ -d "$d" ] && { TAPES=$d; break; }; done
fi
[ -d "$TAPES" ] || { echo "no tape directory (use -t)"; exit 2; }
if [ -z "$FPINFO_EXE" ]; then
  SD=$(cd "$(dirname "$0")/.." && pwd)
  for f in "$SD"/build*/test/fpinfo; do [ -x "$f" ] && { FPINFO_EXE=$f; break; }; done
  [ -z "$FPINFO_EXE" ] && FPINFO_EXE=$(command -v fpinfo)
fi
[ -x "$FPINFO_EXE" ] || { echo "fpinfo not found (build the test directory, or use -f)"; exit 2; }
FPINFO_EXE=$(readlink -f "$FPINFO_EXE"); TAPES=$(readlink -f "$TAPES")
WORK=$(mktemp -d); trap 'rm -rf "$WORK"' EXIT
echo "tapes: $TAPES   fpinfo: $FPINFO_EXE   jobs: $JOBS"

# one file: the table lines of fpinfo with the tape and the file name in front
one() {
  f=$1; tape=$(basename "$(dirname "$f")"); name=$(basename "$f")
  res="$WORK/$tape.$name"
  # the Fortran runtime reads the FPACK files in the native byte order (as for fptobos)
  GFORTRAN_CONVERT_UNIT=native F_UFMTENDIAN= FORT_CONVERT_UNIT=native "$FPINFO_EXE" "$f" < /dev/null > "$res.log" 2>&1
  rc=$?
  rows=$(grep -E '^ +[0-9]+ +[0-9]+ +[0-9]+\.[0-9]+ +[0-9]+\.[0-9]+ *$' "$res.log" | awk -v t="$tape" -v n="$name" '{print t, n, $1, $2, $3, $4}')
  if [ $rc -ne 0 ] || [ -z "$rows" ]; then echo "$f (exit code $rc)" > "$res.failed"; fi
  [ -n "$rows" ] && echo "$rows" > "$res.rows"
  echo "$tape/$name: $(echo "$rows" | grep -c .) run(s), exit code $rc"
}
export -f one; export WORK FPINFO_EXE

find -L "$TAPES" -type f -name 'file*' | sort | xargs -P "$JOBS" -I{} bash -c 'one "$@"' _ {}

{ echo "# tape file run events Ebeam_GeV Ecm_GeV   (made by scan_tapes.sh with $(basename "$FPINFO_EXE"))"
  cat "$WORK"/*.rows 2>/dev/null | sort -k1,1 -k2,2 -k3,3n; } > "$OUT"
cat "$WORK"/*.failed 2>/dev/null > "$OUT.failed"
echo "table: $OUT ($(grep -vc '^#' "$OUT") lines), failed files: $(grep -c . "$OUT.failed") (see $OUT.failed)"
