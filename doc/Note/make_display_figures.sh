#!/bin/sh
# Prepare the figures of the event display section of the note:
#  - the reference pictures of the historic display (doc/eventdisplayreferences)
#  - pictures written by the display tests of the test suite (JADEZ_RPHI_1, JADEZ_MC_1, JADEZ_DATA_1)
# Each figure is made in the formats of the two ways to build the note: EPS (latex, dvips) and PDF
# (pdflatex); the JPEG references are also kept.
# usage: sh make_display_figures.sh [directory of the tests, default ../../buildGNU64/test]
# The tests are run with:  cd <build directory>/test && ctest -R JADEZ     (needs a virtual X display)
T=${1:-../../buildGNU64/test}
F=Figures
mkdir -p $F

# references
for n in 1 2; do
  cp ../eventdisplayreferences/$n.jpg $F/display_ref$n.jpg
  convert $F/display_ref$n.jpg $F/display_ref$n.eps
  convert $F/display_ref$n.jpg $F/display_ref$n.pdf
done

# pictures of the tests: HIGZ PostScript -> cropped EPS and PDF
conv() {  # conv input.ps name
  ps2eps -f -l -q < "$1" > $F/$2.eps
  gs -q -dNOPAUSE -dBATCH -dSAFER -sDEVICE=pdfwrite -dEPSCrop -sOutputFile=$F/$2.pdf $F/$2.eps
}
conv $T/rphidisplay/JADE.001.ps display_rphi      # JADEZ_RPHI_1: RB1 and DEDX -4, like display_ref2
conv $T/mcdisplay/JADE.002.ps   display_mc_rc1    # JADEZ_MC_1:   RC1, hardware with the muon chambers
