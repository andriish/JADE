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

# output of the analysis program test/src/ROOT_analysis_1.cc (histogram of the thrust, ROOT tests of the test suite):
# data sample (ROOT_ANALYSIS_DATA_1), Pythia8 at 34.6 GeV (ROOT_ANALYSIS1_MC_1), PYTHIA 5.7 at 35 GeV (ROOT_ANALYSIS1_MC_2)
# and Pythia8 at 43.8 GeV (ROOT_ANALYSIS1_MC_3). The program writes <name>.pdf, i.e. the files are called *.pdf.pdf here.
# Run first:  cd <build directory>/test && ctest -R "ROOT_ANALYSIS"
thrust() {  # thrust input.pdf name
  pdfcrop --margins 5 "$1" $F/$2.pdf >/dev/null
  pdftops -eps $F/$2.pdf $F/$2.eps 2>/dev/null
}
thrust $T/jtjobfile006_1.pdf.pdf thrust_data
thrust $T/jtjob11.pdf.pdf        thrust_mc1
thrust $T/jtjob12.pdf.pdf        thrust_mc2
thrust $T/jtjob13.pdf.pdf        thrust_mc3
