#!/bin/sh -l
set -x
brew install cmake gcc coreutils wget gengetopt root pythia openmotif libx11
# cernlib and hepmc3 from the JADE tap (.github/homebrew/), bottles from ghcr.io
bash .github/homebrew/install.sh cernlib hepmc3 || exit 1

sh jadeinstall.sh
sh jadetest.sh
