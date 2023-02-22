#!/bin/sh -l
set -x
brew tap davidchall/hep
brew install hepmc3 root cernlib pythia8 wget coreutils gcc

sh jadeinstall.sh  --toolchain=GNU-12
sh jadetest.sh  --toolchain=GNU-12

