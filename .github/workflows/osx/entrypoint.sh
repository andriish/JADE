#!/bin/sh -l
set -x
brew tap davidchall/hep
brew install hepmc3 root cernlib pythia8 wget coreutils gcc

sh jadeinstall.sh
sh jadetest.sh

