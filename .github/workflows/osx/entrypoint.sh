#!/bin/sh -l
set -x
brew install wget coreutils
brew tap davidchall/hep
brew install hepmc3
wget https://root.cern/download/root_v6.22.02.macosx64-10.15-clang110.tar.gz
tar -zxvf root_v6.22.02.macosx64-10.15-clang110.tar.gz
. root/bin/thisroot.sh

sh macrun.sh

