#!/bin/sh -l
set -x
brew install wget coreutils gcc
brew tap davidchall/hep
brew install hepmc3
brew cask install xquartz
wget --no-verbose https://root.cern/download/root_v6.22.02.macosx64-10.15-clang110.tar.gz
tar -zxf root_v6.22.02.macosx64-10.15-clang110.tar.gz
. root/bin/thisroot.sh

sh jadeinstall.sh

brew install pythia8

sh jadetest.sh

