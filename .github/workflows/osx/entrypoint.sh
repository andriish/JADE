#!/bin/sh -l
set -x
brew install wget coreutils gcc
brew tap davidchall/hep
brew install hepmc3
brew cask install xquartz
wget https://root.cern/download/root_v6.22.02.macosx64-10.15-clang110.tar.gz
tar -zxvf root_v6.22.02.macosx64-10.15-clang110.tar.gz
. root/bin/thisroot.sh

sh macrun.sh

