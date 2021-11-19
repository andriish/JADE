#!/bin/sh -l
set -x
uname -a 
cat /etc/issue
dnf -y install epel-release
dnf -y install dnf-plugins-core
dnf config-manager --set-enabled powertools
dnf -y install  gcc gcc-c++ gcc-gfortran make which cmake cmake-data cmake-filesystem HepMC3*
dnf -y install  lapack-static  lapack-devel lapack  gengetopt  blas-devel blas atlas-devel atlas  openblas-devel openblas openblas-serial64 openblas-threads  --skip-broken
dnf -y install  libX11-devel libX11  libXmu-devel libXmu libXau-devel libXau libXcursor-devel  libXcursor  libSM-devel libSM libICE libICE-devel libXext-devel libXext
dnf -y install  root-*6* --exclude=*doc* --exclude=*debug* --skip-broken

sh jadeinstall.sh

dnf -y install pythia8-devel pythia8 pythia8-data 

sh jadetest.sh

out=$?
echo ::set-output name=out::$out
