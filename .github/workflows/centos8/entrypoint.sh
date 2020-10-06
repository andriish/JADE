#!/bin/sh -l
set -x
uname -a 
cat /etc/issue
yum -y install https://dl.fedoraproject.org/pub/epel/epel-release-latest-8.noarch.rpm
yum -y install dnf-plugins-core
dnf config-manager --set-enabled PowerTools
yum -y install  gcc gcc-c++ gcc-gfortran make
yum -y install  cmake cmake-data cmake-filesystem
yum -y install  HepMC3*
yum -y install  lapack-static  lapack-devel lapack  gengetopt  blas-devel blas atlas-devel atlas  openblas-devel openblas openblas-serial64 openblas-threads  --skip-broken
yum -y install  libX11-devel libX11  libXmu-devel libXmu libXau-devel libXau libXcursor-devel  libXcursor  libSM-devel libSM libICE libICE-devel libXext-devel libXext

yum -y install  root-*6* --exclude=*doc* --exclude=*debug* --skip-broken

sh run.sh


out=$?
echo ::set-output name=out::$out
