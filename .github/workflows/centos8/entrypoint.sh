#!/bin/sh -l
set -x
uname -a 
cat /etc/issue
cd /etc/yum.repos.d/
sed -i 's/mirrorlist/#mirrorlist/g' /etc/yum.repos.d/CentOS-*
sed -i 's|#baseurl=http://mirror.centos.org|baseurl=http://vault.centos.org|g' /etc/yum.repos.d/CentOS-*
yum -y install  epel-release dnf-*
dnf config-manager --set-enabled powertools
yum -y install  gcc gcc-c++ gcc-gfortran make which cmake cmake-data cmake-filesystem HepMC3*
yum -y install  lapack-static  lapack-devel lapack  gengetopt  blas-devel blas atlas-devel atlas  openblas-devel openblas openblas-serial64 openblas-threads  --skip-broken
yum -y install  libX11-devel libX11  libXmu-devel libXmu libXau-devel libXau libXcursor-devel  libXcursor  libSM-devel libSM libICE libICE-devel libXext-devel libXext
yum -y install  root-*6* --exclude=*doc* --exclude=*debug* --skip-broken
yum -y install pythia8-devel pythia8 pythia8-data 
yum -y  install  dnf-plugins-core --exclude=*uploa* --exclude=*product* --exclude=*subscr*
dnf -y copr enable averbyts/HEPrpms 
yum -y  install cernlib*

sh jadeinstall.sh


sh jadetest.sh

out=$?
echo ::set-output name=out::$out
