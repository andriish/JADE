#!/bin/sh -l
set -x
uname -a 
cat /etc/issue
yum -y install https://dl.fedoraproject.org/pub/epel/epel-release-latest-7.noarch.rpm
yum -y install  gcc gcc-c++ gcc-gfortran make
yum -y install  cmake*
yum -y install  HepMC3*
yum -y install  *root-* --exclude=*doc* --exclude=*debug* --skip-broken

sh run.sh

out=$?
echo ::set-output name=out::$out
