FROM fedora:39
RUN  yum -y  install  dnf-plugins-core \
                      bc make ninja-build cmake binutils git wget diffutils file sed gawk grep which  \
                      gcc-gfortran gcc-c++ openssl-devel openssl \
                      rpmbuild chrpath \
                      openssl-devel.i686 openssl.i686 glibc-devel.i686 libquadmath.i686 libgfortran.i686 libnsl2-devel.i686\
                      openssl-devel.i686 openssl.i686 blas.i686 blas-devel.i686\
                      libXau.i686 libX11.i686  lapack-devel.i686 lapack.i686 \
                      xbae.i686 xbae-devel.i686 libXaw-devel.i686 libXaw.i686 \
                      motif-devel.i686 motif.i686 \
                      pythia8-devel.i686 pythia8.i686  root-core.i686 root.i686 HepMC3-devel.i686 HepMC3.i686  && yum -y clean all

RUN  wget https://download.copr.fedorainfracloud.org/results/averbyts/HEPrpms/epel-8-x86_64/06600747-cernlib/cernlib-2023.08.14.0-2.el8.src.rpm &&  \
     rpmbuild --rebuild cernlib-2023.08.14.0-2.el8.src.rpm --terget=i686  && yum -y install ~/.rpmbuild/RPMS/i686/* && yum -y clean all