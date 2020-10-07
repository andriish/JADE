# JADE
This is a repository with software of JADE experiment.

The prerequirements to build the software are:
 
 - C++ compiler with C++11 standard support (tested with g++, clang++ and xlC)
 - Fortran compiler (tested with gfortran and xlf)
 - ROOT6 (>6.10 is recommended)
 - cmake (>3.4.5 or higher version required by the used ROOT version)
 - HepMC3 with development packages (>3.1.0)
 - Lapack with development packages 
 - X11 or XQuartz with development packages
 - bash-compatible shell
 - make
 
 The following platforms are supported:
 
  - CentOS 7 x86_64
  - CentOS 8 x86_64
  - MacOSX 10.15/Intel


To build the software:
 - Install the dependencies
   - For CentOS7 all the dependencies are in the default and EPEL repositories  
    ```
    yum -y install https://dl.fedoraproject.org/pub/epel/epel-release-latest-7.noarch.rpm
    yum -y install  gcc gcc-c++ gcc-gfortran make
    yum -y install  cmake3 cmake3-data
    yum -y install  HepMC3*
    yum -y install  lapack-static  lapack-devel lapack  gengetopt  blas-devel blas atlas-devel atlas
    yum -y install  libX11-devel libX11  libXmu-devel libXmu libXau-devel libXau libXcursor-devel  
    yum -y install  libXcursor  libSM-devel libSM libICE libICE-devel libXext-devel libXext
    yum -y install  root-*6* --exclude=*doc* --exclude=*debug* --skip-broken
    ```
   - For CentOS8 all the dependencies are in the default, PowerTools and EPEL repositories  
    ```
    yum -y install https://dl.fedoraproject.org/pub/epel/epel-release-latest-8.noarch.rpm
    yum -y install dnf-plugins-core
    dnf config-manager --set-enabled PowerTools
    yum -y install  gcc gcc-c++ gcc-gfortran make
    yum -y install  cmake cmake-data cmake-filesystem
    yum -y install  HepMC3*
    yum -y install  lapack-static  lapack-devel lapack  gengetopt  blas-devel blas atlas-devel atlas  
    yum -y install  openblas-devel openblas openblas-serial64 openblas-threads
    yum -y install  libX11-devel libX11  libXmu-devel libXmu libXau-devel libXau libXcursor-devel  
    yum -y install  libXcursor  libSM-devel libSM libICE libICE-devel libXext-devel libXext
    yum -y install  root-*6* --exclude=*doc* --exclude=*debug* --skip-broken
    ```
   - For MacOSX
     - Ensure the XCode and homebrew are installed
     - Install the dependencies using homebrew
      ```
      brew install coreutils gcc
      brew tap davidchall/hep
      brew install hepmc3
      brew cask install xquartz
      ```
     - Install ROOT, e.g. from  `https://root.cern/install/all_releases/` and enable it
     - Install cmake
       
- Clone the repository using git 
     ``git clone https://github.com/andriish/JADE``
    
- Run build script inside the repository
    `` sh jadeinstall.sh `` 
    The software will be installed to ``installed`` directory.
    To change the location run ``sh jadeinstall.sh --prefix=/full/path/to/desired/location``
    
Please note that JADE software consists of multiple packages that can be compiled sequenially, 
without invocation of the ``jadeinstall.sh``.


