# JADE
This is a repository with software of JADE experiment.

## General information 

 To build the JADE software, the following dependencies should be satisfied:
 
 - C++ compiler with C++11 standard support
 - Fortran compiler
 - ``ROOT6`` (>6.10 is recommended)
 - ``cmake`` (>3.4.5 or higher version required by the used ROOT version)
 - ``HepMC3`` with development packages (>3.2.0)
 - Lapack with development packages 
 - X11 or XQuartz libraries with development packages
 - bash-compatible shell
 - ``make``
 - ``git`` 
 
 The following platforms/toolchains are supported:
 
  - CentOS 7 x86_64 with **GNU** compilers
  - CentOS 9 x86_64 with **GNU** compilers
  - MacOSX 10.15+ x86_64 with **GNU** gfortran and XCode
  
  In addition, it should be possible to compile the software  with the following platforms/toolchains:
  
  - CentOS 7,8,9 x86_64 with **Intel** compilers
  - MacOSX 10.15+ x86_64 with **Intel** compilers
  - CentOS 7,8,9 x86_64 with **NVidia** compilers
  - CentOS 7,8,9 ppc64le/i686/arm64 with **GNU** compilers
  - CentOS 7,8,9 x86_64 with **SUN** compilers

  However, some of the combinations above will produce excutables that would crash in runtime or
  would not be able to read the input files in different endianess.

  Below are some remarks about other Fortran compilers on the supported operating systems/platforms that could be potentially used.
  The usage of these compilers is intended for the debug purposes only. 
   
   - MacOSX 11.0 arm64 with **GNU** Fortran and XCode
      GNU Fortran is is not supported officialy. This option has not been tested yet.
   - CentOS 7 x86_64/CentOS 8 x86_64 with **NAG** Fortran and gcc 
      nagfor 7.0 compiler will not accept the old style-init. The earlier versions seems to be fine.
   - MacOSX 10.15+ x86_64/MacOSX 11.0 arm64/ with **NAG** Fortran and XCode 
      nagfor 7.0 compiler will not accept the old style-init.  These options have not been tested yet.
   - CentOS 8 ppc64le/CentOS 7 ppc64le with IBM **XL** compilers
      Newest xlf will not accept the jumps inside loops.
   - CentOS 7 x86_64/CentOS 8 x86_64/MacOSX 10.15+ x86_64 with **PGI** (NVidia) compilers
      The **PGI** toolchain is not supported so far, as the runtime requires hardcoded little/big endian flags for the I/O.
   - CentOS 7 x86_64/CentOS 8 x86_64 with **SUN** (Oracle) compilers
      The **SUN** toolchain is not supported so far, as the runtime requires hardcoded little/big endian flags for the I/O.

## Building JADE software:

 - Install the dependencies
   - For CentOS7 all the dependencies are in the default and EPEL repositories  
    ```
    yum -y install https://dl.fedoraproject.org/pub/epel/epel-release-latest-7.noarch.rpm
    yum -y install  gcc gcc-c++ gcc-gfortran make git
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
    yum -y install  gcc gcc-c++ gcc-gfortran make git
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
     - Install the dependencies using homebrew from the homebrew default and homebrew-hep repositories (davidchall/hep)
      ```
      brew install coreutils gcc
      brew tap davidchall/hep
      brew install hepmc3
      brew cask install xquartz
      ```
     - Install ROOT, e.g. from  `https://root.cern/install/all_releases/` and enable it
     - Install cmake
     The git and LAPACK should be preinstalled on the newest MacOS.

    - For Apple Silicon

        - Install Docker desktop from ```https://www.docker.com/products/docker-desktop/```

        - In the terminal, navigate to a folder where the program should be installed (using: ```cd path/to/folder```):
        - Clone the JADE repository: ```git clone https://github.com/andriish/JADE```
	        - If the git package is not installed: ```brew install git```
		        - If homebrew is not installed: ```/bin/bash -c "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/HEAD/install.sh)"```
        - Pull the image for the container: ```docker pull ghcr.io/andriish/fedora39x86_64i686_gnu:latest```
        - Running the container and attaching to it (now you are working inside a different environment where the software packages and workflows are cutout for the application):
          ```
          $ docker run -dit --platform linux/amd64 --name jade_soft -v $"(pwd)":/home ghcr.io/andriish/fedora39x86_64i686_gnu
          $ docker attach jade_soft
          ```
        - Installing the JADE software and running some tests (this has to be done inside the attached container):
          ```
          $ sh jadeinstall.sh --bits=32
          $ sh jadetest.sh --bits=32
          ```
            
- Clone the repository using git 
     ``git clone https://github.com/andriish/JADE``
    
- Run build script inside the repository:
    `` sh jadeinstall.sh `` 
    The software will be installed to ``installed`` directory.
    To change the location run ``sh jadeinstall.sh --prefix=/full/path/to/desired/location``

    For all supported platforms the corresponding compilers should be detected automatically.
    The alternative compiler toolchains can be selected using the ``--toolschain`` flag, e.g.
     ``sh jadeinstall.sh --prefix=/full/path/to/desired/location  --toolchain=NAG`` 
    The supported values for this flag are:
      
     - **GNU** 
     - **Intel** 
     - **XL** 
     - **NAG**
     - **PGI** 
    
Please note that JADE software consists of multiple packages that can be compiled sequenially, 
without invocation of the ``jadeinstall.sh``.

The ``jadeinstall.sh``script will create create the ``/build`` directory, where the data files are converted into ``.root`` files (among other things), which can be found in the ``/build/test``directory.

## Testing
To run some simple tests:

 - Install the dependencies
   - For CentOS7 all the dependencies are in the EPEL repository  
     ```
     yum -y install pythia8-devel pythia8 pythia8-data
     ```
   - For CentOS8 all the dependencies are in the EPEL repository
     ```
     yum -y install pythia8-devel pythia8 pythia8-data
     ```
   - For MacOSX the dependencies are in the homebrew-hep repository (davidchall/hep)
     ```
     brew install pythia8
     ```    
    
 - Run the ``jadetest.sh`` script with ``--prefix=/full/path/to/the /installed/software`` (should be the same as for compilation). 
   The script will  run several sequential tests in the `test` directory.
   The script supports the same values for the ``--toolhain`` flag as the ``jadeinstall.sh`` script.


## Development options

Some cmake scripts support the following options
  - ``JADE_OLD_MC:BOOL``  "Compile old MC executables" default is ``ON``
  - ``JADE_FPE:BOOL``  "Trap floating point exceptions" default is  ``OFF``
  - ``JADE_USE_CERNLIB:BOOL`` "Attempt to use CERNLIB instead of buildin code (picocernlib)" default is  ``OFF``
  


