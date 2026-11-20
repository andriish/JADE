# JADE
This is a repository with software of JADE experiment.

## General information 

 To build the JADE software, the following dependencies should be satisfied:
 
 - C++ compiler with C++11 standard support
 - Fortran compiler
 - ``ROOT6`` (>6.10 is recommended)
 - ``cmake`` (>3.10 or higher version required by the used ROOT version)
 - ``HepMC3`` with development packages (>3.2.0)
 - Lapack with development packages 
 - X11 or XQuartz libraries with development packages
 - bash-compatible shell
 - ``cernlib`` (> 2022 is recommended)
 - ``make``
 - ``git``
 
 Because of certain ``cernlib`` limitations the 64-bit platforms the 32-bit platforms are  better tested with JADE software.
 Therefore it is recommended to use one of those or even use the JADE software in an official container.
 See the available pre-build images for ``x86_64`` here: ``https://github.com/andriish?tab=packages&repo_name=JADE``

## Compiling JADE software in official containers 
 
  - For Linux ``x86_64`` hosts 
         
    - Install docker, podman, or apptainer.

    - Running the container and attaching to it (now you are working inside a different environment where the software packages and workflows are cutout for the application):

      ```
      $ docker run -dit --name jade_soft -v $(pwd):/home ghcr.io/andriish/fedora45x86_64i686_gnu
      $ docker attach jade_soft
      ```
      The first command will run the container and the second will start a shell in the container.
      The container execution can be monitored in the docker-desktop application.
  
    - Installing the JADE software and running some tests (this has to be done inside the attached container):
      ```
      $ sh jadeinstall.sh --bits=32
      $ sh jadetest.sh --bits=32
      ```

  - For Apple Silicon hosts 
  
    - Install Docker desktop from ```https://www.docker.com/products/docker-desktop/```

    - In the terminal, navigate to a folder where the program should be installed (using: ```cd path/to/folder```):
    - Clone the JADE repository: ```git clone https://github.com/andriish/JADE```
	  - If the git package is not installed: ```brew install git```
		- If homebrew is not installed: ```/bin/bash -c "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/HEAD/install.sh)"```

    - Pull the image for the container: ```docker pull ghcr.io/andriish/fedora45x86_64i686_gnu:latest```.
          The container include all required dependencies. The list of containers can be found here: ```https://github.com/andriish?tab=packages&repo_name=JADE```
    - Running the container and attaching to it (now you are working inside a different environment where the software packages and workflows are cutout for the application):

      ```
      $ docker run -dit --platform linux/amd64 --name jade_soft -v $(pwd):/home ghcr.io/andriish/fedora45x86_64i686_gnu
      $ docker attach jade_soft
      ```
      The first command will run the container and the second will start a shell in the container.
      The container execution can be monitored in the docker-desktop application.
  
    - Installing the JADE software and running some tests (this has to be done inside the attached container):
      ```
      $ sh jadeinstall.sh --bits=32
      $ sh jadetest.sh --bits=32
      ```


## Compiling JADE software on other platforms
 
 The following platforms/toolchains are recognized/supported:
 
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

### Building JADE software:

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

The ``jadeinstall.sh``script will create create the ``/build`` directory, where the data files are 
converted into ``.root`` files (among other things), which can be found in the ``/build/test``directory.

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


## External (non-JADE) codes

The JADE experiment code itself is in ``jadesoft/src*``, ``main*``, ``job*``, ``cal``. The following codes
are written by others and are either used as external dependencies or included in the repository.

### Dependencies installed separately

| Code | Used for | Link |
|---|---|---|
| CERNLIB (packlib, kernlib, mathlib, HIGZ/``libgrafX11``) | Optional (``JADE_USE_CERNLIB``), HBOOK, HIGZ graphics of the event display ``jadez`` | https://cernlib.web.cern.ch/ |
| ROOT 6 | ``jtuple``, ``picocernlib``, ``h2root``, analysis tests | https://root.cern |
| HepMC3 | Event record read by ``convert`` and written by the tests | https://gitlab.cern.ch/hepmc/HepMC3 |
| Pythia 8 | Generation of the MC test events | https://pythia.org |
| LAPACK / BLAS | Linear algebra | https://www.netlib.org/lapack/ |
| X11, Motif | Graphics libraries | https://www.x.org, https://motif.ics.com |
| gengetopt | Generates the command line parser of ``convert`` (optional, ``cmdline.c/h`` are included) | https://www.gnu.org/software/gengetopt/ |
| Xvfb, Python 3 + Pillow, Ghostscript | Virtual display, picture conversion and checks in the display tests | https://www.x.org/releases/X11R7.7/doc/man/man1/Xvfb.1.xhtml, https://python-pillow.org, https://ghostscript.com |

### Codes included in the repository

| Location | Code | Origin / link |
|---|---|---|
| ``convert/src/PIDUtils.h``, ``PIDCodes.h`` | MCUtils (PDG ID utilities) | Andy Buckley, https://gitlab.com/hepcedar/mcutils |
| ``convert/src/MathUtils.h`` | HEPUtils | Andy Buckley, https://gitlab.com/hepcedar/heputils |
| ``convert/cmdline.c``, ``cmdline.h`` | Output of gengetopt 2.23 from ``cmdline.ggo`` | https://www.gnu.org/software/gengetopt/ |
| ``test/src/fjcore/`` | fjcore, the core of FastJet 3.5.3 | https://fastjet.fr |
| ``picocernlib/`` | Selected routines and headers of CERNLIB (kernlib, mathlib, ``ffread``), rewritten equivalents based on ROOT | https://cernlib.web.cern.ch/ |
| ``h2root/`` | Copy of ``h2root.cxx`` and of the minicern Fortran/C sources of ROOT 6.40.04 with a 64 bit fix (``h2root/README.md``) | https://github.com/root-project/root (``main/src/h2root.cxx``, ``misc/minicern``) |
| ``jadesoft/util*/mcgen/src/`` | Monte Carlo generators used for historic JADE productions: JETSET 6.3 / 7.4 (``jetset63*.f``, ``jt74opal.f``), PYTHIA 5.7 (``pythia5722.f``, ``pystfu.f``, ``pymaxi.f``), HERWIG 5.8 / 5.9 (``herwig58d.f``, ``herwig59.f``), ARIADNE 4.08 (``ariadne408.f``), COJETS 6.23 (``coj623d.f``), the PX and CKERN jet clustering packages (``px113.f``, ``px114.f``, ``ckern104.f``, ``ckern105.f``) | https://www.lund.tp.se/ (Lund MC: JETSET, PYTHIA, ARIADNE), https://herwig.hepforge.org (HERWIG); original CMZ/CAR sources are kept in ``util/mcgen/src`` |
| ``jadesoft/util/Rbin/``, ``Pbin/`` ``fsplit.c`` | ``fsplit`` Fortran file splitter, University of California (BSD licence) | https://www.freebsd.org |
| ``jadesoft/src*/grafix/ellips.f``, ``disaxi.f`` | Mortran/Sheltran-generated sources carrying the "Copyright Stanford Mortran 3.1" notice | https://www.slac.stanford.edu |

Individual files keep the licence and author notices they came with; check the file headers before reusing them.
The reference pictures in ``doc/eventdisplayreferences/`` and the notes in ``doc/`` are historic JADE material.

## Event display

The interactive event display `jadez`, its graphics backends (HIGZ, OpenPHIGS), the fixes made for
it and its tests are described in ``doc/EventDisplay.md``.

## Development options

Some cmake scripts support the following options
  - ``JADE_OLD_MC:BOOL``  "Compile old MC executables" default is ``ON``
  - ``JADE_FPE:BOOL``  "Trap floating point exceptions" default is  ``OFF``
  - ``JADE_USE_CERNLIB:BOOL`` "Attempt to use CERNLIB instead of buildin code (picocernlib)" default is  ``OFF``
  


