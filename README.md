# JADE
This is a repository with software of JADE experiment.

## Contents

 - [General information](#general-information)
 - [Compiling JADE software in official containers](#compiling-jade-software-in-official-containers)
 - [Building JADE software natively](#building-jade-software-natively)
   - [Supported platforms and toolchains](#supported-platforms-and-toolchains)
   - [Installing the dependencies](#installing-the-dependencies)
   - [Building](#building)
 - [Testing](#testing)
 - [External (non-JADE) codes](#external-non-jade-codes)
 - [Event display](#event-display)
 - [Development options](#development-options)

## General information

 To build the JADE software, the following dependencies should be satisfied:

 - C++ compiler with C++11 standard support
 - Fortran compiler
 - ``ROOT6`` (> 6.10, recommended)
 - ``cmake`` (3.10 or higher; a higher version may be required by the used ROOT version)
 - ``HepMC3`` with development packages (> 3.2)
 - Lapack with development packages
 - X11 (including ``libXt``) or XQuartz libraries with development packages
 - Motif with development packages (e.g. ``motif-devel``), for the event display ``jadez``
 - ``Pythia8`` with development packages, for the Monte Carlo tests (only needed for ``jadetest.sh``)
 - ``gengetopt`` (optional, the generated ``cmdline.c/h`` of ``convert`` are included)
 - bash-compatible shell
 - ``cernlib`` (> 2022, recommended)
 - ``make``
 - ``git``

 Because of certain ``cernlib`` limitations on 64-bit platforms, the 32-bit builds (i686 on x86_64) are the best tested
 configuration of the JADE software. The 64-bit builds pass all the tests as well.
 It is recommended to use one of these configurations, or to use the JADE software in an official container.
 See the available pre-built images for ``x86_64`` and ``aarch64`` here: ``https://github.com/andriish?tab=packages&repo_name=JADE``

## Compiling JADE software in official containers

  - For Linux ``x86_64`` hosts

    - Install docker, podman, or apptainer.

    - Running the container and attaching to it (now you are working inside a different environment where the software packages and workflows are cut out for the application):

      ```
      $ docker run -dit --name jade_soft -v $(pwd):/home ghcr.io/andriish/fedora45x86_64i686_gnu
      $ docker attach jade_soft
      ```
      The first command will run the container and the second will start a shell in the container.
      The container execution can be monitored in the docker-desktop application.

    - For 64-bit builds use the image ``ghcr.io/andriish/fedora45x86_64_gnu`` and ``sh jadeinstall.sh --bits=64`` / ``sh jadetest.sh --bits=64``.
      The Dockerfiles are ``Dockerfile_fedora45x86_64i686_gnu`` (32-bit) and ``Dockerfile_fedora45x86_64_gnu`` (64-bit); they also
      contain the tools needed to run the tests (``ctest``, ``bash``, ``valgrind``, a virtual X server ``xvfb-run`` for the event display tests,
      Python with Pillow and ghostscript).
    - On aarch64 (64-bit ARM) use the image ``ghcr.io/andriish/fedora45aarch64_gnu`` (``Dockerfile_fedora45aarch64_gnu``, built by
      ``.github/workflows/fedora45aarch64_gnu.yml`` on an ARM runner) with the same ``--bits=64`` commands. 32-bit builds are only possible on x86.

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

    - Running the container and attaching to it (now you are working inside a different environment where the software packages and workflows are cut out for the application).
      The list of containers can be found here: ```https://github.com/andriish?tab=packages&repo_name=JADE```
      The container includes all required dependencies. Two options are possible:

      - Native (fast, 64-bit only): the ``aarch64`` image
        ```
        $ docker run -dit --name jade_soft -v $(pwd):/home ghcr.io/andriish/fedora45aarch64_gnu
        $ docker attach jade_soft
        ```
        and then ``sh jadeinstall.sh --bits=64`` and ``sh jadetest.sh --bits=64`` inside the container.

      - Emulated ``x86_64`` (slow, supports 32-bit builds): the ``x86_64`` image run with ``--platform linux/amd64``
        ```
        $ docker run -dit --platform linux/amd64 --name jade_soft -v $(pwd):/home ghcr.io/andriish/fedora45x86_64i686_gnu
        $ docker attach jade_soft
        ```
        and then ``sh jadeinstall.sh --bits=32`` and ``sh jadetest.sh --bits=32`` inside the container.

      The first command will run the container and the second will start a shell in the container.
      The container execution can be monitored in the docker-desktop application.


## Building JADE software natively

### Supported platforms and toolchains

 Supported platforms/toolchains (tested in CI):

  - CentOS Stream 9 and 10 x86_64 with **GNU** compilers
  - Fedora 45 (the official containers) x86_64, i686 on x86_64 and aarch64 with **GNU** compilers

 Tested by hand on Fedora 45 (all tests pass), but not in CI:

  - Fedora 45 x86_64 with **Intel** compilers (oneAPI icx/icpx/ifx 2026.2)
  - Fedora 45 x86_64 with **NVidia** HPC SDK compilers (nvfortran 26.11); the needed fixes are in the ``nvidia`` branch and are not yet in ``master``

 It should be possible to compile the software with the following platforms/toolchains, but this was not tested:

  - MacOSX 10.15+ x86_64 with **GNU** gfortran and XCode
  - MacOSX 10.15+ x86_64 with **Intel** compilers
  - CentOS Stream 9 i686 on x86_64 with **GNU** compilers
  - CentOS Stream 9 and 10 ppc64le with **GNU** compilers
  - CentOS Stream 9 and 10 x86_64 with **SUN** compilers

  However, some of the untested combinations above may produce executables that would crash at runtime or
  would not be able to read the input files in different endianness.

 The **NAG**, **XL** and **SUN** toolchains (see below) in particular might have problems: they may fail to compile the legacy code
 or to read the data in different endianness.

  Below are some remarks about other platforms and Fortran compilers that could be potentially used.
  None of them is tested, and their usage is intended for the debug purposes only.

   - MacOSX 11.0 arm64 with **GNU** Fortran and XCode
      Not supported officially. This option has not been tested yet.
   - Linux x86_64 with **NAG** Fortran and gcc
      nagfor 7.0 compiler will not accept the old style-init. The earlier versions seem to be fine.
   - MacOSX 10.15+ x86_64/MacOSX 11.0 arm64 with **NAG** Fortran and XCode
      nagfor 7.0 compiler will not accept the old style-init. These options have not been tested yet.
   - Linux ppc64le with IBM **XL** compilers
      Newest xlf will not accept the jumps inside loops, so it might have problems.
   - MacOSX 10.15+ x86_64 with **PGI** (NVidia) compilers
      Not tested. On Linux the NVIDIA toolchain works, the runtime endianness is set with ``FORT_CONVERT_UNIT``.
   - CentOS Stream 9 and 10 x86_64 with **SUN** (Oracle) compilers
      The **SUN** toolchain should work, but it has not been tested for years and might have problems.

 Note: CentOS 7 and CentOS 8 are phased out (end of life). They are no longer tested in CI and are no longer supported.

### Installing the dependencies

   - For CentOS Stream 9 and 10 the dependencies are in the default, CRB and EPEL repositories,
     and CERNLIB is in the COPR repository ``averbyts/JADE`` (this is what the CI images do, see ``Dockerfile_centos9x86_64_gnu`` and ``Dockerfile_centos10x86_64_gnu``)
    ```
    yum -y install epel-release dnf-plugins-core
    dnf config-manager --set-enabled crb
    yum -y install  gcc gcc-c++ gcc-gfortran make git which cmake cmake-data cmake-filesystem
    yum -y install  HepMC3*
    yum -y install  lapack-static  lapack-devel lapack  gengetopt  blas-devel blas atlas-devel atlas  openblas-devel openblas openblas-serial64 openblas-threads  --skip-broken
    yum -y install  libX11-devel libX11  libXmu-devel libXmu libXau-devel libXau libXcursor-devel
    yum -y install  libXcursor  libSM-devel libSM libICE libICE-devel libXext-devel libXext
    yum -y install  libXt-devel motif-devel --skip-broken
    yum -y install  root-*6* --exclude=*doc* --exclude=*debug* --skip-broken
    dnf -y copr enable averbyts/JADE
    yum -y install  cernlib*
    ```
   - For MacOSX the XCode and homebrew should be installed. Most dependencies are in the homebrew default repository,
     ``cernlib`` and ``hepmc3`` come from the recipes in this repository (``.github/homebrew/``, see its README):
    ```
    brew install coreutils gcc cmake gengetopt
    brew install --cask xquartz
    brew install openmotif
    bash .github/homebrew/install.sh      # cernlib and hepmc3, run in the root of this repository
    ```
     The last command creates the local tap ``jade/hep``, uses the prebuilt bottles from ghcr.io for Apple Silicon
     when they exist for the current recipe (otherwise Homebrew builds from source) and installs
     ``jade/hep/cernlib`` and ``jade/hep/hepmc3``. Use ``--toolchain=GNU-N`` with the version of the installed
     ``gcc`` (e.g. ``sh jadeinstall.sh --toolchain=GNU-16``), the plain ``GNU`` toolchain does not set the compilers on macOS.
     Install ROOT, e.g. from ``https://root.cern/install/all_releases/``, and enable it.
     Motif (``openmotif``, not tested) is only needed for the event display ``jadez``.
     The git and LAPACK should be preinstalled on the newest MacOS.
     Pythia8 is only needed for the tests, see "Testing".

### Building

- Clone the repository using git
     ``git clone https://github.com/andriish/JADE``

- Run the build script inside the repository:
    `` sh jadeinstall.sh ``
    The software will be installed to the ``installed<toolchain><bits>`` directory, e.g. ``installedGNU64``.
    To change the location run ``sh jadeinstall.sh --prefix=/full/path/to/desired/location``
    (the toolchain name and the number of bits are appended to the prefix as well).
    The word size can be selected with ``--bits=32`` or ``--bits=64`` (the default is the one of the host).
    Remember the values of ``--prefix``, ``--toolchain`` and ``--bits`` used here: ``jadetest.sh`` has to be run with the same ones.

    By default the GNU compilers are used. The other toolchains are selected with the ``--toolchain`` flag, e.g.
     ``sh jadeinstall.sh --prefix=/full/path/to/desired/location --toolchain=Intel``
    The supported values for this flag are:

     - **GNU** (also **GNU-11** to **GNU-16** for the compilers of that version, on macOS ``gcc-N``/``gfortran-N`` from homebrew)
     - **Intel** (oneAPI ``icx``/``icpx``/``ifx``, no fallback to GNU)
     - **PGI** (the NVIDIA HPC SDK: ``nvc``/``nvc++``/``nvfortran``)
     - **XL** (might have problems)
     - **NAG** (might have problems)
     - **SUN** (might have problems)

Please note that JADE software consists of multiple packages that can be compiled sequentially,
without invocation of the ``jadeinstall.sh``.

The ``jadeinstall.sh`` script will create the ``/build`` directory, where the data files are
converted into ``.root`` files (among other things), which can be found in the ``build<toolchain><bits>/test`` directory (e.g. ``buildGNU64/test``).

## Testing

To run some simple tests:

 - Install the dependencies
   - For CentOS Stream 9 and 10 the dependencies are in the EPEL repository
     ```
     yum -y install pythia8-devel pythia8 pythia8-data
     ```
     Optional: the tests of the event display need a virtual X server and tools to check the pictures
     (``yum -y install xorg-x11-server-Xvfb python3 python3-pillow ghostscript``); without ``xvfb-run`` these
     tests are skipped. The ``H2ROOT_*`` and ``ROOT_ANALYSIS_*`` tests need ``jade_h2root`` (see below)
     and are skipped without it. The memory-check tests run only if ``valgrind`` is installed (``yum -y install valgrind``).
   - For MacOSX Pythia8 is in the homebrew default repository
     ```
     brew install pythia
     ```

 - ``jade_h2root``: the tests convert the HBOOK files into ROOT files with ``h2root``. The ``h2root`` of ROOT 6.40.04 (package ``root-hbook``)
   crashes with a segmentation fault on 64-bit machines (it truncates the address of its ntuple buffer to 32 bits).
   Therefore the repository contains a copy of the fixed sources (``h2root/``, see ``h2root/README.md``) that ``jadeinstall.sh`` builds and installs as ``jade_h2root``;
   the tests prefer it, and ``root-hbook`` is not needed. Without ``jade_h2root`` (and without a working ``h2root`` in ``PATH``) the tests are skipped.

 - Run the ``jadetest.sh`` script with exactly the same ``--prefix``, ``--toolchain`` and ``--bits`` options as the ``jadeinstall.sh`` script
   was run with, e.g.
   ``sh jadeinstall.sh --prefix=/full/path --toolchain=Intel --bits=64`` followed by ``sh jadetest.sh --prefix=/full/path --toolchain=Intel --bits=64``.
   The script looks for the software in ``<prefix><toolchain><bits>``, so with different options it will not find the installed software
   (or will test a different build). If an option was omitted for the installation, omit it here as well.
   The script will run several sequential tests in the `test` directory and returns the exit status of ``ctest``.


## External (non-JADE) codes

The JADE experiment code itself is in ``jadesoft/src*``, ``main*``, ``job*``, ``cal``. The following codes
are written by others and are either used as external dependencies or included in the repository.

### Dependencies installed separately

| Code | Used for | Link |
|---|---|---|
| CERNLIB (packlib, kernlib, mathlib, HIGZ/``libgrafX11``) | Required: HBOOK, HIGZ graphics of the event display ``jadez`` | https://cernlib.web.cern.ch/ |
| ROOT 6 | ``jtuple``, ``h2root``, analysis tests | https://root.cern |
| HepMC3 | Event record read by ``convert`` and written by the tests | https://gitlab.cern.ch/hepmc/HepMC3 |
| Pythia 8 | Generation of the MC test events | https://pythia.org |
| LAPACK / BLAS | Linear algebra | https://www.netlib.org/lapack/ |
| X11, Motif | Graphics libraries | https://www.x.org, https://motif.ics.com |
| gengetopt | Generates the command line parser of ``convert`` (optional, ``cmdline.c/h`` are included) | https://www.gnu.org/software/gengetopt/ |
| Xvfb, Python 3 + Pillow, Ghostscript | Virtual display, picture conversion and checks in the display tests | https://www.x.org/releases/X11R7.7/doc/man/man1/Xvfb.1.xhtml, https://python-pillow.org, https://ghostscript.com |

### Codes included in the repository

| Location | Code | Origin / link |
|---|---|---|
| ``convert/src/AtlasPID.h`` | AtlasPID (PDG ID utilities, PDG 2022 numbering) | Andrii Verbytskyi, https://gitlab.cern.ch/averbyts/atlaspid |
| ``convert/cmdline.c``, ``cmdline.h`` | Output of gengetopt 2.23 from ``cmdline.ggo`` | https://www.gnu.org/software/gengetopt/ |
| ``test/src/fjcore/`` | fjcore, the core of FastJet 3.5.3 | https://fastjet.fr |
| ``h2root/`` | ``h2root.cxx`` and the minicern Fortran/C sources of the ROOT branch ``v6-40-00-patches`` (contain the 64 bit fix, ``h2root/README.md``) | https://github.com/root-project/root (``main/src/h2root.cxx``, ``misc/minicern``) |
| ``jadesoft/util*/mcgen/src/`` | Monte Carlo generators used for historic JADE productions: JETSET 6.3 / 7.4 (``jetset63*.f``, ``jt74opal.f``), PYTHIA 5.7 (``pythia5722.f``, ``pystfu.f``, ``pymaxi.f``), HERWIG 5.8 / 5.9 (``herwig58d.f``, ``herwig59.f``), ARIADNE 4.08 (``ariadne408.f``), COJETS 6.23 (``coj623d.f``), the PX and CKERN jet clustering packages (``px113.f``, ``px114.f``, ``ckern104.f``, ``ckern105.f``) | https://www.lund.tp.se/ (Lund MC: JETSET, PYTHIA, ARIADNE), https://herwig.hepforge.org (HERWIG); original CMZ/CAR sources are kept in ``util/mcgen/src`` |
| ``jadesoft/util/Rbin/``, ``Pbin/`` ``fsplit.c`` | ``fsplit`` Fortran file splitter, University of California (BSD licence) | https://www.freebsd.org |
| ``jadesoft/src*/grafix/ellips.f``, ``disaxi.f`` | Mortran/Sheltran-generated sources carrying the "Copyright Stanford Mortran 3.1" notice | https://www.slac.stanford.edu |

Individual files keep the licence and author notices they came with; check the file headers before reusing them.
The reference pictures in ``doc/eventdisplayreferences/`` and the notes in ``doc/`` are historic JADE material.

## Event display

The interactive event display `jadez`, its graphics, the fixes made for
it and its tests are described in ``doc/EventDisplay.md``.

## Development options

Some cmake scripts support the following options
  - ``JADE_OLD_MC:BOOL``  "Compile old MC executables" default is ``ON``
  - ``JADE_FPE:BOOL``  "Trap floating point exceptions" default is  ``OFF``



