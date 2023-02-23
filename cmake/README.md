This directory contains cmake modules that can be (or are) used for 
building JADE software.

  - ``Modules/JADE_compiler_setup.cmake`` is a scripts that sets up the compilers on different platforms.
  - ``Modules/JADE_ROOT_setup.cmake`` is a script that sets up ROOT. The main purpose of this script is 
      to prevent propagation of undesireable compiler flags. 
  - ``Modules/FindPythia8.cmake`` is  needed to look for the Pythia8 installation.

Please note that the cmake scripts that are used to look for some standard libraries, e.g. ``lapack``
are included in the cmake distribution.
