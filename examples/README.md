# Examples of analyses with the JADE software

Each example is a directory with a CMake project, cards and a README of its own; there is no project above them.
They use the installed JADE software through its `JADEConfig.cmake` (`find_package(JADE)`) and read the data from a
directory that is given when the example is configured (`-DJADE_EXAMPLE_DATA=<directory>`); no data are part of the
examples.

- [`k0s`](k0s/README.md): `fptobos` -> `superv` -> `k0s`, K0S -> pi+ pi- (Fortran).
- [`root_analysis`](root_analysis/README.md): `fptobos` -> `superv` -> `ze4v` -> `jzread` -> `jtjob` -> `jade_h2root` ->
  `ROOT_analysis_1`, thrust and energies (ROOT).

The examples are installed to `<prefix>/share/JADE/examples` (`JADE_EXAMPLES_DIR` in `JADEConfig.cmake`).
