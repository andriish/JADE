# Example: thrust and energies with ROOT (the chain `fptobos` -> `superv` -> `ze4v` -> `jzread` -> `jtjob` -> `jade_h2root` -> `ROOT_analysis_1`)

A ROOT analysis of a JADE data tape with the installed JADE software. The example is a CMake project of its own
(`CMakeLists.txt`), it needs only the installed JADE (`find_package(JADE)`, see `JADEConfig.cmake`) and ROOT.

| step | program | card |
|---|---|---|
| `ROOT_FPTOBOS` | `fptobos`: the FPACK tape -> the BOS file `file006.bos` | `fptobos.card.in` (the tape name and the byte order are filled in by CMake) |
| `ROOT_SUPERV` | `superv`: the reconstruction of the events -> `supervfile006.bos` | `superv.card` |
| `ROOT_ZE4V` | `ze4v`: the compressed events (ZE4V format) | `ze4v.card` |
| `ROOT_JZREAD` | `jzread`: reads the compressed events | `jzread.card` |
| `ROOT_JTJOB` | `jtjob`: the HBOOK ntuple `jtjobfile006.hbook` | `jtjob.card` |
| `ROOT_H2ROOT` | `jade_h2root`: the ntuple -> `jtjobfile006.root` | |
| `ROOT_ANALYSIS` | `ROOT_analysis_1` (`ROOT_analysis_1.cc`, a `TSelector`, `jadeanalysis.h`): the thrust (1-T) of tracks and clusters and the energies, as pictures (`jtjobfile006_1.pdf`, `jtjobfile006_1.pdf_energy.pdf`) | |

The programs read their cards from the standard input, in the directory `<build>/run`, which gets the calibration
files of the installation. The environment of the Fortran runtime (byte order of the files) is set with
`jade_fortran_test` of `JADEConfig.cmake`; Fortran is enabled in the project for that reason, although only C++ is compiled.

## Data

The data are not part of the example. `JADE_EXAMPLE_DATA` is the directory with the FPACK tapes and `JADE_EXAMPLE_TAPE`
(default `JAD073/file006`) the tape in it. Without `JADE_EXAMPLE_DATA` the small sample tape of the installed test project
(`<prefix>/share/JADE/test/jadetapes`) is used.

## Running

    cmake -S root_analysis -B build -DCMAKE_PREFIX_PATH=<install prefix> [-DJADE_EXAMPLE_DATA=<directory with the tapes>]
    cmake --build build        # the program ROOT_analysis_1
    ctest --test-dir build     # the seven steps; the results are in build/run

The compilers, the flags and the number of bits are those of the installation. The example is installed to
`<prefix>/share/JADE/examples/root_analysis` and can be built from there.
