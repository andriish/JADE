# Example: K0S -> pi+ pi- (the chain `fptobos` -> `superv` -> `k0s`)

A Fortran analysis of a JADE data tape with the installed JADE software. The example is a CMake project of its own
(`CMakeLists.txt`), it needs only the installed JADE (`find_package(JADE)`, see `JADEConfig.cmake`).

| step | program | card |
|---|---|---|
| `K0S_FPTOBOS` | `fptobos`: the FPACK tape -> the BOS file `file006.bos` | `fptobos.card.in` (the tape name and the byte order are filled in by CMake) |
| `K0S_SUPERV` | `superv`: the reconstruction of the events -> `supervfile006.bos` | `superv.card` |
| `K0S_ANALYSIS` | `k0s` (`k0s.f`): K0S -> pi+ pi- from the pairs of tracks of opposite charges with close helices; prints the mass distribution, writes the distances of the vertices | `k0s.card` (the cuts) |

The programs read their cards from the standard input, in the directory `<build>/run`, which gets the calibration
files of the installation. The environment of the Fortran runtime (byte order of the files) is set with
`jade_fortran_test` of `JADEConfig.cmake`.

## Data

The data are not part of the example. `JADE_EXAMPLE_DATA` is the directory with the FPACK tapes and `JADE_EXAMPLE_TAPE`
(default `JAD073/file006`) the tape in it. Without `JADE_EXAMPLE_DATA` the small sample tape of the installed test project
(`<prefix>/share/JADE/test/jadetapes`) is used.

## Running

    cmake -S k0s -B build -DCMAKE_PREFIX_PATH=<install prefix> [-DJADE_EXAMPLE_DATA=<directory with the tapes>]
    cmake --build build        # the program k0s
    ctest --test-dir build     # the three steps; the results are in build/run (k0s.txt, k0s_dist.txt)

The compilers, the flags and the number of bits are those of the installation. The example is installed to
`<prefix>/share/JADE/examples/k0s` and can be built from there.
