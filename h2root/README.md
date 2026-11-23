# jade_h2root: ROOT's `h2root` with the 64 bit fix

`h2root` converts HBOOK files (the output of `jtjob`, `jzread`) into ROOT files; the
tests `H2ROOT_*` use it. The `h2root` of ROOT 6.40.04 (Fedora `root-hbook`) crashes
with a segmentation fault on 64 bit machines (about 7 runs out of 8), in `hgnt2_`
at `misc/minicern/src/hbook.f`. This directory contains a fixed copy that is built
and installed as `jade_h2root` (by `jadeinstall.sh`, or
`cmake -S h2root -B build/h2root`), and preferred by `test/CMakeLists.txt`.

## The bug

`h2root.cxx` computes the address of its ntuple buffer as a 64 bit number
(`Long_t add = (Long_t)&bigbuf[...]`) but passes it to Fortran through
`hbnam(const int&, ..., const int& ADD, ...)`, a 32 bit `int`: the upper half of the
address is cut off. In `HBNAM`/`HNMADR` (`hbook.f`) the address is held in `INTEGER`
variables and the word offset to the HBOOK store, `IADDW - LOCF(IQ(1))`, is computed
from the truncated address and the full 64 bit address of `IQ`. The stored offset is
only correct when `(address >> 32) mod 4 == 0`, i.e. in about one run in four; in the
other runs `HGNT2` stores the ntuple values through a wrong index and crashes.

## The sources

`src/` holds the unmodified sources of the ROOT branch `v6-40-00-patches` (commit `e1a6647c7b`, "[hbook] Pass the
full 64 bit buffer address to HBNAM", the fix of https://github.com/root-project/root/issues/23628, backport of
the fix in master): `src/h2root.cxx` (ROOT `main/src`) and `src/hbook.f`, `src/kernlib.f`, `src/zebra.f`,
`src/cernlib.c` (ROOT `misc/minicern/src`). LGPL 2.1, (c) the ROOT team, see `src/LICENSE.root` and
https://root.cern. With them 60 of 60 conversions of the test files succeed (ROOT 6.40.04: 1 of 12), and the
tests `H2ROOT_*` pass repeatedly. When the distribution `root-hbook` contains the fix (it is not in
6.40.04), this directory can be removed (the tests fall back to the `h2root` in the `PATH`).
