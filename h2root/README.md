# jade_h2root: ROOT's `h2root` with a 64 bit fix

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

## The fix (`h2root-64bit-address.patch`, applies to ROOT 6.40.04)

* `h2root.cxx`: the address is a `Long64_t` and `hbnam` takes `const Long64_t&`.
* `hbook.f`: `ADDRES`, `IADD`, `IADDW`, `IBYOF` and `LOCF` are `INTEGER*8` in `HBNAM`
  and `HNMADR`; the offset is checked to fit in 32 bits before it is stored (the
  buffer is a static array next to the HBOOK store, so it does), with an error
  message otherwise.

With the fix 60 of 60 conversions of the test files succeed (upstream: 1 of 12), and
the tests `H2ROOT_*` pass repeatedly (`ctest -R H2ROOT --repeat until-fail:15`).

## Files

`src/h2root.cxx` (ROOT `main/src`), `src/hbook.f`, `src/kernlib.f`, `src/zebra.f`,
`src/cernlib.c` (ROOT `misc/minicern/src`): copies of ROOT 6.40.04 (LGPL 2.1,
(c) the ROOT team, see https://root.cern), modified only as described. The patch is
suitable for a bug report to ROOT/Fedora; when upstream is fixed this directory can
be removed (the tests fall back to the `h2root` in the `PATH`).
