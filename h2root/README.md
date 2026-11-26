# jade_h2root: ROOT's `h2root` with the 64 bit fix

`h2root` converts HBOOK files (the output of `jtjob`, `jzread`) into ROOT files; the
tests `H2ROOT_*` use it. The `h2root` of ROOT 6.40.04 (Fedora `root-hbook`) crashes
with a segmentation fault on 64 bit machines (about 7 runs out of 8), in `hgnt2_`
at `misc/minicern/src/hbook.f`. This directory contains a fixed copy that is built
and installed as `jade_h2root` (by `jadeinstall.sh`, or
`cmake -S . -B buildGNU64` (the top-level project; `-DJADE_BUILD_H2ROOT=OFF` leaves it out)), and preferred by `test/CMakeLists.txt`.

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

## Big-endian hosts (s390x)

`src/cernlib.c` differs from the ROOT sources in one place: `vxinvb_` and `vxinvc_` (byte inversion
of the words of the HBOOK exchange format, which are big-endian) do nothing, or copy only, on a
big-endian host (`__BYTE_ORDER__` of the compiler, else the architecture macros `__s390__`/`__s390x__`, `__sparc__`, `__hppa__`, `__m68k__`, `__MIPSEB__`, `__ARMEB__`, `__AARCH64EB__`, and PowerPC unless `__LITTLE_ENDIAN__`). ROOT's copy inverts always, which is right on little-endian hosts
only: on s390x `RZOPEN` could not find the record length of the HBOOK file of `JTJOB`
(`RZOPEN. Error in the input file`, `Error on hropen was 3`). With the change `h2root` converts the file
on s390x (checked in the s390x image). The minicern code is independent of CERNLIB, so the `CERNLIB_PPC`
patch of the CERNLIB package does not matter for `jade_h2root`.

The second change is in `src/h2root.cxx`: with the default `optcwn = 1`, integer columns of up to 16 bits become
ROOT `Short_t`/`Char_t` branches, and the loop that packs the 4-byte Fortran integers into 2 or 1 bytes took the
first bytes in memory, which are the low-order bytes only on a little-endian host. On a big-endian host the
low-order bytes are the last ones (`R__BYTESWAP`, as in the handling of the boolean columns a few lines above):
the packed integer columns (`Irun`, `Id02`, `Nhcj`, `Njce*`, `Njcr*`, ...) were 0 or wrong. With both changes the
conversion on s390x gives the same values in all 181 columns of the ntuple of `JTJOB` as the conversion of the
same file on x86_64 (sums and sums of squares of every column).

