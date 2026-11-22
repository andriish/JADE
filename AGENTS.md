# AGENTS.md

Guidance for AI coding agents working in this repository. See [README.md](README.md) for full user-facing docs.

## What this is

Software of the JADE experiment (legacy e+e- collider experiment): mostly old Fortran 77 (with some C/C++), built with CMake. It is archival code; correctness and bit-compatibility of behaviour matter more than modernisation. Avoid sweeping refactors, reformatting, or "fixing" legacy idioms (old-style init, jumps into loops, hard-coded endianness handling) unless asked.

## Layout

| Directory | Contents |
|---|---|
| `jadesoft/` | Core JADE software. `src`/`src0`/`src2016` = libraries (original / pre-2016 / current), `main*`, `job*`, `util*` = executables and test jobs, `cal` = calibration, `info` = docs. Has its own `CMakeLists.txt` (project JADESOFT). |
| `convert/` | HepMC3 → JADE (CPROD) MC event converter (C++, needs HepMC3 > 3.2). |
| `fptobos/` | `fptobos` (FPACK → BOS) and `unpjad` utilities for archived data tapes. |
| `jtuple/` | Analysis programs producing QCD-observable ntuples (ROOT format; originally PAW). |
| `picocernlib/` | Minimal built-in replacements for needed CERNLIB routines (ROOT-based). |
| `test/` | CTest suite; steering `cards/`, Pythia8 main in `src/`, sample data in `jadetapes/`. |
| `convert_data_files/` | `converttoroot.py` + cards: raw data → ROOT, runs inside the `jade_soft` container. |
| `cmake/` | Shared modules (compiler setup, ROOT setup, Pythia8 finder) and config templates. |
| `.github/homebrew/` | macOS: Homebrew formulae `cernlib` and `hepmc3` (3.3.1), `install.sh` (local tap `jade/hep`, bottles from ghcr.io). Bottles are rebuilt by `.github/workflows/homebrew-bottles.yml` on the newest macOS image (`xcode-27`) whenever a recipe changes; see `.github/homebrew/README.md`. |
| `doc/` | Scanned computing notes (PDF/PS) — do not modify. |

## Build and test

Build order is fixed and driven by `jadeinstall.sh`: `jadesoft` → `convert` → `jtuple` → `fptobos`. Each is an independent CMake project configured out-of-tree in `build/<name>`; the later ones find the installed JADESOFT via `-DJADESOFT_DIR=<prefix>/share/JADESOFT/cmake`.

```
sh jadeinstall.sh [--bits=32|64] [--toolchain=GNU|Intel|XL|NAG|PGI|SUN|GNU-11..GNU-16] [--prefix=/abs/path]
sh jadetest.sh    [--bits=32|64] [--toolchain=...] [--prefix=/abs/path]
```

- Install goes to `installed<toolchain><bits>` (e.g. `installedGNU32`) in the repo root unless `--prefix` is given. `build/` and install dirs are generated — never commit them.
- `jadetest.sh` configures `test/`, builds, and runs `ctest` (10 s timeout). Tests are sequential and depend on one another (FPTOBOS → pythia8/HepMC3 → CPROD → MCJADE → SUPERV → ZE4V → JZREAD → JTJOB); run the whole chain rather than single tests. Pythia8 is required for the MC tests.
- CMake options (jadesoft): `JADE_OLD_MC` (ON), `JADE_FPE` (OFF), `JADE_USE_CERNLIB` (see `jadesoft/CMakeLists.txt` for the actual default), `JADE_FORCE_32` (ON for `--bits=32`).
- Dependencies: gfortran/gcc/g++, ROOT 6, cmake, HepMC3, LAPACK/BLAS, X11 + Motif, CERNLIB, gengetopt. These are rarely installed on a dev host.

### Use the container

32-bit builds are best tested, and cernlib has 64-bit limitations, so the supported workflow is the prebuilt image:

```
docker run -dit --name jade_soft -v $(pwd):/home ghcr.io/andriish/fedora45x86_64i686_gnu
docker attach jade_soft   # then run jadeinstall.sh / jadetest.sh with --bits=32 inside
```

Dockerfiles for Fedora 45 are in the repo root (`Dockerfile_fedora45x86_64i686_gnu` for 32-bit, `Dockerfile_fedora45x86_64_gnu` for 64-bit, `Dockerfile_fedora45aarch64_gnu` for aarch64, built natively by `.github/workflows/fedora45aarch64_gnu.yml` on `ubuntu-24.04-arm`; the scripts reject `--bits=32` on non-x86 hosts and CMake adds `-fsigned-char` for C/C++ on aarch64/arm because the legacy code assumes signed `char`; they install the tools needed by `jadetest.sh`, including `xvfb-run`, Python with Pillow and ghostscript for the display tests); CentOS Stream 9 and 10 have the same kind of images (`Dockerfile_centos9x86_64_gnu`, `Dockerfile_centos10x86_64_gnu`, workflows `centos9x86_64_gnu.yml`, `centos10x86_64_gnu.yml`); CI definitions are in `.github/workflows/` (one file per image, named like its Dockerfile, e.g. `fedora45x86_64_gnu.yml` for `Dockerfile_fedora45x86_64_gnu`, plus `macos27arm64_gnu.yml` for macOS) and `.gitlab-ci.yml`. If a build tool is missing locally, don't try to install the toolchain on the host — say so, or use the container.

## Fortran compilers available for testing

The code should be tested with more than one Fortran compiler. Three are usually available on the developer machine, but none of the vendor ones is on `PATH` by default — locate and enable them first:

| Compiler | Executable | How to locate / enable |
|---|---|---|
| GNU | `gfortran` | `which gfortran` (normally `/usr/bin`). Default toolchain for `jadeinstall.sh` / `jadetest.sh`. |
| Intel oneAPI | `ifx` (`ifort` was removed in oneAPI 2025) | `ls /opt/intel/oneapi/compiler/*/bin/ifx`; enable with `. /opt/intel/oneapi/setvars.sh` (or `module use /opt/intel/oneapi/modulefiles && module load compiler`). |
| NVIDIA HPC SDK | `nvfortran` (`pgfortran` is an alias) | `ls /opt/nvidia/hpc_sdk/Linux_x86_64/*/compilers/bin/nvfortran`; enable with `module use /opt/nvidia/hpc_sdk/modulefiles && module load nvhpc/<version>` (see `module avail nvhpc`), or prepend that `compilers/bin` to `PATH`. |

Clues if they are not found: `which -a gfortran ifx nvfortran`, `module avail`, `ls /opt`, `find /opt /usr/local -name 'ifx' -o -name 'nvfortran' 2>/dev/null`. Check `<compiler> --version` after enabling.

Notes:
- `jadeinstall.sh` / `jadetest.sh` with `--toolchain=Intel` (any order of `--bits`, `--toolchain`, `--prefix`) source `setvars.sh --force` and use `icx`/`icpx`/`ifx` for everything, with no fallback to GNU (they stop if the compilers are missing). Install the Fortran part with `sudo dnf install intel-oneapi-compiler-fortran` (the C++ package alone has no `ifx`); the install goes to `installedIntel64`. Status (64-bit, ifx/icx/icpx 2026.2): builds and all 32 tests pass, including the display tests. Intel-specific points: the data tests need `F_UFMTENDIAN` (the Intel runtime ignores `GFORTRAN_CONVERT_UNIT`); `PXDIAM` (`jtuple/src/px116.f`) guards against a NaN matrix, because Intel's `MAX(0,NaN)` returns the NaN (gfortran's returns 0) and the Jacobi loop never ended on zero-momentum events; `jade56/newdir.f` uses local `CC`,`SS` instead of the `ENTRY NEWDR2` dummies (ifx error #6482); `-fpe0` is only used with `JADE_FPE` (the code handles bit patterns as reals); linking needs `-lgfortran` (CERNLIB is built with gfortran); `convert` needs `-nofor-main`. HIGZ (gfortran-built CERNLIB) writes the PostScript through libgfortran's unit 77, not the unit that jadez opens, so with Intel the pictures land in `fort.77` and `JADE.nnn.ps` stay empty: the display test scripts split `fort.77` into `JADE.fort77.NNN.ps`. `--toolchain=PGI` now means the NVIDIA HPC SDK: it uses the newest `/opt/nvidia/hpc_sdk/Linux_x86_64/<ver>/compilers/bin` with `nvc`/`nvc++`/`nvfortran` (CMake compiler id `NVHPC`). Status (nvfortran 26.11, 64-bit): builds and all 32 tests pass, including the display tests. NVIDIA specifics: the `PGI|NVHPC` flag branches (`-Msave -Mextend -Mpreprocess -Minit-local-zero`, `-Ktrap=fp` only with `JADE_FPE`); `-Mnodaz -Mnoflushz`: nvfortran treats denormal operands as zero by default, but the BOS library compares bit patterns kept in REAL variables (`NOTLOW` in `boslib/star.for` compares a REAL holding the byte 0x5C with another), so every bank looked low priority and `/BCS/` ran out of space (`NOT ENOUGH SPACE IN /BCS/ TO PUT "LGCL"`); `-Mnofma` (also for C): fused multiply-add makes `P*P-Q*Q` of equal numbers a tiny negative number, so `SQRT` in `jmc/jeloss.f` gave NaN and the tracking loop in `JPRABS` never ended (gfortran and ifx do not fuse here); `muininohash.F` instead of `muini.F` (the nvfortran preprocessor breaks `#` inside Hollerith constants); `-lgfortran` for CERNLIB; `-Mnomain` for `convert`; `IEEE_IS_NAN` instead of the `ISNAN` extension in `zsphrcy.f`; `UWP` formats each word through a variable of the matching type (the NVIDIA runtime rejects an `I` descriptor for a REAL item); `FORT_CONVERT_UNIT` (same syntax as `GFORTRAN_CONVERT_UNIT`) in the test macro and the display scripts; a character literal continued over two source lines is padded to column 132 by nvfortran (`-Mextend`), unlike gfortran, which overflowed the 80 character buffer in `grafix/bankls.F` (FORMAT 866 is now one literal). As with Intel, HIGZ writes the PostScript pictures through libgfortran's unit 77, so they end up in `fort.77`, which the display test scripts split into `JADE.fort77.NNN.ps`.
- Use a separate build/install prefix per compiler (e.g. `--prefix=/abs/path/intel`) so artifacts do not mix.
- NVIDIA Fortran (nvfortran) has an endianness runtime too (`FORT_CONVERT_UNIT`, same syntax as `GFORTRAN_CONVERT_UNIT`; the tests set it), but see the NVIDIA status above: some tests still fail on it.
- Intel/NVIDIA modules and compilers may differ per machine; do not hard-code versions in committed files.

## 32-bit CERNLIB on the developer machine

A 32-bit (`i686`) CERNLIB is installed alongside the 64-bit one, so `--bits=32` builds (`JADE_FORCE_32`, `-m32`) can be done natively with `jadeinstall.sh --bits=32` / `jadetest.sh --bits=32` without the container. This is the preferred configuration (see above).

How to check/locate it (Fedora packaging):
- `rpm -qa | grep -i cernlib` — expect both `cernlib-*.i686` (with `-devel`, `-static`, `-packlib-gfortran`, `-utils`) and `cernlib-*.x86_64`.
- 32-bit libraries: `/usr/lib/cernlib/<version>/lib` (e.g. `libpacklib.a`, `libkernlib.a`, `libmathlib.a`, `libgraflib.a`); 64-bit ones are in `/usr/lib64/cernlib/<version>/lib` and are mostly shared-only.
- 32-bit tools: `/usr/lib/cernlib/<version>/bin`.
- Missing? On Fedora: `sudo dnf install cernlib-devel.i686 cernlib-static.i686 cernlib-packlib-gfortran.i686` (needs 32-bit glibc/libgfortran too, e.g. `glibc-devel.i686 libgfortran.i686`). Do not install packages without asking the user.

Notes:
- The 32-bit libraries are built with gfortran; linking them with Intel or NVIDIA Fortran may fail or misbehave, so use the GNU toolchain for CERNLIB-dependent tests.
- With `JADE_FORCE_32` the CMake files prefer `/usr/lib` over `/usr/lib64` for library search, which is how the 32-bit CERNLIB gets picked up. If the 64-bit one is found instead, check `CMAKE_SYSTEM_LIBRARY_PATH` and the `CERNLIB_*_STATIC` lines in the configure output.

## Native 32-bit build: required packages and known pitfalls

`sh jadeinstall.sh --bits=32` and `sh jadetest.sh --bits=32` work natively on the Fedora developer machine with gfortran once these `.i686` packages are installed (names as in Fedora; check with `rpm -q <name>.i686`):

- Toolchain/runtime: `glibc-devel`, `libatomic`, `libstdc++-devel`, `libgcc`, `libgfortran`.
- X11/Motif/numerics: `libX11-devel`, `libXau-devel`, `libxcb-devel`, `libXdmcp-devel`, `libXt-devel`, `motif-devel`, `lapack-devel`, `blas-devel`.
- CERNLIB: `cernlib-devel`, `cernlib-static`, `cernlib-packlib-gfortran` (see above).
- Convert/jtuple/tests: `HepMC3-devel`, `HepMC3-search-devel`, `pythia8-devel`, `liburing-devel` (ROOT's link line needs `liburing.so`), `root-core` plus `root-hist`, `root-tree`, `root-io`, `root-gui`, `root-graf*`, `root-mathcore`, `root-matrix`, `root-net`, `root-physics`, `root-minuit2`, `root-hbook`.

Symptoms of a missing package: `cannot find crt1.o` (`glibc-devel`), `cannot find .../libatomic.so` (`libatomic`), `Could NOT find Threads` (any of those, check `build/*/CMakeFiles/CMakeConfigureLog.yaml`), HepMC3 config "(64bit)" rejected (`HepMC3-devel.i686`), `HEPMC3_SEARCH_LIB-NOTFOUND` (`HepMC3-search-devel`), `No rule to make target '/usr/lib/liburing.so'` (`liburing-devel`). Many `ROOT::<Component>` "file does not exist" CMake warnings are harmless.

Observed results (gfortran, 32-bit): everything builds; 26 of 29 tests pass out of the box, and 29 of 29 once the 32-bit `h2root` is first on `PATH` (see below). Other notes:
- The CMake flag checks print `Fortran_COMPILER_SUPPORTS_<flag> - Failed` for every flag under `-m32`, but the flags are still applied. Harmless noise.
- Libraries install to `installedGNU32/lib64` (GNUInstallDirs on Fedora), not `lib`.
- Installing 32-bit ROOT replaces the 64-bit `root-core` (they conflict), which breaks the other 64-bit `root-*` packages and ROOT-dependent HEP tools (Rivet, VBFNLO, ...). Do not remove or reinstall ROOT packages without asking the user.

### `h2root` (tests H2ROOT_*, and ROOT_ANALYSIS_* which depend on it)

- `/usr/bin/h2root` comes from `root-hbook`. Fedora's x86_64 repo does not publish `root-hbook.i686`, although Koji builds it (`root.spec` has no i686 exclusion; ROOT builds `h2root` whenever Fortran is enabled). Download it manually from `https://kojipkgs.fedoraproject.org/packages/root/<version>/<release>.fc<N>/i686/root-hbook-<version>-<release>.fc<N>.i686.rpm`, matching the installed `root-core` version.
- Even with both `root-hbook.x86_64` and `root-hbook.i686` installed, both own `/usr/bin/h2root` and rpm keeps the **64-bit** binary, which fails with `libCore.so.6.40: cannot open shared object file` because the 64-bit ROOT libraries are gone. Check with `file /usr/bin/h2root`.
- Workaround without touching system packages: extract the 32-bit binary (`rpm2cpio root-hbook-*.i686.rpm | cpio -idm ./usr/bin/h2root` in a scratch dir; `ldd` it to confirm it resolves `/usr/lib/root/libCore.so.6.40`) and put that directory first on `PATH` before running `jadetest.sh`; `test/CMakeLists.txt` finds it with `find_program(H2ROOT NAMES h2root)`. Alternatively run the ROOT tests in the container.
- Do not set `LD_LIBRARY_PATH=/usr/lib64/root`: it breaks the 32-bit ROOT_ANALYSIS tests.

### Event display (`jadez`) and its test

- `jadez` (`jadesoft/main2016/jadez`, graphics code in `src2016/grafix`) is an interactive command-line program that draws with CERNLIB HIGZ on X11. Datasets are plain files in the working directory: input BOS file, `F11LHO.AUPDAT1` (calibration) and `F22BOW.HELP.DATASET` (help). It needs `GFORTRAN_CONVERT_UNIT="big_endian;native:2"`. Dialog: dataset name, `<ENTER>` for calibration, then commands (`RB`, `ZXB`, `RU`, `END`, ...; `HELP`/`MENU` list them). It reads stdin without an `END=` handler, so EOF on stdin crashes it: always finish a scripted session with `END`.
- Test `JADEZ_DATA_1` (`test/jadez_test.sh`) runs a scripted session. It never uses the desktop display: it uses `xvfb-run` if present or the display in `JADEZ_TEST_DISPLAY`, otherwise it is skipped (exit 77). No Xvfb on this machine: start a private headless X server with `weston --backend=headless --renderer=pixman --socket=jadetest --xwayland` (the log prints `xserver listening on display :N`; needs `XDG_RUNTIME_DIR` set to a private 0700 directory) and run `JADEZ_TEST_DISPLAY=:N ctest -R JADEZ`. Stop weston by PID (`pgrep -f 'socket=jadetest'`), not with a `pkill -f` pattern that also matches your own shell.

### Graphics of `jadez`

The full description (interface, all code changes, the three display tests) is in `doc/EventDisplay.md`; keep it up to date when changing the display or the tests.

- Layering: `grafix` (display commands) -> PLOT-10 emulation (`src2016/interface/plot10.F`, `setcol.F`, `igwlcm.F`) -> HIGZ. Only these three files plus `welcme.f` touch the graphics package; the JADE docs (`doc/ComputingNotes`, note 103, section 7.4.2) say the original display used Tektronix PLOT-10 / IPS (DESY IBM) and that the PLOT-10 calls were mapped onto CERNLIB HIGZ in 1999. JADE code never calls PHIGS directly (the PHIGS libs in `jadesoft/util/Scripts/link` are old, commented-out OPAL-style link lines).
- HIGZ: CERNLIB `libgrafX11` (HIGZ + X11 driver) + packlib/kernlib/mathlib. `jadez` uses about 40 HIGZ entry points (ISWN/ISVP/ISELNT, IPL/IPM/ITX, IS* attributes, IXSET*, IGBOX/IXBOX/IGPAVE, ICLRWK/IUWK, IGMETA/ICLWK for the PostScript metafile, IGLOC cursor, ...).
- Test `JADEZ_DATA_1` runs 32 drawing commands (detector views, muon-filter views `RC1/ZXC1/...`, `MUPT`, `MUONS`, ...) with an `H` hardcopy after each, 11 text-only commands, and checks exit code, hardcopy count and that the pictures are coloured (`test/check_colours.py`). Pictures are left in the test working directory (`build/test`): `JADE.nnn.ps` and `JADE.nnn.ps.png` (converted with `gs`).
- Reference pictures in `doc/eventdisplayreferences/` (`1.jpg` (was `1004227_t.jpg`), `2.jpg` (was `CCdat1_04_09.jpg`), historic displays). Lead glass energy numbers around the calorimeter and the statistics panel (top left) are drawn with `DNUM` -> `HEMSYM` (JADE's own stroke font). Until the fix in `grafix/hemsym.f` they were silently lost: the 2023 "Newcernlib" commit left an early `RETURN` there (and its `ITX` replacement is commented out), and the packed-byte decoding used an `INTEGER*2`/`INTEGER*4` `EQUIVALENCE` that only works on big-endian machines. `test/check_colours.py` now requires the orange `ECAL` colour of these numbers. Still missing compared with the references: muon chamber outlines (need `MUEV` hits and muon calibration, absent from the sample data; HIGZ does not draw them either) and the side-view insets (flags 16 "automatic result display" and 17 "automatic display of the orthogonal projections", see `STAT`/`OPT`, are OFF by default). Data files written by `superv` are big-endian on all units: read them with `GFORTRAN_CONVERT_UNIT=big_endian`, while `file006.bos` and `mcjade1.bos` need `big_endian;native:2`.
- Display tests: `JADEZ_DATA_1` (real data sample, many commands), `JADEZ_MC_1` (reconstructed Monte Carlo event `superv1.bos` with the detector hardware `RB1`, `RC1`, ..., `OPT 16` result tables, `OPT 17` orthogonal projections, large dE/dx plot `DEDX-2`; checks for grey chamber boundary lines and the orange lead glass numbers) and `JADEZ_RPHI_1` (matches `doc/eventdisplayreferences/2.jpg`: `OPT 13` and `OPT 14` off, `OPT 16` and `OPT 17` on, `RB1`, then the small dE/dx plot `DEDX -4`; only header lines, no bank tables). Display flags are toggled with `OPT n` (`OPT 30+n` toggles the second flag set `DSPDTM(n)`); `STAT` lists them. Flag 14 on plus a command with a non-zero argument (e.g. `RB1`) suppresses the blue `BANK PATR` header. `DEDX -4` (small plot) does not draw axis captions by design (same in the original source); `DEDX -2` and `-5` do.
- Text bug fixed in `interface/plot10.F` (`GRTEXT`, `EOUTST`): packed text was unpacked with `BLOW` for exactly N bytes and then byte-reversed inside groups of four, which garbles the last partial word of strings whose length is not a multiple of 4 on little-endian machines (axis captions, `keV/cm`, dates, table end characters, curve labels). Whole words are now unpacked.
- Letter spacing of the character-wise text (`GRTEXT`: result tables, header lines): the advance is `HEIGHT*55/117` (original); the 2016 value `125/117` spaced the letters far apart compared with the historic pictures. Known open item: the axis-triad letters "Y" and "Z" (`croax.F`, via `EOUTST`) are not drawn (only "X"), and the small dE/dx plot lacks the electron curve label "e" at the left edge; 

## 64-bit build (`sh jadeinstall.sh --bits=64`, `sh jadetest.sh --bits=64`)

- Needs the x86_64 development packages: `cernlib-devel`, `cernlib-static`, `HepMC3-devel`, `HepMC3-search-devel`, `root-core` (+ `root-cling`, `root-hbook`, the other `root-*`). The 32-bit and 64-bit builds of `root-core`, `root-cling`, `HepMC3-devel`, `HepMC3-search-devel` and `cernlib-packlib-gfortran` ship the same files and **cannot be installed together**: switching between the 32-bit and the 64-bit setup needs `sudo dnf install --allowerasing ...` (it removes the other architecture's ROOT subpackages). `dnf install --assumeno` does not detect these file conflicts.
- Result: all 32 tests pass (including the three display tests with a virtual display, see below), with `jade_h2root`.
- `h2root` of ROOT 6.40.04 segfaults on 64 bit (address truncated to 32 bit in `h2root.cxx` and `hbook.f`). `h2root/` holds the unmodified sources of the ROOT branch `v6-40-00-patches` (commit `e1a6647c7b`, contains the upstream fix), built and installed as `jade_h2root` by `jadeinstall.sh`; `test/CMakeLists.txt` prefers it. See `h2root/README.md`.
- Debugging ROOT code: the debug packages must match the installed version, from the `updates-debuginfo` repository (`sudo dnf --enablerepo=updates-debuginfo install root-hbook-debuginfo-6.40.04-7.fc45 root-debugsource-6.40.04-7.fc45`); the sources are then under `/usr/src/debug/root-6.40.04-7.fc45.x86_64`.

## macOS (Apple Silicon, Homebrew)

`sh jadeinstall.sh --toolchain=GNU-16` and `sh jadetest.sh --toolchain=GNU-16` (only the versioned `GNU-NN` toolchains select Homebrew's `gcc-NN`/`gfortran-NN`). Status (macOS 27.2 arm64, GCC 16.2 built from source, Apple `ld-27106.1`): everything builds, all 32 tests pass, including the three display tests on Xvfb.

- Dependencies: `brew install cmake gcc coreutils wget gengetopt root pythia openmotif libx11`, plus `cernlib` and `hepmc3` from `.github/homebrew/Formula` (`bash .github/homebrew/install.sh` uses the ghcr.io bottles if they exist; for a local build copy the recipes into the tap `jade/hep` and `brew install --build-from-source jade/hep/cernlib jade/hep/hepmc3`). An older `davidchall/hep/hepmc3` must be uninstalled first.
- Homebrew GCC bottle: on a macOS newer than the one the bottle was built for, the bottle can have a stale sysroot (e.g. `MacOSX14.sdk`) and `include-fixed` headers from that SDK. Symptoms: `ld: library 'System' not found` for any Fortran link (CMake "compiler cannot compile a simple test program"); with `SDKROOT=$(xcrun --show-sdk-path)` Fortran works but C files fail inside GCC's `include-fixed/stdio.h` (`unknown type name 'FILE'`, first seen in `convert/cmdline.c`). Fix: `brew reinstall --build-from-source gcc` (about 1-1.5 h on an M1), or put the SDK the bottle expects into `/Library/Developer/CommandLineTools/SDKs/`. Check with `gcc-16 -v` (`--with-sysroot=`, target `aarch64-apple-darwinNN`).
- Pythia8: `cmake/Modules/FindPythia8.cmake` searches `PYTHIA8_ROOT_DIR` (CMake or environment variable), then `$HOMEBREW_PREFIX/opt/pythia`, `/opt/homebrew/opt/pythia`, `/usr/local/opt/pythia`. Homebrew does not link `share/Pythia8/xmldoc` into `/opt/homebrew/share`, so without the keg paths Pythia8 is "not found" and only 10 tests are configured.
- Display tests: Xvfb from XQuartz (`/opt/X11/bin/Xvfb :99 -screen 0 1280x1024x24 -nolisten tcp &`, then `JADEZ_TEST_DISPLAY=:99`). `test/check_colours.py` needs Pillow: put `/opt/homebrew/bin` (Homebrew `python3` with `pillow`) before `/usr/bin` on `PATH`. The `libX11.so.6` error printed by `test/jadez_test.sh` (X connection check with Python ctypes) is diagnostic only.
- `MACOSX_DEPLOYMENT_TARGET`: `jadeinstall.sh` and `jadetest.sh` default it to the major version of the running macOS (`sw_vers`, e.g. `27.0`), which is what the Homebrew libraries (CERNLIB, ROOT, ...) are built for. It used to be 12.0, which gave one `ld: warning: object file ... was built for newer 'macOS' version (27.0) than being linked (12.0)` per CERNLIB object (thousands of lines in the CI log). Set the variable yourself to build for an older macOS (the warnings then return). Do not mute them with `-Wl,-w`: it hides all linker warnings.

### Apple linker bug: COMMON blocks larger than 64 KiB are truncated

The Apple linker (`ld-prime`, seen with `ld-27106.1`; `-ld_classic` is no longer available) merges tentative definitions (Fortran COMMON blocks; gfortran always emits them as tentative) comparing only the **low 16 bits of the sizes**. E.g. 131072 bytes (low 16 bits 0) loses against 400 bytes, 167440 (`0x28E10`) against 40000 (`0x9C40`). The program then has a COMMON smaller than the code uses and overwrites whatever follows: in JADE this gave `superv` SIGSEGV and `mcjade`/`jzread`/`jtjob`/`py57` SIGBUS inside ZEBRA (`HLIMIT` -> `MZPAW` -> `MZSTOR`, writing into the next loaded dylib) because `/PAWC/` (5M words declared) was linked with 10000 words from `libpacklib.a(hbook1.F.o)`, and later `superv` stopping with `KLREAD ERROR: EVENT TIME = 0` (`/CALIBR/` linked with 40000 instead of 167440 bytes). GNU ld (Linux) takes the largest size, so it is macOS-only. A small reproducer: two objects with `COMMON /XX/ A(32768)` and `COMMON /XX/ B(100)`, linked size is 400 bytes.

- Fix used (each block is wrapped in `#ifdef __APPLE__`, so only macOS builds get it; this relies on the Fortran preprocessor, which the compiler setup enables for all toolchains, and on the compiler predefining `__APPLE__`): a `BLOCK DATA` that initialises one element (`DATA X(1) /0/`) gives a **real** definition (zero-fill, no file size) with the largest size; a real definition always wins over tentative ones, on every linker. It must be in an object linked directly into the executable (a `BLOCK DATA` in a static library is not pulled in). Present in: `superv/user.f` (`SVPAWC` /PAWC/, `SVCALB` /CALIBR/), `mcjade/mcuser.f` (`MJPAWC`), `jadez/xuser.F` (`JZBIGC` /CALIBR/ /CWORK/ /PAWC/), `jtuple/src/jzread.F` (`JZPAWC`, `JZPWR4` /PAWCR4/), `jtjob.F` (`JTPAWC`), `taklumi.f` (`TLPAWC`), and the generators `mcpy57`, `mcjt63`, `mcjt74`, `mchw58d`, `mchw59`, `mcar48p`, `mccj623` (`*PAWC`/`*PAW6..8`).
- Keep the sizes in these blocks equal to the largest declaration in the program (a real definition smaller than a tentative one would itself truncate). When adding a program, or enlarging/adding a COMMON larger than 64 KiB (also CERNLIB ones such as `/PAWC/`, `/PAWCR4/`), add or update the block.
- How to check a program: relink with `-Wl,-map,<file>` (take the command from `build/<proj>/CMakeFiles/<exe>.dir/link.txt`), read the linked size and the supplying object of each `_name_` from the map, and compare with the largest `(common)` size (`nm -m`) among the **linked** inputs listed in the map (not all archive members). `nm -n -m <exe>` plus the next symbol's address also shows the linked size.

## Conventions and gotchas

- Fortran runtime must support endianness conversion (GNU/Intel/NVIDIA work; SUN should work but has not been tested for years). Binary I/O formats (BOS, FPACK, CPROD) are endianness- and word-size-sensitive: be careful with `INTEGER`/`REAL` sizes and `-m32` assumptions.
- Prefer editing `src2016`/`main2016`/`job2016`/`util2016` (current code). `src`, `src0`, `main`, `job`, `util` hold original/pre-2016 versions and prebuilt binaries — leave them alone unless the task is specifically about them.
- `convert/src/AtlasPID.h` (PDG ID classification, replaces the former MCUtils/HEPUtils headers) is a copy of https://gitlab.cern.ch/averbyts/atlaspid (`AtlasPID.h`, master; clone with `ssh://git@gitlab.cern.ch:7999/averbyts/atlaspid.git`, the web URL needs a login). Only `AtlasPID::charge3` is used (`WriterJADE.cc`). Compared with the old MCUtils on all Pythia8 particle IDs, known differences: ions (`1000zzzaaa`) get the right charge (MCUtils gave 0); a few excited-baryon codes (e.g. `112214`, `211116`) differ (not analysed); **bug: anti-diquarks (e.g. `-1103`, `-2203`) get the same sign as the diquark** (MCUtils is right). Harmless here because only `status == 1` particles are converted and diquarks are never final-state. Fix it upstream rather than editing the copy.
- Keep Fortran fixed-form column rules (code in cols 7–72, continuation in col 6); lines past col 72 are silently truncated.
- When adding a source file, register it in the relevant `CMakeLists.txt`; sources are listed explicitly.
- Compiler flags and toolchain handling live in `cmake/Modules/JADE_compiler_setup.cmake`; ROOT in `JADE_ROOT_setup.cmake`.
- Verify changes by building in the container and running `jadetest.sh`; report honestly if you could not run them.
- Commit messages in history are short and descriptive (e.g. "Add Fedora45"); PRs target `master`.
