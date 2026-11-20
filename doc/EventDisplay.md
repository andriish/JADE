# JADE event display (`jadez`): graphics backends, code changes and tests

This document describes the interface of the event display, the two graphics
backends, every change made to the code for them, and the tests.

## 1. Interface

`jadez` (`jadesoft/main2016/jadez`) is an interactive, command-line driven
display. Its layers:

```
grafix/*          display commands (RB, ZXB, DEDX, OPT, ...), drawing routines
   |  PLOT-10 calls (MOVEA, DRAWA, EOUTST, SYSSYM, ...)
interface/plot10.F, setcol.F, igwlcm.F    emulation of the Tektronix PLOT-10 package
   |  HIGZ calls (ISWN, ISVP, IPL, IPM, ITX, ISPLCI, IXSETLC, IGMETA, ...)
graphics backend   HIGZ (CERNLIB, default)  or  OpenPHIGS
```

The JADE manual (`doc/ComputingNotes`, note 103, section 7.4.2) says the
original display used PLOT-10 (Tektronix storage tubes, DESY IPS) and that the
PLOT-10 calls were mapped onto CERNLIB HIGZ in 1999. JADE code never calls PHIGS
directly. About 40 HIGZ entry points are used (all in `libgrafX11`); only
`plot10.F`, `setcol.F`, `igwlcm.F` and `welcme.f` touch the graphics package.

Datasets are plain files in the working directory: the input BOS file,
`F11LHO.AUPDAT1` (calibration) and `F22BOW.HELP.DATASET`. Dialog: dataset name,
`<ENTER>` for the calibration file, then commands (`HELP`, `MENU`, `STAT`, `END`).
Display flags ("CDTLs") are listed by `STAT` and toggled by `OPT n`
(`OPT 30+n` toggles the second set `DSPDTM(n)`).

## 2. Graphics backends: `-DJADE_GRAPHICS=HIGZ|OPENPHIGS`

Select the backend when configuring `jadesoft` (default `HIGZ`, unchanged):

```
cmake -S jadesoft -B build_ophigs -DJADE_FORCE_32=ON -DJADE_GRAPHICS=OPENPHIGS \
      -DCMAKE_Fortran_COMPILER=gfortran
cmake --build build_ophigs --target jadez
```

* **HIGZ**: CERNLIB `libgrafX11` + packlib/kernlib/mathlib.
* **OPENPHIGS**: `jadesoft/src2016/openphigs/higz_phigs.F` implements the HIGZ
  routines used by JADE on top of OpenPHIGS' Fortran binding (`libphigs`),
  replacing `libgrafX11`. See `jadesoft/src2016/openphigs/README.md`.

`jadeinstall.sh` has no flag for the backend; call CMake directly. In OpenPHIGS
mode all executables also link `libphigs`.

## 3. Code changes

| File | Change |
|---|---|
| `jadesoft/CMakeLists.txt` | `JADE_GRAPHICS` cache option (`HIGZ`, `OPENPHIGS`); builds `highzphigs` and links `libphigs` for OpenPHIGS; compile definition `HPG_HCTYPE` (hardcopy format: 5 = PNG for 32-bit, 7 = EPS otherwise). |
| `jadesoft/src2016/openphigs/higz_phigs.F`, `chpgs.inc` (new) | The HIGZ subset on OpenPHIGS: normalization transformations done in the layer (NDC unit square), structure 1 = screen picture (`IXSETxx` attributes), structure 2 = hardcopy picture (`ISxxxx` attributes), hardcopy written when `H` is used or at exit, via an OpenPHIGS hardcopy workstation. Marker sizes scaled by 0.005, line widths by 0.35 (min 1), text with line width 1, hardcopy scale factor 2 (`JADE_PHIGS_HCSCALE`), optional screen copy (`JADE_PHIGS_SCREENSHOT`). |
| `jadesoft/src2016/grafix/hemsym.f` | **Bug fix.** `HEMSYM` (JADE's stroke font behind `DNUM`) returned immediately (early `RETURN` left by the Feb 2023 cernlib commit, the `ITX` replacement being commented out), so all `DNUM` output was lost: lead glass energy numbers, vertex numbers, the statistics panel. The `RETURN` is removed. The packed-byte decoding used an `INTEGER*2`/`INTEGER*4` `EQUIVALENCE` that only works on big-endian machines; replaced with plain arithmetic. |
| `jadesoft/src2016/interface/plot10.F` | **Bug fix** in `GRTEXT` and `EOUTST`: packed text was unpacked with `BLOW` for exactly N bytes and then byte-reversed in groups of four, which garbled the last partial word of strings whose length is not a multiple of 4 on little-endian machines (axis captions such as `keV/cm`, dates, table end characters, curve labels). Whole words are now unpacked. **Spacing**: the character advance in `GRTEXT` is again `HEIGHT*55/117` (the 2016 value `125/117` spread the letters of the tables and header lines far apart). |
| `cmake/Modules/FindPythia8.cmake`, `test/CMakeLists.txt` | `FindPythia8` provides the imported target `Pythia8::Pythia8`; the Pythia test program links it. |
| `test/` (new files) | `jadez_test.sh`, `jadez_mc_test.sh`, `jadez_rphi_test.sh`, `check_colours.py` and the tests registered in `test/CMakeLists.txt` (section 4). |
| `doc/eventdisplayreferences/` (new) | Two reference pictures of the historic display. |
| `AGENTS.md` (new) | Project guide for AI coding agents: layout, build and test, 32-bit build packages, compilers, display and backends. |

## 4. Tests of the event display

All three are registered in `test/CMakeLists.txt` and run by `ctest`
(`sh jadetest.sh --bits=32`, or `ctest -R JADEZ` in the build directory).

**Requirement: a virtual X display.** The tests never use the desktop display:
they use `xvfb-run` if installed, otherwise the display given in the environment
variable `JADEZ_TEST_DISPLAY`; otherwise they exit with 77 and are reported as
*Skipped*. Without Xvfb a headless display can be started with

```
export XDG_RUNTIME_DIR=<private 0700 dir>
weston --backend=headless --renderer=pixman --socket=jadetest --xwayland &
# the log says "xserver listening on display :N"
JADEZ_TEST_DISPLAY=:N ctest -R JADEZ
```

(kill weston by process id afterwards). OpenPHIGS needs GL (Mesa llvmpipe is
fine).

| Test | Input | What it does | What it checks |
|---|---|---|---|
| `JADEZ_DATA_1` (about 70 s) | `file006.bos` (real data, from `FPTOBOS_DATA_1`) | 32 drawing commands (views `RA RB RC ZXA..ZYD FW RU CYL FWMU ZC VRX VRZX VRZY VC`, `RB1`, `DET`, `TRLG`, `FADC`, muon-filter views `RC1 ZXC1 ZYC1 ZXD1 ZYD1 FWMU1`, `MUPT`, `MUONS`), each followed by a hardcopy `H`; 11 commands without picture (`TRUE STAT VX EC FC CLUS DEDX TOF MASS MUTYP MUR2`); `END`. Commands that prompt for more input (`COM OPT BL DAX ZV AX PRINT ZOOM`) are not used. | exit code 0; no Fortran runtime error; the event was read; no rejected command; at least one hardcopy per drawing command; coloured pictures including the orange lead glass number colour (`check_colours.py`). |
| `JADEZ_MC_1` (about 30 s) | `superv1.bos`, the reconstructed Monte Carlo event of `SUPERV_MC_1` (big-endian) | `OPT 16` (result tables) and `OPT 17` (orthogonal projections) on; detector hardware views `RB1 RC1 ZXB1 ZYB1 ZXC1 ZYC1 FW1 RU1 CYL1` and the large dE/dx plot `DEDX-2`, each with a hardcopy. | as above, plus the thin grey detector boundary lines (chamber boundaries, JADE colour 0.4/0.4/0.4). |
| `JADEZ_RPHI_1` (about 15 s) | `superv1.bos` | The picture that mimics `doc/eventdisplayreferences/2.jpg`: `OPT 13` and `OPT 14` off (no bank listing, header lines present), `OPT 16` and `OPT 17` on, `RB1` (R-phi view with hardware), `DEDX -4` (small dE/dx plot), `H`, `END`. | exit code; no `NAME:` bank listing printed; colours, orange numbers and grey boundary lines; with OpenPHIGS also a pixel check for the small yellow dE/dx plot in the bottom-left corner. |

`check_colours.py [--grey] [--dedx] files...` analyses the hardcopies: PNG files
by pixel colours (needs Pillow, otherwise skipped), PostScript files by the
`r g b c` colour settings.

**Where the pictures are.** In the test working directory (`build/test`):
`JADE.nnn.ps` for HIGZ (the tests also convert them to `JADE.nnn.ps.png` with
`gs` if available), `JADE.nnn.png` plus `JADE.nnn.screen.png` (what the screen
shows) for OpenPHIGS. `JADEZ_MC_1` and `JADEZ_RPHI_1` write into the
subdirectories `mcdisplay/` and `rphidisplay/`; logs are `jadez_test.log`,
`jadez_mc_test.log`, `jadez_rphi_test.log`.

Test status at the time of writing (32-bit, gfortran): all three pass with the
HIGZ build installed by `jadeinstall.sh --bits=32`, and the three scripts also
pass when run by hand with a `jadez` built with `-DJADE_GRAPHICS=OPENPHIGS`
(`bash test/jadez_rphi_test.sh <jadez> jadesoft/main2016/jadez/F22BOW.HELP.DATASET <work dir>`
in a directory with `superv1.bos` and `aupdat1.b`).

## 5. Known limitations and open items

* OpenPHIGS layer: no graphics input (`IGLOC` returns button 0, so cursor
  commands do not work); hardcopy is PNG in 32-bit builds (OpenPHIGS' EPS export
  asks gl2ps for a 4 GiB buffer that overflows; EPS for 64-bit is untested);
  HIGZ fonts and fill patterns are ignored.
* Not reproduced compared with the reference pictures: muon chamber outlines
  (need `MUEV` hits and muon calibration, absent from the sample data; HIGZ does
  not draw them either); pink dashed track segments; filled hit patterns in the
  insets; the axis-triad letters "Y" and "Z" (only "X" is drawn, in both
  backends); the "e" label of the small dE/dx plot. The small plot `DEDX -4`
  draws no axis captions by design (same in the original source).
* The sample events are not those of the reference pictures, so the pictures
  match in structure, not pixel by pixel.
* 64-bit builds: all tests pass with the HIGZ backend (see `h2root/README.md` for the
  `h2root` fix needed on 64 bit). A 64-bit `jadez` built with `-DJADE_GRAPHICS=OPENPHIGS`
  (EPS hardcopy) builds, but the three display test scripts exited with a non-zero
  code; this has not been investigated.
