# HIGZ on OpenPHIGS

`higz_phigs.F` implements the subset of the CERN HIGZ interface used by the JADE
event display (about 40 routines: `IGINIT IGSSE IGEND IGTERM ISWN ISVP ISELNT IPL
IPM ITX IGBOX IXBOX IGPAVE ICLRWK IUWK IGMETA ICLWK IGLOC IGMESS`, the `IS*` and
`IXSET*` attributes, ...) on top of OpenPHIGS' Fortran binding. It is linked
instead of `libgrafX11` with `cmake -DJADE_GRAPHICS=OPENPHIGS` (see
`doc/EventDisplay.md`).

* Normalization transformations are applied here; PHIGS works in a unit square.
* Structure 1 holds the picture shown on the display workstation (workstation 1,
  type 3), with the `IXSETxx` colours; structure 2 holds a copy with the `ISxxxx`
  colours for the hardcopy (what HIGZ wrote to its PostScript metafile). The
  hardcopy is written by an OpenPHIGS hardcopy workstation when a different file is
  opened (the `H` command) and at exit, never for an empty picture.
* Environment: `JADE_PHIGS_HCSCALE` (hardcopy scale factor, default 2),
  `JADE_PHIGS_SCREENSHOT` (also write `JADE.nnn.screen.png`, the screen picture).
* Compile definition `HPG_HCTYPE`: 5 = PNG (32-bit builds), 7 = EPS.
* Not implemented: graphics input (`IGLOC`), HIGZ fonts and fill patterns.
