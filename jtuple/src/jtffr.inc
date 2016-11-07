C -*- mode: fortran -*-
C FFREAD stuff:
      integer namlen, nbytew, nwnam
      parameter( namlen=120, nbytew=4, nwnam=namlen/nbytew )
      integer maxfil, nbfile
      parameter( maxfil=10 )
      logical lbankw, lcalib, langl, lmc, lze4v, lpedro
      integer maxrec, iobank, iohbk, nbankw, nwpall, nwvect, lrec
      integer nhrpct, nhrzct
      real zvct, rminct, pcut
      real elgct
      integer svbosf(nwnam), hbookf(nwnam), ze4vf(nwnam), hbankf(nwnam)
     &     , hbank(nwnam), hcalf(nwnam)
      character*(namlen) chbkf, cze4vf, cbankf, cbank, ccalf
     &     , cbfile(maxfil)
      common /jtffr/ lbankw, lcalib, langl, lmc, lze4v, lpedro
     &     , maxrec, iobank, nbankw, nwpall, nwvect
     &     , nhrpct, nhrzct, zvct, rminct, pcut
     &     , elgct
     &     , svbosf
     &     , iohbk, lrec, hbookf
     &     , chbkf, cze4vf, cbankf, cbank, ccalf
     &     , cbfile, nbfile
      save /jtffr/
