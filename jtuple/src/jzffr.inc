C -*- mode: fortran -*-
C FFREAD stuff:
      integer namlen, nbytew, nwnam
      parameter( namlen=120, nbytew=4, nwnam=namlen/nbytew )
      integer maxfil
      parameter( maxfil=10 )
      logical lmc, langl, lpedro
      integer nbfile, iohbk, lrec, iform, maxevt, nhrpct, nhrzct
      real zvct, rminct, pcut, elgct
      integer hfname(nwnam), hhbkf(nwnam)
      character*(namlen) cfname(maxfil), chbkf
      common /jzffr/ lmc, langl, lpedro
     &     , nbfile, iform, maxevt
     &     , nhrpct, nhrzct, zvct, rminct, pcut
     &     , elgct
     &     , hfname
     &     , iohbk, lrec, hhbkf
     &     , cfname, chbkf
