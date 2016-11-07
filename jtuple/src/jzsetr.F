      subroutine jzsetr
      implicit none

C     FFREAD common:
#include "jzffr.inc"

C     Local declarations:
      character*(namlen) spaces

C     Message:
      print *, 'JZSETR: read FFREAD cards'

C     Reset file counter:
      nbfile= 0

C     Define FFREAD keys:
      lpedro= .false.
      call ffkey( 'PEDRO', lpedro, 1, 'LOGI' )
      lmc= .false.
      call ffkey( 'MCNTUP', lmc, 1, 'LOGI' )
      langl= .false.
      call ffkey( 'LANGL', langl, 1, 'LOGI' )
      iform= 0
      call ffkey( 'ZE4VFORM', iform, 1, 'INTE' )
      maxevt= 100000
      call ffkey( 'MAXEVT', maxevt, 1, 'INTE' )
      call vblank( hfname, nwnam )
      call ffkey( 'ZE4VFILE', hfname, nwnam, 'MIXED' )
      iohbk= 42
      lrec= 1024
      call vblank( hhbkf, nwnam )
      call ffkey( 'JTUPLE', iohbk, nwnam+2, 'MIXED' )
      nhrpct= 20
      nhrzct= 12
      zvct= 350.0
      rminct= 50.0
      pcut= 0.1
      call ffkey( 'TRKCUT', nhrpct, 5, 'MIXED' )
      elgct= 0.150
      call ffkey( 'CLSCUT', elgct, 1, 'MIXED' )

C     Read FFREAD data:
      call ffgo

C     Convert hollerith to character variables:
      call uhtoc( hhbkf, nbytew, chbkf, namlen )
      chbkf= spaces( chbkf, 1 )

C     The End:
      return
      end
