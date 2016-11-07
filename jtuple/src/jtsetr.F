      subroutine jtsetr
      implicit none

C     FFREAD common:
#include "jtffr.inc"

C     Local declarations:
      character*(namlen) spaces

C     Message:
      print *, 'JTSETR: read FFREAD cards'

C     Reset bosfile counter:
      nbfile= 0

C     Define FFREAD keys:
      lpedro= .false.
      call ffkey( 'PEDRO', lpedro, 1, 'LOGI' )
      lbankw= .false.
      call ffkey( 'LBANKW', lbankw, 1, 'LOGI' )
      lcalib= .true.
      call ffkey( 'LCALIB', lcalib, 1, 'LOGI' )
      langl= .false.
      call ffkey( 'LANGL', langl, 1, 'LOGI' )
      lmc= .true.
      call ffkey( 'MCNTUP', lmc, 1, 'LOGI' )
      lze4v= .false.
      call ffkey( 'ZE4VMODE', lze4v, 1, 'LOGI' )
      maxrec= 10002
      call ffkey( 'MAXREC', maxrec, 1, 'INTE' )
      iobank= 41
      call ffkey( 'IOBANK', iobank, 1, 'INTE' )
      nbankw= 100
      call ffkey( 'NBANKW', nbankw, 1, 'INTE' )
      nwpall= 10
      call ffkey( 'NWPALL', nwpall, 1, 'INTE' )
      nwvect= 10
      call ffkey( 'NWVECT', nwvect, 1, 'INTE' )
      nhrpct= 20
      nhrzct= 12
      zvct= 350.0
      rminct= 50.0
      pcut= 0.1
      call ffkey( 'TRKCUT', nhrpct, 5, 'MIXED' )
      elgct= 0.150
      call ffkey( 'CLSCUT', elgct, 1, 'MIXED' )

      call vblank( svbosf, nwnam )
      call ffkey( 'SVBOSF', svbosf, nwnam, 'MIXED' )
      
      iohbk= 42
      lrec= 1024
      call vblank( hbookf, nwnam )
      call ffkey( 'JTUPLE', iohbk, nwnam+2, 'MIXED' )
      call vblank( hbankf, nwnam )
      call ffkey( 'BANKF', hbankf, nwnam, 'MIXED' )
      call vblank( hbank, nwnam )
      call ffkey( 'BANKS', hbank, nwnam, 'MIXED' )
      call vblank( hcalf, nwnam )
      call ffkey( 'CALIB', hcalf, nwnam, 'MIXED' )

C     Read FFREAD data:
      call ffgo

C     Convert hollerith to character variables:
      call uhtoc( hbookf, nbytew, chbkf, namlen )
      chbkf= spaces( chbkf, 1 )
      call uhtoc( hbankf, nbytew, cbankf, namlen )
      call uhtoc( hbank, nbytew, cbank, namlen )
      call uhtoc( hcalf, nbytew, ccalf, namlen )

      return
      end
