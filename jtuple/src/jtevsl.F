      subroutine jtevsl( lacc )
      implicit none
      logical lacc
#include "jtjtpl.inc"
#include "jtstat.inc"
      real evis, pzbal, pmiss, costt
C     Event selection with ze4v variables only for compatibility
C     with ze4v only real data, i.e. selected tracks and clusters
C     after JADE cluster matching:
      lacc= .true.
C     LG-cluster energy sums in EE and EB calculated after
C     JADE cluster matching and selection:
      if( ebeam.lt.8.0 ) then
         if( ( ee(1).lt.0.2 .or. ee(2).lt.0.2 ) .and. eb.lt.1.2 )
     &        lacc= .false.
      elseif( ebeam.ge.8.0 .and. ebeam.lt.12.0 ) then
         if( ( ee(1).lt.0.4 .or. ee(2).lt.0.4 ) .and. eb.lt.2.0 )
     &        lacc= .false.
      else
         if( ( ee(1).lt.0.4 .or. ee(2).lt.0.4 ) .and. eb.lt.3.0 )
     &        lacc= .false.
      endif
C     Number of good tracks, tracks from vertex region and
C     3-1 prong configuration for ntrkvx=4:
      if( inttr.lt.4 ) lacc= .false.
      if( iltr.lt.3 ) lacc= .false.
      if( itauct.ne.1 ) lacc= .false.
C     Visible energy:
      evis= pgce(4)
      call hfill( 110, evis/ebeam, 0.0, 1.0 )
      if( evis.lt.ebeam ) lacc= .false.
C     z-momentum balance:
      pzbal= pgce(3)
      if( evis.gt.0.0 ) pzbal= pzbal/evis
      call hfill( 111, pzbal, 0.0, 1.0 )
      if( abs(pzbal).gt.0.4 ) lacc= .false.
C     Cut on event vertex z position:
      call hfill( 112, zvert, 0.0, 1.0 )
      if( abs(zvert).gt.150.0 ) lacc= .false.
C     cos(Theta_T):
      costt= 1.0
      if( tvectc(1).ne.0.0 .and. tvectc(2).ne.0.0 .and. 
     &     tvectc(3).ne.0.0 ) then 
c         costt= tvectc(3)/sqrt(tvectc(1)**2+tvectc(2)**2
c     &        +tvectc(3)**2)
         costt= tvectc(3)
      endif
      if( abs(costt).gt.0.8 ) lacc= .false.
      call hfill( 113, costt, 0.0, 1.0 )
C     Missing momentum:
      pmiss= sqrt( pgce(1)**2 + pgce(2)**2 + pgce(3)**2 )
      call hfill( 114, pmiss/ebeam, 0.0, 1.0 )
      if( pmiss.gt.0.6*ebeam ) lacc= .false.

C     Accumulate event selection statistics and set flag in ntuple:
      nevtot= nevtot+1
      if( lacc ) then
         nevacc= nevacc+1
         itkmh= 1
      else
         itkmh= 0
      endif

C     The End:
      return
      end
