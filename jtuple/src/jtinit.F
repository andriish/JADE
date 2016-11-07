      subroutine jtinit
      implicit none

#include "jtffr.inc"
#include "jtstat.inc"

      print *, 'JTINIT: book histos and create ntuple'

C     Initialise statistics counters:
      nrejrp= 0
      nrejrz= 0
      nrejzv= 0
      nrejrm= 0
      nrejp= 0
      nevacc= 0
      nevtot= 0
      nnonr= 0
      ndnonr= 0
      nrejlg= 0
      ntottk= 0
      ntotcl= 0

C     Open file for ascii BOS bank writing:
      if( lbankw ) then
         open( iobank, file=cbankf, status='unknown', form='formatted' )
      endif

C     Book ntuple and histos:
      call jtbknt( lmc, langl )

C     The End:
      return
      end
