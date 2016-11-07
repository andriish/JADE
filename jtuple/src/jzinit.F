      subroutine jzinit
      implicit none

#include "jzffr.inc"
#include "jtstat.inc"

      print *, 'JZINIT: book histos and create ntuple'

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
      ntotcl= 0
      ntottk= 0

C     Book ntuple:
      call jtbknt( lmc, langl )

C     The End:
      return
      end
