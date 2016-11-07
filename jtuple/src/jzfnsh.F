      subroutine jzfnsh
      implicit none

#include "jtstat.inc"

      print *, 'JZFNSH: finish this job'

C     Print out some statistics:
      print *, 'JZFNSH: track cut statistics'
      print *, 'Tracks:', ntottk
      print *, 'Tracks rejected by Nhit r-phi:', nrejrp
      print *, 'Tracks rejected by Nhit r-z:', nrejrz
      print *, 'Tracks rejected by abs(zvert):', nrejzv
      print *, 'Tracks rejected by abs(rmin):', nrejrm
      print *, 'Tracks rejected by p_tot:', nrejp
      print *, 'JZFNSH: cluster cut statistics'
      print *, 'Cluster:', ntotcl
      print *, 'Cluster rejected by elgct:', nrejlg
      print *, 'JZFNSH: event statistics'
      print *, 'Total processed events: ', nevtot
      print *, 'Selected events: ', nevacc

      return
      end
