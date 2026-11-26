C   25/02/82 205101138  MEMBER NAME  MUORDR   (S)           FORTRAN
      SUBROUTINE MUORDR
C----------------------------------------------------------
C    The original routine was a dummy (E.ELSEN, 25/02/82).
C    It is called by MCJADE after the tracking of the particles of
C    an event, before the bank 'MUEV' is written (WRTMCB). Now it
C    runs the toy simulation of the muon chambers (MUMCEV of the
C    directory jademus), if it is switched on with the control
C    card MUMC ON of MCJADE. The geometry of the chambers is
C    compiled in (block data MUMCBD); no calibration data are read.
C----------------------------------------------------------
      REAL SIGTD, SIGTL, YSIGN, CSIGN
      LOGICAL LDUMP, LACT
      COMMON /MUMCCM/ SIGTD, SIGTL, YSIGN, CSIGN, LDUMP, LACT
C
      IF(LACT) CALL MUMCEV
      RETURN
      END
