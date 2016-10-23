C   27/07/87 710121527  MEMBER NAME  TPRFTR   (S)        M  FORTRAN77
C
C-----------------------------------------------------------------------
      SUBROUTINE  TPRFTR
C-----------------------------------------------------------------------
C
C     Author:   C. Bowdery      27/07/87:  Refit central detector tracks
C
C
C
C     Routine to refit central detector tracks (if wanted) and to
C     perform the last stage of traceback for MC events if relevant.
C
C------------------  D E F I N I T I O N S  ----------------------------

      IMPLICIT  INTEGER*2 (H)

C------------------  C O D E  ------------------------------------------

C                            Do R-Phi refitting as required

      CALL TPRPHI

C                            Do R-Z refitting as required

      CALL TPZSFT

C                            Perform traceback if Monte Carlo events

      CALL TPTRCB

      RETURN
      END
