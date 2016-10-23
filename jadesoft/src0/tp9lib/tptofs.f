C   07/02/87            MEMBER NAME  TPTOFS   (S)        M  FORTRAN77
C
C-----------------------------------------------------------------------
      SUBROUTINE  TPTOFS
C-----------------------------------------------------------------------
C
C     Author:  C. Bowdery        7/02/87:  Analyses TOF system
C
C
C
C     Routine to analyse TOF counters and determine particle types.
C     Unwanted banks are deleted at the end.
C
C------------------  D E F I N I T I O N S  ----------------------------

      IMPLICIT  INTEGER*2 (H)

C------------------  C O D E  ------------------------------------------

C                            Do TOF analysis  (may do nothing)

      CALL TPTOFA

C                            Add TOF information to banks (if wanted)

      CALL TPTOFT

C                            Delete TOF bank as requested

      CALL TPTOFD


      RETURN
      END
