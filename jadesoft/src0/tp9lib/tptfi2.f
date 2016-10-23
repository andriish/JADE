C   12/01/88 801121118  MEMBER NAME  TPTFI2   (S)        M  FORTRAN77
C
C-----------------------------------------------------------------------
      SUBROUTINE  TPTFI2( SUMMRY )
C-----------------------------------------------------------------------
C
C     Author:   C. Bowdery      12/01/88:  Get instructions about TOF TP
C
C
C
C     Routine to provide yes/no answer about need for TOF summary.
C
C-----------------   A R G U M E N T S      ----------------------------
C
C     SUMMRY    Out      L*4      TRUE if TP step wanted
C
C------------------  D E F I N I T I O N S  ----------------------------

      IMPLICIT  INTEGER*2 (H)

      CHARACTER  TOF*8, TP*20

      LOGICAL  SUMMRY, SUM, FIRST

      SAVE  SUM, FIRST

      DATA  FIRST / .TRUE. /
      DATA  TOF   / 'TOF' /,  TP / 'TP' /

C------------------  C O D E  ------------------------------------------

C                            If not first call, SUM already SAVEd.

      IF( FIRST ) THEN

C                            Get user request relating to TOF summary

        CALL TPUOPT( TOF, TP, SUM )

        FIRST = .FALSE.

      ENDIF

C                            Set SUMMRY equal to SUM

      SUMMRY = SUM

C                            Record this in the statistics area

      CALL TPSTAT

      RETURN
      END
