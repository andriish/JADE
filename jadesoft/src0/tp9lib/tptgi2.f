C   11/01/88 801112212  MEMBER NAME  TPTGI2   (S)        M  FORTRAN77
C
C-----------------------------------------------------------------------
      SUBROUTINE  TPTGI2( SUMMRY )
C-----------------------------------------------------------------------
C
C     Author:  C. Bowdery       11/01/88:  Get instructions for tagg TP
C
C
C
C     Routine to provide yes/no answer about need for tagging summary.
C
C-----------------   A R G U M E N T S      ----------------------------
C
C     SUMMRY    Out      L*4      TRUE if TP step wanted
C
C------------------  D E F I N I T I O N S  ----------------------------

      IMPLICIT  INTEGER*2 (H)

      CHARACTER  TAGG*8, TP*20

      LOGICAL  SUMMRY, SUM, FIRST

      SAVE  SUM, FIRST

      DATA  FIRST / .TRUE. /
      DATA  TAGG  / 'TAGG' /,  TP / 'TP' /

C------------------  C O D E  ------------------------------------------

C                            If not first call, SUM already SAVEd.

      IF( FIRST ) THEN

C                            Get user request relating to tagg summary

        CALL TPUOPT( TAGG, TP, SUM )

        FIRST = .FALSE.

      ENDIF

C                            Set SUMMRY equal to SUM

      SUMMRY = SUM

C                            Record this in the statistics area

      CALL TPSTAT

      RETURN
      END
