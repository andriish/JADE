C   11/01/88 803161906  MEMBER NAME  TPTRI1   (S)        M  FORTRAN77
C
C-----------------------------------------------------------------------
      SUBROUTINE  TPTRI1( SUMMRY )
C-----------------------------------------------------------------------
C
C     Author:   C. Bowdery      11/01/88:  Get instr. about TR summary
C
C     Last mod: C. Bowdery      16/03/88:  JETC --> TRACK option
C
C     Routine to provide yes/no answer about need for track
C     summary i.e. creating TPTR banks for central detector tracks.
C
C-----------------   A R G U M E N T S      ----------------------------
C
C     SUMMRY    Out      L*4      TRUE if TP step wanted
C
C------------------  D E F I N I T I O N S  ----------------------------

      IMPLICIT  INTEGER*2 (H)

      CHARACTER  TRACK*8, TP*20

      LOGICAL  SUMMRY, SUM, FIRST

      SAVE  SUM, FIRST

      DATA  FIRST / .TRUE. /
      DATA  TRACK / 'TRACK' /,  TP / 'TP' /

C------------------  C O D E  ------------------------------------------

C                            If not first call, SUM already SAVEd.

      IF( FIRST ) THEN

C                            Get user request relating to track summary

        CALL TPUOPT( TRACK, TP, SUM )

        FIRST = .FALSE.

      ENDIF

C                            Set SUMMRY equal to SUM

      SUMMRY = SUM

C                            Record this in the statistics area

      CALL TPSTAT

      RETURN
      END
