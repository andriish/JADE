C   26/01/88 801271425  MEMBER NAME  TPREDC   (S)        M  FORTRAN77
C
C-----------------------------------------------------------------------
      SUBROUTINE  TPREDC( REJECT )
C-----------------------------------------------------------------------
C
C     Author:   C. Bowdery      26/01/88:  Do optional event reduction
C
C
C
C     Routine to handle event reduction if wanted.
C
C-----------------   A R G U M E N T S      ----------------------------
C
C     REJECT    Out      L*4      TRUE if event has been 'rejected'
C
C------------------  D E F I N I T I O N S  ----------------------------

      IMPLICIT  INTEGER*2 (H)

      LOGICAL  REDUCE, REJECT

      REAL     EBEFIX

C------------------  C O D E  ------------------------------------------

C                            Get instruction about event reduction

      CALL TPEVI3( REDUCE )

      IF( REDUCE ) THEN

        EBEFIX = 0.0
        CALL MCREDU( EBEFIX, *10 )

      ENDIF

      REJECT = .FALSE.

      RETURN

C                            Alternate return means event rejected

 10   REJECT = .TRUE.

      RETURN
      END
