C   29/09/86 809301244  MEMBER NAME  TPTAGT   (S)        M  FORTRAN77
C
C-----------------------------------------------------------------------
      SUBROUTINE  TPTAGT
C-----------------------------------------------------------------------
C
C     Author:   C. Bowdery       5/02/87:  Add tagging information to TP
C
C          mod: C. Bowdery       9/02/87:  Remove tidying-up routine
C     Last mod: C. Bowdery      30/09/88:  CALL TPSEFL now
C
C     Routine to add tagging information to the TP banks if wanted.
C
C------------------  D E F I N I T I O N S  ----------------------------

      IMPLICIT  INTEGER*2 (H)

      LOGICAL  SUMMRY

      INTEGER  TAGG

      PARAMETER ( TAGG = 7 )

C------------------  C O D E  ------------------------------------------

C                            Get instruction relating to tagging summary

      CALL TPTGI2( SUMMRY )

      IF( SUMMRY ) THEN

C                            Put summary information into new TPTR banks

        CALL TPTGTP
        CALL TPDIAG()

      ELSE

C                            Set flag to indicate summary not done

         CALL TPSEFL( TAGG, 4000 )

      ENDIF

      RETURN
      END
