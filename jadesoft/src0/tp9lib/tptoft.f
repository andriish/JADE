C   07/02/87 809291216  MEMBER NAME  TPTOFT   (S)        M  FORTRAN77
C
C-----------------------------------------------------------------------
      SUBROUTINE  TPTOFT
C-----------------------------------------------------------------------
C
C     Author:   C. Bowdery       7/02/87:  Add TOF information to TP
C
C     Last mod: C. Bowdery      29/09/88:  CALL TPSEFL now
C
C     Routine to add TOF information to the TP banks if wanted.
C
C------------------  D E F I N I T I O N S  ----------------------------

      IMPLICIT  INTEGER*2 (H)

      LOGICAL  SUMMRY

      INTEGER  TOF

      PARAMETER ( TOF = 3 )

C------------------  C O D E  ------------------------------------------

C                            Get instruction relating to TOF summary

      CALL TPTFI2( SUMMRY )

      IF( SUMMRY ) THEN

C                            Put summary information into TPTR banks

        CALL TPTFTP
        CALL TPDIAG()

      ELSE

C                            Set flag to indicate TOF summary not done

        CALL TPSEFL( TOF, 4000 )

      ENDIF

      RETURN
      END
