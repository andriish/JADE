C   06/11/87 711061733  MEMBER NAME  TPTRCB   (S)        M  FORTRAN77
C
C-----------------------------------------------------------------------
      SUBROUTINE  TPTRCB
C-----------------------------------------------------------------------
C
C     Author:   C. Bowdery       6/11/87:  Do PATR to VECT connection
C
C
C     Routine to perform PATR to VECT connection if needed.
C
C------------------  D E F I N I T I O N S  ----------------------------

      IMPLICIT  INTEGER*2 (H)

      LOGICAL  MC

      INTEGER  IERR

C------------------  C O D E  ------------------------------------------

C                            Is this a MC event?

      CALL TPMC( MC )

C                            If MC then do traceback link.

      IF( MC ) THEN

        CALL BMLT( 1, 'TR4V' )
        CALL BDLM
        CALL MCTR4V( 0, IERR )

        IF( IERR .NE. 0 ) THEN
          CALL TPERRH
        ENDIF

        CALL TPDIAG()

      ENDIF


      RETURN
      END
