C   08/02/87            MEMBER NAME  TPTOFD   (S)        M  FORTRAN77
C
C-----------------------------------------------------------------------
      SUBROUTINE  TPTOFD
C-----------------------------------------------------------------------
C
C     Author:  C. Bowdery        6/02/87:  Delete unwanted TOFR bank
C
C
C
C     Routine to delete unwanted TOFR bank.
C
C------------------  D E F I N I T I O N S  ----------------------------
C
      IMPLICIT  INTEGER*2 (H)
C
      LOGICAL  DELET1
C
C------------------  C O D E  ------------------------------------------

C                            Get instruction relating to bank deletion
C                            DELET1 : delete TOFR bank

      CALL TPTFI3( DELET1 )

      IF( DELET1 ) THEN

        CALL BMLT( 1,'TOFR' )
        CALL BDLM

        CALL TPDIAG()

      ENDIF

      RETURN
      END
