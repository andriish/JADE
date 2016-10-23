C   29/09/86            MEMBER NAME  TPTAGD   (S)        M  FORTRAN77
C
C-----------------------------------------------------------------------
      SUBROUTINE  TPTAGD
C-----------------------------------------------------------------------
C
C     Author:  C. Bowdery        6/02/87:  Delete unwanted tagg banks
C
C
C
C     Routine to delete unwanted tagging banks.
C
C------------------  D E F I N I T I O N S  ----------------------------
C
      IMPLICIT  INTEGER*2 (H)
C
      LOGICAL  DELET1
C
C------------------  C O D E  ------------------------------------------
C
C                            Get instruction relating to bank deletion
C                            DELET1 : delete all tagging banks

      CALL TPTGI3( DELET1 )

      IF( DELET1 ) THEN

        CALL BMLT( 2,'ACLSTAGG' )
        CALL BDLM

        CALL TPDIAG()

      ENDIF

      RETURN
      END
