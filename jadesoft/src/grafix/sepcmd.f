C   16/07/85 507170002  MEMBER NAME  SEPCOM   (S)           FORTRAN
C
C-----------------------------------------------------------------------
      SUBROUTINE SEPCMD( CHARS, NFIELD, ISTAFL, IENDFL , NEXFLD, NEWNEX)
C-----------------------------------------------------------------------
C
C   AUTHOR:   C. BOWDERY  16/07/85 :  SEPARATE OFF THE NEXT COMMAND
C
C
C     THIS ROUTINE FINDS THE NEXT COMMAND IN THE BUFFER 'CHARS' WHICH
C     BEGINS AT POSITION ISTAFL(NEXFLD) AND ENDS AT THE NEXT ';'.
C     ON RETURN  NEWNEX CONTAINS THE NEXT STARTING POSITION.
C
C-----------------------------------------------------------------------
C
      IMPLICIT INTEGER*2 (H)
C
      LOGICAL*1  CHARS(80), CSEMIC
C
      DIMENSION  ISTAFL(80), IENDFL(80)
C
      DATA  CSEMIC/';'/
C
C-------------------  C O D E  -----------------------------------------
C
      DO  10  I = NEXFLD,NFIELD
        ISTA = ISTAFL(I)
        IEND = IENDFL(I)
        DO  6  J = ISTA,IEND
          IF( CHARS(J) .NE. CSEMIC ) GO TO 6
            NEWNEX = I
            RETURN
   6    CONTINUE
  10  CONTINUE
C
C                            NO SEMI-COLON FOUND ==> END OF BUFFER
C
      NEWNEX = NFIELD + 1
      RETURN
      END
