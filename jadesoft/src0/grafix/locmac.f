C   05/08/85 508050935  MEMBER NAME  LOCMAC   (S)           FORTRAN
C
C-----------------------------------------------------------------------
      SUBROUTINE LOCMAC( ICMD, LOCAT )
C-----------------------------------------------------------------------
C
C   AUTHOR:   C. BOWDERY   5/08/85 :  LOCATE MACRO POSITION IN HCMD
C
C
C     GIVEN THE MACRO NUMBER, THIS ROUTINE LOCATES ITS POSITION IN
C     THE HCMD ARRAY OF COMMANDS AND MACROS.
C
C-----------------------------------------------------------------------
C
      IMPLICIT INTEGER*2 (H)
C
      LOGICAL*1  CCMD
C
      COMMON / CGRAP4 / IPOS4,CCMD(8,200),HCMD(5,200)
C
C------------------  C O D E  ------------------------------------------
C
      DO  10  I = 1,IPOS4
        IF( HCMD(1,I) .EQ. ICMD ) GO TO 20
  10  CONTINUE
C
      LOCAT = -1
      RETURN
C
  20  LOCAT = I
      RETURN
      END
