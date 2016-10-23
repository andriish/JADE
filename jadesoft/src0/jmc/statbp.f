C   19/04/79 C9050901   MEMBER NAME  STATBP   (S)           FORTRAN
      SUBROUTINE STATBP( LENGTH, ATBP )
C-----------------------------------------------------------
C
C  VERSION OF 19/04/79  LAST MOD 09/05/79    E.ELSEN
C  PROVIDE DATA FOR BANK 'ATBP' AS DESCRIBED IN
C  JADE NOTE 32.
C  ARRAY ATBP CONTAINS DATA INCLUDING BANK DESCRIPTOR
C  EXTRA ENTRY LHALGL RETURNS LENGTH ONLY.
C----------------------------------------------------------
C
      IMPLICIT INTEGER*2 (H)
C
      COMMON / CBPC   / HBPCAR(24)
C
      DIMENSION ATBP(40), HTBP(80), ITBP(40)
      EQUIVALENCE (HTBP(1),ITBP(1))
C
C
CCC  ZERO ARRAY AND SET BANK DESCRIPTOR
      CALL VZERO( ITBP, 40 )
C
CCC  LOOP OVER COUNTERS AND SET CORRESPONDING ADC AND TDC'S
      DO 1000 J =1,24
      ITOF = J + (J-1)/8 + 54
      IBP = J*2 + (J-1)/6 + 2
      IF( HBPCAR(J) .EQ. 0 ) GO TO 800
C
CCC  FIRED COUNTERS
      HTBP(IBP) = 160
      HTBP(IBP-1) = 160
      HTBP(ITOF) = 50
      GO TO 1000
C
CCC  NOT FIRED COUNTERS
  800 HTBP(ITOF) = 2048
 1000 CONTINUE
C
      CALL MVCL( ATBP, 0, ITBP, 0, 160 )
C
C-----------------------------------------------------------
      ENTRY LHATBP( LENGTH )
C-----------------------------------------------------------
      LENGTH = 40
      RETURN
      END
