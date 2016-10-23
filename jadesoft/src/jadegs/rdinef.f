C   19/02/84 402192309  MEMBER NAME  RDINEF   (JADEGS)      FORTRAN
C
C-----------------------------------------------------------------------
      SUBROUTINE RDINEF( EPS, NHITS, HITS, IERASE )
C-----------------------------------------------------------------------
C
C   AUTHOR:   E. ELSEN    25/04/79 :  SIMULATE WIRE INEFFICIENCIES
C
C   LAST MOD: E. ELSEN    18/05/81 :
C
C     SET SOME DRIFT WIRES IN ARRAY HITS WITH NHITS HITS TO
C     ZERO TO ACOUNT FOR EFFICIENCIES EPS(I) IN RING I.
C     NUMBER OF EREASED HITS IS IERASE.
C
C-----------------------------------------------------------------------
C
      IMPLICIT INTEGER*2 (H)
C
      DIMENSION EPS(3), HITS(2)
C
C------------------  C O D E  ------------------------------------------
C
      IERASE = 0
      IF( NHITS .LE. 0 ) RETURN
      IF( EPS(1)*EPS(2)*EPS(3) .GT. .999 ) RETURN
C
C    LOOP OVER ALL HITS AND ZERO SOME RANDOM WIRES
C
      N4 = NHITS*4 - 3
      DO 1000 J = 1, N4, 4
      IF( HITS(J) .EQ. 0 ) GO TO 1000
          IR = MIN0( HITS(J)/3072 + 1, 3 )
      IF( RN(DUM) .LE. EPS(IR) ) GO TO 1000
      HITS(J) = 0
      IERASE = IERASE + 1
 1000 CONTINUE
      RETURN
      END
