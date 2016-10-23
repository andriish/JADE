C   19/02/84 402192315  MEMBER NAME  RDDDCL   (JADEGS)      FORTRAN
C
C-----------------------------------------------------------------------
      SUBROUTINE RDDDCL( NHITS, HITS, IDEAD )
C-----------------------------------------------------------------------
C
C   AUTHOR:   E. ELSEN    27/07/79 :  DEAD CELL SIMULATION
C
C   LAST MOD: E. ELSEN     5/03/80 :
C
C   SET DEAD WIRES IN ARRAY HITS WITH NHITS HITS TO
C   ZERO . IDEAD COUNTS THE ERASED WIRES
C   NDEAD = NUMBER OF DEAD SINGLE WIRES  ( LIST IN HITD      0-1535 )
C   NCDEAD = NUMBER OF DEAD CELLS        ( LIST IN HCELLD    1-96 )
C   ALL CONSTANTS BLOCKDATA SET IN RDSTAT
C
C-----------------------------------------------------------------------
C
      IMPLICIT INTEGER*2 (H)
C
      COMMON / CRDSTA / NDEAD, NCDEAD, HITD(10), HCELLD(10)
C
      DIMENSION  HITS(1)
C
C------------------  C O D E  ------------------------------------------
C
      IDEAD = 0
      IF( NHITS .LE. 0 ) RETURN
C                                           LOOP OVER HITS
C                                           AND DEAD WIRES
      N4 = NHITS * 4
      IF( NDEAD .LE. 0 ) GO TO 1000
      I4 = 1
      J = 1
C
  100 IF( HITD(J) - HITS(I4)/8 ) 200, 300, 400
C                                           INCREMENT TO NEXT DEAD WIRE
  200 J = J + 1
      IF( J .GT. NDEAD ) GO TO 1000
      GO TO 100
C                                           DEAD WIRE
  300 HITS(I4) = 0
      IDEAD = IDEAD + 1
C                                           NEXT HIT
  400 I4 = I4 + 4
      IF( I4 .GT. N4 ) GO TO 1000
      GO TO 100
C                                           LOOP OVER HITS
C                                           AND DEAD CELLS
 1000 CONTINUE
      IF( NCDEAD .LE. 0 ) GO TO 8000
      I4 = 1
      J = 1
C
 1100 IF( HCELLD(J) - HITS(I4)/128-1 ) 1200, 1300, 1400
C                                           INCREMENT TO NEXT DEAD CELL
 1200 J = J + 1
      IF( J .GT. NCDEAD ) GO TO 8000
      GO TO 1100
C                                           DEAD WIRE
 1300 HITS(I4) = 0
      IDEAD = IDEAD + 1
C                                           NEXT HIT
 1400 I4 = I4 + 4
      IF( I4 .GT. N4 ) GO TO 8000
      GO TO 1100
C
 8000 RETURN
      END
