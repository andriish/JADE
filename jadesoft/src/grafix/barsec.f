C   09/04/84 504171534  MEMBER NAME  BARSEC   (ZS)          FORTRAN
C
C-----------------------------------------------------------------------
      SUBROUTINE BARSEC(FI0,FIOF1,FIOF2,R1,R2,XC,YC,NSEC,DEFI,LLL)
C-----------------------------------------------------------------------
C
C   AUTHOR: S. CARTWRIGHT 10/04/84 :  DRAW CONCENTRIC POLYGONS
C
C
C     DRAW THE BARREL STRUCTURE OF SOME COMPONENTS IN THE JADE DETECTOR
C     BEAM PIPE COUNTERS, TOF COUNTERS, LEADGLASS CYLINDER, Z-CHAMBER
C     AND 1983-.. FORWARD TAGGING APPARATUS.
C
C     ADAPTED FROM SUBROUTINE BARREL TO ENABLE SECTIONS OF THE DOUBLE
C     POLYGON TO BE OMITTED (DEAD SPACES).
C
C     ARGUMENT LIST:
C         FI0          "STARTING" PHI-COORDINATE (CORNER OF POLYGON)
C         FIOF1        STARTING OFFSET (DRAWING WILL START AT FI0+FIOF1)
C         FIOF2        ENDING OFFSET (DRAWING WILL STOP AT FIEND-FI0F2)
C         R1           RADIUS OF INNER POLYGON
C         R2           RADIUS OF OUTER POLYGON
C         XC,YC        COORDINATES FOR CENTRE OF POLYGONS (USUALLY 0,0)
C         NSEC         NUMBER OF SECTORS TO BE DRAWN
C         DEFI         ANGLE SUBTENDED BY ONE SECTOR (TWOPI/NSIDES)
C         LLL          LINE PATTERN
C
C-----------------------------------------------------------------------
C
      IMPLICIT INTEGER*2 (H)
C
      COMMON / CJTRIG / PI,TWOPI
C
C------------------  C O D E  ------------------------------------------
C
C                            DEFINE A ZEROTH POINT
C
      CFIOF1 = COS(FIOF1+FI0)
      SFIOF1 = SIN(FIOF1+FI0)
      X2     = XC + R1*CFIOF1
      Y2     = YC + R1*SFIOF1
      X3     = XC + R2*CFIOF1
      Y3     = YC + R2*SFIOF1
      FISX   = FI0
C
C                             DRAW SHAPE IN PHI SECTORS
C
          NSEC1 = NSEC - 1
      DO  10  I = 1,NSEC1
           X0   = X2
           Y0   = Y2
           X1   = X3
           Y1   = Y3
           FISX = FISX + DEFI
           CSFX = COS(FISX)
           SNFX = SIN(FISX)
           X2   = XC + R1*CSFX
           Y2   = YC + R1*SNFX
           X3   = XC + R2*CSFX
           Y3   = YC + R2*SNFX
C
           CALL DRAMOV(X2,Y2,X0,Y0,LLL)
C
C                             SELECT NORMAL OR DASHED LINE DISPLAY
C
           IF( LLL .EQ. 0 ) GO TO 5
               CALL DASHA(X1,Y1,LLL)
               CALL DASHA(X3,Y3,LLL)
           GO TO 10
C
  5        CALL DRAWA(X1,Y1)
           CALL DRAWA(X3,Y3)
 10   CONTINUE
C
C                            DRAW THE LAST SECTOR
C
      CFIOF2 = COS(FISX+DEFI-FIOF2)
      SFIOF2 = SIN(FISX+DEFI-FIOF2)
      X0     = X2
      Y0     = Y2
      X1     = X3
      Y1     = Y3
      X2     = XC + R1*CFIOF2
      Y2     = YC + R1*SFIOF2
      X3     = XC + R2*CFIOF2
      Y3     = YC + R2*SFIOF2
      CALL DRAMOV(X2,Y2,X0,Y0,LLL)
      IF( LLL .EQ. 0 ) GO TO 15
          CALL DASHA(X1,Y1,LLL)
          CALL DASHA(X3,Y3,LLL)
          CALL DASHA(X2,Y2,LLL)
      GO TO 20
 15   CALL DRAWA(X1,Y1)
      CALL DRAWA(X3,Y3)
      CALL DRAWA(X2,Y2)
 20   RETURN
      END
