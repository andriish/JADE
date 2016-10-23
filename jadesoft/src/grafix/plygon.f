C   10/04/84 807251624  MEMBER NAME  PLYGON   (S)           FORTRAN
C
C-----------------------------------------------------------------------
      SUBROUTINE PLYGON( NN, R, ADX, ADY, L )
C-----------------------------------------------------------------------
C
C   AUTHOR: J. OLSSON      ?      : DRAW POLYGON
C
C LAST MOD: J. HAGEMANN   5/03/88 : 90 DEGREE ROTATION NOW POSSIBLE
C
C
C     DRAW POLYGON WITH NN CORNERS AND RADIUS R, AROUND ADX,ADY POINT
C     IF L NONZERO, A DASHED CODE WILL BE GENERATED
C     IF L < 0    , ROTATION BY 2*PI/(2*NN)
C
C-----------------------------------------------------------------------
C
      COMMON / CJTRIG / PI,TWOPI
C
C
C------------------  C O D E  ------------------------------------------
C
      LA = L
      FISX = 0.
      DEFI = TWOPI/FLOAT(NN)
      IF( LA .GE. 0) GOTO 2
         FISX = DEFI*0.5
         LA = 0
    2 CONTINUE
      CALL MOVEA(ADX+R*COS(FISX),ADY+R*SIN(FISX))
      DO 5 IIK = 1,NN
         FISX = FISX + DEFI
         IF(LA.GT.0) CALL DASHA(ADX+R*COS(FISX),ADY+R*SIN(FISX),LA)
         IF(LA.EQ.0) CALL DRAWA(ADX+R*COS(FISX),ADY+R*SIN(FISX))
    5 CONTINUE
      RETURN
      END
