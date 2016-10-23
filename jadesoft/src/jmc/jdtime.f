C   29/09/77 004081356  MEMBER NAME  JDTIME   (S)           FORTRAN
      SUBROUTINE JDTIME( X1, X2, FOUND, DIST, SLOPE )
C *------------------------------------------------------------
C *
C * VERSION OF 03/10/78       LAST MOD 08/04/80    E.ELSEN
C * CALCULATES MINIMUM DRIFT DISTANCE DIST OF A POINT BETWEEN X1
C * AND X2 FROM ORIGIN. IF FOUND IS TRUE POINT BELONGS TO THIS
C * DRIFT SPACE. SLOPE IS SLOPE OF TRACK IN THIS SPACE ( NECCESSARY
C * FOR AMPLITUDE NORMALIZATION ).
C *------------------------------------------------------------
C
      DIMENSION X1(2),X2(2)
      LOGICAL FOUND
      COMMON / CJXDAT / XSLOPE, YSLOPE, XL(3), XH(3), R3P, RD3P,
     *                  S, S2,
     *                  XSL3L, X3L, XSL3H, X3H, YSL3L, Y3L, YSL3H,
     *                  YHWIDT, SINHLF, COSHLF, DRITAN
C
      SLOPE = 1.E5
      FOUND = .FALSE.
C
C        ----- RETURN IF BOTH POINTS ARE NOT IN THIS CELL  -----
      IF(  ( -S .GT. X1(1) .OR. X1(1) .GT. S ) .AND.
     *     ( -S .GT. X2(1) .OR. X2(1) .GT. S )  ) RETURN
C
      FOUND = .TRUE.
      XLOW = AMAX1( AMIN1( X1(1), X2(1) ), -S )
      XHIGH = AMIN1( AMAX1( X1(1), X2(1) ), S )
      DELTAX = X2(1) - X1(1)
      DELTAY = X2(2) - X1(2)
      D12 = X1(1)*X1(1) + X1(2)*X1(2)
      D22 = X2(1)*X2(1) + X2(2)*X2(2)
C
      IF( D12 .LE. S2 .OR. D22 .LE. S2 ) GO TO 200
C             POINTS ARE NOT IN INNER CIRCLE WITH RADIUS S.
C
          IF( ABS( DELTAX ) .LE. 1.0E-5 ) GO TO 100
C                 POINTS ARE NOT TOO CLOSE TOGETHER.
C
C               ----- PART FOR EXPLICIT MINIMIZATION
                  SLOPE = DELTAY / DELTAX
                  YITCPT = X1(2) - SLOPE*X1(1)
                  XMIN = -S * SLOPE / SQRT( 1. + SLOPE*SLOPE )
                  DIST = ABS( SLOPE*XMIN+YITCPT )
C
C                   CHOOSE CORECT SIGN FOR XMIN
                            DIST1 = ABS( -SLOPE*XMIN+YITCPT )
                            IF( DIST1 .GT. DIST ) GO TO  20
                            XMIN = - XMIN
                            DIST = DIST1
   20            XMIN = AMAX1( AMIN1( XMIN, XHIGH ), XLOW )
CCCCCC CHANGED   DIST = DIST - SQRT( S2 - XMIN*XMIN ) + S
                 DIST = ABS( SLOPE*XMIN+YITCPT )
     *                       - SQRT( S2 - XMIN*XMIN ) + S
                 RETURN
C
C
C              ----- TAKE ONE OF THE POINTS AS MINIMAL DISTANCE
  100   DIST = AMIN1( ABS(X1(2)) + S - SQRT( AMAX1(S2-X1(1)*X1(1),0.) ),
     *                ABS(X2(2)) + S - SQRT( AMAX1(S2-X2(1)*X2(1),0.) ))
           RETURN
C
C              ----- MINIMIZE EUCLIDEAN DISTANCE
  200 DIST = SQRT( AMIN1( D12, D22 ) )
      IF( ABS( DELTAX ) .LE. 1.0E-5 ) RETURN
C
C              ----- EXPLICIT MINIMIZATION
                SLOPE = DELTAY / DELTAX
                YITCPT = X1(2) - SLOPE*X1(1)
                XMIN = -SLOPE * YITCPT / (1.0 + SLOPE*SLOPE)
                IF( XLOW .GT. XMIN .OR. XMIN .GT. XHIGH ) RETURN
                DIST = AMIN1( DIST, ABS(YITCPT)/SQRT(1.+SLOPE*SLOPE) )
C
      RETURN
      END
