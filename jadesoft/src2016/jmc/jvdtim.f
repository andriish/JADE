C   16/11/82 606091755  MEMBER NAME  JVDTIM   (S)           FORTRAN
C
C-----------------------------------------------------------------------
      SUBROUTINE JVDTIM( X1, X2, FOUND, DIST, SLOPE )
C-----------------------------------------------------------------------
C
C   AUTHOR:   J. HAGEMANN  16/11/82 :  CALCULATES MINIMUM DRIFT DISTANCE
C             R. RAMCKE
C
C        CALCULATES MINIMUM DRIFT DISTANCE DIST OF A POINT BETWEEN X1
C        AND X2 FROM ORIGIN. IF FOUND IS TRUE POINT BELONGS TO THIS
C        DRIFT SPACE. SLOPE IS SLOPE OF TRACK IN THIS SPACE.
C-----------------------------------------------------------------------
C
      LOGICAL FOUND
C
      COMMON / CJVDAT / XSLOPV, YSLOPV, XLV, XHV,
     *                  SV, SV2, TANLOR
C
      DIMENSION X1(2),X2(2)
C
C------------------------  C O D E  ------------------------------------
C
      SLOPE = 1.E5
      FOUND = .FALSE.
C
C        ----- RETURN IF BOTH POINTS ARE NOT IN THIS CELL  -----
      IF(  ( -SV .GT. X1(1) .OR. X1(1) .GT. SV ) .AND.
     *     ( -SV .GT. X2(1) .OR. X2(1) .GT. SV ) ) RETURN
C
      FOUND = .TRUE.
      XLOW = AMAX1( AMIN1( X1(1), X2(1) ), -SV )
      XHIGH = AMIN1( AMAX1( X1(1), X2(1) ), SV )
      DELTAX = X2(1) - X1(1)
      DELTAY = X2(2) - X1(2)
      D12 = X1(1)*X1(1) + X1(2)*X1(2)
      D22 = X2(1)*X2(1) + X2(2)*X2(2)
C
      IF( D12 .LE. SV2 .OR. D22 .LE. SV2 ) GO TO 200
C             POINTS ARE NOT IN INNER CIRCLE WITH RADIUS SV.
C
          IF( ABS( DELTAX ) .LE. 1.0E-5 ) GO TO 100
C                 POINTS ARE NOT TOO CLOSE TOGETHER.
C
C               ----- PART FOR EXPLICIT MINIMIZATION
                  SLOPE = DELTAY / DELTAX
                  YITCPT = X1(2) - SLOPE*X1(1)
                  XMIN = -SV * SLOPE / SQRT( 1. + SLOPE*SLOPE )
                  DIST = ABS( SLOPE*XMIN+YITCPT )
C
C                   CHOOSE CORECT SIGN FOR XMIN
                            DIST1 = ABS( -SLOPE*XMIN+YITCPT )
                            IF( DIST1 .GT. DIST ) GO TO  20
                            XMIN = - XMIN
                            DIST = DIST1
   20            XMIN = AMAX1( AMIN1( XMIN, XHIGH ), XLOW )
                 DIST = ABS( SLOPE*XMIN+YITCPT )
     *                       - SQRT( SV2 - XMIN*XMIN ) + SV
                 RETURN
C
C
C              ----- TAKE ONE OF THE POINTS AS MINIMAL DISTANCE
  100 DIST = AMIN1( ABS(X1(2)) + SV - SQRT( AMAX1(SV2-X1(1)*X1(1),0.) ),
     *              ABS(X2(2)) + SV - SQRT( AMAX1(SV2-X2(1)*X2(1),0.) ))
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
