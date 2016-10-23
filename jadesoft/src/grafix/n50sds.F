C   14/03/84 403210103  MEMBER NAME  N50SDS   (JADEGS)      FORTRAN
C
C-----------------------------------------------------------------------
      SUBROUTINE N50SDS
C-----------------------------------------------------------------------
C
C   AUTHOR:   C. BOWDERY  20/03/84 :  DRAW RESULTS OF NORD 50 PATRCH
C
C
C        THIS ROUTINE IS BASED ON A ROUTINE BY HOWARD MILLS WHICH DRAWS
C        TRACKS FOUND BY THE NORD 50 PATTERN RECOGNITION PROGRAM PATRCH.
C        THE TRACK DATA IS 'PATR' BANK FORMAT IN THE 'IPJC' SUB-BANK OF
C        THE 'N50S' BANK. SEE JADE NOTE 78, TABLE 11.
C
C
C-----------------------------------------------------------------------
C
      IMPLICIT INTEGER*2 (H)
C
      LOGICAL RPVIEW, ZXVIEW
C
#include "cdata.for"
#include "cgraph.for"
C
      DATA BMAX      /   0.0175     /
      DATA GT,GT2    /  30.0,   7.5 /
      DATA XCUT,YLIM / 300.0, 110.0 /
C
C-------------------  C O D E  -----------------------------------------
C
C                            CHECK THAT THERE ARE TRACKS TO DRAW
C
      N50S = IDATA( IBLN( 'N50S' ) )
      N50S = N50S * 2
      IF( N50S .GT. 0 ) GO TO 2
        WRITE(JUSCRN,1)
   1    FORMAT(' NO ''N50S'' BANK AVAILABLE')
        RETURN
C
   2  IPJC = HDATA( N50S + 7 )
      IF( IPJC .GT. 0 ) GO TO 4
        WRITE(JUSCRN,3)
   3    FORMAT(' NO JET CHAMBER SUB-BANK IN ''N50S'' BANK'/
     +         ' MAYBE JET CHAMBER WAS NOT ANALYSED BY NORD 50')
        RETURN
C
   4  NTR  = HDATA( N50S + IPJC + 4 )
      IF( NTR .GE. 0 ) GO TO 6
        WRITE(JUSCRN,5)
   5    FORMAT(' NORD 50 PATTERN RECOGNITION NOT DONE')
        RETURN
C
   6  NT   = HDATA( N50S + IPJC + 5 )
      L0   = HDATA( N50S + IPJC + 6 )
      LT   = HDATA( N50S + IPJC + 7 )
C
      IF( NT .GT. 0 ) GO TO 8
        WRITE(JUSCRN,7)
   7    FORMAT(' NORD 50 PATTERN RECOGNITION FOUND NO TRACKS')
        RETURN
C
   8  JPTR   = N50S + IPJC +L0
      IF( LASTVW .LE. 11 ) GO TO 9
        WRITE(JUSCRN,88)
  88    FORMAT(' NORD 50 TRACK RESULTS DISPLAY NOT POSSIBLE HERE')
        RETURN
C
   9  IERROR = 0
      RPVIEW = LASTVW .LE. 3
      ZXVIEW = LASTVW .GE. 4  .AND.  LASTVW .LE. 7
C
      DO  100  I = 1,NT
C
C                            GET COORDINATES OF 1ST & LAST POINTS IN MM
C
        CALL N50SCO( JPTR, X1, Y1, Z1, X2, Y2, Z2, R )
C
        IF( HDATA(JPTR) .EQ. 0 ) GO TO 11
          IF( IERROR .LT. 3 ) WRITE(JUSCRN,10) HDATA(JPTR), I
   10     FORMAT(' BANK ERROR?  THE ZEROTH HALF-WORD IS ',I10/
     +           ' FOR TRACK ',I3,' IN ''N50S''. SHOULD BE ZERO.'/
     +           ' RECOVERY ACTION: CURRENT TRACK WILL BE SKIPPED')
          IERROR = IERROR + 1
          GO TO 100
C
C                            DRAW STRAIGHT LINES FOR Z VIEWS
C
   11   IF( RPVIEW ) GO TO 50
          X  = Z1
          XE = Z2
C
C                            IS Z-RADIUS TRACK DISPLAY WANTED?
C
          IF( .NOT. DSPDTL(9) ) GO TO 20
            R1 = SQRT( X1**2 + Y1**2 )
            R2 = SQRT( X2**2 + Y2**2 )
C
            IF( ZXVIEW ) GO TO 15
              Y  = SIGN(R1,Y1)
              YE = SIGN(R2,Y2)
              GO TO 18
C
  15          Y  = SIGN(R1,X1)
              YE = SIGN(R2,X2)
C
C                            ENSURE TRACK ENDS ARE NOT IN OPPOSITE
C                            OF THE JET CHAMBER DISPLAY
C
  18        IF( Y/YE .LT. 0.0 ) Y = - Y
            GO TO 30
C
C                            PROJECT MODE
C
  20      IF( ZXVIEW ) GO TO 25
            Y  = Y1
            YE = Y2
            GO TO 30
C
  25        Y  = X1
            YE = X2
C
  30      Z0 = HDATA( JPTR + 13 ) * 0.1
          CALL MOVEA( Z0, 0.0 )
          CALL DASHA( X, Y, 3 )
          CALL DRAWA( XE, YE  )
C
C                            FOR Z VIEWS, FLIP SIGN OF XE AS TRNUMB
C                            ALWAYS FLIPS IT SO THAT R VIEWS ARE RIGHT.
C
          XE = - XE
          GO TO 80
C
C                            R-PHI VIEWS   --> CURVED TRACKS UNLESS
C                                              CURVATURE IS VERY SMALL
C
  50      XE = X2
          YE = Y2
          IF( ABS(R) .LE. 2.0E4 ) GO TO 55
            CALL MOVEA( -X1, Y1 )
            CALL DRAWA( -X2, Y2 )
            GO TO 80
C
C                            CIRCULAR ARCS DRAWING SECTION
C
  55      AL     = SQRT( (X2 - X1)**2 + (Y2 - Y1)**2 ) * 0.5
          XM     = (X2 + X1) * 0.5
          YM     = (Y2 + Y1) * 0.5
          THETA  = ATAN2( Y2 - Y1, X2 - X1 )
          ALP    = SIGN( SQRT(R**2 - AL**2), R )
C
          XC     = XM + ALP * SIN(THETA)
          YC     = YM - ALP * COS(THETA)
          BETA   = 2.0 * ACOS(ALP/R)
          DELTA  = ATAN2( Y1 - YC, X1 - XC )
          NSTEPS = ABS(BETA) / BMAX
          IF( NSTEPS .GT. 1 ) GO TO 60
            CALL MOVEA( -X1, Y1 )
            CALL DRAWA( -X2, Y2 )
            GO TO 80
C
  60      BP     = SIGN( BETA / NSTEPS, R )
          CALL MOVEA( -X1, Y1 )
          DO  70  J = 1,NSTEPS
            DELTA = DELTA - BP
            X     = XC + ABS(R) * COS(DELTA)
            Y     = YC + ABS(R) * SIN(DELTA)
            CALL DRAWA( -X, Y )
  70      CONTINUE
C
C                            TRACK NUMBERS
C                            NOTE THAT TRNUMB ALWAYS FLIPS SIGN OF XE
C
  80    XE   = XE + SIGN( GT2, XE )
        YE   = YE + SIGN( GT2, YE )
        IF( XE .GT. 0.0 ) XE = XE + GT
        IF( YE .LT. 0.0 ) YE = YE - GT
C
        CALL TRNUMB( I, 0, XE, YE, 0.0 )
C
C                            POINT TO NEXT TRACK
C
        JPTR = JPTR + LT
 100  CONTINUE
C
      IF( RPVIEW ) RETURN
        CALL MOVEA(  XCUT,  YLIM )
        CALL DASHA(  XCUT, -YLIM, 3 )
        CALL MOVEA( -XCUT,  YLIM )
        CALL DASHA( -XCUT, -YLIM, 3 )
C
      RETURN
      END
