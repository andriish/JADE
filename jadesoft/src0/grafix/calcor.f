C   24/06/85 506240947  MEMBER NAME  CALCOR   (S)           FORTRAN
C
C-----------------------------------------------------------------------
      SUBROUTINE CALCOR( XWIRE, YWIRE, EEX, EEY, DRFWAY )
C-----------------------------------------------------------------------
C
C    AUTHOR:   J. HAGEMANN   17/10/84 :  CALCULATE COORDINATES IN
C                                        R-FI-PLANE FOR VERTEX CHAMBER
C                                        RAW-DATA FROM  B P C H - BANK
C
C-----------------------------------------------------------------------
C
      LOGICAL TBIT, FL18, FL22, FL24
C
#include "cgraph.for"
C
      COMMON/CWORK1/R,FI,R1,FI1,X1,Y1,R2,FI2,X2,Y2,ZET,EX,EY,COSPH,SINPH
     +             ,KZAMP
C
      COMMON / CJTRIG / PI,TWOPI
      COMMON / CPROJ  / XMINR,XMAXR,YMINR,YMAXR,IPRJC,FL18,FL22,FL24
C
C-----------------  C O D E  -------------------------------------------
C
C                            CALCULATE COORDINATES OF DRIFT TIME IN
C                            R-FI-VIEW
      IPRJC = 0
C
      EX    = EEX
      EY    = EEY
C
      XDRF  = DRFWAY*EX
      YDRF  = DRFWAY*EY
C
      X1    = XWIRE + XDRF
      Y1    = YWIRE + YDRF
      R1    = SQRT( X1**2 + Y1**2 )
      FI1   = ATAN2( Y1, X1 )
C
      X2    = XWIRE - XDRF
      Y2    = YWIRE - YDRF
      R2    = SQRT( X2**2 + Y2**2 )
      FI2   = ATAN2( Y2, X2 )
C
      IF( FI1 .LT. 0.0 ) FI1 = FI1 + TWOPI
      IF( FI2 .LT. 0.0 ) FI2 = FI2 + TWOPI
      IF( DSPDTL(9) ) R1 = R1*WRAP(FI1)
      IF( DSPDTL(9) ) R2 = R2*WRAP(FI2)
      IF( .NOT. FL22 .OR. .NOT. FL18 ) GO TO 10
      IF( -X1 .LT. XMINR .OR. -X1 .GT. XMAXR ) IPRJC = 1
      IF( -X2 .LT. XMINR .OR. -X2 .GT. XMAXR ) IPRJC = 1
      IF(  Y1 .LT. YMINR .OR.  Y1 .GT. YMAXR ) IPRJC = 1
      IF(  Y2 .LT. YMINR .OR.  Y2 .GT. YMAXR ) IPRJC = 1
C
   10 CONTINUE
      RETURN
      END
