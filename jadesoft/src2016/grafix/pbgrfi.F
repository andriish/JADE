C   01/11/84            MEMBER NAME  PBGRFI   (S)           FORTRAN
C
C-----------------------------------------------------------------------
      SUBROUTINE PBGRFI( SH1, SH2, SH3, DEFIX )
C-----------------------------------------------------------------------
C
C    AUTHOR:   J. OLSSON        ?     :  LG AMPLITUDES  (R-FI-VIEW)
C
C  LAST MOD:   J. HAGEMANN   10/10/84 :  NOW OWN MEMBER (FROM EVDISP)
C
C-----------------------------------------------------------------------
C
      IMPLICIT INTEGER*2 (H)
C
#include "cdata.for"
#include "cgeo1.for"
#include "cgraph.for"
C
      COMMON /CWORK1/ R,FI,XA,COSFI,X1,Y1,YA,SINFI,X2,Y2,ZET,X3,Y3,X4,Y400001900
     +               ,IMW(200)
C
      COMMON /CJTRIG/ PI,TWOPI,PIHALF,PI3HALF
C
C-----------------  C O D E  -------------------------------------------
C
C  LEAD GLASS AMPLITUDES
      CALL PBGSUM(2)
      call setcol('ECAL')     ! PMF 23/11/99: set colour
      DO 4 NROW=1,84
      IESUM=IMW(NROW)
      IF(IESUM.LE.0) GO TO 4
C CORNER POINTS, START PNT FOR TEXT
      X1 = - RLG*COS((NROW-1)*DEFIX)
      Y1 = RLG*SIN((NROW-1)*DEFIX)
      X2 = - RLG*COS(NROW*DEFIX)
      Y2 = RLG*SIN(NROW*DEFIX)
      X3 = - OUTR2*COS(NROW*DEFIX)
      Y3 = OUTR2*SIN(NROW*DEFIX)
      X4 = - OUTR2*COS((NROW-1)*DEFIX)
      Y4 = OUTR2*SIN((NROW-1)*DEFIX)
      IF((NROW.GE.22).AND.(NROW.LE.63)) GO TO 19
      XA = X4
      YA = Y4
      FI = ATAN2((Y4-Y1),(X4-X1))
      IF(FI.GT.PI*.5) FI = FI + PI
      IF(FI.LE.-PI*.5 + .001) FI = FI + PI
      GO TO 20
   19 CONTINUE
      XA = X2
      YA = Y2
      FI = ATAN2((Y3-Y2),(X3-X2))
      IF(FI.LT.0.) FI = FI + TWOPI
   20 CONTINUE
      COSFI = COS(FI)
      SINFI = SIN(FI)
C START POINT FOR TEXT
      X0 = XA + SH1*COSFI - SH2*SINFI
      Y0 = YA + SH1*SINFI + SH2*COSFI
      CALL DNUM(IESUM,X0,Y0,SH3,FI)
    4 CONTINUE
      call setcol(' ')        ! PMF 23/11/99: reset colour
      RETURN
      END
