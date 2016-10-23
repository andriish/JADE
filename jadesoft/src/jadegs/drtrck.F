C   28/10/81 202121751  MEMBER NAME  DRTRCK   (JADESR)      FORTRAN
      SUBROUTINE DRTRCK(IPTR,XO,YO,DR)
C
C     CALCULATION : DR = DISTANCE (XO,YO)-TRACK(IPTR)
C                        +VE IF OUTSIDE CIRCLE
C                        -VE IF INSIDE CIRCLE
C     P. STEFFEN  10.2.82
C
C
      IMPLICIT INTEGER*2 (H)
C
#include "cdata.for"
C
      REAL*8 XC,YC,RC
C
C                                       CURVATURE + RADIUS OF TRACK
C                                       1. POINT + DIRECTION
      X1    = ADATA(IPTR+ 5)
      Y1    = ADATA(IPTR+ 6)
      DRI   = SQRT(ADATA(IPTR+8)**2 + ADATA(IPTR+9)**2)
      DX1   = ADATA(IPTR+8) / DRI
      DY1   = ADATA(IPTR+9) / DRI
      CRV   = ADATA(IPTR+25)
C     PRINT 2001, X1,Y1,DX1,DY1,CRV,XO,YO
C2001 FORMAT(' DRTRCK     :',10F10.5)
C
C                                       MOMENTUM > 1.7 GEV ?
      IF(ABS(CRV).LT..00010) GOTO 200
C     IF(ABS(CRV).LT.1.E-8) CRV = SIGN(1.E-8,CRV)
C
C                                       CIRCLE EXTRAPOLATION
         RC    = 1. / CRV
         XC    = DY1*RC + X1
         YC    =-DX1*RC + Y1
         RC    = DABS(RC)
         DR    = ( ( (XO-XC)**2+(YO-YC)**2 ) / RC - RC) *.5
C     PRINT 2003, CRV,XC,YC,RC,DR
C2003 FORMAT(' DRTRCK(CIR):',10E13.5)
         RETURN
C
C                                       PARABOLA EXTRAPOLATION
  200 CONTINUE
         DRP =-((XO-X1)*DX1 + (YO-Y1)*DY1)**2 * CRV * .5
         DRO = (YO-Y1)*DX1 - (XO-X1)*DY1
         DR  = (DRO - DRP) * SIGN(1.,CRV)
C     PRINT 2002, CRV,DRP,DRO,DR
C2002 FORMAT(' DRTRCK(PAR):',11F10.5)
         RETURN
C
      END
