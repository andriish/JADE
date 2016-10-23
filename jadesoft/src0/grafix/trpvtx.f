C   28/10/81 807251829  MEMBER NAME  TRPVTX   (S)           FORTRAN
      SUBROUTINE TRPVTX(IPTR,XV,YV,RES)
C
C     EXTRAPOLATION OF TRACK TO VERTEX(XV,YV) +
C     CALCULATION OF POSITION + DIRECTION AT CLOSEST POINT
C     P. STEFFEN                                     82/08/26
C
C
C     INPUT:
C     IPTR = POINTER TO TRACK IN PATR-BANK
C     XV   = X-POSITION OF VERTEX
C     YV   = Y-POSITION OF VERTEX
C
C     OUTPUT:
C     RES(1)   : DR = DISTANCE (XV,YV)-TRACK(IPTR)
C                           +VE IF OUTSIDE CIRCLE
C                           -VE IF INSIDE CIRCLE
C     RES(2)   : X-POSITION OF CLOSEST POINT
C     RES(3)   : Y-POSITION OF CLOSEST POINT
C     RES(4)   : X-DIRECTION AT CLOSEST POINT (ONLY IN R-FI)
C     RES(5)   : Y-DIRECTION AT CLOSEST POINT (ONLY IN R-FI)
C     RES(6)   : )
C     RES(7)   : )  DIRECTION COSINES IN SPACE
C     RES(8)   : )
C     RES(9)   : PT, TRANSVERSE MOMENTUM
C     RES(10)  : P , MOMENTUM
C
C
      IMPLICIT INTEGER*2 (H)
C
      DIMENSION RES(10)
C
      DATA VLIGHT /.0299792458E-3/
C
#include "cdata.for"
C
#include "cgeo1.for"
C
      REAL*8 XC,YC,RC
C
C2001 FORMAT(' DRTRCK     :',10F10.5)
C2002 FORMAT(' DRTRCK(PAR):',11F10.5)
C2003 FORMAT(' DRTRCK(CIR):',10E13.5)
C
C                                       CURVATURE + RADIUS OF TRACK
C                                       1. POINT + DIRECTION
      X1    = ADATA(IPTR+ 5)
      Y1    = ADATA(IPTR+ 6)
      DRI   = SQRT(ADATA(IPTR+8)**2 + ADATA(IPTR+9)**2)
      DX1   = ADATA(IPTR+8) / DRI
      DY1   = ADATA(IPTR+9) / DRI
      CRV   = ADATA(IPTR+25)
      TGTH = ADATA(IPTR+30)
      CSTH  = 1. / SQRT(TGTH**2 + 1.)
      SNTH  = CSTH * TGTH
C     PRINT 2001, X1,Y1,DX1,DY1,CRV,XV,YV
C
C                                       MOMENTUM > 1.7 GEV ?
      IF(ABS(CRV).LT..00010) GOTO 200
C
C                                       CIRCLE EXTRAPOLATION
         RC    = 1. / CRV
         XC    = DY1*RC + X1
         YC    =-DX1*RC + Y1
         RC    = DABS(RC)
         RES(1)= ( ( (XV-XC)**2+(YV-YC)**2 ) / RC - RC) *.5
C     PRINT 2003, CRV,XC,YC,RC,DR

         DX      = XV - XC
         DY      = YV - YC
         ANORM   = SQRT(DX**2 + DY**2)
         DX      = DX / ANORM
         DY      = DY / ANORM
         RES( 2) =-DX * RES(1) + XV
         RES( 3) =-DY * RES(1) + YV
         RES( 4) = DY * SIGN(1.,CRV)
         RES( 5) =-DX * SIGN(1.,CRV)
C     PRINT 2003, DX,DY,(RES(I1),I1=1,5)
         GOTO 300
C
C                                       PARABOLA EXTRAPOLATION
  200 CONTINUE
         DXX     = (XV-X1)*DX1 + (YV-Y1)*DY1
         DRP     =-DXX**2 * CRV * .5
         DRO     = (YV-Y1)*DX1 - (XV-X1)*DY1
         RES( 1) = (DRO - DRP) * SIGN(1.,CRV)
C     PRINT 2002, CRV,DRP,DRO,DR,DXX
C
         RES( 2) = DXX * DX1 - DRP*DY1 + X1
         RES( 3) = DXX * DY1 + DRP*DX1 + Y1
         TG1     =-DXX*CRV
         CS1     = 1. / SQRT(1. + TG1**2)
         SN1     = TG1 * CS1
         RES( 4) = DX1*CS1 - DY1*SN1
         RES( 5) = DX1*SN1 + DY1*CS1
C     PRINT 2002, TG1,SN1,CS1,(RES(I1),I1=1,5)
C
C
  300 CONTINUE
         RES( 6) = RES(4) * CSTH
         RES( 7) = RES(5) * CSTH
         RES( 8) = SNTH
C
C                                     MOMENTUM CALCULATION
         IF(ABS(CRV).LT.1.E-8) CRV = SIGN(1.E-8,CRV)
         RES( 9) = ABS(VLIGHT*BKGAUS/CRV)
         RES(10) = RES(9) / CSTH
C     PRINT 2002, (RES(I1),I1=1,10)
C
         RETURN
C
      END
