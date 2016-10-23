C   18/01/82            MEMBER NAME  MCTRCO   (JADEGS)      FORTRAN
C   15/12/81 112161224  MEMBER NAME  MCTRCO   (JADESR)      FORTRAN
      SUBROUTINE MCTRCO(IPTR1,IPPATR,JPTR2,CHSQTR)
C
C     FIND TRACK IN PATR(10) CORRESP. TO MC-TRACK (IPTR1) IN PATR(12)
C     IPTR1  : POINTER TO TRACK IN PATR(12)-BANK
C     IPPATR : POINTER TO PATR(10)-BANK
C     JPTR2  : POINTER TO FOUND TRACK IN PATR(10)-BANK
C              = 0 : NO TRACK FOUND
C     CHSQTR : ARRAY WITH RESULTS
C
C     TRACK IS SEARCHED FOR BY COMPARING POSITION + DIRECTION OF
C     FIRST + LAST POINT OF TRACKS
C     CHI**2 = 1/4 * ( (DXY/.40)**2 + (DFI/.010)**2    : FIRST POINT
C                    + (DXY/.40)**2 + (DFI/.010)**2 )  : LAST  POINT
C     CHSQTR(1)  : CHI**2
C     CHSQTR(2)  : DXY(FIRST POINT ON TRACK)
C     CHSQTR(3)  : DXY(LAST  POINT ON TRACK)
C     CHSQTR(4)  : DFI(FIRST POINT ON TRACK)
C     CHSQTR(5)  : DFI(LAST  POINT ON TRACK)
C     P. STEFFEN  16.12.81
C
C
      IMPLICIT INTEGER*2 (H)
C
      DIMENSION CHSQTR(5)
C
#include "cdata.for"
C
      DATA LBPRFT /1/
C
C2001 FORMAT(' MCTRCO     :',I6,2F8.1,2F8.3,F10.6,3F10.1)
C2002 FORMAT(' MCTRCO     :',2I3,I6,6F6.1,3F8.5,2F8.1)
C2000 FORMAT(1H0,'EVENT =',3I6,,F8.1)
C
C                                       INITIALIZATION
      DATA LBINIT /0/
      IF(LBINIT.NE.0) GOTO 100
        LBINIT = 1
        NPR = 0
        IQHEAD = IBLN('HEAD')
C
  100 CONTINUE
      NPR = NPR + 1
C
      CHSQTR(1) = 9999999.
      JPTR2  = 0
C
C                                       CURVATURE + RADIUS OF TRACK
C                                       INNER POINT + DIRECTION
C     CALL CHECK( 1)
      X1    = ADATA(IPTR1+ 5)
      Y1    = ADATA(IPTR1+ 6)
      DX1   = ADATA(IPTR1+ 8)
      DY1   = ADATA(IPTR1+ 9)
      ANR1  = SQRT(DX1**2 + DY1**2)
      DX1   = DX1 / ANR1
      DY1   = DY1 / ANR1
C     CALL CHECK( 2)
      CRV1  = ADATA(IPTR1+27)
      IF(ABS(CRV1).LT.1.E-6) CRV1 = SIGN(1.E-6,CRV1)
C                                       CIRCLE EXTRAPOLATION
      RCI   = 1. / CRV1
C     CALL CHECK( 3)
      XCI   = DY1*RCI + X1
      YCI   =-DX1*RCI + Y1
      RCI   = ABS(RCI)
C     IF(NPR.LE.5) PRINT 2001, IPTR1,X1,Y1,DX1,DY1,CRV1,RCI,XCI,YCI
C
C                                       CURVATURE + RADIUS OF TRACK
C                                       OUTER POINT + DIRECTION
      X1    = ADATA(IPTR1+12)
      Y1    = ADATA(IPTR1+13)
      DX1   = ADATA(IPTR1+15)
      DY1   = ADATA(IPTR1+16)
      ANR1  = SQRT(DX1**2 + DY1**2)
C
C
C                                       CHECK IF DIR. AT OUTER POINT
      IF(ANR1.LE..01) RETURN
C
      DX1   = DX1 / ANR1
      DY1   = DY1 / ANR1
C     CALL CHECK( 4)
      CRV1  = ADATA(IPTR1+28)
      IF(ABS(CRV1).LT.1.E-6) CRV1 = SIGN(1.E-6,CRV1)
C                                       CIRCLE EXTRAPOLATION
C     CALL CHECK( 5)
      RCO   = 1. / CRV1
      XCO   = DY1*RCO + X1
      YCO   =-DX1*RCO + Y1
      RCO   = ABS(RCO)
C     IF(NPR.LE.5) PRINT 2001, IPTR1,X1,Y1,DX1,DY1,CRV1,RCO,XCO,YCO
C
C                                       POINTER TO TRACKS IN PATR(10)
      IPTR0  = IDATA(IPPATR+1) + IPPATR
      LDTR   = IDATA(IPPATR+3)
      NTR    = IDATA(IPPATR+2)
C
      IF(NTR.LE.0) RETURN
C
C                                       LOOP OVER ALL TRACKS
C     IF(RC1.LT.1. .OR. RCO.LT.1.) CALL PRPATR
      CHISQ     = 9999999.
      IPTR2 = IPTR0
      DO 200 ITR2=1,NTR
C                                       DIST. OF 1. + LAST POINTCS CSTH200010600
        X2I   = ADATA(IPTR2+ 5)
        Y2I   = ADATA(IPTR2+ 6)
C     CALL CHECK( 6)
        DRI   = ( ( (X2I-XCI)**2+(Y2I-YCI)**2 ) / RCI - RCI) *.5
C     CALL CHECK( 7)
        X2O   = ADATA(IPTR2+12)
        Y2O   = ADATA(IPTR2+13)
        DRO   = ( ( (X2O-XCO)**2+(Y2O-YCO)**2 ) / RCO - RCO) *.5
C     CALL CHECK( 8)
C                                      CHECK IF SAME HEMISPHERE
        DX2I  = ADATA(IPTR2+ 8)
        DY2I  = ADATA(IPTR2+ 9)
        DALF1 = DX1*DX2I + DY1*DY2I
        IF(DALF1 .LT. 0.) GOTO 198
C                                      ANGLE AT INNER POINT
        DX    = X2I - XCI
        DY    = Y2I - YCI
        RRI   = SQRT((DX**2+DY**2) * (DX2I**2+DY2I**2))
        DALFI = ABS(DX*DX2I + DY*DY2I) / RRI
C       IF(RRI.LT.1.) CALL PRPATR
C     CALL CHECK( 9)
C                                      ANGLE AT OUTER POINT
        DX2O  = ADATA(IPTR2+15)
        DY2O  = ADATA(IPTR2+16)
        DX    = X2O - XCO
        DY    = Y2O - YCO
        RRO   = SQRT((DX**2+DY**2) * (DX2O**2+DY2O**2))
        DALFO = ABS(DX*DX2O + DY*DY2O) / RRO
C       IF(RRO.LT.1.) CALL PRPATR
C     CALL CHECK(10)
C                                      CALCULATE CHI**2
        CH    =(DRI/.40)**2+(DRO/.40)**2+(DALFI/.010)**2+(DALFO/.010)**200013800
C     IF(NPR.LE.5) PRINT 2002, ITR2,NAGR,IPTR2,
C    ,     X2I,Y2I,X2O,Y2O,DRI,DRO,DALFI,DALFO,DALF1,CH,CHISQ
        IF(CH.GT.CHISQ) GOTO 198
C                                      SAVE VALUES + TRACK
           CHISQ  = CH
           DRIS   = DRI
           DALFIS = DALFI
           DROS   = DRO
           DALFOS = DALFO
           IPTR2S = IPTR2
C
  198   CONTINUE
C     CALL CHECK(11)
        IPTR2 = IPTR2 + LDTR
  200 CONTINUE
C
      CHISQ  = CHISQ / 4.
      CHSQTR(1) = CHISQ
      CHSQTR(2) = DRIS
      CHSQTR(3) = DROS
      CHSQTR(4) = DALFIS
      CHSQTR(5) = DALFOS
      IF(CHISQ.LT.9999.) JPTR2 = IPTR2S
C
  300 CONTINUE
        IPHEAD = IDATA(IQHEAD)*2
        HRUN   = HDATA(IPHEAD+10)
        HEV    = HDATA(IPHEAD+11)
C       IF(CHISQ.GT.9.) PRINT 2000, HRUN,HEV,JPTR2S,CHISQ
C
      RETURN
C
      END
