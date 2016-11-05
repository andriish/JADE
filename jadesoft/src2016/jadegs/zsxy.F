C   24/09/81 111210224  MEMBER NAME  ZSXY     (HELIX)       FORTRAN
      SUBROUTINE ZSXY(NH1,LRFLAG,XJET1,YJET1,XJET2,YJET2)
C*811119*DITTMANN*************************************************
C*                                                               *
C*       X - Y   C O O R D I N A T E   O F   O N E   H I T       *
C*                                                               *
C*       THIS ROUTINE CONTAINS THE ESSENTIALS OF G. PEARCE'S OLD *
C*       JETXYZ. THE ACCURACY NEEDED FOR X-Y IS ONLY 5 MM,       *
C*       THEREFORE SMALL CORRECTIONS ARE OMITTED.                *
C*       COMMONS /CJDRCH/ AND /CDSMAX/ ARE NEEDED PARTLY.        *
C*       IF THIS SUBROUTINE SHOULD RUN IN AN SUPERVISOR INDEPEN- *
C*       DENT PROGRAM AND THOSE COMMONS ARE NOT FILLED, THE      *
C*       ROUTINE ZSSPEC HAS TO BE CALLED, WHICH FILLS THE        *
C*       NEEDED PARTS OF THE COMMONS.                            *
C*****************************************************************
          IMPLICITINTEGER*2(H)
          LOGICAL TBIT
#include "cdata.for"
#include "cjdrch.for"
#include "cdsmax.for"
C         ------------------
C         DATA AND CONSTANTS
C         ------------------
          INWIRE=HDATA(NH1)
          INWIRE=ISHFTR(INWIRE,3)
          LAYER=LAND(INWIRE,15)
          INCELL=LAND(ISHFTR(INWIRE,4),127)+ 1
          INRING=1
          IF(INCELL.GT.24)INRING=2
          IF(INCELL.GT.48)INRING=3
          DR0=RINCR(INRING)
          IW1=INCELL-24*(INRING-1)
          IF(INRING.EQ.3)GOTO 10
          DXWR=DIRWR1(IW1,1)
          DYWR=DIRWR1(IW1,2)
          GOTO20
 10       DXWR=DIRWR3(IW1,1)
          DYWR=DIRWR3(IW1,2)
C         -------------------------
C         COMPUTE (X,Y,R) OF LAYER
C         INCLUDING WIRE STAGGERING
C         -------------------------
 20       R1=FSENSW(INRING)+LAYER*DR0
          IW1=-1
          IF(TBIT(LAYER,31))IW1=+1
          X1=R1*DXWR+IW1*SWDEPL*DYWR
          Y1=R1*DYWR-IW1*SWDEPL*DXWR
C         -----------------------
C         SET LEFT/RIGHT SOLUTION
C         -----------------------
          NEXTLR=0
          LR1M1=ISIGN(1,LRFLAG)
 30       LR12=1
          IF(LR1M1.GT.0)LR12=2
C         --------------------------------
C         CONVERT DRIFT TIME INTO DISTANCE
C         --------------------------------
          DRIFT=HDATA(NH1+3)*DRIVEL(INCELL,LR12)
C         -------------------------
C         ADD DRIFT TIME TO (X,Y,R)
C         -------------------------
          TRLORX=TRMATS(INCELL,LR12)
          TRLORY=TRMATC(INCELL,LR12)
          XDRFT=LR1M1*TRLORX*DRIFT
          YDRFT=LR1M1*TRLORY*DRIFT
          IF(NEXTLR.EQ.1)GOTO70
          XJET1=X1+XDRFT
          YJET1=Y1+YDRFT
          NEXTLR=1
          LR1M1=-LR1M1
          GOTO30
 70       XJET2=X1+XDRFT
          YJET2=Y1+YDRFT
          RETURN
          END
