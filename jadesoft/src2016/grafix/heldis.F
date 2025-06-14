C   25/06/79 704211053  MEMBER NAME  HELDIS   (S)           FORTRAN
      SUBROUTINE HELDIS(ITR,INDEX,XB,YB,ZB,XE,YE,ZE,CURV)
C---
C---     DRAWS A CURVE BETWEEN ZB,XB (YB) AND ZE,XE (YE)
C---     THE CURVE IS A PROJECTION OF Z-FI(HELIX) FIT, TYPE 2 IN PATR
C---                                         Z = B*FI + A
C---     ITR IS CURRENT TRACK NUMBER
C---
C---     J.OLSSON 25.6.79          LAST CHANGE 01.07.79
C---
C---   THOUROUGHLY MODIFIED BY J.SPITZER 4/87
C---
      IMPLICIT INTEGER*2 (H)
#include "cgraph.for"
#include "cgeo1.for"
      COMMON /CJTRIG/ PI,TWOPI,PIHALF,PI3HALF
      DATA DUM1 /0./
C---
      RAD = 1./ABS(CURV)
      SGN = SIGN(1.,CURV)
C=========================
      UX=XE-XB
      UY=YE-YB
      UU=SQRT(UX**2+UY**2+.0000001)
      IF(RAD.LT..51*UU) RAD=.51*UU
      VV=SQRT((RAD-.5*UU)*(RAD+.5*UU))
      XMIT=.5*(XB+XE)+SGN*VV*UY/UU
      YMIT=.5*(YB+YE)-SGN*VV*UX/UU
      RMIT = SQRT(XMIT**2+YMIT**2+.0000001)
      PHIM = ATAN2(YMIT/RMIT,XMIT/RMIT)
      B=(ZE-ZB)/(2.*SIN(.5*UU/RAD))
      A=ZB
C=========================
C                    START POINT
      XX = XB
      YY = YB
      RR = SQRT(XX*XX + YY*YY +.0000001)
      RCON1 = RMIT*RMIT + RAD*RAD
      RCON2 = 2.*RMIT*RAD
      FIFRST = (RCON1 - RR*RR)/RCON2
      IF(ABS(FIFRST).GT..999999) FIFRST=SIGN(.999999,FIFRST)
      FIFRST = ARCOS(FIFRST)
      IF(FIFRST.LT.0.) FIFRST = FIFRST + TWOPI
      IF(.NOT.DSPDTL(19)) GO TO 11
      FII = PHIM - PI
      XX = XMIT + RAD*COS(FII)
      YY = YMIT + RAD*SIN(FII)
      RR = SQRT(XX*XX + YY*YY + .00000001)
11    ZETP = XX
      IF(INDEX.GT.7) ZETP = YY
      IF(.NOT.DSPDTL(9)) GO TO 2
      ZETP = RR
      IF(YY.LT.0..AND.INDEX.GT.7) ZETP = - ZETP
      IF(XX.LT.0..AND.INDEX.LT.8) ZETP = - ZETP
2     IF(.NOT.DSPDTL(19)) ZETR = ZETP
      ZET = ZB
      IF(DSPDTL(19)) ZET = -B*FIFRST + A
      CALL MOVEA(ZET,ZETP)
C      IF(DSPDTL(30)) WRITE(JUSCRN,333) XB,YB,ZB,XX,YY,RR,ZETP,ZET,A
C333   FORMAT(' XYZB XYR*2 ZET-P ',9E11.3)
C                    END POINT
      XX = XE
      YY = YE
      RR = SQRT(XX*XX + YY*YY +.0000001)
      FIEND = (RCON1 - RR*RR)/RCON2
      IF(ABS(FIEND).GT..999999) FIEND=SIGN(.999999,FIEND)
      FIEND = ARCOS(FIEND)
      IF(FIEND.LT.0.) FIEND = FIEND + TWOPI
      FIDIFF = ABS(FIEND-FIFRST)
      IF(DSPDTL(19)) FIDIFF = FIEND
      FIDEL = - SGN*FIDIFF/25.
      FII = PI + PHIM - SGN*FIFRST
      IF(DSPDTL(19)) FII = PI + PHIM
      FIB = FII
      ZDEL = B*ABS(FIDEL)
C      IF(DSPDTL(30)) WRITE(JUSCRN,334) XE,YE,ZE,XX,YY,RR,ZDEL,B,ZLGPL
C334   FORMAT(' XYZE XYR*2 ZDEL  ',9E11.3)
C      IF(DSPDTL(30)) WRITE(JUSCRN,123)
C     $ ITR,PHIM,FIFRST,FIEND,FIDIFF,FIDEL,FII,FIB
C123   FORMAT(' 123 PHIM FIRST ETC ',I4,7E11.4)
C      IF(DSPDTL(30)) WRITE(JUSCRN,124)
C     $ ITR,CURV,RAD,RMIN,RMIT,XMIT,YMIT,SGN
C124   FORMAT(' 124 CURV RAD ETC ',I4,6E11.4,F5.1)
      DO 34 IF = 1,25
        FII = FII + FIDEL
        XX = XMIT + RAD*COS(FII)
        YY = YMIT + RAD*SIN(FII)
        RR = SQRT(XX*XX + YY*YY)
        ZET = ZET + ZDEL
        ZETP = XX
        IF(INDEX.GT.7) ZETP = YY
        IF(.NOT.DSPDTL(9)) GO TO 32
        ZETP = RR
        IF(YY.LT.0..AND.INDEX.GT.7) ZETP = - ZETP
        IF(XX.LT.0..AND.INDEX.LT.8) ZETP = - ZETP
32    IF(DSPDTL(19).AND.IF.EQ.1) ZETR = ZETP
C     IF(ZETR*ZETP.LT.0.) ZETP = - ZETP
C      IF(DSPDTL(30)) WRITE(JUSCRN,2345) IF,FII,XX,YY,RR,ZET,ZETP
C2345  FORMAT(' LOOP ',I2,F8.4,5E12.4)
C     IF(DSPDTL(30)) CALL PROMPT
34    CALL DRAWA(ZET,ZETP)
      CALL TRNUMB(ITR,0,-ZET,ZETP,DUM1)
      IF(.NOT.DSPDTL(23)) RETURN
C PROLONGATION TO TOF OR LEAD GLASS LIMIT
      CALL MOVEA(ZET,ZETP)
      RTS = RLG
      IF(LASTVW.EQ.4.OR.LASTVW.EQ.8) RTS = RTOF
      Z2 = ZET
      R2 = RR
      NNN = 0
      IFLGX = 0
      FAT = 1.
551   FII = FII + FAT*FIDEL
      NNN = NNN + 1
      IF(NNN.GT.200) RETURN
        XX = XMIT + RAD*COS(FII)
        YY = YMIT + RAD*SIN(FII)
        RR = SQRT(XX*XX + YY*YY)
        IF(RR.LT.RITNK) RETURN
        ZET = ZET + ZDEL*FAT
        ZETP = XX
        IF(INDEX.GT.7) ZETP = YY
        IF(.NOT.DSPDTL(9)) GO TO 549
        ZETP = RR
        IF(YY.LT.0..AND.INDEX.GT.7) ZETP = - ZETP
        IF(XX.LT.0..AND.INDEX.LT.8) ZETP = - ZETP
C     IF(ZETR*ZETP.LT.0.) ZETP = - ZETP
549   IF(IFLGX.NE.0) GO TO 553
CHECK LIMITS
      IF(ABS(ZET).LT.ZENDPL) GO TO 554
      IFLGX = 1
      FAT = - ABS((ABS(ZET) - ZENDPL)/(ZET - Z2))
      GO TO 551
554   IF(RR.LT.RTS) GO TO 553
      IFLGX = 2
      FAT = - (RR-RTS)/(RR-R2)
      GO TO 551
553   R2 = RR
      Z2 = ZET
C      IF(DSPDTL(30)) WRITE(JUSCRN,2346) NNN,FII,XX,YY,RR,ZET,ZETP
C2346  FORMAT(' NNN ',I3,F8.4,5E12.4)
C      IF(DSPDTL(30)) CALL PROMPT
      CALL DRAWA(ZET,ZETP)
      IF(IFLGX.EQ.0) GO TO 551
      RETURN
      END
