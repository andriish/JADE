C   01/11/84 507081529  MEMBER NAME  RZDRAW   (ZS)          FORTRAN
C
C-----------------------------------------------------------------------
      SUBROUTINE RZDRAW(NWIR, XZ, YZ, XZ1, YZ1, XZ2, YZ2, ZZ1, ZZ2, LAB)
C-----------------------------------------------------------------------
C
C    AUTHOR:   J. OLSSON        ?     :  DRAW HITS OF RZ CHAMBERS
C
C  LAST MOD:   J. HAGEMANN   09/10/84 :  NOW OWN MEMBER (FROM EVDISP)
C
C        DRAW THE HITS IN RZ CHAMBERS; LAB GIVES THE MODE
C
C-----------------------------------------------------------------------
C
      IMPLICIT INTEGER*2 (H)
C
#include "cgraph.for"
C
      COMMON /CZGEO/ RZCHI,RZCHA,NZRPSI,NZZ,Z1ZCH,Z2ZCH,ZCHA,ZCHB,ZCHSS,
     +               ZCHDL,ZCHDLL,DLZZ,DLZPHI,DLZW1,DLZW2
      COMMON /CZKON/ ZCVDR,ZCXCH,ZCTZER,ZCAPED,XL1,XL2
C
      COMMON /CJTRIG/ PI,TWOPI,PIHALF,PI3HAF
C
      DATA SCRUNI/8.0/, ICAL/0/
C
C-----------------  C O D E  -------------------------------------------
C
      ICAL = ICAL + 1
      IF(ICAL.NE.1) GO TO 4040
      ADDERZ = 24.*DLZPHI
      ADDRAD = ADDERZ/TWOPI
4040  IF(LAB.NE.1) GO TO 111
C                               RFI DISPLAYS
      IF(ZZ1.LT.0.) GO TO 1010
C  PLUS Z SIGN
      RAD=SQRT(XZ**2+YZ**2+1.)
      RAD=SCRUNI/RAD
      X00 = RAD*XZ
      Y00 = RAD*YZ
      XX=-XZ-X00
      YY=YZ-Y00
      CALL MOVEA(XX,YY)
      XX=-XZ+X00
      YY=YZ+Y00
      CALL DRAWA(XX,YY)
      XX=-XZ-Y00
      YY=YZ+X00
      CALL MOVEA(XX,YY)
      XX=-XZ+Y00
      YY=YZ-X00
      CALL DRAWA(XX,YY)
      CALL DRAMOV(-XZ1,YZ1,-XZ2,YZ2,0)
      RETURN
 1010 CONTINUE
C  MINUS Z SIGN
      RAD=SQRT(XZ**2+YZ**2+1.)
      RAD=1.41421356*RAD
      RAD=SCRUNI/RAD
      X00 = RAD*(-XZ+YZ)
      Y00 = RAD*(-XZ-YZ)
      XX=-XZ-X00
      YY=YZ-Y00
      CALL MOVEA(XX,YY)
      XX=-XZ+X00
      YY=YZ+Y00
      CALL DRAWA(XX,YY)
      XX=-XZ-Y00
      YY=YZ+X00
      CALL MOVEA(XX,YY)
      XX=-XZ+Y00
      YY=YZ-X00
      CALL DRAWA(XX,YY)
      CALL DRAMOV(-XZ1,YZ1,-XZ2,YZ2,0)
      RETURN
111   CONTINUE
      IF(LAB.NE.2) GO TO 222
C                        ZX AND ZY DISPLAYS
      IPAS = 1
      XP = ZZ1
      RP = SQRT(XZ**2+YZ**2)
      RP1 = SQRT(XZ1**2+YZ1**2)
      RP2 = SQRT(XZ2**2+YZ2**2)
      FLAGZ = 1.
      IF(LASTVW.LT.8.AND.XZ.LT.0.) FLAGZ = -1.
      IF(LASTVW.GT.7.AND.YZ.LT.0.) FLAGZ = -1.
      YP = RP*FLAGZ
      YP1 = RP1*FLAGZ
      YP2 = RP2*FLAGZ
8481  XX=XP-SCRUNI
      YY=YP
      CALL MOVEA(XX,YY)
      XX=XP+SCRUNI
      CALL DRAWA(XX,YY)
C     XX=XP
C     YY=YP-SCRUNI
C     CALL MOVEA(XX,YY)
C     YY=YP+SCRUNI
C     CALL DRAWA(XX,YY)
      CALL DRAMOV(XP,YP1,XP,YP2,0)
      IF(IPAS.EQ.2) RETURN
      IPAS = 2
      XP = ZZ2
      GO TO 8481
222   CONTINUE
      IF(LAB.NE.3) GO TO 333
C                        ROLLED OUT RZ DISPLAY
      ADY = 1500. + Z2ZCH
      IPAS = 1
      YP = ADY + ZZ1
      FIP = ATAN2(YZ,XZ)
      FIP1 = ATAN2(YZ1,XZ1)
      FIP2 = ATAN2(YZ2,XZ2)
      IF(FIP.LT.0.) FIP = FIP + TWOPI
      IF(FIP1.LT.0.) FIP1 = FIP1 + TWOPI
      IF(FIP2.LT.0.) FIP2 = FIP2 + TWOPI
      XP = ADDRAD*FIP
      XP1 = ADDRAD*FIP1
      XP2 = ADDRAD*FIP2
223   XX=XP-SCRUNI
      YY=YP
      IF(MOD(NWIR,2).EQ.1)  YY=YP-SCRUNI
      CALL MOVEA(XX,YY)
      XX=XP+SCRUNI
      IF(MOD(NWIR,2).EQ.1)  YY=YP+SCRUNI
      CALL DRAWA(XX,YY)
      XX=XP
      IF(MOD(NWIR,2).EQ.1)  XX=XP+SCRUNI
      YY=YP-SCRUNI
      CALL MOVEA(XX,YY)
      IF(MOD(NWIR,2).EQ.1)  XX=XP-SCRUNI
      YY=YP+SCRUNI
      CALL DRAWA(XX,YY)
      IF(XP2.GT..95*ADDERZ.AND.XP1.LT..05*ADDERZ) XP2=XP2-ADDERZ
      IF(XP1.GT..95*ADDERZ.AND.XP2.LT..05*ADDERZ) XP1=XP1-ADDERZ
      YY=YP
      CALL DRAMOV(XP1,YY,XP2,YY,0)
      IF(IPAS.EQ.2) RETURN
      IPAS = 2
      YP = ADY + ZZ2
      GO TO 223
333   CONTINUE
      RETURN
      END
