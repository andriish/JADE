C   05/05/81 603101205  MEMBER NAME  ZRECAL0  (S)           FORTRAN
      SUBROUTINE ZRECAL(IJETU,IJETC)
C*820817*DITTMANN*******************  LAST CHANGE: 18.2.86 L.BECKER *
C*                                                                  *
C*               Z   C A L I B R A T I O N                          *
C*                                                                  *
C*       DETERMINE HIT STRING PARAMETERS DT, DL, DW (THIS REQUIRES  *
C*       AN EXISTING PATR BANK),                                    *
C*       CORRECT RAW AMPLITUDES FOR PEDESTALS AND GAINS,            *
C*       CORRECT UNDERSHOOT FOR HITS AFTER FIRST HIT,               *
C*       REMOVE Z-T CORRELATION (DISTORTION OF AMPLITUDES IF TRACK  *
C*       NEAR WIRE PLANE),                                          *
C*       CORRECT FOR STRING Z-DISTORTION AND EFFECTIVE WIRE LENGTH. *
C********************************************************************
#include "cdata.for"
      INTEGER*2 HDATA
      COMMON /CZSCAL/ IPVERS,ZAL,RESFAC,SECH(5)
C----*----------------TCORR(4,96),XTALK(2,96),PAR(7,1536)-------------
C-----------
      COMMON/CALIBR/JPOINT(1)
      DIMENSION ACALIB(1)
      EQUIVALENCE (JPOINT(1),ACALIB(1))
C-----------
      COMMON /CWORK/ NHIT,IZB(60),IJC(60),IJH(60),NW(60),LH(60),
     +                LRFL(60),ALU(60),ARU(60),X(60),Y(60),Z(60),S(60),
     +                G(60),DT(60),DL(60),DW(60),FCORR(60)
      DIMENSION IHIT(16),ALT(6),ART(6),TT(6)
C-----------
      IPN = JPOINT(11)
C-----------
      IJETU2 = IJETU*2 + 2
      IJETC2 = IJETC*2 + 2
C                                   SWITCH BETWEEN FORMATS DL8/DL300
      IBNKDS = HDATA(IJETU2-1)
      TSCALE = 1.
      IF( MOD(IBNKDS,10) .GT. 1 ) TSCALE = 64.
C
C        LOOP OVER ALL HITS OF ONE TRACK
C        AND DETERMINE HIT STRING PARAMETERS
      NDT = 0
      DO 9 I=1,NHIT
      JC = IJC(I)
      JU = JC - IJETC2 + IJETU2
      NC = NW(I) / 16
C        FIND HIT STRINGS
      IF(NDT.EQ.0) GOTO 4
      IF(NC.NE.NCL) GOTO 6
      IF(LRFL(I).NE.LRFLL) GOTO 6
      IF(NDT.EQ.16) GOTO 5
    4 NDT = NDT + 1
      IWL = LAND(NW(I),15) + 1
      TL = HDATA(JU+3)/TSCALE
      IHIT(NDT) = I
      IF(NDT.NE.1) GOTO 5
      IWF = IWL
      TF = TL
      NCL = NC
      LRFLL = LRFL(I)
    5 IF(I.NE.NHIT) GOTO 9
C        HIT STRING COMPLETED
    6 DTI = 0.
      DWI = IWL - IWF
      IF(DWI.GT.0.) DTI=(TL-TF)/DWI
      DLI = (IWF+IWL) / 2.
      DO 7 J=1,NDT
      K = IHIT(J)
      IF (K.LE.0 .OR. K.GT.60) GOTO 7
      DT(K) = DTI
      DW(K) = DWI
    7 DL(K) = DLI
      NDT = 0
      IF(I.NE.NHIT) GOTO 4
    9 CONTINUE
C        LOOP OVER ALL HITS OF ONE TRACK
C        AND CALIBRATE AMPLITUDES
      DO 39 I=1,NHIT
      JC = IJC(I)
      JU = JC - IJETC2 + IJETU2
      IW = NW(I) + 1
      NC = NW(I)/16 + 1
      NL = LAND(NW(I),15) + 1
      NH = LH(I)
      IF (NH.LE.0 .OR. NH.GT.60) GOTO 39
      T = HDATA(JU+3)/TSCALE
      IF(T.GT.255.) GOTO 37
C        PEDESTAL AND  GAIN
      J = 2
      IF(NH.GT.1) J=5
C-----AR = HDATA(JU+1)*PAR(J,IW) - PAR(J+2,IW)
      AR = HDATA(JU+1)*ACALIB(IPN+577+J+(IW-1)*7)   -
     +                 ACALIB(IPN+579+J+(IW-1)*7)
C-----AL = HDATA(JU+2)           - PAR(J+1,IW)
      AL = HDATA(JU+2)           - ACALIB(IPN+578+J+(IW-1)*7)
C-----EFFL = PAR(1,IW)
      EFFL = ACALIB(IPN+578+(IW-1)*7)
C
      ALU(I) = AL
      ARU(I) = AR
      IF(AL.LE.0. .OR. AR.LE.0.) GOTO 36
C        MULTIPLE HIT UNDERSHOOT
      IF(NH.EQ.1) GOTO 30
      MH = NH - 1
      JUPH = JU + MH*4
      DO 29 J=1,MH
      K = 2
      IF(J.NE.1) K=5
      TP = HDATA(JUPH+3)/TSCALE
      IF(TP.GT.255.) GOTO 37
C-----ARP = HDATA(JUPH+1)*PAR(K,IW) - PAR(K+2,IW)
      ARP = HDATA(JUPH+1)*ACALIB(IPN+577+K+(IW-1)*7)  -
     +                    ACALIB(IPN+579+K+(IW-1)*7)
C-----ALP = HDATA(JUPH+2)           - PAR(K+1,IW)
      ALP = HDATA(JUPH+2)           - ACALIB(IPN+578+K+(IW-1)*7)
      IF(J.EQ.1) GOTO 25
      KH = J - 1
      DO 24 K=1,KH
      TDIF = TP - TT(K) - SECH(1)
      SECHC = SECH(2)*EXP(-SECH(3)*TDIF) - SECH(4)*EXP(-SECH(5)*TDIF)
      ARP = ARP + ART(K)*SECHC
   24 ALP = ALP + ALT(K)*SECHC
   25 IF(J.EQ.MH) GOTO 26
      ART(J) = ARP
      ALT(J) = ALP
      TT(J) = TP
   26 TDIF = T - TP - SECH(1)
      SECHC = SECH(2)*EXP(-SECH(3)*TDIF) - SECH(4)*EXP(-SECH(5)*TDIF)
      AR = AR + ARP*SECHC
      AL = AL + ALP*SECHC
   29 JUPH = JUPH - 4
C        Z DISTORTION
   30 ZD = 0.
      IF(DW(I).LT.6.) GOTO 32
      IF(ABS(DT(I)).GT.4.) GOTO 32
C-----ZD = XTALK(1,NC) * (NL-DL(I))*DT(I) * EXP(-XTALK(2,NC)*DT(I)**2)
      ZD = ACALIB(IPN+386+(NC-1)*2)  * (NL-DL(I))*DT(I) *
     +                        EXP(-ACALIB(IPN+387+(NC-1)*2)*DT(I)**2)
C        Z-T CORRELATION
C--32 TDIF = TCORR(3,NC) - T
   32 TDIF = ACALIB(IPN+4+(NC-1)*4)  -  T
      IF(TDIF.LT.0.) GOTO 33
C-----ZTCOR = 0.5*TDIF**2*(AL*TCORR(1,NC) - AR*TCORR(2,NC))
      ZTCOR = 0.5*TDIF**2*(AL*ACALIB(IPN+2+(NC-1)*4) -
     +                                      AR*ACALIB(IPN+3+(NC-1)*4))
      AL = AL + ZTCOR
      AR = AR - ZTCOR
C        EFFECTIVE WIRE LENGTH
   33 FCORR(I) = EFFL/(1.+ZD)
      CORR = (FCORR(I)-1.) * (AL-AR)/2.
      AL = AL + CORR
      AR = AR - CORR
C
   36 IF(AR.LT.0.) AR=0.
      IF(AL.LT.0.) AL=0.
      GOTO 38
   37 AR = 0.
      AL = 0.
   38 HDATA(JC+1) = AR*8. + .49
      HDATA(JC+2) = AL*8. + .49
   39 CONTINUE
C
      RETURN
      END
