      SUBROUTINE CIRCCK(MTREL0,IATREL,RSLT)
      IMPLICIT INTEGER*2 (H)
      LOGICAL TBIT
***PMF      DIMENSION IATREL(1),RSLT(6),HATREL(24)
      DIMENSION IATREL(1),RSLT(8),HATREL(24)
#include "cpatlm.for"
#include "cworkpr.for"
#include "cworkeq.for"
      EQUIVALENCE
     ,           (ICELL ,IDWRK(1)),(NHIT  ,IDWRK(2)),(IRING ,IDWRK(3))
     ,         , (IERRCD,IDWRK(4)),(NTRKEL,IDWRK(5))
      DIMENSION XO(20),YO(20), XT(20),YT(20)
      FPAR (X) = (PAR1 *X + PAR2 )*X + PAR3
        DATA NPR /0/
        NPR = NPR + 1
      MTREL = IABS(MTREL0)
      SIGDV0 = RSLT(4)
      IP = 0
      ITREL = MTREL
16000 CONTINUE
        JPTREL = IATREL(ITREL)
        IPTREL = ISHFTR(JPTREL,16)
      IF(
     - LAND(JPTREL,1).EQ.0
     -)THEN
          XO(IP+1) = WRK(IPTREL+ 3)
          XO(IP+2) = WRK(IPTREL+11)
          YO(IP+1) = WRK(IPTREL+ 4)
          YO(IP+2) = WRK(IPTREL+12)
      ELSE
          XO(IP+1) = WRK(IPTREL+ 7)
          XO(IP+2) = WRK(IPTREL+15)
          YO(IP+1) = WRK(IPTREL+ 8)
          YO(IP+2) = WRK(IPTREL+16)
      ENDIF
      IF(
     - LAND(JPTREL,12).EQ.0
     -)THEN
      IF(
     - MTREL0.LT.0
     -)THEN
            ZWZ = XO(IP+1)
            XO(IP+1) = XO(IP+2)
            XO(IP+2) = ZWZ
            ZWZ = YO(IP+1)
            YO(IP+1) = YO(IP+2)
            YO(IP+2) = ZWZ
      ENDIF
          IP = IP + 2
      ELSE
      IF(
     - LAND(JPTREL, 4).NE.0
     -)THEN
            XO(IP+1) = XO(IP+2)
            YO(IP+1) = YO(IP+2)
      ENDIF
          IP = IP + 1
      ENDIF
      ITREL = ITREL - 1
      IF(.NOT.(
     - ITREL.EQ.0
     -))GOTO 16000
16001 CONTINUE
      IP9 = IP
      IP8 = IP - 1
        X1 = XO(  1)
        X2 = XO(IP9)
        Y1 = YO(  1)
        Y2 = YO(IP9)
        XOA = ( X1 + X2 ) / 2
        YOA = ( Y1 + Y2 ) / 2
        COSTH = X2 - X1
        SINTH = Y2 - Y1
        SUMX = 1.0 / SQRT(COSTH**2+SINTH**2)
        COSTH = COSTH * SUMX
        SINTH = SINTH * SUMX
        DMIN = 1000000.
      DO 13000 IP=1,IP9
          XX = XO(IP) - XOA
          YY = YO(IP) - YOA
          YT(IP) = YY * COSTH - XX * SINTH
          XT(IP) = XX * COSTH + YY * SINTH
          DX = ABS(XT(IP))
      IF(
     - DX.LT.DMIN
     -)THEN
            DMIN = DX
            IPA = IP
      ENDIF
13000 CONTINUE
13001 CONTINUE
        XC = XT(IPA)
        YC = YT(IPA)
      S1 = 0.
      S2 = 0.
      S3 = 0.
      S4 = 0.
      S5 = 0.
      S6 = 0.
      S7 = 0.
      DO 13002 IP = 1,IP9
        X = XT(IP)
        Y = YT(IP)
        X2 = X**2
        S1 = S1 + X
        S2 = S2 + X2
        S3 = S3 + X*X2
        S4 = S4 + X2**2
        S5 = S5 + Y*X2
        S6 = S6 + Y*X
        S7 = S7 + Y
13002 CONTINUE
13003 CONTINUE
      S0 = IP9
      F1 = 1. / S4
      XX12 = S3*F1
      XX13 = S2*F1
      YY1  = S5*F1
      XX22 = S2 - S3*XX12
      XX23 = S1 - S3*XX13
      YY2  = S6 - S3*YY1
      XX32 = S1 - S2*XX12
      XX33 = S0 - S2*XX13
      YY3  = S7 - S2*YY1
      IF(
     - XX22.GT.XX32
     -)THEN
        XX23 = XX23 / XX22
        YY2  = YY2  / XX22
        PAR3 = (YY3 - XX32*YY2) / (XX33 - XX32*XX23)
        PAR2 = YY2 - XX23*PAR3
      ELSE
        XX33 = XX33 / XX32
        YY3  = YY3  / XX32
        PAR3 = (YY2 - XX22*YY3) / (XX23 - XX22*XX33)
        PAR2 = YY3 - XX33*PAR3
      ENDIF
      PAR1 = YY1 - XX12*PAR2 - XX13*PAR3
      XCIRC =-.5 * PAR2 / PAR1
      YZW   = FPAR(XCIRC)
      R0 = SIGN(1.E6,PAR1)
      IF(ABS(PAR1).GT..5E-6) R0 = .5 / PAR1
      YCIRC = R0 + YZW
      R0 = ABS(R0)
      IF(
     - R0.GT.2000.
     -)THEN
        SUM2 = 0.
        ZW1 = R0**2
        ZW2 = .5/R0
      DO 13004 IP=1,IP9
          F = FPAR(XT(IP))
          DR = YT(IP) - F
          SUM2 = SUM2 + DR**2
13004 CONTINUE
13005 CONTINUE
        SIG = SQRT(SUM2/(IP9-3))
        DX = 0.
      ELSE
      IF(
     - XC.LT.0.
     -)THEN
          X2 = XC
          Y2 = YC
          X1 = XT(  1)
          Y1 = YT(  1)
      ELSE
          X1 = XC
          Y1 = YC
          X2 = XT(IP9)
          Y2 = YT(IP9)
      ENDIF
        XCIRC = 0.
        YCIRC = 100000.
        DY = Y2 - Y1
        IF(ABS(DY).GT..01)
     +     YCIRC = ((X2-X1)*(X2+X1)/DY + Y2+Y1)*.5
        IF(ABS(YCIRC).GT.100000.) YCIRC = SIGN(100000.,YCIRC)
        T  = YCIRC - YC
        R0 = ABS(T)
        CHARGE = SIGN(1.,T)
        R0 = .5*(R0 + ((YCIRC-YC)**2+XC**2) / R0)
      NTRIAL = 0
16002 CONTINUE
      NTRIAL = NTRIAL + 1
        SUM1 = 0.
        SUM2 = 0.
        SUM3 = 0.
        SUM4 = 0.
        ZW1 = R0**2
        ZW2 = .5/R0
      DO 13006 IP=1,IP9
          XZW = XT(IP)
          YZW = (XZW**2+(YT(IP)-YCIRC)**2 - ZW1) * ZW2
          SUM1 = SUM1 + XZW
          SUM2 = SUM2 + XZW**2
          SUM3 = SUM3 + XZW*YZW
          SUM4 = SUM4 + YZW
13006 CONTINUE
13007 CONTINUE
        TILT = (SUM3*IP9-SUM1*SUM4) / (SUM2*IP9-SUM1**2)
        DR0  = (SUM4 - TILT*SUM1)/IP9
        R0 = R0 + DR0
        DX =-TILT*R0
        SUM1 = 0.
        SUM2 = 0.
        SUM3 = 0.
        SUM4 = 0.
        ZW1 = R0**2
        ZW2 = .5/R0
      DO 13008 IP=1,IP9
          XT(IP) = XT(IP) + DX
          XZW = XT(IP)**2
          YZW = (XZW+(YT(IP)-YCIRC)**2 - ZW1) * ZW2
          SUM1 = SUM1 + XZW
          SUM2 = SUM2 + XZW**2
          SUM3 = SUM3 + XZW*YZW
          SUM4 = SUM4 + YZW
13008 CONTINUE
13009 CONTINUE
        B = (SUM3*IP9-SUM1*SUM4) / (SUM2*IP9-SUM1**2)
        DSAG = (SUM4 - B*SUM1)*2. / IP9
        SAG = (XT(IP9)-XT(1))**2 * .125/R0
        DR0 =-R0*DSAG/SAG
        IF(ABS(DSAG/SAG) .GT. 0.2) DR0 = 0.
        R0ZW  = DR0 + R0
        YCZW  = CHARGE*DR0 + YCIRC
      IF(
     - ABS(YCZW).LT.100000. .AND. ABS(DR0).LT.100000.
     -)THEN
          YCIRC = YCZW
          R0    = R0ZW
          LBCHCH= 0
      ELSE
          LBCHCH= 1
      ENDIF
        SUM1 = 0.
        SUM2 = 0.
        ZW1 = R0**2
        ZW2 = .5/R0
      DO 13010 IP=1,IP9
          DR = ((XT(IP)-XCIRC)**2+(YT(IP)-YCIRC)**2 - ZW1) * ZW2
          SUM1 = SUM1 + DR
          SUM2 = SUM2 + DR**2
13010 CONTINUE
13011 CONTINUE
        DR0 = SUM1 / IP9
        R0 = R0 + DR0
        YCIRC = YCIRC + DR0*CHARGE
        SIG = 0.
        IF(IP9.GT.3) SIG = SQRT(ABS(SUM2/IP9 - DR0**2)*IP9/(IP9-3))
        SIG0= SQRT(SUM2/IP9)
      IF(
     - LBCHCH.EQ.0
     -)THEN
      GOTO 16003
      ENDIF
      IF(
     - NTRIAL.GT.1
     -)THEN
      GOTO 16003
      ENDIF
        YCSAV = YCIRC
        DXSAV = DX
        SGSAV = SIG0
        R0SAV = R0
        CHSAV = CHARGE
        YCIRC  = -YCIRC
        R0     = ABS(YCIRC)
        CHARGE =-CHARGE
      DO 13012 IP=1,IP9
          XT(IP) = XT(IP) - DX
13012 CONTINUE
13013 CONTINUE
      IF(.NOT.(
     - .FALSE.
     -))GOTO 16002
16003 CONTINUE
      IF(
     - NTRIAL.GT.1 .AND. SIG0.GT.SGSAV
     -)THEN
        YCIRC  = YCSAV
        DX     = DXSAV
        SIG0   = SGSAV
        R0     = R0SAV
        CHARGE = CHSAVE
      ENDIF
      ENDIF
      XX = XCIRC - DX
      YY = YCIRC
      X0 = XX * COSTH - YY * SINTH + XOA
      Y0 = YY * COSTH + XX * SINTH + YOA
      RATLR = ABS(XT(1)*2. / R0)
      SGANG  = 0.
      RATCR  = 0.
      IF(
     - SIG.LE.SIGDV0
     -)THEN
        NCLL  = 0
        ICLL0 = 0
        IP = 0
        ITREL = MTREL
16004 CONTINUE
          JPTREL = IATREL(ITREL)
          IPTREL = ISHFTR(JPTREL,16)
          ICLL = IWRK(IPTREL)
          IF(ICLL.NE.ICLL0) NCLL = NCLL + 1
          ICLL0 = ICLL
      IF(
     - LAND(JPTREL,1).EQ.0
     -)THEN
            XO(IP+1) = WRK(IPTREL+ 3)
            XO(IP+2) = WRK(IPTREL+11)
            YO(IP+1) = WRK(IPTREL+ 4)
            YO(IP+2) = WRK(IPTREL+12)
            XT(IP+1) = WRK(IPTREL+ 5)
            XT(IP+2) = WRK(IPTREL+13)
            YT(IP+1) = WRK(IPTREL+ 6)
            YT(IP+2) = WRK(IPTREL+14)
      ELSE
            XO(IP+1) = WRK(IPTREL+ 7)
            XO(IP+2) = WRK(IPTREL+15)
            YO(IP+1) = WRK(IPTREL+ 8)
            YO(IP+2) = WRK(IPTREL+16)
            XT(IP+1) = WRK(IPTREL+ 9)
            XT(IP+2) = WRK(IPTREL+17)
            YT(IP+1) = WRK(IPTREL+10)
            YT(IP+2) = WRK(IPTREL+18)
      ENDIF
      IF(
     - LAND(JPTREL,12).EQ.0
     -)THEN
      IF(
     - IABS(IWRK(IPTREL+2)).GT.5
     -)THEN
              IP = IP + 2
      ELSE
              XT(IP+1) = (XT(IP+1)+XT(IP+2)) * .5
              YT(IP+1) = (YT(IP+1)+YT(IP+2)) * .5
              XO(IP+1) = (XO(IP+1)+XO(IP+2)) * .5
              YO(IP+1) = (YO(IP+1)+YO(IP+2)) * .5
              IP = IP + 1
      ENDIF
      ELSE
      IF(
     - LAND(JPTREL, 4).NE.0
     -)THEN
              XT(IP+1) = XT(IP+2)
              YT(IP+1) = YT(IP+2)
      ENDIF
            IP = IP + 1
      ENDIF
        ITREL = ITREL - 1
      IF(.NOT.(
     - ITREL.EQ.0
     -))GOTO 16004
16005 CONTINUE
        IP9 = IP
        IP8 = IP - 1
      DO 13014 IP=1,IP9
          DX = XT(IP)
          DY = YT(IP)
          XX = XO(IP)
          YY = YO(IP)
      IF(
     - ABS(XX-X0).LT.ABS(YY-Y0)
     -)THEN
            TG1  = DY / DX
            TGC1 =-(XX-X0) / (YY-Y0)
      ELSE
            TG1  = DX / DY
            TGC1 =-(YY-Y0) / (XX-X0)
      ENDIF
          DLTS1 = ABS((TG1-TGC1) / (1.+TG1*TGC1))
          SGANG  = DLTS1**2 + SGANG
13014 CONTINUE
13015 CONTINUE
        SGANG  = SQRT(SGANG/FLOAT(IP9))
        RATCR  = NCLL*200. / R0
      ENDIF
      RSLT(1) = R0
      RSLT(2) = X0
      RSLT(3) = Y0
      RSLT(4) = SIG
      RSLT(5) = IP9 - 3
      RSLT(6) = RATLR
      RSLT(7) = SGANG
      RSLT(8) = RATCR
      RETURN
      END
