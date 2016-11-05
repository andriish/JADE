      SUBROUTINE TRACEO(ITRK,PAR1,PAR2,PAR3)
      IMPLICIT INTEGER*2 (H)
      LOGICAL DEADCL
#include "cheadr.for"
      EQUIVALENCE (HHEADR(18),HRUN)
#include "cdata.for"
#include "ccycp.for"
#include "cworkpr.for"
#include "cworkeq.for"
      EQUIVALENCE
     ,          (HPCO0 ,HPWRK(17)),(HPCO9 ,HPWRK(18)),(HLDCO ,HPWRK(19))
     ,         ,(ICELL ,IDWRK( 1)),(MHIT  ,IDWRK( 2)),(IRING ,IDWRK( 3))
     ,         ,(IERRCD,IDWRK( 4)),(NTRKEL,IDWRK( 5))
#include "cpatlm.for"
#include "cjdrch.for"
#include "cdsmax.for"
      COMMON/CALIBR/JPOINT(100),
     1HMUCAL(8370),HLGMAI(5760),HTAGSY(384),HJPULS(9216),
     1DELTA0(96,2),DELTA1(96,2),DELTA2(96,2),DELTA5(96,2),DELTA6(96,2)
      DIMENSION ITRCLL(6),CSRCLL(6),SNRCLL(6), NCNCK(24)
      DIMENSION CSRNG1(3),SNRNG1(3),CSRNG3(4),SNRNG3(4)
      INTEGER MKBDCL(3) /Z10,Z20,Z40/
      INTEGER MKDDCL(3) /Z01,Z02,Z04/
      DATA LBINIT /0/
      IF(
     - LBINIT .EQ. 0
     -)THEN
        LBINIT = 1
      ASSIGN 17001 TO IZZZ01
      GOTO 17000
17001 CONTINUE
      ENDIF
      HPCO0  = HPFREE
      HLDCO  = 14
      HPFREE = HLDCO*100 + HPCO0
      HPCO9 = HPFREE - 1
      IPCOMX= HPFREE - HLDCO
      R0 = FSENSW(IRING)
      DR = RINCR (IRING)
      X0    = DR*15. + R0
      Y0    = (PAR1*X0 + PAR2) * X0
      TGROT = Y0 / X0
      CSROT = 1. / SQRT(TGROT**2 + 1.)
      SNROT = CSROT*TGROT
      CSROT0= CSROT
      SNROT0= SNROT
      ASSIGN 17003 TO IZZZ02
      GOTO 17002
17003 CONTINUE
      WGHT0 = .01
      Y0    = 0.
      ASSIGN 17005 TO IZZZ03
      GOTO 17004
17005 CONTINUE
      IF(S0.LT.3.5 .OR. SIG.GT.100.) RETURN
      DORIG = PAR3
      IF(PAR1.LT.0) DORIG =-PAR3
      IF(
     - DORIG.GT.11. .OR. DORIG.LT.-3.
     -)THEN
        WGHT0 = 1.0
        Y0 = SIGN(10.,PAR1)
        IF(DORIG.LT.0) Y0 =-SIGN(2.,PAR1)
      ASSIGN 17006 TO IZZZ03
      GOTO 17004
17006 CONTINUE
      ENDIF
      IF(SIG.GT.0.15) RETURN
      IF(S0 .LT. 4.5) RETURN
      IF(SIG.GT.0.06 .AND. S0 .LT. 6.5) RETURN
      JCELL = ICELL
      JRING = IRING
      ASSIGN 17008 TO IZZZ04
      GOTO 17007
17008 CONTINUE
      KCLL = 0
      NHIT = 0
      IPCO = HPCO0
      JRING = 0
      LBCELL = 0
      LBHTMX = 0
16000 CONTINUE
      JRING = JRING + 1
        NHRNG = 0
        NCLL = 0
16002 CONTINUE
        NCLL = NCLL + 1
        KCLL = KCLL + 1
          JCELL = ITRCLL(KCLL)
      IF(
     - JCELL.NE.0
     -)THEN
            CSROT = CSRCLL(KCLL)
            SNROT = SNRCLL(KCLL)
      ASSIGN 17010 TO IZZZ05
      GOTO 17009
17010 CONTINUE
            NHRNG = NHRNG + JHIT
            IF(LBHTMX.NE.0) RETURN
      ENDIF
      IF(.NOT.(
     - NCLL.EQ.2
     -))GOTO 16002
16003 CONTINUE
        IF(NHRNG.LT.2 .AND. JRING.LT.IRING) RETURN
        IF(JCELL.NE.0) LBCELL = LOR(MKBDCL(JRING),LBCELL)
      IF(.NOT.(
     - KCLL.EQ.6
     -))GOTO 16000
16001 CONTINUE
      HPCO9 = IPCO - 1
      CALL LBHTRO(ITRK,PAR1,PAR2,PAR3,INDPAR)
      IF(
     - INDPAR.EQ.0
     -)THEN
        WGHT0 = 0.10
        Y0 = 0.
      ELSE
        WGHT0 = 0.03
        Y0 = PAR3
      ENDIF
      ASSIGN 17011 TO IZZZ03
      GOTO 17004
17011 CONTINUE
      IF(S0.LT.3.5 .OR. SIG.GT.100.) RETURN
      ALBLM1 = 0.6
      ALBLM2 = 3.0
      ASSIGN 17013 TO IZZZ06
      GOTO 17012
17013 CONTINUE
      WGHT0 = PATRLM(2)
      SIG0  = SIG
      ASSIGN 17014 TO IZZZ03
      GOTO 17004
17014 CONTINUE
      IF(S0.LT.7.5 .OR. SIG.GT.100.) RETURN
      ALBLM1 = 0.6
      ALBLM2 = 3.0
      ASSIGN 17015 TO IZZZ06
      GOTO 17012
17015 CONTINUE
15000 CONTINUE
      IF(
     -  NHITLB-S0 .GT. 4.5 .OR. SIG0-SIG.GT..25
     -)THEN
        SIG0  = SIG
      ASSIGN 17016 TO IZZZ03
      GOTO 17004
17016 CONTINUE
      ASSIGN 17017 TO IZZZ06
      GOTO 17012
17017 CONTINUE
      GOTO 15000
      ENDIF
15001 CONTINUE
      IF(
     - SIG.LE.0.1225
     -)THEN
        CALL CKTRKO(LBCKTR,LBCELL)
        IF(LBCKTR.LE.16) RETURN
      ASSIGN 17019 TO IZZZ07
      GOTO 17018
17019 CONTINUE
        HPHT0S = HPHT0
        HPHT9S = HPHT9
        HLDHTS = HLDHT
        HPHT0  = HPCO0
        HPHT9  = HPCO9
        HLDHT  = HLDCO
        CALL STTRKO(LBCKTR)
        HPHT0  = HPHT0S
        HPHT9  = HPHT9S
        HLDHT  = HLDHTS
      ENDIF
      RETURN
17002 CONTINUE
      XR1  = DR*7.5 + R0
      TGB  = PAR1*XR1 * 2. + PAR2
      CSB  = 1. / SQRT(TGB**2+1.)
      SNB  = CSB * TGB
      DRISN  = SINDRI(ICELL,1)
      DRICS  = COSDRI(ICELL,1)
      IHIT = 0
      IPCO = HPCO0
      IP = HPHT9 - HLDHT + 1
16004 CONTINUE
        LBSIDE  = 0
        LBGOOD = 0
      IF(
     - IABS(IWRK(IP+ 9)).EQ.ITRK
     -)THEN
          LBSIDE = -1
          IF(LAND(IWRK(IP+4),2).NE.0) LBSIDE = 1
          IF(IWRK(IP+ 9).LT.0) LBGOOD = 1
      ELSE
      IF(
     - IABS(IWRK(IP+10)).EQ.ITRK
     -)THEN
          LBSIDE = -1
          IF(LAND(IWRK(IP+4),8).NE.0) LBSIDE = 1
          IF(IWRK(IP+10).LT.0) LBGOOD = 1
      ENDIF
      ENDIF
      IF(
     - LBSIDE.NE.0
     -)THEN
          ILAY = IWRK(IP  )
          Y    = SWDEPL
          IF(LAND(ILAY,1).NE.0) Y =-Y
          DS   =  WRK(IP+2)
          X    = ILAY * DR + R0
      IF(
     - DS.LE.DRC
     -)THEN
            DX   =-DS * SNB
            DY   = DS * CSB
      ELSE
            DX   =-(DS-DRC)*DRISN - DRC*SNB
            DY   = (DS-DRC)*DRICS + DRC*CSB
      ENDIF
          X    = DX*LBSIDE + X
          Y    = DY*LBSIDE + Y
          XX   = X*CSROT + Y*SNROT
          YY   =-X*SNROT + Y*CSROT
          IF(LBGOOD.LE.2) IHIT = IHIT + 1
          IWRK(IPCO   ) = ILAY
          IWRK(IPCO+ 1) = IWRK(IP+1)
          IWRK(IPCO+ 2) = LBSIDE
          WRK (IPCO+ 3) = XX
          WRK (IPCO+ 4) = YY
          WRK (IPCO+ 5) = 0.
          WRK (IPCO+ 6) = 0.
          IWRK(IPCO+ 7) = LBGOOD
          WRK (IPCO+ 8) = DS
          IWRK(IPCO+ 9) = ICELL
          IWRK(IPCO+10) = LBGOOD
          WRK (IPCO+11) = TGB
          IWRK(IPCO+12) = IRING
          WRK (IPCO+13) = 0.
          IPCO = IPCO + HLDCO
      ENDIF
      IP = IP - HLDHT
      IF(.NOT.(
     - IP.LT.HPHT0
     -))GOTO 16004
16005 CONTINUE
      NHIT = IHIT
      HPCO9 = IPCO - 1
      GOTO IZZZ02
17004 CONTINUE
      S0 = WGHT0
      S1 = 0.
      S2 = 0.
      S3 = 0.
      S4 = 0.
      S5 = 0.
      S6 = 0.
      S7 = Y0 * WGHT0
      IPCO = HPCO0
16006 CONTINUE
      IF(
     - IWRK(IPCO+ 7).EQ.0
     -)THEN
          X = WRK(IPCO+3)
          Y = WRK(IPCO+4)
          X2 = X**2
          S1 = S1 + X
          S2 = S2 + X2
          S3 = S3 + X*X2
          S4 = S4 + X2**2
          S5 = S5 + Y*X2
          S6 = S6 + Y*X
          S7 = S7 + Y
          S0 = S0 + 1.
      ENDIF
      IPCO = IPCO + HLDCO
      IF(.NOT.(
     - IPCO.GT.HPCO9
     -))GOTO 16006
16007 CONTINUE
      IF(
     - S0.LT.3.5
     -)THEN
        SIG = 1000.
      ELSE
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
          DET  = XX33 - XX32*XX23
      IF(
     - ABS(DET).GT.1.E-30
     -)THEN
            PAR3 = (YY3 - XX32*YY2) / DET
            PAR2 = YY2 - XX23*PAR3
      ENDIF
      ELSE
          XX33 = XX33 / XX32
          YY3  = YY3  / XX32
          DET  = XX23 - XX22*XX33
      IF(
     - ABS(DET).GT.1.E-30
     -)THEN
            PAR3 = (YY2 - XX22*YY3) / DET
            PAR2 = YY3 - XX33*PAR3
      ENDIF
      ENDIF
      IF(
     - ABS(DET).LE.1.E-30
     -)THEN
        SIG = 1000.
      ELSE
        PAR1 = YY1 - XX12*PAR2 - XX13*PAR3
        DEG = S0 - WGHT0 - 2.
        IF(WGHT0.LT..001) DEG = DEG - 1.
        CHISQ = 0.
        DCHIM1 = 0.
        IHITM1 = 0
        IPCO = HPCO0
16008 CONTINUE
      IF(
     - IWRK(IPCO+ 7).EQ.0
     -)THEN
            X = WRK(IPCO+3)
            Y = WRK(IPCO+4)
            F = (PAR1 *X + PAR2 )*X + PAR3
            DCHI = Y - F
            WRK(IPCO+13) = DCHI
            CHISQ = CHISQ + DCHI**2
      ENDIF
        IPCO = IPCO + HLDCO
      IF(.NOT.(
     - IPCO.GT.HPCO9
     -))GOTO 16008
16009 CONTINUE
        SIG    =      CHISQ  / DEG
        SIGLM = TRELLM(16)**2
      ENDIF
      ENDIF
      GOTO IZZZ03
17007 CONTINUE
        ICELL0 = ICELL - 1
        IF(ICELL.GT.24) ICELL0 = ICELL - 25
        IF(ICELL.GT.48) ICELL0 = (ICELL-49)/2
      IF(
     - IRING.NE.3
     -)THEN
          TGR1 = TGROT
          SNR1 = SNROT
          CSR1 = CSROT
      ELSE
      IF(
     - LAND(ICELL,1).EQ.0
     -)THEN
            CSR1 = CSROT*CSFI3 - SNROT*SNFI3
            SNR1 = CSROT*SNFI3 + SNROT*CSFI3
      ELSE
            CSR1 = CSROT*CSFI3 + SNROT*SNFI3
            SNR1 =-CSROT*SNFI3 + SNROT*CSFI3
      ENDIF
          TGR1 = SNR1 / CSR1
      ENDIF
        JRING = 0
15002 CONTINUE
      IF(
     - JRING.LT.3
     -)THEN
        JRING = JRING + 1
          R0 = FSENSW(JRING)
          DR = RINCR (JRING)
          X1 = R0 - DR*2.
          X2 = R0 + DR*17.
          Y1 = (PAR1*X1 + PAR2)*X1 + PAR3
          Y2 = (PAR1*X2 + PAR2)*X2 + PAR3
          XX1 = X1*CSR1 - Y1*SNR1
          YY1 = X1*SNR1 + Y1*CSR1
          XX2 = X2*CSR1 - Y2*SNR1
          YY2 = X2*SNR1 + Y2*CSR1
          ICELL1 = 0
          LBINV  = 0
16010 CONTINUE
      IF(
     - JRING.EQ.3
     -)THEN
      IF(
     - ABS(YY1).LT.3.0 .OR. ABS(YY2).LT.3.0 .OR. YY1*YY2.LE.0.
     -)THEN
              ICELL1 =  2
              LBINV = 1
              IF(YY1.GT.YY2) LBINV =-1
      GOTO 16011
      ENDIF
            DY1 = ABS(YY1) - TGFI1*XX1
            DY2 = ABS(YY2) - TGFI1*XX2
      IF(
     - ABS(DY1).LT.3.0 .OR. ABS(DY2).LT.3.0 .OR. DY1*DY2.LE.0.
     -)THEN
              ICELL1 =  1
              IF(YY1.GT.0) ICELL1 = 3
              LBINV  = 1
              IF((DY2-DY1)*YY1 .LT. 0.) LBINV =-1
      GOTO 16011
      ENDIF
            ICELL1 =  2
            IF(YY1.GT.0.) ICELL1 = 3
      IF(
     - DY1.GT.0
     -)THEN
              ICELL1 =  1
              IF(YY1.GT.0.) ICELL1 = 4
      ENDIF
      GOTO 16011
      ENDIF
          DY1 = ABS(YY1) - TGFI1*XX1
          DY2 = ABS(YY2) - TGFI1*XX2
      IF(
     - ABS(DY1).LT.3.0 .OR. ABS(DY2).LT.3.0 .OR. DY1*DY2.LE.0.
     -)THEN
            ICELL1 =  1
            IF(YY1.GT.0) ICELL1 = 2
            LBINV  =  1
            IF((DY2-DY1)*YY1.LT.0.) LBINV =-1
      GOTO 16011
      ENDIF
          ICELL1 =  2
      IF(
     - DY1.GT.0.
     -)THEN
            ICELL1 =  1
            IF(YY1.GT.0.) ICELL1 = 3
      ENDIF
      IF(.NOT.(
     - .TRUE.
     -))GOTO 16010
16011 CONTINUE
          IC2 = 0
          CSROT2 = 0.
          SNROT2 = 0.
      IF(
     - JRING.NE.3
     -)THEN
            IC1 = ICELL1 + ICELL0 - 1
            IF(IC1.LT. 1)  IC1 = IC1 + 24
            IF(IC1.GT.24)  IC1 = IC1 - 24
            IF(JRING.EQ.2) IC1 = IC1 + 24
            CSROT1 = CSRNG1(ICELL1)
            SNROT1 = SNRNG1(ICELL1)
      IF(
     - LBINV.NE.0
     -)THEN
              IC2 = ICELL1 + ICELL0
              IF(IC2.LT. 1)  IC2 = IC2 + 24
              IF(IC2.GT.24)  IC2 = IC2 - 24
              IF(JRING.EQ.2) IC2 = IC2 + 24
              CSROT2 = CSRNG1(ICELL1+1)
              SNROT2 = SNRNG1(ICELL1+1)
      ENDIF
      ELSE
            IC1 = ICELL1 + ICELL0*2 + 47
            IF(IC1.LT.49)  IC1 = IC1 + 48
            IF(IC1.GT.96)  IC1 = IC1 - 48
            CSROT1 = CSRNG3(ICELL1)
            SNROT1 = SNRNG3(ICELL1)
      IF(
     - LBINV.NE.0
     -)THEN
              IC2 = ICELL1 + ICELL0*2 + 48
              IF(IC2.LT.49)  IC2 = IC2 + 48
              IF(IC2.GT.96)  IC2 = IC2 - 48
              CSROT2 = CSRNG3(ICELL1+1)
              SNROT2 = SNRNG3(ICELL1+1)
      ENDIF
      ENDIF
      IF(
     - LBINV.LT.0
     -)THEN
            IZW    = IC2
            IC2    = IC1
            IC1    = IZW
            ZWZ    = CSROT2
            CSROT2 = CSROT1
            CSROT1 = ZWZ
            ZWZ    = SNROT2
            SNROT2 = SNROT1
            SNROT1 = ZWZ
      ENDIF
          ITRCLL(JRING*2-1) = IC1
          ITRCLL(JRING*2  ) = IC2
          CSRCLL(JRING*2-1) = CSROT1*CSR1 + SNROT1*SNR1
          SNRCLL(JRING*2-1) =-SNROT1*CSR1 + CSROT1*SNR1
          CSRCLL(JRING*2  ) = CSROT2*CSR1 + SNROT2*SNR1
          SNRCLL(JRING*2  ) =-SNROT2*CSR1 + CSROT2*SNR1
      GOTO 15002
      ENDIF
15003 CONTINUE
      GOTO IZZZ04
17009 CONTINUE
        R0 = FSENSW(JRING)
        DR = RINCR (JRING)
        XR1  = DR*7.5 + R0
        TGB  = PAR1*XR1 * 2. + PAR2
        XX   = 1. / SQRT(TGB**2+1.)
        YY   = XX * TGB
        CSB  = XX*CSROT - YY*SNROT
        SNB  = XX*SNROT + YY*CSROT
        TGB  = SNB/CSB
        DSBIN1 = DRIVEL(JCELL,1)
        DS0 = 0.
        IF(HRUN.LE.100) DS0 = DSBIN1*.5
        DRISN  = SINDRI(JCELL,1)
        DRICS  = COSDRI(JCELL,1)
        DRISNF = DRISN * .05
        DRITG  = DRISN/DRICS
        TANBET = ABS((TGB-DRITG)/(TGB*DRITG+1.))
        CCST01 = DELTA0(JCELL,1)*TANBET
        CCST02 = DELTA0(JCELL,2)*TANBET
        CCST11 = DELTA1(JCELL,1)
        CCST12 = DELTA1(JCELL,2)
        CCST21 = DELTA2(JCELL,1)
        CCST22 = DELTA2(JCELL,2)
        CCST51 = DELTA5(JCELL,1) * 10.
        CCST61 = DELTA6(JCELL,1) * 10.
        JHIT = 0
        NHIT   = 0
        NHGOOD = 0
        ILAYL =-99
        IPCO = IPCO - HLDCO
        IP9 = HPTSEC(JCELL+1)
        IP  = HPTSEC(JCELL  )
15004 CONTINUE
      IF(
     - IP.LT.IP9
     -)THEN
          IWIR = HDATA(IP)
          IWIR = ISHFTR(IWIR,3)
          ILAY = LAND(IWIR,15)
          DS =(HDATA(IP+3)) * DSBIN1 + DS0
          DSC = DS
          Y    = SWDEPL
          IF(LAND(ILAY,1).NE.0) Y =-Y
          Y    = (7-ILAY)*CCST51 - CCST61 + Y
          X    = ILAY * DR + R0
      IF(
     - DS.LE.DRC
     -)THEN
      IF(
     - DS.LT.4.0
     -)THEN
      IF(
     - DS.GT.DSD1
     -)THEN
                DSC = (DSD1-DSD0)*DRV0 + (DS-DSD1)*DRV1
      ELSE
                DSC = (DS-DSD0)*DRV0
      ENDIF
              IF(DSC.LT.0.1) DSC = 0.1
      ENDIF
            DXR  =-DSC * SNB
            DYR  = DSC * CSB
            DXL  =-DXR
            DYL  =-DYR
      ELSE
      IF(
     - ILAY.LT. 3
     -)THEN
              DILAY =-(ILAY- 3)**2
              DSCL  = (DILAY*CCST11 + 1.) * DSC
              DSCR  = (DILAY*CCST12 + 1.) * DSC
      ELSE
      IF(
     - ILAY.GT.12
     -)THEN
              DILAY =-(ILAY-12)**2
              DSCL  = (DILAY*CCST21 + 1.) * DSC
              DSCR  = (DILAY*CCST22 + 1.) * DSC
      ELSE
              DSCL = DSC
              DSCR = DSC
      ENDIF
      ENDIF
      IF(
     - DSC.GT.ABERR(7)
     -)THEN
              DWIR  = ILAY - 7.5
              DWIRC = DSC*DRISNF
              DWIRL = DWIR + DWIRC
              DWIRR = DWIR - DWIRC
              DSCL  = (DSCL-ABERR(7))*DWIRL*CCST01 + DSCL
              DSCR  =-(DSCR-ABERR(7))*DWIRR*CCST02 + DSCR
      ENDIF
            DXR  =-(DSCR-DRC)*DRISN - DRC*SNB
            DYR  = (DSCR-DRC)*DRICS + DRC*CSB
            DXL  = (DSCL-DRC)*DRISN + DRC*SNB
            DYL  =-(DSCL-DRC)*DRICS - DRC*CSB
      ENDIF
          XL   = DXL + X
          YL   = DYL + Y
          XXL  = XL*CSROT + YL*SNROT
          YYL  =-XL*SNROT + YL*CSROT
          FL   = (PAR1*XXL + PAR2)*XXL + PAR3
          DFL  = FL - YYL
          XR   = DXR + X
          YR   = DYR + Y
          XXR  = XR*CSROT + YR*SNROT
          YYR  =-XR*SNROT + YR*CSROT
          FR   = (PAR1*XXR + PAR2)*XXR + PAR3
          DFR  = YYR - FR
          NLRSOL = 1
          IF(DS.LT.2.0 .AND. DS.NE.0 .AND. JCELL.NE.ICELL) NLRSOL = 2
          ILRSOL = 0
16012 CONTINUE
          ILRSOL = ILRSOL + 1
      IF(
     - NLRSOL.EQ.1 .AND. ABS(DFL).LT.ABS(DFR) .OR.
     ?         NLRSOL.EQ.2 .AND. ILRSOL.EQ.1
     -)THEN
              LBSIDE =-1
              XX  = XXL
              YY  = YYL
              DF  = DFL
      ELSE
              LBSIDE = 1
              XX  = XXR
              YY  = YYR
              DF  = DFR
      ENDIF
            LBGOOD = 0
            IF(ABS(DF).GT.2.0) LBGOOD = 1
            IF(ABS(DF).GT.5.0) LBGOOD = 8
            IF(DF.LE.-10.0) LBGOOD = 4
            IF(DF.GT.-10.0 .AND. DF.LT.-5.0) LBGOOD = 2
      IF(
     - ILAY.NE.ILAYL .OR. LBGDL.LE.1.AND.LBGOOD.LE.1
     -)THEN
              LBREG = 1
              JHIT = JHIT + 1
              IPCO = IPCO + HLDCO
      ELSE
              LBREG = 0
              IF(LBGOOD.LT.IWRK(IPCO+7)) LBREG = 1
            IF(LBGOOD.GE.4 .AND. ABS(DF).LT.ABS(WRK(IPCO+13))) LBREG = 100079400
      ENDIF
      IF(
     - LBREG.NE.0
     -)THEN
      IF(
     - IPCO.GT.IPCOMX
     -)THEN
                LBHTMX = 1
      GOTO 15005
      ENDIF
              NHIT   = NHIT   + 1
              IF(LBGOOD.LE.1) NHGOOD = NHGOOD + 1
              IWRK(IPCO   ) = ILAY
              IWRK(IPCO+ 1) = IP
              IWRK(IPCO+ 2) = LBSIDE
              WRK (IPCO+ 3) = XX
              WRK (IPCO+ 4) = YY
              WRK (IPCO+ 5) = 0.
              WRK (IPCO+ 6) = 0.
              IWRK(IPCO+ 7) = LBGOOD
              WRK (IPCO+ 8) = DSC
              IWRK(IPCO+ 9) = JCELL
              IWRK(IPCO+10) = LBGOOD
              WRK (IPCO+11) = TANBET
              IWRK(IPCO+12) = JRING
              WRK (IPCO+13) = DF
              ILAYL = ILAY
              LBGDL = LBGOOD
      ENDIF
      IF(.NOT.(
     - ILRSOL.GE.NLRSOL
     -))GOTO 16012
16013 CONTINUE
        IP = IP + 4
      GOTO 15004
      ENDIF
15005 CONTINUE
      IF(
     - LBHTMX.EQ.0
     -)THEN
          IPCO = IPCO + HLDCO
      IF(
     - NHIT.LE.2
     -)THEN
            NRUN = HRUN
      IF(
     - DEADCL(JCELL,NRUN)
     -)THEN
              LBCELL = LOR(LBCELL,MKDDCL(JRING))
              JHIT = 16
              NHIT = 16
      ENDIF
      ENDIF
      ENDIF
      GOTO IZZZ05
17018 CONTINUE
      XST = 10000.
      XEN =     0.
      DO 13000 IPCO = HPCO0,HPCO9,HLDCO
      IF(
     - IWRK(IPCO+7).EQ.0
     -)THEN
          X = WRK(IPCO+3)
          IF(X.GT.XEN) XEN = X
          IF(X.LT.XST) XST = X
      ENDIF
13000 CONTINUE
13001 CONTINUE
      YST = (PAR1*XST + PAR2)*XST + PAR3
      YEN = (PAR1*XEN + PAR2)*XEN + PAR3
      TGST = PAR1*XST*2 + PAR2
      DXST = 1./SQRT(TGST**2+1.)
      DYST = DXST * TGST
      TGEN = PAR1*XEN*2 + PAR2
      DXEN = 1./SQRT(TGEN**2+1.)
      DYEN = DXEN * TGEN
      XMIN = -PAR2*.5 / PAR1
      YMIN = (PAR1*XMIN + PAR2)*XMIN + PAR3
      CURV =-PAR1 * 2.
      JCELLD = ICELL
      IF(ICELL.GT.24) JCELLD = ICELL - 24
      IF(ICELL.GT.48) JCELLD = ICELL - 48
      IF(
     - IRING.NE.3
     -)THEN
        DXWR  = DIRWR1(JCELLD,1)
        DYWR  = DIRWR1(JCELLD,2)
      ELSE
        DXWR  = DIRWR3(JCELLD,1)
        DYWR  = DIRWR3(JCELLD,2)
      ENDIF
      XX = DXWR*CSROT0 - DYWR*SNROT0
      YY = DXWR*SNROT0 + DYWR*CSROT0
      UN = SQRT(XX**2 + YY**2)
      CSROT = XX
      SNROT = YY
      HPTR0 = HPFREE
      IP    = HPTR0 - 1
      IWRK(IP+ 1) = 0
      IWRK(IP+ 2) = 16
      IWRK(IP+ 3) = 0
      IWRK(IP+ 4) = INDPAR + 1
      WRK (IP+ 5) = XST *CSROT - YST *SNROT
      WRK (IP+ 6) = XST *SNROT + YST *CSROT
      WRK (IP+ 7) = SQRT(WRK(IP+ 5)**2 + WRK(IP+ 6)**2)
      WRK (IP+ 8) = DXST*CSROT - DYST*SNROT
      WRK (IP+ 9) = DXST*SNROT + DYST*CSROT
      WRK (IP+10) = 1.
      IWRK(IP+11) = 0
      WRK (IP+12) = XEN *CSROT - YEN *SNROT
      WRK (IP+13) = XEN *SNROT + YEN *CSROT
      WRK (IP+14) = SQRT(WRK(IP+12)**2 + WRK(IP+13)**2)
      WRK (IP+15) = DXEN*CSROT - DYEN*SNROT
      WRK (IP+16) = DXEN*SNROT + DYEN*CSROT
      WRK (IP+17) = 1.
      IWRK(IP+18) = 2
      WRK (IP+19) = ATAN2(SNROT,CSROT)
      WRK (IP+20) = XMIN*CSROT - YMIN*SNROT
      WRK (IP+21) = XMIN*SNROT + YMIN*CSROT
      WRK (IP+22) = PAR1
      WRK (IP+23) = SQRT(SIG)
      IWRK(IP+24) = S0 + .001
      WRK (IP+25) = CURV
      WRK (IP+26) = 0.
      WRK (IP+27) = CURV
      WRK (IP+28) = CURV
      I0 = IP+ 1
      I9 = IP+28
      GOTO IZZZ07
17012 CONTINUE
        IWL = -999
        NHITLB = 0
      DO 13002 IP = HPCO0,HPCO9,HLDCO
          IW0 = IWRK(IP)
          X   = WRK(IP+3)
          Y   = WRK(IP+4)
          F   = (PAR1*X + PAR2)*X + PAR3
          DF  = F - Y
      IF(
     - ABS(DF).LT.ALBLM1
     -)THEN
            LBGOOD = 0
            NHITLB = NHITLB + 1
      ELSE
            LBGOOD = 4
            IF(ABS(DF).LT.ALBLM2) LBGOOD = 1
      ENDIF
          IWRK(IP+ 7) = LBGOOD
          WRK (IP+13) = DF
      IF(
     - IWL.EQ.IW0
     -)THEN
      IF(
     - ABS(DFL).LT.ABS(DF)
     -)THEN
              IF(LBGOOD.EQ.0) NHITLB = NHITLB - 1
              IWRK(IP +7) = 16
      ELSE
              IF(IWRK(IPL+7).EQ.0) NHITLB = NHITLB - 1
              IWRK(IPL+7) = 16
      ENDIF
      ENDIF
          IWL = IW0
          IPL = IP
          DFL = DF
13002 CONTINUE
13003 CONTINUE
      GOTO IZZZ06
17000 CONTINUE
        DFI0 = 3.1415927 / 12.
        DFI1 = DFI0 * .5
        DFI3 = DFI1 * .5
        DFI4 = DFI3 + DFI1
        SNFI0 = SIN(DFI0)
        CSFI0 = COS(DFI0)
        TGFI0 = SNFI0/CSFI0
        SNFI1 = SIN(DFI1)
        CSFI1 = COS(DFI1)
        TGFI1 = SNFI1/CSFI1
        SNFI3 = SIN(DFI3)
        CSFI3 = COS(DFI3)
        TGFI3 = SNFI3/CSFI3
        SNFI4 = SIN(DFI4)
        CSFI4 = COS(DFI4)
        CSRNG1(1) = CSFI0
        CSRNG1(2) = 1.
        CSRNG1(3) = CSFI0
        SNRNG1(1) =-SNFI0
        SNRNG1(2) = 0.
        SNRNG1(3) = SNFI0
        CSRNG3(1) = CSFI4
        CSRNG3(2) = CSFI3
        CSRNG3(3) = CSFI3
        CSRNG3(4) = CSFI4
        SNRNG3(1) =-SNFI4
        SNRNG3(2) =-SNFI3
        SNRNG3(3) = SNFI3
        SNRNG3(4) = SNFI4
        DRC = RINCR(1)*.5 * DRICOS
      IF(
     - HRUN.LE.100
     -)THEN
          DSD0   = .0
          DSD1   = 5.0
          DSD2   = 5.0
          DRV0   = 1.0
          DRV1   = 1.0
      ELSE
          DSD0   =-.63
          DSD1   = 1.8
          DSD2   = 4.0
          DRV0   = 0.8
          DRV1   = (DSD2 - (DSD1-DSD0)*DRV0) / (DSD2-DSD1)
      ENDIF
      GOTO IZZZ01
      END
