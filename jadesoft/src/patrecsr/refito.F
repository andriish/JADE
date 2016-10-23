      SUBROUTINE REFITO(IPTR,IPJHTL,XO,YO,WGHTO)
      IMPLICIT INTEGER*2 (H)
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
      INTEGER DATE(5), IDAY /0/
      DIMENSION ITRCLL(6), NCNCK(24)
      INTEGER MKLRT1 /Z1000000/, MKLRT2 /Z100/
      INTEGER MKBDCL(3) /Z10,Z20,Z40/
      DATA LBINIT /0/
      IF(
     - LBINIT .EQ. 0
     -)THEN
        LBINIT = 1
      ASSIGN 17001 TO IZZZ01
      GOTO 17000
17001 CONTINUE
      ENDIF
      HPFREE = 1
      HPFRE1 = HPFREE
      HPCO0  = HPFREE
      HLDCO  = 14
      HPFREE = HLDCO*100 + HPCO0
      HPCO9 = HPFREE - 1
      IPHEAD = IDATA(IQHEAD)*2
      NRUN = HDATA(IPHEAD+10)
      HPTR0 = HPFREE
      CALL MVC(IWRK(HPTR0),0,IDATA(IPTR+1),0,192)
      ITRK = IDATA(IPTR+1)
      IF(
     - IDATA(IPTR+18).EQ.1
     -)THEN
        ALFA  = ADATA(IPTR+21)
        CRV   = ADATA(IPTR+19)
        IF(ABS(CRV).LT.1.E-8) CRV = SIGN(1.E-8,CRV)
        RAD   =  1./CRV + ADATA(IPTR+20)
        XCIRC = COS(ALFA) * RAD
        YCIRC = SIN(ALFA) * RAD
        CHARGE = SIGN(1.,ADATA(IPTR+25))
      ELSE
        CRV   = ADATA(IPTR+22)*2.
        IF(ABS(CRV).LT.1.E-8) CRV = SIGN(1.E-8,CRV)
        ALFA  = ADATA(IPTR+19)
        XCIRC =-SIN(ALFA)/CRV + ADATA(IPTR+20)
        YCIRC = COS(ALFA)/CRV + ADATA(IPTR+21)
        CHARGE =-SIGN(1.,ADATA(IPTR+22))
      ENDIF
      ZVERT = ADATA(IPTR+31)
      ZSLOP = ADATA(IPTR+30)
      COSTHI = SQRT(ZSLOP**2 + 1.)
      XX    = ADATA(IPTR+12)
      YY    = ADATA(IPTR+13)
      RR    = SQRT(XX**2+YY**2)
      CSROT = XX / RR
      SNROT = YY / RR
      X0   = XO*CSROT + YO*SNROT
      Y0   =-XO*SNROT + YO*CSROT
      WGHT0 = AMIN1(WGHTO,1.0)
      WGHT0 = AMAX1(WGHTO,0.0)
      ASSIGN 17003 TO IZZZ02
      GOTO 17002
17003 CONTINUE
      KCLL = 0
      NHIT = 0
      IPCO = HPCO0
      JRING = 0
      LBCELL = 0
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
      ASSIGN 17005 TO IZZZ03
      GOTO 17004
17005 CONTINUE
            NHRNG = NHRNG + JHIT
      ENDIF
      IF(.NOT.(
     - NCLL.EQ.2
     -))GOTO 16002
16003 CONTINUE
        IF(JCELL.NE.0) LBCELL = LOR(MKBDCL(JRING),LBCELL)
      IF(.NOT.(
     - KCLL.EQ.6
     -))GOTO 16000
16001 CONTINUE
      HPCO9 = IPCO - 1
      JRINGL = 3
      ASSIGN 17007 TO IZZZ04
      GOTO 17006
17007 CONTINUE
      ALBLM1 = 0.6
      ALBLM2 = 3.0
      ASSIGN 17009 TO IZZZ05
      GOTO 17008
17009 CONTINUE
      ASSIGN 17010 TO IZZZ04
      GOTO 17006
17010 CONTINUE
      ASSIGN 17011 TO IZZZ05
      GOTO 17008
17011 CONTINUE
      ASSIGN 17013 TO IZZZ06
      GOTO 17012
17013 CONTINUE
      IF(
     - ABS(PAR1).GT..00030 .AND. SIG.GT..06
     -)THEN
      IF(
     - SIG.GT..10 .OR. IDATA(IPTR+24)-INT(S0).GT.8
     -)THEN
          ALBLM1 = 1.5
          ALBLM2 = 3.0
      ASSIGN 17014 TO IZZZ05
      GOTO 17008
17014 CONTINUE
          JRINGL = 2
      ASSIGN 17015 TO IZZZ04
      GOTO 17006
17015 CONTINUE
          ALBLM1 = 0.6
      ASSIGN 17016 TO IZZZ05
      GOTO 17008
17016 CONTINUE
      ASSIGN 17017 TO IZZZ04
      GOTO 17006
17017 CONTINUE
      ASSIGN 17018 TO IZZZ05
      GOTO 17008
17018 CONTINUE
      IF(
     - SIG.LT..10
     -)THEN
      ASSIGN 17020 TO IZZZ07
      GOTO 17019
17020 CONTINUE
      ENDIF
      ENDIF
      ENDIF
      HPFREE = HPFRE1
      RETURN
17006 CONTINUE
      S0 = WGHT0
      S1 = 0.
      S2 = 0.
      S3 = 0.
      S4 = 0.
      S5 = 0.
      S6 = 0.
      S7 = Y0 * WGHT0
      IPCO = HPCO0
16004 CONTINUE
      IF(
     - IWRK(IPCO+ 7).EQ.0 .AND. IWRK(IPCO+12).LE.JRINGL
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
     -))GOTO 16004
16005 CONTINUE
      IF(
     - S0.LT.2.5
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
          PAR3 = (YY3 - XX32*YY2) / (XX33 - XX32*XX23)
          PAR2 = YY2 - XX23*PAR3
      ELSE
          XX33 = XX33 / XX32
          YY3  = YY3  / XX32
          PAR3 = (YY2 - XX22*YY3) / (XX23 - XX22*XX33)
          PAR2 = YY3 - XX33*PAR3
      ENDIF
        PAR1 = YY1 - XX12*PAR2 - XX13*PAR3
        DEG = S0 - WGHT0 - 2.
        CHISQ = 0.
        DCHIM1 = 0.
        IHITM1 = 0
        IHSTRT = 0
        IPCO = HPCO0
16006 CONTINUE
      IF(
     - IWRK(IPCO+ 7).EQ.0 .AND. IWRK(IPCO+12).LE.JRINGL
     -)THEN
            IF(IHSTRT.EQ.0) IHSTRT = IPCO
            IHEND = IPCO
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
     -))GOTO 16006
16007 CONTINUE
        SIG    =      CHISQ  / DEG
        SIGLM = TRELLM(16)**2
      ENDIF
      GOTO IZZZ04
17002 CONTINUE
      DO 13000 I=1,6
          ITRCLL(I) = 0
13000 CONTINUE
13001 CONTINUE
        IPC0 = IPTR + 34
        IPC9 = IPC0 +  5
        ICELL = 0
      DO 13002 IPC = IPC0,IPC9
          JCELL = IDATA(IPC)
      IF(
     - JCELL.GT. 0 .AND. JCELL.LE.96
     -)THEN
            JRING = 1
            IF(JCELL.GT.24) JRING = 2
            IF(JCELL.GT.48) JRING = 3
            JPC = JRING*2 - 1
      IF(
     - ITRCLL(JPC).EQ.0
     -)THEN
              ITRCLL(JPC) = JCELL
      ELSE
              IF(ITRCLL(JPC).NE.JCELL) ITRCLL(JPC+1) = JCELL
      ENDIF
            ICELL = JCELL
            IRING = JRING
      ENDIF
13002 CONTINUE
13003 CONTINUE
      GOTO IZZZ02
17004 CONTINUE
      IF(
     - JRING.NE.3
     -)THEN
          IC1 = JCELL
          IF(IC1.GT.24) IC1 = IC1 - 24
          CSROT0 = DIRWR1(IC1,1)
          SNROT0 = DIRWR1(IC1,2)
      ELSE
          IC1 = JCELL - 48
          CSROT0 = DIRWR3(IC1,1)
          SNROT0 = DIRWR3(IC1,2)
      ENDIF
        DRICS  = TRMATC(JCELL,2)
        DRISN  = TRMATS(JCELL,2)
        DRITG  = DRISN/DRICS
        DRISNF = DRISN * .05
        R0 = FSENSW(JRING)
        DR = RINCR (JRING)
        R1   = DR*7.5 + R0
        DX   = R1 * CSROT0 - XCIRC
        DY   = R1 * SNROT0 - YCIRC
        RR   = SQRT(DX**2 + DY**2) * CHARGE
        CSB  = DX / RR
        SNB  = DY / RR
        TGB  = CSB/SNB
        DSBIN1 = DRIVEL(JCELL,1)
        IF(NRUN.GT.100) DS0 = T0FIX(JRING)*DSBIN1*64.
        IF(NRUN.LE.100) DS0 = DSBIN1*.5
        TANBET = ABS((TGB-DRITG)/(TGB*DRITG+1.))
        CCST01 = DELTA0(JCELL,1)*TANBET
        CCST02 = DELTA0(JCELL,2)*TANBET
        CCST11 = DELTA1(JCELL,1)
        CCST12 = DELTA1(JCELL,2)
        CCST21 = DELTA2(JCELL,1)
        CCST22 = DELTA2(JCELL,2)
        CCST51 = DELTA5(JCELL,1) * 10.
        CCST52 = DELTA5(JCELL,2) / 121.15
        CCST61 = DELTA6(JCELL,1) * 10.
        CCST62 = DELTA6(JCELL,2) / 121.15
        JHIT = 0
        NHIT   = 0
        NHGOOD = 0
        ILAYL =-99
        IPCO = IPCO - HLDCO
        IPJETC = IDATA(IQJETC)*2
        IP0    = IPJETC + 100
        IPCLL  = IPJETC + 2 + JCELL
        IP     = HDATA(IPCLL  ) + IP0
        IP9    = HDATA(IPCLL+1) + IP0
        IPHL   = IPJHTL + 2 + HDATA(IPCLL)/4
15000 CONTINUE
      IF(
     - IP.LT.IP9
     -)THEN
          LB   = IDATA(IPHL)
          ITR1 = LAND(ISHFTR(LB,17),127)
          ITR2 = LAND(ISHFTR(LB, 1),127)
      IF(
     - ITR1.EQ.ITRK .OR. ITR2.EQ.ITRK
     -)THEN
            LBLR = 0
            IF(ITR1.EQ.ITRK) LBLR = LAND(LB,MKLRT1)
            IF(ITR2.EQ.ITRK) LBLR = LAND(LB,MKLRT2)
            LBSIDE =-1
            IF(LBLR.NE.0) LBSIDE = 1
            LBLR = LBSIDE
            IWIR = HDATA(IP)
            IWIR = ISHFTR(IWIR,3)
            ILAY = LAND(IWIR,15)
            DS =(HDATA(IP+3)) * DSBIN1
      DATA NPRHT /0/
      NPRHT = NPRHT + 1
            X1   = ILAY * DR + R0
            Z1   = X1*ZSLOP + ZVERT
            DDS = (1222.9-ABS(Z1))*ABERR(1) + ABERR(6)*R1*COSTHI
            DSC = DS - DDS + DS0
            Y1   = SWDEPL
            IF(LAND(ILAY,1).NE.0) Y1 =-Y1
            Y1   = (7-ILAY)*(CCST52*Z1+CCST51) - CCST62*Z1-CCST61 + Y1
            X    = X1*CSROT0 - Y1*SNROT0
            Y    = X1*SNROT0 + Y1*CSROT0
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
              DXR  = DSC * CSB
              DYR  = DSC * SNB
              DXL =-DXR
              DYL =-DYR
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
              DXR  = (DSCR-DRC)*DRISN + DRC*CSB
              DYR  = (DSCR-DRC)*DRICS + DRC*SNB
              DXL  =-(DSCL-DRC)*DRISN - DRC*CSB
              DYL  =-(DSCL-DRC)*DRICS - DRC*SNB
      ENDIF
            XL   = DXL + X
            YL   = DYL + Y
            XXL  = XL*CSROT + YL*SNROT
            YYL  =-XL*SNROT + YL*CSROT
            XR   = DXR + X
            YR   = DYR + Y
            XXR  = XR*CSROT + YR*SNROT
            YYR  =-XR*SNROT + YR*CSROT
            NLRSOL = 1
            IF(DS.LT.2.0) NLRSOL = 2
            ILRSOL = 0
16008 CONTINUE
            ILRSOL = ILRSOL + 1
      IF(
     - NLRSOL.EQ.1 .AND. LBSIDE.LT.0  .OR.
     ?           NLRSOL.EQ.2 .AND. ILRSOL.EQ.1
     -)THEN
                LBSIDE =-1
                XX  = XXL
                YY  = YYL
      ELSE
                LBSIDE = 1
                XX  = XXR
                YY  = YYR
      ENDIF
              LBGOOD = 0
              IF(LBSIDE.NE.LBLR) LBGOOD = 1
      IF(
     - ILAY.NE.ILAYL .OR. LBGDL.LE.1.AND.LBGOOD.LE.1
     -)THEN
                LBREG = 1
                JHIT = JHIT + 1
                IPCO = IPCO + HLDCO
      ELSE
                LBREG = 0
                IF(LBGOOD.LT.IWRK(IPCO+7)) LBREG = 1
      ENDIF
      IF(
     - LBREG.NE.0
     -)THEN
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
                WRK (IPCO+ 8) = DS
                IWRK(IPCO+ 9) = JCELL
                IWRK(IPCO+10) = LBGOOD
                WRK (IPCO+11) = TGB
                IWRK(IPCO+12) = JRING
                WRK (IPCO+13) = 0.
                ILAYL = ILAY
                LBGDL = LBGOOD
      ENDIF
      IF(.NOT.(
     - ILRSOL.GE.NLRSOL
     -))GOTO 16008
16009 CONTINUE
      ENDIF
        IPHL = IPHL + 1
        IP   = IP   + 4
      GOTO 15000
      ENDIF
15001 CONTINUE
        IPCO = IPCO + HLDCO
      IF(
     - NHIT.LE.2
     -)THEN
16010 CONTINUE
      IF(
     - JCELL.EQ.17
     -)THEN
      IF(
     - NRUN.GE.539
     -)THEN
                LBCELL = LOR(LBCELL,1)
                JHIT = 16
                NHIT = 16
      ENDIF
      GOTO 16011
      ENDIF
      IF(
     - JCELL.EQ.37
     -)THEN
      IF(
     - NRUN.GE.2303
     -)THEN
                LBCELL = LOR(LBCELL,2)
                JHIT = 16
                NHIT = 16
      ENDIF
      GOTO 16011
      ENDIF
      IF(
     - JCELL.EQ.65 .OR. JCELL.EQ.66 .OR.
     ?         JCELL.EQ.81 .OR. JCELL.EQ.82
     -)THEN
      IF(
     - NRUN.GE.2783
     -)THEN
                LBCELL = LOR(LBCELL,4)
                JHIT = 16
                NHIT = 16
      ENDIF
      GOTO 16011
      ENDIF
      IF(.NOT.(
     - .TRUE.
     -))GOTO 16010
16011 CONTINUE
      ENDIF
      GOTO IZZZ03
17012 CONTINUE
      XST  = WRK(IHSTRT+ 3)
      YST  = WRK(IHSTRT+ 4)
      XEN  = WRK(IHEND + 3)
      YEN  = WRK(IHEND + 4)
      TGST = PAR1*XST*2 + PAR2
      DXST = 1./SQRT(TGST**2+1.)
      DYST = DXST * TGST
      TGEN = PAR1*XEN*2 + PAR2
      DXEN = 1./SQRT(TGEN**2+1.)
      DYEN = DXEN * TGEN
      XMIN = -PAR2*.5 / PAR1
      YMIN = (PAR1*XMIN + PAR2)*XMIN + PAR3
      CURV =-PAR1 * 2.
      ZV0  = ADATA(IPTR+31)
      TGTH = ADATA(IPTR+30)
      CSTH = 1./SQRT(TGTH**2+1.)
      SNTH = CSTH * TGTH
      IP    = HPTR0 - 1
      IWRK(IP+ 1) = ITRK
      IWRK(IP+ 2) = 32
      IWRK(IP+ 3) = IDAY
      IWRK(IP+ 4) = 8
      WRK (IP+ 5) = XST *CSROT - YST *SNROT
      WRK (IP+ 6) = XST *SNROT + YST *CSROT
      WRK (IP+ 7) = SQRT(WRK(IP+ 5)**2 + WRK(IP+ 6)**2) * TGTH + ZV0
      WRK (IP+ 8) = (DXST*CSROT - DYST*SNROT)*CSTH
      WRK (IP+ 9) = (DXST*SNROT + DYST*CSROT)*CSTH
      WRK (IP+10) = SNTH
      IWRK(IP+11) = 0
      WRK (IP+12) = XEN *CSROT - YEN *SNROT
      WRK (IP+13) = XEN *SNROT + YEN *CSROT
      WRK (IP+14) = SQRT(WRK(IP+12)**2 + WRK(IP+13)**2) * TGTH + ZV0
      WRK (IP+15) = (DXEN*CSROT - DYEN*SNROT)*CSTH
      WRK (IP+16) = (DXEN*SNROT + DYEN*CSROT)*CSTH
      WRK (IP+17) = SNTH
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
      GOTO IZZZ06
17019 CONTINUE
      XST  = WRK(IHSTRT+ 3)
      YST  = WRK(IHSTRT+ 4)
      TGST = PAR1*XST*2 + PAR2
      DXST = 1./SQRT(TGST**2+1.)
      DYST = DXST * TGST
      XMIN = -PAR2*.5 / PAR1
      YMIN = (PAR1*XMIN + PAR2)*XMIN + PAR3
      CURV =-PAR1 * 2.
      ZV0  = ADATA(IPTR+31)
      TGTH = ADATA(IPTR+30)
      CSTH = 1./SQRT(TGTH**2+1.)
      SNTH = CSTH * TGTH
      IP    = HPTR0 - 1
      IWRK(IP+ 4) = 16
      WRK (IP+ 5) = XST *CSROT - YST *SNROT
      WRK (IP+ 6) = XST *SNROT + YST *CSROT
      WRK (IP+ 7) = SQRT(WRK(IP+ 5)**2 + WRK(IP+ 6)**2) * TGTH + ZV0
      WRK (IP+ 8) = (DXST*CSROT - DYST*SNROT)*CSTH
      WRK (IP+ 9) = (DXST*SNROT + DYST*CSROT)*CSTH
      WRK (IP+10) = SNTH
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
17008 CONTINUE
        IWL = -999
      DO 13004 IP = HPCO0,HPCO9,HLDCO
          IW0 = IWRK(IP)
          X   = WRK(IP+3)
          Y   = WRK(IP+4)
          F   = (PAR1*X + PAR2)*X + PAR3
          DF  = F - Y
          LBGOOD = 4
          IF(ABS(DF).LT.ALBLM2) LBGOOD = 1
          IF(ABS(DF).LT.ALBLM1) LBGOOD = 0
          IWRK(IP+ 7) = LBGOOD
          WRK (IP+13) = DF
      IF(
     - IWL.EQ.IW0
     -)THEN
      IF(
     - ABS(DFL).LT.ABS(DF)
     -)THEN
              IWRK(IP +7) = 16
      ELSE
              IWRK(IPL+7) = 16
      ENDIF
      ENDIF
          IWL = IW0
          IPL = IP
          DFL = DF
13004 CONTINUE
13005 CONTINUE
      GOTO IZZZ05
17000 CONTINUE
        IQJETC = IBLN('JETC')
        IQHEAD = IBLN('HEAD')
        IWRK(IP+ 3) = IDAY
        CALL DAY2(DATE)
        IDAY = DATE(1)*1000 + DATE(2)
        DRC = RINCR(1)*.5 * DRICOS
      IF(
     - NRUN.LE.100
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
