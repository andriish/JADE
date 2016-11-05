      SUBROUTINE JFETCH(IPTR,IPJHTL,WRK,LHIT,IPRES,INDEX,XO,YO)
      IMPLICIT INTEGER*2 (H)
      DIMENSION WRK(200)
      EQUIVALENCE (ZWZ,IZW)
      COMMON/JSCALD/ JESCAL,JESKEY
#include "cdata.for"
#include "calibr.for"
#include "cjdrch.for"
#include "cdsmax.for"
      COMMON/CCMZCT/ DIMPCT, ZCUTV, ZCUTVV, IZVCST(5), ZCHWW
      DIMENSION IZCHMB(3,2),AZCHMB(3,2)
      COMMON/XYFVT1/MODXYV,NOVL
***PMF      DIMENSION IRESAR(13),RESAR(13),HRESAR(26)
      DIMENSION IRESAR(14),RESAR(14),HRESAR(28)
      EQUIVALENCE (IRESAR(1),RESAR(1),HRESAR(1))
      DATA NCOAR / 15/, DTGB / .15/
      REAL TGCOAR(15) /-99.,-.45, 12*0., 99./
      REAL T0COAR(60) / .000, .000, .000, .000, .000,
     ,     .000, .000,-.020,-.060,-.130,-.030, .100, .200, .200, .200,
     ,                  .000, .000, .010, .110, .100,
     ,     .075, .050, .025, .005, .015, .065, .060, .060, .060, .060,
     ,                  .190, .190, .180, .165, .140,
     ,     .120, .100, .075, .050, .010,-.050,-.075,-.035, .000, .000,
     ,                  .110, .110, .115, .140, .135,
     ,     .085, .045, .030, .040, .050, .055, .055, .055, .055, .055/
      REAL SLCOAR(60) / 60*0./
      INTEGER MKLRT1 /Z'1000000'/, MKLRT2 /Z'100'/
      IF(
     - JESCAL .GT. 0   .AND.   JESKEY .EQ. 54321
     -)THEN
         IP8=ICALIB(13)+6
         IP9=ICALIB(13)+2598
         CALL JFTNEW(IPTR,IPJHTL,WRK,LHIT,IPRES,INDEX,XO,YO,
     +   ACALIB(IP8),ACALIB(IP9))
         RETURN
      ENDIF
      INDX=INDEX
      IF(INDX.EQ.4) INDX=1
      DATA LBINIT /0/
      IF(
     - LBINIT .EQ. 0
     -)THEN
        LBINIT = 1
        NCALL = 0
      ASSIGN 17001 TO IZZZ01
      GOTO 17000
17001 CONTINUE
        PRINT 7777
 7777   FORMAT(' JFETCH : VERSION FROM 13/01/88 CALLED!',/,
     *         ' +++++++++++++++++++++++++++++++++++++++++++++++++++++')
      ENDIF
      NCALL = NCALL + 1
      LHBIT  = LHIT*4
      IPCO = 1
      IPHEAD = IDATA(IQHEAD)*2
      NRUN = HDATA(IPHEAD+10)
      NEVT = HDATA(IPHEAD+11)
      ITRK = IDATA(IPTR+1)
      IF(
     - IDATA(IPTR+18).EQ.1
     -)THEN
        ALFA  = ADATA(IPTR+21)
        CRV   = ADATA(IPTR+19)
        IF(ABS(CRV).LT.1.E-8) CRV = SIGN(1.E-8,CRV)
        RAD   =  1./ABS(CRV) + ADATA(IPTR+20)
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
      TGTH = ADATA(IPTR+30)
      CSTHI = SQRT(TGTH**2 + 1.)
      CSTH  = 1. / CSTHI
      SNTH  = CSTH * TGTH
      IZZZSE=INDX
      GOTO 14002
14003 CONTINUE
        XT = 0.
        YT = 0.
        CSROT = 1.
        SNROT = 0.
        XOT   = 0.
        YOT   = 0.
      GOTO 14001
14004 CONTINUE
        XT    = (ADATA(IPTR+12) + ADATA(IPTR+5)) * .5
        YT    = (ADATA(IPTR+13) + ADATA(IPTR+6)) * .5
        XX    =  ADATA(IPTR+12) - ADATA(IPTR+5)
        YY    =  ADATA(IPTR+13) - ADATA(IPTR+6)
        RR    = SQRT(XX**2+YY**2)
      IF(
     - RR.LT.10.
     -)THEN
           IPRES=IPCO
           RETURN
      ENDIF
        CSROT = XX / RR
        SNROT = YY / RR
        XX    = XO - XT
        YY    = YO - YT
        XOT   = 0.
        YOT   = 0.
      GOTO 14001
14005 CONTINUE
        XT    = (ADATA(IPTR+12) + XO) * .5
        YT    = (ADATA(IPTR+13) + YO) * .5
        XX    =  ADATA(IPTR+12) - XO
        YY    =  ADATA(IPTR+13) - YO
        RR    = SQRT(XX**2+YY**2)
        CSROT = XX / RR
        SNROT = YY / RR
        XX    = XO - XT
        YY    = YO - YT
        XOT   = XX*CSROT + YY*SNROT
        YOT   =-XX*SNROT + YY*CSROT
      GOTO 14001
14002 CONTINUE
      GOTO(
     -14003,14004,14005
     -), IZZZSE
14000 CONTINUE
        RETURN
14001 CONTINUE
      NOVL = 0
      IPC0 = IPTR + 34
      IPC9 = IPC0 +  5
      DO 13000 IPC = IPC0,IPC9
         JCELL = IDATA(IPC)
      IF(
     - JCELL.GT. 0 .AND. JCELL.LE.96
     -)THEN
            JRING = 1
            IF(JCELL.GT.24) JRING = 2
            IF(JCELL.GT.48) JRING = 3
      ASSIGN 17003 TO IZZZ02
      GOTO 17002
17003 CONTINUE
      ENDIF
13000 CONTINUE
13001 CONTINUE
      IF(
     - INDEX.EQ.4 .AND. ZCHWW.GT..1 .AND. ZCHWW.LT.2000.
     -)THEN
         CALL ZCFTNW(NRUN,NEVT,ITRK,TGTH,ZVERT,NZHIT,IZCHMB,AZCHMB)
      IF(
     - NZHIT.GT.0
     -)THEN
      DO 13002 J=1,NZHIT
               HRESAR( 1) = 100+IZCHMB(1,J)
               HRESAR( 2) = IZCHMB(2,J)
               HRESAR( 3) = 0
               HRESAR( 4) = 0
               HRESAR( 5) = 1
               HRESAR( 6) = 101
               XX=AZCHMB(1,J)
               YY=AZCHMB(2,J)
               ZZ=AZCHMB(3,J)
               RESAR ( 4) = XX
               RESAR ( 5) = YY
               RESAR ( 6) = ZZ
               UX=XX-ADATA(IPTR+5)
               UY=YY-ADATA(IPTR+6)
               UU=SQRT(UX**2+UY**2)
            IF(ABS(CURVXY*UU).GT.1.E-5) UU=2.*SIN(.5*CURVXY*UU)/CURVXY
               IF(UX*ADATA(IPTR+8)+UY*ADATA(IPTR+9).LT.0.) UU=-UU
               RESAR ( 7) = UU
               RESAR ( 8) = ZCHWW
               IF(NRUN.LT.24200) RESAR(8)=RESAR(8)*.6
               CALL MVC(WRK(IPCO),0,RESAR(1),0,LHBIT)
               IPCO = IPCO + LHIT
13002 CONTINUE
13003 CONTINUE
      ENDIF
      ENDIF
      IPRES = IPCO
      IF(
     - INDEX.NE.4
     -)THEN
         WRK (IPRES   ) = XT
         WRK (IPRES+ 1) = YT
         WRK (IPRES+ 2) = CSROT
         WRK (IPRES+ 3) = SNROT
         WRK (IPRES+ 9) = XOT
         WRK (IPRES+10) = YOT
         WRK (IPRES+11) = CSTH
         WRK (IPRES+12) = SNTH
      ENDIF
      RETURN
17002 CONTINUE
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
        TANBET = (DRITG - TGB) / (TGB*DRITG + 1.)
      IF(
     - NRUN.LE.100
     -)THEN
          DS0 = DSBIN1*.5
          T0CORR = 0.
      ELSE
          DS0 = T0FIX(JRING)*DSBIN1*64.
      DO 13004 I1=1,NCOAR
            IDX = I1
      IF(
     - TANBET.LT.TGCOAR(IDX)
     -)THEN
      GOTO 13005
      ENDIF
13004 CONTINUE
13005 CONTINUE
          KRNG = JRING
          IF(KRNG.EQ.3 .AND. lAND(JCELL,1).EQ.0) KRNG = 4 ! PMF 11/06/99: land
          IBIN = (KRNG-1)*NCOAR  + IDX
          T0CORR = (TANBET-TGCOAR(IDX)) * SLCOAR(IBIN) + T0COAR(IBIN)
      ENDIF
        IPJCOR = ICALIB(5) + JCELL
        CCST01 = ACALIB(IPJCOR     ) * ABS(TANBET)
        CCST02 = ACALIB(IPJCOR+  96) * ABS(TANBET)
        CCST11 = ACALIB(IPJCOR+ 192)
        CCST12 = ACALIB(IPJCOR+ 288)
        CCST21 = ACALIB(IPJCOR+ 384)
        CCST22 = ACALIB(IPJCOR+ 480)
        CCST51 = ACALIB(IPJCOR+ 576) * 10.
        CCST52 = ACALIB(IPJCOR+ 672) / 121.15
        CCST61 = ACALIB(IPJCOR+ 768) * 10.
        CCST62 = ACALIB(IPJCOR+ 864) / 121.15
        CCST81 = ACALIB(IPJCOR+1152)
        JHIT = 0
        NHIT   = 0
        NHGOOD = 0
        ILAYL =-99
        IPCO = IPCO - LHIT
        IPJET4 = IDATA(IQJETC)
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
            L0GOOD = 0
      IF(
     - ITR1.NE.0 .AND. ITR2.NE.0
     -)THEN
               NOVL   = NOVL + 1
               L0GOOD = 11
               ID1    = LAND(ISHFTR(LB,27), 31)
               ID2    = LAND(ISHFTR(LB,11), 31)
      IF(
     - ITR1 .EQ. ITRK
     -)THEN
                  IF( ID1 .LT. ID2 ) L0GOOD = 2
                  IF( ID1 .GT. ID2 ) L0GOOD = 12
      ELSE
                  IF( ID2 .LT. ID1 ) L0GOOD = 2
                  IF( ID2 .GT. ID1 ) L0GOOD = 12
      ENDIF
      ENDIF
            LBLR = 0
            IF(ITR1.EQ.ITRK) LBLR = LAND(LB,MKLRT1)
            IF(ITR2.EQ.ITRK) LBLR = LAND(LB,MKLRT2)
            LBSIDE =-1
            IF(LBLR.NE.0) LBSIDE = 1
            LBLR = LBSIDE
            IWIR = HDATA(IP)
            IWIR = ISHFTR(IWIR,3)
            ILAY = LAND(IWIR,15)
            IAMPL = HDATA(IP+1)
            IAMPR = HDATA(IP+2)
            DS =(HDATA(IP+3)) * DSBIN1
            X1   = ILAY * DR + R0
            Z1   = X1*TGTH + ZVERT
            DDS = (1222.9-ABS(Z1))*ABERR(1) + ABERR(6)*R1*CSTHI
      IF(
     - NRUN.LE.100
     -)THEN
              DGR = 0.0
      ELSE
              DGR = ((Z1/1222.9)**2 - 1.) * .075
      ENDIF
            DSC =  DS - DDS + DS0
            Y1   = SWDEPL
            IF(LAND(ILAY,1).NE.0) Y1 =-Y1
            Y1   = (7.5-ILAY)*(CCST52*Z1+CCST51) - CCST62*Z1-CCST61 + Y100046600
            X    = X1*CSROT0 - Y1*SNROT0
            Y    = X1*SNROT0 + Y1*CSROT0 - DGR
      IF(
     - DSC.LE.DRC
     -)THEN
      IF(
     - DSC.LT.DSD2
     -)THEN
      IF(
     - DSC.LT.DSD1
     -)THEN
                  DSC = DSC + DDS1 + (DSC-DSD1)*DRV1
      ELSE
                  DSC = DSC + DDS2 + (DSC-DSD2)*DRV2
      ENDIF
                IF(DSC.LT.0.1) DSC = 0.1
      ELSE
                DSC = (DSC-DSD2)/(DRC-DSD2) * T0CORR + DSC
      ENDIF
              DXR  = DSC * CSB
              DYR  = DSC * SNB
              DXL =-DXR
              DYL =-DYR
      ELSE
              DSC = DSC + T0CORR
      IF(
     - ILAY.LT. 3
     -)THEN
                DILAY =-(ILAY- 3)**2
                DSCL  = (DILAY*CCST11 + 1.) * DSC * (1. - CCST81)
                DSCR  = (DILAY*CCST12 + 1.) * DSC * (1. + CCST81)
      ELSE
      IF(
     - ILAY.GT.12
     -)THEN
                DILAY =-(ILAY-12)**2
                DSCL  = (DILAY*CCST21 + 1.) * DSC * (1. - CCST81)
                DSCR  = (DILAY*CCST22 + 1.) * DSC * (1. + CCST81)
      ELSE
                DSCL = DSC * (1. - CCST81)
                DSCR = DSC * (1. + CCST81)
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
            XL   = DXL + X - XT
            YL   = DYL + Y - YT
            XXL  = XL*CSROT + YL*SNROT
            YYL  =-XL*SNROT + YL*CSROT
            XR   = DXR + X - XT
            YR   = DYR + Y - YT
            XXR  = XR*CSROT + YR*SNROT
            YYR  =-XR*SNROT + YR*CSROT
            CALL AMPS2Z( IP,IPJET4,ZZ,WW,LZGOOD)
            NLRSOL = 1
            IF(DSC.LT.2.0) NLRSOL = 2
            ILRSOL = 0
16000 CONTINUE
            ILRSOL = ILRSOL + 1
            LBGOOD = L0GOOD
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
              IF(LBSIDE.NE.LBLR) LBGOOD = LBGOOD + 1
      IF(
     - ILAY.NE.ILAYL .OR. LBGDL.LE.1.AND.LBGOOD.LE.2
     -)THEN
                LBREG = 1
                JHIT = JHIT + 1
                IPCO = IPCO + LHIT
      ELSE
                LBREG = 0
                ZWZ = WRK(IPCO+10)
                IF(LBGOOD.LT.IZW) LBREG = 1
      ENDIF
      IF(
     - LBREG.NE.0
     -)THEN
                NHIT   = NHIT   + 1
                IF(LBGOOD.LE.2) NHGOOD = NHGOOD + 1
      IF(
     - INDEX.NE.4
     -)THEN
                   IRESAR( 1) = ILAY
                   IRESAR( 2) = IP
                   IRESAR( 3) = LBSIDE
                   RESAR ( 4) = XX
                   RESAR ( 5) = YY
                   RESAR ( 6) = ZZ
                   RESAR ( 7) = XX - XOT
                   IF(INDX.EQ.1) RESAR ( 7) = SQRT(XX**2 + YY**2)
                   IRESAR( 8) = LZGOOD
                   RESAR ( 9) = DSC
                   IRESAR(10) = JCELL
                   IRESAR(11) = LBGOOD
                   RESAR (12) = TANBET
                   IRESAR(13) = JRING
                   RESAR (14) = 0.
      ELSE
                   HRESAR( 1) = JCELL
                   HRESAR( 2) = ILAY
                   HRESAR( 3) = LZGOOD
                   HRESAR( 4) = LBGOOD
                   HRESAR( 5) = 1
                   HRESAR( 6) = IP-IPJETC
                   RESAR ( 4) = XX
                   RESAR ( 5) = YY
                   RESAR ( 6) = ZZ
                   UX=XX-ADATA(IPTR+5)
                   UY=YY-ADATA(IPTR+6)
                   UU=SQRT(UX**2+UY**2)
                   CURVXY=ADATA(IPTR+25)
                   IF(ABS(CURVXY).LT.1.E-8) CURVXY = SIGN(1.E-8,CURVXY)
            IF(ABS(CURVXY*UU).GT.1.E-5) UU=2.*SIN(.5*CURVXY*UU)/CURVXY
                   IF(UX*ADATA(IPTR+8)+UY*ADATA(IPTR+9).LT.0.) UU=-UU
                   RESAR ( 7) = UU
                   RESAR ( 8) = WW
      ENDIF
                CALL MVC(WRK(IPCO),0,RESAR(1),0,LHBIT)
                ILAYL = ILAY
                LBGDL = LBGOOD
      ENDIF
      IF(.NOT.(
     - ILRSOL.GE.NLRSOL
     -))GOTO 16000
16001 CONTINUE
      ENDIF
        IPHL = IPHL + 1
        IP   = IP   + 4
      GOTO 15000
      ENDIF
15001 CONTINUE
        IPCO = IPCO + LHIT
      GOTO IZZZ02
17000 CONTINUE
        IQJETC = IBLN('JETC')
        IQHEAD = IBLN('HEAD')
        DRC = RINCR(1)*.5 * DRICOS
        IPHEAD = IDATA(IQHEAD)*2
        NRUN = HDATA(IPHEAD+10)
      IF(
     - NRUN.LE.100
     -)THEN
          DSD0   = .0
          DSD1   = .0
          DSD2   = .0
          DDS0   = .0
          DDS1   = .0
          DDS2   = .0
          DRV1   = .0
          DRV2   = .0
      ELSE
          DSD0   =-0.400
          DSD1   = 0.300
          DSD2   = 2.500
          DDS0   = 0.720
          DDS1   = 0.330
          DDS2   = 0.0
          DRV1   = (DDS0-DDS1) / (DSD0-DSD1)
          DRV2   = (DDS1-DDS2) / (DSD1-DSD2)
      ENDIF
        I9 = NCOAR - 1
      DO 13006 I1=2,I9
          IF(I1.GT.2) TGCOAR(I1   ) = TGCOAR(I1- 1) + DTGB
          SLCOAR(I1   ) = (T0COAR(I1   )-T0COAR(I1- 1)) / DTGB
          SLCOAR(I1+15) = (T0COAR(I1+15)-T0COAR(I1+14)) / DTGB
          SLCOAR(I1+30) = (T0COAR(I1+30)-T0COAR(I1+29)) / DTGB
          SLCOAR(I1+45) = (T0COAR(I1+45)-T0COAR(I1+44)) / DTGB
13006 CONTINUE
13007 CONTINUE
      GOTO IZZZ01
      END
