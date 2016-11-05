      SUBROUTINE JFXYRZ(IPTR,NH1,LRFLAG,NSAME,ICJ,XYRZ)
      IMPLICIT INTEGER*2 (H)
#include "cdata.for"
#include "calibr.for"
#include "cjdrch.for"
#include "cdsmax.for"
      DIMENSION XYRZ(6)
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
      DATA LBINIT /0/
      IF(
     - LBINIT .EQ. 0
     -)THEN
        LBINIT = 1
      ASSIGN 17001 TO IZZZ01
      GOTO 17000
17001 CONTINUE
      ENDIF
      IPHEAD = IDATA(IQHEAD)*2
      NRUN = HDATA(IPHEAD+10)
      ITRK = IDATA(IPTR+1)
      IF(
     - NSAME.EQ.0
     -)THEN
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
        TGTH = ADATA(IPTR+30)
        CSTHI = SQRT(TGTH**2 + 1.)
        CSTH  = 1. / CSTHI
        SNTH  = CSTH * TGTH
      ENDIF
                 JCELL=ICJ
      IF(
     - JCELL.NE.JCOLD
     -)THEN
                    JCOLD=JCELL
                    JRING=1
                    IF(JCELL.GT.24) JRING=2
                    IF(JCELL.GT.48) JRING=3
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
      DO 13000 I1=1,NCOAR
            IDX = I1
      IF(
     - TANBET.LT.TGCOAR(IDX)
     -)THEN
      GOTO 13001
      ENDIF
13000 CONTINUE
13001 CONTINUE
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
      ENDIF
        IP=NH1
            IWIR = HDATA(IP)
            IWIR = ISHFTR(IWIR,3)
            ILAY = IAND(IWIR,15)
            IAMPL = HDATA(IP+1)
            IAMPR = HDATA(IP+2)
            DS =(HDATA(IP+3)) * DSBIN1
            X1   = ILAY * DR + R0
            Z1   = X1*TGTH + ZVERT
            DDS = (1222.9-ABS(Z1))*ABERR(1) + ABERR(6)*R1*CSTHI
            DGR = ((Z1/1222.9)**2 - 1.) * .075
            DSC =  DS - DDS + DS0
            Y1   = SWDEPL
            IF(IAND(ILAY,1).NE.0) Y1 =-Y1
            Y1   = (7.5-ILAY)*(CCST52*Z1+CCST51) - CCST62*Z1-CCST61 + Y100039200
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
            XXL  = DXL + X
            YYL  = DYL + Y
            XXR  = DXR + X
            YYR  = DYR + Y
      IF(
     - LRFLAG.EQ.1
     -)THEN
         XYRZ(1)=XXR
         XYRZ(2)=YYR
         XYRZ(3)=XXL
         XYRZ(4)=YYL
      ELSE
         XYRZ(1)=XXL
         XYRZ(2)=YYL
         XYRZ(3)=XXR
         XYRZ(4)=YYR
      ENDIF
      XYRZ(5)=SQRT(XYRZ(1)**2+XYRZ(2)**2)
      XYRZ(6)=ZVERT+TGTH*XYRZ(5)
      RETURN
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
      DO 13002 I1=2,I9
          IF(I1.GT.2) TGCOAR(I1   ) = TGCOAR(I1- 1) + DTGB
          SLCOAR(I1   ) = (T0COAR(I1   )-T0COAR(I1- 1)) / DTGB
          SLCOAR(I1+15) = (T0COAR(I1+15)-T0COAR(I1+14)) / DTGB
          SLCOAR(I1+30) = (T0COAR(I1+30)-T0COAR(I1+29)) / DTGB
          SLCOAR(I1+45) = (T0COAR(I1+45)-T0COAR(I1+44)) / DTGB
13002 CONTINUE
13003 CONTINUE
      GOTO IZZZ01
      END
