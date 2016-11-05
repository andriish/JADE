      SUBROUTINE FTRELO
      IMPLICIT INTEGER*2 (H)
      LOGICAL TBIT
#include "cheadr.for"
      EQUIVALENCE (HRUN,HHEADR(18)) , (HEV,HHEADR(19))
#include "cworkpr.for"
#include "cworkeq.for"
      EQUIVALENCE
     ,           (ICELL ,IDWRK(1)),(MHIT  ,IDWRK(2)),(IRING ,IDWRK(3))
     ,         , (IERRCD,IDWRK(4)),(NTRKEL,IDWRK(5))
#include "cpatlm.for"
#include "cjdrch.for"
#include "cdsmax.for"
      DIMENSION HPBAK(16),HLYAR(16),HLBAR(16),DSPAR(16),
     ,          XTRAR(16),YTRAR(16),RESAR(16)
      DIMENSION HST( 80), HSTL(5),HSTR(5)
      DATA XCV /152.7/
      REAL SGCRV(3) /.000062,.000035,.000025/
      ITRK = 0
      DR =  RINCR(IRING)
      R0 = FSENSW(IRING)
      DRC = RINCR(1)*.5 * DRICOS
      DO 13000 IP = HPHT0,HPHT9,HLDHT
        ILAY = IWRK(IP  )
        Y    = SWDEPL
        IF(LAND(ILAY,1).NE.0) Y =-Y
        X    = ILAY * DR + R0
        DS   =  WRK(IP+2)
      IF(
     - DS.LE.DRC
     -)THEN
          DX   = 0.
          DY   = DS
      ELSE
          DX   =-(DS-DRC)*DRISIN
          DY   = (DS-DRC)*DRICOS + DRC
      ENDIF
        WRK(IP+5) = X - DX
        WRK(IP+7) = X + DX
        WRK(IP+6) = Y - DY
        WRK(IP+8) = Y + DY
13000 CONTINUE
13001 CONTINUE
      IPHT1 = HPHT9 - HLDHT + 1
16000 CONTINUE
        IL1 = IWRK(IPHT1  )
      IF(
     - IL1.LT. 2
     -)THEN
      GOTO 16001
      ENDIF
        DSP1 =  WRK(IPHT1+2)
      IF(
     - DSP1.GT.0.7 .AND. IWRK(IPHT1+9).LE.0
     -)THEN
          IL2 =  IL1 - 1
          IL3 =  IL2 - 1
          DSLM = 1000.
          IPHT2 = IPHT1 - HLDHT
15000 CONTINUE
      IF(
     - IPHT2.GE.HPHT0 .AND. IWRK(IPHT2).GE.IL2
     -)THEN
            DSP2 = WRK(IPHT2+2)
      IF(
     - DSP2.GT.0.7.AND.IWRK(IPHT2).EQ.IL2.AND.IWRK(IPHT2+9).LE.0
     -)THEN
      IF(
     - ABS(DSP2-DSP1).LE.12.5
     -)THEN
                IPHT3 = IPHT2 - HLDHT
15002 CONTINUE
      IF(
     - IPHT3.GE.HPHT0 .AND. IWRK(IPHT3).GE.IL3
     -)THEN
                  DSP3 = WRK(IPHT3+2)
      IF(
     - DSP3.GT.0.7 .AND.
     ?               IWRK(IPHT3).EQ.IL3 .AND. IWRK(IPHT3+9).LE.0
     -)THEN
                    DSL = DSP2*2.-DSP3-DSP1
      IF(
     - ABS(DSL).LT.DSLM .AND. DSP1+DSP2+DSP3.GT.3.0
     -)THEN
      IF(
     - IWRK(IPHT1+9).EQ.0 .OR. IWRK(IPHT2+9).EQ.0
     ?                   .OR. IWRK(IPHT3+9).EQ.0
     -)THEN
                        DSLM  = ABS(DSL)
                        SDSLM = DSL
                        MP1 = IPHT1
                        MP2 = IPHT2
                        MP3 = IPHT3
      ENDIF
      ENDIF
      ENDIF
                IPHT3 = IPHT3 - HLDHT
      GOTO 15002
      ENDIF
15003 CONTINUE
      ENDIF
      ENDIF
          IPHT2 = IPHT2 - HLDHT
      GOTO 15000
      ENDIF
15001 CONTINUE
16002 CONTINUE
            LBLRLE = LAND(IL2,1)*2 - 1
            IF(SDSLM.LT.0) LBLRLE =-LBLRLE
      IF(
     - DSLM.GE.2.0
     -)THEN
      GOTO 16003
      ENDIF
            LBLRLE = LAND(IL2,1)*2 - 1
            IF(SDSLM.LT.0) LBLRLE =-LBLRLE
      ASSIGN 17001 TO IZZZ01
      GOTO 17000
17001 CONTINUE
            LRLOOP = 1
            SIGLM = .250
            SIG0 = 1000.
15004 CONTINUE
      IF(
     - LRLOOP.LE.2
     -)THEN
      ASSIGN 17003 TO IZZZ02
      GOTO 17002
17003 CONTINUE
      IF(
     - NHTTR.LT.4
     -)THEN
      GOTO 15005
      ENDIF
              WGHT0 = 0.01
              Y0    = 0.
      ASSIGN 17005 TO IZZZ03
      GOTO 17004
17005 CONTINUE
      IF(
     - ABS(PAR3).GT.3.0
     -)THEN
                WGHT0 = 0.1
      IF(
     - PAR1*PAR3.LE.0.
     -)THEN
                  Y0 =-SIGN(2.,PAR1)
      ASSIGN 17006 TO IZZZ03
      GOTO 17004
17006 CONTINUE
      ELSE
                  DORMX = XCV**2*PAR1 + SIGN(2.,PAR1)
      IF(
     - ABS(PAR3).GT.ABS(DORMX)
     -)THEN
                    Y0 = DORMX
      ASSIGN 17007 TO IZZZ03
      GOTO 17004
17007 CONTINUE
      ENDIF
      ENDIF
      ENDIF
              SIGLM = .50
      IF(
     - SIG.GT.SIGLM
     -)THEN
                SIG1 = (SIG*DEG - DCHIM1**2) / (DEG-1.)
      IF(
     - SIG1.GT.SIGLM .AND. LRLOOP.EQ.1
     -)THEN
      GOTO 16003
      ENDIF
      ENDIF
              FETLIM = 0.65
      ASSIGN 17009 TO IZZZ04
      GOTO 17008
17009 CONTINUE
              WGHT0 = .01
      IF(
     - NHTTR.GE.4
     -)THEN
      ASSIGN 17010 TO IZZZ03
      GOTO 17004
17010 CONTINUE
      IF(
     - ABS(PAR3).GT.3.0
     -)THEN
                  WGHT0 = 0.1
      IF(
     - PAR1*PAR3.LE.0.
     -)THEN
                    Y0 =-SIGN(2.,PAR1)
      ASSIGN 17011 TO IZZZ03
      GOTO 17004
17011 CONTINUE
      ELSE
                    DORMX = XCV**2*PAR1 + SIGN(2.,PAR1)
      IF(
     - ABS(PAR3).GT.ABS(DORMX)
     -)THEN
                      Y0 = DORMX
      ASSIGN 17012 TO IZZZ03
      GOTO 17004
17012 CONTINUE
      ENDIF
      ENDIF
      ENDIF
      ENDIF
      IF(
     - SIG.GT.SIGLM
     -)THEN
                SIG1 = (SIG*DEG - DCHIM1**2) / (DEG-1.)
      IF(
     - SIG1.GT.SIGLM .AND. LRLOOP.EQ.1
     -)THEN
      GOTO 16003
      ENDIF
      ENDIF
              SIGCOR = SIG
      IF(
     - LRLOOP.EQ.2
     -)THEN
                DDSTG = DSTAG - DSTAG0
                SIGCOR = SIG - (DEG-DEG0)*.01 + DDSTG*.1 + .01
      ENDIF
      IF(
     - SIGCOR .LT. SIG0
     -)THEN
                LBLR0 = LBLRTR
                SIG0  = SIG
                SIGLM0= SIGLM
                PAR10 = PAR1
                PAR20 = PAR2
                PAR30 = PAR3
                DSTAG0= DSTAG
                DEG0  = DEG
      IF(
     - SIG.LT..08
     -)THEN
      GOTO 15005
      ENDIF
      ENDIF
            LBLRTR =-LBLRTR
            LRLOOP = LRLOOP + 1
      GOTO 15004
      ENDIF
15005 CONTINUE
      IF(
     - SIG0.GT..25
     -)THEN
      GOTO 16003
      ENDIF
            LBLRTR = LBLR0
            SIG    = SIG0
            PAR1   = PAR10
            PAR2   = PAR20
            PAR3   = PAR30
            ITRK = ITRK + 1
            ALBLM1 = 0.7
            ALBLM2 = 2.0
      ASSIGN 17014 TO IZZZ05
      GOTO 17013
17014 CONTINUE
      IF(
     - SIG0.GT..10 .AND. ICELL.LE.48
     -)THEN
      GOTO 16003
      ENDIF
      IF(
     - ABS(PAR1).GT..00040 .OR. NHTGD.LT.4
     -)THEN
      GOTO 16003
      ENDIF
            HPFRE1 = HPFREE
            CALL TRACEO(ITRK,PAR1,PAR2,PAR3)
            HPFREE = HPFRE1
      IF(.NOT.(
     - .TRUE.
     -))GOTO 16002
16003 CONTINUE
      ENDIF
      IPHT1 = IPHT1 - HLDHT
      IF(.NOT.(
     - IPHT1.LT.HPHT0
     -))GOTO 16000
16001 CONTINUE
      RETURN
17000 CONTINUE
      CALL SETS(HST(1),0,160,0)
      DCRV =  SGCRV(IRING)
      CRV1 = -DCRV*20.
      IW1 = IL2
      ILRSOL =-3
16004 CONTINUE
      ILRSOL = ILRSOL + 2
      IF(
     - ILRSOL.LT.0
     -)THEN
          ZW1L = (WRK(MP1+5)+WRK(MP2+5)+WRK(MP3+5)) / 3.
          ZW2L = (WRK(MP1+6)+WRK(MP2+6)+WRK(MP3+6)) / (ZW1L*3.)
          ZW1  = ZW1L
          ZW2  = ZW2L
          X    = WRK(MP1+5)
          Y    = WRK(MP1+6)
          CV1  = (ZW2*X - Y) / ((ZW1-X)*X)
          X    = WRK(MP3+5)
          Y    = WRK(MP3+6)
          CV3  = (ZW2*X - Y) / ((ZW1-X)*X)
          CRVL = (CV1 + CV3) * .5 + CRV1
          CRV0 = CRVL
      ELSE
          ZW1R = (WRK(MP1+7)+WRK(MP2+7)+WRK(MP3+7)) / 3.
          ZW2R = (WRK(MP1+8)+WRK(MP2+8)+WRK(MP3+8)) / (ZW1R*3.)
          ZW1  = ZW1R
          ZW2  = ZW2R
          X    = WRK(MP1+7)
          Y    = WRK(MP1+8)
          CV1  = (ZW2*X - Y) / ((ZW1-X)*X)
          X    = WRK(MP3+7)
          Y    = WRK(MP3+8)
          CV3  = (ZW2*X - Y) / ((ZW1-X)*X)
          CRVR = (CV1 + CV3) * .5 + CRV1
          CRV0 = CRVR
      ENDIF
        CALL SETS(HST(1),0,160,0)
      DO 13002 IP0 = HPHT0,HPHT9,HLDHT
      IF(
     - IWRK(IP0).NE.IW1
     -)THEN
            X    = WRK(IP0+5)
            Y    = WRK(IP0+6)
            DX   = ZW1 - X
            IBNL = 1000
            IBNR = 1000
      IF(
     - ABS(DX).GT.5.0
     -)THEN
              CRV = (ZW2*X - Y) / (DX*X)
              IBNL = (CRV-CRV0) / DCRV + 1
      ENDIF
            X   = WRK(IP0+7)
            Y   = WRK(IP0+8)
            DX  = ZW1 - X
      IF(
     - ABS(DX).GT.5.0
     -)THEN
              CRV = (ZW2*X - Y) / (DX*X)
              IBNR = (CRV-CRV0) / DCRV + 1
      ENDIF
      IF(
     - IABS(IBNL-IBNR).GT.3
     -)THEN
              IF(IBNL.GT.0 .AND. IBNL.LE.40) HST(IBNL) = HST(IBNL) + 1
              IF(IBNR.GT.0 .AND. IBNR.LE.40) HST(IBNR) = HST(IBNR) + 1
      ENDIF
      ENDIF
13002 CONTINUE
13003 CONTINUE
        MHST = 0
        IMAX = 0
      DO 13004 I=19,22
          NHST = HST(I-1) + HST(I  ) + HST(I+1)
      IF(
     - NHST.GE.MHST
     -)THEN
            MHST = NHST
            IMAX = I
      ENDIF
13004 CONTINUE
13005 CONTINUE
      IF(
     - ILRSOL.LT.0
     -)THEN
          MHSTL = MHST
          IMAXL = IMAX
          CALL MVC2(HSTL(1),0,HST(IMAX-2),0,10) !PMF 28/06/99 MVC -> MVC2
      ELSE
          MHSTR = MHST
          IMAXR = IMAX
          CALL MVC2(HSTR(1),0,HST(IMAX-2),0,10) !PMF 28/06/99 MVC -> MVC2
      ENDIF
      IF(.NOT.(
     - ILRSOL.EQ.1
     -))GOTO 16004
16005 CONTINUE
      LBLRTR = 1
      IF(MHSTL.GT.MHSTR) LBLRTR =-1
      IF(IABS(MHSTL-MHSTR).LE.1 .AND. LBLRTR.NE.LBLRLE) LBLRTR=-LBLRTR
      GOTO IZZZ01
17002 CONTINUE
      IF(
     - LBLRTR.LT.0
     -)THEN
        MHST = MHSTL
        IMAX = IMAXL
        CALL MVC(HST(1),0,HSTL(1),0,10)
        ZW1 = ZW1L
        ZW2 = ZW2L
        CRV0 = CRVL
      ELSE
        MHST = MHSTR
        IMAX = IMAXR
        CALL MVC(HST(1),0,HSTR(1),0,10)
        ZW1 = ZW1R
        ZW2 = ZW2R
        CRV0 = CRVR
      ENDIF
      IF(
     - IMAX.EQ.19 .AND.
     ?   HST(1).GT.0 .AND. HST(1).GT.HST(2) .AND. HST(5).NE.0
     -)THEN
        IMAX = IMAX + 1
      ENDIF
      IF(
     - IMAX.EQ.22 .AND.
     ?   HST(5).GT.0 .AND. HST(5).GT.HST(4) .AND. HST(1).NE.0
     -)THEN
        IMAX = IMAX - 1
      ENDIF
      IM1 = IMAX - 1
      IM3 = IMAX + 1
      NHST  = HST(2) + HST(3) + HST(4)
      NHTTR = 0
      IF(
     - NHST.GE.3
     -)THEN
        IHIT = 0
        ILAYL = -1
        LBHDEL = 0
        CALL SETS(HLBAR(1),0,32,0)
      DO 13006 IP0 = HPHT0,HPHT9,HLDHT
          IBNL = -1000
          IBNR = -1000
      IF(
     - IABS(IWRK(IP0)-IW1).LE.1
     -)THEN
      IF(
     - LBLRTR.LT.0
     -)THEN
              IF(IP0.EQ.MP1.OR.IP0.EQ.MP2.OR.IP0.EQ.MP3) IBNL = IMAX
      ELSE
              IF(IP0.EQ.MP1.OR.IP0.EQ.MP2.OR.IP0.EQ.MP3) IBNR = IMAX
      ENDIF
      ELSE
            X   = WRK(IP0+5)
            Y   = WRK(IP0+6)
            DX  = ZW1 - X
      IF(
     - ABS(DX).GT.5.0
     -)THEN
              CRV = (ZW2*X - Y) / (DX*X)
              IBNL = (CRV-CRV0) / DCRV + 1
      ENDIF
            X   = WRK(IP0+7)
            Y   = WRK(IP0+8)
            DX  = ZW1 - X
      IF(
     - ABS(DX).GT.5.0
     -)THEN
              CRV = (ZW2*X - Y) / (DX*X)
              IBNR = (CRV-CRV0) / DCRV + 1
      ENDIF
      ENDIF
      IF(
     - IABS(IBNL-IBNR).GT.3
     -)THEN
            ILAY = IWRK(IP0)
      IF(
     - IBNL.GE.IM1 .AND. IBNL.LE.IM3
     -)THEN
      IF(
     - ILAY.EQ.ILAYL
     -)THEN
                IF(LBHDEL.EQ.0) IHIT = IHIT - 1
                LBHDEL = 1
      ELSE
                IHIT = IHIT + 1
                HPBAK(IHIT  ) = IP0
                HLYAR(IHIT  ) = ILAY
                HLBAR(IHIT  ) =   1
                DSPAR(IHIT  ) = WRK(IP0+2)
                XTRAR(IHIT  ) = WRK(IP0+5)
                YTRAR(IHIT  ) = WRK(IP0+6)
                ILAYL = ILAY
                LBHDEL = 0
      ENDIF
      ELSE
      IF(
     - IBNR.GE.IM1 .AND. IBNR.LE.IM3
     -)THEN
      IF(
     - ILAY.EQ.ILAYL
     -)THEN
                IF(LBHDEL.EQ.0) IHIT = IHIT - 1
                LBHDEL = 1
      ELSE
                IHIT = IHIT + 1
                HPBAK(IHIT  ) = IP0
                HLYAR(IHIT  ) = ILAY
                HLBAR(IHIT  ) =   1
                DSPAR(IHIT  ) = WRK(IP0+2)
                XTRAR(IHIT  ) = WRK(IP0+7)
                YTRAR(IHIT  ) = WRK(IP0+8)
                ILAYL = ILAY
                LBHDEL = 0
      ENDIF
      ENDIF
      ENDIF
      ENDIF
13006 CONTINUE
13007 CONTINUE
        NHTTR = IHIT
      ENDIF
      GOTO IZZZ02
17008 CONTINUE
        IHIT = 0
        IWL  =-1
        CALL SETS(HLBAR(1),0,32,0)
      DO 13008 IP0 = HPHT0,HPHT9,HLDHT
          IW0 = IWRK(IP0)
          X   = WRK(IP0+5)
          Y   = WRK(IP0+6)
          F   = (PAR1 *X + PAR2 )*X + PAR3
          DFL = F - Y
      IF(
     - ABS(DFL).LT.FETLIM
     -)THEN
      IF(
     - IWL.EQ.IW0
     -)THEN
              IHIT = IHIT - 1
      ELSE
              IHIT = IHIT + 1
              HPBAK(IHIT  ) = IP0
              HLYAR(IHIT  ) = IW0
              HLBAR(IHIT  ) =   1
              DSPAR(IHIT  ) = WRK(IP0+2)
              XTRAR(IHIT  ) = WRK(IP0+5)
              YTRAR(IHIT  ) =   Y
              IWL = IW0
      ENDIF
      ENDIF
          X   = WRK(IP0+7)
          Y   = WRK(IP0+8)
          F   = (PAR1 *X + PAR2 )*X + PAR3
          DF  = F - Y
      IF(
     - ABS(DF).LT.FETLIM
     -)THEN
      IF(
     - IWL.EQ.IW0
     -)THEN
              IHIT = IHIT - 1
      ELSE
              IHIT = IHIT + 1
              HPBAK(IHIT  ) = IP0
              HLYAR(IHIT  ) = IW0
              HLBAR(IHIT  ) =   1
              DSPAR(IHIT  ) = WRK(IP0+2)
              XTRAR(IHIT  ) = WRK(IP0+7)
              YTRAR(IHIT  ) =   Y
              IWL = IW0
      ENDIF
      ENDIF
13008 CONTINUE
13009 CONTINUE
        NHTTR = IHIT
      GOTO IZZZ04
17004 CONTINUE
      S0 = WGHT0
      S1 = 0.
      S2 = 0.
      S3 = 0.
      S4 = 0.
      S5 = 0.
      S6 = 0.
      S7 = Y0 * WGHT0
      DO 13010 IHIT = 1,NHTTR
          X = XTRAR(IHIT)
          Y = YTRAR(IHIT)
          X2 = X**2
          S1 = S1 + X
          S2 = S2 + X2
          S3 = S3 + X*X2
          S4 = S4 + X2**2
          S5 = S5 + Y*X2
          S6 = S6 + Y*X
          S7 = S7 + Y
          S0 = S0 + 1.
13010 CONTINUE
13011 CONTINUE
      MHIT = S0
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
      NEVN   = 0
      CHEVN  = 0.
      NUNE   = 0
      CHUNE  = 0.
      IHSTRT = 0
      DO 13012 IHIT = 1,NHTTR
          IF(IHSTRT.EQ.0) IHSTRT = IHIT
          IHEND = IHIT
          X = XTRAR(IHIT)
          Y = YTRAR(IHIT)
          F   = (PAR1 *X + PAR2 )*X + PAR3
          DCHI = Y - F
          RESAR(IHIT) = DCHI
          CHISQ = CHISQ + DCHI**2
      IF(
     - ABS(DCHI).GE.DCHIM1
     -)THEN
            DCHIM1 = ABS(DCHI)
            IHITM1 = IHIT
      ENDIF
          ILAY = HLYAR(IHIT)
      IF(
     - ABS(DCHI).LT. 0.6
     -)THEN
      IF(
     - LAND(ILAY,1).EQ.0
     -)THEN
              NEVN  = NEVN  + 1
              CHEVN = CHEVN + DCHI
      ELSE
              NUNE  = NUNE  + 1
              CHUNE = CHUNE + DCHI
      ENDIF
      ENDIF
13012 CONTINUE
13013 CONTINUE
      SIG    =      CHISQ  / DEG
      DSTAG = -1000.
      IF(NEVN.GT.1.AND. NUNE.GT.1) DSTAG = ABS(CHEVN/NEVN - CHUNE/NUNE)
      SIGLM = .25
      GOTO IZZZ03
17013 CONTINUE
        NHTGD  = 0
      DO 13014 IP0 = HPHT0,HPHT9,HLDHT
          IW0 = IWRK(IP0)
          X   = WRK(IP0+5)
          Y   = WRK(IP0+6)
          F   = (PAR1 *X + PAR2 )*X + PAR3
          DFL = F - Y
          X   = WRK(IP0+7)
          Y   = WRK(IP0+8)
          F   = (PAR1 *X + PAR2 )*X + PAR3
          DFR = F - Y
          DF  = DFL
          IF(ABS(DFR).LT.ABS(DFL)) DF = DFR
      IF(
     - ABS(DF).LT.ALBLM2
     -)THEN
            ILBLR = 1
            IF(DF.EQ.DFR) ILBLR = 2
      IF(
     - ABS(DF).LT.ALBLM1
     -)THEN
              NHTGD  = NHTGD + 1
              IF(IWRK(IP0+ 9).LT.0) IWRK(IP0+ 9) = 0
              IF(IWRK(IP0+10).LT.0) IWRK(IP0+10) = 0
              IWRK(IP0+10) = IWRK(IP0+9)
              IWRK(IP0+ 9) = ITRK
      IF(
     - IWRK(IP0+10).NE.0
     -)THEN
                LB2 = LAND(IWRK(IP0+4),    3)*4
      ELSE
                LB2 = 0
      ENDIF
              LB1 = LAND(IWRK(IP0+4),MKLR1)
              LB1 = LOR(LB1,LB2)
              IWRK(IP0+4) = LOR(LB1,ILBLR)
      ELSE
      IF(
     - IWRK(IP0+ 9).LE.0
     -)THEN
                IWRK(IP0+10) = IWRK(IP0+9)
                IWRK(IP0+ 9) =-ITRK
      IF(
     - IWRK(IP0+10).NE.0
     -)THEN
                  LB2 = LAND(IWRK(IP0+4),    3)*4
      ELSE
                  LB2 = 0
      ENDIF
                LB1 = LAND(IWRK(IP0+4),MKLR1)
                LB1 = LOR(LB1,LB2)
                IWRK(IP0+4) = LOR(LB1,ILBLR)
      ENDIF
      ENDIF
      ENDIF
13014 CONTINUE
13015 CONTINUE
      GOTO IZZZ05
      END
