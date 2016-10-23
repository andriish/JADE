      SUBROUTINE ZRFITO(INDEX,IARG1,X0,Y0,Z0)
      IMPLICIT INTEGER*2 (H)
#include "cdata.for"
#include "cjdrch.for"
#include "cworkpr.for"
#include "cworkeq.for"
      EQUIVALENCE
     ,          (HPCO0 ,HPWRK(17)),(HPCO9 ,HPWRK(18)),(HLDCO ,HPWRK(19))
     ,         ,(HPZR0 ,HPWRK(20)),(HPZR9 ,HPWRK(21)),(HLDZR ,HPWRK(22))
     ,         ,(ICELL ,IDWRK( 1)),(MHIT  ,IDWRK( 2)),(IRING ,IDWRK( 3))
     ,         ,(IERRCD,IDWRK( 4)),(NTRKEL,IDWRK( 5))
      DIMENSION HST(200),IPTRZ(40,2),JPTRBK(40)
      DIMENSION SUMF(40,5),JTRZF(40),SLTRZ(40)
      EQUIVALENCE (ZWZ,IZW)
 2005 FORMAT(' ZRFITO(PST): TRACK',I6,I3,' NOT ACCEPTED')
      DATA LBINIT /0/
      IF(
     - LBINIT.EQ.0
     -)THEN
        LBINIT = 1
        IQPATR = IBLN('PATR')
        IQJHTL = IBLN('JHTL')
      ENDIF
16000 CONTINUE
      IF(
     - INDEX.LT.0
     -)THEN
        HPZR0 = IARG1
        HPZR9 = IARG1 - 1
        HLDZR = 3
        NTRZ  = 0
      GOTO 16001
      ENDIF
      IF(
     - INDEX.EQ.0
     -)THEN
        IPZR = HPZR9
        NHIT3 = 0
      DO 13000 IP=HPCO0,HPCO9,HLDCO
          IF(IWRK(IP+10).GT.1) IWRK(IP+7) = 16
      IF(
     - IWRK(IP+7).LT.8
     -)THEN
            IWRK(IPZR+1) = 0
            WRK (IPZR+2) = WRK(IP+6)
            WRK (IPZR+3) = WRK(IP+5)
             NHIT3 = NHIT3 + 1
            IPZR  = IPZR + HLDZR
      ENDIF
13000 CONTINUE
13001 CONTINUE
      IF(
     - NHIT3.GE.8
     -)THEN
          NTRZ = NTRZ + 1
          IPTRZ(NTRZ,1) = HPZR9 + 1
          IPTRZ(NTRZ,2) = IPZR
          JPTRBK(NTRZ)  = IARG1
          HPZR9 = IPZR
      ELSE
          IPTR1 = IARG1
      PRINT 2005,IPTR1, IDATA(IPTR1+1)
          INDEX =-2
      ENDIF
      GOTO 16001
      ENDIF
      IF(
     - INDEX.GT.0
     -)THEN
      IF(
     - NTRZ.LE.0
     -)THEN
      GOTO 16001
      ENDIF
        IZW = IARG1
        ZVTX0 = ZWZ
        ITRZ = 0
15000 CONTINUE
      IF(
     - ITRZ.LT.NTRZ
     -)THEN
        ITRZ = ITRZ + 1
          IP0 = IPTRZ(ITRZ,1)
          IP9 = IPTRZ(ITRZ,2)
      ASSIGN 17001 TO IZZZ01
      GOTO 17000
17001 CONTINUE
      GOTO 15000
      ENDIF
15001 CONTINUE
      ASSIGN 17003 TO IZZZ02
      GOTO 17002
17003 CONTINUE
      ASSIGN 17005 TO IZZZ03
      GOTO 17004
17005 CONTINUE
      ASSIGN 17006 TO IZZZ02
      GOTO 17002
17006 CONTINUE
      ASSIGN 17008 TO IZZZ04
      GOTO 17007
17008 CONTINUE
      GOTO 16001
      ENDIF
      IF(.NOT.(
     - .TRUE.
     -))GOTO 16000
16001 CONTINUE
      RETURN
17000 CONTINUE
        CALL SETSL(HST(1),0,400,0)
        SL0   =-7.5
        BINSL = .075
        BINIV = 1. / BINSL
      DO 13002 IP1=IP0,IP9,3
          R1 = WRK(IP1+1)
          Z1 = WRK(IP1+2)
          SL = (Z1 - ZVTX0) / R1
          IBIN = (SL - SL0) * BINIV
          IF(IBIN.GT.0 .AND. IBIN.LT.200) HST(IBIN) = HST(IBIN) + 1
13002 CONTINUE
13003 CONTINUE
        IMAX = 1
        MHST = HST(1) + HST(2) + HST(3) + HST(4) + HST(5)
        NHST = MHST
      DO 13004 I= 2,196
          NHST = NHST + HST(I+4) - HST(I-1)
      IF(
     - NHST.GT.MHST
     -)THEN
            IMAX = I
            MHST = NHST
      ENDIF
13004 CONTINUE
13005 CONTINUE
      IF(
     - MHST.LT.5
     -)THEN
          SLP = 1000.
      ELSE
          ISL = HST(IMAX+1) + HST(IMAX+2)*2 + HST(IMAX+3)*3
     +        + HST(IMAX+4)*4
          SLP = (FLOAT(ISL) / MHST + IMAX + .5) * BINSL + SL0
      ENDIF
      DO 13006 IP1=IP0,IP9,3
          R1 = WRK(IP1+1)
          Z1 = WRK(IP1+2)
          F1 = R1*SLP + ZVTX0
          DF = F1 - Z1
      IF(
     - ABS(DF).LT.80.
     -)THEN
            IWRK(IP1) = 0
      ELSE
            IWRK(IP1) = 16
      ENDIF
13006 CONTINUE
13007 CONTINUE
      GOTO IZZZ01
17002 CONTINUE
        ITRZ = 0
        MTRZ = 0
        SUM0 = 0.
        SUM4 = 0.
15002 CONTINUE
      IF(
     - ITRZ.LT.NTRZ
     -)THEN
        ITRZ = ITRZ + 1
          IP0 = IPTRZ(ITRZ,1)
          IP9 = IPTRZ(ITRZ,2)
          S1 = 0.
          S2 = 0.
          S3 = 0.
          S4 = 0.
          S5 = 0.
      DO 13008 IP1=IP0,IP9,3
      IF(
     - IWRK(IP1).EQ.0
     -)THEN
              R1 = WRK(IP1+1)
              Z1 = WRK(IP1+2)
              S1 = 1.    + S1
              S2 = R1    + S2
              S3 = R1*R1 + S3
              S4 = Z1    + S4
              S5 = Z1*R1 + S5
      ENDIF
13008 CONTINUE
13009 CONTINUE
      IF(
     - S1.LT.7.5
     -)THEN
            JTRZF(ITRZ) = 0
      ELSE
            MTRZ = MTRZ + 1
            JTRZF(ITRZ) = MTRZ
            SUM0 = SUM0 + S1
            SUM4 = SUM4 + S4
            SUMF(MTRZ,1) = S1
            SUMF(MTRZ,2) = S2
            SUMF(MTRZ,3) = S3
            SUMF(MTRZ,4) = S4
            SUMF(MTRZ,5) = S5
      ENDIF
      GOTO 15002
      ENDIF
15003 CONTINUE
      IF(
     - MTRZ.GT.0
     -)THEN
          S0 = 0.
          S4 = 0.
      DO 13010 I=1,MTRZ
            S0 = SUMF(I,2)**2 / SUMF(I,3)        + S0
            S4 = SUMF(I,5)*SUMF(I,2) / SUMF(I,3) + S4
13010 CONTINUE
13011 CONTINUE
          S0 = SUM0 - S0
          S4 = SUM4 - S4
          ZVFIT = S4 / S0
      DO 13012 I=1,MTRZ
            SLTRZ(I) = (-SUMF(I,2)*ZVFIT + SUMF(I,5)) / SUMF(I,3)
13012 CONTINUE
13013 CONTINUE
      ENDIF
      GOTO IZZZ02
17004 CONTINUE
        ITRZ = 0
15004 CONTINUE
      IF(
     - ITRZ.LT.NTRZ
     -)THEN
        ITRZ = ITRZ + 1
          JTRZ = JTRZF(ITRZ)
      IF(
     - JTRZ.GT.0
     -)THEN
            SLP  = SLTRZ(JTRZ)
            IP0 = IPTRZ(ITRZ,1)
            IP9 = IPTRZ(ITRZ,2)
      DO 13014 IP1=IP0,IP9,3
              R1 = WRK(IP1+1)
              Z1 = WRK(IP1+2)
              F1 = R1*SLP + ZVFIT
              DF = F1 - Z1
      IF(
     - ABS(DF).LT.80.
     -)THEN
                IWRK(IP1) = 0
      ELSE
                IWRK(IP1) = 16
      ENDIF
13014 CONTINUE
13015 CONTINUE
      ENDIF
      GOTO 15004
      ENDIF
15005 CONTINUE
      GOTO IZZZ03
17007 CONTINUE
        ITRZ = 0
15006 CONTINUE
      IF(
     - ITRZ.LT.NTRZ
     -)THEN
        ITRZ = ITRZ + 1
          JTRZ = JTRZF(ITRZ)
      IF(
     - JTRZ.LE.0
     -)THEN
            IPTR1 = JPTRBK(ITRZ)
            TGTH = ADATA(IPTR1+30)
            Z0TR = ADATA(IPTR1+31)
            CSTH = 1./SQRT(TGTH**2+1.)
            SNTH = CSTH * TGTH
            ADATA(IPTR1+ 8) = ADATA(IPTR1+ 8) * CSTH
            ADATA(IPTR1+ 9) = ADATA(IPTR1+ 9) * CSTH
            ADATA(IPTR1+15) = ADATA(IPTR1+15) * CSTH
            ADATA(IPTR1+16) = ADATA(IPTR1+16) * CSTH
            ADATA(IPTR1+17) = SNTH
            ADATA(IPTR1+ 7) = ADATA(IPTR1+ 7) * TGTH + Z0TR
            ADATA(IPTR1+14) = ADATA(IPTR1+14) * TGTH + Z0TR
      ELSE
            SLP  = SLTRZ(JTRZ)
            IPTR1 = JPTRBK(ITRZ)
            IP0 = IPTRZ(ITRZ,1)
            IP9 = IPTRZ(ITRZ,2)
            S1 = 0.
      DO 13016 IP1=IP0,IP9,3
              R1 = WRK(IP1+1)
              Z1 = WRK(IP1+2)
              F1 = R1*SLP + ZVFIT
              DF = F1 - Z1
              IF(IWRK(IP1).EQ.0) S1 = DF**2 + S1
              WRK(IP1) = DF
13016 CONTINUE
13017 CONTINUE
            MHT = SUMF(JTRZ,1)
            SIG = SQRT(S1 / (MHT-1))
            CSTH = 1./SQRT(SLP**2+1.)
            SNTH = CSTH * SLP
            RVTX = ADATA(IPTR1+17)
            ADATA(IPTR1+ 7) = ADATA(IPTR1+ 7)*SLP + ZVFIT
            ADATA(IPTR1+14) = ADATA(IPTR1+14)*SLP + ZVFIT
            ADATA(IPTR1+ 8) = ADATA(IPTR1+ 8)*CSTH
            ADATA(IPTR1+ 9) = ADATA(IPTR1+ 9)*CSTH
            ADATA(IPTR1+10) = SNTH
            ADATA(IPTR1+15) = ADATA(IPTR1+15)*CSTH
            ADATA(IPTR1+16) = ADATA(IPTR1+16)*CSTH
            ADATA(IPTR1+17) = SNTH
            IDATA(IPTR1+29) = 1
            ADATA(IPTR1+30) = SLP
            ADATA(IPTR1+31) = RVTX*SLP + ZVFIT
            ADATA(IPTR1+32) = SIG
            IDATA(IPTR1+33) = MHT
            IDATA(IPTR1+ 2) = LOR(IDATA(IPTR1+ 2),64)
            IDATA(IPTR1+ 4) = LOR(IDATA(IPTR1+ 4),64)
      ENDIF
      GOTO 15006
      ENDIF
15007 CONTINUE
      GOTO IZZZ04
      END
