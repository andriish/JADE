      SUBROUTINE ZRFIT
      IMPLICIT INTEGER*2 (H)
C-------------------------------
C  MACRO CHEADR .... HEADER BANK
C-------------------------------
      COMMON /CHEADR/ IHEADR(54)
      INTEGER*2 HHEADR(108)
      EQUIVALENCE (IHEADR(1),HHEADR(1))
C --
C --  HHEADR(17) = EXPERIMENT NUMBER
C --  HHEADR(18) = RUN NUMBER
C --  HHEADR(19) = EVENT NUMBER
C --  HHEADR(38) = MAGNETIC FIELD (GAUSS)
C --
C--------- END OF MACRO CHEADR ------------
C-----------------------------------------------------------------------
C                            MACRO CJDRCH .... JET CHAMBER CONSTANTS.
C-----------------------------------------------------------------------
C
      COMMON / CJDRCH / RDEC(4),PSIIN(3),RINCR(3),FIRSTW(3),FSENSW(3),
     +                  RDEPTH,SWDEPL,YSUSPN,TIMDEL(2,3),ZMAX,ZOFFS,
     +                  ZRESOL,ZNORM,ZAL,ZSCAL,DRIDEV,DRICOS,DRISIN,
     +                  PEDES,TZERO(3),DRIROT(96,2),SINDRI(96,2),
     +                  COSDRI(96,2),DRIVEL(96,2),T0FIX(3),
     +                  ABERR(8), DUMJDC(20)
C
C      BLOCK DATA SET TO MC VALUES, KALIBR WILL SET REAL DATA VALUES
C--->  A CHANGE OF THIS COMMON MUST BE DONE SIMULTANEOUSLY WITH  <----
C--->  A CHANGE OF THE BLOCK DATA                                <----
C
C--------------------------- END OF MACRO CJDRCH -----------------------
C
C-----------------------------------------------------------------------
C                            MACRO CGRAPH .... GRAPHICS COMMON
C-----------------------------------------------------------------------
C
      LOGICAL DSPDTL,SSTPS,PSTPS,FREEZE
C
      COMMON / CGRAPH / JUSCRN,NDDINN,NDDOUT,IDATSV(11),ICREC,MAXREC,
     +                  LSTCMD,ACMD,LASTVW,ISTANV,
     +                  SXIN,SXAX,SYIN,SYAX,XMIN,XMAX,YMIN,YMAX,
     +                  DSPDTL(30),SSTPS(10),PSTPS(10),FREEZE(30),
     +                  IREADM,LABEL,LSTPS(10),IPSVAR
C
C------- END OF MACRO CGRAPH -------------------------------------------
C
C----------------------------------------------
C  MACRO CWORKPR .... PATTERN RECOGNITION CWORK
C----------------------------------------------
      COMMON /CWORK/ HPLAST,HPFREE,HPWRK(30),ADWRK(600),
     ,               HPRO,HNTR,HNTCEL(98),IPCL(200),NRHT(200),
     ,               NWR1(200),DS1(200),SL1(200),
     ,               NWR2(200),DS2(200),SL2(200),
     ,               LBL(200),NTREL(200),ICRO(200),
     ,               NTR,HNREL(100),HISTR(9,100),HRES(168),
     ,               NTRLM,RLMTR(3,5),
     ,               WRK(7000)
                     DIMENSION TRKAR(200,11),ITRKAR(200,11),
     ,                         LMRTR(3,5)
                     EQUIVALENCE (IPCL(1),TRKAR(1,1),ITRKAR(1,1))
                     EQUIVALENCE (LMRTR(1,1),RLMTR(1,1))
         DIMENSION IWRK(7000),HWRK(14000),IDWRK(600),HDWRK(1200)
                     EQUIVALENCE (IWRK(1),WRK(1),HWRK(1))
                     EQUIVALENCE (IDWRK(1),ADWRK(1),HDWRK(1))
C---------- END OF MACRO CWORKPR --------------
C-------------------------------------------------------
C  MACRO CWORKEQ .... PATTERN RECOGNITION CWORK POINTERS
C-------------------------------------------------------
      EQUIVALENCE
C                POINTERS FOR FXYZ HIT ARRAY .. PRIMARY L/R SOLUTION
     +          (HPHT0,HPWRK( 1)),(HPHT9,HPWRK( 2)),(HLDHT,HPWRK( 3))
C                POINTERS FOR CWORK SINGLE TRACK PATR BANK
     +         ,(HPTR0,HPWRK( 4)),(HPTR9,HPWRK( 5)),(HLDTR,HPWRK( 6))
C                POINTERS FOR TRACK ELEMENT HIT LABEL ARRAY
     +         ,(HPHL0,HPWRK( 7)),(HPHL9,HPWRK( 8)),(HLDHL,HPWRK( 9))
C                POINTERS FOR FXYZ HIT ARRAY .. OPPOSITE L/R SOLUTION
     +         ,(HPHT0A,HPWRK(10)),(HPHT9A,HPWRK(11)),(HLDHTA,HPWRK(12))
C               POINTER LIMIT ON FXYZ HIT ARRAY
     +         ,(HPHTLM,HPWRK(13))
C               POINTERS FOR
     +         ,(HPTE0,HPWRK(14)),(HPTE9,HPWRK(15)),(HLDTE,HPWRK(16))
C-------------- END OF MACRO CWORKEQ ------------------
      DIMENSION SUM(10),HIST(200)
      EQUIVALENCE
     ,           (ICELL ,ADWRK(1)),(NHIT  ,ADWRK(2)),(IRING ,ADWRK(3))
     ,         , (IERRCD,ADWRK(4)),(NTRKEL,ADWRK(5))
     ,         , (SUM(1),ADWRK(101))
C----------------------------------------------
C  MACRO CPATLM .... PATTERN RECOGNITION LIMITS
C----------------------------------------------
      COMMON /CPATLM/ PATRLM(5),FLINLM(10),TRELLM(20),ZFITLM(10),BKK(20)
     *               ,XYF(20),IGFP(20),XBKK(40),IADMIN(5),YBKK(20)
      INTEGER IXYF(20),LMPATR(5),LMFLIN(10)
      INTEGER LMTREL(20),LMZFIT(10),IBKK(20)
      DIMENSION GFP(20),IXBKK(40),IYBKK(20)
      EQUIVALENCE (PATRLM(1),LMPATR(1)),(IXBKK(1),XBKK(1)),(IYBKK(1),
     *YBKK(1))   ,(FLINLM(1),LMFLIN(1)),(TRELLM(1),LMTREL(1))
     *           ,(ZFITLM(1),LMZFIT(1)),(BKK(1),IBKK(1))
     *           ,(XYF(1),IXYF(1)),(GFP(1),IGFP(1)),(IADMIN(1),IMCERT)
     *           ,(IYBKK(20),IPPASS),(IADMIN(2),IPFAST)
C----------- END OF MACRO CPATLM --------------
      DATA SBRAT/2./, NPKMIN /4/, NBINZ/200/
      IERRLB = 0
      IP0 = HPHT0
      IP9 = HPHT9
      IDP = HLDHT
      IP8 = IP9 - IDP + 1
      Z0 = .5*(WRK(IP0+5)+WRK(IP9-IDP+6))
      R0 = .5*(WRK(IP0+6)+WRK(IP9-IDP+7))
      ZMAXLM = ZMAX + 20.
      NHIT = 0
      NBAD = 0
      DO 13000 IP=HPHT0,HPHT9,HLDHT
        IF(IWRK(IP+ 7).GE.10) IWRK(IP+7) = 16
        IF(ABS(WRK(IP+ 5)).GE.ZMAXLM) IWRK(IP+7) = 16
        IF(IWRK(IP+10).NE. 0) IWRK(IP+7) = LOR(IWRK(IP+7),8)
        IF(IWRK(IP+ 7).EQ. 0) NHIT = NHIT + 1
        IF(IWRK(IP+ 7).LT.16) NBAD = NBAD + 1
13000 CONTINUE
13001 CONTINUE
      NBAD = NBAD - NHIT
      IF(
     - NHIT.LT.16 .AND. NBAD.GE.8
     -)THEN
      NHIT = 0
      DO 13002 IP=HPHT0,HPHT9,HLDHT
      IF(
     - IWRK(IP+ 7).LT.16 .AND. IWRK(IP+10).EQ.0
     -)THEN
            IWRK(IP+7) = 0
            NHIT = NHIT + 1
      ENDIF
13002 CONTINUE
13003 CONTINUE
      ENDIF
      IF(
     - NHIT.LT.3
     -)THEN
        IWRK(HPTR0+28) = 1
        WRK (HPTR0+29) = 0.
        WRK (HPTR0+30) = 0.
        WRK (HPTR0+31) = 1000000.
        IWRK(HPTR0+32) = 0.
        IWRK(HPTR0+47) = LOR(IWRK(HPTR0+47),384)
        RETURN
      ENDIF
      IF(
     - LAND(LMZFIT(10),1).NE.0
     -)THEN
      ASSIGN 17001 TO IZZZ01
      GOTO 17000
17001 CONTINUE
      ENDIF
      IF(
     - LAND(LMZFIT(10),2).NE.0
     -)THEN
      ASSIGN 17003 TO IZZZ02
      GOTO 17002
17003 CONTINUE
      ENDIF
      NITER = 0
16000 CONTINUE
      ASSIGN 17005 TO IZZZ03
      GOTO 17004
17005 CONTINUE
        A1SV   = A1
        B1SV   = B1
        RMSSV  = RMS
        NDEGSV = NDEG
      IF(
     - RMS.LE.ZFITLM(1) .OR. NDEG.LT.6
     -)THEN
      GOTO 16001
      ENDIF
      IF(
     - LAND(LMZFIT(10),1).EQ.0
     -)THEN
      ASSIGN 17006 TO IZZZ01
      GOTO 17000
17006 CONTINUE
      ELSE
      ASSIGN 17008 TO IZZZ04
      GOTO 17007
17008 CONTINUE
      ENDIF
      IF(
     - NBAD.EQ.0
     -)THEN
      GOTO 16001
      ENDIF
      ASSIGN 17009 TO IZZZ03
      GOTO 17004
17009 CONTINUE
      IF(
     - RMS.LE.ZFITLM(1)
     -)THEN
          A1SV   = A1
          B1SV   = B1
          RMSSV  = RMS
          NDEGSV = NDEG
          DZMAX = ZFITLM(2)*3
      ASSIGN 17011 TO IZZZ05
      GOTO 17010
17011 CONTINUE
      GOTO 16001
      ENDIF
      NITER = NITER + 1
      IF(.NOT.(
     - NITER.GE.1
     -))GOTO 16000
16001 CONTINUE
      IF(RMS.GT.ZFITLM(1) .OR. NDEG.LT.6) IERRLB = LOR(IERRLB,128)
      IWRK(HPTR0+28) = 1
      WRK (HPTR0+29) = B1SV
      WRK (HPTR0+30) = A1SV
      WRK (HPTR0+31) = RMSSV
      IWRK(HPTR0+32) = NDEGSV+2
      IWRK(HPTR0+47) = LOR(IWRK(HPTR0+47),IERRLB)
      RETURN
17010 CONTINUE
        NBAD = 0
        NGOOD = 0
      DO 13004 IPHIT = HPHT0,HPHT9,HLDHT
      IF(
     - IWRK(IPHIT+7).LE.8
     -)THEN
            R1 = WRK(IPHIT+6)
            Z1 = WRK(IPHIT+5)
            ZF = R1*B1 + A1
            DZ = Z1 - ZF
      IF(
     - ABS(DZ).GT.DZMAX
     -)THEN
              NBAD = NBAD + 1
              IWRK(IPHIT+7) = 1
      ELSE
              NGOOD = NGOOD + 1
              IWRK(IPHIT+7) = 0
      ENDIF
      ENDIF
13004 CONTINUE
13005 CONTINUE
      IF(
     - NGOOD.LT.LMZFIT(3)
     -)THEN
      DO 13006 IP1 = IP0,IP9,IDP
          IWRK(IP1+7) = LAND(IWRK(IP1+7),14)
13006 CONTINUE
13007 CONTINUE
        NBAD = 0
      ENDIF
      GOTO IZZZ05
17007 CONTINUE
        NBAD = 0
        NGOOD = 0
      DO 13008 IPHIT = HPHT0,HPHT9,HLDHT
      IF(
     - IWRK(IPHIT+7).LT.4
     -)THEN
      IF(
     - IABS(IWRK(IPHIT+8)).GE.1000
     -)THEN
              NBAD = NBAD + 1
              IWRK(IPHIT+7) = LOR(IWRK(IPHIT+7),4)
      ELSE
              NGOOD = NGOOD + 1
              IWRK(IPHIT+7) = 0
      ENDIF
      ENDIF
13008 CONTINUE
13009 CONTINUE
      GOTO IZZZ04
17004 CONTINUE
        IHIT = 0
        SUM(1) = 0.
        SUM(2) = 0.
        SUM(3) = 0.
        SUM(4) = 0.
        SUM(5) = 0.
      DO 13010 IPHIT = IP0,IP9,IDP
      IF(
     - IWRK(IPHIT+7).EQ.0
     -)THEN
            IHIT =  IHIT + 1
            R1 = WRK(IPHIT+6) - R0
            Z1 = WRK(IPHIT+5) - Z0
            SUM( 1) = SUM( 1) + R1
            SUM( 2) = SUM( 2) + R1**2
            SUM( 3) = SUM( 3) + Z1
            SUM( 4) = SUM( 4) + Z1**2
            SUM( 5) = SUM( 5) + R1*Z1
      ENDIF
13010 CONTINUE
13011 CONTINUE
        ANHIT= IHIT
        NDEG = IHIT - 2
      IF(
     - NDEG.GT.0
     -)THEN
          ZW1  = (SUM(5)*ANHIT - SUM(1)*SUM(3))
          ZW2  = (SUM(2)*ANHIT - SUM(1)**2)
          B1   = ZW1 / ZW2
          A1   =(SUM(3) - B1*SUM(1)) / ANHIT + Z0 - B1*R0
          CHSQ = SUM(4)*ANHIT - SUM(3)**2 - ZW1**2/ZW2
          RMS  = CHSQ / (IHIT*NDEG)
          IF(RMS.GT.0) RMS = SQRT(RMS)
      ELSE
          B1   = 0.
          A1   = 0.
          RMS  = 1000000.
      ENDIF
      GOTO IZZZ03
17002 CONTINUE
      SIGL2 = ZFITLM(2)*2.
      SIGL4 = ZFITLM(2)*3.5
      NHIT  = 0
      IP10  = 0
      IP3   = IP0
16002 CONTINUE
      IF(
     - IWRK(IP3+ 7).EQ. 0
     -)THEN
          IF(IP10.LE.0) IP10 = IP3
          NHIT = NHIT + 1
      IF(
     - NHIT.EQ.1
     -)THEN
            R1 = WRK(IP3+6)
            Z1 = WRK(IP3+5)
            IP1 = IP3
      ELSE
      IF(
     - NHIT.EQ.2
     -)THEN
              R2 = WRK(IP3+6)
              Z2 = WRK(IP3+5)
              IP2 = IP3
      ELSE
              R3 = WRK(IP3+6)
              Z3 = WRK(IP3+5)
              DR31 = R3-R1
              DELT = 0.
              IF(ABS(DR31).GT.15.) DELT = Z2-Z1 - (R2-R1)*(Z3-Z1)/DR31
              IF(ABS(DELT).GT.SIGL4) IWRK(IP2+7) = 4
      IF(
     - ABS(DELT).GT.SIGL2
     -)THEN
                IF(IP1.EQ.IP0) IWRK(IP1+7) = 4
                IF(IP3.EQ.IP8) IWRK(IP3+7) = 4
      ENDIF
              R1 = R2
              R2 = R3
              Z1 = Z2
              Z2 = Z3
              IP1 = IP2
              IP2 = IP3
      ENDIF
      ENDIF
      ENDIF
      IP3 = IP3 + IDP
      IF(.NOT.(
     - IP3.GT.IP9
     -))GOTO 16002
16003 CONTINUE
      IP90 = IP2
      IP1 = IP10
      IP2 = 0
      IP3 = IP1
16004 CONTINUE
      IP3 = IP3 + IDP
      IF(
     - IWRK(IP3+ 7).LE.4
     -)THEN
          IP2 = IP3
      GOTO 16005
      ENDIF
      IF(.NOT.(
     - IP3.EQ.IP9
     -))GOTO 16004
16005 CONTINUE
      IF(
     - IP2.LE.0
     -)THEN
        NHIT = 0
      ELSE
16006 CONTINUE
        IP3 = IP3 + IDP
      IF(
     - IWRK(IP3+ 7).LE.4
     -)THEN
      IF(
     - IWRK(IP3+7).EQ.4 .OR. IWRK(IP1+7).EQ.4
     -)THEN
              R1 = WRK(IP1+6)
              Z1 = WRK(IP1+5)
              R2 = WRK(IP2+6)
              Z2 = WRK(IP2+5)
              R3 = WRK(IP3+6)
              Z3 = WRK(IP3+5)
              DR31 = R3-R1
              DELT = 1000000.
              IF(ABS(DR31).GT.15.) DELT = Z2-Z1 - (R2-R1)*(Z3-Z1)/DR31
      IF(
     - ABS(DELT).LT.SIGL2
     -)THEN
                IWRK(IP1+7) = 0
                IWRK(IP3+7) = 0
      ENDIF
      ENDIF
            IP1 = IP2
            IP2 = IP3
      ENDIF
      IF(.NOT.(
     - IP3.GE.IP90
     -))GOTO 16006
16007 CONTINUE
        NHIT = 0
      DO 13012 IP1=IP0,IP9,IDP
          IF(IWRK(IP1+7).EQ.0) NHIT = NHIT + 1
13012 CONTINUE
13013 CONTINUE
      ENDIF
      IF(
     - NHIT.LT.LMZFIT(3)
     -)THEN
      DO 13014 IP1 = IP0,IP9,IDP
          IWRK(IP1+7) = LAND(IWRK(IP1+7),11)
13014 CONTINUE
13015 CONTINUE
      ENDIF
      GOTO IZZZ02
17000 CONTINUE
      DO 13016 I = 1,NBINZ
        HIST(I) = 0
13016 CONTINUE
13017 CONTINUE
      AVRAD  = 0.
      NAVRAD = 0
      IP91 = IP9 - IDP
      Z0HIST = -7000.
      DZHIST = 70.
      DO 13018 IPHIT = IP0,IP91,IDP
      IF(
     - IWRK(IPHIT+7).EQ.0
     -)THEN
          R1 = WRK(IPHIT+6)
          AVRAD  = R1 + AVRAD
          NAVRAD =  1 + NAVRAD
          Z1 = WRK(IPHIT+5)
          IP1 = IPHIT + IDP
      DO 13020 IPHIT2 = IP1,IP9,IDP
      IF(
     - IWRK(IPHIT2+7).EQ.0
     -)THEN
              R2 = WRK(IPHIT2+6)
              Z2 = WRK(IPHIT2+5)
      IF(
     - ABS(R1-R2).GT.ZFITLM(6)
     -)THEN
                ZCON = (Z1*R2 - R1*Z2)/(R2-R1)
                IZV = (ZCON - Z0HIST) / DZHIST + 1
      IF(
     - IZV.GT.0 .AND. IZV.LE.NBINZ
     -)THEN
                  HIST(IZV) = HIST(IZV) + 1
      ENDIF
      ENDIF
      ENDIF
13020 CONTINUE
13021 CONTINUE
      ENDIF
13018 CONTINUE
13019 CONTINUE
      ASSIGN 17013 TO IZZZ06
      GOTO 17012
17013 CONTINUE
      ZVTX = ZPEAK
      IF(
     - INDLB.GT.0
     -)THEN
      DO 13022 I = 1,NBINZ
          HIST(I) = 0
13022 CONTINUE
13023 CONTINUE
        IF(NAVRAD.GT.0) AVRAD = AVRAD / NAVRAD
        Z0HIST = -3000.
        DZHIST = 30.
      DO 13024 IPHIT = IP0,IP9,IDP
      IF(
     - IWRK(IPHIT+7).LE.4
     -)THEN
            IWRK(IPHIT+7) = 0
            ZCON = (WRK(IPHIT+5) - ZVTX) * AVRAD/WRK(IPHIT+6)
            IZV  = (ZCON - Z0HIST) / DZHIST + 1
      IF(
     - IZV.GT.0 .AND. IZV.LE.NBINZ
     -)THEN
              HIST(IZV) = HIST(IZV) + 1
      ENDIF
      ENDIF
13024 CONTINUE
13025 CONTINUE
      ASSIGN 17014 TO IZZZ06
      GOTO 17012
17014 CONTINUE
      IF(
     - INDLB.GT.0
     -)THEN
          ZPRO = ZPEAK
          SLOPE = ZPRO / AVRAD
          SIG0 = ZFITLM(2)*4.
          NHIT = 0
          NBAD = 0
      DO 13026 IPHIT = IP0,IP9,IDP
      IF(
     - IWRK(IPHIT+7).LE.0
     -)THEN
              NHIT = NHIT + 1
              DZ = WRK(IPHIT+6)*SLOPE + ZVTX - WRK(IPHIT+5)
      IF(
     - ABS(DZ).GT.SIG0
     -)THEN
                NBAD = NBAD + 1
                IWRK(IPHIT+7) = LOR(IWRK(IPHIT+7),4)
      ENDIF
      ENDIF
13026 CONTINUE
13027 CONTINUE
          NHIT = NHIT - NBAD
      IF(
     - NHIT.LT.LMZFIT(3)
     -)THEN
      DO 13028 IPHIT = IP0,IP9,IDP
              IWRK(IPHIT+7) = LAND(IWRK(IPHIT+7),11)
13028 CONTINUE
13029 CONTINUE
      ENDIF
      ENDIF
      ENDIF
      GOTO IZZZ01
17012 CONTINUE
        NPEAK = 0
        IH9 = NBINZ-11
      DO 13030 IH=7,IH9
          IHSUM = HIST(IH  )+HIST(IH+1)+HIST(IH+2)+HIST(IH+3)+HIST(IH+4)
      IF(
     - IHSUM.GT.NPEAK
     -)THEN
            NPEAK = IHSUM
            HPEAK = IH
      ENDIF
13030 CONTINUE
13031 CONTINUE
        PEAK = NPEAK
        INDLB =-1
      IF(
     - NPEAK.GE.NPKMIN
     -)THEN
          INDLB = 1
          H1 = HPEAK - 7
          H2 = HPEAK + 7
          NBACK = HIST(H1  )+HIST(H1+1)+HIST(H1+2)+HIST(H1+3)+HIST(H1+4)
     +          + HIST(H2  )+HIST(H2+1)+HIST(H2+2)+HIST(H2+3)+HIST(H2+4)
          BACK = .5 * NBACK
          IF(BACK*SBRAT.GT.PEAK) INDLB = 0
          ZV = HIST(HPEAK+1)   + HIST(HPEAK+2)*2
     +        + HIST(HPEAK+3)*3 + HIST(HPEAK+4)*4
          ZV = ZV / PEAK
          DZ = HIST(HPEAK  )*(ZV   )**2 + HIST(HPEAK+1)*(ZV-1.)**2
     +       + HIST(HPEAK+2)*(ZV-2.)**2 + HIST(HPEAK+3)*(ZV-3.)**2
     +       + HIST(HPEAK+4)*(ZV-4.)**2
          DZ = DZ * DZHIST**2 / PEAK
          DZ = SQRT(DZ)
          ZPEAK     = Z0HIST + (HPEAK+ZV-.5)*DZHIST
      ENDIF
      GOTO IZZZ06
      END
