      SUBROUTINE RINCON
      IMPLICIT INTEGER*2 (H)
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
C   30/10/79 910301050  MEMBER NAME  CWORKMX  (PATRECSR)    FORTRAN
C   16/09/79 C9092001   MEMBER NAME  CWORKMX  (UKSOR)       FORTRAN
C ==========MACRO CWORKMG =============================
      EQUIVALENCE (ADWRK(291),HUSE(1)),(ADWRK(391),K),
     * (ADWRK(392),ICL),(ADWRK(393),LRING),(ADWRK(394),KRING),
     * (ADWRK(395),LR),(ADWRK(396),IRIFLG),(ADWRK(397),IUDFLG),
     * (ADWRK(398),ILIM),(ADWRK(399),LR1),(ADWRK(400),IRL),
     * (ADWRK(401),ITK(1,1)),(ADWRK(441),DTEMP(1)),
     * (ADWRK(451),IPST),(ADWRK(452),IJFLG),(ADWRK(453),ICX),
     * (ADWRK(454),KT),(ADWRK(455),ISDL),(ADWRK(456),IBFIT),
     * (ADWRK(457),ISP),(ADWRK(458),ISKP(1)),(ADWRK(468),HSP1(1)),
     * (ADWRK(478),ITOL),(ADWRK(479),IW),(ADWRK(480),A),
     * (ADWRK(481),DS),(ADWRK(482),IWT),(ADWRK(483),ICT),
     * (ADWRK(484),IKX),(ADWRK(485),LRCORN)
      DIMENSION HUSE(200),DTEMP(10),ITK(10,4),ISKP(10),HSP1(20)
      EQUIVALENCE (ILOUT,IBKK(3)),(ILIN,IBKK(4)),
     * (ILBOT,IBKK(8)),(DCELL,BKK(9))
C ==========ENDMACRO CWORKMG==========================
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
C----------------------------------------------------------------------
C           MACRO CDSMAX .... PATTERN RECOGNITION CONSTANTS.
C----------------------------------------------------------------------
      COMMON/CDSMAX/DSMAX(16,3,2),DIRWR1(24,2),DIRWR3(48,2)
     *             ,DHALF(16,3,2),DTWICE(16,3,2),HMCH(16,3,2)
     *             ,IBCK(9),DBCK(30),TRMATS(96,2),TRMATC(96,2)
C------------------------ END OF MACRO CDSMAX -------------------------
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
      DIMENSION LSTCL(3),LFTCL(3),NCELL(3),TANDEL(3)
      EQUIVALENCE (IBCK(1),LSTCL(1)),(IBCK(4),LFTCL(1))
      EQUIVALENCE (IBCK(7),NCELL(1)),(DBCK(1),TANDEL(1))
      DIMENSION HTEMP(9)
      DATA MSKCR1,MSKCR2,MSKERR,MSKLR0 /Z100,Z200,Z4000,Z1000/
      DATA MSKFIT,MSKAIT /Z20000,ZFFFDFFFF/
      DO 13000 II=1,2
      IRL=0
      ASSIGN 17001 TO IZZZ01
      GOTO 17000
17001 CONTINUE
      IF(
     - HNTCEL(ICX+1) - HNTCEL(ICX).GT.0
     -)THEN
      NTRLX1 = HNTCEL(ICX)
      NTRLX2 = HNTCEL(ICX+1)-1
      KRING = LRING - 1
      LRCORN=0
      DO 13002 KX = NTRLX1,NTRLX2
      IF(
     - HUSE(KX).EQ.0
     -)THEN
      IF(
     - KRING.EQ.2
     -)THEN
      ASSIGN 17003 TO IZZZ02
      GOTO 17002
17003 CONTINUE
      ELSE
      ASSIGN 17005 TO IZZZ03
      GOTO 17004
17005 CONTINUE
      ENDIF
      ENDIF
13002 CONTINUE
13003 CONTINUE
      ENDIF
      ASSIGN 17006 TO IZZZ01
      GOTO 17000
17006 CONTINUE
      KRING=LRING-1
      ICX=ICX+1
      IF(ICX.GT.LSTCL(KRING)) ICX=ICX-NCELL(KRING)
      LRCORN=1
      ASSIGN 17008 TO IZZZ04
      GOTO 17007
17008 CONTINUE
      ASSIGN 17009 TO IZZZ01
      GOTO 17000
17009 CONTINUE
      KRING=LRING-1
      ICX=ICX-1
      IF(ICX.LT.LFTCL(KRING)) ICX=ICX+NCELL(KRING)
      LRCORN=-1
      ASSIGN 17010 TO IZZZ04
      GOTO 17007
17010 CONTINUE
      IF(
     - IRIFLG.EQ.1
     -)THEN
      ASSIGN 17012 TO IZZZ05
      GOTO 17011
17012 CONTINUE
      ENDIF
      KMP1=HISTR(1,NTR)
      KMP1=IABS(KMP1)
      KMP1=IPCL(KMP1)
      KMP2=HISTR(HNREL(NTR),NTR)
      KMP2=IABS(KMP2)
      KMP2=IPCL(KMP2)
      ASSIGN 17014 TO IZZZ06
      GOTO 17013
17014 CONTINUE
      IF(
     - IRIFLG.NE.0.OR.IPST.EQ.0.OR.LR.EQ.0.OR.IBKK(20).NE.0.AND.
     $IJFLG.EQ.0.AND.NRHT(K).GE.IBKK(19).OR.KMP1.NE.KMP2
     -)THEN
      GOTO 13001
      ELSE
      LR=-LR
      ENDIF
13000 CONTINUE
13001 CONTINUE
      RETURN
17011 CONTINUE
      IBFIT=-4
      IRT=IRL
      ITMP=LR
      ITMP1=K
      ITMP2=ICL
      DO 13004 JK=1,IRT
      LRCORN=0
      KX=ITK(JK,1)
      ICX=IPCL(KX)
      IC1=ICX+24
      IC2=IC1
      IF(
     - LRING.EQ.3
     -)THEN
      IC1=2*ICX-1
      IC2=IC1+1
      ENDIF
      ICZ=IC1
      IF(
     - HNTCEL(IC2+1)-HNTCEL(IC1).GT.0
     -)THEN
      NTRLX3=HNTCEL(IC1)
      NTRLX4=HNTCEL(IC2+1)-1
      DO 13006 IB=NTRLX3,NTRLX4
      IBFIT=-4
      IF(
     - HUSE(IB).EQ.0.OR.LAND(LBL(IB),MSKERR).NE.0
     -)THEN
      K=IB
      ICL=IPCL(IB)
      IF(
     - KRING.EQ.2
     -)THEN
      LR=0
      ASSIGN 17016 TO IZZZ07
      GOTO 17015
17016 CONTINUE
      ASSIGN 17017 TO IZZZ02
      GOTO 17002
17017 CONTINUE
      ELSE
      LR=ITK(JK,3)
      ASSIGN 17018 TO IZZZ07
      GOTO 17015
17018 CONTINUE
      ASSIGN 17019 TO IZZZ03
      GOTO 17004
17019 CONTINUE
      ENDIF
      ENDIF
13006 CONTINUE
13007 CONTINUE
      ENDIF
      IC1=ICZ+1
      IF(LRING.EQ.3) IC1=ICZ+2
      IF(IC1.GT.LSTCL(LRING)) IC1=IC1-NCELL(LRING)
      IC2=IC1
      IF(LRING.EQ.3) IC2=IC1+1
      IF(
     - HNTCEL(IC2+1)-HNTCEL(IC1).GT.0
     -)THEN
      NTRLX3=HNTCEL(IC1)
      NTRLX4=HNTCEL(IC2+1)-1
      DO 13008 IB=NTRLX3,NTRLX4
      IBFIT=-4
      IF(
     - HUSE(IB).EQ.0.OR.LAND(LBL(IB),MSKERR).NE.0
     -)THEN
      K=IB
      ICL=IPCL(IB)
      LRCORN=-1
      LR=0
      ASSIGN 17020 TO IZZZ07
      GOTO 17015
17020 CONTINUE
      ASSIGN 17022 TO IZZZ08
      GOTO 17021
17022 CONTINUE
      ENDIF
13008 CONTINUE
13009 CONTINUE
      ENDIF
      IC1=ICZ-1
      IF(LRING.EQ.3) IC1=ICZ-2
      IF(IC1.LT.LFTCL(LRING)) IC1=IC1+NCELL(LRING)
      IC2=IC1
      IF(LRING.EQ.3) IC2=IC1+1
      IF(
     - HNTCEL(IC2+1)-HNTCEL(IC1).GT.0
     -)THEN
      NTRLX3=HNTCEL(IC1)
      NTRLX4=HNTCEL(IC2+1)-1
      DO 13010 IB=NTRLX3,NTRLX4
      IBFIT=-4
      IF(
     - HUSE(IB).EQ.0.OR.LAND(LBL(IB),MSKERR).NE.0
     -)THEN
      K=IB
      ICL=IPCL(IB)
      LRCORN=1
      LR=0
      ASSIGN 17023 TO IZZZ07
      GOTO 17015
17023 CONTINUE
      ASSIGN 17024 TO IZZZ08
      GOTO 17021
17024 CONTINUE
      ENDIF
13010 CONTINUE
13011 CONTINUE
      ENDIF
13004 CONTINUE
13005 CONTINUE
      LR=ITMP
      ICL=ITMP2
      K=ITMP1
      IBFIT=0
      GOTO IZZZ05
17002 CONTINUE
      ASSIGN 17026 TO IZZZ09
      GOTO 17025
17026 CONTINUE
      IF(
     - LR.NE.0
     -)THEN
      LRT=LR
      ASSIGN 17028 TO IZZZ10
      GOTO 17027
17028 CONTINUE
      ELSE
      DSEX=DSEXR
      LRT=1
      SLEX=SLEXR
      ASSIGN 17029 TO IZZZ10
      GOTO 17027
17029 CONTINUE
      DSEX=DSEXL
      LRT=-1
      SLEX=SLEXL
      ASSIGN 17030 TO IZZZ10
      GOTO 17027
17030 CONTINUE
      ENDIF
      GOTO IZZZ02
17004 CONTINUE
      LR1=LR
      IF(LR.EQ.0) LR1=1
      IF(LR.EQ.0.AND.IBKK(20).NE.0.AND.NRHT(K).GE.IBKK(19)) IJFLG=1
      LRT=LR1
      LRS=LR1
      ASSIGN 17032 TO IZZZ11
      GOTO 17031
17032 CONTINUE
      ASSIGN 17034 TO IZZZ12
      GOTO 17033
17034 CONTINUE
      SL=SL1K-FCONT*.5*(W3-W2)
      DSEX=D-SL*(W3-W2)
      ASSIGN 17035 TO IZZZ10
      GOTO 17027
17035 CONTINUE
      GOTO IZZZ03
17000 CONTINUE
      ICX=ICL
      IF(LRING.EQ.3) ICX =(ICX+1)/2
      IF(LRING.EQ.2) ICX = ICX - 24
      GOTO IZZZ01
17013 CONTINUE
      IF(
     - IRIFLG.EQ.1
     -)THEN
      IF(
     - IRL.GT.1
     -)THEN
      CALL CHOOSE
      ENDIF
      IF(
     - IRIFLG.EQ.1
     -)THEN
      K=ITK(1,1)
      IKX=K
      IF(LR.EQ.0.AND.ITK(1,2).EQ.-1) HISTR(1,NTR)=-HISTR(1,NTR)
      ICL=IPCL(IKX)
      LRCORN=0
      IPAR=ITK(1,4)
      IPAR=IPCL(IPAR)+ICL
      IF(LAND(IPAR,1).EQ.1) LRCORN=1
      IF(LRCORN.EQ.1.OR.KRING.EQ.2) IJFLG=0
      LR=ITK(1,3)
      IF(II.EQ.2) CALL COREC
      IF(KMP1.EQ.KMP2.AND.KRING.EQ.1.AND.II.EQ.2.AND.LRCORN.EQ.0)
     * IJFLG=1
      LRING=KRING
      CALL BSTORE
      IPST=0
      ENDIF
      ENDIF
      GOTO IZZZ06
17007 CONTINUE
      IF(
     - HNTCEL(ICX+1)-HNTCEL(ICX).GT.0
     -)THEN
      NTRLX1=HNTCEL(ICX)
      NTRLX2=HNTCEL(ICX+1)-1
      DO 13012 KX=NTRLX1,NTRLX2
      IF(
     - HUSE(KX).EQ.0
     -)THEN
      ASSIGN 17036 TO IZZZ08
      GOTO 17021
17036 CONTINUE
      ENDIF
13012 CONTINUE
13013 CONTINUE
      ENDIF
      GOTO IZZZ04
17021 CONTINUE
      ASSIGN 17038 TO IZZZ13
      GOTO 17037
17038 CONTINUE
      LR1=-LRCORN
      IF(
     - LR.NE.0
     -)THEN
      LRT=LR
      ASSIGN 17039 TO IZZZ10
      GOTO 17027
17039 CONTINUE
      ELSE
      LRT=1
      DSEX=DSEXR
      SLEX=SLEXR
      ASSIGN 17040 TO IZZZ10
      GOTO 17027
17040 CONTINUE
      LRT=-1
      DSEX=DSEXL
      SLEX=SLEXL
      ASSIGN 17041 TO IZZZ10
      GOTO 17027
17041 CONTINUE
      ENDIF
      GOTO IZZZ08
17027 CONTINUE
      IOFF=0
      IF(NRHT(K).LE.6.OR.NRHT(KX).LE.6) IOFF=IOFF+1
      IF(ITOL.NE.1) IOFF=IOFF+2
      IF(LRCORN.NE.0) IOFF=IOFF+4
      DSM=XBKK(IOFF+1)
      SLCON=XBKK(IOFF+9)
      SLX=XBKK(IOFF+17)
      DX=XBKK(IOFF+25)
      ICROSS=0
      MAMB=0
      IF(NRHT(KX).GE.IBKK(19).AND.IBKK(20).NE.0) MAMB=1
      IF(
     - MAMB.NE.0
     -)THEN
      JT=IKX
      IKX=KX
      CALL LFRT(LRC)
      IF(
     - LRC.NE.0
     -)THEN
      IF(LAND(LBL(KX),MSKCR1).NE.0) LRC=-LRC
      IF(LRC.NE.LR1) ICROSS=1
      ENDIF
      IKX=JT
      ENDIF
      EPS=-.00001
      IF(
     - MAMB.EQ.0.OR.MAMB.NE.0.AND.LRC.EQ.0
     -)THEN
      IF(DSEX.LT.0..AND.SL2(KX).LT.EPS.OR.DSEX.LT.DX.AND.
     * DS2(KX).LT.DX.AND.SL2(KX).LT.EPS) ICROSS=1
      ENDIF
      DTMP=DSEX-DS2(KX)
      IF(ICROSS.EQ.1) DTMP=DSEX+DS2(KX)
      IF(
     - ABS(DTMP).LT.DSM
     -)THEN
      IF(ICROSS.EQ.1) LR1=-LR1
      INTFLG=0
      IKX=0
      ICFIT=IBFIT
      IBFIT=-1
      IF(
     - HNTCEL(ICX+1)-HNTCEL(ICX) .GT. 1
     -)THEN
      NTRLX1=HNTCEL(ICX)
      NTRLX2=HNTCEL(ICX+1)-1
      DO 13014 KK=NTRLX1,NTRLX2
      IF(
     - HUSE(KK).EQ.0
     -)THEN
      IW=NWR1(KK)
      IF(
     - IW.GE.ILBOT
     -)THEN
      CALL  INTJN1(KK,KX,INTFLG,DT)
      IF(
     - INTFLG.NE.0
     -)THEN
      GOTO 13015
      ENDIF
      ENDIF
      ENDIF
13014 CONTINUE
13015 CONTINUE
      ENDIF
      IF(
     - LRCORN.NE.0.AND.INTFLG.EQ.0
     -)THEN
      LRY=LR1
      IF(
     - LRCORN.EQ.1
     -)THEN
      LR1=1
      ICT=ICX-1
      IF(ICT.LT.LFTCL(KRING)) ICT=ICT+NCELL(KRING)
      ENDIF
      IF(
     - LRCORN.EQ.-1
     -)THEN
      LR1=2
      ICT=ICX+1
      IF(ICT.GT.LSTCL(KRING)) ICT=ICT-NCELL(KRING)
      ENDIF
      IF(
     - HNTCEL(ICT+1)-HNTCEL(ICT).GT.0
     -)THEN
      A=TRKAR(KX,8)
      DS=TRKAR(KX,7)
      IW=ITRKAR(KX,6)
      IUDFLG=3
      ILIM=ILOUT
      KT=KX
      CALL SIDE1
      ENDIF
      LR1=LRY
      ENDIF
      IBFIT=ICFIT
      IF(
     - INTFLG.EQ.0.AND.IKX.EQ.0
     -)THEN
      ASSIGN 17043 TO IZZZ14
      GOTO 17042
17043 CONTINUE
      ENDIF
      ENDIF
      GOTO IZZZ10
17037 CONTINUE
      ASSIGN 17044 TO IZZZ11
      GOTO 17031
17044 CONTINUE
      IF(
     - KRING.EQ.2
     -)THEN
      IF(
     - LAND(ICL,1).EQ.1
     -)THEN
      KR=3
      IN1=19
      IN2=21
      IN3=20
      JR=KRING
      IN4=10
      IN5=12
      IN6=13
      ASSIGN 17046 TO IZZZ15
      GOTO 17045
17046 CONTINUE
      ENDIF
      IF(
     - LAND(ICL,1).NE.1
     -)THEN
      KR=KRING
      IN1=11
      IN2=13
      IN3=12
      JR=3
      IN4=18
      IN5=20
      IN6=21
      ASSIGN 17047 TO IZZZ15
      GOTO 17045
17047 CONTINUE
      ENDIF
      ELSE
      KR=KRING
      IN1=15
      IN2=17
      IN3=16
      JR=KRING
      IN4=14
      IN5=16
      IN6=17
      ASSIGN 17048 TO IZZZ15
      GOTO 17045
17048 CONTINUE
      ENDIF
      GOTO IZZZ13
17045 CONTINUE
      IF(
     - LRCORN.EQ.1
     -)THEN
      DSMX=DTWICE(NWR2(KX)+1,KR,1)
      X1=DBCK(IN1)
      IF(
     - LR.EQ.0.OR.LR.EQ.1
     -)THEN
      X2=DBCK(IN2)
      LRS=1
      ASSIGN 17049 TO IZZZ12
      GOTO 17033
17049 CONTINUE
      ASSIGN 17051 TO IZZZ16
      GOTO 17050
17051 CONTINUE
      DSEX=DSMX-XQ
      ENDIF
      IF(LR.EQ.0) DSEXR=DSEX
      IF(LR.EQ.0) SLEXR=SLEX
      IF(
     - LR.EQ.0.OR.LR.EQ.-1
     -)THEN
      X2=DBCK(IN3)
      LRS=-1
      ASSIGN 17052 TO IZZZ12
      GOTO 17033
17052 CONTINUE
      ASSIGN 17053 TO IZZZ16
      GOTO 17050
17053 CONTINUE
      DSEX=DSMX+XQ
      ENDIF
      IF(LR.EQ.0) DSEXL=DSEX
      IF(LR.EQ.0) SLEXL=SLEX
      ELSE
      DSMX=DTWICE(NWR2(KX)+1,JR,2)
      X1=DBCK(IN4)
      IF(
     - LR.EQ.0.OR.LR.EQ.1
     -)THEN
      X2=DBCK(IN5)
      LRS=1
      ASSIGN 17054 TO IZZZ12
      GOTO 17033
17054 CONTINUE
      ASSIGN 17055 TO IZZZ16
      GOTO 17050
17055 CONTINUE
      DSEX=DSMX+XQ
      ENDIF
      IF(LR.EQ.0) DSEXR=DSEX
      IF(LR.EQ.0) SLEXR=SLEX
      IF(
     - LR.EQ.0.OR.LR.EQ.-1
     -)THEN
      X2=DBCK(IN6)
      LRS=-1
      ASSIGN 17056 TO IZZZ12
      GOTO 17033
17056 CONTINUE
      ASSIGN 17057 TO IZZZ16
      GOTO 17050
17057 CONTINUE
      DSEX=DSMX-XQ
      ENDIF
      IF(LR.EQ.0) DSEXL=DSEX
      IF(LR.EQ.0) SLEXL=SLEX
      ENDIF
      GOTO IZZZ15
17042 CONTINUE
      IF(
     - KRING.EQ.2.OR.LRCORN.NE.0
     -)THEN
      SL=SLEX
      IF(LRT*LR1.EQ.1) SL=-SL
      IF(ICROSS.EQ.1) SL=-SL
      T=DBCK(6)
      IF(KRING.EQ.2.AND.LRCORN.NE.0) T=DBCK(7)
      IF(KRING.EQ.1.AND.LRCORN.NE.0) T=TANDEL(2)
      IF(LAND(ICL,1).EQ.1.AND.LRCORN.EQ.1.OR.
     * LAND(ICL,1).NE.1.AND.LRCORN.EQ.-1) T=DBCK(22)
      SLEX=(T-SL)/(1.+SL*T)
      ENDIF
      IF(ICROSS.EQ.1) SLEX=-SLEX
      SL=SL2(KX)/RINCR(KRING)
      SLC=SLCOR(SL,LR1)
      SLTMP=SLC-SLEX
      SLOLIM=(ABS(SLEX)+ABS(SLC))/2.*SLX+SLCON
      IF(
     - ABS(SLC-SLEX) .LT. SLOLIM
     -)THEN
      IXXB=0
      IF(
     - IBKK(18).NE.0.AND.IBFIT.EQ.0
     -)THEN
      ASSIGN 17059 TO IZZZ17
      GOTO 17058
17059 CONTINUE
      ENDIF
      IF(
     - IXXB.EQ.0
     -)THEN
      ASSIGN 17061 TO IZZZ18
      GOTO 17060
17061 CONTINUE
      DTEMP(IRL)=ABS(DTMP*(SLC-SLEX))
      ENDIF
      ENDIF
      GOTO IZZZ14
17060 CONTINUE
      IRIFLG=1
      IF(
     - IRL.LT.10
     -)THEN
      IRL=IRL+1
      ITK(IRL,1)=KX
      ITK(IRL,3)=LR1
      ITK(IRL,2)=LRT
      ITK(IRL,4)=K
      ELSE
      ENDIF
      GOTO IZZZ18
17025 CONTINUE
      ASSIGN 17062 TO IZZZ11
      GOTO 17031
17062 CONTINUE
      IF(
     - LAND(ICL,1).EQ.1
     -)THEN
      LR1=-1
      DSMX=DHALF(NWR2(KX)+1,KRING,1)
      IF(
     - LR.EQ.-1.OR.LR.EQ.0
     -)THEN
      LRS=-1
      ASSIGN 17063 TO IZZZ12
      GOTO 17033
17063 CONTINUE
      X1=DBCK(5)
      X2=DBCK(8)
      ASSIGN 17064 TO IZZZ16
      GOTO 17050
17064 CONTINUE
      DSEX=DSMX+XQ
      ENDIF
      IF(LR.EQ.0) DSEXL=DSEX
      IF(LR.EQ.0) SLEXL=SLEX
      IF(
     - LR.EQ.1.OR.LR.EQ.0
     -)THEN
      LRS=1
      ASSIGN 17065 TO IZZZ12
      GOTO 17033
17065 CONTINUE
      X1=DBCK(5)
      X2=DBCK(9)
      ASSIGN 17066 TO IZZZ16
      GOTO 17050
17066 CONTINUE
      DSEX=DSMX-XQ
      ENDIF
      IF(LR.EQ.0) DSEXR=DSEX
      IF(LR.EQ.0) SLEXR=SLEX
      ENDIF
      IF(
     - LAND(ICL,1).NE.1
     -)THEN
      LR1=1
      DSMX=DHALF(NWR2(KX)+1,KRING,2)
      IF(
     - LR.EQ.-1.OR.LR.EQ.0
     -)THEN
      LRS=-1
      ASSIGN 17067 TO IZZZ12
      GOTO 17033
17067 CONTINUE
      X1=DBCK(4)
      X2=DBCK(9)
      ASSIGN 17068 TO IZZZ16
      GOTO 17050
17068 CONTINUE
      DSEX=DSMX-XQ
      ENDIF
      IF(LR.EQ.0) DSEXL=DSEX
      IF(LR.EQ.0) SLEXL=SLEX
      IF(
     - LR.EQ.1.OR.LR.EQ.0
     -)THEN
      LRS=1
      ASSIGN 17069 TO IZZZ12
      GOTO 17033
17069 CONTINUE
      X1=DBCK(4)
      X2=DBCK(8)
      ASSIGN 17070 TO IZZZ16
      GOTO 17050
17070 CONTINUE
      DSEX=DSMX+XQ
      ENDIF
      IF(LR.EQ.0) DSEXR=DSEX
      IF(LR.EQ.0) SLEXR=SLEX
      ENDIF
      GOTO IZZZ09
17033 CONTINUE
      SLC1=SLCOR(SL1K,LRS)
      IF(LAND(LBL(K),MSKCR1).NE.0.AND.LAND(LBL(K),MSKCR2).EQ.0)LRS=-LRS
      SLC2=SLCOR(SL2K,LRS)
      IF(
     - LAND(LBL(K),MSKCR1).NE.0.AND.LAND(LBL(K),MSKCR2).EQ.0
     -)THEN
      SLC2=ABS(SLC2)
      IF(SLC1.LT.0.) SLC2=-SLC2
      ENDIF
      FCONT=(SLC2-SLC1)/(NWR2(K)-NWR1(K))/RINCR(KRING+1)
      IF(ABS(FCONT).GT..001.AND.NRHT(K).LE.8) FCONT=FCONT/2.
      SLEX=SLC1-FCONT*(W3-W2)
      GOTO IZZZ12
17031 CONTINUE
      SL1K=SL1(K)/RINCR(KRING+1)
      SL2K=SL2(K)/RINCR(KRING+1)
      D=DS1(K)
      W3=FSENSW(KRING+1)+NWR1(K)*RINCR(KRING+1)
      W2=FSENSW(KRING)+NWR2(KX)*RINCR(KRING)
      GOTO IZZZ11
17050 CONTINUE
      SL=SL1K-FCONT*.5*(W3-W2)
      X=D*DRICOS/X1
      XQ=W3-(W2*DRICOS+D*X2)/X1
      XQ=X-SL*DRICOS*XQ/(X1-SL*X2)
      GOTO IZZZ16
17015 CONTINUE
      IF(
     - LAND(LBL(K),MSKERR).NE.0.AND.LAND(LBL(K),MSKLR0).EQ.0
     -)THEN
      DO 13016 KTR=1,100
      ITMM=HNREL(KTR)
      IF(
     - ITMM.GT.0
     -)THEN
      JA=HISTR(ITMM,KTR)
      IF(
     - IABS(JA).EQ.K
     -)THEN
      LR=ISIGN(1,JA)
      GOTO 13017
      ENDIF
      ELSE
      GOTO 13017
      ENDIF
13016 CONTINUE
13017 CONTINUE
      ENDIF
      GOTO IZZZ07
17058 CONTINUE
      IXXB=0
      IF(
     - HNREL(NTR).LT.9
     -)THEN
      CALL MVC2(HTEMP(1),0,HISTR(1,NTR),0,18) !PMF 28/06/99 MVC -> MVC2
      IKFLG=IJFLG
      IF(II.EQ.2) CALL COREC
      IJFLG=IKFLG
      HNREL(NTR)=HNREL(NTR)+1
      LRC=LR1
      IF(LAND(LBL(KX),MSKCR1).NE.0) LRC=-LRC
      HISTR(HNREL(NTR),NTR)=KX*LRC
      IF(LR.EQ.0.AND.LRT.EQ.-1) HISTR(1,NTR)=-HISTR(1,NTR)
      IAB=HNREL(NTR)
      CALL BAKFIT(IXXB,2)
      HNREL(NTR)=HNREL(NTR)-1
      CALL MVC2(HISTR(1,NTR),0,HTEMP(1),0,18) !PMF 28/06/99 MVC -> MVC2
      ENDIF
      GOTO IZZZ17
      END
