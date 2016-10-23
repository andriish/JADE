      SUBROUTINE BSTORE
      IMPLICIT INTEGER*2 (H)
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
      IF(HNREL(NTR).EQ.9) RETURN
      HNREL(NTR) = HNREL(NTR) + 1
      KR = HNREL(NTR)
      HISTR(KR,NTR) = IKX*LR
      HUSE(IKX) = 1
      RETURN
      END
      FUNCTION SLCOR(SL,LRS)
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
      IF(LRS.EQ.-1)SLCOR=DRICOS*SL/(1.+SL*DRISIN)
      IF(LRS.EQ.1) SLCOR=DRICOS*SL/(1.-SL*DRISIN)
      RETURN
      END
      SUBROUTINE CHOOSE
      IMPLICIT INTEGER*2 (H)
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
      ICHOOS=1
      IT=IRL-1
15000 CONTINUE
      IF(
     - ICHOOS.EQ.1
     -)THEN
      DO 13000 I=1,IT
      ITMP=I+1
      DO 13002 J=ITMP,IRL
      IF(
     - DTEMP(I).GT.DTEMP(J)
     -)THEN
      TEMP=DTEMP(I)
      DTEMP(I)=DTEMP(J)
      DTEMP(J)=TEMP
      DO 13004 JK=1,4
      TEMP=ITK(I,JK)
      ITK(I,JK)=ITK(J,JK)
      ITK(J,JK)=TEMP
13004 CONTINUE
13005 CONTINUE
      ENDIF
13002 CONTINUE
13003 CONTINUE
13000 CONTINUE
13001 CONTINUE
      IP=ITK(1,4)
      IC=ITK(1,1)
      IF(
     - DTEMP(1).NE.100.
     -)THEN
      IF(
     - IP.NE.K
     -)THEN
      DO 13006 J=1,IRL
      IF(
     - DTEMP(J).NE.100.
     -)THEN
      IF(ITK(J,4).EQ.IP.OR.ITK(J,1).EQ.IC) DTEMP(J)=100.
      ENDIF
13006 CONTINUE
13007 CONTINUE
      ELSE
      GOTO 15001
      ENDIF
      ELSE
      GOTO 15001
      ENDIF
      GOTO 15000
      ENDIF
15001 CONTINUE
      IF(
     - DTEMP(1).NE.100.
     -)THEN
      ITWO=2
      ITHREE=ITWO+1
      ELSE
      IRIFLG=0
      ENDIF
      RETURN
      END
      SUBROUTINE INTJN1(KK,KX,INTFLG,DTMP)
      IMPLICIT INTEGER*2 (H)
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
      DIMENSION HTEMP(9)
      DATA MSKCR1 /Z100/,MSKFIT/Z20000/,MSKAIT/ZFFFDFFFF/
      MAMB=0
      IF(NRHT(KK).GE.IBKK(19).AND.NRHT(KX).GE.IBKK(19)
     * .AND.IBKK(20).NE.0) MAMB=1
      INTFLG=0
      IWX=ITRKAR(KX,6)
      IOL=IBKK(1)-1
      IGAP=IBKK(2)+1
      IF(
     - IWX.LE.IW+IOL.AND.IWX.GE.IW-IGAP
     -)THEN
      ICROSS=1
      SLA=SL1(KK)/RINCR(KRING)
      SLB=SL2(KX)/RINCR(KRING)
      JT=IKX
      IKX=KX
      CALL LFRT(LRA)
      IKX=KK
      CALL LFRT(LR2)
      IKX=JT
      IF(IUDFLG.EQ.3.AND.LR.NE.0.AND.LRA.EQ.0) LRA=LR
      IF(IUDFLG.EQ.6.AND.LR.NE.0.AND.LR2.EQ.0) LR2=LR
      IDIW=IW-IWX
      IF(IDIW.LT.0) IDIW=0
      IF(
     - MAMB.NE.0.AND.LAND(MSKCR1,LBL(KK)).EQ.0.AND.
     ¢ LAND(MSKCR1,LBL(KX)).EQ.0
     -)THEN
      IF(LRA.NE.LR2) ICROSS=-1
      IF(LRA.EQ.0.OR.LR2.EQ.0) ICROSS=0
      ENDIF
      DSEX=TRKAR(KK,4)-SL1(KK)*IDIW
      DX=DSEX-TRKAR(KX,7)
      IF(
     - MAMB.EQ.0.OR.ICROSS.EQ.0
     -)THEN
      IF(SL2(KX).LT.0..AND.SL1(KK).GT.0..AND.LAND(LBL(KK),MSKCR1).EQ.0
     * .AND.LAND(LBL(KX),MSKCR1).EQ.0.AND.(DS1(KK).LT.BKK(5)
     ¢ .OR.DS2(KX).LT.BKK(5))) ICROSS=-1
      ENDIF
      IF(
     - ICROSS.EQ.-1
     -)THEN
      SLA=-SLA
      DX=TRKAR(KX,7)+DSEX
      ENDIF
      IF(
     - LAND(LBL(KK),MSKCR1).NE.0.AND.LAND(LBL(KX),MSKCR1).NE.0
     -)THEN
      DX=TRKAR(KX,7)+DSEX
      SLA=-SLA
      IF(MAMB.NE.0.AND.LRA*LR2.LT.0) DX=1000000.
      ENDIF
      IF(ICROSS.EQ.0) ICROSS=1
      IF(LAND(MSKCR1,LBL(KK)).EQ.0.AND.LAND(MSKCR1,LBL(KX)).NE.0
     * .AND.LRA*LR2.GT.0.AND.MAMB.NE.0) DX=10000000.
      IF(LAND(MSKCR1,LBL(KX)).EQ.0.AND.LAND(MSKCR1,LBL(KK)).NE.0
     * .AND.LRA*LR2.LT.0.AND.MAMB.NE.0) DX=10000000.
      IF(
     - ABS(DX).LT.DCELL
     -)THEN
      SLOLIM=(ABS(SLA)+ABS(SLB))/2.*BKK(14)+BKK(15)
      DTMP=ABS(SLA-SLB)
      IF(
     - DTMP.LT.SLOLIM
     -)THEN
      IB=0
      IF(
     - IBKK(16).NE.0.AND.IBFIT.EQ.0
     -)THEN
      ASSIGN 17001 TO IZZZ01
      GOTO 17000
17001 CONTINUE
      ENDIF
      IF(
     - IB.EQ.0
     -)THEN
      INTFLG=ICROSS
      DTMP=ABS(DTMP*DX)
      ENDIF
      ENDIF
      KMP1=HISTR(1,NTR)
      KMP1=IABS(KMP1)
      KMP1=IPCL(KMP1)
      IF(KMP1.EQ.IPCL(KX).AND.IBKK(20).NE.0.AND.MAMB.EQ.0) IJFLG=1
      ENDIF
      ENDIF
      RETURN
17000 CONTINUE
      IB=0
      IF(
     - HNREL(NTR).LT.9
     -)THEN
      CALL MVC2(HTEMP(1),0,HISTR(1,NTR),0,18) !PMF 28/06/99 MVC -> MVC2
      IKST=IPST
      IKFLG=IJFLG
      IBJ=IKX
      IKX=KX
      LR3=1
      IF(ICROSS.EQ.-1.OR.(LAND(LBL(KX),MSKCR1).NE.0.AND.
     * LAND(LBL(KK),MSKCR1).EQ.0)) LR3=-1
      ASSIGN 17003 TO IZZZ02
      GOTO 17002
17003 CONTINUE
      HNREL(NTR)=HNREL(NTR)+1
      LRC=LR2
      HISTR(HNREL(NTR),NTR)=LRC*KX
      CALL BAKFIT(IB,1)
      IAB=HNREL(NTR)
      HNREL(NTR)=HNREL(NTR)-1
      CALL MVC2(HISTR(1,NTR),0,HTEMP(1),0,18) !PMF 28/06/99 MVC -> MVC2
      IKX=IBJ
      IJFLG=IKFLG
      IPST=IKST
      ENDIF
      GOTO IZZZ01
17002 CONTINUE
      CALL LFRT(LR2)
      IF(
     - LR2.EQ.0
     -)THEN
      IF(
     - LR.NE.0
     -)THEN
      LR2=LR*LR3
      ELSE
      LR2=LR3
      IJFLG=1
      IPST=1
      ENDIF
      ELSE
      IF(
     - LR.EQ.0
     -)THEN
      IF(LR2*LR3.EQ.-1) HISTR(1,NTR)=-HISTR(1,NTR)
      ELSE
      IF(
     - LR2.NE.LR*LR3
     -)THEN
      LR2=-LR2
      ENDIF
      ENDIF
      ENDIF
      GOTO IZZZ02
      END
      SUBROUTINE COREC
      IMPLICIT INTEGER*2 (H)
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
      ITMP=HNREL(NTR)
      DO 13000 I=1,ITMP
      HISTR(I,NTR)=-HISTR(I,NTR)
13000 CONTINUE
13001 CONTINUE
      IJFLG=0
      RETURN
      END
      SUBROUTINE SIDE1
      IMPLICIT INTEGER*2 (H)
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
C----------------------------------------------------------------------
C           MACRO CDSMAX .... PATTERN RECOGNITION CONSTANTS.
C----------------------------------------------------------------------
      COMMON/CDSMAX/DSMAX(16,3,2),DIRWR1(24,2),DIRWR3(48,2)
     *             ,DHALF(16,3,2),DTWICE(16,3,2),HMCH(16,3,2)
     *             ,IBCK(9),DBCK(30),TRMATS(96,2),TRMATC(96,2)
C------------------------ END OF MACRO CDSMAX -------------------------
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
      DIMENSION LSTCL(3),LFTCL(3),NCELL(3),TANDEL(3)
      EQUIVALENCE (IBCK(1),LSTCL(1)),(IBCK(4),LFTCL(1))
      EQUIVALENCE (IBCK(7),NCELL(1)),(DBCK(1),TANDEL(1))
      DIMENSION HTEMP(9)
      DATA MSKCR1 /Z100/,MSKFIT/Z20000/,MSKAIT/ZFFFDFFFF/
      IIWW=IW+1
      IF(IIWW.GT.16) IIWW=16
      IF(IIWW.LT.1) IIWW=1
      BSL=A
      IF(IUDFLG.EQ.6) BSL=-BSL
      DRIFT=DS+.5*BSL
      IRT=LR1
      IM1=6
      IM2=3
      ASSIGN 17001 TO IZZZ01
      GOTO 17000
17001 CONTINUE
      IWEX=HMCH(IIWW,KRING,LR1)+1
      IF(IWEX.GT.16) IWEX=16
      IF(IWEX.LT.1) IWEX=1
      IF(
     - DTMP.LT.2.*CLIM.OR.DTMP.LT.2.
     -)THEN
      ASSIGN 17003 TO IZZZ02
      GOTO 17002
17003 CONTINUE
      IF(
     - IKX.GT.0
     -)THEN
      IF(
     - LRCORN.EQ.0
     -)THEN
      ICX=ICT
      IRIFLG=1
      KT=IKX
      ENDIF
      ELSE
      IWT=ILIM
      ENDIF
      ELSE
      IWT=ILIM
      ENDIF
      RETURN
17002 CONTINUE
      IKX=0
      ICNFLG=0
      IF(LRCORN.EQ.0) IRL=0
      NTRLX1 = HNTCEL(ICT)
      NTRLX2 = HNTCEL(ICT+1)-1
      DO 13000 KX = NTRLX1,NTRLX2
      IF(
     - HUSE(KX).EQ.0
     -)THEN
      IPER=0
      IF(
     - IBKK(20).NE.0.AND.NRHT(KX).GE.IBKK(19)
     -)THEN
      JT=IKX
      IKX=KX
      CALL LFRT(LK)
      IKX=JT
      IF(LAND(MSKCR1,LBL(KX)).NE.0.AND.IUDFLG.EQ.6) LK=-LK
      IF(LK.NE.3-2*LR1) IPER=1
      IF(LK.EQ.0) IPER=0
      ENDIF
      IF(
     - IPER.EQ.0
     -)THEN
      IIWW= ITRKAR(KX,IUDFLG)+1
      IF(IIWW.GT.16) IIWW=16
      IF(IIWW.LT.1) IIWW=1
      BSL=TRKAR(KX,IUDFLG+2)
      IF(IUDFLG.EQ.3) BSL=-BSL
      DRIFT=TRKAR(KX,IUDFLG+1)+.5*BSL
      IRT=3-LR1
      IM1=3
      IM2=6
      ASSIGN 17004 TO IZZZ01
      GOTO 17000
17004 CONTINUE
      IWX=IIWW
      IF(
     - DTMP.LT.2.*CLIM.OR.DTMP.LT.2.
     -)THEN
      IF(
     - IABS(IWX-IWEX).LE.IBKK(6)-1
     -)THEN
      LRS=2*LR1-3
      SLB=A/RINCR(KRING)
      SL=SLCOR(SLB,LRS)
      T=TANDEL(KRING)
      SLEX=(T-SL)/(1.+SL*T)
      LRS=-LRS
      SLE=TRKAR(KX,IUDFLG+2)/RINCR(KRING)
      SLC=SLCOR(SLE,LRS)
      SLOLIM=(ABS(SLE)+ABS(SLB))/2.*BKK(12)+BKK(13)
      DTMP=SLEX-SLC
      IF(
     - ABS(DTMP).LT.SLOLIM
     -)THEN
      IF(
     - LRCORN.NE.0
     -)THEN
      IKX=KX
      GOTO 13001
      ENDIF
      IB=0
      IF(
     - IBKK(17).NE.0.AND.IBFIT.EQ.0
     -)THEN
      ASSIGN 17006 TO IZZZ03
      GOTO 17005
17006 CONTINUE
      ENDIF
      IF(
     - IB.EQ.0
     -)THEN
      IRL=IRL+1
      DTEMP(IRL)=ABS(DTMP)
      ITK(IRL,1)=KX
      ITK(IRL,4)=KT
      ICNFLG=1
      ELSE
      ICNFLG=0
      ENDIF
      ELSE
      ICNFLG=0
      ENDIF
      IF(IRL.GT.0.AND.LRCORN.EQ.0) ICNFLG=1
      ENDIF
      ENDIF
      ELSE
      ENDIF
      ENDIF
13000 CONTINUE
13001 CONTINUE
      IF(
     - ICNFLG.EQ.1
     -)THEN
      IF(
     - IRL.GT.1
     -)THEN
      CALL CHOOSE
      ENDIF
      IKX=ITK(1,1)
      ENDIF
      GOTO IZZZ02
17000 CONTINUE
      CLIM=.5*BSL
      DXNEW=DSMAX(IIWW,KRING,IRT)-DRIFT
      DXOLD=9999.
15000 CONTINUE
      IF(
     - DXNEW.GT.CLIM
     -)THEN
      IF(
     - IIWW.EQ.16.OR.IIWW.EQ.1
     -)THEN
      GOTO 15001
      ENDIF
      DRIFT=DRIFT+BSL
      IF(IUDFLG.EQ.IM1) IIWW=IIWW-1
      IF(IUDFLG.EQ.IM2) IIWW=IIWW+1
      DSMX=DSMAX(IIWW,KRING,IRT)
      DXNEW=DSMX-DRIFT
      IF(
     - ABS(DXNEW).GT.ABS(DXOLD)
     -)THEN
      DTMP=100.
      GOTO 15001
      ENDIF
      IF(
     - DXNEW.LT.1.
     -)THEN
      GOTO 15001
      ELSE
      DXOLD=DXNEW
      ENDIF
      GOTO 15000
      ENDIF
15001 CONTINUE
      DTMP=DXNEW
      GOTO IZZZ01
17005 CONTINUE
      IB=0
      IF(
     - HNREL(NTR).LT.9
     -)THEN
      IKFLG=IJFLG
      CALL MVC2(HTEMP(1),0,HISTR(1,NTR),0,18)  !PMF 28/06/99 MVC -> MVC2
      IF(LR.EQ.-1.AND.LR1.EQ.2.OR.LR.EQ.1.AND.LR1.EQ.1) CALL COREC
      IF(LR.EQ.0.AND.LR1.EQ.1) HISTR(1,NTR)=-HISTR(1,NTR)
      HNREL(NTR)=HNREL(NTR)+1
      IF(LR1.EQ.2) LRC=-1
      IF(LR1.EQ.1) LRC=1
      IF(LAND(LBL(KX),MSKCR1).NE.0.AND.IUDFLG.EQ.6) LRC=-LRC
      IKRA=HISTR(1,NTR)
      IKRA=IABS(IKRA)
      IF(IKRA.EQ.KT.AND.IUDFLG.EQ.3.AND.LAND(MSKCR1,LBL(KT)).NE.0)
     * CALL COREC
      HISTR(HNREL(NTR),NTR)=KX*LRC
      IAB=HNREL(NTR)
      CALL BAKFIT(IB,3)
      HNREL(NTR)=HNREL(NTR)-1
      CALL MVC2(HISTR(1,NTR),0,HTEMP(1),0,18) !PMF 28/06/99 MVC -> MVC2
      IJFLG=IKFLG
      ENDIF
      GOTO IZZZ03
      END
      SUBROUTINE LFRT(LR2)
      IMPLICIT INTEGER*2 (H)
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
      DATA MSKLFT,MSKRT/Z400,Z800/
      LR2=0
      IPST=0
      I1=LAND(LBL(IKX),MSKLFT)
      I2=LAND(LBL(IKX),MSKRT)
      IF(I1.NE.0) LR2=-1
      IF(I2.NE.0) LR2=1
      IF(I1*I2.NE.0) LR2=0
      IF(LR2.NE.0) IPST=1
      RETURN
      END
