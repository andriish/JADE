      SUBROUTINE BSIDE
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
      DIMENSION LSTCL(3),LFTCL(3),NCELL(3),TANDEL(3)
      EQUIVALENCE (IBCK(1),LSTCL(1)),(IBCK(4),LFTCL(1))
      EQUIVALENCE (IBCK(7),NCELL(1)),(DBCK(1),TANDEL(1))
      DATA MSKCR1 /Z100/
      LRCORN=0
      KT=K
      IRIFLG=0
      IWT=ILIM-1
      ICX=ICL
15000 CONTINUE
      IF(
     - IWT.LT.ILIM
     -)THEN
      DS=TRKAR(KT,10-IUDFLG)
      IW=ITRKAR(KT,9-IUDFLG)
      IKX=0
      A=TRKAR(KT,11-IUDFLG)
      ASSIGN 17001 TO IZZZ01
      GOTO 17000
17001 CONTINUE
      ICT=ICX+1
      IF(ICT.GT.LSTCL(KRING)) ICT=ICT-NCELL(KRING)
      LR1=2
      IF(
     - IPER.GE.0
     -)THEN
      IF(
     - HNTCEL(ICT+1)-HNTCEL(ICT).GT.0
     -)THEN
      ASSIGN 17003 TO IZZZ02
      GOTO 17002
17003 CONTINUE
      IF(
     - LR.EQ.1.OR.IMARK.EQ.0.OR.LR.EQ.0
     -)THEN
      CALL SIDE1
      ELSE
      IWT=ILIM
      ENDIF
      ELSE
      IWT=ILIM
      ENDIF
      ELSE
      IWT=ILIM
      ENDIF
      IF(
     - IKX.NE.0
     -)THEN
      IF(
     - LR.EQ.-1
     -)THEN
      CALL COREC
      ENDIF
      LR=-1
      IWT=HMCH(ITRKAR(IKX,9-IUDFLG)+1,KRING,2)
      IF(IUDFLG.EQ.6) IWT=-IWT
      CALL BSTORE
      IPST=0
      LR=1
      IJFLG=0
      ENDIF
      IF(
     - IKX.EQ.0
     -)THEN
      ICT=ICX-1
      IF(ICT.LT.LFTCL(KRING)) ICT=ICT+NCELL(KRING)
      LR1=1
      IF(
     - IPER.LE.0
     -)THEN
      IF(
     - HNTCEL(ICT+1)-HNTCEL(ICT).GT.0
     -)THEN
      ASSIGN 17004 TO IZZZ02
      GOTO 17002
17004 CONTINUE
      IF(
     - LR.EQ.-1.OR.IMARK.EQ.0.OR.LR.EQ.0
     -)THEN
      CALL SIDE1
      ELSE
      IWT=ILIM
      ENDIF
      ELSE
      IWT=ILIM
      ENDIF
      ELSE
      IWT=ILIM
      ENDIF
      IF(
     - IKX.NE.0
     -)THEN
      IF(
     - LR.EQ.0
     -)THEN
      HISTR(1,NTR)=-HISTR(1,NTR)
      ENDIF
      IF(
     - LR.EQ.1
     -)THEN
      CALL COREC
      ENDIF
      LR=1
      IWT=HMCH(ITRKAR(IKX,9-IUDFLG)+1,KRING,1)
      IF(IUDFLG.EQ.6) IWT=-IWT
      CALL BSTORE
      IPST=0
      IJFLG=0
      LR=-1
      ENDIF
      ELSE
      IWT=ILIM
      ENDIF
      GOTO 15000
      ENDIF
15001 CONTINUE
      RETURN
17002 CONTINUE
      IMARK=0
      KC1=HISTR(1,NTR)
      KC1=IPCL(IABS(KC1))
      KC2=HISTR(HNREL(NTR),NTR)
      KC2=IPCL(IABS(KC2))
      IF(
     - KC1.NE.KC2
     -)THEN
      IF(
     - KC1.LE.48
     -)THEN
      IF(KC1-KC2.NE.24) IMARK=1
      ELSE
      IMARK=1
      ENDIF
      ENDIF
      GOTO IZZZ02
17000 CONTINUE
      IPER=0
      IF(
     - IBKK(20).NE.0.AND.LR.NE.0.AND.NRHT(KT).GE.IBKK(19)
     -)THEN
      IPER=LR
      IF(LAND(MSKCR1,LBL(KT)).NE.0.AND.IUDFLG.EQ.3) IPER=-IPER
      ENDIF
      GOTO IZZZ01
      END
