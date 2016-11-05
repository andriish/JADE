      SUBROUTINE SRTREL
      IMPLICIT INTEGER*2 (H)
      LOGICAL TBIT
C----------------------------------------------------------------------
C             MACRO CDATA .... BOS COMMON.
C
C             THIS MACRO ONLY DEFINES THE IDATA/HDATA/ADATA NAMES.
C             THE ACTUAL SIZE OF /BCS/ IS FIXED ON MACRO CBCSMX
C             OR BY OTHER MEANS. A DEFAULT SIZE OF 40000 IS GIVEN HERE.
C
C----------------------------------------------------------------------
C
      COMMON /BCS/ IDATA(40000)
      DIMENSION HDATA(80000),ADATA(40000),IPNT(50)
      EQUIVALENCE (HDATA(1),IDATA(1),ADATA(1)),(IPNT(1),IDATA(55))
      EQUIVALENCE (NWORD,IPNT(50))
C
C------------------------ END OF MACRO CDATA --------------------------
C----------------------------------------------------------------------
C           MACRO CCYCP .... JET CHAMBER HIT POINTERS (PATREC)
C----------------------------------------------------------------------
      INTEGER*4 HPTSEC
      COMMON/CCYCP/HPTSEC(98)
C     HPTSEC(I) = CDATA POINTER TO 1ST I*2 WORD FOR 1ST HIT OF CELL I
C------------------------ END OF MACRO CCYCP --------------------------
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
      EQUIVALENCE
     ,           (ICELL ,IDWRK(1)),(NHIT  ,IDWRK(2)),(IRING ,IDWRK(3))
     ,         , (IERRCD,IDWRK(4)),(NTRKEL,IDWRK(5))
     ,         , (ITR   ,IDWRK(7)),(ITRNG ,IDWRK(8))
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
      DATA MKTREL / Z3000/
      DATA MKLBTC / ZFFF/
      DATA MKLFTR / Z 30000/, MKRGHT / Z20000/
      INTEGER  MKLRHT(3) / Z800, Z0, Z900/
      DATA LBZRCR / Z100/
      DATA NANF /0/
      IF(NANF.EQ.0) IQPATR= IBLN('PATR')
      NANF = 1
      IPPATR = IDATA(IQPATR)
          HPFRE0 = HPFREE
          CALL PRHTAR
      IF(
     - NHIT.GE.5
     -)THEN
            CALL FLINEL
            CALL FTRKEL
      IF(
     - NTRKEL.GT.0
     -)THEN
      ASSIGN 17001 TO IZZZ01
      GOTO 17000
17001 CONTINUE
      ENDIF
            NLINUC = 0
      DO 13000 IP=HPHT0,HPHT9,HLDHT
              IF(IWRK(IP+9).EQ.0 .AND. LAND(IWRK(IP+4),7).NE.0)
     .        NLINUC = NLINUC + 1
13000 CONTINUE
13001 CONTINUE
            IDATA(IPPATR+7) = IDATA(IPPATR+7) + NLINUC
      ENDIF
          HPFREE = HPFRE0
      RETURN
17000 CONTINUE
        ICLL16 = (ICELL-1) * 16
        NTRCLL = 0
      DO 13002 IPTR = HPTR0,HPTR9,HLDTR
      IF(
     - ITR.LE.200 .AND. ITRNG.LT.128
     -)THEN
      IF(
     - LAND(IWRK(IPTR+15),MKTREL).NE.0
     -)THEN
            NTRCLL = NTRCLL + 1
            LB = LAND(IWRK(IPTR+15),MKLBTC)
            DRSP1 = WRK(IPTR+ 8)
            DRSP2 = WRK(IPTR+11)
            IF(DRSP1*DRSP2.LT.0) LB = LOR(LB,LBZRCR)
            LBLR = LAND(IWRK(IPTR+15),MKLFTR)
            IF(DRSP1.LT.0..AND.LBLR.NE.0.AND.LBLR.NE.MKLFTR)
     .         LBLR = LXOR(LBLR,MKLFTR)
            LB = LOR(LB,ISHFTR(LBLR,6))
            ITRKAR(ITR, 1) = ICELL
            ITRKAR(ITR, 2) = IWRK(IPTR+ 2)
            ITRKAR(ITR, 3) = IWRK(IPTR+ 7)
            TRKAR (ITR, 4) = ABS(DRSP1)
            DRSL1          = WRK(IPTR+ 9)
            IF(DRSP1.LT.0.)  DRSL1 =-DRSL1
            TRKAR (ITR, 5) = DRSL1
            ITRKAR(ITR, 6) = IWRK(IPTR+10)
            TRKAR (ITR, 7) = ABS(DRSP2)
            DRSL2          = WRK(IPTR+12)
            IF(DRSP2.LT.0.)  DRSL2 =-DRSL2
            TRKAR (ITR, 8) = DRSL2
            ITRKAR(ITR, 9) = LB
            ITRKAR(ITR,10) = ITRNG
            IDXLR =-1
            IF(LBLR.EQ.MKRGHT) IDXLR = 1
            IF(DRSP1.LT.0.) IDXLR =-IDXLR
            IDXLR1 = IDXLR
            ILAYZ =-1
      DO 13004 IP=HPHT0,HPHT9,HLDHT
      IF(
     - IWRK(IP+9).EQ.IPTR .OR.IWRK(IP+10).EQ.IPTR
     -)THEN
                DRSP = WRK(IP+2)
            IF(IWRK(IP+10).EQ.IPTR .AND. TBIT(IWRK(IP+4),20)) DRSP=-DRSP
                IF(DRSP.NE.0.) IDXLR1 = IDXLR
      IF(
     - DRSP*DRSP1.LT.0.
     -)THEN
                  IDXLR1 =-IDXLR
                  IF(ILAYZ.LT. 0) ILAYZ = IWRK(IP)
      ENDIF
                IPLBHT = (IWRK(IP+1) - HPTSEC(1))/2 + HPHL0
                LBHIT = ITRNG*2
                LBHIT = LOR(LBHIT,MKLRHT(IDXLR1+2))
      IF(
     - HWRK(IPLBHT).EQ.0
     -)THEN
                  HWRK(IPLBHT) = LBHIT
      ELSE
                  IF(HWRK(IPLBHT+1).EQ.0) HWRK(IPLBHT+1) = LBHIT
      ENDIF
      ENDIF
13004 CONTINUE
13005 CONTINUE
            IF(LAND(LB,LBZRCR).NE.0 .AND. ILAYZ.LT.0)
     .                                    ILAYZ = IWRK(IPTR+10) + 1
            IF(ILAYZ.LT.0) ILAYZ = 0
            ITRKAR(ITR,11) = ILAYZ
            ITR   = ITR   + 1
            ITRNG = ITRNG + 1
      ENDIF
      ENDIF
13002 CONTINUE
13003 CONTINUE
      GOTO IZZZ01
      END
