      SUBROUTINE PRHTAR
      IMPLICIT INTEGER*2 (H)
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
     ,            (ICELL,IDWRK(1)),(NHIT,IDWRK(2)),(IRING,IDWRK(3))
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
      DATA LBINIT /0/
      IF(
     - LBINIT .EQ. 0
     -)THEN
        LBINIT = 0
        IQJHTL = IBLN('JHTL')
        DSD0   =-.63
        DSD1   = 1.8
        DSD2   = 4.0
        DRV0   = 0.8
        DRV1   = (DSD2 - (DSD1-DSD0)*DRV0) / (DSD2-DSD1)
      ENDIF
      DSBIN1 = TIMDEL(1,IRING)
      DSBIN2 = TIMDEL(2,IRING)
      IP0 = HPTSEC(ICELL)
      IP9 = HPTSEC(ICELL+1) - 1
      IPHL = (HPTSEC(ICELL)-HPTSEC(1))/4 + IDATA(IQJHTL) + 1
      HPHT0 = HPFREE
      HLDHT = 12
      IPHT  = HPHT0
      JHIT = 0
      NWRD2 = NWORD*2
      DO 13000 IP=IP0,IP9,4
        IPHL = IPHL + 1
      IF(
     - IDATA(IPHL).EQ.0
     -)THEN
      IF(
     - HDATA(IP+1).GT.0 .AND. HDATA(IP+2).GT.0
     -)THEN
          IWIR = HDATA(IP)
          IWIR = ISHFTR(IWIR,3)
          ILAYR = LAND(IWIR,15)
          IF(ILAYR.GE.8) DSBIN1 = DSBIN2
          DS =(HDATA(IP+3)) * DSBIN1
      IF(
     - DS.LT.4.0
     -)THEN
      IF(
     - DS.GT.DSD1
     -)THEN
              DS = (DSD1-DSD0)*DRV0 + (DS-DSD1)*DRV1
      ELSE
              DS = (DS-DSD0)*DRV0
      ENDIF
            IF(DS.LT.0.1) DS = 0.1
      ENDIF
          WRK(IPHT+2)  = AMAX1(DS,0.)
          IWRK(IPHT  ) = ILAYR
          IWRK(IPHT+1) = IP
          IWRK(IPHT+4) = 0
          IWRK(IPHT+5) = 0
          IWRK(IPHT+6) = 0
          IWRK(IPHT+7) = 0
          IPHT = IPHT + HLDHT
          JHIT = JHIT + 1
      ENDIF
      ENDIF
13000 CONTINUE
13001 CONTINUE
      NHIT = JHIT
      HPHT9 = IPHT - 1
      HPFREE= IPHT
      RETURN
      END
