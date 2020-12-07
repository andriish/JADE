      SUBROUTINE RSTBTR(IPJHTL)
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
      DATA MKHTL1 /Z8400/, MKATR /Z100/, MKHTL2 /Z7FFF/
      DATA  MKLRSV / Z800/
      DATA  MKBDHT / Z600/
      DATA MKLFTR,MKRTTR / Z400, Z800/
      DATA LBZRCR / Z100/
      NBYTE = IDATA(IPJHTL)*4 - 4
      CALL MVCL2(HWRK(HPHL0),0,IDATA(IPJHTL+2),0,NBYTE)!PMF 28/06/99 MVCL -> MVCL2
      HLDHL = IDATA(IPJHTL)*2 - 2
      HPHL9 = HLDHL + HPHL0 - 1
      HPFREE = (HPHL9+1)/2 + 1
      HPHT0  = HPFREE
      DO 13000 I=HPHL0,HPHL9,2
        IZW = HWRK(I  )
      IF(
     - LAND(IZW,MKBDHT).NE.0
     -)THEN
          HWRK(I) = HWRK(I+1)
          IZW     = HWRK(I+1)
          HWRK(I+1) = 0
          IF(LAND(IZW,MKBDHT).NE.0) HWRK(I  ) = 0
      ELSE
          IZW = HWRK(I+1)
          IF(LAND(IZW,MKBDHT).NE.0) HWRK(I+1) = 0
      ENDIF
13000 CONTINUE
13001 CONTINUE
      NBYTE = 200
      CALL SETSL(HNREL(1),0,NBYTE,0)
      NTR   = 0
      HNTR  = 0
      ICELL = 1
      ITREL = 0
      ITRBK = 0
16000 CONTINUE
        IPHIT0 = HPTSEC(ICELL  )
        IPHIT9 = HPTSEC(ICELL+1) - 4
        IF(ICELL.EQ.25) ITREL = 0
        IF(ICELL.EQ.49) ITREL = 0
        IRING = 1
        IF(ICELL.GE.25) IRING = 2
        IF(ICELL.GE.49) IRING = 3
        DSBIN1 = TIMDEL(1,IRING)
        DSBIN2 = TIMDEL(2,IRING)
      IF(
     - IPHIT9.GE.IPHIT0
     -)THEN
          IPHTL0 = (IPHIT0-HPTSEC(1))/2 + HPHL0
16002 CONTINUE
            IPHIT = IPHIT0
            IPHTL = IPHTL0
            ITR   = 0
            DSBIN = DSBIN1
            NHT   = 0
            IPHTW = HPHT0
            ZCRLB1 = 0.
            ZCRLB2 = 0.
15000 CONTINUE
      IF(
     - IPHIT.LE.IPHIT9
     -)THEN
              LHTL1 = HWRK(IPHTL  )
              LHTL2 = HWRK(IPHTL+1)
              NTR1  = 0
              NTR2  = 0
             IF(LAND(LHTL1,MKHTL1).EQ.0) NTR1=LAND(ISHFTR(LHTL1,1),127)
             IF(LAND(LHTL2,MKHTL1).EQ.0) NTR2=LAND(ISHFTR(LHTL2,1),127)
      IF(
     - ITR.EQ.0
     -)THEN
      IF(
     - NTR1.NE.0
     -)THEN
                  ITR = NTR1
      ELSE
                  IF(NTR2.NE.0) ITR = NTR2
      ENDIF
                IF(ITR.NE.0) ITREL = ITREL + 1
      ENDIF
      IF(
     - ITR.NE.0 .AND. (NTR1.EQ.ITR .OR. NTR2.EQ.ITR)
     -)THEN
      IF(
     - ITR.EQ.NTR1
     -)THEN
                  LBHIT = LAND(MKATR,LHTL1)
                  SGNLR =-1.
                  IF(LBHIT.NE.0) SGNLR = 1.
                  LBHIT = LOR (ITREL*2,LBHIT)
                  LBHIT = LOR (MKHTL1 ,LBHIT)
                  LBHIT = LOR (MKLRSV ,LBHIT)
                  HWRK(IPHTL  ) = LBHIT
      ENDIF
      IF(
     - ITR.EQ.NTR2
     -)THEN
                  LBHIT = LAND(MKATR,LHTL2)
                  SGNLR =-1.
                  IF(LBHIT.NE.0) SGNLR = 1.
                  LBHIT = LOR (ITREL*2,LBHIT)
                  LBHIT = LOR (MKHTL1 ,LBHIT)
                  LBHIT = LOR (MKLRSV ,LBHIT)
                  HWRK(IPHTL+1) = LBHIT
      ENDIF
                IWIR = HDATA(IPHIT  )
                ILAY = LAND(ISHFTR(IWIR,3),15)
                IF(ILAY.GE.8) DSBIN = DSBIN2
                ITAU = HDATA(IPHIT+3)
                DRSP          = ITAU*DSBIN*SGNLR
                IF(ZCRLB1.EQ.0. .AND. DRSP.NE.0.) ZCRLB1 = SIGN(1.,DRSP)
                IF(ZCRLB1*DRSP.LT.0.) ZCRLB2 =-ZCRLB1
      IF(
     - ZCRLB2*DRSP.LT.0.
     -)THEN
                  HWRK(IPHTL  ) = LHTL1
                  HWRK(IPHTL+1) = LHTL2
      GOTO 15001
      ENDIF
                NHT = NHT + 1
                IWRK(IPHTW  ) = ILAY
                WRK (IPHTW+1) = DRSP
                IPHTW = IPHTW + 2
      ENDIF
            IPHTL = IPHTL + 2
            IPHIT = IPHIT + 4
      GOTO 15000
      ENDIF
15001 CONTINUE
      IF(
     - ITR.GT.0
     -)THEN
      ASSIGN 17001 TO IZZZ01
      GOTO 17000
17001 CONTINUE
      ENDIF
      IF(.NOT.(
     - ITR.EQ.0
     -))GOTO 16002
16003 CONTINUE
      ENDIF
      ICELL = ICELL + 1
      IF(.NOT.(
     - ICELL.GT.96
     -))GOTO 16000
16001 CONTINUE
      HNTR = ITRBK
      ICELL0 = 0
      DO 13002 ITRBK=1,HNTR
        ICELL = IPCL(ITRBK)
      IF(
     - ICELL.NE.ICELL0
     -)THEN
          ICELL0 = ICELL0 + 1
      DO 13004 IC=ICELL0,ICELL
            HNTCEL(IC) = ITRBK
13004 CONTINUE
13005 CONTINUE
          ICELL0 = ICELL
      ENDIF
13002 CONTINUE
13003 CONTINUE
      ICELL0 = ICELL0 + 1
      ITRBK = HNTR + 1
      DO 13006 IC=ICELL0,97
        HNTCEL(IC) = ITRBK
13006 CONTINUE
13007 CONTINUE
      HNTCEL(98) = 0
      CALL TRLORD
      DO 13008 I=HPHL0,HPHL9
        IZW = HWRK(I)
        IZW = LAND(IZW,MKHTL2)
        HWRK(I) = IZW
13008 CONTINUE
13009 CONTINUE
      RETURN
17000 CONTINUE
        HPHT9 = IPHTW - 1
        ITRBK = ITRBK + 1
        DS10  =  WRK(HPHT0+1)
        DS20  =  WRK(HPHT9  )
      IF(
     - NHT.LT.4
     -)THEN
      IF(
     - NHT.EQ.1
     -)THEN
            SL10 = 0.
            SL20 = 0.
      ELSE
            IDWIR =  IWRK(HPHT9-1)-IWRK(HPHT0)
            SL10  =  0.
            IF(IDWIR.NE.0) SL10  =  (DS20-DS10)/IDWIR
            SL20  =  SL10
      ENDIF
      ELSE
          SL10  =  (WRK(HPHT0+7)-DS10)/(IWRK(HPHT0+6)-IWRK(HPHT0  ))
          SL20  =  (WRK(HPHT9-6)-DS20)/(IWRK(HPHT9-7)-IWRK(HPHT9-1))
          WR10  =  (IWRK(HPHT0+6)+IWRK(HPHT0  ))*.5
          WR20  =  (IWRK(HPHT9-7)+IWRK(HPHT9-1))*.5
          DSL   =  0.
          DWR   =  WR20 - WR10
          IF(DWR.GT.0) DSL   =  (SL20-SL10) / DWR
      IF(
     - DSL.NE.0.
     -)THEN
            SL10 = SL10 + (IWRK(HPHT0  )-WR10)*DSL
            SL20 = SL20 + (IWRK(HPHT9-1)-WR20)*DSL
      ENDIF
      ENDIF
        LBTREL       = MKRTTR
        IF(DS10.LT.0.) LBTREL = MKLFTR
        IF(DS10.EQ.0 .AND. DS20.LT.0) LBTREL = MKLFTR
        ITRBKS = ITRBK
        ITRS   = ITR
      IF(
     - LBTREL.EQ.MKLFTR
     -)THEN
          ITRBKS =-ITRBK
          ITRS   =-ITR
          DS10   =-DS10
          SL10   =-SL10
          DS20   =-DS20
          SL20   =-SL20
      ENDIF
        IZRCR = 0
      IF(
     - DS10*DS20 .LT.0.
     -)THEN
          DS20 =-DS20
          SL20 =-SL20
          LBTREL = LOR(LBTREL,LBZRCR)
          DS0 = WRK(HPHT0+1)
      DO 13010 IP=HPHT0,HPHT9,2
      IF(
     - DS0*WRK(IP+1).LT.0
     -)THEN
      GOTO 13011
      ENDIF
            IZRCR = IWRK(IP)
13010 CONTINUE
13011 CONTINUE
          IZRCR = IZRCR + 1
      ENDIF
        IPCL (ITRBK) =  ICELL
        NRHT (ITRBK) =  NHT
        NWR1 (ITRBK) =  IWRK(HPHT0  )
        DS1  (ITRBK) =  DS10
        SL1  (ITRBK) =  SL10
        NWR2 (ITRBK) =  IWRK(HPHT9-1)
        DS2  (ITRBK) =  DS20
        SL2  (ITRBK) =  SL20
        LBL  (ITRBK) =  LBTREL
        NTREL(ITRBK) =  ITREL
        ICRO (ITRBK) =  IZRCR
        MTREL = HNREL(ITR)
        IF(MTREL.LT.9) MTREL = MTREL + 1
        HISTR(MTREL,ITR) = ITRBKS
        HNREL(ITR) = MTREL
        HRES(ITRBK) = ITRS
        NTR = MAX0(NTR,ITR)
      GOTO IZZZ01
      END
