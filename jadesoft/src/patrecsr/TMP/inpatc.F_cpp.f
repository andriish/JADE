      SUBROUTINE INPATC
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
      DATA HHHRUN /-1/
      IPHE = IDATA(IBLN('HEAD'))
      HHRUN = HDATA(2*IPHE+10)
      IF(HHRUN.EQ.HHHRUN) RETURN
      HHHRUN = HHRUN
      NRUN = HHRUN
      CALL ADPATR( NRUN )
      DFI0 = 3.1415927 / 12.
      DFI1 = 3.1415927 / 24.
      DFI3 = 3.1415927 / 48.
      FI = DFI1
      DO 13000 I=1,6
        DX = COS(FI)
        DY = SIN(FI)
        DIRWR1(I   ,1) = DX
        DIRWR1(I   ,2) = DY
        DIRWR1(I+ 6,1) =-DY
        DIRWR1(I+ 6,2) = DX
        DIRWR1(I+12,1) =-DX
        DIRWR1(I+12,2) =-DY
        DIRWR1(I+18,1) = DY
        DIRWR1(I+18,2) =-DX
        FI = FI + DFI0
13000 CONTINUE
13001 CONTINUE
      FI = DFI3
      DO 13002 I=1,12
        DX = COS(FI)
        DY = SIN(FI)
        DIRWR3(I   ,1) = DX
        DIRWR3(I   ,2) = DY
        DIRWR3(I+12,1) =-DY
        DIRWR3(I+12,2) = DX
        DIRWR3(I+24,1) =-DX
        DIRWR3(I+24,2) =-DY
        DIRWR3(I+36,1) = DY
        DIRWR3(I+36,2) =-DX
        FI = FI + DFI1
13002 CONTINUE
13003 CONTINUE
      DO 13004 I=1,96
      K = I
      IF(K.GT.24) K=K-24
      IF(
     - I.LE.48
     -)THEN
      DX=DIRWR1(K,1)
      DY=DIRWR1(K,2)
      ELSE
      DX=DIRWR3(K-24,1)
      DY=DIRWR3(K-24,2)
      ENDIF
        TRMATS(I,1) = -( DY * COSDRI(I,1) + DX * SINDRI(I,1) )
        TRMATS(I,2) = -( DY * COSDRI(I,2) + DX * SINDRI(I,2) )
        TRMATC(I,1) =  ( DX * COSDRI(I,1) - DY * SINDRI(I,1) )
        TRMATC(I,2) =  ( DX * COSDRI(I,2) - DY * SINDRI(I,2) )
13004 CONTINUE
13005 CONTINUE
      FACTL1 = SIN(DFI1) / COS(DFI1+DRIDEV)
      FACTR1 = SIN(DFI1) / COS(DFI1-DRIDEV)
      FACTL3 = SIN(DFI3) / COS(DFI3+DRIDEV)
      FACTR3 = SIN(DFI3) / COS(DFI3-DRIDEV)
      R1 = FSENSW(1)
      R2 = FSENSW(2)
      R3 = FSENSW(3)
      DO 13006 ILAY=1,16
        DSMAX(ILAY,1,1) = R1 * FACTL1
        DSMAX(ILAY,2,1) = R2 * FACTL1
        DSMAX(ILAY,3,1) = R3 * FACTL3
        DSMAX(ILAY,1,2) = R1 * FACTR1
        DSMAX(ILAY,2,2) = R2 * FACTR1
        DSMAX(ILAY,3,2) = R3 * FACTR3
        R1 = R1 + RINCR(1)
        R2 = R2 + RINCR(2)
        R3 = R3 + RINCR(3)
13006 CONTINUE
13007 CONTINUE
      CALL INBACK
      RETURN
      END
      SUBROUTINE INBACK
      IMPLICIT INTEGER*2 (H)
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
       DIMENSION LFTCL(3),LSTCL(3),NCELL(3),TANDEL(3)
       EQUIVALENCE (IBCK(1),LSTCL(1)),(IBCK(4),LFTCL(1))
       EQUIVALENCE (IBCK(7),NCELL(1)),(DBCK(1),TANDEL(1))
      DIMENSION GB(3)
      DATA GB/376.,586.,797./
      NCELL(1)=24
      NCELL(2)=24
      NCELL(3)=48
      LSTCL(1)=24
      LSTCL(2)=48
      LSTCL(3)=96
      LFTCL(1)=1
      LFTCL(2)=25
      LFTCL(3)=49
      DEL=3.141593/48.
      DBCK(4)=COS(DEL-DRIDEV)
      DBCK(5)=COS(DEL+DRIDEV)
      DBCK(6)=TAN(DEL)
      DBCK(7)=TAN(DEL+3.141593/24.)
      DBCK(8)=SIN(DEL)
      DBCK(9)=-DBCK(8)
      DEL=DEL+3.141593/24.
      DBCK(10)=COS(DEL-DRIDEV)
      DBCK(11)=COS(DEL+DRIDEV)
      DBCK(12)=SIN(DEL)
      DBCK(13)=-DBCK(12)
      DEL=6.283185/24.
      DBCK(14)=COS(DEL-DRIDEV)
      DBCK(15)=COS(DEL+DRIDEV)
      DBCK(16)=SIN(DEL)
      DBCK(17)=-DBCK(16)
      DEL=DEL+3.141593/48.
      DBCK(18)=COS(DEL-DRIDEV)
      DBCK(19)=COS(DEL+DRIDEV)
      DBCK(20)=SIN(DEL)
      DBCK(21)=-DBCK(20)
      DBCK(22)=TAN(3.141593/12.+3.141593/48.)
      DO 13000 KRING=1,3
      DEL=3.141593/NCELL(KRING)
      TANDEL(KRING)=TAN(DEL*2.)
      DO 13002 IW=1,16
      DSMX=DSMAX(IW,KRING,1)
      IF(KRING.EQ.3) DSMSP=DSMAX(IW,2,1)
      ALSIN=-DRISIN
      AL=-DRIDEV
      ASSIGN 17001 TO IZZZ01
      GOTO 17000
17001 CONTINUE
      HMCH(IW,KRING,1)=IK
      DHALF(IW,KRING,1)=DT
      DTWICE(IW,KRING,1)=DTW
      DSMX=DSMAX(IW,KRING,2)
      IF(KRING.EQ.3) DSMSP=DSMAX(IW,2,2)
      ALSIN=DRISIN
      AL=DRIDEV
      ASSIGN 17002 TO IZZZ01
      GOTO 17000
17002 CONTINUE
      HMCH(IW,KRING,2)=IK
      DHALF(IW,KRING,2)=DT
      DTWICE(IW,KRING,2)=DTW
13002 CONTINUE
13003 CONTINUE
13000 CONTINUE
13001 CONTINUE
      RETURN
17000 CONTINUE
      W=FSENSW(KRING)+FLOAT(IW-1)*RINCR(KRING)
      X=SQRT(W**2+DSMX**2-2.*W*DSMX*ALSIN)
      DTW=1.-TAN(DEL)*TAN(DEL-AL)
      DTW=2.*DSMX/DTW
      IF(
     - KRING.EQ.2
     -)THEN
      DTW=1.-TAN(DEL*.5)*TAN(DEL-AL)
      DTW=DSMX*(1.+TAN(DEL*.5)/TAN(DEL))/DTW
      ENDIF
      IF(
     - KRING.EQ.3
     -)THEN
      DTW=1.-TAN(DEL*1.5)*TAN(DEL-AL)
      DTW=DSMSP*(1.+TAN(DEL*1.5)/TAN(DEL))/DTW
      ENDIF
      DT=TAN(DEL*0.5)*TAN(DEL*0.5-AL)
      DT=DSMX*0.5*(1.-DT)
      IF(
     - X.LT.GB(KRING)
     -)THEN
      IK=-5
      E=X*COS(AL+DEL)/DRICOS
      E=E-FSENSW(KRING)
      IF(E.GE.-5.) IK=(E+5.)/RINCR(KRING)
      IF(IK.GT.15) IK=20
      ELSE
      IK=20
      ENDIF
      GOTO IZZZ01
      END
