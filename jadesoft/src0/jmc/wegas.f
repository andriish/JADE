C   20/01/83 606061351  MEMBER NAME  WEGAS    (S)           FORTRAN
      SUBROUTINE WEGAS(FXN,BCC,NCALL,ITMX,XL,XU,AVGI,SD,CHI2A)
C
C     SUBROUTINE PERFORMS N-DIMENSIONAL MONTE CARLO INTEGRATION
C     - BY PETER LEPAGE  SEPT 1976/(REV)APR 1978
C
C     COPIED FROM T00VER.AXOLIB.S       02/04/82
C     CHANGED                           20/06/82
C
C     ****                                                          ****
C     ****  THIS IS A MODIFIED VERSION OF THE ORIGINAL VEGAS        ****
C     ****  ROUTINE IN 'T00VER.AXOLIB.S'                            ****
C     ****  THE MAIN CHANGES ARE :                                  ****
C     ****  - ONLY FOR 3-DIMENSIONAL INTEGRATION                    ****
C     ****  - ALL REDUNDANT STATEMENTS REMOVED TO OBTAIN FASTER     ****
C     ****    EXECUTION                                             ****
C     ****  - NO DIVIDE CHECKS FOR INTEGRATION REGIONS WITH 'KINK'  ****
C     ****  - REDUCED NUMBER OF INPUT PARAMETERS                    ****
C     ****                                                          ****
C
      DIMENSION D(50,3),DI(50,3),XIN(50),R(50),DX(3),IA(3)
     1,KG(3),DT(3),XI(50,3)
      DIMENSION XL(3),XU(3)
      DIMENSION QRAN(3),X(3)
      DATA NDMX,ONE,MDS/50,1.,1/
      DATA XI /150*1./
      DSQRT(XXX)=SQRT(XXX)
      DLOG(XXX)=ALOG(XXX)
      NDO=1
      DO 1 J=1,3
    1 XI(1,J)=ONE
      ENTRY VEGAS1(FXN,BCC,NCALL,ITMX,XL,XU,AVGI,SD,CHI2A)
C      -INITIALIZES CUMMULATIVE VARIABLES, BUT NOT GRID
      IT=0
      SI=0.
      SI2=SI
      SWGT=SI
      SCHI=SI
      SCALLS=SI
      ENTRY VEGAS2(FXN,BCC,NCALL,ITMX,XL,XU,AVGI,SD,CHI2A)
C      - NO INITIALIZATION
      ND=NDMX
      NG=1
      IF(MDS.EQ.0)GO TO 2
      NG=(NCALL/2.)**.333
      MDS=1
      IF((2*NG-NDMX).LT.0)GO TO 2
      MDS=-1
      NPG=NG/NDMX+1
      ND=NG/NPG
      NG=NPG*ND
    2 K=NG*NG*NG
      NPG=NCALL/K
      IF(NPG.LT.2)NPG=2
      CALLS=NPG*K
      DXG=ONE/NG
      DV2G=DXG*DXG*DXG*DXG*DXG*DXG/NPG/NPG/(NPG-ONE)
      XND=ND
      NDM=ND-1
      DXG=DXG*XND
      XJAC=ONE
      DO 3 J=1,3
      DX(J)=XU(J)-XL(J)
    3 XJAC=XJAC*DX(J)
      IF(ND.EQ.NDO)GO TO 8
      RC=NDO/XND
      DO 7 J=1,3
      K=0
      XN=0.
      DR=XN
      I=K
    4 K=K+1
      DR=DR+ONE
      XO=XN
      XN=XI(K,J)
    5 IF(RC.GT.DR)GO TO 4
      I=I+1
      DR=DR-RC
      XIN(I)=XN-(XN-XO)*DR
      IF(I.LT.NDM)GO TO 5
      DO 6 I=1,NDM
    6 XI(I,J)=XIN(I)
    7 XI(ND,J)=ONE
      NDO=ND
      ACC=BCC
    8 CONTINUE
      ENTRY VEGAS3(FXN,BCC,NCALL,ITMX,XL,XU,AVGI,SD,CHI2A)
C      - MAIN INTEGRATION LOOP
    9 IT=IT+1
      ITS=IT
      TI=0.
      TSI=TI
      DO 10 J=1,3
      KG(J)=1
      DO 10 I=1,ND
      D(I,J)=TI
   10 DI(I,J)=TI
   11 FB=0.
      F2B=FB
      K=0
   12 K=K+1
      DO 112 J=1,3
  112 QRAN(J)=RN(0)
      WGT=XJAC
      DO 15 J=1,3
      XN=(KG(J)-QRAN(J))*DXG+ONE
      IA(J)=XN
      IAJ=IA(J)
      IF(IAJ.GT.1)GO TO 13
      XO=XI(IAJ,J)
      RC=(XN-IA(J))*XO
      GO TO 14
   13 XO=XI(IAJ,J)-XI(IAJ-1,J)
      RC=XI(IAJ-1,J)+(XN-IAJ)*XO
   14 X(J)=XL(J)+RC*DX(J)
   15 WGT=WGT*XO*XND
      F=FXN(X)*WGT
      F1=F/CALLS
      W=WGT/CALLS
      F2=F*F
      FB=FB+F
      F2B=F2B+F2
      DO 16 J=1,3
      IAJ=IA(J)
      DI(IAJ,J)=DI(IAJ,J)+F/CALLS
   16 IF(MDS.GE.0)D(IAJ,J)=D(IAJ,J)+F2
      IF(K.LT.NPG)GO TO 12
      F2B=DSQRT(F2B*NPG)
      F2B=(F2B-FB)*(F2B+FB)
      TI=TI+FB
      TSI=TSI+F2B
      IF(MDS.GE.0)GO TO 18
      DO 17 J=1,3
      IAJ=IA(J)
   17 D(IAJ,J)=D(IAJ,J)+F2B
   18 K=3
   19 KG(K)=MOD(KG(K),NG)+1
      IF(KG(K).NE.1)GO TO 11
      K=K-1
      IF(K.GT.0)GO TO 19
      TI=TI/CALLS
      TSI=TSI*DV2G
C
C     CHECK FOR TSI
C
      IF(TSI.GT.0.) GOTO 29
      AVGI = 0.
      SD = 0.
      CHI2A = 0.
      RETURN
C
   29 TI2=TI*TI
      WGT=TI2/TSI
      SI=SI+TI*WGT
      SI2=SI2+TI2
      SWGT=SWGT+WGT
      SCHI=SCHI+TI2*WGT
      SCALLS=SCALLS+CALLS
      AVGI=SI/SWGT
      SD=SWGT*IT/SI2
      CHI2A=0.
      IF(IT.GT.1)CHI2A=SD*(SCHI/SWGT-AVGI*AVGI)/(IT-.999)
      SD=DSQRT(ONE/SD)
      SDAV=SD/AVGI
      IF(ABS(SDAV).LE.ACC.OR.IT.GE.ITMX)NOW=2
      DO 23 J=1,3
      XO=D(1,J)
      XN=D(2,J)
      D(1,J)=(XO+XN)/2.
      DT(J)=D(1,J)
      DO 22 I=2,NDM
      D(I,J)=XO+XN
      XO=XN
      XN=D(I+1,J)
      D(I,J)=(D(I,J)+XN)/3.
   22 DT(J)=DT(J)+D(I,J)
      D(ND,J)=(XN+XO)/2.
   23 DT(J)=DT(J)+D(ND,J)
      DO 28 J=1,3
      RC=0.
      DO 24 I=1,ND
      R(I)=0.
      IF(D(I,J).LE.0)GO TO 24
      XO=DT(J)/D(I,J)
      R(I)=((XO-ONE)/XO/DLOG(XO))
   24 RC=RC+R(I)
      RC=RC/XND
      K=0
      XN=0
      DR=XN
      I=K
   25 K=K+1
      DR=DR+R(K)
      XO=XN
      XN=XI(K,J)
   26 IF(RC.GT.DR)GO TO 25
      I=I+1
      DR=DR-RC
      XIN(I)=XN-(XN-XO)*DR/R(K)
      IF(I.LT.NDM)GO TO 26
      DO 27 I=1,NDM
   27 XI(I,J)=XIN(I)
   28 XI(ND,J)=ONE
      SDAV=SD/AVGI
      IF(IT.LT.ITMX.AND.ACC.LT.ABS(SDAV))GO TO 9
      RETURN
      END
