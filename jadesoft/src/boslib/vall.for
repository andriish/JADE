C   07/06/96 606071918  MEMBER NAME  VALL     (S4)          FORTG1
      SUBROUTINE VALL(X,L,AAW,ASCHR,NB)
      DIMENSION X(100),Y(100),XL(2),XF(2)
      DIMENSION DX(2)
      REAL    BINS(10)/0.1,0.15,0.2,0.25,0.4,0.5,0.6,0.8,1.0,1.5/
      REAL*8 XA,XZ,BN,BIN,FEX,SCHR,AW,TEN
      REAL*8 DN
      TEN=10
      N=MIN0(100,L)
      DO 10 I=1,N
   10 Y(I)=X(I)
      CALL SORT4(Y,N)
      XM=Y(N/2+1)
      XL(1)=XM-Y(1)
      XL(2)=Y(N)-XM
      NT=N/10
      IF(NT.LT.1) NT=1
      XF(1)=XM-Y(NT)
      XF(2)=Y(N+1-NT)-XM
      DO 20 I=1,2
      DX(I)=AMIN1(2.*XF(I),XL(I))
   20 CONTINUE
      IF(DX(1)+DX(2).GT.0.0) GOTO 30
      DO 22 I=1,2
   22 DX(I)=AMAX1(1.E-4,ABS(XM)*1.E-4)
   30 XA=XM-DX(1)
      XZ=XM+DX(2)
      BN=NB
      BIN=(XZ-XA)/BN
      IF(BIN.LE.0.0) BIN=1.E-4
      DN=DLOG10(BIN)
      ND=DN
      FEX=TEN**ND
   32 IF(BIN/FEX.LE.1.0) GOTO 34
      FEX=FEX*TEN
   34 BIN=BIN/FEX
      DO 40 I=1,9
      KI=I
      IF(BINS(I).GE.BIN) GOTO 50
   40 CONTINUE
      KI=9
   50 BIN=BINS(KI)
      SCHR=BIN*FEX
      AW=IDINT(XA/(TEN*SCHR))*SCHR*TEN
      IF(XA-AW.LT.0.0) AW=AW-SCHR*TEN
      IF(AW+BN*SCHR.GE.XZ) GOTO 100
      KI=KI+1
      IF(KI.LE.10) GOTO 50
  100 AAW=AW
      ASCHR=SCHR
      RETURN
      END
