C   07/06/96 606071847  MEMBER NAME  HMEAN    (S4)          FORTG1
      SUBROUTINE HMEAN(INDK,Q)
C
      COMMON/BCS/IW(1)
      REAL RW(1)
      EQUIVALENCE (RW(1),IW(1))
      REAL Q(10),Y(46),XL(2),XF(2),DX(2)
      N=IW(INDK+1)
      DO 10 I=1,N
      Y(I)=RW(INDK+4+I)
      IF(N.EQ.46) RW(INDK+4+I)=0.0
   10 CONTINUE
      CALL SORT4(Y,N)
      XM=0.5*(Y((N+1)/2)+Y(N/2+1))
      XL(1)=XM-Y(1)
      XL(2)=Y(N)-XM
      NT=MAX0(1,1+N/10)
      XF(1)=XM-Y(NT)
      XF(2)=Y(N-NT+1)-XM
      DO 20 I=1,2
      DX(I)=AMIN1(1.2*XL(I),2.0*XF(I))
      IF(DX(I).LE.0.0) DX(I)=AMAX1(1.E-4,ABS(XM)*1.E-4)
   20 CONTINUE
      XA=XM-DX(1)
      XZ=XM+DX(2)
      NREP=0
   25 NREP=NREP+1
      SA=0.0
      SB=0.0
      SC=0.0
      NA=0
      NB=0
      DO 30 I=1,N
      IF(Y(I).LT.XA.OR.Y(I).GT.XZ) GOTO 30
      SA=SA+Y(I)
      SC=SC+Y(I)*Y(I)
      NA=NA+1
   30 CONTINUE
      NT=(NA+6)/5
      J=0
      DO 32 I=1,N
      IF(Y(I).LT.XA.OR.Y(I).GT.XZ) GOTO 32
      J=J+1
      IF(J.LT.NT.OR.J.GE.NA-NT) GOTO 32
      SB=SB+Y(I)
      NB=NB+1
   32 CONTINUE
      Q(1)=N
      Q(2)=SA/FLOAT(NA)
      Q(4)=XM
      Q(3)=Q(4)
      IF(N.GT.10.AND.NB.GT.5) Q(3)=SB/FLOAT(NB)
      NT=MAX0(1,(N+5)/7)
      Q(6)=XM-Y(NT)
      Q(7)=Y(N-NT+1)-XM
      Q(5)=0.5*(Q(6)+Q(7))
      Q(8)=N-NA
      Q(9)=XA
      Q(10)=XZ
      RW(INDK+2)=XA
      RW(INDK+3)=XZ
      IF(N.LT.46) GOTO 35
      IF(Q(6)+Q(7).LE.0.0) GOTO 35
      IF(NREP.GE.2) GOTO 35
      XA=Q(4)-3.0*Q(6)
      XZ=Q(4)+3.0*Q(7)
      GOTO 25
   35 SIGS=SC-FLOAT(NA)*Q(2)*Q(2)
      SIG=0.0
      IF(NA.GT.1.AND.SIGS.GT.0.0) SIG=SQRT(SIGS/FLOAT(NA-1))
      IF(SIG.NE.0.0) Q(5)=SIG
      IF(N.LE.10) Q(6)=Q(5)
      IF(N.LE.10) Q(7)=Q(5)
      IF(N.LT.46) GOTO 100
      DO 40 I=1,N
      J=46.0*(Y(I)-XA)/(XZ-XA)+1.0
      IF(J.LE.0.OR.J.GT.46) J=0
   40 IW(INDK+4+J)=IW(INDK+4+J)+1
  100 RETURN
      END
