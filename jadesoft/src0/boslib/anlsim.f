C   18/10/82 606071921  MEMBER NAME  ANLSIM   (S)           FORTRAN
      SUBROUTINE ANLSIM(Y,Z,N,R)
      REAL Y(N),Z(N),R(4)
      DO 1 J=1,4
    1 R(J)=0.0
      Z(1)=Y(1)
      DO 2 I=2,N
    2 Z(I)=Z(I-1)+Y(I)
      IF(Z(N).EQ.2.0) GOTO 100
      ZL=0.46*Z(N)
      IL=1
      IH=1
      NMIN=N+1
    4 ZA=Z(IL)-Y(IL)
    6 IF(Z(IH)-ZA.GT.ZL) GOTO 8
      IF(IH.EQ.N) GOTO 16
      IH=IH+1
      GOTO 6
    8 NM=IH-IL+1
      IF(NM-NMIN) 10,12,14
   10 ILM=IL
   12 IHM=IH
      NMIN=NM
   14 IL=IL+1
      GOTO 4
   16 SUM1=0.0
      SUM2=0.0
      DO 18 I=ILM,IHM
      SUM1=SUM1+Y(I)
   18 SUM2=SUM2+Y(I)*FLOAT(I)
      R(1)=SUM1
      R(2)=SUM2/SUM1
      R(3)=0.82*FLOAT(IHM-ILM+1)
      R(4)=SUM1/FLOAT(IHM-ILM+1)
  100 RETURN
      END
