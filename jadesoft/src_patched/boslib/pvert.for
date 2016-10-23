C   07/06/96 606071905  MEMBER NAME  PVERT    (S4)          FORTRAN
      SUBROUTINE PVERT(X,N,NN)
C     PURPOSE
C        PRINT THE ARRAY X OF DIMENSION N (MAX 120) IN IABS(NN) LINES
C
C     USAGE
C        CALL PVERT(X,N,NN)
C        IABS(NN)    =NUMBER OF DIGITS (3 TO 6)
C        NN NEGATIVE = PRINT NO INDEXLINE BELOW ARRAY
      COMMON/CCONVT/JM,PX(32,9)
      REAL X(1)
      INTEGER PX,BLK/'    '/,PE/'  E '/,PM/'   .'/
      character*1 LX(128,8),CHM/'-'/,CH(12)/'0','1','2',
     1     '3','4','5','6','7','8','9',' ','*'/
      EQUIVALENCE (PX(1,1),LX(1,1))
C
      JM=0
      DO 10 I=1,9
      DO 10 J=1,32
   10 PX(J,I)=BLK
      IF(N.LE.0) GOTO 100
      M=MIN0(120,N)
C
      JL=0
      XM=0.0
      DO 20 J=1,M
      IF(ABS(X(J)).LE.XM) GOTO 14
      XM=ABS(X(J))
      MX=J
   14 IF(X(J)) 16,20,18
   16 LX(8+J,1)=CHM
   18 JL=J
   20 CONTINUE
      IF(XM.EQ.0.0) GOTO 100
      IF(JL.LE.0) GOTO 100
      KN=IABS(NN)
      KN=MAX0(3,KN)
      KN=MIN0(6,KN)
      KE=DLOG10(DBLE(XM))
      IF(XM.LT.1.0) KE=KE-1
   22 FAC=1.D1**(KN-1-KE)
      IJ=FAC*XM+0.5
      IF(IJ.LT.10**KN) GOTO 24
      KE=KE+1
      GOTO 22
   24 IA=2+KN
C
      DO 40 J=1,JL
      IJ=FAC*ABS(X(J))+0.5
      IM=0
      IF(IJ.EQ.0) GOTO 40
      DO 30 I=1,KN
      IF(IJ.EQ.0) GOTO 40
      IH=IJ/10
      IN=IJ-10*IH
      IF(IN.NE.0.AND.IM.EQ.0) IM=IA-I+1
      LX(8+J,IA-I)=CH(IN+1)
   30 IJ=IH
   40 JM=MAX0(IM,JM)
      KL=KE
   50 IF(KE.LT.KN) GOTO 55
      KE=KE-3
      GOTO 50
   55 IF(KE.GE.0) GOTO 60
      KE=KE+3
      GOTO 55
   60 IN=KE+2
      IZ=KL-KE
      PX(2,IN)=PM
      PX(1,IN)=PE
      IF(IZ.GE.0) GOTO 70
      LX(4,IN)=CHM
      IZ=-IZ
   70 I1=IZ/10
      I2=MOD(IZ,10)
      LX(5,IN)=CH(I1+1)
      LX(6,IN)=CH(I2+1)
      JM=MIN0(2+KN,JM)
      JM=MAX0(IN+1,JM)
C
      DO 80 J=1,JL
      IF(X(J).NE.0.0.AND.LX(8+J,JM-1).EQ.CH(11)) LX(8+J,JM-1)=CH(1)
   80 CONTINUE
      DO 85 I=JM,8
      DO 85 J=1,27
   85 PX(J,I)=BLK
      DO 90 J=1,JL
      LX(8+J,JM)=CHM
      IF(MOD(J,2).EQ.1) GOTO 90
      I=MOD(J,10)+1
      LX(8+J,JM+1)=CH(I)
      IF(I.NE.1) GOTO 90
      I=MOD(J/10,10)+1
      LX(8+J,JM)=CH(I)
   90 CONTINUE
      LX(8+MX,JM)=CH(12)
      JM=JM+1
      IF(NN.LT.0) JM=JM-2
      DO 95 J=1,JM
   95 WRITE(6,101) (PX(I,J),I=1,32)
C
  100 RETURN
  101 FORMAT(3X,32A4)
      END
