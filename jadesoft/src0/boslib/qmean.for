C   07/06/96 606071905  MEMBER NAME  QMEAN    (S4)          FORTG1
      SUBROUTINE QMEAN(NA,KA,Q)
      COMMON/BCS/IW(1)
      REAL RW(1)
      EQUIVALENCE (RW(1),IW(1))
      REAL Q(10),XL(5),YL(5)
      REAL*8 S(4)
      LOGICAL NADD
      DO 10 I=1,10
   10 Q(I)=0.0
      IF(KA.LT.0) GOTO 100
      CALL BLOC(IND,'MEA*',NA,&100)
      INDK=IND+50*KA
      IF(INDK.GE.IND+IW(IND)) GOTO 100
      N=IW(INDK+1)
      IF(N.EQ.0) GOTO 100
      IF(N.GE.46) GOTO 20
      CALL HMEAN(INDK,Q)
      GOTO 100
   20 SUM=IW(INDK+1)-IW(INDK+4)
      BIN=(RW(INDK+3)-RW(INDK+2))/46.0
      XA=RW(INDK+2)
      XL(1)=SUM*0.1585
      XL(2)=SUM*0.23
      XL(3)=SUM*0.5
      XL(4)=SUM*0.77
      XL(5)=SUM*0.8415
      DO 25 I=1,4
   25 S(I)=0.0
      XB=XA
      SUM=0.0
      NADD=.TRUE.
      DO 40 I=1,46
      DC=IW(INDK+4+I)
      XZ=XB+BIN
      IF(DC.EQ.0.0) GOTO 35
      S(1)=S(1)+DC*(XB+XZ)*0.5
      S(2)=S(2)+DC
      IF(NADD) GOTO 26
      S(3)=S(3)+DC*(XB+XZ)*0.5
      S(4)=S(4)+DC
   26 DO 30 J=1,5
      IF(SUM.GE.XL(J)) GOTO 30
      IF(SUM+DC.LT.XL(J)) GOTO 30
      FR=(XL(J)-SUM)/DC
      YL(J)=XB+FR*BIN
      IF(MOD(J,2).NE.0) GOTO 30
      IF(J.EQ.4) FR=-1.0+FR
      S(3)=S(3)+FR*DC*(XB+XZ)*0.5
      S(4)=S(4)+FR*DC
      IF(J.EQ.2) NADD=.FALSE.
      IF(J.EQ.4) NADD=.TRUE.
   30 CONTINUE
   35 SUM=SUM+DC
   40 XB=XZ
      Q(1)=N
      Q(2)=S(1)/S(2)
      Q(3)=S(3)/S(4)
      Q(4)=YL(3)
      Q(6)=YL(3)-YL(1)
      Q(7)=YL(5)-YL(3)
      Q(5)=0.5*(Q(6)+Q(7))
      Q(8)=IW(INDK+4)
      Q(9)=RW(INDK+2)
      Q(10)=RW(INDK+3)
  100 RETURN
      END
