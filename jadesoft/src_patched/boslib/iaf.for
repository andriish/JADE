C   07/06/96 606071848  MEMBER NAME  IAF      (S4)          FORTG1
      FUNCTION IAF(NB,IW)
      INTEGER IL(2),IB/4H    /
      LOGICAL*1LL(8)
      EQUIVALENCE(IL(1),LL(1))
      DATA IL(2)/0/
      NL=0
      NC=IABS(NB)
      IF(NC.NE.0) GOTO 10
      IL(1)=IB
      GOTO 100
   10 IL(1)=IW
      DO 30 I=1,4
      LL(8)=LL(5-I)
      IF(IL(2).NE.64) GOTO 30
      NL=I
      KL=MOD(NC,10)
      IF(KL.EQ.0.AND.NC.EQ.0) GOTO 20
      IL(2)=240+KL
      LL(5-I)=LL(8)
   20 NC=NC/10
   30 CONTINUE
      IF(NB.GT.0) GOTO 100
      IF(NL.EQ.0) GOTO 100
      IL(2)=96
      LL(5-NL)=LL(8)
  100 IAF=IL(1)
      RETURN
      END
