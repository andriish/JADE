C   07/06/96 606071853  MEMBER NAME  KBN      (S4)          FORTG1
      FUNCTION KBN(X,XAR,NB)
      REAL XAR(1)
      KBN=0
      NL=1
      NH=NB
      NF=NH
      IF(X.LT.XAR(NL)) GOTO 100
      IF(X.GE.XAR(NH)) GOTO 30
   10 NF=(NL+NH)/2
      IF(NL.EQ.NF) GOTO 30
      IF(X.LT.XAR(NF)) GOTO 20
      NL=NF
      GOTO 10
   20 NH=NF
      GOTO 10
   30 KBN=NF
  100 RETURN
      END
