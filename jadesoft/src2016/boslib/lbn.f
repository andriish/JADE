C   07/06/96 606071853  MEMBER NAME  LBN      (S4)          FORTRAN
      FUNCTION LBN(NX,X)
      COMMON/BCS/IW(1)
      REAL RW(1)
      EQUIVALENCE (RW(1),IW(1))
      LB=0
      CALL BLOC(J,'$BIN',NX,&100)
    5 NJ=MIN0(IW(J),4)+1
      GOTO (100,100,10,15,40),NJ
   10 NB=100
      XB=100.0
      GOTO 20
   15 NB=IW(J+3)
      IF(NB.GT.1000.OR.NB.LE.0) GOTO 10
      XB=NB
   20 LB=XB*(X-RW(J+1))/(RW(J+2)-RW(J+1))+1.0
      IF(LB.GT.NB) GOTO 30
      IF(LB.GE.0)  GOTO 100
   25 LB=0
      GOTO 100
   30 LB=NB+1
      GOTO 100
   40 NB=IW(J)-1
      NL=J+1
      NH=J+IW(J)
      IF(X.LT.RW(NL)) GOTO 25
      IF(X.GE.RW(NH)) GOTO 30
   50 LB=(NL+NH)/2
      IF(LB.EQ.NL) GOTO 60
      IF(X.LT.RW(LB)) GOTO 55
      NL=LB
      GOTO 50
   55 NH=LB
      GOTO 50
   60 LB=LB-J
      GOTO 100
C
      ENTRY LLBN(NX,X)
      J=NX
      GOTO 5
  100 LBN=LB
      RETURN
      END
