C   07/06/96 606071842  MEMBER NAME  DEFST    (S4)          FORTG1
      SUBROUTINE DEFST(IND)
      COMMON/BCS/IW(1)
      REAL RW(1)
      EQUIVALENCE (IW(1),RW(1))
      REAL H(100)
C     CALL BY UHIST
      IF(IW(IND+20).GE.1) GOTO 100
      CALL VALL(RW(IND+21),IW(IND+2),RW(IND+3),RW(IND+4),100)
      N=IW(IND+2)
      DO 10 I=1,N
      H(I)=RW(IND+20+I)
   10 RW(IND+20+I)=0
      DO 20 I=1,N
      K=(H(I)-RW(IND+3))/RW(IND+4)+21
      IF(K.LE.20) K=5
      IF(K.GT.120) K=6
   20 RW(IND+K)=RW(IND+K)+1.0
      IW(IND+20)=1
  100 RETURN
      END
