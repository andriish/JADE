C   07/06/96 606071853  MEMBER NAME  ITODA    (S4)          FORTG1
      SUBROUTINE ITODA(I,R,M)
      REAL*8 R,S,B/8H        /
      LOGICAL*1 LS(8),LCH(10)/1H0,1H1,1H2,1H3,1H4,1H5,1H6,1H7,1H8,1H9/
      EQUIVALENCE (S,LS(1))
      N=IABS(M)
      J=IABS(I)
      S=B
      IF(J.EQ.0.AND.M.GT.0) GOTO 100
      DO 10 L=1,N
      K=J
      J=J/10
      K=K-J*10
      LS(N+1-L)=LCH(K+1)
      IF(J.EQ.0) GOTO 100
   10 CONTINUE
  100 R=S
      RETURN
      END
