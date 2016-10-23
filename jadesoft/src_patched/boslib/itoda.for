C   07/06/96 606071853  MEMBER NAME  ITODA    (S4)          FORTG1
      SUBROUTINE ITODA(I,R,M)
      character*8 R,S,B/'        '/
      character*1 LS(8),LCH(10)/'0','1','2','3','4','5','6','7','8','9'/
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
