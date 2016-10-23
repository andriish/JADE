C   07/06/96 606071922  MEMBER NAME  BANDSL   (S4)          FORTG1
      SUBROUTINE BANDSL(A,N,M,IER)
      REAL A(M,N)
      IER=1
      MM=M-1
C     DECOMPOSITION
      DO 40 L=1,N
      IF(A(1,L).LE.0.0) GOTO 100
      A(1,L)=SQRT(A(1,L))
      DO 10 J=2,MM
   10 A(J,L)=A(J,L)/A(1,L)
      IA=L+1
      IB=MIN0(N,L+MM-1)
      IF(IB.LT.IA) GOTO 40
      DO 30 I=IA,IB
      JB=MM+L-I
      IF(JB.LT.1) GOTO 30
      DO 20 J=1,JB
   20 A(J,I)=A(J,I)-A(I-L+1,L)*A(I-L+J,L)
   30 CONTINUE
   40 CONTINUE
C     FORWARD
      DO 60 L=1,N
      S=A(M,L)
      DO 50 J=2,MM
      LJ=L-J+1
      IF(LJ.LT.1) GOTO 60
   50 S=S-A(M,LJ)*A(J,LJ)
   60 A(M,L)=S/A(1,L)
C     BACKWARD
      DO 80 LB=1,N
      L=N+1-LB
      S=A(M,L)
      DO 70 J=2,MM
      LJ=L+J-1
      IF(LJ.GT.N) GOTO 80
   70 S=S-A(M,LJ)*A(J,L)
   80 A(M,L)=S/A(1,L)
      IER=0
  100 RETURN
      END
