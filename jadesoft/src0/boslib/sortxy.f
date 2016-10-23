C   07/06/96 606071911  MEMBER NAME  SORTXY   (S4)          FORTG1
      SUBROUTINE SORTXY(X,Y,N)
      REAL X(N),Y(N),W
      M=N
   10 M=M/2
      IF(M.EQ.0) RETURN
      K=N-M
      DO 40 J=1,K
      I=J
      GOTO 30
   20 I=I-M
      IF(I.LT.1) GOTO 40
   30 L=I+M
      IF(X(L).LT.X(I)) GOTO 50
   40 CONTINUE
      GOTO 10
C     EXCHANGE I AND L
C     ----------------
   50 W=X(I)
      X(I)=X(L)
      X(L)=W
      W=Y(I)
      Y(I)=Y(L)
      Y(L)=W
C     ----------------
      GOTO 20
      END
