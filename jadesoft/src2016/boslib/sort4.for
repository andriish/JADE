C   07/06/96 606071911  MEMBER NAME  SORT4    (S4)          FORTG1
      SUBROUTINE SORT4(X,N)
      REAL X(N),W
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
      IF(X(L).LT.X(I)) THEN 
      W=X(I)
      X(I)=X(L)
      X(L)=W
      GOTO 20
      ENDIF
   40 CONTINUE
      GOTO 10
      END

CORIGINAL
c      SUBROUTINE SORT4(X,N)
c      REAL X(N),W
c      M=N
c   10 M=M/2
c      IF(M.EQ.0) RETURN
c      K=N-M
c      DO 40 J=1,K
c      I=J
c      GOTO 30
c   20 I=I-M
c      IF(I.LT.1) GOTO 40
c   30 L=I+M
c      IF(X(L).LT.X(I)) GOTO 50
c   40 CONTINUE
c      GOTO 10
cC     EXCHANGE I AND L
cC     ----------------
c   50 W=X(I)
c      X(I)=X(L)
c      X(L)=W
cC     ----------------
c      GOTO 20
c      END