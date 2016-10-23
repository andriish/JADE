C   07/06/96 606071919  MEMBER NAME  YMAX     (S4)          FORTG1
      SUBROUTINE YMAX(Y,N,R)
      REAL Y(1),R(18,2)
      REAL X(18)
      DO 2 I=1,2
      DO 2 J=1,18
    2 R(J,I)=0.0
C
C     R(1)  = X(-)     R(4)  = Y(-)     R(7)  = SIGMA(-)
C     R(2)  = X(0)     R(5)  = Y(0)     R(8)  = SIGMA(0)
C     R(3)  = X(+)     R(6)  = Y(+)     R(9)  = SIGMA(+)
C
C         PEAK            +- 1 SIGMA        TOTAL
C     R(10) = SIGNAL   R(13) = SIGNAL   R(16) = SIGNIF.
C     R(11) = BACKG.   R(14) = BACKG.   R(17) = TOTAL
C     R(12) = S/(S+B)  R(15) = S/(S+B)  R(18) = ERROR
C
C
      NM=N-1
      IF(NM.LT.3) GOTO 100
      X(1)=0.0
      X(2)=0.0
      X(3)=0.0
      DO 40 I=2,NM
      D2=Y(I+1)-Y(I-1)
      DD2=Y(I+1)-Y(I)-Y(I)+Y(I-1)
      IF(I.EQ.2) GOTO 30
      IF(DD1.LT.0.0.OR.DD2.GE.0.0) GOTO 10
      X(1)=FLOAT(I)+DD2/(DD1-DD2)
      X(2)=0.0
   10 IF(D1*D2.GE.0.0) GOTO 20
      X(2)=FLOAT(I)+D2/(D1-D2)
   20 IF(DD1.GE.0.0.OR.DD2.LT.0.0) GOTO 30
      X(3)=FLOAT(I)+DD2/(DD1-DD2)
      IF(X(1).EQ.0.0) GOTO 30
      IF(X(2).EQ.0.0) X(2)=0.5*(X(1)+X(3))
      DO 22 J=1,3
      K=X(J)+0.5
      Z=X(J)-FLOAT(K)
      X(J+3)=Y(K)+0.5*Z*(Y(K+1)-Y(K-1))
     1           +0.5*Z*Z*(Y(K+1)-Y(K)-Y(K)+Y(K-1))
   22 CONTINUE
      X(7)=X(2)-X(1)
      X(8)=0.5*(X(3)-X(1))
      X(9)=X(3)-X(2)
      S=2.5415*(X(5)-0.5*(X(4)+X(6)))
      B=X(5)-S
      IF(B.GE.0.0) GOTO 24
      S=X(5)
      B=0.0
   24 X(10)=S
      X(11)=B
      X(12)=S/(S+B)
      X(17)=2.5066*X(8)*X(10)
      S=0.6828*X(17)
      B=2.0*X(8)*X(11)
      X(13)=S
      X(14)=B
      X(15)=S/(S+B)
      X(16)=S/SQRT(S+B)
      X(18)=X(17)/X(16)
      IF(R(16,1).GT.X(16)) GOTO 27
      DO 26 J=1,18
      R(J,2)=R(J,1)
   26 R(J,1)=X(J)
      GOTO 30
   27 IF(R(16,2).GT.X(16)) GOTO 30
      DO 28 J=1,18
   28 R(J,2)=X(J)
   30 D1 =D2
   40 DD1=DD2
      WRITE(6,101) R
  100 RETURN
  101 FORMAT(1X,9G13.5)
      END
