C   18/10/82 606071921  MEMBER NAME  ANLDET   (S)           FORTRAN
      SUBROUTINE ANLDET(Y,N,Q)
      COMMON/HILUS/MP(18)
      REAL Y(1),R(18),Q(10,3)
      REAL X(18),Z(3),ZL(3),XX(2)
      MP(17)=N
      DO 1 J=1,18
      MP(J)=0
    1 R(J)=0.0
      DO 3 J=1,3
      DO 3 I=1,10
    3 Q(I,J)=0.0
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
      IL=0
      NM=N-1
      IF(NM.LT.3) GOTO 100
C
C     FIND ABSOLUTE MAXIMUM
C
      DO 6 I=1,N
      MP(1)=MP(1)+1
      Q(1,1)=Q(1,1)+Y(I)
      IF(IL.EQ.0) GOTO 2
      IF(YMAX-Y(I)) 2,4,6
    2 IL=I
      YMAX=Y(I)
    4 IR=I
    6 CONTINUE
      XMAX=0.5*FLOAT(IL+IR)
      IMAX=XMAX+0.5
      B=Y(IMAX+1)-Y(IMAX)-Y(IMAX)+Y(IMAX-1)
      IF(B.EQ.0.0) GOTO 8
      XMAX=FLOAT(IMAX)+0.5*(Y(IMAX-1)-Y(IMAX+1))/B
      IMAX=XMAX+0.5
    8 XX(1)=XMAX
      XX(2)=XMAX
C
C     FIND HALF HEIGHTS ON THE LEFT AND ON THE RIGHT SIDE, FIRST
C     WITHOUT BACKGROUND SUBTRACTION, LATER WITH BACKGROUND SUBTR.
C
      DO 24 K=1,2
      MP(2)=MP(2)+1
      IA=3-2*K
      XA=IA
      LL=2
      X(1)=XMAX
      X(2)=XMAX
   10 DO 12 L=1,LL
      Z(L)=0.0
      J=X(L)+0.5
      IF(J.LE.1.OR.J.GE.N) GOTO 12
      ZZ=X(L)-FLOAT(J)
      Z(L)=Y(J)+0.5*ZZ*(Y(J+1)-Y(J-1))
     1      +0.5*ZZ*ZZ*(Y(J+1)-Y(J)-Y(J)+Y(J-1))
   12 CONTINUE
      GOTO (14,14,18),LL
   14 IF(Z(2).GT.0.5*Z(1)) GOTO 20
   16 F1=Z(1)-Z(2)-Z(2)
      F2=ZL(1)-ZL(2)-ZL(2)
      XX(K)=X(2)+XA*F1/(F2-F1)
C     WRITE(6,101) LL,K,XX,(X(LKJ),LKJ=1,3),Z,ZL
C 101 FORMAT(1X,2I2,11F11.4)
      IF(LL.EQ.3) GOTO 24
      LL=3
      XA=-XA
      X(3)=X(2)+X(2)-X(1)
      GOTO 10
   18 Z(1)=Z(1)-Z(3)
      Z(2)=Z(2)-Z(3)
      IF(Z(2).GT.0.5*Z(1)) GOTO 16
   20 DO 22 L=1,LL
   22 ZL(L)=Z(L)
      MP(4)=MP(4)+1
      X(2)=X(2)+XA
      GOTO 10
   24 CONTINUE
      R(2)=XMAX
      R(7)=(XMAX-XX(2))*0.85
      R(9)=(XX(1)-XMAX)*0.85
      R(8)=0.5*(R(7)+R(9))
      R(1)=R(2)-R(7)
      R(3)=R(2)+R(9)
      DO 26 J=1,3
      K=R(J)+0.5
      ZZ=R(J)-FLOAT(K)
      R(J+3)=Y(K)+0.5*ZZ*(Y(K+1)-Y(K-1))
     1           +0.5*ZZ*ZZ*(Y(K+1)-Y(K)-Y(K)+Y(K-1))
   26 CONTINUE
      S=2.5415*(R(5)-0.5*(R(4)+R(6)))
      B=R(5)-S
      IF(B.GE.0.0) GOTO 28
      S=R(5)
      B=0.0
   28 R(10)=S
      R(11)=B
      R(12)=S/(S+B)
      R(17)=2.5066*R(8)*R(10)
      S=0.6828*R(17)
      B=2.0*R(8)*R(11)
      R(13)=S
      R(14)=B
      R(15)=S/(S+B)
      R(16)=S/SQRT(S+B)
      R(18)=R(17)/R(16)
      Q(2,1)=1.0
      Q(3,1)=R(2)
      Q(4,1)=R(8)
      Q(5,1)=R(7)
      Q(6,1)=R(9)
      Q(7,1)=R(15)
      Q(8,1)=R(16)
      Q(9,1)=R(17)
      Q(10,1)=R(18)
C     WRITE(6,102) R
C 102 FORMAT(1X,9G13.4)
C
C     SEARCH FOR FURTHER PEAKS BY LOOKING FOR
C     ZEROS OF FIRST DERIVATIVE
C
      NM=N-1
      IF(NM.LT.3) GOTO 100
      X(1)=0.0
      X(2)=0.0
      X(3)=0.0
      MP(6)=MP(6)+1
      DO 70 I=2,NM
      MP(7)=MP(7)+1
      D2=Y(I+1)-Y(I-1)
      DD2=Y(I+1)-Y(I)-Y(I)+Y(I-1)
      IF(I.EQ.2) GOTO 60
      IF(DD1.LT.0.0.OR.DD2.GE.0.0) GOTO 40
      X(1)=FLOAT(I)+DD2/(DD1-DD2)
      X(2)=0.0
   40 IF(D1*D2.GE.0.0) GOTO 50
      X(2)=FLOAT(I)+D2/(D1-D2)
   50 IF(DD1.GE.0.0.OR.DD2.LT.0.0) GOTO 60
      X(3)=FLOAT(I)+DD2/(DD1-DD2)
      IF(X(1).EQ.0.0) GOTO 60
      IF(X(2).EQ.0.0) X(2)=0.5*(X(1)+X(3))
      DO 52 J=1,3
      K=X(J)+0.5
      CZ=X(J)-FLOAT(K)
      X(J+3)=Y(K)+0.5*CZ*(Y(K+1)-Y(K-1))
     1           +0.5*CZ*CZ*(Y(K+1)-Y(K)-Y(K)+Y(K-1))
   52 CONTINUE
      MP(8)=MP(8)+1
      X(7)=X(2)-X(1)
      X(8)=0.5*(X(3)-X(1))
      X(9)=X(3)-X(2)
      S=2.5415*(X(5)-0.5*(X(4)+X(6)))
      B=X(5)-S
      IF(B.GE.0.0) GOTO 54
      S=X(5)
      B=0.0
   54 X(10)=S
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
      MP(9)=MP(9)+1
      IF(X(2).GT.Q(3,1)-0.25*Q(4,1).AND.
     1   X(2).LT.Q(3,1)+0.25*Q(4,1)) GOTO 60
      IF(X(16).LT.Q(8,2)) GOTO 57
      DO 56 J=1,10
   56 Q(J,3)=Q(J,2)
      J=2
      MP(10)=MP(10)+1
      GOTO 58
   57 IF(X(16).LT.Q(8,3)) GOTO 60
      J=3
   58 Q(3,J)=X(2)
      Q(4,J)=X(8)
      Q(5,J)=X(7)
      Q(6,J)=X(9)
      Q(7,J)=X(15)
      Q(8,J)=X(16)
      Q(9,J)=X(17)
      Q(10,J)=X(18)
   60 D1 =D2
   70 DD1=DD2
      IF(Q(3,2).NE.0.0) Q(2,1)=2.0
      IF(Q(3,3).NE.0.0) Q(2,1)=3.0
  100 RETURN
      END
