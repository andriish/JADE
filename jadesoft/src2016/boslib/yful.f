C   07/06/96 606071919  MEMBER NAME  YFUL     (S4)          FORTG1
      SUBROUTINE YFUL(Y,N,R)
      REAL Y(1),R(18)
      REAL X(3),Z(3),ZL(3),XX(2)
      DO 1 J=1,18
    1 R(J)=0.0
      IP=0
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
      DO 6 I=1,N
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
      DO 24 K=1,2
      IA=3-2*K
      XA=IA
      LL=2
      X(1)=XMAX
      X(2)=XMAX
   10 IF(LL.EQ.3) X(3)=X(2)+X(2)-X(1)
      DO 12 L=1,LL
      Z(L)=0.0
      J=X(L)+0.5
      IF(J.LE.1.OR.J.GE.N) GOTO 12
      ZZ=X(L)-FLOAT(J)
      Z(L)=Y(J)+0.5*ZZ*(Y(J+1)-Y(J-1))
     1      +0.5*ZZ*ZZ*(Y(J+1)-Y(J)-Y(J)+Y(J-1))
   12 CONTINUE
      IP=IP+1
      IF(IP.GT.30) GOTO 100
      GOTO (14,14,18),LL
   14 IF(Z(2).GT.0.5*Z(1)) GOTO 20
   16 F1=Z(1)-Z(2)-Z(2)
      F2=ZL(1)-ZL(2)-ZL(2)
      XX(K)=X(2)+XA*F1/(F2-F1)
      IF(LL.EQ.3) GOTO 24
      LL=3
      XA=-XA
      GOTO 10
   18 Z(1)=Z(1)-Z(3)
      Z(2)=Z(2)-Z(3)
      IF(Z(2).GT.0.5*Z(1)) GOTO 16
   20 DO 22 L=1,LL
   22 ZL(L)=Z(L)
      X(2)=X(2)+XA
      GOTO 10
   24 CONTINUE
      R(2)=XMAX
      R(7)=(XMAX-XX(2))*0.9058
      R(9)=(XX(1)-XMAX)*0.9058
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
      WRITE(6,102) R
  100 RETURN
  101 FORMAT(1X,10G13.5)
  102 FORMAT(1X,9G13.5)
      END
