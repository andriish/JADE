C   07/06/96 606071854  MEMBER NAME  LINFIL   (S4)          FORTG1
      SUBROUTINE LINFIL(X,Y,N,A)
      REAL X(1),Y(1),A(7)
      REAL Z(200),XR(200),YR(200),Q(400),P(5,5),B(5),S(100)
      EQUIVALENCE (S(1),Q(101))
C
C     FILTER LINEAR DEPENDENCE OF Y ON X
C
C     A(1) = INTERCEPT
C     A(2) = SLOPE
C     A(3) = SIGMA (SINGLE POINT)
C     A(4) = LINEARITY
C            IF LINEAR TERM INSIGNIFICANT, DISTRIBUTED ACCORDING
C            TO THE CHISQUARE DISTRIBUTION WITH 1 DEGREE OF
C            FREEDOM. THEREFORE BELOW 3.84 WITH 95 PC C.L.
C     A(5) = NONLINEARITY
C            REDUCTION OF SIGMA BY POLYNOMIAL OF 2.DEGREE FOR
C            N LESS EQUAL 50 OR BY POLYNOMIAL OF 4.DEGREE FOR
C            LARGER N. SHOULD BE ABOUT 1 FOR NO NONLINEARITY.
C     A(6) = RATIO OF GOOD/TOTAL POINTS
C     A(7) = MEAN Y
C
      IF(N.LE.5) GOTO 90
      K=1+(N-1)/200
      DO 50 IG=1,2
      L=0
      IS=0
   10 NS=MIN0(N-IS,200)
      IF(IS+NS.LT.N.AND.N-IS-NS.LT.100) NS=(N-IS)/2
      GOTO (12,16),IG
   12 DO 14 I=1,NS
   14 Z(I)=Y(I+IS)
      GOTO 20
   16 DO 18 I=1,NS
   18 Z(I)=Y(I+IS)-A(1)-A(2)*X(I+IS)
   20 CALL SM353(Z,Z,NS)
      I=0
      M=0
   30 IF(I.EQ.NS) GOTO 40
      I=I+1
      IF(M.NE.0) GOTO 35
      SX=0.0
      SY=0.0
   35 M=M+1
      SX=SX+X(I+IS)
      SY=SY+Z(I)
      IF(M.NE.K.AND.I.NE.NS) GOTO 30
      L=L+1
      XR(L)=SX/FLOAT(M)
      YR(L)=SY/FLOAT(M)
      M=0
      IF(NS-I.GE.K) GOTO 30
      IF(NS+IS.EQ.N) GOTO 30
      NS=I
   40 IS=IS+NS
      IF(IS.LT.N) GOTO 10
      IF(K.GT.1) CALL SM353(YR,YR,L)
      IF(L.LE.12) GOTO 46
      LC=L
      L =0
      DO 44 I=1,LC
      IF(L.NE.0.AND.YR(L).EQ.YR(I).AND.M.LT.3) GOTO 42
C     NOT EQUAL
      L=L+1
      YR(L)=YR(I)
      M=1
      XS=XR(I)
      GOTO 44
C     EQUAL
   42 M=M+1
      XS=XS+XR(I)
   44 XR(L)=XS/FLOAT(M)
      CALL SM353(YR,YR,L)
   46 LS=1
      IF(L.GT.10) LS=2
      L=L+2-LS-LS
      IF(IG.EQ.2) GOTO 50
      CALL ORTPOL(XR(LS),YR(LS),1.0,-L,2,Q,P)
      CALL ORTOPL(P,A,NP)
   50 CONTINUE
C
      NDEG=5
      IF(L.LT.50) NDEG=3
      CALL ORTPOL(XR(LS),YR(LS),1.0,-L,NDEG,Q,P)
      FN=(L-NDEG)*P(5,2)
      FD=(L-   2)*P(5,NDEG)
      CALL ORTRED(P,-1.0,2,NP)
      CALL ORTOPL(P,B,NP)
      J=0
      DO 59 I=1,N,K
      J=J+1
      YF=Y(I)-A(1)-A(2)*X(I)
      CALL POLFUN(X(I),YP,DY,B,NP)
   59 Q(J)=ABS(YF-YP)
      CALL SORT4(Q,J)
      SF=Q((J+1)/2)
      IF(SF.LE.0.0) GOTO 90
      DO 51 I=1,100
   51 Q(I)=0.0
      DO 52 I=1,N
      YF=Y(I)-A(1)-A(2)*X(I)
      CALL POLFUN(X(I),YP,DY,B,NP)
      NB=1.0+ABS(YF-YP)/SF
      IF(NB.LE.100) Q(NB)=Q(NB)+1.0
   52 CONTINUE
      S(1)=Q(1)
      DO 53 I=2,100
   53 S(I)=S(I-1)+Q(I)
      DO 54 I=1,50
      IF(S(I).GT.0.5*S(100)) GOTO 55
   54 CONTINUE
      I=50
   55 SL=0.01+1.0/FLOAT(N)
      DO 56 J=I,50
      IF(S(J+J)-S(J).LT.SL*S(J)) GOTO 57
   56 CONTINUE
      J=50
   57 SU=0.0
      DO 58 I=1,J
   58 SU=SU+Q(I)*(FLOAT(I)-0.5)**2
      SF=SF*SQRT(SU/S(J))
      L=0
      M=0
      NN=0
C
      DO 80 I=1,N
      YF=Y(I)-A(1)-A(2)*X(I)
      CALL POLFUN(X(I),YP,DY,B,NP)
      CH=ABS(YF-YP)/SF
      IF(CH.LT.3.0) GOTO 60
      IF(I.EQ.N.AND.M.NE.0) GOTO 75
      GOTO 80
   60 IF(M.NE.0) GOTO 70
      SX=0.0
      SY=0.0
   70 M=M+1
      SX=SX+X(I)
      SY=SY+Y(I)
      NN=NN+1
      IF(M.NE.K.AND.I.NE.N) GOTO 80
   75 L=L+1
      XR(L)=SX/FLOAT(M)
      YR(L)=SY/FLOAT(M)
      M=0
   80 CONTINUE
      IF(L.LE.5) GOTO 90
C
      CALL ORTPOL(XR,YR,1.0,-L,2,Q,P)
      CALL ORTRED(P,-1.0,2,NP)
      CALL ORTOPL(P,A,NP)
      SIGS=SQRT(P(5,2)/FLOAT(L-2))
      A(3)=SIGS*SQRT(FLOAT(K))
      A(4)=(P(4,2)/SIGS)**2
      A(5)=1.0
      IF(FN*FD.NE.0.0) A(5)=SQRT(FN/FD)
      A(6)=FLOAT(N-NN)/FLOAT(N)
      CALL ORTRED(P,0.0,1,NP)
      CALL ORTOPL(P,A(7),NP)
      GOTO 100
C
   90 DO 95 I=1,7
   95 A(I)=0.0
      CALL SM353(Y,YR,N)
      IF(N.LT.2) GOTO 100
      CALL ORTPOL(X,YR,1.0,-N,2,Q,P)
      CALL ORTRED(P,-1.0,2,NP)
      CALL ORTOPL(P,A,NP)
      A(3)=SQRT(P(2,1))
      CALL ORTRED(P,0.0,1,NP)
      CALL ORTOPL(P,A(7),NP)
  100 RETURN
      END
