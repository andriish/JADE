C   07/06/96 606071908  MEMBER NAME  SMDIST   (S4)          FORTG1
      SUBROUTINE SMDIST(Y,YH,Z,N,IP,MODE)
      REAL Y(N),YH(N),Z(N),H(320)
C
C                 -      - -- ----
C     CALL SMDIST(Y,YH,Z,N,IP,MODE)
C                   -- -
C
C     SMOOTH N DATA Y( ) ASSUMED AT EQUIDISTANT ABSCISSA VALUES
C
C      Y( ) = INPUT DATA
C     YH( ) = AUXILIARY ARRAY
C      Z( ) = RESULT (SMOOTHED VALUES)
C
C     Y( ) AND YH( ) MAY BE THE SAME ARRAY.
C
C        IP = 0   FOR POISSON DISTRIBUTED (COUNTER) DATA
C           = NOT EQUAL 0 OTHERWISE
C      MODE = 1   353QH
C           = 2   353QH TWICE
C           = 3   353QH TWICE + SPLINE FIT
C
C
C
      DO 5  I=1,N
    5 YH(I)=Y(I)
      IF(IP.EQ.0) CALL SMPTEQ(YH,N)
      DO 10 I=1,N
   10 Z(I)=YH(I)
      DO 60 K=1,2
      CALL SMOPER(Z,N,54321)
      IF(MODE.EQ.1) GOTO 90
      GOTO (20,40),K
   20 NSP=3+0.2*FLOAT(N)
      NSP=MIN0(64,NSP)
      CALL SMSPLE(Z,YH,W,N,H,NSP,-1,CHDF)
      DO 30 I=1,N
      ZZ=Z(I)
      Z(I)=YH(I)-ZZ
   30 YH(I)=ZZ
      GOTO 60
   40 DO 50 I=1,N
      Z(I)=YH(I)+Z(I)
   50 YH(I)=Z(I)
   60 CONTINUE
      IF(MODE.EQ.2.OR.N.LT.10) GOTO 90
      DO 80 K=1,2
      IF(K.EQ.2) GOTO 62
      NSP=2+(N+1)/2
      IF(NSP.GT.64) GOTO 65
      GOTO 64
   62 NSP=2.0+YH(N)*0.5
      IF(NSP.GT.64) GOTO 65
   64 CALL SMSPLE(Z,YH,W,N,H,NSP,-K,CHDS)
      GOTO 72
   65 KL=(N+101)/112
      LK=(N+12*KL-11)/KL
      CHDS=0.0
      JA=1
      DO 70 LL=1,KL
      JB=MIN0(N,JA+LK-1)
      IF(K.EQ.1) NSP=2+(JB-JA)/2
      IF(K.EQ.2) NSP=2+(YH(JB)-YH(JA))*0.5
      NSP=MIN0(64,NSP)
      CALL SMSPLE(Z(JA),YH(JA),W,JB-JA+1,H,NSP,-K,CHDF)
      CHDS=CHDS+CHDF
   70 JA=JB-10
      CHDS=CHDS/FLOAT(KL)
   72 CHDS=18.0*SQRT(CHDS)
C     RMS DEVIATION IS REDUCE BY A FACTOR OF 18.0 BY PREVIOUS
C     SMOOTH OPERATIONS
      IF(K.EQ.2) GOTO 80
      D1=Z(2)-Z(1)
      NM=N-1
      DO 74 I=2,NM
      D2=Z(I+1)-Z(I)
      DD=D2-D1
      Z(I)=YH(I)
      YH(I)=0.4
      IF(ABS(DD).GT.CHDS) YH(I)=1.0
   74 D1=D2
      Z(1)=YH(1)
      Z(N)=YH(N)
      YH(1)=YH(2)
      YH(N)=YH(NM)
      CALL SMOPER(YH,N,51)
      DO 76 I=2,N
   76 YH(I)=YH(I-1)+YH(I)
   80 CONTINUE
   90 IF(IP.NE.0) GOTO 100
      CALL SMEQTP(Z,N)
  100 RETURN
      END
