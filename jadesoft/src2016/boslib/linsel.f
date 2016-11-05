C   07/06/96 606071854  MEMBER NAME  LINSEL   (S4)          FORTG1
      SUBROUTINE LINSEL(Y,X,H,N,Q)
      REAL X(N),Y(N),H(N),Q(9)
      EXTERNAL SMTWO
C
C                 - -   -
C     CALL LINSEL(Y,X,H,N,Q)
C                         -
C
C     FILTER LINEAR DEPENDENCE OF Y ON X, RESULT IN ARRAY Q( )
C     ARRAYS X( ) AND Y( ) ARE CHANGED, BUT ONLY IN THE ORDER
C     OF POINTS, H( ) IS AN AUXILIARY ARRAY.
C     POINTS WITH LARGE DEVIATION FROM THE MAIN LINEAR DEPENDENCE
C     ARE DELETED BY CUTS, BASED ON EMPIRICAL AND STATISTICAL
C     METHODS
C
C      I      Q(I)
C      1      NR OF ENTRIES (N)
C      2      INTERCEPT
C      3      SLOPE
C      4      ERROR OF SLOPE
C      5      MEAN X
C      6      MEAN Y
C      7      ERROR OF MEAN Y
C      8      ERROR (STANDARD DEVIATION SIGMA) OF SINGLE POINT
C      9      NR OF DELETED POINTS
C
C     PARAMETRIZATIONS
C
C     1.)  Y(X) = Q(2) + Q(3) * X
C          ERROR OF Q(2)            = SQRT OF Q(7)**2+(Q(3)*Q(4))**2
C          CORRELATION Q(2) Q(3)    = -Q(3)*Q(4)**2
C     2.)  Y(X) = Q(6) + Q(3) * (X-Q(5))
C          PROPAGATED ERROR OF Y(X) = SQRT OF Q(7)**2+(Q(4)*(X-Q(5))**2
C
      CALL SORTXY(X,Y,N)
      CALL LINALL(Y(1),X(1),N,Q)
      CALL SMONE(Y,H,N)
      IA=0
      IF(N.GT.10) IA=2
      IF(N.GT.30) GOTO 5
C     LESS THAN 31 POINTS
      CALL LINALL(Y(1),X(1),N,Q)
      QS=Q(8)
      CALL LINALL(H(1+IA),X(1+IA),N-2*IA,Q)
      IF(Q(8).LE.QS) GOTO 10
      CALL LINALL(Y(1),X(1),N,Q)
      Q(8)=0.75*Q(8)
      GOTO 10
C     MORE THAN 30 POINTS
    5 CALL SMONE(H,H,N)
      IA=4
      CALL LINALL(H(1+IA),X(1+IA),N-2*IA,Q)
C
   10 CONTINUE
      CUT=2.0
      CUT2=CUT*CUT
      CF=Q(7)**2+Q(8)**2
      LOOP=0
   15 LOOP=LOOP+1
      J=0
      K=N
      DO 20 I=1,N
      J=J+1
      YE=Q(6)+Q(3)*(X(J)-Q(5))
      CA=(YE-Y(J))**2
      IF(I.EQ.1) CAMIN=CA
      CAMIN=AMIN1(CAMIN,CA)
      CB=CF+((X(J)-Q(5))*Q(4))**2
      IF(CA.LE.CB*CUT2) GOTO 20
      XY=X(J)
      X(J)=X(K)
      X(K)=XY
      XY=Y(J)
      Y(J)=Y(K)
      Y(K)=XY
      J=J-1
      K=K-1
   20 CONTINUE
      IF(LOOP.GE.2.OR.K.GE.2) GOTO 30
      CF=CF+CAMIN
      GOTO 15
   30 CALL LINALL(Y,X,K,Q)
      Q(9)=FLOAT(N-K)
  100 RETURN
      END
