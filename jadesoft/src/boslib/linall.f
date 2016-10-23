C   07/06/96 606071854  MEMBER NAME  LINALL   (S4)          FORTG1
      SUBROUTINE LINALL(Y,X,N,Q)
      REAL X(N),Y(N),Q(8)
      REAL*8 XN,SX,SY,SXX,SXY,SYY
C                 - - -
C     CALL LINALL(Y,X,N,Q)
C                       -
C
C     FIT LINEAR DEPENDENCE OF Y ON X, RESULT IN ARRAY Q( )
C     ARRAYS X( ) AND Y( ) ARE NOT CHANGED.
C
C      I      Q(I)
C      1      NR OF ENTRIES (N)
C      2      INTERCEPT
C      3      SLOPE
C      4      ERROR OF SLOPE
C      5      MEAN X
C      6      MEAN Y
C      7      ERROR OF MEAN Y
C      8      MEAN SIGMA OF SINGLE POINT
C
C     PARAMETRIZATIONS
C
C     1.)  Y(X) = Q(2) + Q(3) * X
C          ERROR OF Q(2)            = SQRT OF Q(7)**2+(Q(3)*Q(4))**2
C          CORRELATION Q(2) Q(3)    = -Q(3)*Q(4)**2
C     2.)  Y(X) = Q(6) + Q(3) * (X-Q(5))
C          PROPAGATED ERROR OF Y(X) = SQRT OF Q(7)**2+(Q(4)*(X-Q(5))**2
C
      IF(N.EQ.0) GOTO 25
      XN=N
C
      SX=0.0
      SY=0.0
      DO 10 I=1,N
      SX=SX+X(I)
   10 SY=SY+Y(I)
      SX=SX/XN
      SY=SY/XN
C
      SXX=0.0
      SXY=0.0
      SYY=0.0
      DO 20 I=1,N
      XR=X(I)-SX
      YR=Y(I)-SY
      SXX=SXX+XR*XR
      SXY=SXY+XR*YR
   20 SYY=SYY+YR*YR
C
   25 DO 30 I=1,8
   30 Q(I)=0.0
      IF(N.EQ.0) GOTO 100
      Q(1)=XN
      Q(5)=SX
      Q(6)=SY
      IF(SXX.LE.0.0) GOTO 40
      Q(3)=SXY/SXX
      SYY=SYY-SXY*SXY/SXX
   40 IF(XN.LE.2.0) GOTO 50
      Q(8)=DSQRT(SYY/(XN-2.0D0))
      Q(4)=Q(8)/DSQRT(SXX)
      Q(7)=Q(8)/DSQRT(XN)
   50 Q(2)=Q(6)-Q(3)*Q(5)
  100 RETURN
      END
