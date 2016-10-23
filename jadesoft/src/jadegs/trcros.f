C   08/07/80 708261442  MEMBER NAME  TRCROS   (JADEGS)      FORTRAN
C
C-----------------------------------------------------------------------
      SUBROUTINE TRCROS( AR1, AR2, S1,S2, XYZC )
C-----------------------------------------------------------------------
C     ORIGINAL  06-03-80  09:55 S.YAMADA
C          MOD  08-05-80  14:45
C          MOD  13-10-81  18:35 E.ELSEN     AMAX1 INTRODUCED
C     LAST MOD  26-08-87  14:35 C.BOWDERY   RENAMED FROM TPTCRS
C
C     CROSSING POINT OF TWO LINES IS CALCULATED.
C
C     INPUT
C     -----
C     AR1=(X1,Y1,Z1,DX1,DY1,DZ1)   LINE PARAMETERS OF THE LINE #1
C     AR2=(X2,Y2,Z2,DX2,DY2,DZ2)   LINE PARAMETERS OF THE LINE #2
C     OUTPUT
C     ------
C     S1=DISTANCE FROM (X1,Y1,Z1) TO THE CROSSING POINT
C     S2=DISTANCE FROM (X2,Y2,Z2) TO THE CROSSING POINT
C
C     S1 AND S2 HAVE SIGNS W.R.T. THE INPUT DIRECTIONS.
C     S1 AND S2 HAVE SIGNS W.R.T. THE INPUT DIRECTIONS,
C     +  FOR THE SAME DIRECTION, - FOR THE OPPOSITE DIRECTION.
C     XYZC(I,J)   CROSSING POINT ON THE J-TH TRACK EXTRAPOLATION.
C                 I=1,2,3  FOR X,Y,Z
C
C-----------------------------------------------------------------------
C
      DIMENSION  AR1(6), AR2(6), XYZC(3,2)
C
C------------------  C O D E  ------------------------------------------
C
      X1 = AR1(1)
      Y1 = AR1(2)
      Z1 = AR1(3)
C
C                            NORMALIZED DIRECTION IN THE (X,Y) PLANE
C
      DS1 = AMAX1( SQRT(AR1(4)**2+AR1(5)**2), 1.E-6 )
      DX1 = AR1(4)/DS1
      DY1 = AR1(5)/DS1
      DZ1 = AR1(6)/DS1
C
      X2 = AR2(1)
      Y2 = AR2(2)
      Z2 = AR2(3)
C
C                            NORMALIZED DIRECTION IN THE (X,Y) PLANE
C
      DS2 = AMAX1( SQRT(AR2(4)**2+AR2(5)**2), 1.E-6 )
      DX2 = AR2(4)/DS1
      DY2 = AR2(5)/DS1
      DZ2 = AR2(6)/DS1
C
C                            THE EQUATION S FOR THE LINES
C                              X=DX1*S1+X1                  X=DX2*S2+X2
C                              Y=DY1*S1+Y1                  Y=DY2*S2+Y2
C                            THE EQUATIONS FOR THE DISTANCES
C                            TO THE CROSSING POINT
C                              DX1*S1 - DX2*S2 = X2 - X1 = DX
C                              DY1*S1 - DY2*S2 = Y2 - Y1 = DY
C
      DX    = X2-X1
      DY    = Y2-Y1
      COS12 = DX1*DX2+DY1*DY2
      IF( ABS(COS12).GT.0.99 ) GO TO 10
      DD    = DX1*DY2-DX2*DY1
      IF( DD.EQ.0. ) GO TO 10
C
      S1 = (DX*DY2-DY*DX2)/DD
      S2 = (DX*DY1-DY*DX1)/DD
      GO TO 100
C
C                            ALMOST PARALLEL CASE
C
   10 S1 =  0.5*(DX*DX1 + DY*DY1)
      S2 = -0.5*(DX*DX2 + DY*DY2)
C
  100 XYZC(1,1) = DX1*S1+X1
      XYZC(2,1) = DY1*S1+Y1
      XYZC(3,1) = DZ1*S1+Z1
      XYZC(1,2) = DX2*S2+X2
      XYZC(2,2) = DY2*S2+Y2
      XYZC(3,2) = DZ2*S2+Z2
C
      RETURN
      END
