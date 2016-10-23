C   07/06/78 C8071801   MEMBER NAME  JMULSC0  (SOURCE)      FORTRAN
      SUBROUTINE JMULSC( P, XRAD )
C  *--------------------------------------------------------
C  *
C  *  VERSION OF 01/06/78
C  *  CALCULATE MULTIPLE SCATTERING .
C  *    P = MOMENTUM VECTOR OF INCIDENT PARTICLE
C  *         THIS VECTOR IS OVERWRITTEN BY SCATTERD MOMENTUM.
C  *    P(1) ... P(3) MOMENTUM COMPONENTS
C  *    P(4)          ENERGY OF PARTICLE
C  *    P(5)          REST MASS
C  *    P(6)          TOTAL MOMENTUM
C  *    XRAD = THICKNESS OF RADIATOR IN RADIATION LENGTH
C  *  TOTAL ANGLE IS RANDOMLY GENERATED.
C  *--------------------------------------------------------
C
C
      DIMENSION P(6),X2(3),X3(3)
      DATA PI/3.14159/
C
      IF( XRAD .LT. 1.E-30 ) RETURN
C
C  CHANGE P TO UNIT VECTOR
      DO 100 I =1,3
  100 P(I) = P(I)/ P(6)
C
C  FIND OTHER UNIT VECTORS THAT ARE PERPENDICULAR.
      CTH = P(3)
      IF(ABS(CTH).LT.1.E-30) GO TO 1
      STH = SQRT( 1. - CTH*CTH )
      IF(STH.LT.1.E-30) GO TO 2
      SPHI = P(2)/STH
      CPHI = P(1)/STH
      X2(1) = SPHI
      X2(2) = -CPHI
      X2(3) = 0.
      X3(1) = CPHI*CTH
      X3(2) = SPHI*CTH
      X3(3) = -STH
      GO TO 3
    1 X2(1) = P(2)
      X2(2) = -P(1)
      X2(3) = 0.
      X3(1) = 0.
      X3(2) = 0.
      X3(3) = -1.
      GO TO 3
    2 X2(1) = 0.
      X2(2) = -1.
      X2(3) = 0.
      X3(1) = 1.
      X3(2) = 0.
      X3(3) = 0.
C
C  FIND SCATTERING ANGLE
    3 THSTR = 21.E-3 / P(6) * P(4) / P(6) * SQRT(XRAD)
      CALL NVERT( THSTR, 0., TH1 )
C
      PHI = 2.*PI*RN(D)
      COSPHI = COS( PHI )
      SINPHI = SIN( PHI )
      PABS=0.
      DO 4 I = 1,3
      P(I) = P(I)+TH1*COSPHI*X2(I)+TH1*SINPHI*X3(I)
    4 PABS = PABS+P(I)**2
      PABS = SQRT(PABS)
C
C P IS CHANGED TO ACTUAL SIZE P(6).
      DO 5 I=1,3
    5 P(I) = P(I) / PABS * P(6)
      RETURN
      END
