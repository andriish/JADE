C   07/06/78 402092120  MEMBER NAME  JMULSC   (S)           FORTRAN
C
C-----------------------------------------------------------------------
      SUBROUTINE JMULSC( P, XRAD )
C-----------------------------------------------------------------------
C
C   AUTHOR:   W. BARTEL   01/06/78 :  CALCULATE MULTIPLE SCATTERING
C
C   LAST MOD  J. HAGEMANN 30/01/84 :  NEW FORMULA USED. GENERAL TIDY-UP
C
C-----------------------------------------------------------------------
C
C       CALCULATE MULTIPLE SCATTERING.
C       TOTAL ANGLE IS RANDOMLY GENERATED.
C
C       P             MOMENTUM VECTOR OF INCIDENT PARTICLE
C                     THIS VECTOR IS OVERWRITTEN BY SCATTERED MOMENTUM.
C
C       P(1) ... P(3) MOMENTUM COMPONENTS
C       P(4)          ENERGY OF PARTICLE
C       P(5)          REST MASS
C       P(6)          TOTAL MOMENTUM
C
C       XRAD          THICKNESS OF RADIATOR IN RADIATION LENGTH
C
C-----------------------------------------------------------------------
C
      DIMENSION  P(6), X2(3), X3(3)
C
      DATA  PI / 3.14159 /
C
C-------------------  C O D E  -----------------------------------------
C
      IF( XRAD .LT. 1.E-30 ) RETURN
C
C                            CHANGE P TO UNIT VECTOR
C
      DO  100  I = 1,3
        P(I) = P(I) / P(6)
  100 CONTINUE
C
C                            FIND OTHER UNIT VECTORS THAT ARE
C                            PERPENDICULAR.
C
      CTH = P(3)
      IF( ABS(CTH) .LT. 1.E-30 ) GO TO 1
      STH = SQRT( 1.0 - CTH * CTH )
      IF( STH .LT. 1.E-30 ) GO TO 2
C
      SPHI  =  P(2) / STH
      CPHI  =  P(1) / STH
C
      X2(1) =  SPHI
      X2(2) = -CPHI
      X2(3) =  0.0
C
      X3(1) =  CPHI * CTH
      X3(2) =  SPHI * CTH
      X3(3) = -STH
      GO TO 3
C
    1 X2(1) =  P(2)
      X2(2) = -P(1)
      X2(3) =  0.0
C
      X3(1) =  0.0
      X3(2) =  0.0
      X3(3) = -1.0
      GO TO 3
C
    2 X2(1) =  0.0
      X2(2) = -1.0
      X2(3) =  0.0
C
      X3(1) =  1.0
      X3(2) =  0.0
      X3(3) =  0.0
C
C                            FIND SCATTERING ANGLE
C
C                            THE OLD FORMULA WAS:
C
C              THSTR = 21.E-3 / P(6) * P(4) / P(6) * SQRT(XRAD)
C
C                            THE NEW FORMULA IS:
C
    3 THSTR = 20.E-3 / P(6)*P(4) / P(6) *SQRT(XRAD)*(1.+ALOG10(XRAD)/9.)
      CALL NVERT( THSTR, 0., TH1 )
C
      PHI    = 2.0 * PI * RN(D)
      COSPHI = COS( PHI )
      SINPHI = SIN( PHI )
      PABS   = 0.0
C
      DO  4  I = 1,3
        P(I) = P(I) + TH1 * COSPHI * X2(I) + TH1 * SINPHI * X3(I)
        PABS = PABS + P(I)**2
    4 CONTINUE
C
      PABS = SQRT(PABS)
C
C                            P IS CHANGED TO ACTUAL SIZE P(6).
C
      DO  5  I = 1,3
        P(I) = P(I) / PABS * P(6)
    5 CONTINUE
      RETURN
      END
