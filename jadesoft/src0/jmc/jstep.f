C   29/09/78 606101309  MEMBER NAME  JSTEP9   (S)           FORTRAN
C
C-----------------------------------------------------------------------
      SUBROUTINE JSTEP( R, P, DRTOT, DRMAX )
C-----------------------------------------------------------------------
C
C   AUTHOR:   E. ELSEN     27/10/78 :  DOES THE STEPPING THROUGH DET.
C
C        MOD  E. ELSEN     13/11/81 :  MADE SAVE AGAINST MAG FIELD < 0
C
C   LAST MOD  J. HAGEMANN  28/08/84 :  DRMAX NOW GIVEN AS ACT. PARAMETER
C
C        THIS SUBROUTINE CALCULATES NEXT POINT R OF
C        TRACK OF PARTICLE WITH MOMENTUM P.
C        STARTING POINT R IS OVERWRITTEN BY COORDINATES OF NEXT
C        POINT. ALL NECCESSARY INFORMATION ABOUT
C        PARTICLE IS STORED UNDER VECTOR P :
C
C           P(1) = X - COMPONENT OF MOMENTUM
C           P(2) = Y - COMPONENT OF MOMENTUM
C           P(3) = Z - COMPONENT OF MOMENTUM
C           P(4) = TOTAL ENERGY OF PARTICLE
C           P(5) = REST MASS OF PARTICE
C           P(6) = LENGTH OF THREE VECTOR P
C                  ALL UNITS IN GEV.
C           P(7) = CHARGE OF PARTICLE IN UNITS OF
C                  ELECTRON CHARGE.
C
C        THE FIRST THREE COMPONENTS OF P WILL BE
C        CHANGED TO NEW MOMENTUM VECTOR IN R+DR.
C        DRTOT IS STEPLENGTH.
C-----------------------------------------------------------------------
C
      COMMON/CGEO1/BKGAUS
C
      DIMENSION R(3),P(7)
C
      DATA ANGLIN  / 0.03 /
C
C------------------------  C O D E  ------------------------------------
C
      EB = 0.3E-4 * ABS(BKGAUS)
      DT = DRMAX / P(6)
      ODT = EB * DT
                  IF( ODT .LT. ANGLIN ) GO TO 10
                  ODT = ANGLIN
                  DT = ODT / EB
   10 ODT = ODT*P(7)
      IF( BKGAUS .LT. 0. ) ODT = - ODT
C
C
      DP1 =  P(2)*ODT
      DP2 = -P(1)*ODT
      ODT2 = ODT*ODT
      CORREC = 1. - .5*ODT2 + .375*ODT2*ODT2
C
C
      R(1) = ( P(1) + .5*DP1 ) * DT  + R(1)
      R(2) = ( P(2) + .5*DP2 ) * DT  + R(2)
      R(3) = P(3)*DT + R(3)
C
C
      P(1) = ( P(1) + DP1 ) * CORREC
      P(2) = ( P(2) + DP2 ) * CORREC
C
C
      DRTOT = ( ( DP1*DP1 + DP2*DP2 ) * .125 + P(6) ) * DT
      RETURN
      END
