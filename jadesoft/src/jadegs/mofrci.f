C   19/12/83 312192005  MEMBER NAME  MOFRCI   (JADEGS)      FORTRAN
C
C-----------------------------------------------------------------------
      SUBROUTINE MOFRCI( IND, R, P, CHARGE )
C-----------------------------------------------------------------------
C
C    VERSION OF 27/07/79     LAST MOD  10/08/79    E.ELSEN
C    FIND MOMENTUM P OF TRACK IND IN PATR BANK AT POSITION R.
C    MAG FIELD IS TAKEM FROM HEAD BANK.
C
C-----------------------------------------------------------------------
C
      IMPLICIT INTEGER*2 (H)
      COMMON / BCS / IW(1)
      DIMENSION RW(1), HW(1)
      EQUIVALENCE (HW(1),RW(1),IW(1))

      DIMENSION R(3), P(3)
      DATA BKGAUS / -4.5 /
C                                                FETCH MAG FIELD
      NPHEAD = IW( IBLN('HEAD') )
      IF( NPHEAD .EQ. 0 ) GO TO 10
      BKGAUS = HW( NPHEAD*2 + 30 ) / 1000.
C                                                FETCH TRACK PARAMETERS
   10 CALL PRTOCI( IND, CAP, RM, PHIM, SIG )
      DZDR = RW( IND + 30 )
C                                                TANGENT DIRECTION
      RMIT = RM + 1. / CAP
      RX = R(1) - RMIT*COS( PHIM )
      RY = R(2) - RMIT*SIN( PHIM )
      RTOT = SQRT( RX*RX + RY*RY ) * SIG
      EX = RY / RTOT
      EY = -RX / RTOT
C                                                RADIAL MOMENTUM
      PRAD = .3E-4*ABS(BKGAUS)/CAP
C                                                TOTAL MOMENTUM
      P(1) = PRAD * EX
      P(2) = PRAD * EY
      P(3) = PRAD * DZDR
C
      CHARGE = SIG
      IF( BKGAUS .LT. 0. ) CHARGE = -CHARGE
C
      RETURN
      END
