C   17/05/88 805171200  MEMBER NAME  CRDOCA   (S)           FORTRAN77
      SUBROUTINE CRDOCA( IND, R, P, RDOCA, CHARGE )
C-----------------------------------------------------------
C
C    VERSION OF 27/07/79     LAST MOD  17/01/85    E.ELSEN
C    FIND MOMENTUM P OF TRACK IND IN PATR BANK AT POSITION R.
C    RDOCA IS THE POINT OF CLOSEST APPROACH ON THE TRACK
C    MAG FIELD IS TAKEN FROM HEAD BANK.
C-----------------------------------------------------------
C
      IMPLICIT INTEGER*2 (H)
      COMMON / BCS / IW(1)
      DIMENSION RW(1), HW(1)
      EQUIVALENCE (HW(1),RW(1),IW(1))
      REAL*8 RMIT,RX,RY,RTOT,RHO
C
      DIMENSION R(3), P(3), RDOCA(3)
      DATA BKGAUS / -4.5 /
C                                                FETCH MAG FIELD
      NPHEAD = IW( IBLN('HEAD') )
      IF( NPHEAD .EQ. 0 ) GO TO 10
      BKGAUS = HW( NPHEAD*2 + 30 ) / 1000.
C                                                FETCH TRACK PARAMETERS
   10 CALL PRTOCI( IND, CAP, RM, PHIM, SIG )
      DZDR = RW( IND + 30 )
C                                                TANGENT DIRECTION
      RHO  = 1. / CAP
      RMIT = RM + RHO
      RX = R(1) - RMIT*COS( PHIM )
      RY = R(2) - RMIT*SIN( PHIM )
      RTOT = DSQRT( RX*RX + RY*RY )
      EX = RY / RTOT * SIG
      EY = -RX / RTOT * SIG
C                                                RADIAL MOMENTUM
      PRAD = .3E-4*ABS(BKGAUS)/CAP
C                                                TOTAL MOMENTUM
      P(1) = PRAD * EX
      P(2) = PRAD * EY
      P(3) = PRAD * DZDR
C                                           DOCA POINT
      RDOCA(1) = R(1) - RX*(1.D0-RHO/RTOT)
      RDOCA(2) = R(2) - RY*(1.D0-RHO/RTOT)
      RDOCA(3) = DZDR*SQRT(RDOCA(1)**2+RDOCA(2)**2)
C
      CHARGE = SIG
      IF( BKGAUS .LT. 0. ) CHARGE = -CHARGE
C
      RETURN
      END
