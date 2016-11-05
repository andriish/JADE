C   21/07/80 312171955  MEMBER NAME  PRTOCI   (JADEGS)      FORTRAN
      SUBROUTINE PRTOCI( IND, CAP, RMIN, PHIMIT, SIG )
C
C *---------------------------------------------------------
C *  VERSION OF 25/07/79   LAST MOD  26/07/79   E.ELSEN
C *  FIND CIRCLE PARAMETERS FOR TRACK WITH POINTER IN PATR
C *  FOR PARABOLA FIT PARAMETERS IN PATR THE AREA BETWEEN CIRCLE
C *  AND PARABOLA IS MINIMIZED.
C *---------------------------------------------------------
C
      IMPLICIT INTEGER*2 (H)
      COMMON / BCS / IW(1)
      DIMENSION RW(1), HW(1)
      EQUIVALENCE (HW(1),RW(1),IW(1))
      DATA EPS / 1.E-7 /
C                                           BRANCH ACCORDING TO FIT TYPE
      IF( IW(IND+18) .EQ. 1 ) GO TO 1000
C                                           FIND LENGTH OF PARABOLA BRAN
      PSI = RW(IND+19)
      COSPSI = COS( PSI )
      SINPSI = SIN( PSI )
      X = (RW(IND+20)-RW(IND+5))*COSPSI + (RW(IND+21)-RW(IND+6))*SINPSI
      CURV = -RW(IND+22) * 2.
      SIG =1.
      IF( CURV .LT. 0. ) SIG = -1.
      CAP = CURV * SIG
      C = CAP
      EPSLOC = AMAX1( EPS, CAP * .001 )
C                                           INITIALISATION FOR NEWTON
C                                           APPROXIMATION
      ITER = 0
      A = 20. / ( 3.*X*X )
C                                           LOOP
  100 IF( ITER .GE. 10 ) GO TO 300
      ITER = ITER + 1
      PC = C*C*C + A*( C - CAP )
      PPC = 3.*C*C + A
      DC = - PC / PPC
      C = C + DC
      IF( ABS( DC ) .GT. EPSLOC ) GO TO 100
      CAP = C
C                                           COMPUTE STANDARD
C                                           PARAMETERS
  300 CONTINUE
      RHO = SIG / AMAX1( CAP, 1.E-8 )
      XCEN = RW(IND+20) + RHO*SINPSI
      YCEN = RW(IND+21) - RHO*COSPSI
      PHIMIT = ATAN2( YCEN, XCEN )
      RMIN = SQRT( YCEN*YCEN + XCEN*XCEN ) - ABS( RHO )
      RETURN
C
C                                           CIRCLE PARAMETERS
 1000 CAP = ABS( RW(IND+19) )
      RMIN = RW( IND+20 )
      PHIMIT = RW(IND+21)
      SIG = 1.
      IF( RW(IND+27) .LT. 0. ) SIG = -1.
      RETURN
      END
