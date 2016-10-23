C   21/11/77 007252320  MEMBER NAME  JEGAIN   (JADEGS)      FORTRAN
      SUBROUTINE JEGAIN( P, STRAC, POT, ZARO, * )
C  *---------------------------------------------------------
C  *
C  *  VERSION OF 21/11/77
C  *  FINDS ENERGY LOSS IN ABSORBER WITH POT AND ZARO
C  *  AND INCREASES ENERGY.
C  *  P IS CHANGED .
C  *---------------------------------------------------------
C
      DIMENSION P(6)
C
      DATA D / .1535E-4 /
C
C  FIND REDUCED VARIABLES
      A = 2. * D * ZARO * STRAC / P(5)
      B = 1.022E6 / POT
      XLOW = P(4) / P(5)
      IF( XLOW * XLOW .GT. 16.9205 ) GO TO 400
C
C  INITIALIZE GAUSS METHOD
      ITER = 0
      INCREM = 0
      DXOLD = 1.E13
      X = XLOW
C
  100 ITER = ITER + 1
      IF( ITER .GT. 15 ) RETURN1
      X2 = X * X
      X21 = X2 -1.
      FACLOG = ALOG( B * X21 )
      DX = ( XLOW - X + A * ( X2 / X21 * FACLOG - 1. ) ) /
     *     ( 1. + 2.*A/(X21*X21) * ( FACLOG - X2 ) )
      ADX = ABS( DX )
      INCREM = INCREM + 1
      IF( ADX .LE. DXOLD ) INCREM = 0
      IF( INCREM .GE. 9 ) RETURN1
      DXOLD = ADX
      X = X + DX
      IF( ADX .GT. 0.001 * ABS(X) ) GO TO 100
C
  200 P(4) = X * P(5)
      PTOT = SQRT( P(4)*P(4) - P(5)*P(5) )
      FACT = PTOT / P(6)
      P(6) = PTOT
      DO 300 I = 1,3
  300 P(I) = P(I) * FACT
      RETURN
C
  400 XLOW2 = XLOW * XLOW
      X = XLOW + A*(XLOW2/(XLOW2-1.)*ALOG(B*(XLOW2-1.)) - 1. )
      GO TO 200
C
      END
      BLOCK DATA
      COMMON / CJJONI / POTBEA, ZAROBE
     *                 ,POTTRI, ZAROTR
     *                 ,POTIVE, ZAROIV
     *                 ,POTRH0, ZAROR0
C    *                  POTJET, ZAROJE,
C    *                  POTRH1, ZAROR1,
C    *                  POTRH2, ZAROR2,
C    *                  POTRH3, ZAROR3,
C    *                  POTOVE, ZAROOV,
C    *                  POTTOF, ZAROTO,
C    *                  POTVES, ZARVES,
C    *                  POTZJL, ZAROJL,
C    *                  POTZJR, ZAROJR
C       GIVES AVERAGE IONISATION POTENTIAL AND VALUE FOR Z*RHO/A FOR
C       BETHE BLOCH FORMULA FOR : BEAMPIPE, BEAM PIPE COUNTER, INNER
C       VESSEL WALL, 4 DIFFERENT ROHACELL LAYERS, DRIFTGAS , OUTER
C       VESSEL WALL, TOF COUNTER, VESSEL WALL IN Z, LEFT AND RIGHT
C       ALU SUPPORTS FOR WIRES AND ROHACELL
      DATA POTBEA,ZAROBE        / 213.,1.3      /
      DATA POTTRI,ZAROTR        / 36.1,.59      /
      DATA POTIVE,ZAROIV        / 213.,1.3      /
      DATA POTRH0, ZAROR0       / 102., 0.11 /
C     DATA POTJET,ZAROJE        / 284.4,.00295   /
C     DATA POTRH1,ZAROR1        / 102.,.07     /
C     DATA POTRH2,ZAROR2        / 102.,.07     /
C     DATA POTRH3, ZAROR3       / 102. , .09  /
C     DATA POTOVE,ZAROOV        / 213.,1.3     /
C     DATA POTTOF,ZAROTO        / 36.1,.59      /
C     DATA POTVES,ZARVES        / 213.,1.3     /
C     DATA POTZJL, ZAROJL       / 213., 1.3       /
C     DATA POTZJR, ZAROJR       / 213., 1.3       /
      END
