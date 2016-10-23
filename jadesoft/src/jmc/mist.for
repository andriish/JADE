C   12/07/79 312081235  MEMBER NAME  MIST     (S)           FORTRAN
      SUBROUTINE MCHIST
C-----------------------------------------------------------
C
C   VERSION OF 09/05/79  LAST MOD    29/05/80    E.ELSEN
C   COMPUTE SOME HISTOGRAMS FOR EVERY EVENT.
C-----------------------------------------------------------
C
      IMPLICIT INTEGER*2 (H)
      COMMON / BCS / IW(1)
      DIMENSION RW(1), HW(1)
      EQUIVALENCE (HW(1), RW(1), IW(1))
C
      DATA ICALL / 0 /
C
      IF( ICALL .NE. 0 ) GO TO 100
      ICALL = 1
      CALL THIST(  1,'# OF NEUTRAL PARTICLES (INPUT)$' )
      CALL DHIST(  1, 0., 100. )
      CALL THIST(  2,'# OF CHARGED PARTICLES (INPUT)$' )
      CALL DHIST(  2, 0., 100. )
      CALL THIST(  3,'# OF CHARGED PARTICLES (PATR)$' )
      CALL DHIST(  3, 0., 100. )
      CALL THIST(  4,'# OF I.D. HITS$' )
      CALL DHIST(  4, 0., 1500. )
      CALL THIST(  5,'LG TOT ENERGY$' )
      CALL DHIST(  5, 0., 50. )
      CALL THIST(  6,'LG END CAP ENERGY$' )
      CALL DHIST(  6, 0., 50. )
      CALL THIST(  7,'LG BARREL ENERGY$' )
      CALL DHIST(  7, 0., 50. )
      CALL THIST(  8,'SUM PTOT$' )
      CALL DHIST(  8, 0., 50. )
      CALL THIST(  9,'SUM PT$' )
      CALL DHIST(  9, 0., 50. )
      CALL GEPIC
      CALL DST0 (  1,4,80,'LEAD GLASS ENERGY     ;')
C
C                                           # OF INPUT PARTICLES
  100 IND = IW( IBLN('VECT') )
      IF( IND .EQ. 0 ) GO TO 1000
      CALL UHIST( 1, FLOAT( IW( IND +  6  ) ) )
      CALL UHIST( 2, FLOAT( IW( IND +  5  ) ) )
C
C                                           # OF PATR TRACKS
 1000 IND = IW( IBLN('PATR') )
      IF( IND .EQ. 0 ) GO TO 2000
      CALL UHIST( 3, FLOAT( IW( IND + 2   ) ) )
C
      IPH = IW( IBLN( 'HEAD' ) )
      IF( IPH .EQ. 0 ) GO TO 2000
      BKGAUS = HW( IPH*2 + 30 ) * .001
      BKGAUS = ABS( BKGAUS )
      PT = 0.
      PTOT = 0.
      LP = IW( IND + 3 )
      IP0 = IND + IW( IND + 1 )
      IP9 = IP0 + ( IW(IND+2) - 1 ) * LP
      DO 1100 J = IP0, IP9, LP
      ACURV = ABS( RW(J+25) )
      PTRANS = .3E-4 * BKGAUS / AMAX1( ACURV, 1.E-6 )
      PT = PT + PTRANS
 1100 PTOT = PTOT + PTRANS * SQRT( 1. + RW(J+30)**2 )
      CALL UHIST( 8, PTOT )
      CALL UHIST( 9, PT )

C
C                                           # OF JETC HITS
 2000 IND = IW( IBLN('JETC') )
      IF( IND .EQ. 0 ) GO TO 3000
      NHITS = ( HW(IND*2+99) - HW(IND*2+3) ) / 4
      CALL UHIST( 4, FLOAT( NHITS ) )
C
C                                           # LG ENERGY
 3000 IND = IW( IBLN('ALGN') )
      IF( IND .EQ. 0 ) GO TO 4000
      ETOT = 0.
      ECAP = 0.
C
      IL = IND*2 + 6
      IH = IL + HW(IL) - 3
      IEC = IL + HW(IND*2+4) - 3
C
      DO 3010 I = IL,IH, 2
      IF( I .LE. IEC ) GO TO 3010
      ECAP = ECAP + HW(I+2)
 3010 ETOT = ETOT + HW(I+2)
      ECAP = ECAP * .001
      ETOT = ETOT * .001
C
      CALL UHIST(   5, ETOT )
      CALL UHIST(   6, ECAP )
      CALL UHIST(   7, ETOT - ECAP )
      CALL HIST(1,ETOT)
C
 4000 RETURN
      END
