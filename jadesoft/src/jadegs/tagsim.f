C   19/05/81 105191642  MEMBER NAME  TAGSIM   (JADEGS)      FORTRAN
      SUBROUTINE TAGSIM( ITAG )
C-----------------------------------------------------------
C  VERSION OF 03/08/79     LAST MOD 03/08/79    E.ELSEN
C  SIMULATE TAGGING TRIGGER
C  ROUTINE CHECKS MC INPUT FOUR VECTOR BANK FOR ELECTRONS
C  ABOVE PCUT GEV MOMENTUM WITHIN THE ANGULAR ACCEPTANCE
C  OF THL AND THH
C  ITAG = 2 IF BOTH SIDES WERE HIT
C       = 1 IF ONLY ONE SIDE WAS HIT
C       = 0 ELSE
C-----------------------------------------------------------
C
      COMMON / BCS / IW(1)
      DIMENSION RW(1),HW(1)
      EQUIVALENCE (HW(1),RW(1),IW(1))
C
      DATA THL / .034 /
      DATA THH / .080 /
      DATA PCUT / 1.5 /
C
      ITAG = 0
C                                           GET VECT POINTER
      NPVECT = IW( IBLN('VECT'))
      IF( NPVECT .EQ. 0 ) RETURN
C
C                                           LOOP OVER PARTICLES
C                                           CHECK FOR PHOTONS OR
C                                           ELECTRONS
      IP0 = NPVECT + IW(NPVECT+1)
      L = IW( NPVECT + 2 )
      IP9 = IP0 + L * ( IW( NPVECT + 4 ) - 1 )
      ITAGP = 0
      ITAGM = 0
      DO 1000 J = IP0, IP9, L
      IF( IW(J+7) .GT. 2 ) GO TO 1000
      PTOT = SQRT(RW(J+1)**2 +RW(J+2)**2 + RW(J+3)**2 )
      IF( PTOT .LT. PCUT ) GO TO 1000
      COSP = RW(J+3)/PTOT
      IF( COS(THH).GT.ABS(COSP) .OR. ABS(COSP).GT.COS(THL) ) GO TO 1000
      IF( COSP .GE. 0. ) ITAGP = 1
      IF( COSP .LT. 0. ) ITAGM = 1
 1000 CONTINUE
C                                           TAGGING WORD
      ITAG = ITAGP + ITAGM
      RETURN
      END
