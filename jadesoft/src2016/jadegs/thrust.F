      SUBROUTINE THRUST( P, NP, NMAXP, PTHR, AXIS, ITMERR )
      DIMENSION P(1), AXIS(3)
      DIMENSION PIN(320), IPERM(200),IPS(320),PS(320)
      COMMON / CWORK / WORK(840)
      EQUIVALENCE (WORK(1),PIN(1)),(WORK(321),IPERM(1))
      EQUIVALENCE (WORK(521),PS(1),IPS(1))
      DATA IPINC / 4 /
      DATA NLIMIT / 15 /
      DATA NSEC / 2 /
      DATA IER0 / 0/, IER1 /0/, IER2 /0/
      ITMERR = 0
      AXIS(1) = 0.
      AXIS(2) = 0.
      AXIS(3) = 1.
      PTHR = .5
      IF(
     - NP .LE. 0
     -)THEN
      IF(
     - IER0 .LT. 10
     -)THEN
      WRITE(6,9103)
 9103 FORMAT(' +++ ERROR IN THRUST.  CALLED WITH ZERO PARTICLES +++')
      ENDIF
      IER0 = IER0 + 1
      RETURN
      ENDIF
      NMAXP1 = NMAXP
      IF(
     -  NP.GE.NLIMIT .AND. ( NMAXP.EQ.0 .OR. NMAXP.GT.NLIMIT )
     -)THEN
      IF(
     - IER1 .LT. 10
     -)THEN
      WRITE(6,9101) NP,NLIMIT,NMAXP
 9101 FORMAT(' +++++  WARNING FROM THRUST ROUTINE  +++++'/
     +       2X,I4,' ARE TOO MANY INPUT VECTORS. INTERNAL LIMIT=',I4,
     +' INPUT CUTOFF NMAXP=',I4,'. ONLY NLIMIT PARTICLES ARE PERMUTED')
      NMAXP1 = NLIMIT
      ENDIF
      IER1 = IER1 + 1
      ENDIF
      NTOT = NP
      CALL BALANC( P, NTOT, PSUM )
      IF(
     - PSUM .LT. 1.E-6
     -)THEN
      IF(
     - IER2 .LT. 10
     -)THEN
      WRITE(6,9104)
 9104 FORMAT(' +++ ERROR IN THRUST. MOMENTUM SUM AFTER BALANCE=0. +++')
      ENDIF
      IER2 = IER2 + 1
      RETURN
      ENDIF
      IF(
     - NTOT.GT.NMAXP1 .AND. NMAXP1.NE. 0
     -)THEN
      ASSIGN 17001 TO IZZZ01
      GOTO 17000
17001 CONTINUE
      NP1 = NTOT
      NTOT = NMAXP1 - 1
      ASSIGN 17003 TO IZZZ02
      GOTO 17002
17003 CONTINUE
      CALL BALANC( PIN, NTOT, PSUM1 )
      ASSIGN 17005 TO IZZZ03
      GOTO 17004
17005 CONTINUE
      ASSIGN 17007 TO IZZZ04
      GOTO 17006
17007 CONTINUE
      ELSE
      CALL UCOPY( P, PIN, NTOT*4 )
      ASSIGN 17008 TO IZZZ03
      GOTO 17004
17008 CONTINUE
      ENDIF
      RETURN
17000 CONTINUE
      DO 13000 J=1,NTOT
      IPERM(J) = J*4
13000 CONTINUE
13001 CONTINUE
      M = NTOT / 2
15000 CONTINUE
      IF(
     -  M.GT.0
     -)THEN
      K = NTOT - M
      DO 13002 J=1,K
      I = J
15002 CONTINUE
      IF(
     -  I.GT.0
     -)THEN
      ILOW = IPERM(I)
      IHIGH = IPERM(I+M)
      IF(
     -  P(IHIGH) .GT. P(ILOW)
     -)THEN
      IPERM(I) = IHIGH
      IPERM(I+M) = ILOW
      I = I - M
      ELSE
      GOTO 15003
      ENDIF
      GOTO 15002
      ENDIF
15003 CONTINUE
13002 CONTINUE
13003 CONTINUE
      M = M/2
      GOTO 15000
      ENDIF
15001 CONTINUE
      GOTO IZZZ01
17002 CONTINUE
      DO 13004 J=1,NTOT
      IPJ = IPERM(J)
      J4 = J*4
      PIN(J4  ) = P(IPJ  )
      PIN(J4-1) = P(IPJ-1)
      PIN(J4-2) = P(IPJ-2)
      PIN(J4-3) = P(IPJ-3)
13004 CONTINUE
13005 CONTINUE
      GOTO IZZZ02
17004 CONTINUE
      NPERM = 2**(NTOT-1)
      NOUT = ( NPERM - 1) / 8192 + 1
      NIN = MIN0( 8192, NPERM )
      NUP = NTOT*4 - 3
      PS(1) = 0.
      PS(2) = 0.
      PS(3) = 0.
      IPS(4) = 1
      PM2 = 0.
      J = 1
      K = 1
      DO 13006 IOUT = 1, NOUT
      IF(
     - JUHR(NSEC) .EQ. 2
     -)THEN
      AXIS(1) = 0.
      AXIS(2) = 0.
      AXIS(3) = 1.
      PTHR = .5
      ITMERR = 1
      WRITE(6,9102) NSEC
 9102 FORMAT(' +++++++   TIME PROBLEMS IN THRUST - ROUTINE   ++++++++'/
     +       '        REMAINING TIME IS LESS THAN ',I4,' SECS.'/
     +       '         AXIS WAS SET TO Z AXIS AND THRUST = .5')
      RETURN
      ENDIF
      DO 13008 IIN = 1, NIN
      PS1 = PS(K  ) + PIN(J  )
      PS2 = PS(K+1) + PIN(J+1)
      PS3 = PS(K+2) + PIN(J+2)
      PCL2 = PS1*PS1 + PS2*PS2 + PS3*PS3
      IF(
     -  PCL2 .GT. PM2
     -)THEN
      PM2 = PCL2
      AXIS(1) = PS1
      AXIS(2) = PS2
      AXIS(3) = PS3
      ENDIF
      IF(
     - J .LT. NUP
     -)THEN
      J = J + 4
      K = K + 4
      PS(K  ) = PS1
      PS(K+1) = PS2
      PS(K+2) = PS3
      IPS(K+3) = J
      ELSE
      J = IPS(K+3)
      K = K - 4
      ENDIF
13008 CONTINUE
13009 CONTINUE
13006 CONTINUE
13007 CONTINUE
      AXISL = SQRT( PM2)
      DO 13010  J=1,3
      AXIS(J) = AXIS(J) / AXISL
13010 CONTINUE
13011 CONTINUE
      PTHR = 2. * AXISL / PSUM
      GOTO IZZZ03
17006 CONTINUE
      NIPINC = NP1*IPINC
      SPCOS = 0.
      DO 13012 J=1,NIPINC,IPINC
      SPCOS = SPCOS + ABS( AXIS(1)*P(J)+AXIS(2)*P(J+1)+AXIS(3)*P(J+2) )
13012 CONTINUE
13013 CONTINUE
      PTHR = SPCOS / PSUM
      GOTO IZZZ04
      END
      SUBROUTINE BALANC( P, NTOT, PSUM )
      DIMENSION P(1)
      PSUM1 = 0.
      PSUM2 = 0.
      PSUM3 = 0.
      PSUM  = 0.
      N4 = NTOT*4
      DO 13000 J=1,N4,4
      PSUM1 = P(J  ) + PSUM1
      PSUM2 = P(J+1) + PSUM2
      PSUM3 = P(J+2) + PSUM3
      PSUM  = P(J+3) + PSUM
13000 CONTINUE
13001 CONTINUE
      PTOT = SQRT( PSUM1*PSUM1 + PSUM2*PSUM2 + PSUM3*PSUM3 )
      IF(
     -  PTOT .GT. .01
     -)THEN
      P(N4+1) = -PSUM1
      P(N4+2) = -PSUM2
      P(N4+3) = -PSUM3
      P(N4+4) = PTOT
      PSUM = PSUM + PTOT
      NTOT = NTOT + 1
      ENDIF
      RETURN
      END
