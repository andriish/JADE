C   04/11/82 606091755  MEMBER NAME  JVTXCH   (S)           FORTRAN
C
C-----------------------------------------------------------------------
      SUBROUTINE JVTXCH ( R, P, PENETR, PENETZ, JRETRN, RDOWN, RUP,
     +                    ZDOWN, ZUP, DRMAX, * )
C-----------------------------------------------------------------------
C
C   AUTHOR:   J. HAGEMANN  12/11/82 :  PROPAGATES CHARGED PARTICLE
C             R. RAMCKE                THROUGH THE VERTEX CHAMBER
C
C   LAST MOD  J. HAGEMANN  21/09/84 :  USES NEW VERSION OF DRSTEP
C
C-----------------------------------------------------------------------
C
      IMPLICIT INTEGER*2(H)
      LOGICAL FOUND, ENRLOS, ELOSS, MULSC
C
      COMMON / CSTORE / MHITS, IPV, HSTORE(2000)
C
      COMMON / CJTRLE / TOTLEN, STPLEN
      COMMON / CJSWLD / ITIMOD, MULSC, ELOSS
C
      COMMON / CJVCEL / MCELL, MWIRE
C
      COMMON / CJVTXC / RVEC, ANG1, ANG2, DISTPW, FIRSTP, DISTW1,
     +                  ANGL, COSLOR, SINLOR,
     +                  ZRESV, ZMAXV, ZOFFV, ZNAMP, ZALV, TIMEV
      COMMON / CIJONV / POTVXC, ZAROVC,
     +                  POTVGA, ZAROVG
      COMMON / CGEOV  / RPIPV, DRPIPV, XRLPIV, RVXC, DRVXC, XRLVXC,
     +                  ZVXCM, DZVCM, XRZVCM, ZVXCP, DZVCP, XRZVCP,
     +                  XRVTXC
C
      DIMENSION R(5), P(7), RX(5), RXOLD(3), RXREL1(3), RXREL2(3)
      DIMENSION HDV(4)
      EQUIVALENCE (HDV(1), HMWTOT), (HDV(2), HITAML),
     +            (HDV(3), HITAMR), (HDV(4), HITIM)
C
      DATA  ITERMX / 300 /
      DATA  PMIN / 0.01 /
      DATA  BINWID / 0.003 /
      DATA  TWOPI  / 6.2831853 /
C
C------------------------  C O D E  ------------------------------------
C
      PENETR = 0.
      PENETZ = 0.
      R(4) = SQRT( R(1) * R(1) + R(2) * R(2) )
C
 10   IF ( R(3) .GE. ZDOWN ) GOTO 20
           JRETRN = 3
           RETURN
 20   IF ( R(3) .LE. ZUP )   GOTO 30
           JRETRN = 2
           RETURN
 30   IF ( R(4) .GE. RDOWN ) GOTO 40
           JRETRN = 1
           RETURN
 40   IF ( R(4) .LE. RUP )   GOTO 50
           JRETRN = 0
           RETURN
C
  50  R(5) = ATAN2(R(2), R(1))
           IF ( R(5) .LT. ANG1 ) R(5) = R(5) + TWOPI
           ICELL = IFIX(( R(5) - ANG1 )/ANG2 + .4999)
           ICELL = MOD( ICELL, MCELL )
           PHI = FLOAT( ICELL )*ANG2 + ANG1
           COSPHI = COS(PHI)
           SINPHI = SIN(PHI)
C                                                 ROTATE COORD.
      CALL JROTAT ( R, COSPHI, SINPHI )
      CALL JROTAT ( P, COSPHI, SINPHI )
      RX(1) = R(1)
      RX(2) = R(2)
      RX(3) = R(3)
C                                                 CHECK FRAME OF CELL
      HMWOLD = 0
C
C
      XTRACK = 0.
      XLIMIT = (( P(6)*P(6)/P(4)/21.E-3*BINWID)**2*XRVTXC)**.333333
      XLIMIT = AMIN1( AMAX1(10.,XLIMIT), 100. )
      ETRACK = 0.
      ENRLOS = .FALSE.
      IF(ELOSS.AND.(P(4)/P(5).LT.3..OR.P(5).LT..001)) ENRLOS=.TRUE.
      ELIMIT = AMIN1( 100., 500.*P(6)*P(6))
C
C
      CALL JFTEST ( RX, P, JRETRN, *2000 )
      JRETRN = 4
      MWBASE = ICELL * MWIRE
C
      RXPRO = RX(1) + RX(2) * TANLOR
      MW = ( RXPRO - DISTW1 ) / DISTPW + 1.5
C                                                 PREPARE CHANGE
C                                                 TO NEW COORD.
C                                                 TRX=TRANSLATION
      TRX = DISTW1 + ( MW - 1) * DISTPW
C                                                 START TRACKING
      ITERAT = 0
200   ITERAT = ITERAT + 1
C                                                 SAVE OLD COORD.
           RXOLD(1) = RX(1)
           RXOLD(2) = RX(2)
           RXOLD(3) = RX(3)
      CALL JSTEP( RX, P, DRTOT, DRMAX )
           TOTLEN = TOTLEN + DRTOT
           XTRACK = XTRACK + DRTOT
           ETRACK = ETRACK + DRTOT
C                                                 CHECK
      CALL JFTEST ( RX, P, JRETRN, *2000 )
C                                                 TEST FOR DEATH SPACE
      IF ( MW .LT. 1 .OR. MW .GT. MWIRE ) GOTO 600
C
      RXTRA = RXOLD(1) - TRX
      RYTRA = RXOLD(2)
C
C      WRITE ( 6, 390 ) RXTRA,  RYTRA
C390   FORMAT (' RXTRA =',  F8.3, '   RYTRA= ' , F8.3 )
C
      RXREL1(1) = RXTRA * COSLOR + RYTRA * SINLOR
      RXREL1(2) = RYTRA * COSLOR - RXTRA * SINLOR
C
      RXTRA = RX(1) - TRX
      RYTRA = RX(2)
C
C      WRITE ( 6, 396 ) RXTRA,  RYTRA
C396   FORMAT (' RXTRA =',  F8.3, '   RYTRA= ' , F8.3 )
C
      RXREL2(1) = RXTRA * COSLOR + RYTRA * SINLOR
      RXREL2(2) = RYTRA * COSLOR - RXTRA * SINLOR
C                                                COORD.REL FOR FIRST
C                                                STEP POINT
        ASSIGN 600 TO LABEL
C
 400  CALL JVDTIM ( RXREL1, RXREL2, FOUND, DIST, SLOPE )
      IF ( .NOT. FOUND ) GOTO 500
C                                                ABSOLUTE WIRE NUMBER
      HMWTOT = MWBASE + MW
C--------------------------------------------------------------------
C                                                DRIFT TIME IN MICROM.
      AITIM = AMIN1 (DIST/TIMEV, 32767.)
      HITIM = HFIX(AITIM)
C                                                Z - COORD. (MM)
      HITAML = RX(3) + SIGN(.5, RX(3))
      HITAMR = 0
C---------------------------------------------------------------------
      IF ( HMWOLD .NE. HMWTOT ) GOTO 460
C                                                CHANGE IN WIRE NUMBER ?
      IF ( HSTORE(IPV - 1) .LE. HITIM ) GOTO 500
      GOTO 470
C                                                SMALLER DRIFT TIME FOR
C                                                THE SAME WIRE ?
 460  MHITS = MHITS + 1
      IPV = IPV + 4
      IF (IPV .GT. 2000) RETURN 1
C
C
 470  CALL MVC ( HSTORE, 2*IPV - 10 , HDV , 0 , 8 )
C
C
           HMWOLD = HSTORE(IPV - 4)
C
 500  CONTINUE
      GOTO LABEL, (600, 900)
C                                               WIRE NUMBER FOR 2.POINT
 600  MW1 = MW
      RXPRO = RX(1) + RX(2) * TANLOR
      MW = ( RXPRO - DISTW1 ) / DISTPW + 1.5
C                                                 PREPARE CHANGE
C                                                 TO NEW COORD.
C                                                 TRX=TRANSLATION
      TRX = DISTW1 + ( MW - 1) * DISTPW
C
      IF ( MW1 .EQ. MW .OR. MW .LT. 1
     +     .OR. MW .GT. MWIRE )       GOTO 900
C
      RXTRA = RXOLD(1) - TRX
      RYTRA = RXOLD(2)
      RXREL1(1) = RXTRA * COSLOR + RYTRA * SINLOR
      RXREL1(2) = RYTRA * COSLOR - RXTRA * SINLOR
C
      RXTRA = RX(1) - TRX
      RYTRA = RX(2)
      RXREL2(1) = RXTRA * COSLOR + RYTRA * SINLOR
      RXREL2(2) = RYTRA * COSLOR - RXTRA * SINLOR
      ASSIGN 900 TO LABEL
      GOTO 400
C
 900  CONTINUE
          IF ( TOTLEN .GT. STPLEN )  GOTO 2000
C
                 IF( .NOT. ENRLOS .OR. ETRACK .LT. ELIMIT ) GO TO 930
       CALL JELOSS ( P, ETRACK, POTVGA, ZAROVG, XRVTXC,
     +               RX, COSPHI, SINPHI )
                       ETRACK = 0.
                 IF( P(6) .LE. PMIN ) GO TO 2000
  930            IF( .NOT. MULSC .OR. XTRACK .LT. XLIMIT ) GO TO 1000
       CALL JMULSC( P, XTRACK/XRVTXC )
                       XTRACK = 0.
C
 1000  IF ( ITERAT .LT. ITERMX ) GOTO 200
C                                                 ROTATE BACK
 2000 CALL JROTAT ( RX, COSPHI, -SINPHI )
      CALL JROTAT ( P, COSPHI, -SINPHI)
      R(1) = RX(1)
      R(2) = RX(2)
      R(3) = RX(3)
C
      IF ( JRETRN .EQ. 5 ) GOTO 50
      R(4) = SQRT( R(1) * R(1) + R(2) * R(2) )
      IF ( JRETRN .EQ. 0 ) PENETR = R(4) - RUP
      IF ( JRETRN .EQ. 1 ) PENETR = RDOWN - R(4)
      IF ( JRETRN .EQ. 2 ) PENETZ = R(3) - ZUP
      IF ( JRETRN .EQ. 3 ) PENETZ = ZDOWN - R(3)
C
      RETURN
      END
