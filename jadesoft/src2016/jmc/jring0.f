C   25/08/83 308251856  MEMBER NAME  JRING0   (S1)          FORTRAN
C
C-----------------------------------------------------------------------
       SUBROUTINE JRING( IRING, ICELL, COSAL, SINAL, X, P, IRETRN , * )
C-----------------------------------------------------------------------
C
C  AUTHOR:  E. ELSEN     07/12/78 : TRACK THROUGH JET CHAMBER RING
C
C       MOD E. ELSEN     07/10/81 :
C  LAST MOD C. BOWDERY   18/08/81 : NEW CODE FOR 4V/JETC ASSOCIATION
C
C       PROPAGATE PARTICLE IN CELL ICELL IN RING IRING.
C
C       X AND P ARE THE APROPRIATE COORDINATES IN CELL/RING.
C       COSAL AND SINAL ARE ASSOCIATED COS AND SIN OF ROTATION.
C       IRETRN IS RETURN CODE:
C
C       IRETRN = 0 : NEXT ELEMENT, INCREASING X
C              = 1 : NEXT ELEMENT, DECREASING X
C              = 2 : NEXT ELEMENT, INCREASING Z
C              = 3 : NEXT ELEMENT, DECREASING Z
C              = 4 : PARTICLE STOPPED OR DECAYED
C              = 5 : NEXT ELEMENT, CHANGING PHI
C
C       DRIFT INFORMATION IS STORED IN HLIST ARRAY
C
C         HLIST(4(HIT-1)+1) = WIRE NUMBER
C         HLIST(4(HIT-1)+2) = COORDINATE IN MM
C         HLIST(4(HIT-1)+3) = NORMALIZED AMPLITUDE
C         HLIST(4(HIT-1)+4) = DRIFT TIME
C
C       THE 4 VECTOR PARTICLE/JETC HIT ASSOCIATION IS STORED IN H4VHTR
C
C         HV4HTR( HIT )     = 2 * PARTICLE_NUMBER + VECT_BANK_NUMBER
C
C  RETURN 1 IF OVERFLOW IN HLIST REGISTER
C-----------------------------------------------------------------------
C
      IMPLICIT INTEGER*2 (H)
      LOGICAL FOUND, ENRLOS, ELOSS, MULSC
C
      DIMENSION X(3), P(7), X1(3), X1PRIM(2), X2PRIM(2), HDRIFT(4)
C
C             THESE 3 COMMONS STORE THE INFORMATION ABOUT
C             THE 4 VECTOR TRACK, WHICH VECT BANK IT IS IN AND
C             THE HIT INFORMATION THAT IS TO BE CREATED BY THIS S/R.
C
      COMMON / CJTCDC / IBANK, IPART
      COMMON / C4VHIT / H4VHTR(400), H4VHIT(4000)
      COMMON / CWORK  / NHITS, INEXT, HLIST(1600)
C
      EQUIVALENCE (HDRIFT(1),HNWTOT),(HDRIFT(2),HIAMPL),
     *            (HDRIFT(3),HIAMPR),(HDRIFT(4),HITIM)
C
      COMMON / CJCELL / NCELL(3),
     *                  NWIRES(3)
      COMMON / CJTRLE / TOTLEN, STPLEN
      COMMON / CJSWLO / ITIMOD, MULSC, ELOSS
C
      COMMON / CJDRCH / RDEC(4),
     *                  PSIIN(3),
     *                  RINCR(3),
     *                  FIRSTW(3),
     *                  FSENSW(3),
     *                  RDEPTH,
     *                  SWDEPL,
     *                  YSUSPN,
     *                  TIMDEL(6), ZMAX, ZOFFS, ZRESOL, ZNORM,ZAL,ZSCAL,
     *                  DRIDEV,DRICOS,DRISIN
CAV
     + ,AVFOO(611)           
      COMMON / CJIONI / POTBEA, ZAROBE,
     *                  POTTRI, ZAROTR,
     *                  POTIVE, ZAROIV,
     *                  POTRH0, ZAROR0,
     *                  POTJET, ZAROJE,
     *                  POTRH1, ZAROR1,
     *                  POTRH2, ZAROR2,
     *                  POTRH3, ZAROR3,
     *                  POTOVE, ZAROOV,
     *                  POTTOF, ZAROTO,
     *                  POTVES, ZARVES,
     *                  POTZJL, ZAROJL,
     *                  POTZJR, ZAROJR
C
      COMMON / CGEO1  / BKGAUS, RPIP,DRPIP,XRLPIP, RBPC,DRBPC,XRLBPC,
     *                  RITNK,DRITNK,XRLTKI, R0ROH,DR0ROH,XR0ROH,
     *                  R1ROH,DR1ROH,XR1ROH, R2ROH,DR2ROH,XR2ROH,
     *                  R3ROH,DR3ROH,XR3ROH, ROTNK,DROTNK,XRLTKO,
     *                  RTOF,DRTOF,XRTOF, RCOIL, DRCOIL, XRCOIL,
     *                  ZJM,DZJM,XRZJM, ZJP,DZJP,XRZJP,
     *                  ZTKM,DZTKM,XRZTKM, ZTKP,DZTKP,XRZTKP,
     *                  ZBPPL,ZBPMI,ZTOFPL,ZTOFMI,
     *                  XRJETC,
     *                  RLG,ZLGPL,ZLGMI,OUTR2,CTLIMP,CTLIMM,DELFI,
     *                  BLXY,BLZ,BLDEP,ZENDPL,ZENDMI,DEPEND,
     *                  XHOL1,XHOL2,YHOL1,YHOL2,BLFI
CAV     *                  XHOL1,XHOL2,YHOL1,YHOL2
CAV  Same size required
C
      COMMON / CJXDAT / XSLOPE, YSLOPE, XL(3), XH(3), R3P, RD3P,
     *                  S, S2,
     *                  XSL3L, X3L, XSL3H, X3H, YSL3L, Y3L, YSL3H,
     *                  YHWIDT, SINHLF, COSHLF, DRITAN
C
      DATA BINWID / 0.005 /
      DATA PMIN / 0.01 /
C
C---------------------------------- C O D E ----------------------------
C
      HNWOLD = 0
C
C                  CHECK FRAME CONDITIONS AND INITIALIZE ANGLES
C
  1   IPOS = 0
      CALL JRCHCK( X, P, IRING, IPOS, IRETRN, *2000, COSAL,SINAL)
      IRETRN = 4
      SINSUS = SINAL
      COSSUS = COSAL
C
      IF( IRING .NE. 3 ) GO TO 10
          IPOS   = SIGN( 1.2, X(2) )
          SINPHI = SIGN( SINHLF, X(2) )
          COSPHI = COSHLF
          SINSUS = SINAL*COSPHI + COSAL*SINPHI
          COSSUS = COSAL*COSPHI - SINPHI*SINAL
          CALL JROTAT( X, COSPHI, SINPHI )
          CALL JROTAT( P, COSPHI, SINPHI )
 10   NWBASE = ICELL*NWIRES(IRING)
      IF( IRING .LT. 3 ) GO TO 20
          NWBASE = NWBASE*2 + NWIRES(2)*NCELL(2)
          IF( IPOS  .GT. 0 ) NWBASE = NWBASE + NWIRES(3)
 20   IF( IRING .GT. 1 ) NWBASE = NWBASE + NCELL(1)*NWIRES(1)
C
C                  INITIALIZE ENERGY LOSS AND MULTIPLE SCATTERING  -----
C
      XTRACK = 0.
      XLIMIT = (( P(6)*P(6)/P(4)/21.E-3*BINWID)**2*XRJETC)**.333333
      XLIMIT = AMIN1( AMAX1(10.,XLIMIT), 100. )
      ETRACK = 0.
      ENRLOS = .FALSE.
      IF(ELOSS.AND.(P(4)/P(5).LT.3..OR.P(5).LT..001)) ENRLOS=.TRUE.
      ELIMIT = AMIN1( 100., 500.*P(6)*P(6))
C
C                  INITIALIZE FIRST PRIMED COORDINATES   -----
C
      XPROJ = X(1) + X(2)*DRITAN
      NW    = ( XPROJ - FSENSW(IRING) ) / RINCR(IRING) +1.5
      DIREC = SWDEPL
      IF( MOD(NW,2) .EQ. 0 ) DIREC = -DIREC
C
      YDOWN = ( X(3) - ZOFFS ) / ZMAX
      YDOWN = YSUSPN*( YDOWN*YDOWN - 1. )
      SX    = FSENSW(IRING) + (NW-1)*RINCR(IRING) + SINSUS*YDOWN
      SY    = DIREC + COSSUS*YDOWN
C
C               -----    START TRACKING ------
C
      DO  1000  ITERAT = 1,400
C
C
         X1(1)  = X(1)
         X1(2)  = X(2)
         X1(3)  = X(3)
         CALL JSTEP( X, P, DRTOT )
C
         TOTLEN = TOTLEN + DRTOT
         XTRACK = XTRACK + DRTOT
         ETRACK = ETRACK + DRTOT
C
C                      CHECK FRAME CONDITIONS FOR X -----
C
         CALL JRCHCK( X, P, IRING, IPOS, IRETRN, *2000, COSSUS, SINSUS )
C
C
         IF( NW .LT. 1 .OR. NW .GT. NWIRES(IRING) ) GO TO 600
C
             XHELP     = X1(1) - SX
             YHELP     = X1(2) - SY
             X1PRIM(1) = XHELP*DRICOS + YHELP*DRISIN
             X1PRIM(2) = YHELP*DRICOS - XHELP*DRISIN
             XHELP     = X(1) - SX
             YHELP     = X(2) - SY
             X2PRIM(1) = XHELP*DRICOS + YHELP*DRISIN
             X2PRIM(2) = YHELP*DRICOS - XHELP*DRISIN
C
C
C                      FIND DRIFT TIME  IN THIS COORDINATE SYSTEM  -----
C
             ASSIGN 600 TO LABEL
C
 400         CALL JDTIME( X1PRIM, X2PRIM, FOUND, DIST, SLOPE )
             IF( .NOT. FOUND ) GO TO 500
C                              -----    ABSOLUTE WIRE NUMBER
                HNWTOT = NWBASE + NW
C                              -----    DRIFT TIME BINNING
                NRH    = IRING*2
                IF( NW .LE. NWIRES(IRING)/2 ) NRH = NRH - 1
                AITIM  = AMIN1( DIST/TIMDEL(NRH), 32767. )
                HITIM  = AITIM
C                                  -----  Z - AMPLITUDES
                COSANG = SQRT( 1./(1.+SLOPE*SLOPE) +
     *                               (P(1)*P(1)+P(2)*P(2))/(P(6)*P(6)) )
C
                HIAMPL = X(3)+SIGN(0.5,X(3))
                HIAMPR = ZNORM/COSANG+0.5
C
C                            REGISTRATION OF DRIFT TIMES   -----
C
C                                     -----  SAME WIRE NUMBER AS BEFORE
C
                IF( HNWOLD .NE. HNWTOT ) GO TO 460
C
C                                     -----  SMALLER DRIFT TIME
C
                    IF( HLIST(INEXT-1) .LE. HITIM ) GO TO 500
                    GO TO 450
  460           NHITS = NHITS + 1
                INEXT = INEXT + 4
                IF( INEXT .LE. 1601 ) GO TO 450
                   WRITE(6,465)
  465              FORMAT('0MORE THAN 400 JET CHAMBER HITS FOR THIS',
     *                    ' TRACK. EXCESS HITS LOST.')
                   RETURN 1
C
  450           CALL MVC( HLIST, 2*INEXT-10, HDRIFT, 0, 8 )
                HNWOLD = HLIST(INEXT-4)
C
C                     ---- STORE THE TRACK NO. AND BANK NO.
C                     ---- FOR THIS HIT IN H4VHIT
C
                H4VHTR(NHITS) = 2 * IPART + IBANK
C
  500    GO TO LABEL, ( 600, 900 )
C
C
C       -----   CHECK DRIFT TIMES IN PRIMED SYSTEM OF SECOND POINT -----
C
C
C                                    -----  WIRE NUMBER FOR SECOND POINT
 600     NW1   = NW
         XPROJ = X(1) + X(2)*DRITAN
         NW    = ( XPROJ - FSENSW(IRING) ) / RINCR(IRING) + 1.5
         DIREC = SWDEPL
         IF( MOD(NW,2) .EQ. 0 ) DIREC = -DIREC
C
         YDOWN = ( X(3) - ZOFFS ) / ZMAX
         YDOWN = YSUSPN*( YDOWN*YDOWN - 1. )
         SX    = FSENSW(IRING) + (NW-1)*RINCR(IRING)+ SINSUS*YDOWN
         SY    = DIREC + COSSUS*YDOWN
C
C                     ----- END IF SAME NUMBER OR NW OUT OF BOUNDS
C
         IF( NW1.EQ.NW .OR. NW.LT.1 .OR. NW.GT.NWIRES(IRING) ) GO TO 900
C
C
            XHELP     = X1(1) - SX
            YHELP     = X1(2) - SY
            X1PRIM(1) = XHELP*DRICOS + YHELP*DRISIN
            X1PRIM(2) = YHELP*DRICOS - XHELP*DRISIN
            XHELP     = X(1) - SX
            YHELP     = X(2) - SY
            X2PRIM(1) = XHELP*DRICOS + YHELP*DRISIN
            X2PRIM(2) = YHELP*DRICOS - XHELP*DRISIN
C
            ASSIGN  900 TO LABEL
            GO TO 400
C
C
  900    CONTINUE
         IF( TOTLEN .GT. STPLEN ) GO TO 2000
         IF( .NOT. ENRLOS .OR. ETRACK .LT. ELIMIT ) GO TO 930
         CALL JELOSS(P,ETRACK, POTJET, ZAROJE, XRJETC, X, COSSUS,SINSUS)
         ETRACK = 0.
         IF( P(6) .LE. PMIN ) GO TO 2000
 930       IF( .NOT. MULSC .OR. XTRACK .LT. XLIMIT ) GO TO 1000
              CALL JMULSC( P, XTRACK/XRJETC )
              XTRACK = 0.
C
 1000 CONTINUE
C
C
C     ----- ROTATE BACK TO OLD SYSTEM AND REPEAT IF CHANGE IN RING 3 ---
C
 2000 IF( IPOS .EQ. 0 ) GO TO 2100
        CALL JROTAT( X, COSPHI, -SINPHI )
        CALL JROTAT( P, COSPHI, -SINPHI )
        IF( IRETRN .EQ. 6 ) GO TO 1
C
 2100 RETURN
      END
