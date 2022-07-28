C   02/10/78 110071139  MEMBER NAME  JRCHCK0  (S)           FORTRAN
      SUBROUTINE JRCHCK( X, P, IRING, IPOS, IRETRN, *, CA, SA )
C---------------------------------------------------------
C
C  VERSION OF 02/01/79  LAST MOD 07/10/81         E.ELSEN
C  CHECKS FRAME CONDITIONS FOR DRIFT SPACE IN RING IRING.
C  IN RING 3 PARAMETER IPOS INDICATES COORDINATE SYSTEM
C  OF PROPAGATION.
C  PARTICLE IS PROPAGATED THROUGH SEPARATING WALL BETWEEN CELLS
C  IN RING 3.
C  IRETRN IS RETURN CODE:
C   IRETRN=0 : NEXT ELEMENT, INCREASING X
C         =1 : NEXT ELEMENT, DECREASING X
C         =2 : NEXT ELEMENT, INCREASING Z
C         =3 : NEXT ELEMENT, DECREASING Z
C         =4 : PARTICLE STOPPED IN SYSTEM OR DECAY CONDITION
C              REACHED.
C         =5 : NEXT ELEMENT, CHANGING PHI
C         =6 : NEXT ELEMENT, CHANGING PHI TOWARDS THE NEIGHBORING
C              SPACE IN SAME CELL ( ONLY VALID FOR RING THREE ).
C  IF PARTICLE IS IN FRAME RETURN ELSE RETURN 1.
C---------------------------------------------------------
C
      DIMENSION X(3), P(7)
C
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
      COMMON/CGEO1/BKGAUS, RPIP,DRPIP,XRLPIP, RBPC,DRBPC,XRLBPC,
     *             RITNK,DRITNK,XRLTKI, R0ROH,DR0ROH,XR0ROH,
     *             R1ROH,DR1ROH,XR1ROH, R2ROH,DR2ROH,XR2ROH,
     *             R3ROH,DR3ROH,XR3ROH, ROTNK,DROTNK,XRLTKO,
     *             RTOF,DRTOF,XRTOF, RCOIL, DRCOIL, XRCOIL,
     *             ZJM,DZJM,XRZJM, ZJP,DZJP,XRZJP,
     *             ZTKM,DZTKM,XRZTKM, ZTKP,DZTKP,XRZTKP,
     *             ZBPPL,ZBPMI,ZTOFPL,ZTOFMI,
     *             XRJETC,
     *             RLG,ZLGPL,ZLGMI,OUTR2,CTLIMP,CTLIMM,DELFI,
     *             BLXY,BLZ,BLDEP,ZENDPL,ZENDMI,DEPEND,
     *             XHOL1,XHOL2,YHOL1,YHOL2,BLFI
CAV     *             XHOL1,XHOL2,YHOL1,YHOL2
CAV Same size required in the line above
C
      COMMON / CJXDAT / XSLOPE, YSLOPE, XL(3), XH(3), R3P, RD3P,
     *                  S, S2,
     *                  XSL3L, X3L, XSL3H, X3H, YSL3L, Y3L, YSL3H,
     *                  YHWIDT, SINHLF, COSHLF, DRITAN
C
      DATA EPSIL0, EPSIL1 / 1.E-3 , 5.E-3 /
C
C
C
C
C    -----    CHECK FRAME CONDITIONS    -----
C
   10 IF( IRING .EQ. 3 ) GO TO 110
C
      IF( X(1) .LE. XH(IRING) + EPSIL0 ) GO TO 20
                          IRETRN = 0
                          RETURN 1
   20 IF( X(1) .GE. XL(IRING) - EPSIL0 ) GO TO 30
                          IRETRN = 1
                          RETURN 1
   30 IF( ABS(X(2)) .LE. YSLOPE*X(1)+EPSIL1 ) GO TO 40
                          IRETRN = 5
                          RETURN 1
C
C
  110 IF( IPOS .NE. 0 )  GO TO 210
      IF( X(1) .LE. XSLOPE*ABS(X(2))+R3P + EPSIL0 ) GO TO 120
                          IRETRN = 0
                          RETURN 1
  120 IF( X(1) .GE. XL(3) - EPSIL0 ) GO TO 130
                          IRETRN = 1
                          RETURN 1
  130 IF( ABS(X(2)) .GE. YHWIDT - EPSIL1 ) GO TO 30
            CALL JTRAPZ( 3, X, P, XL(3), R3P, ZJM, ZJP,
     *                   POTRH3, ZAROR3, DR3ROH/XR3ROH, IRETRN,
     *                   CA, SA )
                 IF( IRETRN .EQ. 5 ) RETURN
                 RETURN 1
C
C  PART FOR IPOS .NE. 0
C
  210 IF( X(1) .LE. XSL3H*ABS(X(2)) + X3H + EPSIL0 ) GO TO 220
                               IRETRN = 0
                               RETURN 1
  220 IF( X(1) .GE. XSL3L*ABS(X(2)) + X3L - EPSIL0 ) GO TO 230
                               IRETRN = 1
                               RETURN 1
C
C       -----  RING 3 IPOS NE 0
  230 IF( IPOS .EQ. -1 ) GO TO 250
      IF( X(2) .LE. YSL3H*X(1) + EPSIL1  ) GO TO 240
                               IRETRN = 5
                               RETURN 1
  240 IF( X(2) .GE. YSL3L*X(1) + Y3L - EPSIL0 ) GO TO 40
                               IRETRN = 6
                               RETURN 1
C
C
  250 IF( X(2) .GE. -YSL3H*X(1) - EPSIL1  ) GO TO 260
                               IRETRN = 5
                               RETURN 1
  260 IF( X(2) .LE. -YSL3L*X(1) - Y3L + EPSIL0 ) GO TO 40
                               IRETRN = 6
                               RETURN 1
C
C
C      -----  PART FOR TESTS IN + - Z
   40 IF( X(3) .LE. ZJP ) GO TO 50
                          IRETRN = 2
                          RETURN 1
   50 IF( X(3) .GE. ZJM ) RETURN
                          IRETRN = 3
                          RETURN 1
      END
