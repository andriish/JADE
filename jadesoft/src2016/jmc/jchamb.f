C   29/09/77 606101226  MEMBER NAME  JCHAMB9  (S)           FORTRAN
C
C-----------------------------------------------------------------------
      SUBROUTINE JCHAMB( R, P, PENETR, PENETZ, IRETRN, DRMAX, * )
C-----------------------------------------------------------------------
C
C   AUTHOR:   E. ELSEN    05/01/78 :  PROPAGATES PARTICLE IN JET CHAMBER
C                                     THROUGH DIFFERENT LAYERS
C        MOD  E. ELSEN    02/10/81 :
C   LAST MOD  J. HAGEMANN 21/09/84 :  CHANGES DUE TO MODIFICATION OF
C             R. RAMCKE               SUBROUTINES JRING, JTRAPZ
C
C    ENERGY LOSS AND MULTIPLE SCATTERING ARE CONSIDERED ACCORDING
C    TO SWITCHES ELOSS AND MULSC
C    ROUTINE USES ROTATED COORDINATE SYSTEM
C    IRETRN IS RETRUN CODE:
C        IRETRN = 0 : NEXT ELEMENT, INCREASING X
C               = 1 : NEXT ELEMENT, DECREASING X
C               = 2 : NEXT ELEMENT, INCREASING Z
C               = 3 : NEXT ELEMENT, DECREASING Z
C               = 4 : PARTICLE STOPPED IN SYSTEM OR DECAY CONDITION
C                     REACHED.
C    RETURN 1 IF OVERFLOW IN EVENT REGISTER
C-----------------------------------------------------------------------
C
      COMMON / CJCELL / NCELL(3),
     *                  NWIRES(3)
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
CAV  Same size required
      COMMON / CJXDAT / XSLOPE, YSLOPE, XL(3), XH(3), R3P, RD3P,
     *                  S, S2,
     *                  XSL3L, X3L, XSL3H, X3H, YSL3L, Y3L, YSL3H,
     *                  YHWIDT, SINHLF, COSHLF, DRITAN
C
      DIMENSION R(5), P(7), DR(3)
      DIMENSION IERR(7)
C
      DATA TWOPI / 6.283185 /
*** PMF  20/06/00 Loop counter provided by subroutines TRCDTV, TRCDET
      INTEGER ILOOP1,ILOOP2
      COMMON/CLOOP/ ILOOP1,ILOOP2
*** PMF (end)
C
C------------------  C O D E  ------------------------------------------
C
C      -----ZERO CONTROL ARRAY IERR(7)
                   DO 881 I6=1,7
  881              IERR(I6) = 0
C      -----       END  OF CONTROL SECTION
         DPSI = TWOPI / FLOAT( NCELL(1) )
         PENETR = 0.
         PENETZ = 0.
C
C
C     ----   CHECK CHAMBER FRAME AND CHANGE TO ROTATED SYSTEM -----
C
   10 IF( R(3) .GE. ZJM ) GO TO 20
                    IRETRN = 3
                    RETURN
   20 IF( R(3) .LE. ZJP ) GO TO 30
                    IRETRN = 2
                    RETURN
   30 R(5) = ATAN2( R(2), R(1) )
                IF( R(5) .LT. PSIIN(1) ) R(5) = R(5) + TWOPI
                ICELL = ( R(5) - PSIIN(1) ) / DPSI + .5
                ICELL = MOD( ICELL, NCELL(1) )
                PHI = FLOAT( ICELL )* DPSI + PSIIN(1)
                COSPHI = COS( PHI )
                SINPHI = SIN( PHI )
                CALL JROTAT( R, COSPHI, SINPHI )
                CALL JROTAT( P, COSPHI, SINPHI )
C
      IF( R(1) .GE. R0ROH ) GO TO 50
*** PMF 20/06/00
*   Check the loop counter provided by TRCDET, TRCDTV
*   in order to detect infinite loops which sometimes happen 
*   for decreasing R (IRETRN = 1). This is due to rounding errors
*   in the calculation of the particle position here and in
*   subroutine JPRTLO.
      IF ( ILOOP1 .GT. 99 ) THEN
       IF ( ABS(R(1)-R0ROH)/R0ROH .LT. 1.E-5 ) THEN
       R(1)=R0ROH
       PRINT *,'*** JCHAMB: Infinite loop detected!!!(PMF 02/01/01)'
       PRINT *,'*** IRETRN = 1 case ignored after ',iloop1,' loops!!!'
       ILOOP1=0
       GOTO 50
       ELSE
       PRINT *,'*** JCHAMB: Bad infinite loop detected!!!(PMF 20/06/00)'
       PRINT *,'*** Will stop now!!!'
       STOP
       ENDIF
      ENDIF
*** PMF(end)
                          IRETRN = 1
                          GO TO 1000
C  50    XXX = XSLOPE*ABS(R(2))+RD3P
   50 IF( R(1) .LE. XSLOPE*ABS( R(2) ) + RD3P ) GO TO 100
                          IRETRN = 0
                          GO TO 1000
C
C
C    -----   START TRACKING THROUGH DIFFERENT LAYERS     -----
C
C    -----     FIRST ROHACELL
  100 CALL JTRAPZ( 1, R, P, R0ROH, R0ROH+DR0ROH, ZJM, ZJP,
     *                   POTRH0, ZAROR0, DR0ROH/XR0ROH, IRETRN,
     *                   COSPHI, SINPHI, DRMAX )
           IF( IRETRN .NE. 0 ) GO TO 1000
C
C    -----     FIRST RING
  200 CALL JRING( 1, ICELL, COSPHI, SINPHI, R, P, IRETRN, DRMAX, *2000 )
           GO TO ( 100, 1000, 1000, 1000, 1000 ), IRETRN
C
C    -----     SECOND ROHACELL
  300 CALL JTRAPZ( 1, R, P, R1ROH, R1ROH+DR1ROH, ZJM, ZJP,
     *                   POTRH1, ZAROR1, DR1ROH/XR1ROH, IRETRN,
     *                   COSPHI, SINPHI, DRMAX )
           GO TO ( 200, 1000, 1000, 1000, 1000 ), IRETRN
C
C    -----     SECOND RING
  400 CALL JRING( 2, ICELL, COSPHI, SINPHI, R, P, IRETRN, DRMAX, *2000 )
           GO TO ( 300, 1000, 1000, 1000, 1000 ), IRETRN
C
C    -----     THIRD ROHACELL
  500 CALL JTRAPZ( 1, R, P, R2ROH, R2ROH+DR2ROH, ZJM, ZJP,
     *                   POTRH2, ZAROR2, DR2ROH/XR2ROH, IRETRN,
     *                   COSPHI, SINPHI, DRMAX )
           GO TO ( 400, 1000, 1000, 1000, 1000 ), IRETRN
C
C    -----     THIRD RING
  600 CALL JRING( 3, ICELL, COSPHI, SINPHI, R, P, IRETRN, DRMAX, *2000 )
           GO TO ( 500, 1000, 1000, 1000, 1000 ), IRETRN
C
C    -----     FOURTH ROHACELL
  700 CALL JTRAPZ( 2, R, P, R3P, RD3P, ZJM, ZJP,
     *                   POTRH3, ZAROR3, DR3ROH/XR3ROH, IRETRN,
     *                   COSPHI, SINPHI , DRMAX )
           IF( IRETRN .EQ. 1 ) GO TO 600
C
C
C    ----   TRANSFORM INTO OLD COORDINATE SYSTEM    -----
 1000                   CALL JROTAT( R, COSPHI, -SINPHI )
                        CALL JROTAT( P, COSPHI, -SINPHI )
C
C      -----UPDATE AND CHECK CONTROL ARRAY IERR(7)
                   IERR(IRETRN+1) = IERR(IRETRN+1) + 1
                   IF( IERR(IRETRN+1) .LT. 30 ) GO TO 884
C 883  WRITE(6,9110) IERR
C9110  FORMAT(//'   *******   FORCED RETURN FROM JCHAMB *****'/
C    *          '             FAKING STOPPING PARTICLE'/
C    *          '             CONTROL ARRAY IERR (0-6)',7(2X,I4)//)
                   IRETRN = 4
                   RETURN
  884              CONTINUE
C      -----       END  OF CONTROL SECTION
C                                     IF IRETRN.EQ. 5 RESTART
C                                     TRACKING IN NEXT CELL
      IF( IRETRN .EQ. 5 ) GO TO 10
C
        R(4) = SQRT( R(1)*R(1) + R(2)*R(2) )
C    *          ' ---------------------------------------------------')
        IF( IRETRN .EQ. 2 ) PENETZ = R(3) - ZJP
        IF( IRETRN .EQ. 3 ) PENETZ = ZJM - R(3)
        RETURN
C
C   ----    ERROR EXIT FOR OVERFLOW IN HIT REGISTER   -----
 2000  RETURN 1
       END
      SUBROUTINE JROTAT( X, COSPHI, SINPHI )
C  *--------------------------------------------------------
C  *
C  *  VERSION OF 26/09/78
C  *  ROTATES THREE VECTOR X IN X Y PLANE.
C  *  COSPHI AND SINPHI DEFINE ROTATING ANGLE
C  *--------------------------------------------------------
C
      DIMENSION X(2)
C
      XX = X(1)
      X(1) = X(1)*COSPHI + X(2)*SINPHI
      X(2) = X(2)*COSPHI - XX*SINPHI
      RETURN
      END
