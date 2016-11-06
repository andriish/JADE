C   17/05/79 606091757  MEMBER NAME  TRCDTV   (S)           FORTRAN
C
C-----------------------------------------------------------------------
      SUBROUTINE TRCDTV( P, R0, RR, * )
C-----------------------------------------------------------------------
C
C   AUTHOR:   J. HAGEMANN  03/11/82 :  MODIFICATION OF SUBROUTINE
C             R. RAMCKE                TRCDET : BEAM PIPE COUNTERS
C                                      HAVE BEEN REMOVED FOR
C                                      INSTALLATION OF VERTEX CHAMBER
C
C        MOD  J. HAGEMANN  21/09/84 :  CHANGES DUE TO NEW VERSION OF
C                                      SUBROUTINE JSTEP
C   LAST MOD  J. HAGEMANN  17/04/86 :  CHANGES DUE TO NEW VERSION OF
C                                      TRCDET(ELS) FROM K-H.H. (K DECAY)
C
C        PROPAGATES PARTICLE FROM STARTING POINT THROUGH  DETECTOR  UP
C        TO COIL IN R OR LEAD GLASS IN Z.
C
C        R AND P WILL BE CHANGED AND INDICATE LAST  VALUES  IN  PLACE.
C        MULTIPLE SCATTERING IS INCLUDED IF MULSC  .EQ.  TRUE.  ENERGY
C        LOSS IS INCLUDED IF ELOSS .EQ. TRUE.
C
C        IF  ELOSS.EQ..FALSE.  PARTICLE  IS  PROPAGATED  TRMAX  MM  AT
C        MAXIMUM ( SEE DATA STATEMENT ).
C        MOMENTUM CUT IS AT 10. MEV/C.
C        IF PARTICLE STOPS IN SYSTEM, P = 0.
C        IF PARTICLE LEAVES UNDER A SMALL ANGLE IT IS NOT  PROPAGATED.
C
C        RETURN1, IF OVERFLOW IN EVENT REGISTER HITAR.
C
C        P(1)..P(4)     4-VECTOR OF PARTICLE TO BE TRACKED
C        P(5)           MASS IN GEV
C        P(6)           ABS VALUE OF 3 MOMENTUM
C        P(7)           CHARGE
C        P(8)           TYPE OF PARTICLE
C        P(9)..P(10)    FREE ( WILL CONTAIN TRACKING INFORMATION )
C
C        R0(1)..R0(3)   3_VECTOR FOR STARTING POINT OF PARTICLE
C
C        TRACK INFORMATION IS STORED IN CJPATR, KPATR  INDICATING  THE
C        TOTAL LENGTH OF ARRAY APATR. ( JADE COMP.NOTE #12 )
C        OVERFLOW CAUSES OVERWRITING OF LAST TRACK RECORD.
C        TYPE OF STARTING POINT ( APATR(KPATR+4) )  MUST  BE  SUPPLIED
C        FROM CALLING ROUTINE.
C        TRCOFS INTRODUCED IN COMMON /CJTRLE/
C
C        MAXIMUM STEP LENGTH IN EACH DETECTOR PART NOW SET IN DATA
C        STATEMENTS (DRMX1, DRMX2, DRMX3).
C
C----------------------------------------------------------------------
C
      IMPLICIT INTEGER*2 (H)
C
      LOGICAL  ELOSS, MULSC
C
      COMMON / CWORK  / NHITS, INEXT, HLIST(1600)
      COMMON / CWSET  / NHALL, ISTART, HPCELL(98), HITAR(16000)
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
     *                  XHOL1,XHOL2,YHOL1,YHOL2

      COMMON / CJTRLE / TOTLEN, STPLEN, TRCOFS
      COMMON / CJSWLO / ITIMOD, MULSC, ELOSS
C
      COMMON / CJVTXC / RVEC, ANG1, ANG2, DISTPW, FIRSTP, DISTW1,
     *                  ANGL, COSLOR, SINLOR,
     *                  ZRESV, ZMAXV, ZOFFV, ZNAMP, ZALV, TIMEV
      COMMON / CIJONV / POTVXC, ZAROVC,
     *                  POTVGA, ZAROVG
      COMMON / CGEOV  / RPIPV, DRPIPV, XRLPIV, RVXC, DRVXC, XRLVXC,
     *                  ZVXCM, DZVCM, XRZVCM, ZVXCP, DZVCP, XRZVCP,
     *                  XRVTXC
      COMMON / CSTORE / MHITS, IPV, HSTORE(2000)
C
      DIMENSION  PV2(10), P(10), R0(3),R(5),RR(3)
C
      DATA TRMAX / 5000. /
      DATA PCUT / .01 /
C
      DATA DRMX1  /  5.0  /
      DATA DRMX2  /  3.0  /
      DATA DRMX3  /  1.5  /
*** PMF  06/02/00 Loop counter to detect infinite loops.
*   Infinite loops sometimes happen (very rarely) if the particle
*   is located nearly exactly at the border of the drift chamber frame
*   given by subroutine JCHAMB.
*   Common /CLOOP/ is used in subroutines JPRTHI, JCHAMB.
      INTEGER ILOOP1,ILOOP2
      COMMON/CLOOP/ ILOOP1,ILOOP2
*** PMF (end)
C
C--------------------  C O D E  ----------------------------------------
C
*** PMF 06/02/00 initialize loop counter
      ILOOP1=0
      ILOOP2=0
*** PMF (end)
      R(1) = R0(1)
      R(2) = R0(2)
      R(3) = R0(3)
      R(4) = SQRT( R(1)*R(1) + R(2)*R(2) )
C
CCC  SET DUMMY VALUE FOR TRACK POINT, INITIALIZE HLIST ARRAY POINTERS
C
      IPOTYP = 0
   10 NHITS  = 0
      INEXT  = 1
C
      PENETR = 0.
      PENETZ = 0.
      TOTLEN = 0.
      STPLEN = 1.E19   !1.E40
C
C   DON'T TRACK PARTICLES WITH P<PCUT
C   DON'T TRACK PARTICLES WHICH START BEHIND COIL OR ENDCAP LG.
C                                       OCT.25. 1983 W.BARTEL
C
      IF( P(6) .LT. PCUT ) GO TO 9100
      IF( R(4) .GT. RCOIL) GO TO 9000
      IF( ABS(R(3)) .GT. ZENDPL) GO TO 9000
C
C                           DEFINE PI MU DECAY PARAMETERS
      CALL PIKMUF( P, PV2, STPLEN )
      IF( .NOT. ELOSS ) STPLEN = AMIN1( TRMAX, STPLEN )
C
C
C                            INSIDE BEAM PIPE
  100 CALL JPRFRE( R, P, PENETR, PENETZ,  0.  , RPIPV - 2*DRPIPV,
     *                     ZENDMI, ZENDPL, DRMX1,
     *                     *9100, *9200, *9200, *9100 )
C
C                            INSIDE BEAM PIPE (REDUCED STEP LENGTH)
  150 CALL JPRFRE( R, P, PENETR, PENETZ, RPIPV - 2*DRPIPV, RPIPV,
     *                     ZENDMI, ZENDPL, DRMX3,
     *                      *100, *9200, *9200, *9100 )
C
C                            BEAM PIPE WALL
  200 CALL JPRABS( R, P, PENETR, PENETZ, RPIPV, RPIPV + DRPIPV,
     *             ZENDMI, ZENDPL, POTBEA, ZAROBE, DRPIPV/XRLPIV,
     *             DRMX3,  *150, *9200, *9200, *9100 )
C
C                            INSIDE VERTEX CHAMBER
  250 CALL JVTXCH( R, P, PENETR, PENETZ, JRETRN, RPIPV + DRPIPV, RVXC,
     *             ZVXCM, ZVXCP, DRMX2, *9350 )
                 GOTO( 200, 450, 350, 9100 ), JRETRN
C
C                            OUTER VERTEX CHAMB. WALL
  500 CALL JPRABS( R, P, PENETR, PENETZ, RVXC, RVXC + DRVXC,
     *       ZVXCM + DZVCM, ZVXCP + DZVCP, POTVXC, ZAROVC, DRVXC/XRLVXC,
     *             DRMX3,  *250, *470, *370, *9100 )
C
C                            BETWEEN VERTEX CHAMB. AND INNER VESSEL WALL
  550 CALL JPRFRE( R, P, PENETR, PENETZ, RVXC + DRVXC, RITNK,
     *                     ZVXCM + DZVCM, ZVXCP + DZVCP, DRMX2,
     *                      *500, *470, *370, *9100 )
C
C                            INNER VESSEL WALL
  600 CALL JPRABS( R, P, PENETR, PENETZ, RITNK , RITNK + DRITNK,
     *            ZTKM+DZTKM, ZTKP+DZTKP, POTIVE, ZAROIV, DRITNK/XRLTKI,
     *            DRMX1,  *550, *8300, *7300, *9100 )
C
*** PMF 20/06/2000
*     Loop counter to detect infinite loops when returning 
*     from subroutine JCHAMB for decreasing R.
      GOTO 700
  701 ILOOP1=ILOOP1+1
*** PMF (end)
C
C                           BETWEEN INNER VESSEL WALL AND FIRST ROHACELL
  700 CALL JPRTLO( R, P, PENETR, PENETZ, RITNK  + DRITNK , R0ROH,
     *                     ZJM, ZJP, DRMX1,
     *                      *600, *8700, *7700, *9100 )
C
*** PMF 06/02/00:
*     Loop counter to detect infinite loops when returning
*     from subroutine JPRTHI for decreasing R.
      GOTO 2000
 2001 ILOOP2=ILOOP2+1
*** PMF (end)
CCC  TENTATIVE FILLING OF CJPATR COMMON
 2000 CALL JIPATR( P, R )
C
CCC  TRACK PARTICLES THRU JETCHAMBER
      CALL JCHAMB( R, P, PENETR, PENETZ, IRETRN, DRMX1, *9400 )
C
CCC  UPDATE CJPATR COMMON
      CALL JUPATR( P, R )
C
CCC  BRANCH ACCORDIING TO RETURN CODE FROM JCHAMB
      GO TO ( 701, 8000, 7000, 9100 ) , IRETRN !PMF 20/06/00: Label 700 changed to label 701
C
C
C                            BETWEEN LAST ROHACELL AND OUTER VESSEL WALL
 3100 CALL JPRTHI( R, P, PENETR, PENETZ, R3ROH  + DR3ROH , ROTNK,
     *                     ZJM, ZJP, DRMX1,
     *                     *2001, *8600, *7600, *9100 ) ! PMF 06/02/00: *2000 changed to *2001
C
C                            IN OUTER VESSEL WALL
 3200 CALL JPRABS( R, P, PENETR, PENETZ, ROTNK , ROTNK + DROTNK,
     *             ZTKM+DZTKM,ZTKP+DZTKP, POTOVE, ZAROOV, DROTNK/XRLTKO,
     *             DRMX1,  *3100, *8300, *7300, *9100 )
C
C                            BETWEEN OUTER VESSEL WALL AND TOF COUNTER
 3300 CALL JPRFRE( R, P, PENETR, PENETZ, ROTNK  + DROTNK , RTOF ,
     *                     ZENDMI, ZENDPL, DRMX1,
     *                     *3200, *9000, *9000, *9100 )
C                            IN TOF COUNTER
 3400 CALL JPRTOF( R, P, PENETR, PENETZ, RTOF  , RTOF  + DRTOF ,
     *             ZTOFMI, ZTOFPL, POTTOF, ZAROTO, DRTOF/XRTOF ,
     *             DRMX1,  *3300, *8400, *7400, *9100 )
C                            BETWEEN TOF COUNTER AND COIL
 3500 CALL JPRFRE( R, P, PENETR, PENETZ, RTOF   + DRTOF  , RCOIL,
     *                     ZENDMI, ZENDPL, DRMX1,
     *                     *3400, *9000, *9000, *9100 )
      GO TO 9000
C
C
C                      - Z - PART
C
C                           IN END PLATE OF VERTEX CHAMBER
  350 CALL JPRABS( R, P, PENETR, PENETZ, RPIPV + DRPIPV, RVXC + DRVXC,
     *             ZVXCM + DZVCM, ZVXCM, POTVXC, ZAROVC, -DZVCM/XRZVCM,
     *             DRMX1,  *200, *250, *370, *9100 )
            GO TO 550
C                 BETWEEN END PLATE OF VERTEX CHAMB. AND OUTSIDE
  370 CALL JPRFRE( R, P, PENETR, PENETZ, RPIPV + DRPIPV, RITNK,
     *             ZENDMI, ZVXCM + DZVCM, DRMX1,
     *                     *200, *350, *9200, *9100 )
            GO TO 600
C                           IN END PLATE OF INNER DETECTOR
 7000 CALL JPRABS( R, P, PENETR, PENETZ, R0ROH , R3ROH + DR3ROH,
     *             ZJM+DZJM, ZJM, POTZJL, ZAROJL, -DZJM/XRZJM,
     *             DRMX1,  *7700, *2000, *7100, *9100 )
            GO TO 7600
C                  BETWEEN INNER DETECTOR END PLATE AND VESSEL END PLATE
 7100 CALL JPRFRE( R, P, PENETR, PENETZ, RITNK + DRITNK, ROTNK,
     *               ZTKM, ZJM + DZJM, DRMX1,
     *                      *600, *7000, *7200, *9100 )
            GO TO 3200
C                           IN VESSEL END PLATE
 7200 CALL JPRABS( R, P, PENETR, PENETZ, RITNK + DRITNK , ROTNK,
     *             ZTKM +DZTKM, ZTKM, POTVES, ZARVES, -DZTKM/XRZTKM,
     *             DRMX1,  *600, *7100, *7300, *9100 )
            GO TO 3200
C                           BETWEEN VESSEL END PLATE AND LEAD GLASS
 7300 CALL JPRFRE( R, P, PENETR, PENETZ, RITNK  , ROTNK + DROTNK,
     *               ZENDMI, ZTKM + DZTKM, DRMX1,
     *                      *370, *7200, *9000, *9100 )
            GO TO 3300
C                           BETWEEN TOF COUNTER  AND LEAD GLASS
 7400 CALL JPRFRE( R, P, PENETR, PENETZ, RTOF   , RTOF + DRTOF,
     *                      ZENDMI, ZTOFMI, DRMX1,
     *                     *3300, *3400, *9000, *9100 )
            GO TO 3500
C                           BETWEEN END PLAT OF I DETC. AND OUTER TNK W
 7600 CALL JPRFRE( R, P, PENETR, PENETZ, R3ROH + DR3ROH, ROTNK,
     *                        ZJM + DZJM, ZJM, DRMX1,
     *                      *7000, *3100,  *7100, *9100 )
            GO TO  3200
C                           BETWEEN INNER TNK W AND END PLATE OF I DETC.
 7700 CALL JPRFRE( R, P, PENETR, PENETZ, RITNK + DRITNK, R0ROH,
     *                        ZJM + DZJM, ZJM, DRMX1,
     *                      *600, *700,  *7100, *9100 )
            GO TO  7000
C
C
C                      + Z - PART
C
C                           IN END PLATE OF VERTEX CHAMBER
  450 CALL JPRABS( R, P, PENETR, PENETZ, RPIPV + DRPIPV, RVXC + DRVXC,
     *             ZVXCP, ZVXCP + DZVCP, POTVXC, ZAROVC, DZVCP/XRZVCP,
     *             DRMX1,  *200, *470, *250, *9100 )
            GO TO 550
C                 BETWEEN END PLATE OF VERTEX CHAMB. AND OUTSIDE
  470 CALL JPRFRE( R, P, PENETR, PENETZ, RPIPV + DRPIPV, RITNK,
     *            ZVXCP + DZVCP, ZENDPL, DRMX1,
     *                     *200, *9200, *450, *9100 )
            GO TO 600
C                           IN END PLATE OF INNER DETECTOR ( + Z )
 8000 CALL JPRABS( R, P, PENETR, PENETZ, R0ROH , R3ROH + DR3ROH,
     *             ZJP, ZJP+DZJP, POTZJR, ZAROJR, DZJP/XRZJP,
     *             DRMX1,  *8700, *8100, *2000, *9100 )
            GO TO 8600
C                  BETWEEN INNER DETECTOR END PLATE AND VESSEL END PLATE
 8100 CALL JPRFRE( R, P, PENETR, PENETZ, RITNK + DRITNK, ROTNK,
     *               ZJP + DZJP, ZTKP, DRMX1,
     *                      *600, *8200, *8000, *9100 )
            GO TO 3200
C                           IN VESSEL END PLATE
 8200 CALL JPRABS( R, P, PENETR, PENETZ, RITNK + DRITNK , ROTNK,
     *             ZTKP, ZTKP +DZTKP, POTVES, ZARVES, DZTKP/XRZTKP,
     *             DRMX1,  *600, *8300, *8100, *9100 )
            GO TO 3200
C                           BETWEEN VESSEL END PLATE AND LEAD GLASS
 8300 CALL JPRFRE( R, P, PENETR, PENETZ, RITNK  , ROTNK + DROTNK,
     *               ZTKP + DZTKP, ZENDPL, DRMX1,
     *                      *470, *9000, *8200, *9100 )
            GO TO 3300
C                           BETWEEN TOF COUNTER  AND LEAD GLASS
 8400 CALL JPRFRE( R, P, PENETR, PENETZ, RTOF   , RTOF + DRTOF,
     *                      ZTOFPL, ZENDPL, DRMX1,
     *                     *3300, *9000, *3400, *9100 )
            GO TO 3500
C                           BETWEEN END PLAT OF I DETC. AND OUTER TNK W
 8600 CALL JPRFRE( R, P, PENETR, PENETZ, R3ROH + DR3ROH, ROTNK,
     *                        ZJP, ZJP + DZJP, DRMX1,
     *                      *8000, *8100,  *3100, *9100 )
            GO TO  3200
C                           BETWEEN INNER TNK W AND END PLATE OF I DETC.
 8700 CALL JPRFRE( R, P, PENETR, PENETZ, RITNK + DRITNK, R0ROH,
     *                        ZJP, ZJP + DZJP, DRMX1,
     *                      *600, *8100,  *700, *9100 )
            GO TO  8000
C
C
C
C
 9100 CONTINUE
C                          DECAY OR STOPPING PARTICLE
             IF( STPLEN .GT. TOTLEN ) GO TO 9130
                 IF( .NOT. ELOSS .AND. STPLEN.EQ.TRMAX ) GO TO 9130
                 IF( P(6) .LE. PCUT ) GO TO 9130
             STOPL=STPLEN
             IF(ABS(P(8)-5.).LT.1.E-3) CALL TRKADC(P,R,R0,STOPL,*9120)
                    CALL PIKDEC( P, PV2, STPLEN )
                    TRCOFS = STPLEN + SQRT(R0(1)**2 + R0(2)**2
     *                              + R0(3)**2)
                    CALL SVECT1( PV2, R )
C                                           FINISH THIS PARTICLE
 9120               P(1) = 0.
                    P(2) = 0.
                    P(3) = 0.
                    P(6) = 0.
                    P(4) = P(5)
                    CALL JAPATR( IPOTYP )
                    GO TO 9030
C
C                          PARTICLE STOPPED IN  SYSTEM
 9130               P(1) = 0.
                    P(2) = 0.
                    P(3) = 0.
                    P(6) = 0.
                    P(4) = P(5)
C
C
C                                           FILL CELL INFORMATION
C                                           INTO TRACK ARRAY
 9000 CALL JAPATR( IPOTYP )
C                                           ORDER DRIFT TIMES
C                                           AND INSERT INTO HITAR
 9030 CALL JHITIN( *9400)
C
 9200 CONTINUE
      RR(1) = R(1)
      RR(2) = R(2)
      RR(3) = R(3)
      RETURN
C
C                                           OVERFLOW IN HIT REGISTER
C
 9350 WRITE( 6, 9351 )
 9351 FORMAT(' 0TRCDET: OVERFLOW IN VERTEX CHAMBER HIT REGISTER')
 9400 RETURN 1
      END
