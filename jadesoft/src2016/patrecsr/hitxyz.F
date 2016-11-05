C   24/10/80 110141330  MEMBER NAME  HITXYZ   (PATRECSR)    FORTRAN
          SUBROUTINE HITXYZ(IPHIT,LRSOLN,PHITRK,GRADZR,ZRINT,RESULT)
          IMPLICIT INTEGER*2(H)
          REAL*4 RESULT(14)
          LOGICAL TBIT
C
C
C-----------------------------------------------------------------------
C
C    ----------------------  SUBROUTINE HITXYZ -------------------------
C    ---------- G.F.PEARCE .. LAST UPDATE : 1600 ON 24/10/80 -----------
C    ------------ LATEST ABERATIONS BY T.NOZAKI ON 19/9/80 -------------
C
C     SUBROUTINE TO CALCULATE COORDINATES IN REAL SPACE OF ONE HIT
C     IN THE JADE JET CHAMBERS.
C
C     ALL INPUT/OUTPUT IS TRANSMITTED IN THE CALL PARAMETER LIST.
C     THIS CALL LIST IS DESCRIBED BELOW :
C
C     REQUIRED AS INPUT
C     -----------------
C
C 1.  IPHIT  = POINTER TO HIT IN 'JETC'
C
C 2.  LRSOLN = LEFT/RIGHT SOLUTION (-VE FOR LEFT, +VE FOR RIGHT)
C
C 3.  PHITRK = AZIMUTHAL ANGLE OF THE TRACK (IN R-PHI SPACE) AT THIS HIT
C
C 4.  GRADZR = GRADIENT  FROM THE TRACK FIT IN THE Z-R PLANE.
C
C 5.  ZRINT  = INTERCEPT FROM THE TRACK FIT IN THE Z-R PLANE.
C
C     RESULT ARRAY (OUTPUT FROM HITXYZ)
C     ---------------------------------
C
C (1).  LAYER STRUCK BY HIT (0-15)
C
C (2).  POINTER TO HIT IN 'JETC'
C
C (3).  POINTER TO HIT LABEL ARRAY   (DUMMY .. NOT SET BY HITXYZ)
C
C (4).  COMPUTED X COORDINATE
C
C (5).  COMPUTED Y COORDINATE
C
C (6).  COMPUTED Z COORDINATE
C
C (7).  COMPUTED R COORDINATE
C
C (8).  Z-R ERROR FLAG    (0 => GOOD HIT, 10 => BAD HIT)
C
C (9).  LEFT/RIGHT SOLUTION (-VE FOR LEFT, +VE FOR RIGHT)
C
C (10). CELL NUMBER
C
C (11). X-Y ERROR FLAG    (0 => GOOD HIT, 2 => BAD HIT)
C
C (12). SEC(GAMMA)        WHERE GAMMA IS THE ANGLE OF THE TRACK
C                         TO THE NORMAL TO THE DRIFT SPACE DIRN
C
C (13). RING NUMBER
C
C (14). CHI OF POINT TO FIT IN X-Y    (DUMMY .. NOT SET BY HITXYZ)
C
C-----------------------------------------------------------------------
C
C
          REAL*4 XYZHIT(14)
          LOGICAL*1 CALLED/.FALSE./
#include "cdata.for"
      COMMON / CJDRCH / RDEC(4),PSIIN(3),RINCR(3),FIRSTW(3),FSENSW(3),
     * RDEPTH,SWDEPL,YSUSPN,TIMDEL(2,3),ZMAX,ZOFFS,ZRESOL,ZNORM,ZAL,
     * ZSCAL,DRIDEV,DRICOS,DRISIN,PEDES,TZERO(3),
     * DRIROT(96,2),SINDRI(96,2),COSDRI(96,2),DRIVEL(96,2),T0FIX(3),
     * CABER1,CABER2,CABER3,CABER4,CABER5,CABER6,CABER7,CABER8
#include "cdsmax.for"
#include "cpatlm.for"
      COMMON/CALIBR/JPOINT(100),
     1HMUCAL(8370),HLGMAI(5760),HTAGSY(384),HJPULS(9216),
     1DELTA0(96,2),DELTA1(96,2),DELTA2(96,2),DELTA5(96,2),DELTA6(96,2)
C
C         ------------------------
C         EQUIVALENCE RESULT ARRAY
C         ------------------------
C
          EQUIVALENCE (XYZHIT( 1),LAYER)
          EQUIVALENCE (XYZHIT( 2),NH1)
          EQUIVALENCE (XYZHIT( 3),NH)
          EQUIVALENCE (XYZHIT( 4),XHIT1)
          EQUIVALENCE (XYZHIT( 5),YHIT1)
          EQUIVALENCE (XYZHIT( 6),ZHIT1)
          EQUIVALENCE (XYZHIT( 7),RHIT1)
          EQUIVALENCE (XYZHIT( 8),IERZRF)
          EQUIVALENCE (XYZHIT( 9),LRSIDE)
          EQUIVALENCE (XYZHIT(10),INCELL)
          EQUIVALENCE (XYZHIT(11),IERXYF)
          EQUIVALENCE (XYZHIT(12),SECGAM)
          EQUIVALENCE (XYZHIT(13),INRING)
          EQUIVALENCE (XYZHIT(14),CHIXYF)
          NH1=IPHIT
          LRSIDE=LRSOLN
C
C         ----------------
C         FETCH RUN NUMBER
C         ----------------
C
          IF(CALLED)GOTO100
          IPHEAD=IBLN('HEAD')
          CALLED=.TRUE.
  100     NRUN=2*IDATA(IPHEAD)
          IF(NRUN.GT.0)NRUN=HDATA(NRUN+10)
C
C         ------------------
C         DATA AND CONSTANTS
C         ------------------
C
          INWIRE=HDATA(NH1)
          INWIRE=ISHFTR(INWIRE,3)
          IAMPL=HDATA(NH1+1)
          IAMPR=HDATA(NH1+2)
          IDRIFT=HDATA(NH1+3)
          LAYER=LAND(INWIRE,15)
          INCELL=LAND(ISHFTR(INWIRE,4),127)+ 1
          INRING=1
          IF(INCELL.GT.24)INRING=2
          IF(INCELL.GT.48)INRING=3
          DR0=RINCR(INRING)
          JCELL=INCELL-24*(INRING-1)
          IF(INRING.EQ.3)GOTO160
          COSPHW=DIRWR1(JCELL,1)
          SINPHW=DIRWR1(JCELL,2)
          GOTO170
  160     COSPHW=DIRWR3(JCELL,1)
          SINPHW=DIRWR3(JCELL,2)
C
C         ---------------------------
C         CALCULATE Z AND FLAG ERRORS
C         ---------------------------
C
  170     IF(IAMPR.LE.0.OR.IAMPL.LE.0)GOTO180
          ZHIT1=IAMPR+IAMPL
          ZHIT1=.5*ZAL*FLOAT(IAMPR-IAMPL)/ZHIT1
          IERZRF=0
          GOTO190
  180     IERZRF=10
          ZHIT1=0.0
  190     CONTINUE
C
C         -------------------
C         COMPUTE WIRE RADIUS
C         -------------------
C
          RWIRE=FSENSW(INRING)+LAYER*DR0
C
C         ---------------
C         WIRE STAGGERING
C         ---------------
C
          STAG=-SWDEPL
          IF(TBIT(LAYER,31))STAG=+SWDEPL
C
C         -------------------
C         COMPUTE X,Y OF WIRE
C         -------------------
C
          XWIRE=RWIRE*COSPHW+STAG*SINPHW
          YWIRE=RWIRE*SINPHW-STAG*COSPHW
C
C         -----------------------
C         SET LEFT/RIGHT SOLUTION
C         -----------------------
C
          LRFLAG=ISIGN(1,LRSIDE)
  200     LR12=1
          IF(LRFLAG.GT.0)LR12=2
C
C         ---------------
C         A FEW CONSTANTS
C         ---------------
C
          SINA=SINDRI(INCELL,1)
          COSA=COSDRI(INCELL,1)
          TANA=SINA/COSA
          TRLORX=TRMATS(INCELL,1)
          TRLORY=TRMATC(INCELL,1)
          DRFVEL=DRIVEL(INCELL,1)
          TANPHW=SINPHW/COSPHW
C
C         -----------------------------
C         COMPUTE BETA AND GAMMA ANGLES
C         -----------------------------
C
          SINPHI=SIN(PHITRK)
          COSPHI=COS(PHITRK)
          COSGAM=TRLORY*COSPHI-TRLORX*SINPHI
          SINGAM=TRLORX*COSPHI+TRLORY*SINPHI
          TANGAM=SINGAM/COSGAM
          SECGAM=1./COSGAM
          SINBET=SINPHI*COSPHW-COSPHI*SINPHW
          COSBET=COSPHI*COSPHW+SINPHI*SINPHW
          TANBET=SINBET/COSBET
          TLTCOR=1./(COSA+SINA*TANBET)
C
C         --------------------------------
C         CONVERT DRIFT TIME INTO DISTANCE
C         --------------------------------
C
          DRIFT=IDRIFT*DRFVEL
C
C         -------------
C         T0 CORRECTION
C         -------------
C
          IF(NRUN.GT.0)DRIFT=DRIFT + 64*T0FIX(INRING)*DRFVEL
          IF(NRUN.LE.0)DRIFT=DRIFT+0.5*DRFVEL
C
C         --------------------------------------------
C         CALCULATE ROUGH COORDINATES (XHIT,YHIT,RHIT)
C         --------------------------------------------
C
          XHIT=XWIRE+LRFLAG*TRLORX*DRIFT
          YHIT=YWIRE+LRFLAG*TRLORY*DRIFT
          RHIT=SQRT(XHIT**2+YHIT**2)
C
C         -----------------------------
C         CALCULATE FITTED Z-COORDINATE
C         -----------------------------
C
          ZFIT=GRADZR*RHIT+ZRINT
C
C         -----------------------------
C         CORRECTION FOR TIME OF FLIGHT
C         -----------------------------
C
          DDRIFT=CABER6*SQRT(RHIT**2+ZFIT**2)
          DRIFT=DRIFT-DDRIFT
C
C         ---------------------------
C         PROPOGATION TIME ALONG WIRE
C         ---------------------------
C
          DDRIFT=CABER1*(1222.9-ABS(ZFIT))
          DRIFT=DRIFT-DDRIFT
C
C         -----------------------------
C         CORRECTION FOR WIRE POSITIONS
C         -----------------------------
C
          DDRIFT=CABER8*((DELTA5(INCELL,1)+DELTA5(INCELL,2)*ZFIT/1211.5)
     +    *(LAYER-7)+(DELTA6(INCELL,1)+DELTA6(INCELL,2)*ZFIT/1211.5))
     +        *TLTCOR
          DRIFT=ABS(DRIFT*LRFLAG-DDRIFT*10.)
C
C         --------------------------------
C         DRIFT PATH DISPERSION CORRECTION
C         --------------------------------
C
          DDRIFT=0.
          IF(DRIFT.GT.CABER2.AND.TANGAM.GE.0.)GOTO250
          IF(DRIFT.GT.CABER3.AND.TANGAM.LT.0.)GOTO255
          DDRIFT=(SECGAM-1.)*DRIFT
          GO TO 260
  250     DDRIFT=(SECGAM-1.)*CABER2
          GO TO 260
  255     DDRIFT=(SECGAM-1.)*CABER3
  260     CONTINUE
          DRIFT=DRIFT+DDRIFT
C
C         ----------------------------------
C         DRIFT VELOCITY VARIATION NEAR WIRE
C         ----------------------------------
C
          DDRIFT=0.
          IF(DRIFT.LT.CABER4 ) DDRIFT=CABER5*(DRIFT-CABER4)**2
          DRIFT=DRIFT+DDRIFT
C
C         ---------------------
C         EDGE WIRE DISTORTIONS
C         ---------------------
C
          IF(LAYER.GE.3) GO TO 271
          DDRIFT=DELTA1(INCELL,LR12)*(LAYER-3)**2*DRIFT
          GO TO 272
 271      IF(LAYER.LE.12)  GO TO 273
          DDRIFT=DELTA2(INCELL,LR12)*(LAYER-12)**2*DRIFT
 272      DRIFT=DRIFT-DDRIFT*CABER8
 273      CONTINUE
C
C         -------------------------------------
C         FIELD DISTORTION AT LARGE DRIFT TIMES
C         -------------------------------------
C
          DDRIFT=0.
          IF(DRIFT.LT.CABER7) GO TO 280
          WMID=7.5+(DRIFT*0.1+(7.5-LAYER)*TANBET*TLTCOR)*SINA*0.5
          DDRIFT=LAYER-WMID
          DDRIFT=-LRFLAG*DELTA0(INCELL,LR12)
     -                                     *TANGAM*DDRIFT*(DRIFT-CABER7)
          DRIFT=DRIFT-DDRIFT*CABER8
 280      CONTINUE
C
C         -------------------------
C         FINALLY CALCULATE (X,Y,R)
C         -------------------------
C
 300      XHIT1=XWIRE+LRFLAG*TRLORX*DRIFT
          YHIT1=YWIRE+LRFLAG*TRLORY*DRIFT
          RHIT1=SQRT(XHIT1**2+YHIT1**2)
          IERXYF=0
          NH=RESULT(3)
          CALLMVCL(RESULT(1),0,XYZHIT(1),0,56)
          RETURN
          END
