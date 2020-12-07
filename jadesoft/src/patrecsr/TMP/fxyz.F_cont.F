C   08/12/80 308171157  MEMBER NAME  FXYZNC   (JADESR)      FORTRAN
      SUBROUTINE FXYZ(NT)
      IMPLICIT INTEGER*2 (H)
      LOGICAL TBIT
C
C
C-----------------------------------------------------------------------
C
C         -------------- SUBROUTINE FXYZ(NT) -----------------
C         ---- G.F.PEARCE .. LAST UPDATE : 1030 ON 31/10/80 ----
C
C     THIS SUBROUTINE CALCULATES THE COORDINATES IN REAL SPACE OF ALL
C     HITS ASSOCIATED BY THE TRACK RECOGNITION PROGRAM WITH A SINGLE
C     TRACK. THE SINGLE TRACK FOR WHICH THIS IS DONE IS DEFINED BY THE
C     USER ON INPUT BY THE NUMBER NT, THIS BEING THE NUMBER OF THE
C     TRACK IN THE OUTPUT COMMON FROM SUBROUTINE BACKTR.
C
C     THE RESULTS ARE PLACED IN THE ARRAY WRK IN COMMON/CWORK/.
C     HPHT0    = POINTER TO FIRST WORD OF FIRST HIT IN WRK.
C     HPHT9    = POINTER TO LAST WORD OF LAST HIT IN WRK.
C     HLDHT    = NUMBER OF 4-BYTE WORDS STORED PER HIT IN WRK.
C     HPHT0A   = POINTER TO FIRST WORD OF FIRST HIT FOR COORDINATES
C                CALCULATED USING THE OPPOSITE L/R SOLUTION.
C                IF BACKTRACE HAS UNAMBIGUOUSLY RESOLVED THE L/R SOLN
C                THEN NO REVERSED L/R SOLN IS COMPUTED AND THIS POINTER
C                IS SET NEGATIVE.
C     HPHT9A   = POINTER TO LAST WORD OF LAST HIT IN WRK FOR OPPOSITE
C                L/R SOLUTION. ONLY SET WHEN IDWRK(1) IS NOT NEGATIVE.
C     HPHTLM   = LOWEST ALLOWABLE HPHT0 (CORRESPONDING TO NHITMAX)
C
C-----------------------------------------------------------------------
C
C
      COMMON/CADMIN/IEVTP,NREC,NRWRIT,NRERR
      COMMON/CHEADR/HEAD(20)
C
#include "calibr.for"
C
#include "cdata.for"
#include "ccycp.for"
#include "cworkpr.for"
#include "cworkeq.for"
      COMMON / CJDRCH / RDEC(4),PSIIN(3),RINCR(3),FIRSTW(3),FSENSW(3),
     * RDEPTH,SWDEPL,YSUSPN,TIMDEL(2,3),ZMAX,ZOFFS,ZRESOL,ZNORM,ZAL,
     * ZSCAL,DRIDEV,DRICOS,DRISIN,PEDES,TZERO(3),
     * DRIROT(96,2),SINDRI(96,2),COSDRI(96,2),DRIVEL(96,2),T0FIX(3),
     * CAB(8)
C  TEMPORARY INSTALLMENT OF ARRAY T0FIX, WILL BE PERMANENT LATER
#include "cdsmax.for"
#include "cpatlm.for"
C
C-----------------------------------------------------------------------
C    EQUIVALENCE OUTPUT BANK TO ADWRK FOR EASE OF TRANSFER TO HIT BANK
C-----------------------------------------------------------------------
C
         EQUIVALENCE (ADWRK( 1),LAYER)
         EQUIVALENCE (ADWRK( 2),NH1)
         EQUIVALENCE (ADWRK( 3),NH)
         EQUIVALENCE (ADWRK( 4),XHIT1)
         EQUIVALENCE (ADWRK( 5),YHIT1)
         EQUIVALENCE (ADWRK( 6),ZHIT1)
         EQUIVALENCE (ADWRK( 7),RHIT1)
         EQUIVALENCE (ADWRK( 8),IERZRF)
         EQUIVALENCE (ADWRK( 9),NLMIDO)
         EQUIVALENCE (ADWRK(10),INCELL)
         EQUIVALENCE (ADWRK(11),IERXYF)
         EQUIVALENCE (ADWRK(12),BETA)
         EQUIVALENCE (ADWRK(13),INRING)
         EQUIVALENCE (ADWRK(14),CHIXYF)
C -- MASKS MK1 TO EXTRACT TRACK ELEMENT NUMBER FROM HIT LABEL
C --       MK2 TO EXTRACT WIRE CROSSING FLAG
C --       MK3 TO EXTRACT FLAG ON UNRESOLVED L/R SOLUTION IN BACKTRACE
C --       MK4 TO EXTRACT FLAG ON TEST TRACK ELEMENT.
         INTEGER*4 MK1/Z00FE/ , MK2/Z100/ , MK3/Z1000/ , MK4/Z20000/
         HPWRK(30) = NT
C
C        ----------------------
C        POINTER INITIALISATION
C        ----------------------
C
         HLDHT = 14
         LONG = HLDHT * 4
         MAXHT = 80
         INHIT = HPFREE + MAXHT * HLDHT
         IF (INHIT.LE.HPLAST) GOTO10
C ----- ERROR 1 --------- NOT ENOUGH SPACE IN CWORK
         CALL ERRORM('FXYZ    ',1,NT)
         INHIT = HPLAST
         MAXHT = (HPLAST-HPFREE)/HLDHT
         IF (MAXHT.GE.8) GOTO10
C ----- ERROR 2 --------- HAD TO ABANDON TRACK NT
         CALL ERRORM('FXYZ    ',2,NT)
         HPHT0 = 1
         HPHT9 = 1
         GOTO1600
 10      HPHT9 = INHIT - 1
         HPHTLM = HPFREE
         HPFREE = INHIT
C
C        -------------------------------
C        OPPOSITE L/R AMBIGUITY POINTERS
C        AMBIGUITY 1 AT TOP, 2 AT BOTTOM
C        ------------------------------
C
         INHIT2 = -1
         IW1 = HISTR(1,NT)
         IW1 = LBL(IABS(IW1))
         LRNOWN = LAND(IW1,MK3)
         IF (TBIT(IXYF(1),26)) LRNOWN = 0
         IF (LRNOWN.EQ.0) GOTO50
C
         HLDHTA = HLDHT
         MAXHT = MAXHT / 2
         INHIT2 = INHIT - MAXHT * HLDHT
         HPHT9A = INHIT2 - 1
C
 50      NHIT = 0
         CHIXYF = 0
         NL = HNREL(NT)
         IF (NL.GT.0) GOTO60
C ----- ERROR 3 --------- NO TRACK ELEMENTS FROM BACKTRACE
         CALL ERRORM('FXYZ    ',3,NT)
         GOTO1590
 60      NLLAST = - 1
         NRLAST = - 1
C
C
C---------------------------------------------------------------------
C   LOOP OVER EACH TRACK ELEMENT BELONGING TO THIS TRACK. FOR
C   EACH TRACK ELEMENT STORE ALL REQUIRED INFORMATION BEFORE LOOPING
C   OVER HITS.
C---------------------------------------------------------------------
C
C
 1140    CONTINUE
C
C        -------------------------------
C        POINTER TO TRACK ELEMENT IN
C        COMMON/MIDOUT/. THIS CARRIES A
C        +VE/-VE SIGN INDICATING A RIGHT/
C        LEFT SOLUTION FROM BACKTRACING
C        -------------------------------
C
         NLMIDO = HISTR(NL,NT)
         IF (NLMIDO.NE.0)GOTO1150
C ----- ERROR 4 --------- TRACK ELEMENT ZERO FROM BACKTRACE
         CALL ERRORM('FXYZ    ',4,NT)
         GOTO1580
 1150    ILMIDO = IABS(NLMIDO)
         LABEL = LBL(ILMIDO)
C
C        -------------------------------
C        IS THIS A TEST TRACK ELEMENT ?
C        -------------------------------
C
         IERXYF=LAND(LABEL,MK4)
         IF(IERXYF.NE.0)IERXYF=2
C
C        -------------------------------
C        CELL NUMBER AND RING NUMBER
C        -------------------------------
C
         INCELL = IPCL(ILMIDO)
         INRING = 1
         IF(INCELL.GT.24)INRING = 2
         IF(INCELL.GT.48)INRING = 3
C
C        -------------------------------
C        LEFT/RIGHT AMBIGUITY FLAG FOR
C        FIRST HIT ON TRACK ELEMENT
C        -------------------------------
C
         IF (NLMIDO.GT.0) GOTO1160
         LRFLAG = - 1
         LR12 = 1
         GOTO1170
 1160    LRFLAG = + 1
         LR12   = 2
C
C        -------------------------------
C        LBZRCR = LABEL FOR WIRE CROSSING
C        IWCRS  = FIRST WIRE # AFTER ELEMENT
C                 CROSSES WIRE BOUNDARY.
C        -------------------------------
C
 1170    LBZRCR = LAND(LABEL,MK2)
         IWCRS = ICRO(ILMIDO)
C
C        ------------------------------
C        LORENTZ ANGLE TRANSFORMATION
C        AND DRIFT VELOCITY
C        ------------------------------
C
         TRLORX = TRMATS(INCELL,1)
         TRLORY = TRMATC(INCELL,1)
         SINLOR = SINDRI(INCELL,1)
         COSLOR = COSDRI(INCELL,1)
         DRFVEL = DRIVEL(INCELL,1)
C
C        ----------------
C        ABERATION ARRAYS
C        ----------------
C
C
C        CORRECTION CONSTANTS FOR INCELL
         IPJCOR = ICALIB(5) + INCELL
         IPJCOS = IPJCOR + (LR12-1)*96
         CCST0  = ACALIB(IPJCOS     )
         CCST1  = ACALIB(IPJCOS+ 192)
         CCST2  = ACALIB(IPJCOS+ 384)
         CCST51 = ACALIB(IPJCOR+ 576) * 10.
         CCST61 = ACALIB(IPJCOR+ 768) * 10.
         CCST81 = ACALIB(IPJCOR+1152)
C
C        -------------------------------
C        RING RADIUS AND WIRE SEPARATION
C        -------------------------------
C
         R0  = FSENSW(INRING)
         DR0 = RINCR(INRING)
C
C        --------------------------------------
C        COMPUTE AVERAGE ANGLE OF TRACK TO WIRE
C        PLANE OVER THIS TRACK ELEMENT. USE THIS
C        ANGLE TO COMPUTE THE CORRECTION TO DRIFT
C        SPACE REQUIRED TO CENTRE DRIFT TIME ON
C        DRIFT CHARGE CENTRE.
C        ---------------------------------------
C
         DXWR = DR0 * COSLOR
         DYWR = DR0 * SINLOR
C        FIRST HALF OF HITS
         BETA  = ( LRFLAG * SL1(ILMIDO) - DYWR )
         BETA = SQRT( DXWR**2 + BETA**2 ) / DXWR
C        SECOND HALF OF HITS
         LAYERM = LRFLAG
         IF (IWCRS.NE.0) LAYERM = - LAYERM
         BETA2 = ( LAYERM * SL2(ILMIDO) - DYWR )
         BETA2 = SQRT( DXWR**2 + BETA2**2 ) / DXWR
         BETA = (BETA + BETA2) / 2
         IF (BETA.LT.1.0) BETA = 1.0
         IF (BETA.GT.1.1) BETA = 1.1
         TANBET=SQRT(BETA**2-1.)
         BETAM1 = BETA-1
C
C        -----------------------------------
C        WMID FOR FIELD DISTORTION ABERATION
C        -----------------------------------
C
         WMID=7.5+(DS1(ILMIDO)+DS2(ILMIDO))*SINLOR/40
C
C        -------------------------------
C        TRACK ELEMENT NUMBER AS STORED
C        IN HIT LABEL DATA BANK.
C        -------------------------------
C
         NLWANT = NTREL(ILMIDO)
C
C        ----------------------------
C        DIRECTION OF WIRE PLANE
C        AND DRIFT SPACE DIRECTION
C        ----------------------------
C
         IW1 = INCELL - 24*(INRING-1)
         IF (INRING.EQ.3) GOTO1180
         DXWR   = DIRWR1(IW1,1)
         DYWR   = DIRWR1(IW1,2)
         GOTO1190
 1180    DXWR   = DIRWR3(IW1,1)
         DYWR   = DIRWR3(IW1,2)
C
C        ------------------------------
C        WIRE STAGGERING ( +/- SWDEPL )
C        ------------------------------
C
 1190    DDXWR = + SWDEPL * DYWR
         DDYWR = - SWDEPL * DXWR
C
C
C----------------------------------------------------------------------
C   LOOP OVER ALL HITS FOR THIS CELL AND SEARCH FOR HITS BELONGING
C   TO THIS TRACK ELEMENT.
C----------------------------------------------------------------------
C
C         ----------------------------
C         DETERMINE POINTERS TO
C         HIT ARRAY AND HIT LABEL
C         ----------------------------
          NH0 = HPTSEC(INCELL)
          NH9 = HPTSEC(INCELL+1)-1
          IF (NH0.GT.0.AND.NH9.GT.0) GOTO1230
C ----- ERROR 6 --------- HPTSEC POINTER IS -VE
         CALL ERRORM('FXYZ    ',6,NT)
          GOTO1580
 1230     NH  = ISHFTR((NH0-HPTSEC(1)),1) + HPHL0
          NH1 = NH0
C
C         ----------------------------
C         LOOP OVER HITS IN CELL
C         ACCEPT HITS ON NLWANT BUT
C         PROTECT AGAINST HIT APPEARING
C         ON SUCCESSIVE TRACK ELEMENTS.
C         ----------------------------
C
 1240     CONTINUE
          IW1 = HWRK(NH)
          IW2 = HWRK(NH+1)
          NL1 = ISHFTR(LAND(IW1,MK1),1)
          NL2 = ISHFTR(LAND(IW2,MK1),1)
          IF(NL1.NE.NLWANT.AND.NL2.NE.NLWANT)GOTO1570
C
C         ------------------------------
C         THIS HIT IS ON REQUIRED TREL
C         REVERSE L/R AMBIGUITY IF THIS
C         HIT CROSSES WIRE BOUNDARY
C         -----------------------------
C
          IWIRE = HDATA(NH1  )
          IWIRE = ISHFTR(IWIRE,3)
          LAYER = LAND(IWIRE,15)
          IF(LBZRCR.EQ.0 .OR. LAYER.NE.IWCRS)GOTO1250
C
C         ---------------------
C         REVERSE L/R AMBIGUITY
C         ---------------------
C
          LRFLAG = - LRFLAG
          NLMIDO = - NLMIDO
          IPJCOS = IPJCOR + (2-LR12)*96
          CCST0  = ACALIB(IPJCOS     )
          CCST1  = ACALIB(IPJCOS+ 192)
          CCST2  = ACALIB(IPJCOS+ 384)
C
C         ------------------------------
C         REJECT THIS HIT IF IT WAS ALSO
C         ON THE PRECEDING TRACK ELEMENT
C         ------------------------------
C
 1250     IF(INRING.NE.NRLAST)GOTO1260
          IF(NL1.EQ.NLLAST.OR.NL2.EQ.NLLAST)GOTO1570
C
C         ------------------------------
C         LOAD DRIFT TIME AND Z AMPLIT.
C         ------------------------------
C
 1260     IAMPL=HDATA(NH1+1)
          IAMPR=HDATA(NH1+2)
          IDRIFT=HDATA(NH1+3)
C
C         -----------------------------
C         FLAG BAD Z-COORDINATE IF DRIFT
C         TIME FOR THIS HIT IS WITHIN
C         DOUBLE HIT RESLN. OF ANOTHER
C         HIT ON THIS LAYER.
C         -----------------------------
C
          IERZRF=0
CC--CC    CLOSEST HIT WITH LOWER DRIFT TIME
CC--CC    NH2=NH1-4
CC--CC    IF(NH2.LT.NH0)GOTO1270
CC--CC    IWIRE2=HDATA(NH2)
CC--CC    IWIRE2=ISHFTR(IWIRE2,3)
CC--CC    IF(IWIRE2.NE.IWIRE)GOTO1270
CC--CC    NEARH2=IDRIFT-HDATA(NH2+3)
CC--CC    IF(IABS(NEARH2).LE.IXYF(18))IERZRF=16
CC--CC    CLOSEST HIT WITH HIGHER DRIFT TIME
C1270     NH3=NH1+4
CC--CC    IF(NH3.GT.NH9)GOTO1280
CC--CC    IWIRE3=HDATA(NH3)
CC--CC    IWIRE3=ISHFTR(IWIRE3,3)
CC--CC    IF(IWIRE3.NE.IWIRE)GOTO1280
CC--CC    NEARH3=IDRIFT-HDATA(NH3+3)
CC--CC    IF(IABS(NEARH3).LE.IXYF(18))IERZRF=16
C
C         ------------------------
C         CREATE SPACE FOR NEW HIT
C         ------------------------
C
 1280     IF(NHIT.LT.MAXHT)GOTO1290
C ----- ERROR 5 --------- RAN OUT OF SPACE IN CWORK FOR TRACK NT
          CALLERRORM('FXYZ    ',5,NT)
          GOTO1590
 1290     NHIT=NHIT+1
          INHIT=INHIT-HLDHT
C
C-----------------------------------------------------------------------
C   COMPUTE (X,Y,Z,R) FOR THIS HIT AND STORE IN THE COORDINATE ARRAY WRK
C-----------------------------------------------------------------------
C
C         ------------
C         Z-COORDINATE
C         ------------
C
          IF(IAMPR.LE.0.OR.IAMPL.LE.0)GOTO1310
          ZHIT1=IAMPR+IAMPL
          ZHIT1=.5*ZAL*FLOAT(IAMPR-IAMPL)/ZHIT1
          GOTO1320
 1310     IERZRF=16
          ZHIT1=0.0
 1320     CONTINUE
C
C         -----------
C         WIRE RADIUS
C         -----------
C
          RWIRE=R0+LAYER*DR0
C
C         ----------------------------
C         CORRECTION FOR WIRE POSITION
C         AT Z=0 PERP. TO WIRE PLANE
C         ----------------------------
C
          DWIRE = CCST51*(LAYER-7.5) + CCST61
          DWIREX = + DWIRE * DYWR
          DWIREY = + DWIRE * DXWR
C
C         ----------------
C         WIRE COORDINATES
C         ----------------
C
          ISTAG=-1
          IF(TBIT(LAYER,31))ISTAG=+1
          DX1=RWIRE*DXWR+ISTAG*DDXWR+DWIREX
          DY1=RWIRE*DYWR+ISTAG*DDYWR-DWIREY
C
C         --------------------------------
C         CONVERT DRIFT TIME INTO DISTANCE
C         --------------------------------
C
          DRIFT=IDRIFT*DRFVEL
          IF(HEAD(18).LE.0)DRIFT=DRIFT+0.5*DRFVEL
C
C         --------------------------------
C         DRIFT PATH DISPERSION CORRECTION
C         --------------------------------
C
          IF(DRIFT.GT.4.0)GOTO1360
          DRIFT=DRIFT+BETAM1*DRIFT
          GOTO1370
 1360     DRIFT=DRIFT+BETAM1*4.0
 1370     CONTINUE
C
C         --------------------------
C         EDGE WIRE FIELD DISTORTION
C         --------------------------
C
          IF(LAYER.GE.3)GOTO1380
          DRIFT=DRIFT*(1.-CCST1*(LAYER-3)**2)
          GOTO1390
 1380     IF(LAYER.LE.12)GOTO1390
          DRIFT=DRIFT*(1.-CCST2*(LAYER-12)**2)
 1390     CONTINUE
C
C         --------------------------------------
C         FIELD DISTORTIONS AT LARGE DRIFT TIMES
C         --------------------------------------
C
          IF(DRIFT.GT.CAB(7))
     +    DRIFT=DRIFT+LRFLAG*CCST0*(LAYER-WMID)*TANBET*(DRIFT-CAB(7))
C
C         ----------------------------------
C         DRIFT VELOCITY VARIATION NEAR WIRE
C         ----------------------------------
C
          IF(DRIFT.LT.CAB(4))DRIFT=DRIFT+CAB(5)*(DRIFT-CAB(4))**2
C
C         -------------------------
C         RESET -VE DRIFT TIMES TO
C         HELP L/R AMBIGUITY SOLN.
C         AS REQUESTED BY P.STEFFEN
C         -------------------------
C
          IF(DRIFT.LT.0.)DRIFT=0.05
C
C         ---------------------
C         X,Y AND R COORDINATES
C         ---------------------
C
          IF(LRFLAG.GT.0) GOTO 20
            XHIT1=DX1 - TRLORX*DRIFT * (1. - CCST81)
            YHIT1=DY1 - TRLORY*DRIFT * (1. - CCST81)
            GOTO 25
   20     CONTINUE
            XHIT1=DX1 + TRLORX*DRIFT * (1. + CCST81)
            YHIT1=DY1 + TRLORY*DRIFT * (1. + CCST81)
   25     CONTINUE
          DR2=DRIFT/RWIRE
          DR1=0.5*DR2**2
          DR2=-LRFLAG*DR2*SINLOR
          RHIT1=DR1+DR2
          RHIT1=RWIRE*(1+RHIT1-.5*RHIT1**2)
C
C         -----------------------------
C         LOAD COORDINATES AND INCREASE
C         POINTER INHIT FOR NEXT HIT
C         ------------------------------
C
          CALLMVCL(WRK(INHIT),0,ADWRK(1),0,LONG)
C
C         --------------------------------
C         IF BACKTR UNCERTAIN ABOUT L/R
C         SOLUTION, COMPUTE BOTH.
C         --------------------------------
C
          IF(LRNOWN.EQ.0)GOTO1570
          XHIT1=DX1-LRFLAG*TRLORX*DRIFT
          YHIT1=DY1-LRFLAG*TRLORY*DRIFT
          DR2=DRIFT/RWIRE
          DR1=0.5*DR2**2
          DR2=-LRFLAG*DR2*SINLOR
          RHIT1=DR1-DR2
          RHIT1=RWIRE*(1+RHIT1-.5*RHIT1**2)
          INHIT2=INHIT2-HLDHT
          CALLMVCL(WRK(INHIT2),0,ADWRK(1),0,LONG)
          IWRK(INHIT2+8)=-IWRK(INHIT2+8)
C
C         ----------------
C         FETCH NEXT LAYER
C         ----------------
C
 1570     NH=NH+2
          NH1=NH1+4
          IF(NH1.LE.NH9)GOTO1240
C
C         ------------------------
C         FETCH NEXT TRACK ELEMENT
C         ------------------------
C
          NLLAST=NLWANT
          NRLAST=INRING
 1580     NL=NL-1
          IF(NL.NE.0)GOTO1140
C
C         ---------------------------
C         END OF FXYZ - SET POINTERS
C         ---------------------------
C
 1590     HPHT0=INHIT
          HPHT0A=INHIT2
 1600     CONTINUE
C2005 FORMAT('0TRACK:',I6,/,(1X,3I6,4F9.3,4I4,F9.3,I6,F9.3))
C     PRINT 2005, HLDHT,(WRK(I),I=HPHT0,HPHT9)
          RETURN
          END
