C   08/12/80 308171346  MEMBER NAME  PATROLNC (JADESR)      FORTRAN
      SUBROUTINE PATROL(RMIN,RMAX)
      IMPLICIT INTEGER*2(H)
      LOGICAL TBIT
C
C
C----------------------------------------------------------------------
C       --------------  SUBROUTINE PATROL  -----------------
C       ---- G.F.PEARCE .. LAST UPDATE : 2020 ON  4/11/82 ----
C   SUBROUTINE TO SEARCH AND RECORD HITS MISSED BY THE PATTERN
C   RECOGNITION PROGRAMS. IF SUFFICIENT NEW HITS ARE FOUND A REFIT
C   OF THE TRACK IS MADE AND PATROL THEN RECALLS ITSELF.
C
C   TO SATISFY CERTAIN COMPLAINTS MANY VERBOSE COMMENTS HAVE BEEN
C   INTRODUCED. HOPEFULLY EVERYONE ELSE WILL DO THE SAME, BUT I DOUBT IT
C
C  CONTROLLING PATROL.
C  ===================
C                   CONTROL OF THE PARAMETERS AND LIMITS USED BY
C  THIS PROGRAM IS ACHIEVED THROUGH THE ARRAY GFP IN COMMON CPATLM.
C  FOR DETAILS SEE THE INITIALISING ROUTINE INPATR.
C
C  PATROL ERROR MESSAGES
C  =====================
C  PATROL ERROR 1 => PATROL CALLED FOR A TRACK WITH ILLEGAL FIT TYPE
C  PATROL ERROR 2 => NOT ENOUGH ROOM IN CWORK TO STORE ANY MORE HITS
C  PATROL ERROR 3 => ILLEGAL RETURN VALUE - THIS SHOULD NEVER HAPPEN
C  PATROL ERROR 4 => THERE WERE NO GOOD HITS ON THE TRACK WHEN PATROL
C                    WAS CALLED - PATROL IGNORES THIS TRACK.
C  PATROL ERROR 5 => A POINTER WAS -VE. PROBABLY HPTSEC OVERFLOWED
C  PATROL ERROR 6 => PATROL FOUND A HOLE BIGGER THAN THE TRACK. THIS
C                    IS ALMOST CERTAINLY A BACKTRACE ERROR RESULTING
C                    IN THE WRONG ORDERING OF TRACK ELEMENTS. (THEY
C                    SHOULD BE SUPPLIED BY BACKTR RADIALLY ORDERED)
C  PATROL ERROR 7 => OVERSIZE CORRECTION WHEN CONVERTING A PARABOLA TO
C                    A CIRCLE. THE PREVIOUS ITERATION IS USED AND
C                    PATROL CONTINUES.
C  PATROL ERROR 8 => TOO MANY REFITS CALLED BY PATROL. PATROL STOPS
C                    AND RETURNS THOSE HITS ALREADY FOUND.
C  PATROL ERROR 9 => FAILED TO CALCULATE DRIFT PATH DISPERSION
C                    CORRECTION. IT IS ASSUMED TO BE MAXIMUM.
C
C----------------------------------------------------------------------
C
C
#include "calibr.for"
C
#include "cworkpr.for"
C
#include "cworkeq.for"
C
C=======EQUIVALENCES TO MACROS=======
      REAL*4 SL12(600,2)
      EQUIVALENCE (SL1(1),SL12(1,1))
      INTEGER*4 MINCL(3),MAXCL(3)
      EQUIVALENCE (MINCL(1),IBCK(4)) , (MAXCL(1),IBCK(1))
C====================================
C
#include "cdatamin.for"
C
#include "ccycp.for"
C
#include "cdsmax.for"
C
      COMMON / CJDRCH / RDEC(4),PSIIN(3),RINCR(3),FIRSTW(3),FSENSW(3),
     * RDEPTH,SWDEPL,YSUSPN,TIMDEL(2,3),ZMAX,ZOFFS,ZRESOL,ZNORM,ZAL,
     * ZSCAL,DRIDEV,DRICOS,DRISIN,PEDES,TZERO(3),
     * DRIROT(96,2),SINDRI(96,2),COSDRI(96,2),DRIVEL(96,2),T0FIX(3),
     * CAB(8)
C
#include "cpatlm.for"
C
      COMMON/CHEADR/HEAD(20)
C
C-------------------------------------------
C    EQUIVALENCE HIT BANK TO ADWRK
C-------------------------------------------
C
          EQUIVALENCE(ADWRK( 1),LAYERJ)
          EQUIVALENCE(ADWRK( 2),NH1)
          EQUIVALENCE(ADWRK( 3),NH)
          EQUIVALENCE(ADWRK( 4),XJETHT)
          EQUIVALENCE(ADWRK( 5),YJETHT)
          EQUIVALENCE(ADWRK( 6),ZJETHT)
          EQUIVALENCE(ADWRK( 7),RJETHT)
          EQUIVALENCE(ADWRK( 8),IERZRF)
          EQUIVALENCE(ADWRK( 9),LRTREL)
          EQUIVALENCE(ADWRK(10),INCELL)
          EQUIVALENCE(ADWRK(11),IERXYF)
          EQUIVALENCE(ADWRK(12),BETA)
          EQUIVALENCE(ADWRK(13),INRING)
          EQUIVALENCE(ADWRK(14),CHIXYF)
C
C-------------------------------------------
C         EQUIVALENCE TRACK BANK TO ADWRK
C-------------------------------------------
C
          EQUIVALENCE(ADWRK(21),XSTR)
          EQUIVALENCE(ADWRK(22),YSTR)
          EQUIVALENCE(ADWRK(23),RSTR)
          EQUIVALENCE(ADWRK(28),XEND)
          EQUIVALENCE(ADWRK(29),YEND)
          EQUIVALENCE(ADWRK(30),REND)
          EQUIVALENCE(ADWRK(34),IAMFIT)
          EQUIVALENCE(ADWRK(35),COEFF1,RAD)
          EQUIVALENCE(ADWRK(36),COEFF2,DMID)
          EQUIVALENCE(ADWRK(37),COEFF3,EPSILN)
          EQUIVALENCE(ADWRK(38),COEFF4)
          EQUIVALENCE(ADWRK(40),NPUSED)
          EQUIVALENCE(ADWRK(41),CURV)
C
C-------------------------------------------
C        EQUIVALENCES TO CWORK 'ZERO' ARRAY
C-------------------------------------------
C
          REAL*4 ZERO(100)
          EQUIVALENCE (ADWRK(41),ZERO(1))
          LOGICAL*1 CLFLAG(100)
          INTEGER*2 WIRMSK(96),RGFLAG(4)
          EQUIVALENCE (ZERO( 1),CLFLAG(1))
C         EQUIVALENCE (ZERO(25),CLFLAG(100))
          EQUIVALENCE (ZERO(26),WIRMSK(1))
C         EQUIVALENCE (ZERO(73),WIRMSK(96))
          EQUIVALENCE (ZERO(74),RGFLAG(1))
C         EQUIVALENCE (ZERO(75),RGFLAG(3))
          EQUIVALENCE (ZERO(76),JETCL)
          EQUIVALENCE (ZERO(77),NCELLS)
          EQUIVALENCE (ZERO(78),LAYTRY)
          EQUIVALENCE (ZERO(79),LAYINT)
          EQUIVALENCE (ZERO(80),LAYEXT)
C
C-------------------------------------------
C     EQUIVALENCES TO CWORK OF WORK ARRAY
C-------------------------------------------
C
          INTEGER*2 CELLPN(20,2),NWSET(20),WIRSET
C
C-------------------------------------------
C        DECLARATION OF 'MEMORY' VARIABLES
C-------------------------------------------
C
          REAL*4 PIE/3.14159/,TWOPIE/6.28318/
          INTEGER*4 BIT26/Z00000020/,BIT31N/ZFFFFFFFE/
          INTEGER*4 MSKLBL/Z000000FE/
          INTEGER*2 LAYBIT(16)/
     #    Z0001 , Z0002 , Z0004 , Z0008 ,
     #    Z0010 , Z0020 , Z0040 , Z0080 ,
     #    Z0100 , Z0200 , Z0400 , Z0800 ,
     #    Z1000 , Z2000 , Z4000 , Z8000 /
          INTEGER*2 LAYMSK(16,2) /
     #    ZFFFE , ZFFFC , ZFFF8 , ZFFF0 ,
     #    ZFFE0 , ZFFC0 , ZFF80 , ZFF00 ,
     #    ZFE00 , ZFC00 , ZF800 , ZF000 ,
     #    ZE000 , ZC000 , Z8000 , Z0000 ,
     #    Z0000 , Z0001 , Z0003 , Z0007 ,
     #    Z000F , Z001F , Z003F , Z007F ,
     #    Z00FF , Z01FF , Z03FF , Z07FF ,
     #    Z0FFF , Z1FFF , Z3FFF , Z7FFF /
          LOGICAL*1 PRINT
          NTRAK = HPWRK(30)
          NREFIT=0
C#######################################################################
      PRINT = .FALSE.
      IF(NTRAK.EQ.IGFP(20)) PRINT = .TRUE.
      IF(IGFP(20).LT.0.AND.NTRAK.GE.IABS(IGFP(20))) PRINT=.TRUE.
C#######################################################################
C==============================================
C==============================================
C         INITIALISATION
C==============================================
C==============================================
          KNTRL=IGFP(1)
 510      CONTINUE
C#######################################################################
          IF(PRINT)PRINT511,NTRAK,IGFP(1)
 511  FORMAT(1X,38('=')/' PATROL TRACK',I4,' GFP(1) = ',Z8/1X,38('='))
C#######################################################################
          ISTORY=IWRK(HPTR0+47)
          LNBYTE=HLDHT * 4
C==============================================
C==============================================
C         COMPUTE TRACK PARAMETERS
C==============================================
C==============================================
          IAMFIT=IWRK(HPTR0+17)
          IF(IAMFIT.EQ.1.OR.IAMFIT.EQ.2)GOTO520
C ----- ERROR 1 ILLEGAL FIT TYPE
          CALLERRORM('PATROL  ',1,NTRAK)
          GOTO7000
 520      CURV=WRK(HPTR0+24)
C#######################################################################
      IF(PRINT.AND.ABS(CURV).GE.GFP(14)) PRINT521,CURV
 521  FORMAT(' PATROL ABANDONED .. CURVATURE TOO HIGH ',E11.4)
C#######################################################################
          IF(ABS(CURV).GE.GFP(14))GOTO7000
          COEFF1=WRK(HPTR0+18)
          COEFF2=WRK(HPTR0+19)
          COEFF3=WRK(HPTR0+20)
          COEFF4=WRK(HPTR0+21)
          XSTR=WRK(HPTR0+4)
          YSTR=WRK(HPTR0+5)
          RSTR=WRK(HPTR0+6)-5.
          XEND=WRK(HPTR0+11)
          YEND=WRK(HPTR0+12)
          REND=WRK(HPTR0+13)+5.
          TRLEN2=(XSTR-XEND)**2+(YSTR-YEND)**2
          GOTO(910,920),IAMFIT
C
C         --------------------
C         TYPE 1 .. CIRCLE FIT
C         --------------------
C
 910      RAD=1./RAD
          DMID=DMID+RAD
          DMIDSQ=DMID**2
          XMID=DMID*COS(EPSILN)
          YMID=DMID*SIN(EPSILN)
          GOTO990
C
C         -----------------------
C         TYPE 2 .. PARABOLA FIT
C         -----------------------
C
 920      S1=SIN(COEFF1)
          S2=COS(COEFF1)
          RAD=1./(2*COEFF4)
          XMID=COEFF2-RAD*S1
          YMID=COEFF3+RAD*S2
C         CORRECT INSCRIBED CIRCLE
          IF(ABS(RAD).GE.30000)GOTO931
          IS3=0
 930      IS3=IS3+1
          S4=(XSTR-XMID)**2+(YSTR-YMID)**2-RAD**2
          S4=8*S4/TRLEN2
          S5=S4*RAD
          IF(ABS(S4).LE.0.4)GOTO933
C ----- ERROR 7 OVERSIZE CIRCLE CORRECTION
          IF(PRINT)CALLERRORM('PATROL  ',7,NTRAK)
          GOTO931
 933      RAD=RAD+S5
          XMID=XMID-S5*S1
          YMID=YMID+S5*S2
C#######################################################################
      IF(PRINT) PRINT981,RAD,XMID,YMID,S4
 981  FORMAT(' CIRCLE CORRECTION..RAD/X0/Y0 =',3E11.4,' CORRN =',F7.3)
C#######################################################################
          IF(S4.GT.GFP(15).AND.IS3.LT.5)GOTO930
 931      EPSILN=ATAN2(YMID,XMID)
          DMIDSQ=XMID**2 + YMID**2
          DMID=SQRT(DMIDSQ)
C
C         --------------------------
C         CLEAN UP CIRCLE PARAMETERS
C         --------------------------
C
 990      RAD=ABS(RAD)
          RADSQ=RAD**2
          RAD2=RAD*2
C
C==============================================
C==============================================
C         CALCULATE RESIDUAL CUTS
C==============================================
C==============================================
C
          NPUSED=IWRK(HPTR0+23)
          S1=10*NPUSED*ABS(CURV)
          IF(S1.GT.1.0)S1=1.0
          CODE0=GFP(4)+S1*GFP(5)
          CODE2=GFP(3)
C#######################################################################
      IF(PRINT) PRINT991,NPUSED,CODE0,CODE2,RSTR,REND
 991  FORMAT(' PATROL CUTS..NPUSED =',I4,' CODE0/CODE2 =',2F4.1/
     #       '           RSTR/REND =',2E11.4)
C#######################################################################
C
C==============================================
C==============================================
C         FLAG HITS ALREADY ATTACHED TO TRACK
C==============================================
C==============================================
C
          CALLSETSL(ZERO(1),0,320,0.)
          LAYSNT=0
          INCELL=0
          JETWR=-1
          IPHT=HPHT9-HLDHT+1
 1010     IF(IWRK(IPHT+10).EQ.0)GOTO1030
 1020     IPHT=IPHT-HLDHT
          IF(IPHT.LT.HPHT0)GOTO1060
          GOTO1010
 1030     JNCELL=IWRK(IPHT+9)
          IF(JNCELL.NE.INCELL)GOTO1050
C
C         ----------------------
C         NEW LAYER --- OLD CELL
C         ----------------------
C
 1040     IS1=IS1+1
          LAYSNT=LAYSNT+1
          LAYER=IWRK(IPHT)+1
          WIRSET=hLOR(WIRSET,LAYBIT(LAYER)) ! PMF 10/06/99: hlor (N.B.: WIRSET,LAYBIT= I*2)
          IPHST=IPHT
          GOTO1020
 1050     IF(INCELL.EQ.0)GOTO1070
C
C         --------------------------------
C         CELL COMPLETED -- LOAD END FLAGS
C         --------------------------------
C
 1060     CELLPN(NC,2)=IPHST
          WIRMSK(INCELL)=WIRSET
          NWSET(NC)=IS1
          IF(IPHT.LT.HPHT0)GOTO1110
C
C         ----------------------------
C         NEW CELL -- INITIALISE FLAGS
C         ----------------------------
C
 1070     INCELL=JNCELL
          INRING=IWRK(IPHT+12)
          RGFLAG(INRING)=INRING + 1
          WIRSET=WIRMSK(INCELL)
          IF(CLFLAG(INCELL))GOTO1080
          NCELLS=NCELLS+1
          NC=NCELLS
          IS1=0
          CLFLAG(INCELL)=.TRUE.
          CELLPN(NC,1)=IPHT
          GOTO1040
C
C         -----------------------------
C         OLD CELL AGAIN -- RESET FLAGS
C         -----------------------------
C
 1080     DO 1090 NC=1,NCELLS
          JNCELL=CELLPN(NC,1)
          JNCELL=IWRK(JNCELL+9)
          IF(JNCELL.EQ.INCELL)GOTO1100
 1090     CONTINUE
 1100     IS1=NWSET(NC)
          GOTO1040
 1110     IF(LAYSNT.GE.4)GOTO1120
C ----- ERROR 4 NO GOOD HITS FROM FIT !!!
          CALLERRORM('PATROL  ',4,NTRAK)
          GOTO7000
 1120     IF(TBIT(KNTRL,30))GOTO3000
C
C==============================================
C==============================================
C         LOOK FOR NEW HITS IN CELLS WITH HITS
C==============================================
C==============================================
C
          DO 2200 NC=1,NCELLS
          IRETRN=1
          IPHT=CELLPN(NC,1)
          INCELL=IWRK(IPHT+9)
          INRING=IWRK(IPHT+12)
C
C         -----------------------
C         CHECK FOR UNUSED LAYERS
C         -----------------------
C
          IS1=NWSET(NC)
C#######################################################################
          NH0 = HPTSEC(INCELL)
          NH9 = HPTSEC(INCELL+1) - 1
          IS2 = ISHFTR(NH9-NH0,2) + 1
      IF (PRINT) PRINT2011,INCELL,WIRMSK(INCELL),IS1,IS2
 2011 FORMAT(' SEARCH OLD CELL',I3,' MASK = Z',Z4,' WIRES GOT =',I3,
     #' HITS AVAILABLE =',I4)
C#######################################################################
          IF(IS1.EQ.16)GOTO2100
C         NH0=FIRST HIT, NH9 = LAST HIT
          NH0=HPTSEC(INCELL)
          NH9=HPTSEC(INCELL+1)-4
          IF(NH0.GT.0)GOTO2010
C ----- ERROR 5 POINTER -VE .. SUSPECT HPTSEC OVERFLOW
          CALLERRORM('PATROL  ',5,NTRAK)
          GOTO2100
 2010     IS2=ISHFTR(NH9-NH0,2)+1
          IF(IS2.LE.IS1)GOTO2100
C
C         -----------------------
C         CHECK ALL UNUSED LAYERS
C         -----------------------
C
C         SET IPHST TO LOWEST R HIT (HIGH IPHT)
CCCC      IPHST=IPHT
          IPHST=HPHT9 - HLDHT + 1
          WIRSET=WIRMSK(INCELL)
          DO 2020 NH1=NH0,NH9,4
          INWIRE=HDATA(NH1)
          INWIRE=ISHFTR(INWIRE,3)
          LAYER=LAND(INWIRE,15)+1
          MAND=hLAND(WIRSET,LAYBIT(LAYER)) ! PMF 28/06/99: hland (N.B.: WIRSET,LAYBIT= I*2)
          IF(MAND.NE.0)GOTO2020
          GOTO8000
 2020     CONTINUE
*** PMF 15/10/99: new label added for a later GOTO statement
*** PMF 28/05/03: this interpretation might be wrong!!!
 2021     CONTINUE
*** PMF (end)
          WIRMSK(INCELL)=WIRSET
C
C==============================================
C==============================================
C  LOOK FOR NEW HITS IN CELLS ADJACENT TO CELLS
C  ALREADY CONTAINING HITS ON THE TRACK
C  IEND = 1 => LOW R.   IEND = 2 => HIGH R.
C==============================================
C==============================================
C
 2100     IRETRN=2
          JNCELL = INCELL
          DO 2195 IEND=1,2
C
C         --------------------------
C         REJECT WALL CROSSING IF NO
C         LAYERS IN THE ADJACENT CELL
C         COULD HAVE BEEN HIT.
C         --------------------------
C
          IPHT=CELLPN(NC,IEND)
          LAYER=IWRK(IPHT) + 1
          NLMIDO=IWRK(IPHT+8)
          LRINDX=1
          IF(NLMIDO.GT.0)LRINDX=2
          NWLIM=HMCH(LAYER,INRING,LRINDX)+1
C######################################################################
      INCELL = JNCELL + ISIGN(1,NLMIDO)
      IF(INCELL.GT.MAXCL(INRING)) INCELL=MINCL(INRING)
      IF(INCELL.LT.MINCL(INRING)) INCELL= MAXCL(INRING)
      IF (PRINT) PRINT2111,IEND,INCELL,LAYER,NLMIDO,NWLIM
 2111 FORMAT('        END',I2,' = CELL',I3,' LAYER',I3,' NLMIDO =',I5,
     #' NWLIM =',I3)
C######################################################################
          IF(NWLIM.LT.1.OR.NWLIM.GT.16)GOTO2195
C
C         ------------------------------------------
C         HAS THE ADJACENT CELL ALREADY BEEN CHECKED
C         ------------------------------------------
C
          INCELL=JNCELL+ISIGN(1,NLMIDO)
          IF(INCELL.GT.MAXCL(INRING))INCELL=MINCL(INRING)
          IF(INCELL.LT.MINCL(INRING))INCELL=MAXCL(INRING)
C######################################################################
      IF (PRINT.AND.CLFLAG(INCELL)) PRINT2121,INCELL
 2121 FORMAT('          CELL',I3,'  ALREADY CHECKED')
C######################################################################
          IF (CLFLAG(INCELL)) GOTO2195
C
C         -----------------------------------
C         ARE THERE ANY HITS IN THIS NEW CELL
C         -----------------------------------
C
          NH0 = HPTSEC(INCELL)
          NH9 = HPTSEC(INCELL+1) - 4
          IF (NH0.GT.0) GOTO2130
          CALLERRORM('PATROL  ',5,NTRAK)
          GOTO2195
 2130     CONTINUE
C######################################################################
      IF (PRINT.AND.(NH9.LT.NH0)) PRINT2122,INCELL,NH0,NH9
 2122 FORMAT('          CELL',I3,' HAS NO HITS',2I10)
C######################################################################
          IF(NH9.LT.NH0)GOTO2195
C
C         -------------------------------
C         REJECT CELL IF END HIT TOO FAR
C         FROM CELL BOUNDARY IN DRIFT SPACE
C         --------------------------------
C
          NLMIDO=IABS(NLMIDO)
          IF(NLMIDO.GT.199)GOTO2180
          DRIFT=HDATA(IWRK(IPHT+1)+3)
          IF(LAYER.GE.9)GOTO2140
          DRIFT=DRIFT*TIMDEL(1,INRING)
          GOTO2150
 2140     DRIFT=DRIFT*TIMDEL(2,INRING)
 2150     S2=SL12(NLMIDO,IEND)
          IS1=LAYER
          IF(IEND.EQ.2)GOTO2160
          S2=-S2
          IF (IS1.NE.1)IS1=IS1-1
          GOTO2170
 2160     IF(IS1.NE.16)IS1=IS1+1
 2170     DRIFT=ABS(DRIFT+S2)
          S2=DSMAX(IS1,INRING,LRINDX)*GFP(11)
C######################################################################
      IF (PRINT.AND.(DRIFT.LT.S2)) PRINT2171,INCELL,DRIFT,S2
 2171 FORMAT('          CELL',I3,' OUTSIDE DSMAX ',2F6.1)
C######################################################################
          IF(DRIFT.LT.S2)GOTO2195
C
C         ---------------------------------
C         MASK OUT DISALLOWED HITS AND
C         CHECK FIT RESIDUAL ON ALLOWED HITS
C         ---------------------------------
C
C         SET START POINTER TO LOWEST R HIT (HIGH IPHT)
 2180     IPHST=HPHT9-HLDHT+1
          WIRSET=WIRMSK(INCELL)
          WIRSET=hLOR(WIRSET,LAYMSK(NWLIM,IEND)) ! PMF 10/06/99: hlor (N.B.: WIRSET,LAYMSK= I*2)
C######################################################################
      IF (PRINT) PRINT2181,WIRMSK(INCELL),LAYMSK(NWLIM,IEND)
 2181 FORMAT('        SEARCH WITH WIRE MASK =Z',Z4,' AND Z',Z4)
C######################################################################
          NH1=NH0
 2185     INWIRE=HDATA(NH1)
          INWIRE=ISHFTR(INWIRE,3)
          LAYER=LAND(INWIRE,15)+1
          MAND=hLAND(WIRSET,LAYBIT(LAYER))! PMF 28/06/99: hland (N.B.: WIRSET,LAYBIT= I*2)
          IF(MAND.NE.0)GOTO2190
          GOTO8000
 2190     NH1=NH1+4
          IF(NH1.LE.NH9)GOTO2185
          WIRMSK(INCELL)=WIRSET
 2195     CONTINUE
 2200     CONTINUE
C
C==============================================
C==============================================
C         LOOK FOR NEW HITS IN MISSING RINGS
C==============================================
C==============================================
C
C
C--------------------------------
C  STEP 1. DETERMINE MISSING RING
C--------------------------------
C
 3000     NRMISS=RGFLAG(1)+RGFLAG(2)+RGFLAG(3)-1
C
C         -------------------------
C         IS THERE A MISSING RING ?
C         -------------------------
C
          IF(NRMISS.GE.7)GOTO4000
          IF (TBIT(KNTRL,28).AND.NRMISS.NE.5)GOTO4000
C         REQUIRED TO AVOID RUTHERFORD 0C4
          LAYLST=0
          IPHST=HPHT9-HLDHT+1
          GOTO(3110,3120,3130,3140,3150,3160),NRMISS
C         BOTH RINGS 2 AND 3 MISSING
 3110     INRING=2
          GOTO3200
C         BOTH RINGS 1 AND 3 MISSING
 3120     INRING=1
          GOTO3200
C         BOTH RINGS 1 AND 2 MISSING
 3130     INRING=2
          GOTO3200
C         ONLY RING 3 MISSING
 3140     INRING=3
          GOTO3200
C         ONLY RING 2 MISSING
 3150     INRING=2
          GOTO3200
C         ONLY RING 1 MISSING
 3160     INRING=1
C
C-----------------------------------------------
C  STEP 2.  DETERMINE CELLS INTERSECTED BY TRACK
C-----------------------------------------------
C
 3200     IRETRN=3
          S1=FSENSW(INRING)
          S2=S1+15*RINCR(INRING)
          IF(.NOT.TBIT(NRMISS,30))GOTO3240
          S1=S2
          S2=FSENSW(INRING)
C
C         ---------------------------------
C         COMPUTE RING INTERSECTION AZIMUTH
C         FOR ENTRY AND EXIT RADII
C         ---------------------------------
C
 3240     S1=(S1**2+DMIDSQ-RADSQ)/(2*S1*DMID)
          S2=(S2**2+DMIDSQ-RADSQ)/(2*S2*DMID)
C#######################################################################
      IF (ABS(S1).LE.1.0.OR.ABS(S2).LE.1.0) GOTO3245
      IF (PRINT) PRINT3241,INRING
 3241 FORMAT(' RING',I2,' NOT REACHED BY TRACK')
 3245  CONTINUE
C#######################################################################
          IF(ABS(S1).LE.1.0)GOTO3250
          IF(ABS(S2).GT.1.0)GOTO4000
          S1=SIGN(1.0,S1)
          GOTO3260
 3250     IF(ABS(S2).GT.1.0)S2=SIGN(1.0,S2)
 3260     S1=ARCOS(S1)
          S2=ARCOS(S2)
C
C         ------------------------
C         HAVE TWO SOLNS OF COURSE
C         DECIDE ON CORRECT ONE.
C         ------------------------
C
          S3=EPSILN-S1
          S1=EPSILN+S1
          S5=ATAN2(YSTR,XSTR)
          S4=ABS(S1-S5)
          IF(S4.GT.PIE)S4=TWOPIE-S4
          S5=ABS(S3-S5)
          IF(S5.GT.PIE)S5=TWOPIE-S5
          IF(S5.LT.S4)GOTO3270
          S2=EPSILN+S2
          GOTO3280
 3270     S1=S3
          S2=EPSILN-S2
 3280     IF(S1.LT.0.)S1=S1+TWOPIE
          IF(S2.LT.0.)S2=S2+TWOPIE
C
C         ---------------------------------
C         COMPUTE CELL RANGE TO BE SEARCHED
C         ---------------------------------
C
          S3=0.2618
          IF(INRING.EQ.3)S3=0.1309
          INCELL=S1/S3
          INCEL2=S2/S3
          INC=ISIGN(1,INCEL2-INCELL)
          IS1=MINCL(INRING)
          IS2=MAXCL(INRING)
          INCELL=IS1+INCELL
          INCEL2=IS1+INCEL2
          IF(INCELL.EQ.IS1.AND.INCEL2.EQ.IS2)INC=-1
          IF(INCELL.EQ.IS2.AND.INCEL2.EQ.IS1)INC=+1
C#######################################################################
      IF (PRINT) PRINT3296,INRING,INCELL,INCEL2,INC,NRMISS
 3296 FORMAT(' SEARCH RING',I2,' CELLS',I3,' TO',I3,' INC/NRMISS =',2I2)
C######################################################################
C
C-----------------------------------
C  STEP 3. LOOP OVER SELECTED CELLS
C-----------------------------------
C
          WIRSET=WIRMSK(INCELL)
C
C         ---------------------------------
C         GET POINTERS TO HITS IN THIS CELL
C         NH0 = FIRST HIT , NH9 = LAST HIT
C         ---------------------------------
C
 3410     NH0=HPTSEC(INCELL)
          NH9=HPTSEC(INCELL+1)-4
C######################################################################
      IF (PRINT.AND.(NH9.LT.NH0)) PRINT3411,INRING,INCELL,NH0,NH9
 3411 FORMAT(' RING',I2,' CELL',I3,' HAS NO HITS',2I10)
C######################################################################
          IF(NH0.GT.0)GOTO3420
          CALLERRORM('PATROL  ',5,NTRAK)
          GOTO3500
 3420     IF(NH9.LT.NH0)GOTO3500
          INCNH1=4
          IF(.NOT.TBIT(NRMISS,30))GOTO3430
          IS1=NH0
          NH0=NH9
          NH9=IS1
          INCNH1=-4
C
C         ---------------------------------
C         LOOP OVER ALL HITS IN CELL INCELL
C         ---------------------------------
C
 3430     NH1=NH0
          LAYLST=0
          LAYTRY=0
C######################################################################
      IF (PRINT) PRINT3431,INCELL,WIRSET
 3431 FORMAT('            CELL',I3,' SET MASK =Z',Z4)
C######################################################################
 3440     INWIRE=HDATA(NH1)
          INWIRE=ISHFTR(INWIRE,3)
          LAYER=LAND(INWIRE,15)+1
          MAND=hLAND(WIRSET,LAYBIT(LAYER)) ! PMF 28/06/99: hland (N.B.: WIRSET,LAYBIT= I*2)
          IF(MAND.NE.0)GOTO3480
          GOTO8000
C         GOOD HIT
 3450     LAYLST=LAYER
          GOTO3480
C         BAD HIT
 3470     IF(NRMISS.NE.5.AND.LAYTRY.GE.IGFP(6))GOTO3510
 3480     IF(NH1.EQ.NH9)GOTO3490
          NH1=NH1+INCNH1
          GOTO3440
C
C         --------------
C         GOTO NEXT CELL
C         --------------
C
 3490     WIRMSK(INCELL)=WIRSET
 3500     IF(INCELL.EQ.INCEL2)GOTO3520
          INCELL=INCELL+INC
          IF(INCELL.GT.MAXCL(INRING))INCELL=MINCL(INRING)
          IF(INCELL.LT.MINCL(INRING))INCELL=MAXCL(INRING)
C         SET MINIMUM LAYER FOR NEW CELL
          WIRSET=WIRMSK(INCELL)
          IF(LAYLST.EQ.0)GOTO3410
          IS1=1
          IF(INC.GT.0)IS1=2
          NWLIM=HMCH(LAYLST,INRING,IS1)+1
          IS2=2
          IF(NH9.LT.NH0)IS2=1
          WIRSET=hLOR(WIRSET,LAYMSK(NWLIM,IS2)) ! PMF 10/06/99: hlor (N.B.: WIRSET,LAYMSK= I*2)
C######################################################################
      IF(PRINT)PRINT3501,INCELL,LAYMSK(NWLIM,IS2),WIRSET
 3501 FORMAT('     "OR" NEXT CELL',I3,' MASK WITH Z',Z4,' GIVING Z',Z4)
C######################################################################
          GOTO3410
C
C         -------------------------
C         END OF LOOP OVER NEW RING
C         -------------------------
C
 3510     WIRMSK(INCELL)=WIRSET
 3520     CONTINUE
C
C==============================================
C==============================================
C         END OF PATROL HIT SEARCH
C==============================================
C==============================================
C
 4000     CONTINUE
C######################################################################
      IF (PRINT) PRINT4001,LAYINT,LAYEXT
 4001 FORMAT(' HIT SEARCH OVER. FOUND INTERP. =',I3,' EXTRAP. =',I3)
      IF(PRINT)CALLPCWORK(0,0,0,1,0)
C######################################################################
C
C         -----------------------------------------
C         LOOK FOR UNWANTED HITS FROM EXTRAPOLATION
C         -----------------------------------------
C
          IPHT=HPHT0
          S4=REND
 4010     S1=0.
          IS2=0
          IS3=IPHT
 4020     IF(IWRK(IPHT+10).NE.0)GOTO4040
          RJETHT=WRK(IPHT+6)
          IF(S1.EQ.0.)GOTO4030
          IF(ABS(RJETHT-S1).GT.GFP(8))GOTO4050
 4030     S1=RJETHT
          IS2=IS2+1
          IF(HLDHT*(RJETHT-S4).LT.0.)GOTO4060
 4040     IPHT=IPHT+HLDHT
          GOTO4020
C         DEMAND IGFP(9) CONSEC HITS
 4050     IF(IS2.LT.IGFP(9))GOTO4055
C         DONT ALLOW BIG HOLES
          IS5=IPHT
 4051     IS5=IS5-HLDHT
          IF(IS5.GE.HPHT0.AND.IS5.LT.HPHT9)GOTO4052
C ----- ERROR 6 PATROL FINDS HOLE BIGGER THAN TRACK. PROBABLE CAUSE
C ----- ERROR 6 IS A BACKTRACE ERROR IN TRACK ELEMENT ORDERING
          CALLERRORM('PATROL  ',6,NTRAK)
          IF(HLDHT.LT.0)HLDHT=-HLDHT
          CALLPCWORK(0,0,0,1,0)
          GOTO7000
 4052     IF(IWRK(IS5+10).NE.0)GOTO4051
          S2=(WRK(IPHT+3)-WRK(IS5+3))**2+(WRK(IPHT+4)-WRK(IS5+4))**2
          S2=SQRT(S2)-GFP(7)
          IF(IWRK(IS5+12).NE.IWRK(IPHT+12))S2=S2-GFP(12)
          IF(S2.LE.0.0)GOTO4056
 4055     IS3=IPHT
 4056     IS2=0
          GOTO4030
C         TRUE PATROL END POINT FOUND
C         MASK OUT UNWANTED HITS
 4060     IF(HLDHT.LT.0)GOTO4090
          IPHT=IS3
 4070     IPHT=IPHT-HLDHT
          IF(IPHT.LT.HPHT0)GOTO4080
C######################################################################
      IF (PRINT) PRINT4071,IPHT,IWRK(IPHT+9),IWRK(IPHT),IWRK(IPHT+8)
     # ,IWRK(IPHT+10)
 4071 FORMAT(' DELETE HIT..IPHT',I5,' CELL',I3,' LAYER',I3,' LRTREL',I5
     #,' IERXYF',I3)
C######################################################################
          IF(IWRK(IPHT+10).NE.0)GOTO4070
          IF(WRK(IPHT+6).GT.REND)LAYEXT=LAYEXT-1
          IWRK(IPHT+10)=2
          GOTO4070
 4080     IPHT=HPHT9-HLDHT+1
          HLDHT=-HLDHT
          S4=RSTR
          GOTO4010
 4090     CONTINUE
          IPHT=IS3
 4091     IPHT=IPHT-HLDHT
          IF(IPHT.GT.HPHT9)GOTO4100
          IF(IWRK(IPHT+10).NE.0)GOTO4091
          IF(WRK(IPHT+6).LT.RSTR)LAYEXT=LAYEXT-1
          IWRK(IPHT+10)=2
C######################################################################
      IF (PRINT) PRINT4071,IPHT,IWRK(IPHT+9),IWRK(IPHT),IWRK(IPHT+8)
     # ,IWRK(IPHT+10)
C######################################################################
          GOTO4091
 4100     HLDHT=-HLDHT
C######################################################################
      IF (PRINT) PRINT4101,LAYINT,LAYEXT
 4101 FORMAT(' END OF DELETE.   FOUND INTERP. =',I3,' EXTRAP. =',I3)
      IF (PRINT) CALLPCWORK(0,0,0,1,0)
C######################################################################
C
C         -------------------------------------
C         CALL REFIT WITH NEW HITS IF NECESSARY
C         -------------------------------------
C
 5000     IF(TBIT(KNTRL,31))GOTO7000
C         REFIT IF HAVE MANY MORE HITS
          IF(LAYINT.GE.IGFP(10))GOTO5010
          IF(LAYEXT.GT.0)GOTO5010
          GOTO7000
 5010     NREFIT=NREFIT+1
          IF(NREFIT.LE.IGFP(13))GOTO5015
C ----- ERROR 8 PATROL HAS CALLED TOO MANY REFITS .. END PATROL
                   CALLERRORM('PATROL  ',8,NTRAK)
          GOTO7000
 5015     IXYF1=IXYF(1)
          IXYF(1)=1
C######################################################################
      IF (PRINT) PRINT5011
 5011 FORMAT(' CALL XYFIT ')
C######################################################################
          RSTRSV=RSTR+5.
          RENDSV=REND-5.
          CALLXYFIT
          IXYF(1)=IXYF1
C         REPEAT PATROL IF FIT WAS GOOD
          IF(WRK(HPTR0+22).LE.GFP(2))GOTO510
C
C         ----------------------------------
C         REFIT WAS BAD. MASK OUT PATROLLED
C         HITS AND REINSTATE ORIGINAL FIT
C         ----------------------------------
C
          IWRK(HPTR0+47)=LOR (IWRK(HPTR0+47),BIT26)
          IWRK(HPTR0+47)=LAND(IWRK(HPTR0+47),BIT31N)
          IPHT=HPHT0+10
 5020     IF(IWRK(IPHT).EQ.1)IWRK(IPHT)=0
          IF(IABS(IWRK(IPHT-2)).LT.1000)GOTO5021
          RJETHT=WRK(IPHT-4)
          IF(RJETHT.GE.RSTRSV.AND.RJETHT.LE.RENDSV)GOTO5021
          IWRK(IPHT)=2
 5021     IPHT=IPHT+HLDHT
          IF(IPHT.LT.HPHT9)GOTO5020
C######################################################################
      IF (PRINT) PRINT4201
 4201 FORMAT(' PATROL RE-FIT WAS BAD. DELETE PATROLLED HITS AND REFIT')
C######################################################################
          IXYF1=IXYF(1)
          IXYF(1)=1
          CALLXYFIT
          IXYF(1)=IXYF1
C
C         ----------------------
C         FINAL EXIT FROM PATROL
C         ----------------------
C
 7000     IWRK(HPTR0+47)=ISTORY
C######################################################################
      IF (PRINT) PRINT4801,LAYINT
 4801 FORMAT(1X,10('#'),' EXIT PATROL. LAYINT =',I3)
C#######################################################################
          RETURN
C
C==============================================
C==============================================
C         CALCULATE RESIDUAL OF TEST HIT AND
C         LOAD INTO HIT ARRAY IF ACCEPTABLE
C==============================================
C==============================================
C
C         -------------------------
C         COMPUTE HIT LABEL POINTER
C         IS1=0 => HIT NOT ON TREL
C         -------------------------
C
 8000     IF(NH1.GT.0)GOTO8001
          CALLERRORM('PATROL  ',5,NTRAK)
          GOTO8750
 8001     NH=ISHFTR((NH1-HPTSEC(1)),1)+HPHL0
          IS1=HWRK(NH)
          IS1=LAND(IS1,MSKLBL)
          IF(IS1.EQ.0)GOTO8010
          IF(TBIT(KNTRL,29))GOTO8750
          GOTO8020
 8010     IF(TBIT(KNTRL,25))GOTO8750
 8020     CONTINUE
C
C         ----------------------------------
C         SET UP CELL AND LAYER CONSTANTS.
C         SKIP IF SAME WIRE ETC AS LAST TIME
C         ----------------------------------
C
          IF(INWIRE.EQ.JETWR)GOTO8120
          JETWR=INWIRE
C         NEW LAYER BEING TRIED .. INCREMENT LAYTRY
          LAYTRY=LAYTRY+1
          IF(INCELL.EQ.JETCL)GOTO8100
          JETCL=INCELL
          TRLORX=TRMATS(INCELL,1)
          TRLORY=TRMATC(INCELL,1)
          SINLOR=SINDRI(INCELL,1)
          COSLOR=COSDRI(INCELL,1)
          DRFVEL=DRIVEL(INCELL,1)
C         CORRECTION CONSTANTS FOR INCELL
          IPJCOR = ICALIB(5) + INCELL
          CCST01 = ACALIB(IPJCOR     )
          CCST02 = ACALIB(IPJCOR+  96)
          CCST11 = ACALIB(IPJCOR+ 192)
          CCST12 = ACALIB(IPJCOR+ 288)
          CCST21 = ACALIB(IPJCOR+ 384)
          CCST22 = ACALIB(IPJCOR+ 480)
          CCST51 = ACALIB(IPJCOR+ 576) * 10.
          CCST61 = ACALIB(IPJCOR+ 768) * 10.
          CCST81 = ACALIB(IPJCOR+1152)
          RSENSW=FSENSW(INRING)
          DR0=RINCR(INRING)
          IS1=INCELL-24*(INRING-1)
          IF(INRING.EQ.3)GOTO8030
          DXWR=DIRWR1(IS1,1)
          DYWR=DIRWR1(IS1,2)
          GOTO8040
 8030     DXWR=DIRWR3(IS1,1)
          DYWR=DIRWR3(IS1,2)
 8040     SWDEPX=SWDEPL*DXWR
          SWDEPY=SWDEPL*DYWR
C
C         ------------------------------
C         COMPUTE (X,Y,R) OF SIGNAL WIRE
C         ------------------------------
C
 8100     LAYERJ=LAYER-1
          RWIRE=RSENSW+LAYERJ*DR0
          IF(TBIT(LAYERJ,31))GOTO8110
          XWIRE=RWIRE*DXWR-SWDEPY
          YWIRE=RWIRE*DYWR+SWDEPX
          GOTO8120
 8110     XWIRE=RWIRE*DXWR+SWDEPY
          YWIRE=RWIRE*DYWR-SWDEPX
 8120     CONTINUE
          IF(HPHT0.GT.HPHTLM)GOTO8200
C ----- ERROR 2 NOT ENOUGH ROOM IN CWORK
          CALLERRORM('PATROL  ',2,NTRAK)
          GOTO7000
C
C         --------------------------------
C         COMPUTE DRIFT DISTANCE FROM TIME
C         NOTE THAT DRIFT DISTANCE IS +VE
C         --------------------------------
C
 8200     IDRIFT=HDATA(NH1+3)
          DRIFT=IDRIFT*DRFVEL
          IF(HEAD(18).LE.0)DRIFT=DRIFT+0.5*DRFVEL
C
C         --------------------------------
C         DRIFT PATH DISPERSION CORRECTION
C         --------------------------------
C
          XJETHT=XWIRE-TRLORX*DRIFT-XMID
          YJETHT=YWIRE-TRLORY*DRIFT-YMID
          XJET2 =XWIRE+TRLORX*DRIFT-XMID
          YJET2 =YWIRE+TRLORY*DRIFT-YMID
          S3=SINLOR*DXWR-COSLOR*DYWR
          S4=COSLOR*DXWR+SINLOR*DYWR
          XX = ABS(S3*XJETHT+S4*YJETHT)
          IF(XX.NE.0.)GOTO1350
          CALLERRORM('PATROL  ',9,NTRAK)
C ----- ERROR 9  FAILED TO CALCULATE DRIFT PATH DISPERSION
C ----- ERROR 9  CORRECTION. ASSUME IT TO BE MAXIMUM
          BETA = 1.1
          GOTO1355
 1350     BETA = RAD/XX
          IF(BETA.LT.1.0)BETA=1.0
          IF(BETA.GT.1.2)BETA=1.1
 1355     XX=ABS(S3*XJET2+S4*YJET2)
          IF(XX.NE.0.) GOTO1360
          CALLERRORM('PATROL  ',9,NTRAK)
          BETA2 = 1.1
          GOTO1365
 1360     BETA2=RAD/XX
          IF(BETA2.LT.1.0)BETA2=1.0
          IF(BETA2.GT.1.1)BETA2=1.1
 1365     CONTINUE
C
          SCALE=DRIFT
          IF(DRIFT.GT.4.)SCALE=4.
          DRIFT1=DRIFT+(BETA-1.)*SCALE
          DRIFT2=DRIFT+(BETA2-1.)*SCALE
C
C         --------------------------
C         EDGE WIRE FIELD DISTORTION
C         --------------------------
C
          IF(LAYER.GE.3)GOTO1370
          DRIFT1=DRIFT1*(1.-CCST11*(LAYER-3)**2)
          DRIFT2=DRIFT2*(1.-CCST12*(LAYER-3)**2)
          GOTO1380
 1370     IF(LAYER.LE.12)GOTO1380
          DRIFT1=DRIFT1*(1.-CCST21*(LAYER-12)**2)
          DRIFT2=DRIFT2*(1.-CCST22*(LAYER-12)**2)
 1380     CONTINUE
C         ----------------------------------
C         DRIFT VELOCITY VARIATION NEAR WIRE
C         ----------------------------------
C
          IF(DRIFT1.LT.CAB(4))DRIFT1=DRIFT1+CAB(5)*(DRIFT1-CAB(4))**2
          IF(DRIFT2.LT.CAB(4))DRIFT2=DRIFT2+CAB(5)*(DRIFT2-CAB(4))**2
C         ---------------------
C         RESET -VE DRIFT TIMES
C         ---------------------
C
          IF(DRIFT1.LT.0.)DRIFT1=0.05
          IF(DRIFT2.LT.0.)DRIFT2=0.05
C
C         --------------------------------
C         COMPUTE X,Y COORDINATES FOR BOTH
C         LEFT (-VE) AND RIGHT (+VE) SOLNS
C         --------------------------------
C
          XJETHT=XWIRE-TRLORX*DRIFT1
          YJETHT=YWIRE-TRLORY*DRIFT1
          XJET2 =XWIRE+TRLORX*DRIFT2
          YJET2 =YWIRE+TRLORY*DRIFT2
C
C         --------------------------------
C         COMPUTE RESIDUALS FOR BOTH L/R
C         SOLUTIONS AND TAKE THE SMALLEST
C         --------------------------------
C
          CHIXYF=(XJETHT-XMID)**2+(YJETHT-YMID)**2
          CHIXYF=(CHIXYF-RADSQ)/RAD2
          S2=(XJET2-XMID)**2+(YJET2-YMID)**2
          S2=(S2-RADSQ)/RAD2
          LRTREL=-1
          DRIFT=DRIFT1
          IF(ABS(CHIXYF).LT.ABS(S2))GOTO8410
          LRTREL=1
          DRIFT=DRIFT2
          XJETHT=XJET2
          YJETHT=YJET2
          CHIXYF=S2
          BETA=BETA2
C
C         ----------------------
C         DETERMINE ACCEPT CODE
C         ----------------------
C
 8410     S2=ABS(CHIXYF)
          IERXYF=3
          IF(S2.GT.CODE2)GOTO8750
          IF(S2.GT.CODE0)GOTO8420
          IERXYF=0
          LAYTRY=0
          GOTO8430
 8420     IERXYF=2
 8430     CONTINUE
C
C         --------------------------------
C         COMPUTE RADIAL COORDINATE
C         TEST AGAINST USER DEFINED LIMITS
C         --------------------------------
C
          RJETHT=DRIFT/RWIRE
          RJETHT=RJETHT*(0.5*RJETHT-LRTREL*SINLOR)
          RJETHT=RWIRE*(1+RJETHT-.5*RJETHT**2)
          IF(RJETHT.LT.RMIN.OR.RJETHT.GT.RMAX)GOTO8750
C
C         -----------------------------
C         SET IERZRF = 16 IF THERE IS A
C         SECOND HIT WITHIN THE SOFTWARE
C         DOUBLE HIT RESOLUTION IXYF(18)
C         -----------------------------
C
          IERZRF=0
CC--CC    CLOSEST HIT WITH LOWER DRIFT TIME
CC--CC    IS2=NH1-4
CC--CC    IF(IS2.LT.NH0)GOTO8510
CC--CC    IS1=HDATA(IS2)
CC--CC    IS1=ISHFTR(IS1,3)
CC--CC    IF(IS1.NE.INWIRE)GOTO8510
CC--CC    IS2=IDRIFT-HDATA(IS2+3)
CC--CC    IF(IABS(IS2).LE.IXYF(18))IERZRF=16
CC--CC    CLOSEST HIT WITH HIGHER DRIFT TIME
C8510     IS2=NH1+4
CC--CC    IF(IS2.GT.NH9)GOTO8520
CC--CC    IS1=HDATA(IS2)
CC--CC    IS1=ISHFTR(IS1,3)
CC--CC    IF(IS1.NE.INWIRE)GOTO8520
CC--CC    IS2=IDRIFT-HDATA(IS2+3)
CC--CC    IF(IABS(IS2).LE.IXYF(18))IERZRF=16
C
C         ----------------------------------
C         COMPUTE Z-COORDINATE. IF EITHER Z-
C         AMPLITUDE IS <= 0 SET IERZRF = 16
C         ----------------------------------
C
 8520     IS1=HDATA(NH1+1)
          IS2=HDATA(NH1+2)
          IF(IS2.LE.0.OR.IS1.LE.0)GOTO8540
          ZJETHT=IS2+IS1
          ZJETHT=.5*ZAL*FLOAT(IS2-IS1)/ZJETHT
          GOTO8600
 8540     IERZRF=16
          ZJETHT=0
C
C         -----------------------------------
C         START SEARCH FOR INSERTION POINT AT
C         IPHT AND WORK DOWN (TO HIGHER R)
C         UNTIL A HIT OF GREATER R IS FOUND.
C         -----------------------------------
C
 8600     IF(IWRK(IPHST+1).EQ.NH1)GOTO8640
          IF(WRK(IPHST+6).GE.RJETHT)GOTO8610
          IPHST=IPHST-HLDHT
          IF(IPHST.GE.HPHT0)GOTO8600
C
C         -----------------------------------------
C         IPHST POINTS TO FIRST HIT WITH R > RJETHT
C         MAKE ROOM FOR NEW HIT IN HIT BANK
C         -----------------------------------------
C
 8610     LRTREL=1000*LRTREL
          IS1=4*(IPHST-HPHT0+HLDHT)
          HPHT0=HPHT0-HLDHT
          IF(IS1.NE.0)CALLMVCL(WRK(HPHT0),0,IWRK(HPHT0),LNBYTE,IS1)
          CALLMVCL(WRK(IPHST),0,ADWRK(1),0,LNBYTE)
C         IPHST NOW POINTS TO THE HIT JUST INSERTED
C
C         -----------------------------------
C         IF MEMORY OF CELL POINTERS IS STILL
C         REQUIRED THEY MUST NOW BE RESET TO
C         ALLOW FOR THE NEWLY INSERTED HIT
C         -----------------------------------
C
          IF(IRETRN.EQ.3)GOTO8700
          DO 8630 N=1,NCELLS
          IF(CELLPN(N,2).LE.IPHST)CELLPN(N,2)=CELLPN(N,2)-HLDHT
          IF(N.EQ.NC)GOTO8630
          IF(CELLPN(N,1).LE.IPHST)CELLPN(N,1)=CELLPN(N,1)-HLDHT
 8630     CONTINUE
          GOTO8700
 8640     IS1=IABS(IWRK(IPHST+8))
          IF(IS1.LT.1000)IS1=IS1+1000
          LRTREL=LRTREL*IS1
          CALLMVCL(WRK(IPHST),0,ADWRK(1),0,LNBYTE)
C
C         ---------------
C         GOOD HIT RETURN
C         ---------------
C
 8700     WIRSET=hLOR(WIRSET,LAYBIT(LAYER)) ! PMF 10/06/99: hlor (N.B.: WIRSET,LAYBIT= I*2)
C######################################################################
      LL = ISIGN(LAYERJ,LRTREL)
      IF (PRINT) PRINT8701,INCELL,LL,BETA,RJETHT,CHIXYF,IERXYF,IPHST
 8701 FORMAT(' CELL',I3,' LAYER',I3,' BETA',F5.2,' R=',F6.1,
     #' RES=',F7.2,I3,' ---ACCEPT---',I5)
C######################################################################
          IF(IERXYF.NE.0)GOTO8750
          IF(RJETHT.GT.REND.OR.RJETHT.LT.RSTR)GOTO8710
          LAYINT=LAYINT+1
          GOTO8720
 8710     LAYEXT=LAYEXT+1
*** PMF 15/10/99:new GOTO label introduced behind the 'END DO'-CONTINUE statement at label 2020
* 8720     GOTO(2020,2190,3450),IRETRN
 8720     GOTO(2021,2190,3450),IRETRN
*** PMF (end)
C
C         ---------------
C         BAD  HIT RETURN
C         ---------------
C
 8750     CONTINUE
C######################################################################
      IF (IERXYF.LE.2) GOTO8755
      LL = ISIGN(LAYERJ,LRTREL)
      IF (PRINT) PRINT8751,INCELL,LL,BETA,RJETHT,CHIXYF,IERXYF
 8751 FORMAT(' CELL',I3,' LAYER',I3,' BETA',F5.2,' R=',F6.1,
     #' RES=',F7.2,I3,' ---REJECT---')
 8755  CONTINUE
C######################################################################
*** PMF 15/10/99: new GOTO label introduced behind the 'END DO'-CONTINUE statement at label 2020
*          GOTO(2020,2190,3470),IRETRN
          GOTO(2021,2190,3470),IRETRN
*** PMF (end)
C ----- ERROR 3 ILLEGAL RETURN VALUE
          CALLERRORM('PATROL  ',3,NTRAK)
          RETURN
      END
