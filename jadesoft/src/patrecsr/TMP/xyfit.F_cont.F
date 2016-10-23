C   15/02/81 610020943  MEMBER NAME  XYFIT    (PATRECSR)    FORTRAN
          SUBROUTINE XYFIT
          IMPLICIT INTEGER*2(H)
#include "cworkpr.for"
#include "cworkeq.for"
#include "cpatlm.for"
#include "cxyfit.for"
C
C
C-------------------------------------------------------------------
C
C       ERROR IN PARFIT: DIVISION BY ZERO IN CASE OF CURV. = 0.0
C       CURV. SET TO 1.E-8 IN THIS CASE (P. STEFFEN, 02/10/86)
C-------------------------------------------------------------------
C
C       ------------------ SUBROUTINE XYFIT   ----------------
C       ---- G.F.PEARCE .. LAST UPDATE : 1200 ON  4/02/81 ----
C
C    ROUTINE TO PERFORM A FAST PARABOLA OR CIRCLE FIT TO A GIVEN TRACK
C    IN REAL COORDINATE SPACE. THE TRACK INPUT IS PROVIDED VIA THE
C    WORKING COMMON CWORK. THIS IS BEST DONE BY THE ROUTINE FXYZ.
C
C    THE FIT RESULT IS RETURNED AS A SINGLE TRACK TRACK BANK AGAIN IN
C    THE WORKING COMMON CWORK. (REFER TO POINTERS HPTR0,HPTR9,HLDTR)
C
C    FIT HISTORY IS RETURNED AS THE FOLLOWING BIT STRUCTURE IN THE TRACK
C    HISTORY WORD :
C    BIT 31   ...  FINAL FIT WAS GOOD/BAD (BIT OFF/ON)
C    BIT 30   ...  L/R AMBIGUITIES STILL UNCERTAIN AFTER FIT (BIT ON)
C    BIT 29   ...  FIT WAS TRIED WITHOUT WORST TRACK ELEMENT (BIT ON)
C    BIT 28   ...  FIT WAS TRIED USING ONLY THE BEST TRACK ELEMENT
C    BIT 27   ...  PATROL FOUND GREATER THAN 3 NEW HITS THAT WERE ALSO
C             ...  ON ANOTHER TRACK.
C    BIT 26   ...  PATROL CALLED A RE-FIT WHICH FAILED
C
C    CONTROLLING XYFIT.
C    =================
C                     CONTROL OF THE PARAMETERS AND LIMITS USED BY
C    THIS PROGRAM IS ACHIEVED THROUGH THE ARRAY XYF IN COMMON CPATLM.
C    FOR DETAILS SEE THE INITIALISING ROUTINE INPATR.
C
C-------------------------------------------------------------------
C
C
          NTRAK = HPWRK(30)
C#######################################################################
      PRINT = .FALSE.
      IF (NTRAK.EQ.IXYF(20)) PRINT=.TRUE.
      IF (IXYF(20).LT.0.AND.NTRAK.GE.IABS(IXYF(20))) PRINT=.TRUE.
      IF (TBIT(IXYF(1),28)) PRINT=.FALSE.
      IF (PRINT) PRINT9,NTRAK,IXYF(1)
 9    FORMAT(1X,39('=')/' XYFIT - TRACK',I4,' XYF(1) = ',Z8/1X,39('='))
C#######################################################################
C
C         ===================================
C         ===================================
C         INITIALISATION AND TESTS FOR ERRORS
C         ===================================
C         ===================================
C
          ISTORY=0
          KNTROL=IXYF(1)
          SETFLG=.TRUE.
          NHIT=(HPHT9-HPHT0+1)/HLDHT
          IF(NHIT.GE.4.AND.NHIT.LE.80)GOTO20
C ERRORM 1 ---------- NOT ENOUGH HITS TO FIT --------------------------
          CALLERRORM('   XYFIT',1,NTRAK)
C         ----------------------
C         ABANDON FIT .. GIVE UP
C         ----------------------
 10       RMSFIT=1000000.
          ISTORY=LOR(ISTORY,1)
          GOTO990
C
C         =======================================
C         =======================================
C         FIT PARABOLA AND DETERMINE L/R SOLUTION
C         =======================================
C         =======================================
C
 20       CALLLRFIT
          IF(FATAL)GOTO10
          IF(.NOT.BADFIT.OR.NBAD.EQ.0)GOTO700
          IF(TBIT(KNTROL,31))GOTO700
C
C         =========================================
C         =========================================
C         TRY A FIT WITHOUT THE WORST TRACK ELEMENT
C         =========================================
C         =========================================
C
          CALLTRLCHI
C         ------------------------
C         FIND WORST TRACK ELEMENT
C         (DEMAND > 5 POINTS LEFT)
C         ------------------------
          IF(NTRTOT.LE.1)GOTO900
          CHI=0
          NTRMSK=0
          DO 420 N=1,NTRTOT
          CHITR(N)=CHITR(N)/NHITIN(N)
          IF(NHIT-NHITIN(N).LE.4)GOTO420
          IF(CHITR(N).LE.CHI)GOTO420
          CHI=CHITR(N)
          NTRMSK=JTR(N)
 420      CONTINUE
          IF(TBIT(KNTROL,27))GOTO500
C#######################################################################
      IF (PRINT) PRINT421,NTRMSK
 421  FORMAT(' ****** MASK OUT TRACK ELEMENT',I3)
C#######################################################################
C         ----------------------------
C         MASK OUT WORST TRACK ELEMENT
C         (NTRBAD) AND REPEAT THE FIT.
C         ----------------------------
          IF(NTRMSK.EQ.0)GOTO900
          IPHIT=HPHT0+10
430       ISP1=IABS(IWRK(IPHIT-2))
          IF(ISP1.EQ.NTRMSK.OR.ISP1.EQ.1000)GOTO440
          IWRK(IPHIT)=0
          GOTO450
 440      IWRK(IPHIT)=9
 450      IPHIT=IPHIT+HLDHT
          IF(IPHIT.LT.HPHT9)GOTO430
C         -----------------
C         CALL PARABOLA FIT
C         -----------------
          ISTORY=LOR(ISTORY,4)
          CALLPARFIT
          IF(FATAL)GOTO10
          IF(.NOT.BADFIT)GOTO600
C
C         ===============================
C         ===============================
C         TRY A FIT WITH ONLY THE LONGEST
C         ===============================
C         ===============================
C
 500      IF(NTRTOT.LE.2)GOTO900
          ISP1=0
          DO 530 N=1,NTRTOT
          IF(NHITIN(N)-ISP1)530,510,520
 510      IF(CHITR(N).GT.CHI)GOTO530
 520      CHI=CHITR(N)
          ISP1=NHITIN(N)
          NTRMSK=JTR(N)
 530      CONTINUE
C#######################################################################
      IF (PRINT) PRINT531,NTRMSK
 531  FORMAT(' ****** FIT ONLY TRACK ELEMENT ',I4)
C#######################################################################
          IF (ISP1.LE.4) GOTO900
C         ----------------------------------
C         MASK OUT ALL TRACK ELEMENTS EXCEPT
C         THE LONGEST (NTRGUT) AND REFIT.
C         ----------------------------------
          IPHIT=HPHT0+10
 540      ISP1=IABS(IWRK(IPHIT-2))
          IF(ISP1.EQ.NTRMSK)GOTO550
          IWRK(IPHIT)=9
          GOTO560
 550      IWRK(IPHIT)=0
 560      CONTINUE
          IPHIT=IPHIT+HLDHT
          IF(IPHIT.LT.HPHT9)GOTO540
          ISTORY=LOR(ISTORY,8)
          CALLPARFIT
          IF(FATAL)GOTO10
          IF(BADFIT)GOTO900
C
C         =======================================
C         =======================================
C         REFIT IF SOME BAD HITS WERE RECOLLECTED
C         =======================================
C         =======================================
C
 600      IF(NEXTRA.LE.IGFP(10))GOTO700
C#######################################################################
      IF (PRINT) PRINT601,NEXTRA
 601  FORMAT(' ****** REFIT WITH EXTRA POINTS .. ',I5)
C#######################################################################
          CALLPARFIT
          IF(FATAL)GOTO900
          IF(.NOT.BADFIT)GOTO600
C
C         ================================
C         ================================
C         CONVERT PARABOLA FIT INTO CIRCLE
C         ================================
C         ================================
C
 700      IF(TBIT(KNTROL,30))GOTO800
C#######################################################################
      IF (PRINT.AND.ABS(CURV).LT.XYF(6))PRINT701,CURV,XYF(6)
 701  FORMAT(' ****** NO CIRCLE FIT .. LOW CURVATURE ',2E11.4)
C#######################################################################
          IF(ABS(CURV).LT.XYF(6))GOTO800
C#######################################################################
      IF (PRINT.AND.CHIFIT.GT.XYF(10))PRINT702,CHIFIT,XYF(10)
 702  FORMAT(' ****** NO CIRCLE FIT .. BAD PARAB CHI ',2E11.4)
C#######################################################################
          IF(CHIFIT.GT.XYF(10))GOTO800
C         ----------
C         FIT CIRCLE
C         ----------
          CALLCIRFIT
          IF(.NOT.FATAL)CALLRESIDS
C
C         ===================================
C         ===================================
C         RECALCULATE ABERRATIONS IF REQUIRED
C         ===================================
C         ===================================
C
 800      IF(.NOT.TBIT(KNTROL,29))GOTO900
          IF(NGOOD.LE.3)GOTO900
          CALLNEWINC
C
C         ==================================
C         ==================================
C         LOAD RESULTS INTO CWORK TRACK BANK
C         ==================================
C         ==================================
C
 900      NPTYPE=NMPROG
          RMSFIT=SQRT(CHIFIT)
          NPUSED=NPTS
          COEFF2=XMID+C2*COSTH-C3*SINTH
          COEFF3=YMID+C2*SINTH+C3*COSTH
          X1=XFIRST-C2
          X2=XLAST-C2
          GOTO(910,950),NMPROG
C         -------------------------
C         COMPUTE CIRCLE PARAMETERS
C         CURVATURE, R0-R, EPSILON
C         -------------------------
 910      CURVT=1.0 / C4
          COEFF4=ATAN2(COEFF3,COEFF2)
          COEFF2=SQRT(COEFF2**2+COEFF3**2)-ABS(C4)
          COEFF3=COEFF4
          COEFF1=ABS(CURVT)
C         CIRCLE END POINTS AND GRADIENTS
          Y1=C4**2-X1**2
          IF(Y1.LT.0.) PRINT9991,Y1,NREC,HRUN,HEV,HPWRK(30)
 9991   FORMAT(' XYFIT .. -VE SQRT ON Y1',E11.4,4I10)
          IF(Y1.LT.0.)RMSFIT=1000000.
          IF(Y1.LT.0.)Y1=1.
          Y1=SIGN(SQRT(Y1),CURVT)
          SP1=-X1/Y1
          Y1=Y1+C3
          Y2=C4**2-X2**2
         IF(Y2.LT.0.)PRINT9992,Y2,NREC,HRUN,HEV,HPWRK(30)
 9992   FORMAT(' XYFIT .. -VE SQRT ON Y2',E11.4,4I10)
          IF(Y2.LT.0.)RMSFIT=1000000.
          IF(Y2.LT.0.)Y2=1.
          Y2=SIGN(SQRT(Y2),CURVT)
          SP2=-X2/Y2
          Y2=Y2+C3
          GOTO980
C         --------------------------------------
C         COMPUTE PARABOLA PARAMETERS
C         Y1 = C4 * X1**2
C         WHERE X1 = (X-C2)*COSTH + (Y-C3)*SINTH
C         AND   Y1 = (Y-C3)*COSTH - (X-C2)*SINTH
C         --------------------------------------
 950      COEFF1=ATAN2(SINTH,COSTH)
C         COEFF2=COEFF2
C         COEFF3=COEFF3
          COEFF4=C4
          CURVT=-2*C4
C         PARABOLA END POINTS
          Y1=C4*X1**2+C3
          Y2=C4*X2**2+C3
C         PARABOLA END DIRECTIONS
          SP1=-CURVT*X1
          SP2=-CURVT*X2
 980      CONTINUE
C         -----------------
C         FITTED END POINTS
C         -----------------
          XSTR=XFIRST*COSTH-Y1*SINTH+XMID
          YSTR=XFIRST*SINTH+Y1*COSTH+YMID
          RSTR=SQRT(XSTR**2+YSTR**2)
          XEND=XLAST*COSTH-Y2*SINTH+XMID
          YEND=XLAST*SINTH+Y2*COSTH+YMID
          REND=SQRT(XEND**2+YEND**2)
C         -----------------
C         DIRECTION COSINES
C         -----------------
          CS1 = 1. / SQRT(SP1**2 + 1.)
          SN1 = CS1 * SP1
          DXSTR=CS1*COSTH-SN1*SINTH
          DYSTR=CS1*SINTH+SN1*COSTH
          DRSTR=0
          CS2 = 1. / SQRT(SP2**2 + 1.)
          SN2 = CS2 * SP2
          DXEND=CS2*COSTH-SN2*SINTH
          DYEND=CS2*SINTH+SN2*COSTH
          DREND=0.
C         ------------------------
C         START AND END CURVATURES
C         ------------------------
          CURV1=CURVT
          CURV2=CURVT
          DCURV=0.
C         ------------------
C         TYPE OF END POINTS
C         ------------------
          TYPE1=0
          TYPE2=0
C         -------------------------
C         LOAD OUTPUT BANK IN CWORK
C         -------------------------
 990      IPHIT=HPTR0+3
          CALLMVCL(WRK(IPHIT),0,ADWRK(1),0,100)
          IPHIT=HPTR0+47
          IF(BADFIT)ISTORY=LOR(ISTORY,1)
          IWRK(IPHIT)=LOR(IWRK(IPHIT),ISTORY)
C#######################################################################
      IF (PRINT) CALL PCWORK(0,0,0,0,1)
C#######################################################################
C
C         ==========================================
C         ==========================================
C         COMPUTE TRACK ELEMENT CHI**2 FOR BACKTRACE
C         ==========================================
C         ==========================================
C
          IF(TBIT(KNTROL,28))CALLTRLCHI
          IF(TBIT(KNTROL,28))CALL MVCL(ADWRK(81),0,JTR(1),0,80)
          RETURN
          END
          SUBROUTINE LRFIT
          IMPLICIT INTEGER*2(H)
C
C
C
C
C
C=======================================================================
C=======================================================================
C=======================================================================
C=======================================================================
C
C               DETERMINE LEFT/RIGHT AMBIGUITY BY FITTING
C
C=======================================================================
C=======================================================================
C=======================================================================
C=======================================================================
C
C
C
C
C
#include "cpatlm.for"
#include "cworkpr.for"
#include "cworkeq.for"
#include "cxyfit.for"
          LOGICAL*1 LRDONE
C         --------------
C         FIT SOLUTION 1
C         --------------
          SETFLG=.FALSE.
          CALLPARFIT
          SETFLG=.TRUE.
          IF(FATAL)RETURN
 6010     IF(HPHT0A.LE.0)RETURN
CONTROL BIT 26 IS TESTED IN FXYZ
          LRDONE=.FALSE.
          CHILST=CHIFIT
C         --------------
C         FIT SOLUTION 2
C         --------------
          LPHT0=HPHT0
          LPHT9=HPHT9
          HPHT0=HPHT0A
          HPHT9=HPHT9A
          CALLPARFIT
          HPHT0=LPHT0
          HPHT9=LPHT9
          IF(FATAL)GOTO6070
          IF(CHILST.LT.CHIFIT)GOTO6040
C         ---------------
C         KEEP SOLUTION 2
C         ---------------
          IPHIT=HPHT0A
          ISP1=NHIT*HLDHT*4
          CALLMVCL(WRK(LPHT0),0,WRK(IPHIT),0,ISP1)
          IF((CHILST-CHIFIT).GT.XYF(7)*CHIFIT)LRDONE=.TRUE.
          GOTO6060
C         ---------------
C         KEEP SOLUTION 1
C         ---------------
 6040     IF((CHIFIT-CHILST).GT.XYF(7)*CHILST)LRDONE=.TRUE.
          CHILST=CHIFIT
          CALLPARFIT
          IF(FATAL)GOTO6080
C         ---------------------
C         IS L/R NOW RESOLVED ?
C         ---------------------
 6060     IF(.NOT.LRDONE)ISTORY=LOR(ISTORY,2)
C#######################################################################
      IF(LRDONE.AND.PRINT) PRINT6061,CHILST,CHIFIT
 6061 FORMAT(' ****** L/R SOLNS FITTED SUCCESSFULLY ..',2F10.2)
      IF(.NOT.LRDONE.AND.PRINT)PRINT6062,CHILST,CHIFIT
 6062 FORMAT(' ****** L/R SOLNS FITTED UNSUCCESSFULLY ..',2F10.2)
C#######################################################################
          HPHT0A=-HPHT0A
          RETURN
C         --------------------
C         SOLN 2 FIT WAS FATAL
C         --------------------
 6070     FATAL=.FALSE.
C ERRORM 5 ------------ FITTING SOLN 1 IN LRFIT WAS FATAL -------------
          CALLERRORM('   XYFIT',5,NTRAK)
          CALLRESIDS
          GOTO6060
C         -------------------------
C         RECALLING FIT 1 WAS FATAL
C         -------------------------
 6080     CONTINUE
C ERRORM 6 ---------- RECALLING SOLN 1 IN LRFIT WAS FATAL -------------
          CALLERRORM('   XYFIT',6,NTRAK)
          RETURN
          END
          SUBROUTINE PARFIT
          IMPLICIT INTEGER*2(H)
C
C
C
C
C
C=======================================================================
C=======================================================================
C=======================================================================
C=======================================================================
C
C               FORM PARABOLIC LEAST SQUARES FIT TO TRACK
C
C=======================================================================
C=======================================================================
C=======================================================================
C=======================================================================
C
C
C
C
C
#include "cworkpr.for"
#include "cworkeq.for"
#include "cpatlm.for"
#include "cxyfit.for"
          FATAL=.FALSE.
C         ---------------------------
C         FIND INNER AND OUTER POINTS
C         ON TRACK TO DETERMINE SLOPE
C         ---------------------------
C         OUTER POINT (BEGINNING OF ARRAY)
          IS3=0
          IS1=0
          IPHIT=HPHT0+10
 5010     IF(IWRK(IPHIT).NE.0)GOTO5020
          IS3=IS3+1
          IS2=IPHIT
          IF(IS1.EQ.0)IS1=IPHIT
 5020     IPHIT=IPHIT+HLDHT
          IF(IPHIT.LT.HPHT9)GOTO5010
          IF(IS3.GE.4)GOTO5030
C ERRORM 2 ---------- NO GOOD POINTS TO FIT ---------------------------
          IF(IS3.LE.0)CALLERRORM('   XYFIT',2,NTRAK)
C ERRORM 3 ---------- NOT ENOUGH GOOD POINTS TO FIT -------------------
          IF(IS3.GT.0)CALLERRORM('   XYFIT',3,NTRAK)
          FATAL=.TRUE.
          RETURN
 5030     CONTINUE
          XEND=WRK(IS1-7)
          YEND=WRK(IS1-6)
          XSTR=WRK(IS2-7)
          YSTR=WRK(IS2-6)
          NMPROG=2
C         --------------------------
C         EVALUATE SLOPE OF SYMMETRY
C         AXES AND ROUGH ORIGIN.
C         --------------------------
          COSTH=XEND-XSTR
          SINTH=YEND-YSTR
          XMID=(XSTR+XEND)/2
          YMID=(YSTR+YEND)/2
          SP1=1.0/SQRT(COSTH**2+SINTH**2)
          COSTH=COSTH*SP1
          SINTH=SINTH*SP1
C         -------------------------
C         TRANSFORM COORDINATES AND
C         PERFORM PARABOLIC FIT.
C         -------------------------
          ISP5=0
          SUMX=0
          SUMY=0
          SUMXY=0
          SUMX2=0
          SUMX3=0
          SUMX4=0
          SUMX2Y=0
          NPTS=0
          IPHIT=HPHT0+3
          DO 5050 N=1,NHIT
          SP2=WRK(IPHIT)-XMID
          Y1=WRK(IPHIT+1)-YMID
          X1=SP2*COSTH+Y1*SINTH
          Y1=Y1*COSTH-SP2*SINTH
          AX(N)=X1
          AY(N)=Y1
          USED(N)=.FALSE.
          IF(IWRK(IPHIT+7).NE.0)GOTO5050
          USED(N)=.TRUE.
          NPTS=NPTS+1
          SUMX=SUMX+X1
          SUMY=SUMY+Y1
          SUMXY=SUMXY+X1*Y1
          SP2=X1**2
          SUMX2=SUMX2+SP2
          SUMX3=SUMX3+X1*SP2
          SUMX4=SUMX4+SP2**2
          SUMX2Y=SUMX2Y+SP2*Y1
 5050     IPHIT=IPHIT+HLDHT
C         ---------------------------
C         COMPUTE PARABOLA PARAMETERS
C         ---------------------------
 5060     CONTINUE
          SP1=SUMX*SUMY/NPTS-SUMXY
          SP2=SUMX*SUMX2/NPTS-SUMX3
          SP3=SUMX**2/NPTS-SUMX2
          SP4=SUMX2*SUMY/NPTS-SUMX2Y
          SP5=SUMX2**2/NPTS-SUMX4
          C2=SP3*SP5-SP2**2
          C4=(SP3*SP4-SP1*SP2)/C2
C
C !!!!!   THE FOLLOWING STATEMENT INSERTED IN ORDER TO AVOID
C !!!!!   DIVISION BY ZERO 5 STATEMENTS FURTHER ON (P.STEFFEN)
          IF(ABS(C4) .LT. 1.E-8) C4 = 1.E-8
C
          C2=(SP1*SP5-SP2*SP4)/C2
          C3=(SUMY-C4*SUMX2-C2*SUMX)/NPTS
          CURV=-2*C4
          C2=C2/CURV
          C3=C3-C4*C2*C2
          NDF=NPTS-4
          ENTRYRESIDS
C
C
C
C
C
C=======================================================================
C=======================================================================
C=======================================================================
C=======================================================================
C
C          COMPUTE CHI**2 AND HIT RESIDUALS FOR CURRENT FIT
C
C=======================================================================
C=======================================================================
C=======================================================================
C=======================================================================
C
C
C
C
C
C    FLAG = 0 => HIT WITHIN  ACCEPTANCE CUT (CODE0)
C    FLAG = 1 => HIT OUTSIDE ACCEPTANCE CUT BUT USED IN FIT
C    FLAG = 2 => HIT OUTSIDE ACCEPTANCE CUT BUT WITHIN DOUBLE HIT RESLN
C    FLAG = 3 => HIT OUTSIDE DOUBLE HIT RESOLUTION
          NBAD=0
          NEXTRA=0
          ISP5=0
          CHIFIT=0
C         COMPUTE RESIDUAL CUT
          RADBIT=10*NPTS*ABS(CURV)
          IF(RADBIT.GT.1.0)RADBIT=1.0
          CODE0=XYF(4)+RADBIT*XYF(5)
          CODE2=GFP(3)
          CHSLIM=XYF(2)+RADBIT*XYF(3)
          IF(NMPROG.EQ.2)GOTO4001
          SP1=C4**2
          SP2=0.5/ABS(C4)
 4001     CONTINUE
          IPHIT=HPHT0+10
          DO 4050 N=1,NHIT
          X1=AX(N)
          SP3=(X1-C2)**2
          Y1=AY(N)-C3
          IF(NMPROG.EQ.2)GOTO4010
          CHI=(SP3+Y1**2-SP1)*SP2
          GOTO4020
 4010     CHI=C4*SP3-Y1
 4020     WRK(IPHIT+3)=CHI
          IF(USED(N))CHIFIT=CHIFIT+CHI**2
          IF(.NOT.SETFLG)GOTO4050
          CHI=ABS(CHI)
          IF(CHI.GT.CODE0)GOTO4030
C         GOOD RESIDUAL TO NEW FIT
          IWRK(IPHIT)=0
          IF(USED(N))GOTO4025
          NEXTRA=NEXTRA+1
          GOTO4050
 4025     XFIRST=X1
          IF(ISP5.EQ.0)ISP5=N
          GOTO4050
C         BAD RESIDUAL TO NEW FIT
 4030     IWRK(IPHIT)=2
          IF(CHI.GT.CODE2)IWRK(IPHIT)=3
          IF(.NOT.USED(N))GOTO4050
          IWRK(IPHIT)=1
          NBAD=NBAD+1
 4050     IPHIT=IPHIT+HLDHT
          IF (NDF.GT.0)CHIFIT=CHIFIT/NDF
          NGOOD=NPTS-NBAD+NEXTRA
C####################################################################
      RAD = 1.0/CURV
      IF(PRINT)PRINT4051,NMPROG,NPTS,NGOOD,NBAD,NEXTRA,RAD,
     + CODE0,CHSLIM,CHIFIT
 4051 FORMAT(' FIT ',I1,' #POINTS USED/GOOD/BAD/EXTRA =',4I3,
     +' RADIUS =',E11.4,' CODE0 =',F4.1,' CHSLIM =',F4.1,
     +'  CHI**2 =',F7.3,' ######')
      IF (PRINT) CALL PCWORK(0,0,0,1,0)
C####################################################################
          IF(ISP5.NE.0)GOTO4080
          XLAST=AX(1)
          XFIRST=AX(NHIT)
          GOTO4090
 4080     XLAST=AX(ISP5)
C         ------------------------
C         LABEL AS GOOD OR BAD FIT
C         ------------------------
 4090     BADFIT=.FALSE.
          IF(CHIFIT.GT.CHSLIM)BADFIT=.TRUE.
          RETURN
          END
          SUBROUTINE CIRFIT
          IMPLICIT INTEGER*2(H)
C
C
C
C
C
C=======================================================================
C=======================================================================
C=======================================================================
C=======================================================================
C
C                    CIRCLE FIT USING PARABOLA
C
C=======================================================================
C=======================================================================
C=======================================================================
C=======================================================================
C
C
C
C
C
#include "cworkpr.for"
#include "cworkeq.for"
#include "cxyfit.for"
#include "cpatlm.for"
          FATAL=.TRUE.
C ERRORM 4 ------------ NOT ENOUGH HITS FOR CIRCLE -------------------
          IF(NPTS.LE.3)CALLERRORM('   XYFIT',4,NTRAK)
C         ---------------------------
C         START WITH INSCRIBED CIRCLE
C         ---------------------------
          NSERR=0
          IF(CURV.NE.0.0)GOTO900
C ERRORM 10 ----------- CURVATURE ZERO FROM PARFIT -------------------
          CALLERRORM('   XYFIT',10,NTRAK)
          RETURN
 900      C4C=1.0/ABS(CURV)
          C3C=C3-SIGN(C4C,CURV)
          NCLOOP=0
C         NO FIT FOR NPTS <= 4
          IF(NPTS.LE.4)GOTO1051
C         ----------------------------------
C         DETERMINE AVERAGE RADIAL DEVIATION
C         -----------------------------------
 1000     SP2=C4C**2
          DR1=0.
          CHICIR=0.
          DO 1010 N=1,NHIT
          IF(.NOT.USED(N))GOTO1010
          X1=AX(N)-C2
          Y1=AY(N)-C3C
          SP1=X1**2+Y1**2-SP2
          DR1=DR1+SP1
          CHICIR=CHICIR+SP1**2
 1010     CONTINUE
C         --------------------
C         STOP ITERATING NOW ?
C         --------------------
          IF(C4C.NE.0.0)GOTO1011
C ERRORM 11 ------------- CIRCLE FIT ERROR ZERO RADIUS --------------
          CALL ERRORM('   XYFIT',11,NTRAK)
          RETURN
 1011     CHICIR=CHICIR/(4.0*SP2*(NPTS-3))
C#######################################################################
          IF(PRINT)PRINT1012,NCLOOP,CHICIR,CHIL1,CHIFIT
 1012     FORMAT(' CIRCLE FIT LOOP',I3,1X,' CHI**2 =',F7.3,' OLD=',F7.3,
     +    ' PARAB=',F7.3)
C#######################################################################
          IF(CHICIR.LE.0.)GOTO1050
          IF(NCLOOP.EQ.0)GOTO1020
          IF(NCLOOP.GE.8)GOTO1080
          IF(CHICIR.GT.CHIL1)GOTO1070
          IF(ABS(CHICIR-CHIL1).LE.XYF(8)*CHIL1)GOTO1050
C         ------------------------------------
C         CORRECT FOR AVERAGE RADIAL DEVIATION
C         ------------------------------------
 1020     CHIL1=CHICIR
          C3CL1=C3C
          C4CL1=C4C
          NCLOOP=NCLOOP+1
          IF(C4C.NE.0.0)GOTO1021
C ERRORM 12 ------------- CIRCLE FIT ERROR,ZERO RADIUS -----------------
          CALL ERRORM('   XYFIT',12,NTRAK)
          RETURN
 1021     DR1=DR1/(2.0*C4C*NPTS)
          C4C=C4C+DR1
          C3C=C3C-SIGN(DR1,CURV)
C         -------------------------
C         FIT PARABOLA TO RESIDUALS
C         -------------------------
          SP2=C4C**2
          SP1=0.5/C4C
          SUMX=0
          SUMY=0
          SUMXY=0
          SUMX2=0
          N=NHIT
 1030     IF(.NOT.USED(N))GOTO1040
          X2=AX(N)-C2
          SP3=X2**2
          Y2=AY(N)-C3C
          Y2=-(SP3+Y2**2-SP2)*SP1
          SUMX=SUMX+X2
          SUMX2=SUMX2+SP3
          SUMXY=SUMXY+X2*Y2
          SUMY=SUMY+Y2
 1040     N=N-1
          IF(N.GT.0)GOTO1030
C         ------------------------------
C         SOLUTION FOR CIRCLE PARAMETERS
C         ------------------------------
          SPA=SUMX2*NPTS-SUMX**2
          IF( (SPA.NE.0.0).AND.(SP2.NE.0.0).AND.(X1.NE.X2) )GOTO1041
C ERRORM 14 ----------- CIRCLE FIT PARAMETER ERROR ---------------
          CALL ERRORM('   XYFIT',14,NTRAK)
          RETURN
 1041     SP1=(SUMXY*NPTS-SUMX*SUMY)/SPA
          DR2=2*C4C*(SUMY-SP1*SUMX)/NPTS
          SP3=(X2-X1)**2/(4*SP2)
          IF(SP3.GT.1.0)GOTO1060
          SP3=(1.0-SQRT(1.0-SP3))*C4C
          IF(SP3.NE.0.)GOTO1045
C ERRORM 7 ---------- TRACK START AND END ERROR -----------------------
          CALLERRORM('   XYFIT',7,NTRAK)
          RETURN
 1045     DR2=-DR2/SP3
          C4C=C4C+DR2
          C3C=C3C-SIGN(DR2,CURV)
C#######################################################################
          IF(PRINT)PRINT1046,DR1,DR2,C4C,X1,X2
 1046 FORMAT(' ..  DR1=',E11.4,' DR2=',E11.4,
     + '  RAD=',E11.4,'  FIRST/LAST PTS=',2E11.4)
C#######################################################################
          GOTO1000
C         ----------------
C         CIRCLE FIT ENDED
C         ----------------
 1050     IF(CHICIR.GT.10*CHIFIT)GOTO1090
 1051     NMPROG=1
          C3=C3C
          C4=-SIGN(C4C,C4)
          CURV=1.0/C4
          NDF=NPTS-3
          FATAL=.FALSE.
C#######################################################################
          IF(PRINT)PRINT1052,NCLOOP,NPTS,C4
 1052     FORMAT(' EXIT CIRCLE FIT AFTER',I2,' ITERATIONS. NPTS=',I3,
     +    ' RADIUS =',E11.4)
C#######################################################################
          RETURN
C         ------------------------
C         FAILURE .. SAGITTA ERROR
C         ------------------------
 1060     CONTINUE
C#######################################################################
          IF(PRINT)PRINT1061,NSERR,SP3
 1061     FORMAT(' ****** CIRCLE SAGITTA ERROR ',I4,2X,E11.4)
C#######################################################################
          IF(NSERR.GE.2)RETURN
          NSERR=NSERR+1
          C4C=ABS(X2-X1)*0.6
          C3C=C3-SIGN(C4C,CURV)
          GOTO1000
C         --------------------------
C         FAILURE .. NON-CONVERGENCE
C         --------------------------
 1070     CONTINUE
C#######################################################################
      IF (PRINT) PRINT1071,CHICIR,CHIL1,CHIFIT
 1071 FORMAT(' CIRCLE FIT NOT CONVERGING ',3E11.4)
C#######################################################################
          IF(CHIL1.GT.CHIFIT)RETURN
          C4C=C4CL1
          C3C=C3CL1
          CHICIR=CHIL1
C#######################################################################
      IF (PRINT) PRINT1076
 1076 FORMAT(' CONVERGENCE SAVED BY USING LAST ITERATION')
C#######################################################################
          GOTO1050
C         -------------------------
C         FAILURE .. TOO MANY LOOPS
C         -------------------------
 1080     IF(CHICIR.GT.CHIFIT)GOTO1050
C#######################################################################
          IF(PRINT)PRINT1081
 1081     FORMAT(' CIRCLE FIT USING TOO MANY ITERATIONS')
C#######################################################################
          RETURN
C         ----------------------------
C         FAILURE .. CHI**2 > 10*PARAB
C         ----------------------------
 1090     CONTINUE
          RETURN
          END
          SUBROUTINE NEWINC
          IMPLICIT INTEGER*2(H)
C
C
C
C
C
C=======================================================================
C=======================================================================
C=======================================================================
C=======================================================================
C
C          ADJUST ALL HIT COORDINATES FOR NEW TRACK INCLINATION
C
C=======================================================================
C=======================================================================
C=======================================================================
C=======================================================================
C
C
C
C
C
C%MACRO 'F11GOD.PATRECSR(CJDRCH)'
C%MACRO 'F11GOD.PATRECSR(CDSMAX)'
C%MACRO 'F11GOD.PATRECSR(CWORKPR)'
C%MACRO 'F11GOD.PATRECSR(CWORKEQ)'
C%MACRO 'F11GOD.PATRECSR(CXYFIT)'
C-- ERROR 1 ==> NEWINC COMMENTED OUT
          CALL ERRORM('NEWINC  ',1,0)
C         INTEGER*4 BIT29N/ZFFFB/
C         IF(.NOT.TBIT(KNTROL,29))RETURN
C         SP2=XMID+C2*COSTH-C3*SINTH
C         SP3=YMID+C2*SINTH+C3*COSTH
C         IPHIT=HPHT0+3
C         GOTO(2010,2020),NMPROG
C2010     R1=ABS(C4)
C         GOTO2030
C2020     R1=1.0/(2*C4)
C         SP2=SP2-R1*SINTH
C         SP3=SP3+R1*COSTH
C         R1=ABS(R1)
C2030     INCELL=IWRK(IPHIT+6)
C         INRING=IWRK(IPHIT+9)
C         IF(IWRK(IPHIT+5).GT.0)GOTO2040
C         LRFLAG=-1
C         LRAMB=1
C         GOTO2050
C2040     LRFLAG=+1
C         LRAMB=2
C2050     TRLORX=TRMATS(INCELL,LRAMB)
C         TRLORY=TRMATC(INCELL,LRAMB)
C         SINLOR=SINDRI(INCELL,LRAMB)
C         COSLOR=COSDRI(INCELL,LRAMB)
C         ISP5=INCELL-24*(INRING-1)
C         IF(INRING.EQ.3)GOTO2060
C         DXWR=DIRWR1(ISP5,1)
C         DYWR=DIRWR1(ISP5,2)
C         GOTO2070
C2060     DXWR=DIRWR3(ISP5,1)
C         DYWR=DIRWR3(ISP5,2)
C2070     TRABX=SINLOR*DXWR-COSLOR*DYWR
C         TRABY=COSLOR*DXWR+SINLOR*DYWR
C         X1=WRK(IPHIT)
C         Y1=WRK(IPHIT+1)
C         BETA=R1/ABS(TRABX*(X1-SP2)+TRABY*(Y1-SP3))
C         IF(BETA.LT.1.0)BETA=1.0
C         IF(BETA.GT.1.2)BETA=1.1
C         SP5=LRFLAG*(BETA-WRK(IPHIT+8))*2.9
C         WRK(IPHIT)=X1+TRLORX*SP5
C         WRK(IPHIT+1)=Y1+TRLORY*SP5
C         WRK(IPHIT+8)=BETA
C         IPHIT=IPHIT+HLDHT
C         IF(IPHIT.LT.HPHT9)GOTO2030
C         KNTROL=LAND(KNTROL,BIT29N)
C2090     CONTINUE
C#######################################################################
C     IF (PRINT) PRINT2091
C2091 FORMAT(' ****** ABERRATIONS CORRECTED ')
C#######################################################################
          RETURN
          END
          SUBROUTINE TRLCHI
          IMPLICIT INTEGER*2(H)
C
C
C
C
C
C=======================================================================
C=======================================================================
C=======================================================================
C=======================================================================
C
C          COMPUTE CHI**2 CONTRIBUTION FROM EACH TRACK ELEMENT
C
C=======================================================================
C=======================================================================
C=======================================================================
C=======================================================================
C
C
C
C
C
#include "cworkpr.for"
#include "cworkeq.for"
#include "cxyfit.for"
          ISP2=0
          J=0
          IPHIT=HPHT0+8
 3010     ISP1=IABS(IWRK(IPHIT))
C         -----------------------
C         IGNORE 'PATROLLED' HITS
C         -----------------------
          IF(ISP1.GE.1000)GOTO3030
          IF(ISP1.EQ.ISP2)GOTO3020
C         -----------------
C         NEW TRACK ELEMENT
C         -----------------
          J=J+1
          NHITIN(J)=0
          CHITR(J)=0
          ISP2=ISP1
          JTR(J)=ISP1
C         --------------------------------
C         INCREMENT TRACK ELEMENT COUNTERS
C         --------------------------------
 3020     CHITR(J)=CHITR(J)+WRK(IPHIT+5)**2
          NHITIN(J)=NHITIN(J)+1
 3030     IPHIT=IPHIT+HLDHT
          IF(IPHIT.LT.HPHT9)GOTO3010
          NTRTOT=J
C######################################################################
      IF (PRINT) PRINT3031,(JTR(N),NHITIN(N),CHITR(N),N=1,NTRTOT)
 3031 FORMAT(' TREL CHI**2 COMPUTED ',10(I3,I3,F10.4))
C######################################################################
          RETURN
          END
