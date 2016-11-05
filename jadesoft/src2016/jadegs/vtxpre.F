C   04/07/79 308061945  MEMBER NAME  VTXPREGL (JADEGS)      FORTRAN
      SUBROUTINE VTXPRE(IH,IP)
C*800623*DITTMANN***************************************************
C*                                                                 *
C*   P R E P A R A T I O N   F O R   X Y Z   V E R T E X   F I T   *
C*                                                                 *
C*       IH = POINTER TO 'HEAD' BANK                               *
C*       IP = POINTER TO 'PATR' BANK (AS IN JADE COMP NOTE 12).    *
C*                                                                 *
C*       VTXPRE COPIES ALL TRACKS INTO /CWORK1/, FLAGS TRACKS WHICH*
C*       SHOULD NOT BE USED IN VERTEX FIT, AND ADDS COULOMB        *
C*       SCATTERING ERRORS IN MATERIAL BEFORE FIRST WIRE.          *
C*       FOR DETAILS OF THE T-ARRAY SEE COMMENT IN SUBR. VERTEX.   *
C*                                                                 *
C*       SUBR. VTXINI MUST BE CALLED BEFORE FIRST CALL TO VTXPRE   *
C*******************************************************************
C
C   MAGNETIC FIELD CORRECTION   16.8.1982     J.OLSSON
C   MAGNETIC FIELD RECORRECTION  83/08/04     R.G.GLASSER
C
C*******************************************************************
      IMPLICIT INTEGER*2 (H)
#include "cdata.for"
C
      COMMON /CWORK1/ NT,T(1500)
      DIMENSION IT(2)
      EQUIVALENCE (T(1),IT(1))
C        CONSTANTS
      COMMON /CVTXC/ XB,YB,ZB,RTANK,DTANK,X0INN,SIGX0,SIGZ0,PNTMIN,
     +               DISTB,COLL2,MITER,DSCONV,PRCUT,IREJTR,EEDPMN,
     +               EEDPMX,EEDTMX,EEDRMX,SEMAX,SIMAX,SIGFAC,EEXYMN,
     +               EEXYMX,PHEMAX,SIG1
       REAL*8 AA,BB,CC,DD,EE
       REAL H03
C                  DEFAULT VALUES
       DATA LFIELD,LRUN/-4848,-1/
C
      NT = 0
      IF(IH.EQ.0) RETURN
      IH2 = 2*IH
      NRUN = HDATA(IH2+10)
C     NOTE THIS CODING FIXES UP ONLY VERY LOW FIELDS
         IFIELD = HDATA(IH2+30)
         IF(IABS(IFIELD).GE.100) GO TO 5408
C    RESET MAGNETIC FIELD FOR PERIOD WHEN READ OUT SET IT TO 0
      IF(NRUN.EQ.11908) HDATA(IH2+30) = -4848
      IF(NRUN.GE.12255.AND.NRUN.LE.12270) HDATA(IH2+30) = -4847
      IF(NRUN.GE.13651.AND.NRUN.LE.13653) HDATA(IH2+30) = -4844
         IFIELD = HDATA(IH2+30)
         IF(IABS(IFIELD).GE.100) GO TO 5408
      IF (NRUN .NE. LRUN)  WRITE(6,5407) NRUN,IFIELD,LFIELD
5407  FORMAT(' * VTXPRE #### *   WARNING: RUN ',I6,' HAS MAGNETIC FIELD
     $ = ',I6,' GAUSS - USING ',I6)
      HDATA(IH2+30)=LFIELD
         IFIELD = HDATA(IH2+30)
      LRUN=NRUN
5408  H03 = .00003 * ABS(FLOAT(IFIELD))
      IF(IP.EQ.0) RETURN
      NT = IDATA(IP+2)
      IF(NT.LE.0) RETURN
      IF(NT.GT.50) NT=50
      L0 = IDATA(IP+1)
      LT = IDATA(IP+3)
C
      KT = IP + L0
      JT = 0
      DO 19 I=1,NT
      DO 11 J=1,30
   11 IT(JT+J) = 0
      IPFLAG = IDATA(KT+44)
C        MOST OF THE FOLLOWING TRACK REJECT CRITERIA ARE DUE
C        TO FAULTS IN THE PATR BANK
      IF(ADATA(KT+25).EQ.0.) GOTO 18
      PT = SQRT(ADATA(KT+8)**2+ADATA(KT+9)**2)
      IF(PT.EQ.0.) GOTO 18
      IF(ADATA(KT+10).EQ.0.) GOTO 18
      T(JT+2) = -1. / ADATA(KT+25)
      T(JT+3) = ATAN2(ADATA(KT+9),ADATA(KT+8))
      TTH = ADATA(KT+10) / PT
      CTH = 1. / SQRT(1.+TTH**2)
      T(JT+4) = ATAN(TTH)
      T(JT+5) = ADATA(KT+5)
      T(JT+6) = ADATA(KT+6)
      T(JT+7) = ADATA(KT+7)
      SIGX = ADATA(KT+23)
      SIGZ = ADATA(KT+32)
C----     THIS CLUDGE SHOULD SOMEHOW ACCOUNT FOR SYSTEMATIC ERRORS
      SIGX = SIGX * SIGFAC
      SIGZ = SIGZ * SIGFAC
C----
      XNPT = IDATA(KT+24)
      ZNPT = IDATA(KT+33)
C        CHECK RESIDUAL
      IF(SIGX.EQ.0..OR.SIGZ.EQ.0.) GOTO 18
      IF(SIGX.GT.SIG1*SIGX0) GOTO 18
      IF(SIGZ.GT.SIG1*SIGZ0) GOTO 18
C        CHECK NUMBER OF POINTS ON TRACK
      IF(XNPT.LT.PNTMIN .OR. ZNPT.LT.PNTMIN) GOTO 18
      PROJL = ABS((ADATA(KT+14)-ADATA(KT+7))/TTH)
      IF(PROJL.LT.1.) GOTO 18
      SXNPT = SQRT(XNPT)
      SZNPT = SQRT(ZNPT)
      T(JT+8) = SIGX * 14./(PROJL*SXNPT)
      T(JT+9) = SIGZ * 4.*CTH**2/(PROJL*SZNPT)
      T(JT+10) = SIGX * 3. / SXNPT
      T(JT+11) = T(JT+10)
      T(JT+12) = SIGZ * 2.45 / SZNPT
      IT(JT+13) = IDATA(KT+24)
      T(JT+21) = SIN(T(JT+3))
      T(JT+22) = COS(T(JT+3))
      T(JT+23) = TTH
      T(JT+24) = CTH
      IT(JT+1) = 2
      IF(IPFLAG.EQ.2) GOTO 14
C----   THIS CLUDGE CORRECTS AN ERROR IN THE PATR BANK
      AA = - T(JT+5) + T(JT+2)*T(JT+21)
      BB = - T(JT+6) - T(JT+2)*T(JT+22)
      IF(IDATA(KT+18).EQ.2) AA=AA+ADATA(KT+20)
      IF(IDATA(KT+18).EQ.2) BB=BB+ADATA(KT+21)
      DR1M = DSQRT(AA**2+BB**2)
      DR = DR1M - ABS(T(JT+2))
      IF(IDATA(KT+18).NE.2) DR=DR-ADATA(KT+20)
      T(JT+5) = T(JT+5) + DR*AA/DR1M
      T(JT+6) = T(JT+6) + DR*BB/DR1M
      T(JT+7) = ADATA(KT+31) + ADATA(KT+30)*SQRT(T(JT+5)**2+T(JT+6)**2)
C----
C      PROJECTED ARC LENGTH TO INTERSECT WITH INNER TANK
   14 XM = T(JT+5) - T(JT+2)*T(JT+21)
      YM = T(JT+6) + T(JT+2)*T(JT+22)
      AA = XM
      BB = YM
      AA = AA**2 + BB**2
      BB = T(JT+2)
      BB = BB**2
      CC = RTANK**2
      DD = (AA-BB+CC)/2.D0
      EE = AA*CC - DD**2
C        ANY INTERSECT ?
      IF(EE.LE.0.D0) GOTO 17
      EE = DSQRT(EE)
      XT1 = (XM*DD + YM*EE) / AA
      YT1 = (YM*DD - XM*EE) / AA
      XT2 = (XM*DD - YM*EE) / AA
      YT2 = (YM*DD + XM*EE) / AA
      D1 = SQRT((XT1-T(JT+5))**2+(YT1-T(JT+6))**2)
      D2 = SQRT((XT2-T(JT+5))**2+(YT2-T(JT+6))**2)
      IF(D1.LT.D2) GOTO 16
      XT = XT2
      YT = YT2
      XT2 = XT1
      YT2 = YT1
      XT1 = XT
      YT1 = YT
   16 SR = SIGN(1.,T(JT+2))
      PHI = ATAN2(-SR*(XM-XT1),SR*(YM-YT1))
      THETA = PHI - T(JT+3)
      IF(ABS(THETA).GT.3.141593) THETA=THETA-SIGN(1.,THETA)*6.283185
      T(JT+27) = T(JT+2)*THETA
      PHI = ATAN2(-SR*(XM-XT2),SR*(YM-YT2))
      THETA = PHI - T(JT+3)
      IF(ABS(THETA).GT.3.141593) THETA=THETA-SIGN(1.,THETA)*6.283185
      T(JT+26) = T(JT+2)*THETA
C        MULT. SCATTERING IN INNER TANK WALL AND BEAMPIPE
      PT = H03*T(JT+2)
      BETA = 1.
      TH2 = (15./(PT*BETA))**2 * X0INN * T(JT+24)
      T(JT+16) = SQRT(TH2)
   17 CONTINUE
   18 JT = JT + 30
   19 KT = KT + LT
C
C
      RETURN
      END
