      SUBROUTINE VTXPRE(IH,IP) 
C*800623*DITTMANN***************************************************
C*  COPIED FROM F22KLE.VERTEX.S(VTXPRE)   18.9.86
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
C*                                                                 *
C*    MODIFIED FOR VERTEX CHAMBER HARDWARE     860211 C.K.         *
C*                                                                 *
C*    MODIFIED COMFIT-ERROR OF SCATTERING-ANGLE   860217 C.K.      *
C*                                                                 *
C*    +++ BIG MODIFICATION +++                    860611 C.K.      *
C*                                                                 *
C*    * FOR EACH TRACK NOW 40 INSTEAD OF 30 WORDS IN T()           *
C*                                                                 *
C*    * FITS IN RPHI AND ZS ARE TREATED INDEPENDENT                *
C*      ( DIFFERENT FIRST POINT, MULT. SCATTERING POSSIBLE,        *
C*        THIS IS NECCESSARY FOR 'COMFIT'TED TRACKS         )      *
C*                                                                 *
C*    +++ END MODIFICATION +++                                     *
C*                                                                 *
C*    - CURVATURE-CORRECTION                                       *
C*    - RECALCULATION OF FIRST MEASURED POINT FOR TYP 2 TRACKS     *
C*                                                                 *
C*    ... COVARIANCES FOR ERROR CALCULATION ...   860714 C.K.      *
C*                                                                 *
C*******************************************************************
      IMPLICIT INTEGER*2 (H)
C PMF 03.11.98 
      LOGICAL TBIT
#include "cdata.for"
C
C%MACRO MVERTEX1
C     MACRO FOR VERTEX-FIT ROUTINES
      COMMON /CWORK1/ NT,T(2000),NV,V(200),A(300),B(24),NTIND(20),S(20),
     +                CHITR(20),
     +                JTGOD(50),JTBAD(50),VSAVE(10),V2(20,20)
C
      DIMENSION IT(2),IV(2)
      EQUIVALENCE (T(1),IT(1)),(V(1),IV(1))
C%MACRO MVERTEX2
C     MACRO FOR VERTEX-FIT ROUTINES ( AXIS AND STATISTICS )
      COMMON /CVTX2/ MODE,TAXIS(12),SVR,HVTXST(120)
C
      DIMENSION IVTXST(1)
C
C        CONSTANTS
      COMMON /CVTXC/ XB,YB,ZB,RTANK,DTANK,X0INN,SIGX0,SIGZ0,PNTMIN,
     +               DISTB,COLL2,MITER,DSCONV,PRCUT,IREJTR,EEDPMN,
     +               EEDPMX,EEDTMX,EEDRMX,SEMAX,SIMAX,SIGFAC,EEXYMN,
     +               EEXYMX,PHEMAX,SIG1
      REAL*8 AA,BB,CC,DD,EE,RR,DR,DR1M
      REAL*8 PHI,SPHI,CPHI,XD,YD,PARKOR,DSR,DPHI
      REAL H03
C                  DEFAULT VALUES
      DATA LFIELD,LRUN/-4848,-1/
C
C
      NT = 0
C-------------------------------------------------------
C  UPDATE OF DTANK AND X0INN
C
      DTANK = RITNK - RBPIPE(DUM)
      X0INN = XBPIPE(DUM)
C-------------------------------------------------------
      IF(IH.EQ.0) RETURN
      IH2 = 2*IH
      NRUN = HDATA(IH2+10)
      NVAV = HDATA(IH2+18)
C
C---- LOCATE RUN-VERTEX IF NEEDED
C
      IF (TBIT(MODE,31)) CALL WTXCRV(NRUN,RVX,RVY,RVDX,RVDY)
C     NOTE THIS CODING FIXES UP ONLY VERY LOW FIELDS
         IFIELD = HDATA(IH2+30)
         IF(IABS(IFIELD).GE.100) GO TO 5408
      IF (NRUN .NE. LRUN)  WRITE(6,5407) NRUN,IFIELD,LFIELD
5407  FORMAT(' * WTXPRE #### *   WARNING: RUN ',I6,' HAS MAGNETIC FIELD
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
      DO 11 J=1,40
   11 IT(JT+J) = 0
      IPFLAG = IDATA(KT+44)
C        MOST OF THE FOLLOWING TRACK REJECT CRITERIA ARE DUE
C        TO FAULTS IN THE PATR BANK
      IF(ADATA(KT+25).EQ.0.) GOTO 18
      PT = SQRT(ADATA(KT+8)**2+ADATA(KT+9)**2)
      IF(PT.EQ.0.) GOTO 18
      IF(ADATA(KT+10).EQ.0.) GOTO 18
CCC   T(JT+2) = -1. / ADATA(KT+25)
      RR = -1.D0 / PARKOR(KT)
      T(JT+2) = SNGL(RR)
      T(JT+3) = ATAN2(ADATA(KT+9),ADATA(KT+8))
      TTH = ADATA(KT+10) / PT
      CTH = 1. / SQRT(1.+TTH**2)
      T(JT+4) = ATAN(TTH)
      T(JT+5) = ADATA(KT+5)
      T(JT+6) = ADATA(KT+6)
      T(JT+7) = ADATA(KT+7)
      SR = SIGN(1.,T(JT+2))
      DSR = DBLE(SR)
      ITYP = IDATA(KT+18)
      XP0  = ADATA(KT+20)
      YP0  = ADATA(KT+21)
C
C *** RECALCULATION OF FIRST MEASURED POINT WITH CORRECTED CURVATURE
C
      IF (ITYP.NE.2) GOTO 111
      XD = DBLE(XP0)
      YD = DBLE(YP0)
      SPHI = DSIN(DBLE(ADATA(KT+19)))
      CPHI = DCOS(DBLE(ADATA(KT+19)))
      AA = XD - RR*SPHI
      BB = YD + RR*CPHI
      DPHI = DATAN2( DSR*(DBLE(T(JT+5))-AA), DSR*(BB-DBLE(T(JT+6))) )
      XD = XD + RR*(DSIN(DPHI)-SPHI)
      YD = YD - RR*(DCOS(DPHI)-CPHI)
      T(JT+3) = SNGL(DPHI)
      T(JT+5) = SNGL(XD)
      T(JT+6) = SNGL(YD)
C
C     CHANGE TO TYP 1 ?
C
      IF (ABS(T(JT+2)).GT.20000.) GOTO 111
      IF (.NOT.TBIT(MODE,26)) GOTO 111
C
C     LIST OF CHANGED ITEMS IN PATR BANK
C
      ADATA(KT+5) = T(JT+5)
      ADATA(KT+6) = T(JT+6)
      ADATA(KT+8) = COS(T(JT+3)) * PT
      ADATA(KT+9) = SIN(T(JT+3)) * PT
      IDATA(KT+18) = 1
      ADATA(KT+19) = 1./ABS(T(JT+2))
      ADATA(KT+20) = SNGL( DSQRT(AA*AA+BB*BB) - DABS(RR) )
      ADATA(KT+21) = SNGL( DATAN2( BB, AA ) )
      ADATA(KT+22) = 0.
      ADATA(KT+25) = -1./T(JT+2)
C
  111 CONTINUE
C
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
      T(JT+9) = SIGZ * 4.24*CTH**2/(PROJL*SZNPT)
      T(JT+10) = SIGX * 3. / SXNPT
      T(JT+11) = T(JT+10)
      T(JT+12) = SIGZ * 2.45 / SZNPT
C *** FOR NEW ERROR-CALCULATION
C     RPHI DEFAULT COVARIANCES ( FROM ID WITH 48 WIRES )
      SIG2N    = SIGX*SIGX/XNPT
      T(JT+20) = PROJL
      T(JT+32) = 154.0*SIG2N/PROJL**4
      T(JT+33) =   0.0
      T(JT+34) = -19.6*SIG2N/PROJL**2
      T(JT+35) =   0.0
      T(JT+36) =  2.45*SIG2N
      T(JT+40) =  10.7*SIG2N/PROJL**2
C     ZS PARAMETERS
      T(JT+37) = SIGZ*SIGZ/ZNPT
      T(JT+38) = PROJL
C     TRUE COVARIANCES FROM COMFIT
      IF ((LT.NE.64).OR.(IDATA(KT+2).NE.301).OR.(IDATA(KT+44).NE.3))
     +   GOTO 139
C     RAMCKE TEST
      IF (ADATA(KT+52).EQ.0.0) GOTO 139
      T(JT+32) = ADATA(KT+52)
      T(JT+33) = ADATA(KT+53)
      T(JT+34) = ADATA(KT+54)
      T(JT+35) = ADATA(KT+55)
      T(JT+36) = ADATA(KT+56)
      T(JT+40) = ADATA(KT+57)
  139 CONTINUE
C ***
      IT(JT+13) = IDATA(KT+24)
      T(JT+15) = 0.
      T(JT+21) = SIN(T(JT+3))
      T(JT+22) = COS(T(JT+3))
      T(JT+23) = TTH
      T(JT+24) = CTH
      IT(JT+1) = 2
      IF(IPFLAG.EQ.2) GOTO 14
C----   THIS CLUDGE CORRECTS AN ERROR IN THE PATR BANK
      AA = -DBLE(T(JT+5)) + RR*DBLE(T(JT+21))
      BB = -DBLE(T(JT+6)) - RR*DBLE(T(JT+22))
      IF(ITYP.EQ.2) AA=AA+DBLE(XP0)
      IF(ITYP.EQ.2) BB=BB+DBLE(YP0)
      DR1M = DSQRT(AA**2+BB**2)
      DR = DR1M - DABS(RR)
      IF(ITYP.NE.2) DR=DR-DBLE(ADATA(KT+20))
      T(JT+5) = T(JT+5) + DR*AA/DR1M
      T(JT+6) = T(JT+6) + DR*BB/DR1M
      T(JT+7) = ADATA(KT+31) + ADATA(KT+30)*SQRT(T(JT+5)**2+T(JT+6)**2)
C----
C
   14 XM = T(JT+5) - T(JT+2)*T(JT+21)
      YM = T(JT+6) + T(JT+2)*T(JT+22)
C
C---- START ITERATION AT RUNVERTEX ?
C
      IF (TBIT(MODE,31)) T(JT+15) =
     +      ( ATAN2( SR*(RVX - XM), SR*(YM - RVY) )
     +      - T(JT+3) ) * T(JT+2)
C
C---- MC WITHOUT MULTIPLE SCATTERING ?
C
      X0VX = XFWVXC(IH)
      X0ID = XFWID(IH) - X0VX
      IF (NRUN.GE.100) GOTO 149
      IF (NVAV.NE.-1)  GOTO 149
         X0ID = 0.
         X0VX = 0.
  149 CONTINUE
C
      PT = H03*T(JT+2)
C
C     INTERSECT WITH INNER TANKWALL
C
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
   16 PHI = ATAN2(-SR*(XM-XT1),SR*(YM-YT1))
      THETA = PHI - T(JT+3)
      IF(ABS(THETA).GT.3.141593) THETA=THETA-SIGN(1.,THETA)*6.283185
      T(JT+27) = T(JT+2)*THETA
      T(JT+18) = T(JT+27)
      PHI = ATAN2(-SR*(XM-XT2),SR*(YM-YT2))
      THETA = PHI - T(JT+3)
      IF(ABS(THETA).GT.3.141593) THETA=THETA-SIGN(1.,THETA)*6.283185
      T(JT+26) = T(JT+2)*THETA
C        MULT. SCATTERING IN INNER TANK WALL (AND VTXC)
      BETA = 1.
      TH2 = (14.1/(PT*BETA))**2 * X0ID * T(JT+24)
     +      * ( 1. + 1./9. * ALOG10( X0ID / T(JT+24) ) )**2
      T(JT+16) = SQRT(TH2)
   17 CONTINUE
C
C     INTERSECT WITH BEAMPIPE
C
      CC = (RBPIPE(DUM)+DRPIPE(DUM))**2
      DD = (AA-BB+CC)/2.D0
      EE = AA*CC - DD**2
C        ANY INTERSECT ?
      IF(EE.LE.0.D0) GOTO 172
      EE = DSQRT(EE)
      XT1 = (XM*DD + YM*EE) / AA
      YT1 = (YM*DD - XM*EE) / AA
      XT2 = (XM*DD - YM*EE) / AA
      YT2 = (YM*DD + XM*EE) / AA
      D1 = SQRT((XT1-T(JT+5))**2+(YT1-T(JT+6))**2)
      D2 = SQRT((XT2-T(JT+5))**2+(YT2-T(JT+6))**2)
      IF(D1.LT.D2) GOTO 171
      XT = XT2
      YT = YT2
      XT2 = XT1
      YT2 = YT1
      XT1 = XT
      YT1 = YT
  171 PHI = ATAN2(-SR*(XM-XT1),SR*(YM-YT1))
      THETA = PHI - T(JT+3)
      IF(ABS(THETA).GT.3.141593) THETA=THETA-SIGN(1.,THETA)*6.283185
      T(JT+19) = T(JT+2)*THETA
C        MULT. SCATTERING IN BEAMPIPE
      BETA = 1.
      TH2VX = 0.
      IF (X0VX.GT.0.) TH2VX = (14.1/(PT*BETA))**2 * X0VX * T(JT+24)
     +      * ( 1. + 1./9. * ALOG10( X0VX / T(JT+24) ) )**2
      T(JT+17) = SQRT(TH2VX)
  172 CONTINUE
C
C---- TRACK WITH VTXC ?
C
      R1 = SQRT(T(JT+5)**2+T(JT+6)**2)
      IT(JT+31) = 0
      T(JT+39) = 0.
      IF (R1.GE.RTANK) GOTO 18
      IT(JT+31) = 1
C     DISTANCE 1. WIRE VTXC TO 1. WIRE ID
      DS = 120.
      IF ((IDATA(KT+44).NE.1).AND.(IDATA(KT+44).NE.3)) GOTO 175
C     GET PROJ. TRACKLENGTH FROM ZS-FIT
      PROJLZ = ABS((ADATA(KT+45)-ADATA(KT+46))/TTH)
      DS = T(JT+38) - PROJLZ
C
  175 CONTINUE
      T(JT+38) = T(JT+38) - DS
      T(JT+39) = -DS
C
   18 JT = JT + 40
   19 KT = KT + LT
C
C
      RETURN
      END
