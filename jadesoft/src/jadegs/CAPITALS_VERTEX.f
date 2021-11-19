C   18/05/79 006251731  MEMBER NAME  VERTEX   (JADEGS)      FORTRAN
      SUBROUTINE VERTEX
C*800623*DITTMANN****************************************************
C*                                                                  *
C*             X - Y - Z   V E R T E X   F I T                      *
C*                                                                  *
C*       INPUT PARAMETERS IN /CWORK1/ (MM, MEV, RADIANS)            *
C*       ================                                           *
C*       NT    = TOTAL NR OF TRACKS OF EVENT                        *
C*       IT(1) =         FLAG (0 = TRACK INCOMPLETE OR BAD, NOT USED*
C*                             1 = GOOD, BUT DO NOT USE IN VERTEXFIT*
C*                             2 = GOOD)                            *
C*       T( 2) = +-R     RADIUS(+ MEANS ANTICLOCKWISE LOOKING TO -Z)*
C*          3  = PHI     AZIMUTH AT POINT XT,YT,ZT                  *
C*          4  = THETA   POLAR ANGLE TO XY-PLANE(0=VERTICAL TO BEAM)*
C*          5  = XT      .                                          *
C*          6  = YT      . FIRST MEASURED POINT ON TRACK            *
C*          7  = ZT      .                                          *
C*          8  = DPHI    ERROR OF PHI                               *
C*          9  = DTHETA  ERROR OF THETA                             *
C*         10  = DXT     .                                          *
C*         11  = DYT     . ERROR OF XT,YT,ZT                        *
C*         12  = DZT     .                                          *
C*         13  = NPT     NUMBER OF POINTS ON TRACK (INTEGER)        *
C*         14  = 0.      NOT USED ON INPUT                          *
C*         15  = 0.      NOT USED ON INPUT                          *
C*         16-30         FOR INTERNAL USE (SEE BELOW)               *
C*        (31-60)        2. TRACK                                   *
C*         .....         ........                                   *
C*                                                                  *
C*       OUTPUT PARAMETERS IN /CWORK1/ (MM, MEV, RADIANS)           *
C*       =================                                          *
C*       FOR TRACKS WITH IT(1) GT 0                                 *
C*       IT(1) =         FLAG (3 = TRACK WAS USED IN VERTEXFIT)     *
C*       T( 3) = PHI     AZIMUTH AT POINT XT,YT,ZT                  *
C*          5  = XT      .                                          *
C*          6  = YT      . POINT ON TRACK NEAREST TO VERTEX         *
C*          7  = ZT      .                                          *
C*         10  = DXT     .                                          *
C*         11  = DYT     . ERROR OF XT,YT,ZT                        *
C*         12  = DZT     .                                          *
C*         14  = NV      NUMBER OF VERTEX TO WHICH TRACK BELONGS (I)*
C*         15  = S       EXTRAPOLATED ARC LENGTH (USUALLY NEGATIVE) *
C*       ALL OTHER T'S ARE UNCHANGED                                *
C*                                                                  *
C*       NV    = TOTAL NUMBER OF VERTICES                           *
C*       IV(1) =         FLAG (0 = NO VERTEX FIT                    *
C*                             1 = BAD VERTEX FIT                   *
C*                             2 = VERTEX OF 1- OR COLLINEAR 2-PRONG*
C*                             3 = GOOD VERTEX FIT                  *
C*                             4 = E+E- PAIR VERTEX                 *
C*                             5 = ISOLATED SINGLE TRACK VERTEX)    *
C*       V( 2) = X       .                                          *
C*          3  = Y       . VERTEX COORDINATES                       *
C*          4  = Z       .                                          *
C*          5  = DX      .                                          *
C*          6  = DY      . ERROR OF X,Y,Z                           *
C*          7  = DZ      .                                          *
C*       IV(8) = NTR     NUMBER OF TRACKS USED IN VERTEX FIT        *
C*       V( 9) = CHI2    CHISQARE OF FIT (N.D.F. = 2*NTR-3)         *
C*       IV(10)= NTRALL  NUMBER OF TRACKS BELONGING TO THIS VERTEX  *
C*        (11-20)        2. VERTEX                                  *
C*         .....         .........                                  *
C*                                                                  *
C*       INTERNAL PARAMETERS                                        *
C*       ===================                                        *
C*       IT(1) IS SET NEGATIV TEMPORARILY IF TRACK BELONGS TO VERTEX*
C*       T(16) = COULOMB SCATTERING ERROR                           *
C*       T(17) =                                                    *
C*       T(21) = SIN(PHI0)                                          *
C*       T(22) = COS(PHI0)                                          *
C*       T(23) = TAN(THETA)                                         *
C*       T(24) = COS(THETA)                                         *
C*       T(26) = S TO TANKWALL FAR                                  *
C*       T(27) = S TO TANKWALL NEAR                                 *
C*       T(28) = S.D. X                                             *
C*       T(29) = S.D. Y                                             *
C*       T(30) = S.D. Z                                             *
C*                                                                  *
C*       THIS ROUTINE CALLS SUBROUTINE SMINVD FOR SOLVING THE       *
C*       MATRIX EQUATION                                            *
C********************************************************************
      COMMON /CWORK1/ NT,T(1500),NV,V(200),A(55),B(10),NTIND(7),S(7),
     *                CHITR(7)
      DIMENSION IT(2),IV(2)
      EQUIVALENCE (T(1),IT(1)),(V(1),IV(1))
      REAL*8 A,B,DET
C        CONSTANTS
      COMMON /CVTXC/ XB,YB,ZB,RTANK,DTANK,X0INN,SIGX0,SIGZ0,PNTMIN,
     *               DISTB,COLL2,MITER,DSCONV,PRCUT,IREJTR
C
      IF(NV.LE.0 .OR. NV.GT.20) RETURN
      LV = (NV-1)*10
      IV(LV+1) = 0
      IF(NT.LE.0) RETURN
    1 ITER = 0
      CHI2 = 0.
      ITFIN = 0
C****    CHECK TRACKS
C****    IF MORE THAN 7, REJECT THOSE WITH FEWEST MEASURED POINTS
      N = 0
      J = 0
      DO 5 I=1,NT
      IF(IT(J+1).LE.1) GOTO 5
      N = N + 1
      IF(N.GT.7) GOTO 2
      K = N
      GOTO 4
    2 MINPT = 48
      DO 3 K=1,7
      L = NTIND(K)
      IF(IT(L+13).GE.MINPT) GOTO 3
      IMIN = K
      MINPT = IT(L+13)
    3 CONTINUE
      IF(IT(J+13).LE.MINPT) GOTO 5
      K = IMIN
    4 NTIND(K) = J
      S(K) = 0.
    5 J = J + 30
      IF(N.GT.7) N=7
      NTR = N
      IF(NTR.EQ.0) RETURN
      IF(NTR.EQ.1) GOTO 50
      IF(NTR.NE.2) GOTO 10
C        CHECK IF COLLINEAR 2-PRONG
      J1 = NTIND(1)
      J2 = NTIND(2)
      COSW = T(J1+24)*T(J2+24) * (T(J1+21)*T(J2+21)+T(J1+22)*T(J2+22))
     *     + SIN(T(J1+4))*SIN(T(J2+4))
      COSW = ABS(COSW)
      IF(COSW.GT.COLL2) GOTO 50
C****
C****    VERTEX FIT
   10 CONTINUE
      N = NTR + 3
      L = (N*(N+1))/2
      DO 11 I=1,L
   11 A(I) = 0.D0
      IF(ITFIN.EQ.1) GOTO 13
      DO 12 I=1,N
   12 B(I) = 0.D0
   13 DO 19 I=1,NTR
      J = NTIND(I)
      N = I + 3
      L = (N*(N-1))/2
      PHI = T(J+3) + S(I)/T(J+2)
      SPHI = SIN(PHI)
      CPHI = COS(PHI)
      XT = T(J+5) + T(J+2)*(SPHI-T(J+21))
      YT = T(J+6) - T(J+2)*(CPHI-T(J+22))
      ZT = T(J+7) + S(I)*T(J+23)
      SSPACE = S(I)
      SSCOUL = S(I)-T(J+27)
      IF(SSCOUL.GT.0.) SSCOUL=0.
      DXY = T(J+10)**2 + (T(J+8)*SSPACE)**2 + (T(J+16)*SSCOUL)**2
      DZ = (T(J+9)*SSPACE)**2 + (T(J+16)*SSCOUL)**2
      DXYLIM = 1. / (2.*ABS(T(J+2))*SQRT(DXY))
      DDX = 1. / DXY
      IF(DDX.LT.DXYLIM) DDX=DXYLIM
      DDY = 1. / DXY
      IF(DDY.LT.DXYLIM) DDY=DXYLIM
      DDZ = 1. / (T(J+12)**2 + DZ/(T(J+24)**2)**2)
      IF(ITFIN.EQ.0) GOTO 15
      IT(J+1) = 3
      DDX = DDX * SPHI**2
      DDY = DDY * CPHI**2
      CHITR(I) = DDX*(XT-B(1))**2 + DDY*(YT-B(2))**2+ DDZ*(ZT-B(3))**2
      CHI2 = CHI2 + CHITR(I)
      GOTO 16
   15 B(1) = B(1) + XT*DDX
      B(2) = B(2) + YT*DDY
      B(3) = B(3) + ZT*DDZ
      B(N) = - XT*CPHI*DDX - YT*SPHI*DDY - ZT*T(J+23)*DDZ
   16 A(1) = A(1) + DDX
      A(3) = A(3) + DDY
      A(6) = A(6) + DDZ
      A(L+1) = -CPHI*DDX
      A(L+2) = -SPHI*DDY
      A(L+3) = -T(J+23)*DDZ
      A(L+N) = CPHI**2*DDX + SPHI**2*DDY + T(J+23)**2*DDZ
   19 CONTINUE
      IF(ITFIN.EQ.1) GOTO 30
C        SOLVE MATRIX EQUATION
      CALL SMINVD(A,B,N,1,DET)
      IF(DET.EQ.0.0) GOTO 100
      DSUM = 0.
      DO 23 I=1,NTR
      J = NTIND(I)
      DS = B(I+3)
      IF(ABS(DS/T(J+2)).GT.1.5) GOTO 25
      DSUM = DSUM + ABS(DS)
   23 S(I) = S(I) + DS
      ITER = ITER + 1
      IF(ITER.LT.MITER .AND. DSUM.GT.DSCONV) GOTO 10
C        FIT FINISHED, REPEAT ONCE MORE WITH PROPER ERRORS
      ITFIN = 1
      GOTO 10
C        EXTRAPOLATION OF TRACK I EXCEEDS 90 DEG
C        REMOVE TRACK AND RESTART IF MORE THAN ONE LEFT
   25 IF(NTR.LE.2) GOTO 100
      IT(J+1) = 1
      GOTO 1
   30 CALL SMINVD(A,DUMMY,N,0,DET)
      IF(A(1).LT.0.0 .OR. A(3).LT.0.0 .OR. A(6).LT.0.0) GOTO 100
      NDF = 2*NTR - 3
      PR = PROB(CHI2,NDF)
      IV(LV+1) = 1
      IF(PR.GT.PRCUT) IV(LV+1)=3
      V(LV+2) = B(1)
      V(LV+3) = B(2)
      V(LV+4) = B(3)
      V(LV+5) = DSQRT(A(1))
      V(LV+6) = DSQRT(A(3))
      V(LV+7) = DSQRT(A(6))
      IV(LV+8) = NTR
      V(LV+9) = CHI2
      IF(IREJTR.EQ.0) GOTO 100
      IF(IV(LV+1).EQ.3) GOTO 100
      IF(NTR.LE.2) GOTO 100
C        BAD CHISQARE
C        CHECK IF THERE IS ONE TRACK WHICH CONTRIBUTES MOST TO CHI2
      CHIMAX = 0.
      DO 32 I=1,NTR
      IF(CHITR(I).LT.CHIMAX) GOTO 32
      CHIMAX = CHITR(I)
      NTMAX = I
   32 CONTINUE
      CHICUT = 3. / NTR
      IF(CHICUT.GT..9) CHICUT=.9
      CHICUT = CHICUT * CHI2
      IF(CHIMAX.LT.CHICUT) GOTO 100
      J = NTIND(NTMAX)
      IT(J+1) = 1
      GOTO 1
C****
C****    1-PRONG OR COLLINEAR 2-PRONG
   50 IV(LV+1) = 2
      J = NTIND(1)
      CALL VTXPNT(J,XB,YB,XT1,YT1,ZT1,DXT21,DYT21,DZT21,PHIT,ST)
      IT(J+1) = 3
      IF(NTR.EQ.2) GOTO 52
      V(LV+2) = XT1
      V(LV+3) = YT1
      V(LV+4) = ZT1
      V(LV+5) = SQRT(DXT21)
      V(LV+6) = SQRT(DYT21)
      V(LV+7) = SQRT(DZT21)
      IV(LV+8) = 1
      V(LV+9) = 0.
      IV(LV+10) = 1
      GOTO 100
   52 J = NTIND(2)
      CALL VTXPNT(J,XB,YB,XT2,YT2,ZT2,DXT22,DYT22,DZT22,PHIT,ST)
      IT(J+1) = 3
      V(LV+2) = (XT1+XT2)/2.
      V(LV+3) = (YT1+YT2)/2.
      V(LV+4) = (ZT1+ZT2)/2.
      V(LV+5) = SQRT((DXT21+DXT22)/2.)
      V(LV+6) = SQRT((DYT21+DYT22)/2.)
      V(LV+7) = SQRT((DZT21+DZT22)/2.)
      IV(LV+8) = 2
      V(LV+9) = 0.
      IV(LV+10) = 2
      SDXY1 = SQRT(((XT1-V(LV+2))**2/DXT21 + (YT1-V(LV+3))**2/DYT21)/2.)
      IF(SDXY1.GT.3.) IV(LV+1)=1
      SDXY2 = SQRT(((XT2-V(LV+2))**2/DXT22 + (YT2-V(LV+3))**2/DYT22)/2.)
      IF(SDXY2.GT.3.) IV(LV+1)=1
      SDZ1 = ABS(ZT1-V(LV+4))/SQRT(DZT21)
      IF(SDZ1.GT.3.) IV(LV+1)=1
      SDZ2 = ABS(ZT2-V(LV+4))/SQRT(DZT22)
      IF(SDZ2.GT.3.) IV(LV+1)=1
C
  100 RETURN
      END
C   05/08/79 002080919  MEMBER NAME  VTXAFT   (JADEGS)      FORTRAN
      SUBROUTINE VTXAFT
C*800124*DITTMANN***************************************************
C*                                                                 *
C*          F I N I S H   X Y Z   V E R T E X   F I T              *
C*                                                                 *
C*       CALCULATE TRACK PARAMETERS AT VERTEX-NEAREST POINT        *
C*       FOR DETAILS OF THE T,V-ARRAYS SEE COMMENT IN SUBR. VERTEX.*
C*                                                                 *
C*******************************************************************
C
      COMMON /CWORK1/ NT,T(1500),NV,V(200)
      DIMENSION IV(2),IT(2)
      EQUIVALENCE (V(1),IV(1)),(T(1),IT(1))
C
      IF(NV.EQ.0) RETURN
C****
C****    EXTRAPOLATE ALL TRACKS TO VERTEX
      J = 0
      DO 9 I=1,NT
      IF(IT(J+1).EQ.0) GOTO 9
      IF(IT(J+14).EQ.0) GOTO 9
      LV = (IT(J+14)-1)*10
      CALL VTXPNT(J,V(LV+2),V(LV+3),XT,YT,ZT,DXT2,DYT2,DZT2,PHI,SS)
      T(J+3) = PHI
      T(J+5) = XT
      T(J+6) = YT
      T(J+7) = ZT
      T(J+10) = SQRT(DXT2)
      T(J+11) = SQRT(DYT2)
      T(J+12) = SQRT(DZT2)
      T(J+15) = SS
    9 J = J + 30
C
  100 RETURN
      END
C   05/02/80 002080923  MEMBER NAME  VTXBNK   (JADEGS)      FORTRAN
      SUBROUTINE VTXBNK(IPPATR)
C*800205*OLSSON*****************************************************
C*                                                                 *
C* C R E A T E  B A N K  GVTX  F R O M  V E R T E X  R E S U L T S *
C*                                                                 *
C*******************************************************************
C             IPPATR IS POINTER TO 'PATR' ;  SAME BOSBANK NR IS USED
      IMPLICIT INTEGER*2 (H)
#include "cdata.for"
      COMMON /CWORK1/ NT,T(1500),NV,V(200)
      DIMENSION IV(2),IT(2)
      EQUIVALENCE (V(1),IV(1)),(T(1),IT(1))
C
      IF(NV.EQ.0) GO TO 100
C****
C
      NBNK = IDATA(IPPATR-2)
      NWRES = 2 + NV*10 + NT*15
      CALL CCRE(IPHT,'GVTX',NBNK,NWRES,IERR)
      IF(IERR.NE.0) GO TO 100
      CALL BSAW(1,'GVTX')
      IDATA(IPHT+1) = NV
      DO 1  INV = 1,NV
      INVX = IPHT+1+(INV-1)*10
      ISS = (INV-1)*10
      DO 1  I = 1,10
  1   IDATA(INVX+I) = IV(ISS+I)
      IDATA(IPHT+2+NV*10) = NT
C
      DO 2  INT = 1,NT
      INTX = IPHT+2+NV*10 + (INT-1)*15
      ISS = (INT-1)*30
      DO 2  I = 1,15
  2   IDATA(INTX+I) = IT(ISS+I)
  100 RETURN
      END
C   09/01/80 006251729  MEMBER NAME  VTXEE    (JADEGS)      FORTRAN
      SUBROUTINE VTXEE
C*800604*DITTMANN********************************************
C*                                                          *
C*              X - Y - Z    E E   P A I R S                *
C*                                                          *
C*       A DESCRIPTION OF THE T AND V ARRAYS CAN BE FOUND   *
C*       IN SUBR. VERTEX                                    *
C************************************************************
      COMMON /CWORK1/ NT,T(1500),NV,V(200)
      DIMENSION IT(2),IV(2)
      EQUIVALENCE (T(1),IT(1)),(V(1),IV(1))
C        CONSTANTS
      COMMON /CVTXC/ XB,YB,ZB,RTANK,DTANK,X0INN,SIGX0,SIGZ0,PNTMIN,
     *               DISTB,COLL2,MITER,DSCONV,PRCUT,IREJTR,EEDPMN,
     *               EEDPMX,EEDTMX,EEDRMX,SEMAX,SIMAX,SIGFAC,EEXYMN,
     *               EEXYMX
C
      IF(NT.LE.1) GOTO 100
C****
C****    LOOP OVER ALL TRACK PAIRS
      J1 = 0
      NT1 = NT - 1
      DO 9 I=1,NT1
      IF(IT(J1+1).EQ.0) GOTO 9
      M = I + 1
      J2 = I*30
      DO 8 K=M,NT
      IF(IT(J2+1).EQ.0) GOTO 8
C        OPPOSITE CHARGE
      IF(T(J1+2)*T(J2+2).GT.0.) GOTO 8
C        MEASURED R-PHI OPENING
      DPHI = T(J1+3) - T(J2+3)
      IF(T(J1+2).LT.0.) DPHI=-DPHI
      IF(ABS(DPHI).GT.3.141593) DPHI=DPHI-SIGN(1.,DPHI)*6.283185
      IF(DPHI.LT.EEDPMN .OR. DPHI.GT.EEDPMX) GOTO 8
      ITANK = 0
      XM1 = T(J1+5) - T(J1+2)*T(J1+21)
      YM1 = T(J1+6) + T(J1+2)*T(J1+22)
      XM2 = T(J2+5) - T(J2+2)*T(J2+21)
      YM2 = T(J2+6) + T(J2+2)*T(J2+22)
C        EXTRAPOLATION LENGTH
      IF(T(J1+27).EQ.0. .OR. T(J2+27).EQ.0.) GOTO 6
      IF(T(J1+27).LT.SEMAX .OR. T(J2+27).LT.SEMAX) GOTO 6
C        PAIR ORIGIN IN TANK (ADD SQRT(2/3)*COULOMB ERROR)
      ST1 = T(J1+27)
      ST2 = T(J2+27)
      CALL VTXS(J1,ST1,XT1,YT1,ZT1,DXT12,DYT12,DZT12,PHIT1)
      CALL VTXS(J2,ST2,XT2,YT2,ZT2,DXT22,DYT22,DZT22,PHIT2)
C        DISTANCE IN TANK
      DPHI = PHIT1 - PHIT2
      IF(T(J1+2).LT.0.) DPHI=-DPHI
      IF(ABS(DPHI).GT.3.141593) DPHI=DPHI-SIGN(1.,DPHI)*6.283185
      PTANK2 = (T(J1+16)**2+T(J2+16)**2) / 1.5
      STANK2 = 49. * PTANK2
      DX2 = (XT1-XT2)**2
      DY2 = (YT1-YT2)**2
      DXY = SQRT(DX2+DY2)
      DIST = SQRT((XT2-XM1)**2+(YT2-YM1)**2)
      IF(DIST.LT.ABS(T(J1+2))) DXY=-DXY
      DZ = ABS(ZT1-ZT2)
      DTH = ABS(T(J1+4)-T(J2+4))
      SDTH = DTH / SQRT(T(J1+9)**2+T(J2+9)**2+PTANK2)
      IF(SDTH.GT.EEDTMX) GOTO 6
      CTH = COS((T(J1+4)+T(J2+4))/2.)**2
      SDZ = DZ / SQRT(DZT12+DZT22+STANK2/CTH**2)
      IF(SDZ.GT.EEDTMX) GOTO 6
      SDPHI = ABS(DPHI)/SQRT(T(J1+8)**2+T(J2+8)**2+PTANK2)
      IF(SDPHI.GT.EEDRMX) GOTO 5
      SDXY = ABS(DXY) / SQRT(1./(1./DXT12+1./DYT12)+
     *                       1./(1./DXT22+1./DYT22)+STANK2)
      IF(SDXY.GT.EEDRMX .AND. (DXY.LT.0..OR.DXY.GT.EEXYMN)) GOTO 5
      ITANK = 1
      GOTO 6
C        DISTANCE AT BEAM PIPE
    5 DPHIT = DTANK/ABS(T(J1+2)) + DTANK/ABS(T(J2+2))
      DPHI = DPHI - DPHIT
      SDPHI = ABS(DPHI)/SQRT(T(J1+8)**2+T(J2+8)**2+PTANK2)
      IF(SDPHI.GT.EEDRMX) GOTO 6
      DXY = DXY - DPHIT*DTANK/2.
      SDXY = ABS(DXY) / SQRT(1./(1./DXT12+1./DYT12) +
     *             1./(1./DXT22+1./DYT22) + STANK2)
      IF(SDXY.GT.EEDRMX .AND. (DXY.LT.0..OR.DXY.GT.EEXYMN)) GOTO 6
      ITANK = 1
C        VERTEX WHERE TRACKS ARE PARALLEL
    6 CALL VTXPNT(J1,XM2,YM2,XT1,YT1,ZT1,DXT12,DYT12,DZT12,PHIT1,ST1)
      CALL VTXPNT(J2,XM1,YM1,XT2,YT2,ZT2,DXT22,DYT22,DZT22,PHIT2,ST2)
      DX2 = (XT1-XT2)**2
      DY2 = (YT1-YT2)**2
      DXY = SQRT(DX2+DY2)
      IF(DXY.GT.EEXYMX) GOTO 8
      DIST = SQRT((XT2-XM1)**2+(YT2-YM1)**2)
      IF(DIST.LT.ABS(T(J1+2))) DXY=-DXY
      DZ = ABS(ZT1-ZT2)
      IF(ITANK.EQ.1) GOTO 7
C        PAIR ORIGIN IN CHAMBER
      IF(ST1.LT.SEMAX .OR. ST2.LT.SEMAX) GOTO 8
      IF(ST1.GT.SIMAX .OR. ST2.GT.SIMAX) GOTO 8
      IF(T(J1+27).NE.0. .AND. ST1-T(J1+27).LT.0.) GOTO 8
      IF(T(J2+27).NE.0. .AND. ST2-T(J2+27).LT.0.) GOTO 8
C        DISTANCE IN CHAMBER
      DTH = ABS(T(J1+4)-T(J2+4))
      SDTH = DTH / SQRT(T(J1+9)**2+T(J2+9)**2)
      IF(SDTH.GT.EEDTMX) GOTO 8
      SDZ = DZ / SQRT(DZT12+DZT22)
      IF(SDZ.GT.EEDTMX) GOTO 8
      SDXY = ABS(DXY)/SQRT(1./(1./DXT12+1./DYT12)+1./(1./DXT22+
     *       1./DYT22))
      IF(SDXY.GT.EEDRMX .AND. (DXY.LT.0..OR.DXY.GT.EEXYMN)) GOTO 8
C
    7 IF(NV.EQ.20) GOTO 14
      NV = NV + 1
      LV = (NV-1)*10
      IV(LV+1) = 4
      DT1 = SQRT(DXT12)
      DT2 = SQRT(DXT22)
      V(LV+2) = (XT1/DT1+XT2/DT2) / (1./DT1+1./DT2)
      DT1 = SQRT(DYT12)
      DT2 = SQRT(DYT22)
      V(LV+3) = (YT1/DT1+YT2/DT2) / (1./DT1+1./DT2)
      DT1 = SQRT(DZT12)
      DT2 = SQRT(DZT22)
      V(LV+4) = (ZT1/DT1+ZT2/DT2) / (1./DT1+1./DT2)
      V(LV+5) = SQRT(DXT12+DXT22) / 2.
      V(LV+6) = SQRT(DYT12+DYT22) / 2.
      V(LV+7) = SQRT(DZT12+DZT22) / 2.
      V(LV+8) = SQRT(DX2+DY2)
      IV(LV+9) = I
      IV(LV+10) = K
    8 J2 = J2 + 30
    9 J1 = J1 + 30
C****
C****    CLEAN UP
      IF(NV.EQ.0) GOTO 100
   14 I = 1
   15 LV1 = (I-1)*10
      IF(I.EQ.NV) GOTO 19
      M = I + 1
      LV2 = I*10
      DO 16 K=M,NV
      IF(IV(LV1+9).EQ.IV(LV2+9) .OR. IV(LV1+10).EQ.IV(LV2+9) .OR.
     *   IV(LV1+10).EQ.IV(LV2+10)) GOTO 17
   16 LV2 = LV2 + 10
      GOTO 19
C        TWO PAIRS WITH SAME TRACK, TAKE PAIR WITH SMALLER DISTANCE
   17 M = I
      IF(V(LV1+8).LT.V(LV2+8)) M=K
      NV = NV - 1
      IF(M.GT.NV) GOTO 15
      DO 18 K=M,NV
      LV2 = K*10
      LV1 = LV2 - 10
      DO 18 J=1,10
   18 V(LV1+J) = V(LV2+J)
      GOTO 15
   19 J1 = (IV(LV1+9)-1)*30
      J2 = (IV(LV1+10)-1)*30
      IV(LV1+8) = 2
      V(LV1+9) = 0.
      IV(LV1+10) = 2
      IT(J1+1) = 3
      IT(J2+1) = 3
      IT(J1+14) = I
      IT(J2+14) = I
      I = I + 1
      IF(I.LE.NV) GOTO 15
C
  100 RETURN
      END
C   04/07/79 006251728  MEMBER NAME  VTXINI   (JADEGS)      FORTRAN
      SUBROUTINE VTXINI
C*800623*DITTMANN***************************************************
C*                                                                 *
C* I N I T I A L I S A T I O N   O F   X Y Z   V E R T E X   F I T *
C*                                                                 *
C*       TO BE CALLED ONCE AND BEFORE FIRST CALL TO VTXPRE, VERTEX *
C*******************************************************************
      COMMON /CVTXC/ XB,YB,ZB,RTANK,DTANK,X0INN,SIGX0,SIGZ0,PNTMIN,
     *               DISTB,COLL2,MITER,DSCONV,PRCUT,IREJTR,EEDPMN,
     *               EEDPMX,EEDTMX,EEDRMX,SEMAX,SIMAX,SIGFAC,EEXYMN,
     *               EEXYMX,PHEMAX,SIG1,SIG2,SIG3,CSECV
C
C        MEAN VERTEX COORDINATES
      XB = 0.
      YB = 0.
      ZB = 0.
C        OUTER RADIUS OF INNER TANK WALL
      RTANK = 174.
C        OUTER DISTANCE BEAM PIPE TO TANK WALL
      DTANK = 50.
C        RADIATION LENGTH BETWEEN BEAM AND FIRST WIRE
      X0INN = 0.16
C        MEAN TRACK RESIDUAL IN XY AND ZR PLANE
      SIGX0 = 0.55
      SIGZ0 = 30.
C        ARTIFICIAL FACTOR TO ACCOUNT FOR SYSTEMATIC ERRORS
      SIGFAC = 2.0
      SIGX0 = SIGX0 * SIGFAC
      SIGZ0 = SIGZ0 * SIGFAC
C        MINIMUM NUMBER OF POINTS IN XY AND Z TRACK
      PNTMIN = 5.
C        MAXIMUM TRACK RESIDUAL IN XY AND Z (SIG<SIG1*SIGX0)
      SIG1 = 4.
C        MAXIMUM DISTANCE OF TRACKS TO AVERAGE BEAM
C        USED IN PRIMARY VERTEX SEARCH
      DISTB = 20.
C        MAXIMUM OPENING ANGLE OF COLLINEAR 2-PRONGS
      COLL2 = 0.98
C        MAXIMUM NUMBER OF ITERATIONS IN VERTEX FIT
      MITER = 7
C        CONVERGENCE PARAMETER
      DSCONV = 0.1
C        MINIMUM PROBABILITY FOR GOOD VERTEX
      PRCUT = 0.001
C        REJECT BAD TRACKS DURING VERTEX FIT (0=NO, 1=YES)
      IREJTR = 1
C        EE PAIRS: MINIMUM AND MAXIMUM MEASURED PHI DIFFERENCE (RADIAN)
      EEDPMN = -0.07
      EEDPMX =  0.8
C        EE PAIRS: MAXIMUM THETA DIFFERENCE (STD.DEV.)
      EEDTMX = 3.0
C        EE PAIRS: MAXIMUM DISTANCE WHERE TRACKS ARE PARALLEL (STD.DEV.)
      EEDRMX = 3.0
C        EE PAIRS: MAXIMUM POSITIVE XY DISTANCE, NO ST.DEV. CUT (MM)
      EEXYMN = 4.0
C        EE PAIRS: MAXIMUM XY DISTANCE WHERE TRACKS ARE PARALLEL (MM)
      EEXYMX = 20.
C        MAXIMUM TRACK EXTRAPOLATION AND INTRAPOLATION(ARC LENGTH)
      SEMAX = -300.
      SIMAX = 25.
C        MAXIMUM TRACK EXTRAPOLATION (PHI)
      PHEMAX = 1.0
C        MAXIMUM TRACK DISTANCE TO VERTEX DURING FIT (STD.DEV.)
      SIG2 = 2.
C        MAXIMUM TRACK DISTANCE TO VERTEX AFTER FIT (STD.DEV.)
      SIG3 = 4.
C        SEC. VERTICES: MINIMUM COS OF TRACK TO MAIN VERTEX (OR XB,YB)
      CSECV = -0.15
C
      RETURN
      END
C   11/12/79 006251730  MEMBER NAME  VTXPNT   (JADEGS)      FORTRAN
      SUBROUTINE VTXPNT(J,XP,YP,XT,YT,ZT,DXT2,DYT2,DZT2,PHIT,ST)
C*800623*DITTMANN************************************************
C*                                                              *
C*     S U P P O R T   F O R   X Y Z   V E R T E X   F I T      *
C*                                                              *
C*       COMMON CWORK1 MUST BE FILLED PROPERLY                  *
C*                                                              *
C*       ENTRY: VTXS                                            *
C****************************************************************
      COMMON /CWORK1/ NT,T(1500)
C
C        ENTRY VTXPNT
C****    CALCULATE POINT XT,YT,ZT ON HELIX J WHICH IS NEAREST
C****    TO POINT XP,YP IN X-Y PROJECTION
      XM = T(J+5) - T(J+2)*T(J+21)
      YM = T(J+6) + T(J+2)*T(J+22)
      SR = SIGN(1.,T(J+2))
      PHIT = ATAN2(-SR*(XM-XP),SR*(YM-YP))
      THETA = PHIT - T(J+3)
      IF(ABS(THETA).GT.3.141593) THETA=THETA-SIGN(1.,THETA)*6.283185
      ST = T(J+2)*THETA
      SPHI = SIN(PHIT)
      CPHI = COS(PHIT)
      XT = XM + T(J+2)*SPHI
      YT = YM - T(J+2)*CPHI
      ZT = T(J+7) + ST*T(J+23)
      SSPACE = ST
      SSCOUL = ST-T(J+27)
      IF(SSCOUL.GT.0.) SSCOUL=0.
      DXY = T(J+10)**2 + (T(J+8)*SSPACE)**2 + (T(J+16)*SSCOUL)**2
      DZ = (T(J+9)*SSPACE)**2 + (T(J+16)*SSCOUL)**2
      DXYLIM = 1. / (2.*ABS(T(J+2))*SQRT(DXY))
      DXT2 = SPHI**2 / DXY
      DYT2 = CPHI**2 / DXY
      IF(DXT2.LT.DXYLIM) DXT2=DXYLIM
      IF(DYT2.LT.DXYLIM) DYT2=DXYLIM
      DXT2 = 1. / DXT2
      DYT2 = 1. / DYT2
      DZT2 = T(J+12)**2 + DZ/(T(J+24)**2)**2
      RETURN
C
         ENTRY VTXS(J,S,XT,YT,ZT,DXT2,DYT2,DZT2,PHIT)
C****    CALCULATE POINT XT,YT,ZT ON HELIX J FOR A GIVEN
C****    PROJECTED ARC LENGTH
      THETA = S / T(J+2)
      PHIT = T(J+3) + THETA
      SPHI = SIN(PHIT)
      CPHI = COS(PHIT)
      XT = T(J+5) + T(J+2)*(SPHI-T(J+21))
      YT = T(J+6) - T(J+2)*(CPHI-T(J+22))
      ZT = T(J+7) + S*T(J+23)
      SSPACE = S
      SSCOUL = S-T(J+27)
      IF(SSCOUL.GT.0.) SSCOUL=0.
      DXY = T(J+10)**2 + (T(J+8)*SSPACE)**2 + (T(J+16)*SSCOUL)**2
      DZ = (T(J+9)*SSPACE)**2 + (T(J+16)*SSCOUL)**2
      DXYLIM = 1. / (2.*ABS(T(J+2))*SQRT(DXY))
      DXT2 = SPHI**2 / DXY
      DYT2 = CPHI**2 / DXY
      IF(DXT2.LT.DXYLIM) DXT2=DXYLIM
      IF(DYT2.LT.DXYLIM) DYT2=DXYLIM
      DXT2 = 1. / DXT2
      DYT2 = 1. / DYT2
      DZT2 = T(J+12)**2 + DZ/(T(J+24)**2)**2
      RETURN
C
      END
C   07/11/79 006251731  MEMBER NAME  VTXSRC   (JADEGS)      FORTRAN
      SUBROUTINE VTXSRC
C*800623*DITTMANN********************************************
C*                                                          *
C*        X - Y - Z    V E R T E X    S E A R C H           *
C*                                                          *
C*       1. SEARCH FOR E+E- PAIRS                           *
C*       2. SEARCH FOR MAIN VERTEX AROUND AVERAGE BEAM      *
C*       3. SEARCH FOR SECONDARY VERTICES                   *
C*                                                          *
C*       A DESCRIPTION OF THE T AND V ARRAYS CAN BE FOUND   *
C*       IN SUBR. VERTEX                                    *
C************************************************************
      COMMON /CWORK1/ NT,T(1500),NV,V(200),XXXXX(151),
     *                JTGOD(50),JTBAD(50),VSAVE(10),V2(20,7)
      DIMENSION IT(2),IV(2),IV2(20,7)
      EQUIVALENCE (T(1),IT(1)),(V(1),IV(1)),(V2(1,1),IV2(1,1))
C        CONSTANTS
      COMMON /CVTXC/ XB,YB,ZB,RTANK,DTANK,X0INN,SIGX0,SIGZ0,PNTMIN,
     *               DISTB,COLL2,MITER,DSCONV,PRCUT,IREJTR,EEDPMN,
     *               EEDPMX,EEDTMX,EEDRMX,SEMAX,SIMAX,SIGFAC,EEXYMN,
     *               EEXYMX,PHEMAX,SIG1,SIG2,SIG3,CSECV
C
      LOGICAL VREP,RESTOR
C
      NV = 0
      IF(NT.LE.0) RETURN
      IPRVTX = 0
C****
C****    SEARCH FOR GAMMA CONVERSIONS
      CALL VTXEE
      IF(NV.EQ.0) GOTO 5
      LV = 0
      DO 4 I=1,NV
      J = 0
      DO 3 K=1,NT
      IF(IT(J+14).EQ.I) IT(J+1)=-IT(J+1)
    3 J = J + 30
    4 LV = LV + 10
C****
C****    SEARCH FOR PRIMARY VERTEX AROUND AVERAGE BEAM
    5 NTGOD = 0
      NTBAD = 0
      J = 0
      DO 7 I=1,NT
      IF(IT(J+1).LE.0) GOTO 7
      CALL VTXPNT(J,XB,YB,XT,YT,ZT,DXT2,DYT2,DZT2,PHI,SS)
      DXY = SQRT((XT-XB)**2+(YT-YB)**2)
      IF(DXY.GT.DISTB) GOTO 6
      IF(SS.LT.SEMAX-RTANK) GOTO 6
      NTGOD = NTGOD + 1
      JTGOD(NTGOD) = J
      GOTO 7
    6 IT(J+1) = 1
      NTBAD = NTBAD + 1
      JTBAD(NTBAD) = J
    7 J = J + 30
      IF(NTGOD.EQ.0) GOTO 39
      IF(NV.EQ.20) GOTO 90
      NV = NV + 1
      LV = (NV-1)*10
      VREP = .FALSE.
      RESTOR = .FALSE.
C****
C****    VERTEX FIT
   10 CALL VERTEX
      IF(IV(LV+1).GE.1) GOTO 20
      IF(VREP) GOTO 60
      GOTO 40
C****
C****    CHECK VERTEX AND ITS TRACKS
   20 NTBAD = 0
      NTBAD3 = 0
      NTGOD = 0
      NTGOD3 = 0
      J = 0
      DO 25 I=1,NT
      IF(IT(J+1).LE.0) GOTO 25
C        SMALLEST DISTANCE TO VERTEX
      CALL VTXPNT(J,V(LV+2),V(LV+3),XT,YT,ZT,DXT2,DYT2,DZT2,PHI,SS)
      DXT2 = DXT2 + V(LV+5)**2
      DYT2 = DYT2 + V(LV+6)**2
      DZT2 = DZT2 + V(LV+7)**2
      SDX = ABS(XT-V(LV+2)) / SQRT(DXT2)
      SDY = ABS(YT-V(LV+3)) / SQRT(DYT2)
      SDZ = ABS(ZT-V(LV+4)) / SQRT(DZT2)
      T(J+15) = SS
      T(J+28) = SDX
      T(J+29) = SDY
      T(J+30) = SDZ
      IF(IT(J+1).EQ.1) GOTO 23
      IF(SDX.GT.SIG2 .OR. SDY.GT.SIG2 .OR. SDZ.GT.SIG2) GOTO 23
C        EXTRAPOLATION LENGTH
      IF(SS.GT.SIMAX) GOTO 23
      SSJC = SS
      IF(T(J+27).EQ.0.) GOTO 22
      IF(SS.LT.T(J+26)-SIMAX) GOTO 23
      IF(SS.LT.T(J+27)) SSJC=T(J+27)
   22 IF(SSJC.LT.SEMAX) GOTO 23
      IF(ABS(SS/T(J+2)).GT.PHEMAX) GOTO 23
      NTGOD = NTGOD + 1
      JTGOD(NTGOD) = J
      IF(IT(J+1).EQ.3) NTGOD3=NTGOD3+1
      GOTO 25
   23 NTBAD = NTBAD + 1
      JTBAD(NTBAD) = J
      IF(IT(J+1).EQ.3) NTBAD3=NTBAD3+1
   25 J = J + 30
      IF(RESTOR) GOTO 30
      IF(NTGOD3.EQ.7) GOTO 30
      IF(NTBAD3.EQ.0 .AND. NTGOD.EQ.NTGOD3) GOTO 30
      IF(.NOT.VREP .AND. NTGOD.LT.3) GOTO 40
      IF(NTGOD.LE.2) GOTO 60
C        REPEAT VERTEX FIT
      DO 27 I=1,NTGOD
      J = JTGOD(I)
   27 IT(J+1) = 2
      IF(NTBAD.EQ.0) GOTO 10
      DO 28 I=1,NTBAD
      J = JTBAD(I)
   28 IT(J+1) = 1
      GOTO 10
C****
C****    GOOD VERTEX FOUND
   30 IF(NTBAD.EQ.0) GOTO 37
      IF(IV(LV+8).EQ.1) GOTO 37
C        COLLECT ALL TRACKS FITTING TO VERTEX
      NTB = 0
      DO 33 I=1,NTBAD
      J = JTBAD(I)
      IF(T(J+28).GT.SIG3.OR.T(J+29).GT.SIG3.OR.T(J+30).GT.SIG3) GOTO 32
      IF(T(J+15).GT.SIMAX) GOTO 32
      SSJC = T(J+15)
      IF(T(J+27).EQ.0.) GOTO 31
      IF(T(J+15).LT.T(J+26)-SIMAX) GOTO 32
      IF(T(J+15).LT.T(J+27)) SSJC=T(J+27)
   31 IF(SSJC.LT.SEMAX) GOTO 32
      IF(ABS(T(J+15)/T(J+2)).GT.PHEMAX) GOTO 32
      NTGOD = NTGOD + 1
      JTGOD(NTGOD) = J
      IT(J+1) = 1
      GOTO 33
   32 NTB = NTB + 1
      JTBAD(NTB) = J
   33 CONTINUE
      NTBAD = NTB
   37 IV(LV+10) = NTGOD
      DO 38 I=1,NTGOD
      J = JTGOD(I)
      IT(J+1) = -IT(J+1)
      IT(J+14) = NV
   38 CONTINUE
      IF(IPRVTX.NE.0) GOTO 39
      IF(ABS(V(LV+2)-XB).LT.3.*DISTB .AND. ABS(V(LV+3)-YB).LT.3.*DISTB)
     *   IPRVTX=NV
   39 IF(NTBAD.EQ.0) GOTO 90
      NTGOD = 0
      IF(NV.EQ.20) GOTO 90
      NV = NV + 1
      LV = (NV-1)*10
C****
C****    VERTEX SEARCH
   40 IF(NTBAD.EQ.0) GOTO 241
      DO 41 I=1,NTBAD
      J = JTBAD(I)
   41 IT(J+1) = 1
  241 IF(NTGOD.EQ.0) GOTO 43
      DO 141 I=1,NTGOD
      J = JTGOD(I)
      NTBAD = NTBAD + 1
      JTBAD(NTBAD) = J
  141 IT(J+1) = 1
   43 NVL = NV
      NV2 = 0
      N1 = 1
      IF(NTBAD.LT.2) GOTO 50
C        TWO TRACK VERTEX
   42 J1 = JTBAD(N1)
      IT(J1+1) = 2
      N21 = N1 + 1
      IF(N21.GT.NTBAD) GOTO 45
      DO 44 N2=N21,NTBAD
      J2 = JTBAD(N2)
      IT(J2+1) = 2
      CALL VERTEX
      IF(IV(LV+1).LE.1) GOTO 44
C        CHECK THIS VERTEX
C        EXTRAPOLATION LENGTH TRACK 1
      CALL VTXPNT(J1,V(LV+2),V(LV+3),XT1,YT1,ZT1,DXT12,DYT12,DZT12,
     *            PHIT1,ST1)
      IF(ST1.GT.SIMAX) GOTO 44
      SSJC = ST1
      IF(T(J1+27).EQ.0.) GOTO 143
      IF(ST1.LT.T(J1+26)-SIMAX) GOTO 44
      IF(ST1.LT.T(J1+27)) SSJC=T(J1+27)
  143 IF(SSJC.LT.SEMAX) GOTO 44
      IF(ABS(ST1/T(J1+2)).GT.PHEMAX) GOTO 44
C        EXTRAPOLATION LENGTH TRACK 2
      CALL VTXPNT(J2,V(LV+2),V(LV+3),XT2,YT2,ZT2,DXT22,DYT22,DZT22,
     *            PHIT2,ST2)
      IF(ST2.GT.SIMAX) GOTO 44
      SSJC = ST2
      IF(T(J2+27).EQ.0.) GOTO 243
      IF(ST2.LT.T(J2+26)-SIMAX) GOTO 44
      IF(ST2.LT.T(J2+27)) SSJC=T(J2+27)
  243 IF(SSJC.LT.SEMAX) GOTO 44
      IF(ABS(ST2/T(J2+2)).GT.PHEMAX) GOTO 44
C        COMPARE TRACK DIRECTIONS WITH BEAM
      DZ = ABS(ZT1-ZT2)
      COSW12 = 0.
      IF(IPRVTX.EQ.0) GOTO 147
      LPV = (IPRVTX-1)*10
      DVX = V(LV+2) - V(LPV+2)
      DVY = V(LV+3) - V(LPV+3)
      DVZ = V(LV+4) - V(LPV+4)
      DV12 = SQRT(DVX**2+DVY**2+DVZ**2)
      COSW1 = (COS(PHIT1)*T(J1+24)*DVX + SIN(PHIT1)*T(J1+24)*DVY +
     *         SIN(T(J1+4))*DVZ) / DV12
      IF(COSW1.LT.CSECV) GOTO 44
      COSW2 = (COS(PHIT2)*T(J2+24)*DVX + SIN(PHIT2)*T(J2+24)*DVY +
     *        SIN(T(J2+4))*DVZ) / DV12
      IF(COSW2.LT.CSECV) GOTO 44
      COSW12 = COSW1 * COSW2
      GOTO 47
  147 DVX = V(LV+2) - XB
      DVY = V(LV+3) - YB
      DV12 = SQRT(DVX**2+DVY**2)
      IF(DV12.LT.DISTB) GOTO 47
      COSW1 = (COS(PHIT1)*DVX + SIN(PHIT1)*DVY) / DV12
      IF(COSW1.LT.CSECV) GOTO 44
      COSW2 = (COS(PHIT2)*DVX + SIN(PHIT2)*DVY) / DV12
      IF(COSW2.LT.CSECV) GOTO 44
      COSW12 = COSW1 * COSW2
C        CHECK IF MORE TRACKS FIT
   47 NT2 = 2
      DO 149 K=1,NTBAD
      J = JTBAD(K)
      IF(IT(J+1).EQ.3) GOTO 149
      CALL VTXPNT(J,V(LV+2),V(LV+3),XT,YT,ZT,DXT2,DYT2,DZT2,PHI,SS)
      SDX = ABS(XT-V(LV+2)) / SQRT(DXT2)
      SDY = ABS(YT-V(LV+3)) / SQRT(DYT2)
      SDZ = ABS(ZT-V(LV+4)) / SQRT(DZT2)
      IF(SDX.GT.SIG2 .OR. SDY.GT.SIG2 .OR. SDZ.GT.SIG2) GOTO 149
      IF(SS.GT.SIMAX) GOTO 149
      SSJC = SS
      IF(T(J+27).EQ.0.) GOTO 148
      IF(SS.LT.T(J+26)-SIMAX) GOTO 149
      IF(SS.LT.T(J+27)) SSJC=T(J+27)
  148 IF(SSJC.LT.SEMAX) GOTO 149
      IF(ABS(SS/T(J+2)).GT.PHEMAX) GOTO 149
      NT2 = NT2 + 1
  149 CONTINUE
C        TEMPORARY STORE OF ALL TWO TRACK VERTICES
      NV2 = NV2 + 1
      IV2(NV2,1) = NV
      IV2(NV2,2) = NT2
      IV2(NV2,3) = J1
      IV2(NV2,4) = J2
      V2(NV2,5) = COSW12
      V2(NV2,6) = DZ
      IV2(NV2,7) = IV(LV+1)
      IF(NV.EQ.20) GOTO 45
      NV = NV + 1
      LV = (NV-1)*10
C
   44 IT(J2+1) = 1
      IT(J1+1) = 1
      N1 = N1 + 1
      IF(N1.LE.NTBAD) GOTO 42
C        CHOOSE BEST TWO TRACK VERTEX
   45 IT(J1+1) = 1
      IT(J2+1) = 1
      IF(NV2.EQ.0) GOTO 50
      NT2MAX = 0
      DO 46 I=1,NV2
      IF(IV2(I,2).LT.NT2MAX) GOTO 46
      IF(IV2(I,2).EQ.NT2MAX) GOTO 144
      NT2MAX = IV2(I,2)
      LV2 = I
      GOTO 46
  144 IF(IV2(I,7).EQ.IV2(LV2,7)) GOTO 145
      IF(IV2(I,7).GT.IV2(LV2,7)) LV2=I
      GOTO 46
  145 IF(IPRVTX.EQ.0) GOTO 146
      IF(V2(I,5).GT.V2(LV2,5)) LV2=I
      GOTO 46
  146 IF(V2(I,6).LT.V2(LV2,6)) LV2=I
   46 CONTINUE
C        REPEAT VERTEX FIT
      J1 = IV2(LV2,3)
      J2 = IV2(LV2,4)
      NV = NVL
      LV = (NVL-1)*10
      LV2 = (IV2(LV2,1)-1)*10
      J1SAVE = J1
      J2SAVE = J2
      DO 48 K=1,10
      VSAVE(K) = V(LV2+K)
   48 V(LV+K) = V(LV2+K)
      DO 49 K=1,NTBAD
      J = JTBAD(K)
      IT(J+1)=2
   49 CONTINUE
      IT(J1+1) = 3
      IT(J2+1) = 3
      VREP = .TRUE.
      RESTOR = .FALSE.
      GOTO 20
C        SINGLE TRACK VERTEX
   50 DO 52 N1=1,NTBAD
      J1 = JTBAD(N1)
      IF(N1.EQ.1) GOTO 51
      IF(NV.EQ.20) GOTO 90
      NV = NV + 1
      LV = (NV-1)*10
   51 IV(LV+1) = 5
      V(LV+2) = T(J1+5)
      V(LV+3) = T(J1+6)
      V(LV+4) = T(J1+7)
      V(LV+5) = T(J1+10)
      V(LV+6) = T(J1+11)
      V(LV+7) = T(J1+12)
      IV(LV+8) = 1
      V(LV+9) = 0.
      IV(LV+10) = 1
      IT(J1+1) = -3
      IT(J1+14) = NV
   52 CONTINUE
      GOTO 90
C        SEARCH NOT CONVERGING
C        EMERGENCY ACTION: RESTORE 2-TRACK VERTEX (OTHERWISE DEAD LOOP)
   60 J1 = J1SAVE
      J2 = J2SAVE
      DO 62 I=1,10
   62 V(LV+I) = VSAVE(I)
      IF(NTBAD.EQ.0) GOTO 66
      DO 63 I=1,NTBAD
      J = JTBAD(I)
      IT(J+1)=2
   63 CONTINUE
   66 IF(NTGOD.EQ.0) GOTO 69
      DO 67 I=1,NTGOD
      J = JTGOD(I)
      IT(J+1)=2
   67 CONTINUE
   69 IT(J1+1) = 3
      IT(J2+1) = 3
      RESTOR = .TRUE.
      GOTO 20
C****
C****    CLEAN UP
   90 IF(NV.EQ.0) GOTO 100
      J = 0
      DO 91 I=1,NT
      IF(IT(J+1).LT.0) IT(J+1)=-IT(J+1)
   91 J = J + 30
      IF(NV.EQ.1) GOTO 100
      IF(IPRVTX.NE.0) GOTO 93
      IPRVTX = 1
      DXYP = 1.E20
      LV = 0
      DO 92 I=1,NV
      DXY = V(LV+2)**2+V(LV+3)**2
      IF(DXY.GT.DXYP) GOTO 92
      IPRVTX = I
      DXYP = DXY
   92 LV = LV + 10
   93 IF(IPRVTX.EQ.1) GOTO 100
C        MOVE PRIMARY VERTEX TO FIRST POSITION
      M = IPRVTX - 1
      LV = M*10
      DO 94 I=1,10
   94 VSAVE(I) = V(LV+I)
      DO 96 I=1,M
      DO 95 J=1,10
   95 V(LV+J) = V(LV+J-10)
   96 LV = LV - 10
      DO 97 I=1,10
   97 V(I) = VSAVE(I)
      J = 0
      DO 99 I=1,NT
      IF(IT(J+1).EQ.0) GOTO 99
      IF(IT(J+14).EQ.IPRVTX) GOTO 98
      IF(IT(J+14).LT.IPRVTX) IT(J+14)=IT(J+14)+1
      GOTO 99
   98 IT(J+14) = 1
   99 J = J + 30
C
  100 RETURN
      END
