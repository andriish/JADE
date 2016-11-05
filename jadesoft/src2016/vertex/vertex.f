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
     +                CHITR(7)
      DIMENSION IT(2),IV(2)
      EQUIVALENCE (T(1),IT(1)),(V(1),IV(1))
      REAL*8 A,B,DET
C        CONSTANTS
      COMMON /CVTXC/ XB,YB,ZB,RTANK,DTANK,X0INN,SIGX0,SIGZ0,PNTMIN,
     +               DISTB,COLL2,MITER,DSCONV,PRCUT,IREJTR
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
     +     + SIN(T(J1+4))*SIN(T(J2+4))
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
