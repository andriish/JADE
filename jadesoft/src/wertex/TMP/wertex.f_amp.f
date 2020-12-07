C   18/05/79 807031524  MEMBER NAME  WERTEX   (JADEGS)      FORTRAN
      SUBROUTINE VERTEX 
C*800623*DITTMANN****************************************************
C*  COPIED FROM F22KLE.VERTEX.S(VERTEX)   18.9.86
C*                                                                  *
C*    THIS VERSION WAS INSTALLED AS A FORERUNNER OF THE NEW VERTEX- *
C*    PACKAGE. THE LATTER WAS RELEASES IN WINTER 87/88 AND DEVIATES *
C*    CONSIDERABLY FROM THE PRESENT MEMBER WERTEX. SINCE WERTEX IS  *
C*    STILL USED FOR BACKWARD COMPATIBILITY, IT IS KEPT ON JADEGS/GL*
C*    THERE IS HOWEVER NOW A CONFLICT WITH THE OLD MACRO MEMBERS ON *
C*    F11GOD.PATRECSR: THESE ARE THE ONES USED IN THIS MEMBER WERTEX*
C*    AND ARE DIFFERENT FROM THE MACROS WITH THE SAME NAMES WHICH   *
C*    ARE SITUATED ON F22KLE.VERTEX.S AND USED IN THE NEW VERSIONS  *
C*    OF THE ROUTINES. FOR THIS REASONS, ALL MACROS IN THIS MEMBER  *
C*    WERE EXPANDED ON 3.7.88. A ROUTINE USED INTERNALLY ONLY HAS   *
C*    BEEN RENAMED:   VTXCRV IS NOW WTXCRV                          *
C*                                                 J.O.  3.7.88     *
C*                                                                  *
C*  UPDATES :  860217 C.K. : PROPER ERRORS DDX,DDY (SPHI,CPHI)      *
C*                           (SPHI,CPHI -DEPENDENCE) INTRODUCED     *
C*                                                                  *
C*  UPDATES :  860316 C.K. : PROPER ERRORS DXY,DPHI INTRODUCED      *
C*                           ( ACCOUNTING OF CURVATURE-ERROR )      *
C*                                                                  *
C*  MODIFICATION OF 20.5.86                  KLE/RAM                *
C*  MAX NO OF TRACKS FOR VERTEX-FIT NOW 20 ( OLD 7)                 *
C*  COMMON CWORK1 NOW IN MACRO MVERTEX                              *
C*  MODIFICATION OF 27.5.86                  KLE/RAM                *
C*  MODES 2, 3, 4 AND STATISTICS                                    *
C*                                                                  *
C*  MODIFICATION OF 03.06.86                 KLE                    *
C*     END OF ITERAION IF MAX(|DS|) NOT SUM(|DS|) < DSCONV          *
C*                                                                  *
C*    +++ BIG MODIFICATION +++                    860611 C.K.       *
C*                                                                  *
C*    * FOR EACH TRACK NOW 40 INSTEAD OF 30 WORDS IN T()            *
C*                                                                  *
C*    * FITS IN RPHI AND ZS ARE TREATED INDEPENDENT                 *
C*      ( DIFFERENT FIRST POINT, MULT. SCATTERING POSSIBLE,         *
C*        THIS IS NECCESSARY FOR 'COMFIT'TED TRACKS         )       *
C*                                                                  *
C*    +++ END MODIFICATION +++                                      *
C*                                                                  *
C*    - HELIX- AND ERROR-CALCULATIONS WITH VTXS (SEE VTXPNT)        *
C*                                                                  *
C%MACRO VTXDEF
C********************************************************************
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
C*         14  = 0       NOT USED ON INPUT                          *
C*         15  = S0      INITIAL ARCLENGTH ( = 0. OR CLOSEST        *
C*                          APPROACH TO RUNVERTEX )                 *
C*         16-40         FOR INTERNAL USE (SEE BELOW)               *
C*        (41-80)        2. TRACK                                   *
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
C*       T(16) = COULOMB SCATTERING ERROR ( TANKWALL )              *
C*       T(17) = COULOMB SCATTERING ERROR ( NEW BEAMPIPE ONLY )     *
C*       T(18) = S TO TANKWALL NEAR                                 *
C*       T(19) = S TO BEAMPIPE NEAR                                 *
C*       T(20) = PROJ. TRACKLENGTH IN RPHI                          *
C*       T(21) = SIN(PHI0)                                          *
C*       T(22) = COS(PHI0)                                          *
C*       T(23) = TAN(THETA)                                         *
C*       T(24) = COS(THETA)                                         *
C*       T(25)   UNUSED                                             *
C*       T(26) = S TO TANKWALL FAR                                  *
C*       T(27) = S TO TANKWALL NEAR                                 *
C*       T(28) = S.D. X                                             *
C*       T(29) = S.D. Y                                             *
C*       T(30) = S.D. Z                                             *
C*                                                                  *
C*       NEW INTERNAL PARAMETERS ( 31 - 40 )               BY KLE   *
C*       -----------------------                                    *
C*       IT(31)= 1 FOR 'COMFIT'TED TRACKS, 0 ELSE                   *
C*                                      ( COVAR. DEFAULTS :   )     *
C*       T(32) = RPHI COVARIANCE : X**4 ( 180 SIG**2/N / L**4 )     *
C*       T(33) = RPHI COVARIANCE : X**3 ( 0.0 )                     *
C*       T(34) = RPHI COVARIANCE : X**2 ( -18 SIG**2/N / L**2 )     *
C*       T(35) = RPHI COVARIANCE : X**1 ( 0.0 )                     *
C*       T(36) = RPHI COVARIANCE : X**0 ( 9/4 SIG**2/N )            *
C*       T(37) = SIG**2/NPT IN ZS                                   *
C*       T(38) = PROJ. TRACKLENGTH IN ZS                            *
C*       T(39) = S0(ZS) - S0(RPHI)                                  *
C*       T(40) = COVARIANCE TERM FOR ANGULAR ERROR                  *
C*                                                                  *
C********************************************************************
C*                                                                  *
C*   MODES :                                                        *
C*  =========                                                       *
C*   BIT 31 ON --> STARTPOINT IS RUNVERTEX ( NOT FIRST MEASURED )   *
C*   BIT 30 ON --> RUNVERTEX CONSTRAIN                              *
C*   BIT 29 ON --> AXIS CONSTRAIN                                   *
C*   BIT 28 ON --> FITS IN RPHI AND Z ARE INDEPENDENT               *
C*   BIT 27 ON --> LOCAL STATISTICS FROM VERTEX                     *
C*   BIT 26 ON --> OVERWRITE PATR-BANK FOR TYP 2 WITH HELIX-        *
C*                 PARAMETERS                                       *
C*   BIT 25 ON --> MESSAGE FROM VTXEE IF FAILED                     *
C*                                                                  *
C********************************************************************
C*                                                                  *
C*       THIS ROUTINE CALLS SUBROUTINE SMINVD FOR SOLVING THE       *
C*       MATRIX EQUATION                                            *
C*                                                                  *
C*       MAINING OF TAXIS:                                          *
C*              1,2,3: X,Y,Z OF RUN-VERTEX                          *
C*              4,5,6: DX**2,DY**2,DZ**2 ERROR**2 OF RUN-VERTEX     *
C*              7,8,9: EX,EY,EZ UNIT-VECTOR OF CONSTRAIN-AXIS       *
C*              10,11,12: ERROR**2 OF CONSTRAIN-AXIS                *
C*                                                                  *
C*                                                                  *
C*   FOR DETAILS OF STATISTICS SEE VTXSTA                           *
C*                                                                  *
C********************************************************************
      IMPLICIT INTEGER*2 (H)
C PMF 03.11.98 
      LOGICAL TBIT
C%MACRO MVERTEX1
C     MACRO FOR VERTEX-FIT ROUTINES
      COMMON /CWORK1/ NT,T(2000),NV,V(200),A(300),B(24),NTIND(20),S(20),
     *                CHITR(20),
     *                JTGOD(50),JTBAD(50),VSAVE(10),V2(20,20)
C
      DIMENSION IT(2),IV(2)
      EQUIVALENCE (T(1),IT(1)),(V(1),IV(1))
C%MACRO MVERTEX2
C     MACRO FOR VERTEX-FIT ROUTINES ( AXIS AND STATISTICS )
      COMMON /CVTX2/ MODE,TAXIS(12),SVR,HVTXST(120)
C
      DIMENSION IVTXST(1)
C
C
      COMMON /CVTXC/ XB,YB,ZB,RTANK,DTANK,X0INN,SIGX0,SIGZ0,PNTMIN,
     *               DISTB,COLL2,MITER,DSCONV,PRCUT,IREJTR
C
      REAL*8 A,B,DET
C
      LOGICAL LWARN
      LOGICAL LOR, LAXIS, LRVCON, LRVST, LNOZ, LOCSTA
C**
      DATA NCALL / 0 /,LWARN / .TRUE. /
C
      LRVST  = TBIT(MODE,31)
      LRVCON = TBIT(MODE,30)
      LAXIS  = TBIT(MODE,29)
      LNOZ   = TBIT(MODE,28)
      LOCSTA = TBIT(MODE,27)
      LOR    = LRVCON .OR. LAXIS
C
      IERR = 1
      IF(NV.LE.0 .OR. NV.GT.20) GOTO 100
C
      NCALL = NCALL + 1
      IF (NCALL.EQ.1) WRITE(6,9000)
 9000 FORMAT(/
     *' *** NEW VERSION OF VERTEX-PACKAGE IS RUNNING. 24.7.86 C.K. ***'/
     *' ( 40 WORDS PER TRACK IN T(), XY-ERRORS FROM COVARIANCE MATRIX )'
     */)
      IF (NCALL.EQ.1 .AND. LAXIS) WRITE(6,9001)
 9001 FORMAT(
     * ' *******  SPECIAL VERSION OF VTXPRE/VERTEX  ********',/
     * ' *******     AXIS-CONSTRAINT IS RUNNING     ********'/)
      IF (NCALL.EQ.1 .AND. LRVCON) WRITE(6,9002)
 9002 FORMAT(
     * ' *******  SPECIAL VERSION OF VTXPRE/VERTEX  ********',/
     * ' *******   RUNVERTEX-CONSTRAINT IS RUNNING  ********'/)
C
      LV = (NV-1)*10
      IV(LV+1) = 0
      IERR = 2
      IF(NT.LE.0) GOTO 100
C**
      IF(.NOT.LOR) GOTO 55
         TERR = TAXIS(4)*TAXIS(5)*TAXIS(6)
         IF(TERR .GT. 0.0) GOTO 55
         IF(LWARN) WRITE(6,881)
 881     FORMAT(/' ==== WARNING FROM VERTEX: TAXIS NOT SET',
     *          ' , BUT REQUIRED --> FLAG IGNORED ==== '/)
         LWARN  = .FALSE.
         LAXIS  = .FALSE.
         LRVCON = .FALSE.
 55   CONTINUE
C**
      NREJ  = 0
      NREJV = 0
C                                           ITERATION-LOOP
    1 CONTINUE
      NTRV = 0
      ITER = 0
      CHI2 = 0.
      ITFIN = 0
C                                           CHECK TRACKS
C                                           IF MORE THAN 20,REJECT
C                                           WITH FEWEST MEASURED POINTS
      N = 0
      J = 0
      DO 5 I=1,NT
         IF(IT(J+1).LE.1) GOTO 5
         N = N + 1
         IF(N.GT.20) GOTO 2
            K = N
            GOTO 4
    2    MINPT = 48
         DO 3 K=1,20
            L = NTIND(K)
            IF(IT(L+13).GE.MINPT) GOTO 3
            IMIN = K
            MINPT = IT(L+13)
    3    CONTINUE
         IF(IT(J+13).LE.MINPT) GOTO 5
         K = IMIN
    4    NTIND(K) = J
C        S(K) = 0.
         S(K) = T(J+15)
    5    J = J + 40
C**
      SVR  = 0.0
      DSMAX = 0.0
      IMAX = 0
C**
      IF(N.GT.20) N=20
      NTR = N
      B(1) = 0.0D0
      B(2) = 0.0D0
      IF (LAXIS) N = N + 1
      IERR = 3
      IF(N.LE.0) GOTO 100
      IF (LRVCON) GOTO 10
      IERR = 8
      IF(N.EQ.1) GOTO 50
      IF(N.NE.2) GOTO 10
C                                           CHECK IF COLLINEAR 2-PRONG
      J1 = NTIND(1)
      J2 = NTIND(2)
      COSW = T(J1+24)*T(J2+24) * (T(J1+21)*T(J2+21)+T(J1+22)*T(J2+22))
     *       + SIN(T(J1+4))*SIN(T(J2+4))
      COSW = ABS(COSW)
      IERR = 9
      IF(COSW.GT.COLL2) GOTO 50
C
C                                           VERTEX FIT
   10 CONTINUE
         N = NTR + 3
C**
         IF (LOR) N = N + 1
C**
         L = (N*(N+1))/2
         DO 11 I=1,L
   11       A(I) = 0.D0
         IF(ITFIN.EQ.1) GOTO 13
            DO 12 I=1,N
   12          B(I) = 0.D0
   13    CONTINUE
C**
         IF(.NOT.LOR) GOTO 130
            DDAX = 1./(TAXIS(4) + SVR**2*TAXIS(10))
            DDAY = 1./(TAXIS(5) + SVR**2*TAXIS(11))
            DDAZ = 1./(TAXIS(6) + SVR**2*TAXIS(12))
            XAXIS = TAXIS(1) + SVR*TAXIS(7)
            YAXIS = TAXIS(2) + SVR*TAXIS(8)
            ZAXIS = TAXIS(3) + SVR*TAXIS(9)
            DDAZZ = DDAZ
            IF (LNOZ) DDAZ = 0.
 130     CONTINUE
C**
         DO 19 I=1,NTR
            J = NTIND(I)
            N = I + 3
            L = (N*(N-1))/2
C
C   VTXS IS AN ENTRY POINT IN VTXPNT      J.O. COMMENT
C
            CALL VTXS(J,S(I),XT,YT,ZT,DXT2,DYT2,DZT2,PHIT,DPHIT)
C
            SPHI = SIN(PHIT)
            CPHI = COS(PHIT)
C
            DDX = 1. / DXT2
            DDY = 1. / DYT2
            DDZ = 1. / DZT2
C
            DDZZ = DDZ
            IF (LNOZ) DDZ = 0.
            IF(ITFIN.EQ.0) GOTO 15
               IT(J+1) = 3
               CHITR(I) = DDX*(XT-B(1))**2 + DDY*(YT-B(2))**2
     *                                     + DDZ*(ZT-B(3))**2
               CHI2 = CHI2 + CHITR(I)
               IF ((T(J+5)**2+T(J+6)**2).LT.RTANK**2) NTRV = NTRV + 1
               GOTO 16
   15       CONTINUE
            B(1) = B(1) + DBLE(XT)*DBLE(DDX)
            B(2) = B(2) + DBLE(YT)*DBLE(DDY)
            B(3) = B(3) + DBLE(ZT)*DBLE(DDZZ)
            B(N) = - XT*CPHI*DDX - YT*SPHI*DDY - ZT*T(J+23)*DDZ
   16       A(1) = A(1) + DDX
            A(3) = A(3) + DDY
            A(6) = A(6) + DDZZ
            A(L+1) = -CPHI*DDX
            A(L+2) = -SPHI*DDY
            A(L+3) = -T(J+23)*DDZ
            A(L+N) = CPHI**2*DDX + SPHI**2*DDY + T(J+23)**2*DDZ
   19    CONTINUE
C**
         IF (.NOT.LOR) GOTO 200
            N = NTR + 4
            L = N*(N-1)/2
            IF(ITFIN .EQ. 1) GOTO 190
               B(1) = B(1) + DBLE(XAXIS)*DBLE(DDAX)
               B(2) = B(2) + DBLE(YAXIS)*DBLE(DDAY)
               B(3) = B(3) + DBLE(ZAXIS)*DBLE(DDAZZ)
               B(N) = - XAXIS*TAXIS(7)*DDAX
     *                - YAXIS*TAXIS(8)*DDAY
     *                - ZAXIS*TAXIS(9)*DDAZ
  190       CONTINUE
            A(1) = A(1) + DDAX
            A(3) = A(3) + DDAY
            A(6) = A(6) + DDAZZ
            A(L+1) = -TAXIS(7)*DDAX
            A(L+2) = -TAXIS(8)*DDAY
            A(L+3) = -TAXIS(9)*DDAZ
            A(L+N) = TAXIS(7)**2*DDAX +
     *               TAXIS(8)**2*DDAY +
     *               TAXIS(9)**2*DDAZ
  200    CONTINUE
C**
         IF(ITFIN.EQ.1) GOTO 30
C                                          SOLVE MATRIX EQUATION
C======
C        WRITE(6,8990)ITFIN,N
C8990    FORMAT(' ---> FOR SMINVD ITFIN,N: ',2I5)
C        WRITE(6,8997)(A(JJ),JJ= 1,L)
C8997    FORMAT(' A: ',9G13.4)
C        WRITE(6,8998)(B(JJ),JJ= 1,N)
C8998    FORMAT(' B: ',9G13.4)
C========
         CALL SMINVD(A,B,N,1,DET)
         IERR = 4
         IF(DET.EQ.0.0) GOTO 100
C======
C        WRITE(6,8928)(B(JJ),JJ= 1,N)
C8928    FORMAT(' B  ',9G13.4)
C========
         DSUM  = 0.
         DSMAX = 0.
         IMAX  = 0
         DO 23 I=1,NTR
            J = NTIND(I)
            DS  = B(I+3)
            ADS = ABS(DS)
            IF(ABS(DS/T(J+2)).GT.1.5) GOTO 25
            DSUM = DSUM + ADS
            S(I) = S(I) + DS
            T(J+15) = S(I)
            IF (ADS.LE.DSMAX) GOTO 23
               IMAX = I
               DSMAX = ADS
   23    CONTINUE
C**
         IF (.NOT.LAXIS) GOTO 230
            DS = B(NTR + 4)
            ADS = ABS(DS)
            DSUM = DSUM + ADS
            SVR = SVR + DS
            IF (ADS.LE.DSMAX) GOTO 230
               IMAX = -IMAX
               DSMAX = ADS
  230    CONTINUE
C**
         ITER = ITER + 1
CHANGED  IF(ITER.LT.MITER .AND. DSUM.GT.DSCONV) GOTO 10
         IF(ITER.LT.MITER .AND. DSMAX.GT.DSCONV) GOTO 10
C                                          FIT FINISHED, REPEAT ONCE
C                                          MORE WITH PROPER ERRORS
         ITFIN = 1
C
      GOTO 10
C                         EXTRAPOLATION OF TRACK I EXCEEDS 90 DEG
C                         REMOVE TRACK AND RESTART IF MORE THAN ONE LEFT
   25 IERR = 5
      IF(NTR.LE.2) GOTO 100
      IT(J+1) = 1
      NREJ = NREJ + 1
      IF ((T(J+5)**2+T(J+6)**2).LT.RTANK**2) NREJV = NREJV + 1
C
      GOTO 1
C
   30 CALL SMINVD(A,DUMMY,N,0,DET)
      IERR = 6
      IF(A(1).LT.0.0 .OR. A(3).LT.0.0 .OR. A(6).LT.0.0) GOTO 100
      NDF = 2*NTR - 3
      IF (LNOZ) NDF = NTR - 2
C**
      IF (.NOT.LOR) GOTO 300
         NDF = NDF + 2
         CHI2 = CHI2 + DDAX*(XAXIS-B(1))**2
     *               + DDAY*(YAXIS-B(2))**2
     *               + DDAZ*(ZAXIS-B(3))**2
 300  CONTINUE
C**
      PR = 1.
      IF (NDF.GT.0) PR = PROB(CHI2,NDF)
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
      IERR = -1
      IF(IREJTR.EQ.0) GOTO 100
      IF(IV(LV+1).EQ.3) GOTO 100
      IERR = 7
      IF(NTR.LE.2) GOTO 100
C                                         BAD CHISQUARE
C                                         CHECK IF THERE IS ONE TRACK
C                                         WHICH CONTRIBUTES MOST TO CHI200047300
      CHIMAX = 0.
      DO 32 I=1,NTR
         IF(CHITR(I).LT.CHIMAX) GOTO 32
         CHIMAX = CHITR(I)
         NTMAX = I
   32 CONTINUE
      CHICUT = 3. / NTR
      IF(CHICUT.GT..9) CHICUT=.9
      CHICUT = CHICUT * CHI2
      IERR = 0
      IF(CHIMAX.LT.CHICUT) GOTO 100
      J = NTIND(NTMAX)
      IT(J+1) = 1
      NREJ = NREJ + 1
      IF ((T(J+5)**2+T(J+6)**2).LT.RTANK**2) NREJV = NREJV + 1
C
      GOTO 1
C
C                                           1-PRONG OR COLLINEAR 2-PRONG
C
   50 CONTINUE
      IV(LV+1) = 2
      J = NTIND(1)
      CALL VTXPNT(J,XB,YB,XT1,YT1,ZT1,DXT21,DYT21,DZT21,PHIT,DPHIT,ST)
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
      CALL VTXPNT(J,XB,YB,XT2,YT2,ZT2,DXT22,DYT22,DZT22,PHIT,DPHIT,ST)
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
  100 CONTINUE
      IF (.NOT.LOCSTA) GOTO 120
      IF (IERR.LE.0) GOTO 110
      CALL VTXERR(IERR)
      GOTO 120
  110 CONTINUE
      WRITE(6,9345) LRVST,LRVCON,LAXIS,LNOZ,NTR,NTRV,NREJ,NREJV,
     *              ITER,DSMAX,NDF,CHI2
 9345 FORMAT(' START AT RV, RV-CON., AXIS-CON., NO Z : ',4L2/
     *       ' NTR,NREJ           : ',4I6/
     *       ' ITER,DSMAX,NDF,CHI2 : ',I6,G13.4,I6,G13.4)
C
  120 CONTINUE
      HVTXST( 1) = IERR
      HVTXST(19) = ITER
      IDSMAX = 30000
      IF (DSMAX.LT.30.) IDSMAX = IFIX( DSMAX * 1000. )
      HVTXST(20) = IDSMAX
      HVTXST( 2) = HVTXST(2) + 1
      HVTXST(4+IERR) = HVTXST(4+IERR) + 1
      IF (LRVST)  HVTXST(14) = HVTXST(14) + 1
      IF (LRVCON) HVTXST(15) = HVTXST(15) + 1
      IF (LAXIS)  HVTXST(16) = HVTXST(16) + 1
      IF (LNOZ)   HVTXST(17) = HVTXST(17) + 1
      IF (IERR.GT.0) RETURN
C
      HVTXST(18) = IFIX(PR*10000.)
      IF (NTR .GT.19) NTR  = 19
      IF (NTRV.GT.19) NTRV = 19
      HVTXST(21+NTR)  = HVTXST(21+NTR)  + 1
      HVTXST(41+NTRV) = HVTXST(41+NTRV) + 1
      IF (NREJ .GT.19) NREJ  = 19
      IF (NREJV.GT.19) NREJV = 19
      HVTXST(61+NREJ)  = HVTXST(61+NREJ)  + 1
      HVTXST(81+NREJV) = HVTXST(81+NREJV) + 1
      IF (ITER.GT.20) ITER = 20
      HVTXST(100+ITER)  = HVTXST(100+ITER)  + 1
C
      RETURN
      END
