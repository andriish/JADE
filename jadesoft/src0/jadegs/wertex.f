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
     &' *** NEW VERSION OF VERTEX-PACKAGE IS RUNNING. 24.7.86 C.K. ***'/
     &' ( 40 WORDS PER TRACK IN T(), XY-ERRORS FROM COVARIANCE MATRIX )'
     &/)
      IF (NCALL.EQ.1 .AND. LAXIS) WRITE(6,9001)
 9001 FORMAT(
     & ' *******  SPECIAL VERSION OF VTXPRE/VERTEX  ********',/
     & ' *******     AXIS-CONSTRAINT IS RUNNING     ********'/)
      IF (NCALL.EQ.1 .AND. LRVCON) WRITE(6,9002)
 9002 FORMAT(
     & ' *******  SPECIAL VERSION OF VTXPRE/VERTEX  ********',/
     & ' *******   RUNVERTEX-CONSTRAINT IS RUNNING  ********'/)
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
     &          ' , BUT REQUIRED --> FLAG IGNORED ==== '/)
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
     &                                     + DDZ*(ZT-B(3))**2
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
     &                - YAXIS*TAXIS(8)*DDAY
     &                - ZAXIS*TAXIS(9)*DDAZ
  190       CONTINUE
            A(1) = A(1) + DDAX
            A(3) = A(3) + DDAY
            A(6) = A(6) + DDAZZ
            A(L+1) = -TAXIS(7)*DDAX
            A(L+2) = -TAXIS(8)*DDAY
            A(L+3) = -TAXIS(9)*DDAZ
            A(L+N) = TAXIS(7)**2*DDAX +
     &               TAXIS(8)**2*DDAY +
     &               TAXIS(9)**2*DDAZ
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
     &               + DDAY*(YAXIS-B(2))**2
     &               + DDAZ*(ZAXIS-B(3))**2
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
     &              ITER,DSMAX,NDF,CHI2
 9345 FORMAT(' START AT RV, RV-CON., AXIS-CON., NO Z : ',4L2/
     &       ' NTR,NREJ           : ',4I6/
     &       ' ITER,DSMAX,NDF,CHI2 : ',I6,G13.4,I6,G13.4)
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
      SUBROUTINE VTXAFT
C*800124*DITTMANN***************************************************
C*  COPIED FROM F22KLE.VERTEX.S(VTXAFT)   18.9.86
C*                                                                 *
C*          F I N I S H   X Y Z   V E R T E X   F I T              *
C*                                                                 *
C*       CALCULATE TRACK PARAMETERS AT VERTEX-NEAREST POINT        *
C*       FOR DETAILS OF THE T,V-ARRAYS SEE COMMENT IN SUBR. VERTEX.*
C*                                                                 *
C*******************************************************************
C
C%MACRO MVERTEX1
C     MACRO FOR VERTEX-FIT ROUTINES
      COMMON /CWORK1/ NT,T(2000),NV,V(200),A(300),B(24),NTIND(20),S(20),
     *                CHITR(20),
     *                JTGOD(50),JTBAD(50),VSAVE(10),V2(20,20)
C
      DIMENSION IT(2),IV(2)
      EQUIVALENCE (T(1),IT(1)),(V(1),IV(1))
C
      IF(NV.EQ.0) RETURN
C****
C****    EXTRAPOLATE ALL TRACKS TO VERTEX
      J = 0
      DO 9 I=1,NT
      IF(IT(J+1).EQ.0) GOTO 9
      IF(IT(J+14).EQ.0) GOTO 9
      LV = (IT(J+14)-1)*10
      CALL VTXPNT(J,V(LV+2),V(LV+3),XT,YT,ZT,DXT2,DYT2,DZT2,PHI,DPHI,SS)
      T(J+3) = PHI
      T(J+5) = XT
      T(J+6) = YT
      T(J+7) = ZT
C *** CORRECT ERROR OF PHI,THETA FOR MULT. SCATTERING
      SSCOUL = SS - T(J+18)
      SSCVXC = SS - T(J+19)
      DPHMS2 = 0.
      IF ((SSCOUL.LT.0.).AND.(T(J+18).LT.0.)) DPHMS2 = T(J+16)**2
      IF (SSCVXC.LT.0.) DPHMS2 = DPHMS2 + T(J+17)**2
      T(J+8) = SQRT( DPHI**2 + DPHMS2 )
C
      SSCOUL = SS + T(J+39) - T(J+18)
      SSCOUL = SS + T(J+39) - T(J+19)
      DTHMS2 = 0.
      IF(SSCOUL.LT.0.) DTHMS2 = T(J+16)**2
      IF (SSCVXC.LT.0.) DTHMS2 = DTHMS2 + T(J+17)**2
      T(J+9) = SQRT( T(J+9)**2 + DTHMS2 )
C ***
      T(J+10) = SQRT(DXT2)
      T(J+11) = SQRT(DYT2)
      T(J+12) = SQRT(DZT2)
      T(J+15) = SS
    9 J = J + 40
C
  100 RETURN
      END
      SUBROUTINE VTXBNK(IPPATR)
C*800205*OLSSON*****************************************************
C*                                                                 *
C* C R E A T E  B A N K  GVTX  F R O M  V E R T E X  R E S U L T S *
C*                                                                 *
C*******************************************************************
C             IPPATR IS POINTER TO 'PATR' ;  SAME BOSBANK NR IS USED
      IMPLICIT INTEGER*2 (H)
#include "cdata.for"
C%MACRO MVERTEX1
C     MACRO FOR VERTEX-FIT ROUTINES
      COMMON /CWORK1/ NT,T(2000),NV,V(200),A(300),B(24),NTIND(20),S(20),
     *                CHITR(20),
     *                JTGOD(50),JTBAD(50),VSAVE(10),V2(20,20)
C
      DIMENSION IT(2),IV(2)
      EQUIVALENCE (T(1),IT(1)),(V(1),IV(1))
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
1     IDATA(INVX+I) = IV(ISS+I)
      IDATA(IPHT+2+NV*10) = NT
C
      DO 2  INT = 1,NT
      INTX = IPHT+2+NV*10 + (INT-1)*15
      ISS = (INT-1)*40
      DO 2  I = 1,15
2     IDATA(INTX+I) = IT(ISS+I)
  100 RETURN
      END
      SUBROUTINE VTXEE
C*800604*DITTMANN********************************************
C*  COPIED FROM F22KLE.VERTEX.S(VTXEE)   18.9.86
C*                                                          *
C*              X - Y - Z    E E   P A I R S                *
C*                                                          *
C*       A DESCRIPTION OF THE T AND V ARRAYS CAN BE FOUND   *
C*       IN SUBR. VERTEX                                    *
C*860612*KLEINWORT*******************************************
C*                                                          *
C*       MODIFIED TO BE USED WITH VTXC                      *
C*                                                          *
C************************************************************
      IMPLICIT INTEGER*2(H)
C
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
      ITEST = 0
      NT1 = NT - 1
      DO 9 I=1,NT1
      IF(IT(J1+1).EQ.0) GOTO 9
      M = I + 1
      J2 = I*40
      DO 28 K=M,NT
      IPOINT = 1
      IF(IT(J2+1).EQ.0) GOTO 8
C
      ITEST  = 1
      IT1  = I
      IT2  = K
C        OPPOSITE CHARGE
      IF(T(J1+2)*T(J2+2).GT.0.) GOTO 8
C        MEASURED R-PHI OPENING
      ITEST  = 2
      DPHI = T(J1+3) - T(J2+3)
      IF(T(J1+2).LT.0.) DPHI=-DPHI
      IF(ABS(DPHI).GT.3.141593) DPHI=DPHI-SIGN(1.,DPHI)*6.283185
      IF(DPHI.LT.EEDPMN .OR. DPHI.GT.EEDPMX) GOTO 8
      ITANK = 0
      IPOINT = 2
      ITEST  = 6
      XM1 = T(J1+5) - T(J1+2)*T(J1+21)
      YM1 = T(J1+6) + T(J1+2)*T(J1+22)
      XM2 = T(J2+5) - T(J2+2)*T(J2+21)
      YM2 = T(J2+6) + T(J2+2)*T(J2+22)
      ICFT1 = IT(J1+31)
      ICFT2 = IT(J2+31)
C        EXTRAPOLATION LENGTH
      IF(T(J1+18).EQ.0. .OR. T(J2+18).EQ.0.) GOTO 6
      ITEST  = 7
      IF(T(J1+18).LT.SEMAX .OR. T(J2+18).LT.SEMAX) GOTO 6
C        PAIR ORIGIN IN TANK (ADD SQRT(2/3)*COULOMB ERROR)
      ST1 = T(J1+18)
      ST2 = T(J2+18)
C
C   VTXS IS AN ENTRY POINT IN VTXPNT       J.O.  COMMENT
C
      CALL VTXS(J1,ST1,XT1,YT1,ZT1,DXT12,DYT12,DZT12,PHIT1,DPHIT1)
      CALL VTXS(J2,ST2,XT2,YT2,ZT2,DXT22,DYT22,DZT22,PHIT2,DPHIT2)
C        DISTANCE IN TANK
      DPHI = PHIT1 - PHIT2
      IF(T(J1+2).LT.0.) DPHI=-DPHI
      IF(ABS(DPHI).GT.3.141593) DPHI=DPHI-SIGN(1.,DPHI)*6.283185
      PTCFT2 =
     &   (FLOAT(1-ICFT1)*(T(J1+16)**2+T(J1+17)**2)
     &   +FLOAT(1-ICFT2)*(T(J2+16)**2+T(J2+17)**2)) / 1.5
      STCFT2 = 49. * PTCFT2
      PTANK2 = (T(J1+16)**2+T(J2+16)**2+T(J1+17)**2+T(J2+17)**2) / 1.5
      STANK2 = 49. * PTANK2
      DX2 = (XT1-XT2)**2
      DY2 = (YT1-YT2)**2
      DXY = SQRT(DX2+DY2)
      DIST = SQRT((XT2-XM1)**2+(YT2-YM1)**2)
      IF(DIST.LT.ABS(T(J1+2))) DXY=-DXY
      DZ = ABS(ZT1-ZT2)
      DTH = ABS(T(J1+4)-T(J2+4))
      SDTH = DTH / SQRT(T(J1+9)**2+T(J2+9)**2+PTANK2)
      ITEST  = 3
      IF(SDTH.GT.EEDTMX) GOTO 6
      CTH = COS((T(J1+4)+T(J2+4))/2.)**2
      SDZ = DZ / SQRT(DZT12+DZT22+STANK2/CTH**2)
      ITEST  = 5
      IF(SDZ.GT.EEDTMX) GOTO 5
      SDPHI = ABS(DPHI)/SQRT(DPHIT1**2+DPHIT2**2+PTCFT2)
      ITEST  = 2
      IF(SDPHI.GT.EEDRMX) GOTO 5
      SDXY = ABS(DXY) / SQRT(1./(1./DXT12+1./DYT12)+
     *                       1./(1./DXT22+1./DYT22)+STCFT2)
      ITEST  = 4
      IF(SDXY.GT.EEDRMX .AND. (DXY.LT.0..OR.DXY.GT.EEXYMN)) GOTO 5
      ITANK = 1
      GOTO 6
C
    5 CONTINUE
      IPOINT = 3
      ITEST  = 6
C        EXTRAPOLATION LENGTH
      IF(T(J1+18).EQ.0. .OR. T(J2+18).EQ.0.) GOTO 6
      ITEST  = 7
      IF(T(J1+18).LT.SEMAX .OR. T(J2+18).LT.SEMAX) GOTO 6
C        PAIR ORIGIN IN BEAMPIPE (ADD SQRT(2/3)*COULOMB ERROR)
      ST1 = T(J1+19)
      ST2 = T(J2+19)
      CALL VTXS(J1,ST1,XT1,YT1,ZT1,DXT12,DYT12,DZT12,PHIT1,DPHIT1)
      CALL VTXS(J2,ST2,XT2,YT2,ZT2,DXT22,DYT22,DZT22,PHIT2,DPHIT2)
C        DISTANCE IN BEAMPIPE
      DPHI = PHIT1 - PHIT2
      IF(T(J1+2).LT.0.) DPHI=-DPHI
      IF(ABS(DPHI).GT.3.141593) DPHI=DPHI-SIGN(1.,DPHI)*6.283185
      PTCFT2 = PTCFT2 +
     &   (FLOAT(ICFT1)*T(J1+17)**2+FLOAT(ICFT2)*T(J2+17)**2) / 1.5
      STCFT2 = DRPIPE(DUM)**2 * PTCFT2
      STANK2 = DRPIPE(DUM)**2 * PTANK2
      DX2 = (XT1-XT2)**2
      DY2 = (YT1-YT2)**2
      DXY = SQRT(DX2+DY2)
      DIST = SQRT((XT2-XM1)**2+(YT2-YM1)**2)
      IF(DIST.LT.ABS(T(J1+2))) DXY=-DXY
      DZ = ABS(ZT1-ZT2)
      CTH = COS((T(J1+4)+T(J2+4))/2.)**2
      SDZ = DZ / SQRT(DZT12+DZT22+STANK2/CTH**2)
      ITEST  = 5
      IF(SDZ.GT.EEDTMX) GOTO 6
      SDPHI = ABS(DPHI)/SQRT(DPHIT1**2+DPHIT2**2+PTCFT2)
      ITEST  = 2
      IF(SDPHI.GT.EEDRMX) GOTO 6
      SDXY = ABS(DXY) / SQRT(1./(1./DXT12+1./DYT12)+
     *                       1./(1./DXT22+1./DYT22)+STCFT2)
      ITEST  = 4
      IF(SDXY.GT.EEDRMX .AND. (DXY.LT.0..OR.DXY.GT.EEXYMN)) GOTO 6
      ITANK = 1
C
    6 CONTINUE
      IPOINT = 4
      ITEST  = 4
C        VERTEX WHERE TRACKS ARE PARALLEL
      CALL VTXPNT
     &   (J1,XM2,YM2,XT1,YT1,ZT1,DXT12,DYT12,DZT12,PHIT1,DPHIT1,ST1)
      CALL VTXPNT
     &   (J2,XM1,YM1,XT2,YT2,ZT2,DXT22,DYT22,DZT22,PHIT2,DPHIT2,ST2)
      DX2 = (XT1-XT2)**2
      DY2 = (YT1-YT2)**2
      DXY = SQRT(DX2+DY2)
      DIST = SQRT((XT2-XM1)**2+(YT2-YM1)**2)
      IF(DIST.LT.ABS(T(J1+2))) DXY=-DXY
      IF(ABS(DXY).GT.EEXYMX) GOTO 8
      DZ = ABS(ZT1-ZT2)
      IF(ITANK.EQ.1) GOTO 7
C        PAIR ORIGIN IN CHAMBER
      IPOINT = 5
      ITEST  = 7
      IF(ST1.LT.SEMAX .OR. ST2.LT.SEMAX) GOTO 8
      ITEST  = 8
      IF(ST1.GT.SIMAX .OR. ST2.GT.SIMAX) GOTO 8
      ITEST  = 9
      IF(T(J1+18).NE.0. .AND. ST1-T(J1+18).LT.0.) GOTO 8
      IF(T(J2+18).NE.0. .AND. ST2-T(J2+18).LT.0.) GOTO 8
C        DISTANCE IN CHAMBER
      DTH = ABS(T(J1+4)-T(J2+4))
      SDTH = DTH / SQRT(T(J1+9)**2+T(J2+9)**2)
      ITEST  = 3
      IF(SDTH.GT.EEDTMX) GOTO 8
      SDZ = DZ / SQRT(DZT12+DZT22)
      ITEST  = 5
      IF(SDZ.GT.EEDTMX) GOTO 8
      SDXY = ABS(DXY)/SQRT(1./(1./DXT12+1./DYT12)+1./(1./DXT22+
     *       1./DYT22))
      ITEST  = 4
      IF(SDXY.GT.EEDRMX .AND. (DXY.LT.0..OR.DXY.GT.EEXYMN)) GOTO 8
C
    7 ITEST = 0
      IF(NV.EQ.20) GOTO 14
      NV = NV + 1
      LV = (NV-1)*10
      IV(LV+1) = 4
      DT1 = SQRT(DXT12)
      DT2 = SQRT(DXT22)
      DXX2 = 1./(1./DT1+1./DT2)
      V(LV+2) = (XT1/DT1+XT2/DT2) * DXX2
      DT1 = SQRT(DYT12)
      DT2 = SQRT(DYT22)
      DYY2 = 1./(1./DT1+1./DT2)
      V(LV+3) = (YT1/DT1+YT2/DT2) * DYY2
      DT1 = SQRT(DZT12)
      DT2 = SQRT(DZT22)
      DZZ2 = 1./(1./DT1+1./DT2)
      V(LV+4) = (ZT1/DT1+ZT2/DT2) * DZZ2
      V(LV+5) = SQRT(DXX2)
      V(LV+6) = SQRT(DYY2)
      V(LV+7) = SQRT(DZZ2)
      V(LV+8) = SQRT(DX2+DY2)
      IV(LV+9) = I
      IV(LV+10) = K
    8 CONTINUE
C****
      IF (TBIT(MODE,25)) CALL VTXEER(IT1,IT2,IPOINT,ITEST)
C****L
   28 J2 = J2 + 40
    9 J1 = J1 + 40
      IF(NV.EQ.0) GOTO 100
   14 I = 1
   15 LV1 = (I-1)*10
      IF(I.EQ.NV) GOTO 19
      M = I + 1
      LV2 = I*10
      DO 16 K=M,NV
      IF ( IV(LV1+ 9).EQ.IV(LV2+ 9) .OR. IV(LV1+10).EQ.IV(LV2+ 9) .OR.
     *     IV(LV1+10).EQ.IV(LV2+10) ) GOTO 17
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
   19 J1 = (IV(LV1+9)-1)*40
      J2 = (IV(LV1+10)-1)*40
      V(LV1+9) = 0.
      IV(LV1+8) = 2
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
      SUBROUTINE VTXEER(IT1,IT2,IPOINT,ITEST)
C*860529*KLEINWORT***************************************************
C*  COPIED FROM F22KLE.VERTEX.S(VTXEER)   18.9.86
C*                                                                  *
C*   PRINT ERROR-TEXT FOR   V T X E E                               *
C*                                                                  *
C********************************************************************
C
      IMPLICIT INTEGER*2(H)
C
#include "cdata.for"
C
      REAL*8 DTEXT(14)
C
      DATA DTEXT / 'PRETEST ', 'TANKWALL', 'BEAMPIPE', 'PAR.TEST',

     & 'CHAMBER ', 'CHARGE  ', 'D(PHI)  ', 'D(THETA)', 'D(XY)   ',
     & 'D(Z)    ', 'INTRSECT', 'EXTRAP.S', 'INTERP.S', 'RADIUS  ' /
C
      IF (ITEST.EQ.0) GOTO 600
C
      IF ((IPOINT.LT.1).OR.(IPOINT.GT.5)) GOTO 500
      IF ((ITEST.LT.0).OR.(ITEST.GT.9)) GOTO 500
C
      WRITE(6,9000) DTEXT(IPOINT),DTEXT(ITEST+5),IT1,IT2
 9000 FORMAT(' VTXEE FAILED IN ',A8,' DUE TO INVALID ',A8,
     &' WITH TRACKS :',2I3)
C
      IH = IDATA(IBLN('HEAD'))
      IF (IH.LE.0) GOTO 600
C
      NRUN  = HDATA( 2*IH+ 10 )
      NREC  = HDATA( 2*IH+ 11 )
      WRITE(6,9010) NRUN,NREC
 9010 FORMAT(' RUN,REC : ',2I6)
C
      GOTO 600
C
  500 CONTINUE
      WRITE(6,9020) IPOINT,ITEST
 9020 FORMAT(' ??? STRANGE ENTRY TO VTXEER : ',2I5)
C
  600 CONTINUE
      RETURN
      END
      SUBROUTINE VTXERR(IERR)
C*860529*KLEINWORT***************************************************
C*  COPIED FROM F22KLE.VERTEX.S(VTXERR)   18.9.86
C*                                                                  *
C*   PRINT ERROR-TEXT FOR   V E R T E X -FIT                        *
C*                                                                  *
C********************************************************************
      IMPLICIT INTEGER*2 (H)
C%MACRO MVERTEX2
C     MACRO FOR VERTEX-FIT ROUTINES ( AXIS AND STATISTICS )
      COMMON /CVTX2/ MODE,TAXIS(12),SVR,HVTXST(120)
C
      DIMENSION IVTXST(1)
C
C
      REAL*8 DTEXT(27)
      DIMENSION NSTART(10)
C
      DATA NSTART / 1, 4, 5, 6, 8, 14, 19, 24, 25, 28 /
      DATA DTEXT /
     &'NV < 1 O','R NV > 2','0       ',
     &'NT  <= 0',
     &'NTR <= 0',
     &'DET = 0.','0       ',
     &'NTR < 2 ','AFTER RE','JECTING ','TRACK WI','TH DS/R ','> 90 DEG',
     &'ERROR OF',' VERTEXC','OORDINAT','E(S) = 0','.0      ',
     &'NTR < 2 ','AFTER RE','JECTING ','BAD TRAC','K       ',
     &'NTR = 1 ',
     &'COLLINEA','R 2-PRON','G       ' /
C
      IF ((IERR.LT.1).OR.(IERR.GT.9)) GOTO 100
C        IF (HVTXST(4+IERR).GT.0)
      N1 = NSTART(IERR)
      N2 = NSTART(IERR+1)-1
      WRITE(6,9000) IERR,(DTEXT(K),K=N1,N2)
 9000 FORMAT(' ERROR ',I2,' : ',10A8)
      GOTO 200
C
  100 CONTINUE
C
      WRITE(6,9010) IERR
 9010 FORMAT(' ERROR ',I6,' IS UNKNOWN ')
C
  200 CONTINUE
C
      RETURN
      END
      SUBROUTINE VTXINI
C*800623*DITTMANN***************************************************
C*  COPIED FROM F22KLE.VERTEX.S(VTXINI)   18.9.86
C*                                                                 *
C* I N I T I A L I S A T I O N   O F   X Y Z   V E R T E X   F I T *
C*                                                                 *
C*       TO BE CALLED ONCE AND BEFORE FIRST CALL TO VTXPRE, VERTEX *
C*                                                                 *
C*    MODIFIED FOR VERTEX CHAMBER HARDWARE     860211 C.K.         *
C*       ADDITIONAL CONSTANTS ARE TAKEN FROM COMMON /CGEOV/        *
C*       JADEBD MUST BE DECLARED AS EXTERNAL                       *
C*                                                                 *
C*    MODES AND STATISTICS                        860527 C.K.      *
C*                                                                 *
C*******************************************************************
      IMPLICIT INTEGER*2 (H)
      COMMON /CVTXC/ XB,YB,ZB,RTANK,DTANK,X0INN,SIGX0,SIGZ0,PNTMIN,
     *               DISTB,COLL2,MITER,DSCONV,PRCUT,IREJTR,EEDPMN,
     *               EEDPMX,EEDTMX,EEDRMX,SEMAX,SIMAX,SIGFAC,EEXYMN,
     *               EEXYMX,PHEMAX,SIG1,SIG2,SIG3,CSECV
C
C%MACRO MVERTEX2
C     MACRO FOR VERTEX-FIT ROUTINES ( AXIS AND STATISTICS )
      COMMON /CVTX2/ MODE,TAXIS(12),SVR,HVTXST(120)
C
      DIMENSION IVTXST(1)
C
C
C        RESET MODE AND CLEAR STATISTICS
      MODE = 0
      DO 10 I = 1, 120
         HVTXST(I) = 0
   10 CONTINUE
C
C        MEAN VERTEX COORDINATES
      XB = 0.
      YB = 0.
      ZB = 0.
C        OUTER RADIUS OF INNER TANK WALL
      RTANK = 174.
C        OUTER DISTANCE BEAM PIPE TO TANK WALL
      DTANK = 81.
C
C THE VARIABLES DTANK AND X0INN ARE UPDATED IN VTXPRE ACC. TO JADE
C STATUS, I.E. OLD OR NEW BEAMPIPE AND BP-COUNTERS OR VTX CHAMBER
C
C        RADIATION LENGTH BETWEEN BEAM AND FIRST WIRE
      X0INN = 0.1312
C        MEAN TRACK RESIDUAL IN XY AND ZR PLANE
      SIGX0 = 0.55
      SIGZ0 = 30.
C        ARTIFICIAL FACTOR TO ACCOUNT FOR SYSTEMATIC ERRORS
      SIGFAC = 1.0
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
      COLL2 = 0.99
C        MAXIMUM NUMBER OF ITERATIONS IN VERTEX FIT
      MITER = 12
C        CONVERGENCE PARAMETER
      DSCONV = 0.05
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
      SUBROUTINE VTXPNT(J,XP,YP,XT,YT,ZT,DXT2,DYT2,DZT2,PHIT,DPHIT,ST)
C*800623*DITTMANN************************************************
C*  COPIED FROM F22KLE.VERTEX.S(VTXPNT)   18.9.86
C*                                                              *
C*     S U P P O R T   F O R   X Y Z   V E R T E X   F I T      *
C*                                                              *
C*860611*KLEINWORT***********************************************
C*                                                              *
C*  REWRITTEN FOR PROPER ERRORS AND USE WITH 'COMFIT'ED TRACKS  *
C*                                                              *
C*  - AS NEW PARAMETER DPHIT THE ERROR OF PHI IS CALCULATED     *
C*    ( WITHOUT MULTIPLE SCATTERING ! )                         *
C*                                                              *
C*       COMMON CWORK1 MUST BE FILLED PROPERLY                  *
C*                                                              *
C*       ENTRY: VTXS                                            *
C****************************************************************
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
      REAL*8 XM,YM,RR,PHI,SPHI,CPHI,SR
C        ENTRY VTXPNT
C****    CALCULATE POINT XT,YT,ZT ON HELIX J WHICH IS NEAREST
C****    TO POINT XP,YP IN X-Y PROJECTION
      RR = DBLE(T(J+2))
      XM = DBLE(T(J+5)) - RR*DBLE(T(J+21))
      YM = DBLE(T(J+6)) + RR*DBLE(T(J+22))
      SR = DBLE(SIGN(1.,T(J+2)))
      PHI = DATAN2(-SR*(XM-DBLE(XP)),SR*(YM-DBLE(YP)))
      SPHI = DSIN(PHI)
      CPHI = DCOS(PHI)
      PHIT = SNGL(PHI)
      PHI = PHI - DBLE(T(J+3))
      IF (DABS(PHI).GT.3.141593D0) PHI=PHI-DSIGN(1.D0,PHI)*6.283185D0
      ST = SNGL(RR*PHI)
      XT = SNGL(XM + RR*SPHI)
      YT = SNGL(YM - RR*CPHI)
      ZT = T(J+7) + ST*T(J+23)
      SSPACE = ST
      GOTO 100
C
C
         ENTRY VTXS(J,SJ,XT,YT,ZT,DXT2,DYT2,DZT2,PHIT,DPHIT)
C****    CALCULATE POINT XT,YT,ZT ON HELIX J FOR A GIVEN
C****    PROJECTED ARC LENGTH SJ
      RR = DBLE(T(J+2))
      PHI = DBLE(SJ) / RR + DBLE(T(J+3))
      PHIT = SNGL(PHI)
      SPHI = DSIN(PHI)
      CPHI = DCOS(PHI)
      XT = SNGL(DBLE(T(J+5)) + RR*(SPHI-DBLE(T(J+21))))
      YT = SNGL(DBLE(T(J+6)) - RR*(CPHI-DBLE(T(J+22))))
      ZT = T(J+7) + SJ*T(J+23)
      SSPACE = SJ
C
C *** CALCULATION OF RPHI-ERRORS
C
  100 CONTINUE
C
      SSCOUL = SSPACE - T(J+18)
      IF ((SSCOUL.GT.0.).OR.(T(J+18).GT.0.)) SSCOUL=0.
      SSCVXC = SSPACE - T(J+19)
      IF(SSCVXC.GT.0.) SSCVXC=0.
C
      X = SSPACE - 0.5*T(J+20)
C                                      ERROR IN XY (SQUARED)
C
C                                      MULTIPLE SCATTERRING ( TANKWALL )
      DXY = ( T(J+16) * SSCOUL )**2 +
C                                      MULTIPLE SCATTERRING ( BEAMPIPE )
     &      ( T(J+17) * SSCVXC )**2 +
C                                      PARABOLA FIT
     &      ((( T(J+32)   *X + T(J+33) ) *X +
     &          T(J+34) ) *X + T(J+35) ) *X + T(J+36)
C
C                                      ERROR IN PHI (NOT SQUARED)
C
C                                      NO MULTIPLE SCATTERRING
C                                      PARABOLA FIT
      XX = 2.0 * X
      DPHIT = ( T(J+32) *XX + T(J+33) ) *XX + T(J+40)
      IF (DPHIT.GE.0.) DPHIT = SQRT( DPHIT )
C
      DXYLIM = 1. / (2.*ABS(T(J+2))*SQRT(ABS(DXY)))
      DXT2 = SNGL(SPHI**2) / DXY
      DYT2 = SNGL(CPHI**2) / DXY
      IF(ABS(DXT2).LT.DXYLIM) DXT2=DXYLIM
      IF(ABS(DYT2).LT.DXYLIM) DYT2=DXYLIM
      DXT2 = 1. / DXT2
      DYT2 = 1. / DYT2
C
C *** CALCULATION OF ZS-ERRORS
C
      SSPACE = SSPACE + T(J+39)
C
      SSCOUL = SSPACE - T(J+18)
      IF(SSCOUL.GT.0.) SSCOUL=0.
      SSCVXC = SSPACE - T(J+19)
      IF(SSCVXC.GT.0.) SSCVXC=0.
C
      X = SSPACE/T(J+38) - 0.5
C                                      MULTIPLE SCATTERRING
      DZT2 = ( T(J+16) * SSCOUL )**2 + ( T(J+17) * SSCVXC )**2 +
C                                      LINE FIT (WITH TRIPLETS)
     &         T(J+37) * ( 1.5 + 18.*X**2 )
C
      RETURN
C
      END
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
#include "cdata.for"
C
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
C        CONSTANTS
      COMMON /CVTXC/ XB,YB,ZB,RTANK,DTANK,X0INN,SIGX0,SIGZ0,PNTMIN,
     *               DISTB,COLL2,MITER,DSCONV,PRCUT,IREJTR,EEDPMN,
     *               EEDPMX,EEDTMX,EEDRMX,SEMAX,SIMAX,SIGFAC,EEXYMN,
     *               EEXYMX,PHEMAX,SIG1
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
     &   GOTO 139
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
     &      ( ATAN2( SR*(RVX - XM), SR*(YM - RVY) )
     &      - T(JT+3) ) * T(JT+2)
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
     &      * ( 1. + 1./9. * ALOG10( X0ID / T(JT+24) ) )**2
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
     &      * ( 1. + 1./9. * ALOG10( X0VX / T(JT+24) ) )**2
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
      SUBROUTINE VTXSRC
C*800623*DITTMANN********************************************
C*  COPIED FROM F22KLE.VERTEX.S(VTXSRC)   18.9.86
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
C        CONSTANTS
      COMMON /CVTXC/ XB,YB,ZB,RTANK,DTANK,X0INN,SIGX0,SIGZ0,PNTMIN,
     *               DISTB,COLL2,MITER,DSCONV,PRCUT,IREJTR,EEDPMN,
     *               EEDPMX,EEDTMX,EEDRMX,SEMAX,SIMAX,SIGFAC,EEXYMN,
     *               EEXYMX,PHEMAX,SIG1,SIG2,SIG3,CSECV
C
C%MACRO MVERTEX1
C     MACRO FOR VERTEX-FIT ROUTINES
      COMMON /CWORK1/ NT,T(2000),NV,V(200),A(300),B(24),NTIND(20),S(20),
     *                CHITR(20),
     *                JTGOD(50),JTBAD(50),VSAVE(10),V2(20,20)
C
      DIMENSION IT(2),IV(2)
      EQUIVALENCE (T(1),IT(1)),(V(1),IV(1))
C
      DIMENSION IV2(20,20)
      EQUIVALENCE (V2(1,1),IV2(1,1))
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
    3 J = J + 40
    4 LV = LV + 10
C****
C****    SEARCH FOR PRIMARY VERTEX AROUND AVERAGE BEAM
    5 NTGOD = 0
      NTBAD = 0
      J = 0
      DO 7 I=1,NT
      IF(IT(J+1).LE.0) GOTO 7
      CALL VTXPNT(J,XB,YB,XT,YT,ZT,DXT2,DYT2,DZT2,PHI,DPHI,SS)
      DXY = SQRT((XT-XB)**2+(YT-YB)**2)
      IF(DXY.GT.DISTB) GOTO 6
      IF(SS.LT.SEMAX-RTANK) GOTO 6
      NTGOD = NTGOD + 1
      JTGOD(NTGOD) = J
      GOTO 7
    6 IT(J+1) = 1
      NTBAD = NTBAD + 1
      JTBAD(NTBAD) = J
    7 J = J + 40
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
      CALL VTXPNT(J,V(LV+2),V(LV+3),XT,YT,ZT,DXT2,DYT2,DZT2,PHI,DPHI,SS)
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
   25 J = J + 40
      IF(RESTOR) GOTO 30
      IF(NTGOD3.EQ.20) GOTO 30
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
     *            PHIT1,DPHIT1,ST1)
      IF(ST1.GT.SIMAX) GOTO 44
      SSJC = ST1
      IF(T(J1+27).EQ.0.) GOTO 143
      IF(ST1.LT.T(J1+26)-SIMAX) GOTO 44
      IF(ST1.LT.T(J1+27)) SSJC=T(J1+27)
  143 IF(SSJC.LT.SEMAX) GOTO 44
      IF(ABS(ST1/T(J1+2)).GT.PHEMAX) GOTO 44
C        EXTRAPOLATION LENGTH TRACK 2
      CALL VTXPNT(J2,V(LV+2),V(LV+3),XT2,YT2,ZT2,DXT22,DYT22,DZT22,
     *            PHIT2,DPHIT2,ST2)
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
C#######################################################
C 03.6.86    PROTECTION AGAINST DIVIDE WITH ZERO    J.OLSSON
      IF(DV12.LE.10.E-8) GO TO 44
C#######################################################
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
      CALL VTXPNT(J,V(LV+2),V(LV+3),XT,YT,ZT,DXT2,DYT2,DZT2,PHI,DPHI,SS)
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
   91 J = J + 40
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
   99 J = J + 40
C
  100 RETURN
      END
      FUNCTION XFWID(IPHEAD)
C-----------------------------------------------------------------------
C
C
C     RETURNS THE NUMBER OF RADIATION LENGTHS FROM THE INSIDE OF THE
C     BEAM PIPE TO THE FIRST GAS MOLECULE IN THE JET CHAMBER TANK.
C     BASED ON DATE IN HEAD BANK.
C     MC-DATA: NEW GEOMETRY IS FORCED IF FLAG LVTXC IS TRUE
C     ARGUMENT DUMMY IS IGNORED.
C
C   RADIATION LENGTH UPDATE, 9.6.1986    J.O.
C   RADIATION LENGTH UPDATE, 11.6.1986    J.O.
C-----------------------------------------------------------------------
C
      IMPLICIT INTEGER*2 (H)
C
      LOGICAL NEWDET,LVTXC,LNHARD
C
#include "cdata.for"
#include "cadmin.for"
C
      COMMON / CVCEX  / LVTXC
C
C------------------  C O D E  ------------------------------------------
C
      IH2    = 2*IPHEAD
C
      IMONTH = HDATA(IH2+7)
      IYEAR  = HDATA(IH2+8)
C
C                            OLD GEOMETRY (PRIOR TO MAY 1984)
C
C
C   FOR VALUE OF RADIATION LENGTH, SEE JCN 87
C
      XFWID = 0.1604
C                            NEW DETECTOR HARDWARE IN MONTE-CARLO DATA?
C                            THEN LVTXC IS TRUE (FLAG IS CHECKED AND
C                                                SET IN RDMTCO)
C
      LNHARD = (IMONTH .GE. 5  .AND.  IYEAR .EQ. 1984)
     +                          .OR.  IYEAR .GE. 1985
      NEWDET = LVTXC .OR. (IEVTP.EQ.0 .AND. LNHARD)
C
      IF( .NOT. NEWDET ) RETURN
C
C                            NEW GEOMETRY
C
      XFWID = 0.1443
C
      IF ((IEVTP.NE.0).AND.(IYEAR.LE.1985)) XFWID = 0.1334
C
      RETURN
      END
      FUNCTION XFWVXC(IPHEAD)
C-----------------------------------------------------------------------
C
C
C     RETURNS THE NUMBER OF RADIATION LENGTHS FROM THE INSIDE OF THE
C     BEAM PIPE TO THE FIRST GAS MOLECULE IN THE VERTEX CHAMBER.
C     BASED ON DATE IN HEAD BANK.
C     MC-DATA: NEW GEOMETRY IS FORCED IF FLAG LVTXC IS TRUE
C     ARGUMENT DUMMY IS IGNORED.
C
C   RADIATION LENGTH UPDATE, 9.6.1986    J.O.
C   RADIATION LENGTH UPDATE, 11.6.1986    J.O.
C
C       X0(BEAMPIPE)                0.03371
C       X0(1.KAPTON)                0.00011
C       X0(2.KAPTON+CU)             0.00148
C       X0(3.KAPTON+CU)             0.00262
C       X0(COOLING PIPES)           0.00107
C                                  =========
C                                   0.03899
C
C-----------------------------------------------------------------------
C
      IMPLICIT INTEGER*2 (H)
C
      LOGICAL NEWDET,LVTXC,LNHARD
C
#include "cdata.for"
#include "cadmin.for"
C
      COMMON / CVCEX  / LVTXC
C
C------------------  C O D E  ------------------------------------------
C
      IH2    = 2*IPHEAD
C
      IMONTH = HDATA(IH2+7)
      IYEAR  = HDATA(IH2+8)
C
C                            OLD GEOMETRY (PRIOR TO MAY 1984)
C
C
C   FOR VALUE OF RADIATION LENGTH, SEE JCN 87
C
      XFWVXC = 0.00000
C                            NEW DETECTOR HARDWARE IN MONTE-CARLO DATA?
C                            THEN LVTXC IS TRUE (FLAG IS CHECKED AND
C                                                SET IN RDMTCO)
C
      LNHARD = (IMONTH .GE. 5  .AND.  IYEAR .EQ. 1984)
     +                          .OR.  IYEAR .GE. 1985
      NEWDET = LVTXC .OR. (IEVTP.EQ.0 .AND. LNHARD)
C
      IF( .NOT. NEWDET ) RETURN
C
C                            NEW GEOMETRY
C
      XFWVXC = 0.03899
C
      IF ((IEVTP.NE.0).AND.(IYEAR.LE.1985)) XFWVXC = 0.03371
C
      RETURN
      END
      REAL FUNCTION PARKOR*8(IP)
C COPIED AND RENAMED FROM F22KLE.JVTXC.S(PARCOR)   18.9.1986
C
C     IP : POINTER TO TRACK IN PATR-BANK
C
C     CURVATURE-CORRECTION FOR CIRCLES FITTED WITH A PARABOLA
C
      REAL*8 P,Q,R,CM,D,U,V,C
C
      INTEGER*2 HDATA
#include "cdata.for"
C
C *** MEASURED CURVATURE
      CM = -ADATA(IP+22)
      C  =  ADATA(IP+25) * 0.5
      IF (IDATA(IP+18).NE.2) GOTO 100
C *** MEASURED PROJ. TRACKLENGTH
      R  = ABS( ( ADATA(IP+7) - ADATA(IP+14) ) / ADATA(IP+30) ) * 0.5
C
C     CARDANS RULE
C
      P =  7.D0/(18.D0*R*R)
      Q = -7.D0*CM/(12.D0*R*R)
C
      D = DSQRT(Q*Q+P*P*P)
      U = (-Q+D)**(1./3.)
      V = ( Q+D)**(1./3.)
      C = U-V
C
  100 CONTINUE
      PARKOR = C * 2.0D0
C
      RETURN
      END
      SUBROUTINE WTXCRV( NRUN, RVX, RVY ,RVDX, RVDY)
C
C   THIS SUBROUTINE RETURNS FOR A GIVEN RUNNUMBER 'NRUN'
C   THE WITH THE VERTEXCHAMBER CALCULATED RUNVERTEX '(RVX,RVY)'
C   AND ERRORS (RVDX,RVDY)
C   ( IN DETECTOR-COORDINATES )
C
C
      DIMENSION VSTORE(5,37), VS1(5,19), VS2(5,18)
C
      EQUIVALENCE (VSTORE(1,1),VS1(1,1)), (VSTORE(1,20),VS2(1,1))
C
      DATA RLAST  / 24187. /, IPER / 1 /, NCALL / 0 /
C
      DATA VS1 /
     &    20275.,  1.177    ,  .9093E-01,  1.722    ,  .5080E-01,
     &    20366., -1.971    ,  .2840    ,  1.267    ,  .1860    ,
     &    20370.,  1.847    ,  .4167    ,  1.455    ,  .1442    ,
     &    20385., -.4718    ,  .1308    ,  1.868    ,  .6758E-01,
     &    20980.,  .7311    ,  .1489    ,  1.984    ,  .5378E-01,
     &    21008., -.4295    ,  .1269    ,  1.949    ,  .7287E-01,
     &    21211.,  .2094    ,  .8336E-01,  2.052    ,  .4016E-01,
     &    21310., -1.267    ,  .7253E-01,  1.975    ,  .3805E-01,
     &    21329., -.2575    ,  .3390E-01,  1.976    ,  .1680E-01,
     &    21808., -.6072E-01,  .5099E-01,  1.592    ,  .3412E-01,
     &    22026., -.5669    ,  .4785E-01,  1.665    ,  .2744E-01,
     &    22162.,  .4993    ,  .6722E-01,  1.862    ,  .3471E-01,
     &    22214., -.1608E-01,  .6566E-01,  1.937    ,  .3577E-01,
     &    22367.,  .6297    ,  .6978E-01,  1.855    ,  .4481E-01,
     &    22416., -.3393    ,  .6317E-01,  1.910    ,  .2997E-01,
     &    22559., -1.412    ,  .4251E-01,  1.925    ,  .2345E-01,
     &    22854., -.4245    ,  .1020    ,  1.908    ,  .5556E-01,
     &    22897.,  .9606    ,  .1222    ,  1.858    ,  .8161E-01,
     &    22901.,  .2585    ,  .1539    ,  2.215    ,  .8853E-01/
      DATA VS2 /
     &    22915., -.4097    ,  .1003    ,  1.963    ,  .4599E-01,
     &    22976.,  .1262    ,  .1114    ,  1.735    ,  .6050E-01,
     &    23044., -.7860    ,  .2345    ,  2.029    ,  .1177    ,
     &    23054.,  .2327    ,  .8883E-01,  1.830    ,  .5139E-01,
     &    23096.,  .8635    ,  .5580E-01,  1.764    ,  .2453E-01,
     &    23211.,  .3967E-01,  .6786E-01,  1.631    ,  .3873E-01,
     &    23326.,  .6146    ,  .5488E-01,  1.684    ,  .3087E-01,
     &    23418.,  1.595    ,  .2312E-01,  1.730    ,  .1193E-01,
     &    23838., -.3356    ,  .3926E-01,  1.736    ,  .2447E-01,
     &    23993.,  .9898    ,  .1815    ,  1.437    ,  .9115E-01,
     &    24008.,  .3999    ,  .9235E-01,  1.608    ,  .7332E-01,
     &    24016.,  1.018    ,  .9523E-01,  1.683    ,  .4518E-01,
     &    24034.,  1.135    ,  .1226    ,  1.432    ,  .7125E-01,
     &    24050.,  1.041    ,  .1383    ,  1.775    ,  .7137E-01,
     &    24057.,  .3728    ,  .7962E-01,  1.614    ,  .3897E-01,
     &    24094., -.8727    ,  .5520E-01,  1.648    ,  .2776E-01,
     &    24143.,  .6927    ,  .7144E-01,  1.630    ,  .3140E-01,
     &    99999.,  0.000    ,  0.000    ,  0.000    ,  0.000        /
C
      IF (NCALL.EQ.0) WRITE(6,9000)
 9000 FORMAT(/
     &' *** WARNING : RUNVERTICES ARE PRELEMINARY (18.4.86 C.K.) ***'/)
      NCALL = NCALL + 1
C
      RVX  = 0.
      RVDX = 0.
      RVY  = 0.
      RVDY = 0.
C
      RUN = FLOAT(NRUN)
C
      IF ((RUN.LT.VSTORE(1,1)).OR.(RUN.GT.RLAST)) GOTO 999
C
      IF (RUN.LT.VSTORE(1,IPER)) GOTO 300
C
  200 CONTINUE
      IF (RUN.LT.VSTORE(1,IPER+1)) GOTO 400
      IPER = IPER + 1
      GOTO 200
C
  300 CONTINUE
      IPER = IPER - 1
      IF (RUN.LT.VSTORE(1,IPER)) GOTO 300
C
  400 CONTINUE
C
      RVX  = VSTORE(2,IPER)
      RVDX = VSTORE(3,IPER)
      RVY  = VSTORE(4,IPER)
      RVDY = VSTORE(5,IPER)
C
  999 CONTINUE
C
      RETURN
      END
