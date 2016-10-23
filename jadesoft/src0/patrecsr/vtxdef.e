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
