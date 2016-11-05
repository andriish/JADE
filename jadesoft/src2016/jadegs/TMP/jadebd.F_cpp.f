C   01/11/84 903021643  MEMBER NAME  JADEBD   (JADEGS)      FORTRAN
C
C-----------------------------------------------------------------------
C     J A D E B D   B L O C K   D A T A
C-----------------------------------------------------------------------
C
C     THIS MEMBER CONTAINS A DUMMY SUBROUTINE JADEBD WHICH IS
C     REFERENCED SO THAT THE BLOCK DATA SUBPROGRAM IS LOADED
C     CORRECTLY. THIS BLOCK DATA INITIALISES JADE ANALYSIS PACKAGES.
C
C-----------------------------------------------------------------------
C
      SUBROUTINE JADEBD
      RETURN
      END
C
C-----------------------------------------------------------------------
      BLOCKDATA BL3             ! PMF 01/07/99 add name
C-----------------------------------------------------------------------
C
C
C        MOD  J. HAGEMANN 22/04/83 :  CORRECTION OF SOME GEOMETRICAL AND
C             R. RAMCKE            :  PHYSICAL PARAMETERS
C        MOD  C. BOWDERY  19/12/83 :  JADEBD CREATED. RECOMMENTED.
C        MOD  C. BOWDERY   9/01/84 :  DELETE COMMON /CUNIT/. OBSOLETE.
C        MOD  C. BOWDERY   8/02/84 :  COMMON /CADMIN/ NOW INCLUDED.
C        MOD  C. BOWDERY  13/03/84 :  CDTL DEFAULTS CHANGED.
C        MOD  J. HAGEMANN 21/09/84 :  VERTEX CHAMBER GEOMETRY INCLUDED
C                                     IN NEW COMMONS
C        MOD  C. BOWDERY  20/04/85 :  NEW COMMON / CKALPR /
C        MOD  J. OLSSON   10/06/85 :  NEW COMMON / TODAY /
C                                UPDATE OF VTX CH CONSTANTS (J.HAGEMANN)
C        MOD  J. HAGEMANN 16/07/85 :  UPDATE OF XRLPIV, XRLVXC IN COMMON
C                                     / CGEOV /
C        MOD  J. OLSSON   10/06/87 :  UPDATE OF ZSFIT COMMONS
C        MOD  C. BOWDERY  24/07/87 :  INCLUDE Z CHAMBER COMMONS
C        MOD  C. BOWDERY  29/07/87 :  ZCHWW ADDED TO /CCMZCT/
C        MOD  J. OLSSON   16/11/87 :  CGEO3 UPDATED FOR OUTER RADIUS
C   LAST MOD  J. OLSSON   16/11/87 :  /CDEDXU/ UPDATED ACC. TO K. AMBRUS
C
C------------------  D E C L A R A T I O N S  --------------------------
C
C- - - - - - - - -   CTLIM   - - - - - - - - - - - - - - - - - - - - - -
C
C                            TIMEOUT VALUE FOR SUPERV ( SET TO 2 SECS)
C
      COMMON / CTLIM  / ISECLF
C
      DATA ISECLF/2/
C
C- - - - - - - - -   CADMIN  - - - - - - - - - - - - - - - - - - - - - -
C
C                            GENERAL ADMINISTRATION COMMON
C
C
C                        --- MACRO CADMIN ---
C
      LOGICAL*1 LBREAD
C
      COMMON / CADMIN / IEVTP,NRREAD,NRWRIT,NRERR,LBREAD(4),IFLG,IERCAL,
     +                  ISMEAR,IJETCI,NFLAGS(10)
C
C                                 NFLAGS IS AN ARRAY OF GENERAL FLAGS
C                                   (1) : USED BY RDDATE
C                                   (2) : USED BY RDTRIG
C                                   (3) : USED BY RDTRIG
C                                   (4) : USED BY RDTRIG / PRSTAT
C                                   (5) : USED BY EVREAD -COUNTS RECORDS
C                                   (6) : USED BY SUPERV -COUNTS ERRORS
C                                   (7) : USED BY EVWRIT -'HEAD'LESS EVS
C                                   (8) : USED BY EVREAD/RDMTCO (EVWRIT)
C                                   (9) : USED BY RDMTCO/EVWRIT
C                                  (10) : FREE
C
C                                  BLOCK DATA SET IN MEMBER JADEBD
C
C
      DATA IEVTP,NRREAD,NRWRIT,NRERR / -1,0,0,0 /, LBREAD / 4*.FALSE. /
      DATA IFLG / 0 /, IERCAL / 0 /, ISMEAR / 0 /, IJETCI / 0 /
      DATA NFLAGS / 0,0,-1,1,0,  0,0,0,0,0 /
C
C- - - - - - - - -   CBSTR   - - - - - - - - - - - - - - - - - - - - - -
C
C                            WRITE PARAMETERS FOR SUPERV
C
      COMMON / CBSTR  / MODE,MAXRL,SAFEOF
C
      LOGICAL SAFEOF
C
      DATA MODE / 2 /, MAXRL / 1558 /, SAFEOF / .FALSE. /
C
C- - - - - - - - -   CIOUNI  - - - - - - - - - - - - - - - - - - - - - -
C
C                            INPUT/OUTPUT LOGICAL UNIT NUMBERS (SUPERV)
C
C                            IUNIT   = EVENT INPUT
C                            JUNIT   = EVENT OUTPUT
C                            NCALI   = MAX NO. OF CALIBRATION FILES
C                            KUNITA  = ASTART CALIBRATION FILES (DEAD)
C                            LUNITA  = AUPDAT/BUPDAT CALIBRATION FILES
C
C==MACRO CIOUNI=========================================
      COMMON/CIOUNI/IUNIT,JUNIT,NCALI,KUNITA(10),LUNITA(10)
C==ENDMACRO CIOUNI========================================
C
      DATA IUNIT  /  2           /
      DATA JUNIT  /  3           /
      DATA NCALI  /  2           /
      DATA KUNITA /  0,  0, 8*0  /
      DATA LUNITA / 21, 22, 8*0  /
C
C- - - - - - - - -   CGRAPH  - - - - - - - - - - - - - - - - - - - - - -
C
C                            GRAPHICS PARAMETERS FOR SUPERV
C
C-----------------------------------------------------------------------
C                            MACRO CGRAPH .... GRAPHICS COMMON
C-----------------------------------------------------------------------
C
      LOGICAL DSPDTL,SSTPS,PSTPS,FREEZE
C
      COMMON / CGRAPH / JUSCRN,NDDINN,NDDOUT,IDATSV(11),ICREC,MAXREC,
     +                  LSTCMD,ACMD,LASTVW,ISTANV,
     +                  SXIN,SXAX,SYIN,SYAX,XMIN,XMAX,YMIN,YMAX,
     +                  DSPDTL(30),SSTPS(10),PSTPS(10),FREEZE(30),
     +                  IREADM,LABEL,LSTPS(10),IPSVAR
C
C------- END OF MACRO CGRAPH -------------------------------------------
C
C
      DATA NDDINN,NDDOUT,MAXREC,IREADM,LABEL / 3*0,1,0 /
      DATA FREEZE / 30*.FALSE. /, JUSCRN / 6 /, LASTVW / 0 /, IPSVAR /1/
C
      DATA DSPDTL/3*.TRUE.,.FALSE.,2*.TRUE.,2*.FALSE.,.TRUE.,2*.FALSE.,
     + 3*.TRUE., 5*.FALSE., 2*.TRUE., 6*.FALSE., .TRUE., 2*.FALSE. /
      DATA LSTCMD/1/
C
C- - - - - - - - -   CMCCAL  - - - - - - - - - - - - - - - - - - - - - -
C
C                            KLREAD FLAGS FOR MC CALIBRATION READING
C                            0 = DONT READ    1 = READ  (SEE KLREAD)
C
***  wird nur in KLREAD benoetigt, ist dort aber schon definiert
***  wird auskommentiert 02.02.98
***      COMMON / CMCCAL / LBMC(15) 
***C
***      DATA LBMC / 0,0,0,0,0,  0,0,0,0,0,  0,0,0,0,0 /
C
C- - - - - - - - -   CJCELL  - - - - - - - - - - - - - - - - - - - - - -
C
      COMMON / CJCELL / NCELL(3), NWIRES(3)
C
C                            NCELL  = NUMBER OF CELLS PER RING
C                            NWIRES = NUMBER OF WIRES PER CELL
C
      DATA NCELL  / 24,24,48 /
      DATA NWIRES / 16,16,16 /
C
C- - - - - - - - -   CJDRCH  - - - - - - - - - - - - - - - - - - - - - -
C
C-----------------------------------------------------------------------
C                            MACRO CJDRCH .... JET CHAMBER CONSTANTS.
C-----------------------------------------------------------------------
C
      COMMON / CJDRCH / RDEC(4),PSIIN(3),RINCR(3),FIRSTW(3),FSENSW(3),
     +                  RDEPTH,SWDEPL,YSUSPN,TIMDEL(2,3),ZMAX,ZOFFS,
     +                  ZRESOL,ZNORM,ZAL,ZSCAL,DRIDEV,DRICOS,DRISIN,
     +                  PEDES,TZERO(3),DRIROT(96,2),SINDRI(96,2),
     +                  COSDRI(96,2),DRIVEL(96,2),T0FIX(3),
     +                  ABERR(8), DUMJDC(20)
C
C      BLOCK DATA SET TO MC VALUES, KALIBR WILL SET REAL DATA VALUES
C--->  A CHANGE OF THIS COMMON MUST BE DONE SIMULTANEOUSLY WITH  <----
C--->  A CHANGE OF THE BLOCK DATA                                <----
C
C--------------------------- END OF MACRO CJDRCH -----------------------
C
C
C                            RDEC   = MEAN RADII OF CHAMBER WALLS
C                            PSIIN  = ANGULAR INCREMENT OF FIRST WIRE
C                                     LINE.
C                            FIRSTW = RADII OF FIRST MAIN POTENTIAL
C                                     WIRES IN EACH RING
C                            FSENSW = RADII OF FIRST SENSE WIRES IN
C                                     EACH RING
C                            RINCR  = DISTANCE BETWEEN TWO POTENTIAL
C                                     WIRES
C                            RDEPTH = DIFFERENCE OF TWO CHAMBER RADII
C                            SWEDPL = WIRE STAGGERING, .GT.0 MEANS IN
C                                     POS PHI FOR ZEROTH WIRE
C
C                            YSUSPN = MAXIMUM VERTICAL DEPLACEMENT OF
C                                     WIRE DUE TO GRAVITATION
C                            TIMDEL = RESOLUTION IN EACH CHAMBER IN MM.
C                            ZMAX   = SENSITIVE HALF LENGTH OF WIRES
C                            ZOFFS  = OFFSET OF WIRES
C                            ZRESOL = RESOLUTION IN Z
C                            ZNORM |
C                            ZAL   |= PARAMETERS FOR CALCULATION OF
C                            ZSCAL |  Z AMPLITUDES
C                            DRIDEV|
C                            DRICOS|= TILT ANGLE, COS, SIN
C                            DRISIN|
C                            PEDES  = OLD OVERALL Z AMPLIFIER  PEDESTAL.
C                                     UNUSED.
C                            TZERO(3) = OVERALL DRIFT TIME  ZEROES  FOR
C                                       EACH OF THE THREE  JET  CHAMBER
C                                       RINGS.
C
C                            TOFIX(3) = TEMPORARY TZEROS  FOR  TOF  AND
C                                       PROP.TIME     CORRECTIONS    IN
C                                       PATTERN RECOGNITION
C
C                            ABERR(8) = ABERRATION CORRECTION CONSTANTS
C                                       USED IN FXYZ,JETXYZ
C                            DRIROT(96,2),SINDRI(96,2),COSDRI(96,2)
C                                       INDEPENDENT LORENTZ ANGLES  FOR
C                                       THE 96 JET  CHAMBER  CELLS  AND
C                                       TWO DRIFT  DIRECTIONS  AND  THE
C                                       CORRESPONDING     SINES     AND
C                                       COSINES.
C                            ARRAY   ABERR(8)   SET   TO   MC   VALUES,
C                            OVERWRITTEN BY KALIBR FOR REAL DATA
C                                                 J.OLSSON  17.9.80
C
      DATA RDEC   / 196., 396.4 , 596.4, 797.33 /
      DATA PSIIN  / 0.1309 , 0.1309 , 0.065450 /
      DATA FIRSTW / 206., 416., 627.33 /
      DATA FSENSW / 211., 421., 632.33 /
      DATA RINCR  / 10., 10., 10.      /
      DATA RDEPTH / 200.4 /
      DATA SWDEPL / 0.2   /
      DATA YSUSPN / 0.0   /
C                            NOTE THAT TIMDEL IS ALWAYS OVERWRITTEN
      DATA TIMDEL / 6*0.005 /
C
      DATA ZMAX, ZOFFS, ZRESOL, ZNORM,ZAL,ZSCAL
     *            / 1180., -10., 1., 1000., 2687., 10. /
      DATA DRIDEV /-0.349066 /,
     *     DRICOS / 0.939693 /,
     *     DRISIN /-0.342020 /
C                           PEDES IS UNUSED
      DATA PEDES  / 50.  /
      DATA TZERO  / 3*0. /
      DATA DRIROT / 192*-0.349066 /
      DATA SINDRI / 192*-0.342020 /
C
      DATA COSDRI / 192*0.939693  /
      DATA T0FIX  / 0.65, 0.71, 0.76 /
      DATA ABERR  / 0., 5., 5., 2.5,0.,0., 99999.,0. /
      DATA DUMJDC / 20*0. /
C
C- - - - - - - - -   CJIONI  - - - - - - - - - - - - - - - - - - - - - -
C
      COMMON / CJIONI / POTBEA, ZAROBE,
     *                  POTTRI, ZAROTR,
     *                  POTIVE, ZAROIV,
     *                  POTRH0, ZAROR0,
     *                  POTJET, ZAROJE,
     *                  POTRH1, ZAROR1,
     *                  POTRH2, ZAROR2,
     *                  POTRH3, ZAROR3,
     *                  POTOVE, ZAROOV,
     *                  POTTOF, ZAROTO,
     *                  POTVES, ZARVES,
     *                  POTZJL, ZAROJL,
     *                  POTZJR, ZAROJR
C
C                            GIVES  AVERAGE  IONISATION  POTENTIAL  AND
C                            VALUE FOR Z*RHO/A FOR BETHE BLOCH  FORMULA
C                            FOR : BEAMPIPE, BEAM PIPE  COUNTER,  INNER
C                            VESSEL WALL, 4 DIFFERENT ROHACELL  LAYERS,
C                            DRIFTGAS , OUTER VESSEL WALL, TOF COUNTER,
C                            VESSEL WALL  IN  Z,  LEFT  AND  RIGHT  ALU
C                            SUPPORTS FOR WIRES AND ROHACELL  (  VALUES
C                            FOR THE AVERAGE  IONISATION  POTENTIAL  OF
C                            ALUMINIUM FROM SOVIET JOURNAL  OF  NUCLEAR
C                            PHYSICS 9, PAGE 583 F., 1969 )
C
      DATA  POTBEA , ZAROBE        / 160.3,1.3      /
      DATA  POTTRI , ZAROTR        / 36.1,.59       /
      DATA  POTIVE , ZAROIV        / 160.3,1.3      /
      DATA  POTRH0 , ZAROR0        / 102., 0.11     /
      DATA  POTJET , ZAROJE        / 284.4,.00295   /
      DATA  POTRH1 , ZAROR1        / 102.,.07       /
      DATA  POTRH2 , ZAROR2        / 102.,.07       /
      DATA  POTRH3 , ZAROR3        / 102. , .09     /
      DATA  POTOVE , ZAROOV        / 160.3,1.3      /
      DATA  POTTOF , ZAROTO        / 36.1,.59       /
      DATA  POTVES , ZARVES        / 160.3,1.3      /
      DATA  POTZJL , ZAROJL        / 160.3, 1.3     /
      DATA  POTZJR , ZAROJR        / 160.3, 1.3     /
C
C- - - - - - - - -   CJSWLO  - - - - - - - - - - - - - - - - - - - - - -
C
      COMMON / CJSWLO / ITIMOD, MULSC, ELOSS
C
      LOGICAL  ELOSS, MULSC
C
C                            ITIMOD DETERMINES TYPE OF DRIFT TIME
C                            CALCULATION ( SEE JDTIM ).
C
C                            MULSC: IF TRUE MULTIPLE SCATTERING INCLUDED
C                            ELOSS: IF TRUE ENERGY LOSS IS INCLUDED
C
      DATA ITIMOD, MULSC, ELOSS / 1 , .TRUE., .TRUE. /
C
C- - - - - - - - -   CJTRIG  - - - - - - - - - - - - - - - - - - - - - -
C
      COMMON / CJTRIG / PI,TWOPI,PIHALF,PI3HAF
      DATA  PI,TWOPI / 3.141593 , 6.283185 / , PIHALF / 1.5707963 /
      DATA  PI3HAF   / 4.712389 /
C
C- - - - - - - - -   CGEO1   - - - - - - - - - - - - - - - - - - - - - -
C
C                            JADE GEOMETRICAL CONSTANTS
C
C-----------------------------------------------------------------------
C                            MACRO CGEO1 .... JADE GEOMETRY
C-----------------------------------------------------------------------
C
      COMMON / CGEO1 / BKGAUS,
     +                 RPIP,DRPIP,XRLPIP,   RBPC,DRBPC,XRLBPC,
     +                 RITNK,DRITNK,XRLTKI, R0ROH,DR0ROH,XR0ROH,
     +                 R1ROH,DR1ROH,XR1ROH, R2ROH,DR2ROH,XR2ROH,
     +                 R3ROH,DR3ROH,XR3ROH, ROTNK,DROTNK,XRLTKO,
     +                 RTOF,DRTOF,XRTOF,    RCOIL,DRCOIL,XRCOIL,
     +                 ZJM,DZJM,XRZJM,ZJP,DZJP,XRZJP,ZTKM,DZTKM,XRZTKM,
     +                 ZTKP,DZTKP,XRZTKP,ZBPPL,ZBPMI,ZTOFPL,ZTOFMI,
     +                 XRJETC,RLG,ZLGPL,ZLGMI,OUTR2,CTLIMP,
     +                 CTLIMM,DELFI,BLXY,BLZ,BLDEP,ZENDPL,ZENDMI,DEPEND,
     +                 XHOL1,XHOL2,YHOL1,YHOL2,BLFI
C
C------------------------- END OF MACRO CGEO1 --------------------------
C
C
C             BKGAUS = MAGNETIC FIELD IN KGAUSS
C             RPIP   = RADIUS OF BEAM PIPE
C             DRPIP  = THICKNESS OF BEAM PIPE IN MM
C             XRLPIP =     "     "    "    "   " RADL
C
C             RBPC   = RADIUS OF BEAM PIPE COUNTERS
C             DRBPC  = THICKNESS OF BEAM PIPE COUNTERS IN MM
C             XRLBPC =     "     "    "    "      "     " RADL
C
      DATA BKGAUS                    / -4.85               /
      DATA RPIP, DRPIP, XRLPIP       / 123.,  5.3, 0.0596  /
      DATA RBPC, DRBPC, XRLBPC       / 150., 10., 0.0238   /
C
C             RITNK  = INNER RADIUS OF THE TANK
C             DRITNK = THICKNESS OF INNER TANK WALL IN MM
C             XRLTKI =     "     "    "     "   "   "  RADL
C
C             R0ROH  = RADIUS OF ZEROTH ROHACELL SUPPORT
C             DR0ROH = THICKNESS OF ZEROTH ROHACELL SUPPORT IN MM
C             XR0ROH =     "     "     "      "        "     " RADL
C
      DATA RITNK, DRITNK, XRLTKI     / 167.,  7., 0.0787   /
      DATA R0ROH, DR0ROH, XR0ROH     / 181., 15., 0.0076   /
C
C             R1ROH  = RADIUS OF FIRST ROHACELL SUPPORT
C             DR1ROH = THICKNESS OF FIRST ROHACELL SUPPORT IN MM
C             XR1ROH =     "     "    "      "        "     " RADL
C
C             R2ROH  = RADIUS OF SECOND ROHACELL SUPPORT
C             DR2ROH = THICKNESS OF SECOND ROHACELL SUPPORT IN MM
C             XR2ROH =     "     "    "       "        "     " RADL
C
      DATA R1ROH, DR1ROH, XR1ROH     / 376., 30.,0.0109    /
      DATA R2ROH, DR2ROH, XR2ROH     / 586., 30.,0.0109    /
C
C             R3ROH  = RADIUS OF THIRD ROHACELL SUPPORT
C             DR3ROH = THICKNESS OF THIRD ROHACELL SUPPORT IN MM
C             XR3ROH =     "     "    "      "        "     " RADL
C
C             ROTNK  = INNER RADIUS OF OUTER WALL OF THE TANK
C             DROTNK = THICKNESS OF OUTER TANK WALL IN MM
C             XRLTKO =     "     "    "     "   "    " RADL
C
      DATA R3ROH, DR3ROH, XR3ROH     / 799.04, 25., 0.0147 /
      DATA ROTNK, DROTNK, XRLTKO     / 835., 12., 0.1348   /
C
C             RTOF   = RADIUS OF TOF COUNTERS
C             DRTOF  = THICKNESS OF TOF COUNTERS IN MM
C             XRTOF  =     "     "   "     "      " RADL
C
C             RCOIL  = RADIUS OF COIL
C             DRCOIL = THICKNESS OF COIL IN MM
C             XRCOIL =     "     "    "   " RADL
C
C             ZJM    = Z COORDINATE OF ALU SUPPORT FOR ROHACELL ( - Z )
C             DZJM   = THICKNESS IN MM
C             XRZJM  =     "      " RADL
C
      DATA RTOF,  DRTOF,  XRTOF      / 920., 20., 0.0476   /
      DATA RCOIL, DRCOIL, XRCOIL     / 968., 90., 0.756    /
      DATA ZJM ,  DZJM,   XRZJM      / -1180., -30., 0.337 /
C
C             ZJP    = Z COORDINATE OF ALU SUPPORT FOR ROHACELL ( + Z )
C             DZJP   = THICKNESS IN MM
C             XRZJP  =     "      " RADL
C
C             ZTKM   = Z COORD. OF TANK WALL FOR INNER DETECTOR ( - Z )
C             DZTKM  = THICKNESS IN MM
C             XRZTKM =     "      " RADL
C
C             ZTKP   = Z COORD. OF TANK WALL FOR INNER DETECTOR ( + Z )
C             DZTKP  = THICKNESS IN MM
C             XRZTKP =     "      " RADL
C
      DATA ZJP, DZJP, XRZJP          /  1180.,  30., 0.337 /
      DATA ZTKM,DZTKM,XRZTKM         / -1350., -74., 0.832 /
      DATA ZTKP,DZTKP,XRZTKP         /  1350.,  74., 0.832 /
C
C             ZTOFPL = MAX Z COORDINATE COVERED BY TOF COUNTERS AT +Z
C             ZTOFMI = MAX Z COORDINATE COVERED BY TOF COUNTERS AT -Z
C             ZBPPL  = MAX Z COORDINATE OF BEAM PIPE COUNTERS AT +Z
C             ZBPMI  = MAX Z COORDINATE OF BEAM PIPE COUNTERS AT -Z
C             XRJETC = RADL OF CHAMBER GAS IN MM
C
      DATA ZTOFPL,ZTOFMI             /  1500., -1500.      /
      DATA ZBPPL, ZBPMI              /   750.,  -750.      /
      DATA XRJETC                    / 29910.              /
C
C
C                            LEAD GLASS DATA
C
C
C             RLG    = INNER RADIUS OF LEAD GLASS ARRAY
C             ZLGPL  = MAXIMUM +Z-COMPONENT OF CENTRAL LEAD GLASS ARRAY
C             ZLGMI  = MAXIMUM -Z-COMPONENT OF CENTRAL LEAD GLASS ARRAY
C
      DATA  RLG, ZLGPL, ZLGMI / 1100.,1696.,-1696. /
C
C             OUTR2  = (RLG + BLDEP)
C             CTLIMP = COS THMAX FOR PARTICLES HITTING THE CENTRAL LG
C             CTLIMM = COS THMAX FOR PARTICLES HITTING THE CENTRAL LG
C
      DATA  OUTR2, CTLIMP, CTLIMM / 1400.,0.839,-0.839 /
C
C             DELFI  = ANGULAR INCREMENT CORRESPONDING TO ONE BLOCK
C             BLXY   = DIMENSION OF ONE LEAD GLASS BLOCK IN XY DIRECTION
C             BLZ    = DIMENSION OF ONE LEAD GLASS BLOCK IN Z DIRECTION
C             BLDEP  = DEPTH OF A LEAD GLASS BLOCK
C             BLFI   = DIMENSION OF A BLOCK IN PHI
C
      DATA  DELFI, BLXY, BLZ, BLDEP, BLFI / 0.0748,140.,106.,300.,82.28/
C
C             ZENDPL = Z COORDINATE FOR END CAP LG FACE AT +Z
C             ZENDMI = Z COORDINATE FOR END CAP LG FACE AT -Z
C             DEPEND = DEPTH OF LG COUNTER END CAP
C
      DATA  ZENDPL, ZENDMI, DEPEND / 1514.,-1514.,226. /
C
C             XHOL1,XHOL2  PARAMETERS FOR HOLE AROUND PIPE
C             YHOL1,YHOL2  PARAMETERS FOR HOLE AROUND PIPE
C
      DATA  XHOL1, XHOL2 / 140., 320. /
      DATA  YHOL1, YHOL2 / 140., 320. /
C
C- - - - - - - - -   CGEO2  *  CGEO2COM  - - - - - - - - - - - - - - - -
C
C---------  MACRO CGEO2COM       GEOMETRY OF FORWARD DETECTOR   -------
C
C---  LEAD GLASS BLOCKS
C     FENDC:  WIDTH (AND HEIGHT) OF BLOCKS
C     XYHOL1: DISTANCE FROM BEAM CENTRE TO EDGE OF FIRST
C             HORIZONTAL BLOCK
C     XYHOL2: DISTANCE FROM BEAM CENTRE TO EDGE OF FIRST
C             VERTICAL BLOCK
C     BLDPFW: DEPTH OF BLOCKS
C     ZMINBL: DISTANCE FROM INTERACTION POINT TO FRONT SURFACE
C             OF LEAD GLASS BLOCKS (-Z-DIRECTION, PLUTO/CELLO)
C     ZPLUBL: DISTANCE FROM INTERACTION POINT TO FRONT SURFACE
C             OF LEAD GLASS BLOCKS (+Z-DIRECTION, MARK J)
C---  LUMONITORS
C     XSC:    LENGTH OF LONG EDGE (1: A-COUNTER, 2: B-COUNTER)
C     YSC:    LENGTH OF SHORT EDGE
C     RSC:    DISTANCE FROM BEAM CENTRE TO CENTRE OF INNERMOST LONG
C             EDGE ON A-COUNTER
C     ZMISC:  DISTANCE FROM INTERACTION POINT TO FRONT SURFACE OF
C             LUMONITOR (-Z-DIRECTION, TOWARDS PLUTO/CELLO)
C     ZPLSC:  DISTANCE FROM INTERACTION POINT TO FRONT SURFACE OF
C             LUMONITOR (+Z-DIRECTION, TOWARDS MARK J)
C     DZSC:   THICKNESS OF LUMONITORS
C
C---  DRIFT CHAMBERS
C
C  CHX(I,J): XPOSITION OF WIRE 0 IN CHAMBER I (1-3) IN PLANE J (1-4)
C  CHY AND CHZ ANALOGOUS.     WLEN IS SENSITIVE LENGTH OF WIRES
C  PITCH IS WIRE DISTANCE IN XY-PLANE, WZDIS DISTANCE IN Z BETWEEN
C  THE ODD AND EVEN WIRE PLANES
C  WIRE 0 IS CLOSEST TO THE BEAM LINE
C  PRESENT DATA SETTING OF WIRES BASED ON "EDUCATED GUESSES"
C
C     ALL VALUES IN MM      BLOCK DATA SETTING IN SUBR. JADISP
C
C     DATA FENDC/81./,XYHOL1,XYHOL2/141.,151.5/,BLDPFW/400./,
C    1 ZMINBL,ZPLUBL /-5250.,5250./,
C    2 XSC /230.,150./, YSC /150.,70./, RSC /152.48,192.48/,
C    3 ZMISC/-4235.,-4135./, ZPLSC/4235.,4135./, DZSC/6./,
C    4 CHX/140.,-400.,-140.,140.,-400.,-140.,
C    5 140.,-400.,-140.,140.,-400.,-140./,
C    6 CHY /-400.,140.,-400.,-400.,140.,-400.,
C    7 -400.,140.,-400.,-400.,140.,-400./,
C    8 CHZ /-4770.,-4720.,-4770.,-4020.,-3970.,-4020.,
C    9 4020.,3970.,4020.,4770.,4720.,4770./,
C    A WLEN/800./, PITCH /25./, WZDIS/10./
C
C------ END MACRO CGEO2COM
C-----------------------------------------------------------------------
C                            MACRO CGEO2 .... JADE TAGGING GEOMETRY
C-----------------------------------------------------------------------
C
      COMMON / CGEO2 / FENDC,XYHOL1,XYHOL2,BLDPFW,ZMINBL,ZPLUBL,
     +                 XSC(2),YSC(2),RSC(2),ZMISC(2),ZPLSC(2),DZSC,
     +                 CHX(3,4),CHY(3,4),CHZ(3,4),WLEN,PITCH,WZDIS
C
C--------------------------- END OF MACRO CGEO2 ------------------------
C
C
      DATA FENDC / 81. /, XYHOL1, XYHOL2 / 141.,151.5 /, BLDPFW / 400. /
      DATA ZMINBL, ZPLUBL / -5250., 5250. /
      DATA XSC / 230.,150. /, YSC / 150.,70. /, RSC / 152.48,192.48 /
      DATA ZMISC / -4235.,-4135. /, ZPLSC / 4235.,4135. /, DZSC / 6. /
C
      DATA CHX / -140., -400., 140., -140., -400., 140.,
     +           -140., -400., 140., -140., -400., 140. /
C
      DATA CHY / -400., 140., -400., -400., 140., -400.,
     +           -400., 140., -400., -400., 140., -400. /
C
      DATA CHZ / -4770., -4720., -4770., -4020., -4070., -4020.,
     +            4020.,  4070.,  4020.,  4770.,  4720.,  4770. /
C
      DATA WLEN / 800. /, PITCH / 25. /, WZDIS / 10. /
C
C- - - - - - - - -   CGEO3  *  CGEO3COM  - - - - - - - - - - - - - - - -
C
C----------------------------------------------------------------------
C      MACRO CGEO3COM..  COMMENTS TO CONTENT OF COMMON /CGEO3/
C----------------------------------------------------------------------
C  ZMINM2 AND ZPLUM2 ARE DISTANCES FROM INTERACTION POINT TO FRONT SUR-
C  FACE OF -Z AND +Z FORWARD LEADGLASS, 1981-82 VERSION. THE BLOCK
C  DIMENSIONS ARE THE SAME AS FOR 1979-80, IN CGEO2
C
C  1983- ... TAGGING APPARATUS: LEAD SCINTILLATER UNITS
C  NRPBSC ARE NUMBER OF MODULES ON ONE SIDE IN Z
C  PBSCR1-4 ARE THE RADII FROM BEAMLINE TO SEGMENT JOINTPOINTS
C  PBSCZ1-4 GIVE THE Z COORDINATES, FROM MINUS Z TO PLUS Z
C------------------------ END OF MACRO CGEO3COM -----------------------
C----------------------------------------------------------------------
C      MACRO CGEO3 .... JADE FORWARD DETECTOR GEOMETRY, 1981-3 VERSION
C----------------------------------------------------------------------
C
      COMMON / CGEO3 / ZPLUM2,ZMINM2,NRPBSC,PBSCR(4),PBSCZ(4)
C
C------------------------ END OF MACRO CGEO3 --------------------------
C
C
      DATA ZMINM2,ZPLUM2 / -2950.,2950. /
      DATA NRPBSC / 8 /
      DATA PBSCR  / 104.02,120.02,152.02,264.02 /
C 4TH COMPONENT UPDATED 16.11.87, AFTER CORRECTION FROM ALEX FINCH
C 4TH COMPONENT UPDATED 02.03.89, IT WAS CORRECT AFTER ALL !   J.O.
C     DATA PBSCR  / 104.02,120.02,152.02,232.02 /
      DATA PBSCZ  / -3470.,-2950.,2950.,3470.   /
C
C- - - - - - - - -   CDLATC  - - - - - - - - - - - - - - - - - - - - - -
C
C
C                            THRESHOLDS FOR LEADGLASS LATCHES
C                            THIS IS USED IN STLATC. THIS IS  OUTDATED,
C                            SINCE LATC BANK  IS  DELETED  AND  CREATED
C                            ANEW IN THE TRIGGER SIMULATION ROUTINES.
C
      COMMON / CDLATC / LGBRT, LGQT, LGET(4)
C
C                            LGBRT = THRESHOLD FOR LG ROW LATCHES
C                            LGQT  = THRESHOLD FOR LG END CAP QUADS
C                            LGET  = THRESHOLDS FOR LG TOTAL ENERGY
C                                    (ALL IN MEV)
C
      DATA LGBRT / 10 /
      DATA LGQT  / 10 /
      DATA LGET  / 500, 1000, 2000, 5000 /
C
C- - - - - - - - -   CJVCEL  - - - - - - - - - - - - - - - - - - - - - -
C
C-----------------------------------------------------------------------
C                            MACRO CJVCEL     VERTEX CHAMBER CELLS/WIRES
C-----------------------------------------------------------------------
C
      COMMON / CJVCEL / MCELL,MWIRE
C
C--------------------------- END OF MACRO CJVCEL -----------------------
C
C
C                            MCELL = NUMBER OF CELLS IN VERTEX CHAMBER
C                            MWIRE = NUMBER OF WIRES PER CELL IN VTXCH
C
      DATA MCELL / 24 /
      DATA MWIRE /  7 /
C
C- - - - - - - - -   CJVTXC  - - - - - - - - - - - - - - - - - - - - - -
C
C-----------------------------------------------------------------------
C                            MACRO CJVTXC     VERTEX CHAMBER
C-----------------------------------------------------------------------
C
      COMMON / CJVTXC / RVEC, ANG1, ANG2, DISTPW, FIRSTP, DISTW1,
     +                  ANGL, COSLOR, SINLOR,
     +                  ZRESV, ZMAXV, ZOFFV, ZNAMP, ZALV, TIMEV,
     +                  DRILOR(24), SNLORA(24), CSLORA(24),
     +                  DRVELO(24)
C
C--------------------------- END OF MACRO CJVTXC -----------------------
C
C
C                            RVEC   = OUTER RADIUS OF BEAM PIPE
C                            ANG1   = ANGLE BETWEEN FIRST SIGNAL WIRE
C                                     LINE AND X-AXIS OF DETECTOR
C                                     COORDINATE SYSTEM
C                            ANG2   = BETWEEN TWO MAIN POTENTIAL WIRE
C                                     LINES
C
      DATA RVEC, ANG1, ANG2          / 93., .06545, .2618 /
C
C                            DISTPW = DISTANCE BETWEEN TWO POTENTIAL
C                                     WIRES
C                            FIRSTP = RADIUS OF FIRST MAIN POTENTIAL
C                                     WIRE
C                            DISTW1 = RADIUS OF FIRST SENSE WIRE
C
      DATA DISTPW, FIRSTP, DISTW1    / 9., 103.5, 99. /
C
C                            ANGL   = TILT ANGLE
C                            COSLOR = COS(ANGL)
C                            SINLOR = SIN(ANGL)
C
      DATA ANGL, COSLOR, SINLOR      / -0.279301, 0.961248, -0.275684 /
C
C                            ZRESV  = Z-RESOLUTION
C                            ZMAXV  = EFFECTIVE WIRE LENGTH ?
C                            ZOFFV  = OFFSET OF VERTEX DETECTOR IN
C                                     Z-DIRECTION
C                            ZNAMP  =   PARAMETERS FOR CALCULATION OF
C                            ZALV   =   Z-AMPLITUDES
C
      DATA ZRESV, ZMAXV, ZOFFV       / 20.0, 517.0, 0.0 /
      DATA ZNAMP, ZALV               / 78.0, 1034.0 /
C
C                            TIMEV  = VERTEX CHAMBER DRIFT TIME BIN
C                                     WIDTH  (MM)
      DATA TIMEV                     / 0.001 /
C
C                            DRILOR =  LORENTZ-ANGLE IN EVERY CELL
C                            SNLORA, CSLORA = SIN, COS (DRILOR)
      DATA DRILOR                    / 24*-0.279301 /
      DATA SNLORA                    / 24*-0.275684 /
      DATA CSLORA                    / 24*0.961248  /
C
C                            DRVELO = DRIFT VELOCITY IN EACH CELL
      DATA DRVELO                    / 24*0.001 /
C
C- - - - - - - - -   CIJONV  - - - - - - - - - - - - - - - - - - - - - -
C
      COMMON / CIJONV / POTVXC, ZAROVC,
     *                  POTVGA, ZAROVG
C
C                            GIVES AVERAGE IONISATION POTENTIAL
C                            AND VALUE FOR Z*RHO/A IN
C                            BETHE BLOCH FORMULA FOR :
C                               1) BEAMPIPE AND OUTER VERTEX CHAMBER
C                                  WALL (ALUMINIUM)
C                               2) CHAMBER GAS
C
      DATA POTVXC,ZAROVC             / 160.3, 1.3      /
      DATA POTVGA,ZAROVG             / 284.4, 0.00080  /
C
C
C- - - - - - - - -   CGEOV   - - - - - - - - - - - - - - - - - - - - - -
C
C-----------------------------------------------------------------------
C                            MACRO CGEOV      VERTEX CHAMBER GEOMETRY
C-----------------------------------------------------------------------
C
      COMMON / CGEOV  / RPIPV,DRPIPV,XRLPIV,RVXC,DRVXC,XRLVXC,
     +                  ZVXCM,DZVCM,XRZVCM, ZVXCP,DZVCP,XRZVCP,
     +                  XRVTXC
C
C--------------------------- END OF MACRO CGEOV ------------------------
C
C
C                            RPIPV  = INNER RADIUS OF NEW BEAM PIPE
C                            DRPIPV = THICKNESS OF BEAM PIPE (MM)
C                            XRLPIV = THICKNESS OF BEAM PIPE (RADL)
C                           ( 3 MM ALUMINIUM +                0.03371
C                             20 MU KAPTON + ARALDIT +        0.00011
C                             75 MU KAPTON + 1*17.5 MU CU +   0.00148
C                             50 MU KAPTON + 2*17.5 MU CU +   0.00262
C                             2 COOLING PIPES                 0.00107 )
C
CCC   DATA RPIPV, DRPIPV, XRLPIV     / 90., 3., 0.0337 /
      DATA RPIPV, DRPIPV, XRLPIV     / 90., 3., 0.03899 /
C
C                            RVXC   = RADIUS OF OUTER VERTEX CHAMBER
C                                     WALL
C                            DRVXC  = THICKNESS IN (MM)
C                            XRLVXC = THICKNESS IN (RADL)
C                           ( 1 MM ALUMINIUM +                0.01124
C                             0.5 MM G10 +                    0.00258
C                             50 MU KAPTON + 2*17.5 MU CU +   0.00262 )
C
CCC   DATA RVXC, DRVXC, XRLVXC       / 159., 1., 0.0112 /
      DATA RVXC, DRVXC, XRLVXC       / 159., 1., 0.01644 /
C
C                            ZVXCM  = Z-COORDINATE OF VERTEX CHAMBER
C                                     END PLATE ( -Z )
C                            DZVCM  = THICKNESS IN (MM)
C                            XRZVCM = THICKNESS IN (RADL)
      DATA ZVXCM,DZVCM,XRZVCM        / -380., -15., 0.1685 /
C
C                            ZVXCP  = Z-COORDINATE OF VERTEX CHAMBER
C                                     END PLATE ( +Z )
C                            DZVCP  = THICKNESS IN (MM)
C                            XRZVCP = THICKNESS IN (RADL)
      DATA ZVXCP,DZVCP,XRZVCP        /  380.,  15., 0.1685 /
C
C                            XRVTXC = RADL OF CHAMBER GAS IN (MM)
      DATA XRVTXC                    / 29910. /
C
C- - - - - - - - -   CIEVS   - - - - - - - - - - - - - - - - - - - - - -
C
C                            IEVMIN = 1ST  EVENT REQUESTED FOR TRACKING
C                            IEVMAX = LAST EVENT REQUESTED FOR TRACKING
C
      COMMON / CIEVS  / KIEV,IEVMIN,IEVMAX
C
      DATA IEVMIN /      1 /
      DATA IEVMAX / 999999 /
C
C- - - - - - - - -   CVFLAG  *  CVERR  - - - - - - - - - - - - - - - - -
C
C                            MESSAGES AND FLAGS USED (MAINLY) FOR INPUT
C                            ERROR CHECKING IN THE MONTE CARLO
C
      LOGICAL VTEST
C
      COMMON / CVFLAG / VTEST(20)
      COMMON / CVERR  / MESSAG(20)
C
      DATA MESSAG / 20 * 0      /
      DATA VTEST  / 20 * .TRUE. /
C
C- - - - - - - - -   CFLAG   - - - - - - - - - - - - - - - - - - - - - -
C
C        LFLAG(1) = SMEAR GAMMA AND ELECTRON ENERGIES
C        LFLAG(2) = GAMMA CONVERSION IN OUTER TANK AND COIL (TRKGAM)
C        LFLAG(3) = ABSORPTION LOSSES
C        LFLAG(4) = 3 DIM SHOWER PROFILE FIT TO EGGS CODE SLOW!!!!!
C        LFLAG(5) = .TRUE.   -->  WITH VERTEX CHAMBER TRACKING
C                 = .FALSE.  -->  WITHOUT VERTEX CHAMBER TRACKING
C                                 BUT OLD BEAM PIPE GEOMETRY AND
C                                 BEAM PIPE COUNTERS (BEFORE MAI 84)
C
C           ====> DEFAULT .TRUE.,.FALSE.,.TRUE.,.FALSE.,.FALSE.
C
      LOGICAL * 1 LFLAG
C
      COMMON / CFLAG  / LFLAG(10)
C
      DATA LFLAG  / .TRUE.,.FALSE.,.TRUE.,.FALSE.,.FALSE.,5*.FALSE. /
C
C- - - - - - - - -   TODAY   - - - - - - - - - - - - - - - - - - - - - -
C
C /TODAY/ HOLDS A DATE TO SPECIFY VERSION OF JADE DETECTOR IN MC
C                         TRACKING JOBS.
C
C       HDATE(1) GIVES SECONDS, HDATE(2) GIVES MINUTES
C       HDATE(3) GIVES HOURS,   HDATE(4) GIVES DAYS
C       HDATE(5) GIVES MONTHS,  HDATE(6) GIVES YEARS
C               (17.5 IS THE NORWEGIAN NATIONAL DAY...)
      INTEGER*2 HDATE
      COMMON /TODAY/ HDATE(6)
C
      DATA HDATE /1,1,1,17,5,1985/
C
C
C- - - - - - - - -   CKALPR  - - - - - - - - - - - - - - - - - - - - - -
C
      COMMON / CKALPR / NOHEAD, IBADF, IBADEN, LASTR1, LASTR2
C
      DATA  NOHEAD / 0 /, IBADF / 0 /, IBADEN / 0 /
      DATA  LASTR1 / -1 /, LASTR2 / -1 /
C
C------  USER COMMON - DEDXBN
C
      COMMON /CDEDXU/ NHFCUT,PMCUT
      DATA NHFCUT / 10 /, PMCUT/0.01/
C
C   COMMON FOR USE IN FLIGHT TIME CORRECTION, SUBR. JFTNEW
C
      COMMON /CFLMAS/ AFLMAS
      DATA  AFLMAS /0.511E-3/
C
C   COMMON FOR USE FOR CUTS IN SUBR. ZSRFTV
C
      COMMON/ CCMZCT / DIMPCT, ZCUTV, ZCUTVV, IZVCST(5), ZCHWW
      DATA  DIMPCT /15./, ZCUTV /800./, ZCUTVV /1600./, IZVCST/5*0/
      DATA  ZCHWW /50.0 /
C
C   Z-RESOLUTION FOR DIFFERENT PERIODS  (SUBR. AMPS2Z)
C
C
C  COMMON FOR Z-RESOLUTION PARAMETERS,   USED IN SUBR. AMPS2Z
C
      COMMON / CZSPRM / NZSPRD,
C                                           PARMS FOR RESOLUTIONS
     * AZSRS0(3), AZSRSA(3),
C                                           PARMS FOR CUTS
     * AZSCT1(3), AZSCT2(3), AZSCT3(3), AZSCT4(3),
C                                           SECOND HIT
     * AZSSHT(5,3),
C                                           SECOND HIT DISTANCE
     * AZSSHD(3),
C                                           AVE RESOLUTIONS
     * AZSSAV(3),
C                                           ZSPD BANK FILL FLAG
     * LZSPDF
      LOGICAL LZSPDF

      DATA NZSPRD / 1 /
C  FOLLOWING LINE CHANGED 20/5/88  J.H./ E.E.
CCCC  DATA AZSRS0 / 4., 4., 13. /, AZSRSA / 4480., 7000., 10000. /
      DATA AZSRS0 / 10., 10., 13. /, AZSRSA / 4480., 7000., 10000. /
      DATA AZSCT1 / 140., 140., 225. /,
     *     AZSCT2 / 100., 100., 150. /,
     *     AZSCT3 / 100., 100., 150. /,
     *     AZSCT4 / 100., 100., 180. /,
     *     AZSSHT /12.3, .183, .011 , .365, .106,
     *             7.5, .045, .0015, .200, .055 ,
     *             7.5, .045, .0015, .200, .055 /,
     *     AZSSHD / 3., 3., 2. /,
     *     AZSSAV / 19., 19., 32. /,
CCCC *     AZSSAV / 13., 17., 32. /,  CHANGED 20/5/88  J.H./E.E.
     *     LZSPDF / .FALSE. /
C
C- - - - - - - - -   CZGEO, CZCAL, CZMKON   - - - - - - - - - - - - - -
C
C                            Z CHAMBER CONSTANTS
C
C
C
C                            RZCHI  = RADIUS FOR BARLOW
C                            RZCHA  = RADIUS FOR BARHIGH
C                            NZRPHI = NUMBER OF CELLS IN R-PHI
C                            NZZ    = NUMBER OF CELLS IN Z
C                            Z1ZCH  = LOWER END OF Z-CHAMBER
C                            Z2ZCH  = UPPER END OF Z-CHAMBER
C
C                            WIRE POSITIONS INSIDE ONE CELL
C
C                            ZCHA  = DIST. OF 1ST WIRE FROM BARLOW: 5 MM
C                            ZCHB  = DIST. OF 2ND WIRE FROM BARLOW:15 MM
C                            ZCHSS = STAGGERING OF THE WIRES:       1 MM
C
C                            DEAD SPACES OF WHOLE Z CHAMBER
C
C                            ZCHDL = DEAD SPACES FOR EACH HALF SHELL ON
C                                    TOP AND BOTTOM, JEWEILS +/- DL
C                            ZCHDLL= DEAD SPACES RIGHT AND LEFT FOR GAS
C                                    IN/OUTFLOW, JEWEILS +/- DLL
C
C                            DLZZ   = SEGMENT LENGTH IN Z
C                            DLZPMI = INNER SEGMENT LENGTH IN R-PHI
C                            DLZW1  = WIRE LENGTH OF WIRE1 IN ONE SEG.
C                            DLZW2  =      ....      WIRE2  ....
C
       COMMON / CZGEO / RZCHI,RZCHA,NZRPHI,NZZ,Z1ZCH,Z2ZCH,
     +                  ZCHA, ZCHB, ZCHSS, ZCHDL, ZCHDLL,
     +                  DLZZ, DLZPMI, DLZW1, DLZW2
C
       DATA  RZCHI,RZCHA  / 864.5,884.5 /
       DATA  NZRPHI,NZZ   / 24,16 /
       DATA  Z1ZCH,Z2ZCH  /-1200.,1200. /
       DATA  ZCHA, ZCHB, ZCHSS   / 5., 15., 1. /
       DATA  ZCHDL, ZCHDLL    /100., 20. /
       DATA  DLZZ, DLZPMI, DLZW1, DLZW2  /150., 227.63, 228.94, 231.58/
C
C                            CHAMBER CONSTANTS PULSEHEIGHTS, RESOLUTIONS
C
C                            ZSIGX  = SPAT. RES. TRANS. TO WIRE (0.3 MM)
C                            ZA0    = MEAN (?) AMPLITUDE    1000
C                            ZDA0   = WIDTH OF PULSE HEIGHT SPEC. +/-30000064653
C                            ZSIGAL = SPATIAL RES. ALONG WIRE 25MM .. 1%
C                            ZDPAUF = DOUBLE PULSE RESOLUTION  (4 MM)
C
C                            WIRE-SUPPORT
C
C                            ZDSPAC = SPACE IN R-PHI AT ONE SIDE OF
C                                     ONE OF 24 R/PHI-SEGMENTS
C                                     WITH REDUCED EFFICIENCY ZEFF.
C                                     THE REAL DEADSPACE FOR ONE WIRE
C                                     SUPPORT IS THEN TWICE AS BIG AS
C                                     ZDSPAC.   1.25 MM
C
C                            ZEFF   = EFFICIENCY IN DEAD SPACE   10%
C                            NZ00   = AVERAGE NUMBER OF RANDOM HITS
C                            NZDD   = FLUCTUATION OF N00 -GLEICHVERTEILT
C
      COMMON / CZCAL /  ZSIGX, ZA0, ZDA0, ZSIGAL, ZDPAUF,
     +                  ZDSPAC, ZEFF, NZ00, NZDD
C
      DATA  ZSIGX, ZA0, ZDA0, ZSIGAL / 0.30, 1000.,300., 25. /
      DATA  ZDPAUF                   / 4. /
      DATA  ZDSPAC, ZEFF             / 1.25, .10 /
      DATA  NZ00, NZDD               / 5, 2 /
C
C                            CHAMBER CONSTANTS FOR MC.
C
C                            ZCVDR = DRIFT VELOCITY IN MM/TIMEBIN
C                            ZCXCH = (A2-A1)/A = XCH*(L1-L2)/L
C                                    CONSTANT FOR CHARGE DIVISION
C                            ZCTZER= TIME PEDESTAL
C                            ZCAPED= AMPLITUDE PEDESTAL
C                            ZXLI  = ZXCH * L  (INNER WIRE )
C                            ZXLO  = ZXCH * L  ( OUTER WIRE )
C
      COMMON / CZMKON / ZCVDR, ZCXCH, ZCTZER, ZCAPED, ZXLI, ZXLO
C
      DATA ZCVDR, ZCXCH, ZCTZER, ZCAPED   / 0.34, 1., 0., 0. /
      DATA ZXLI, ZXLO  / 2547.28, 2578.96 /
C
C   COMMONS BELOW BELONG TO F22KLE.JVTXC.GS/GL    <<<<========
C
C-
C   --- FOR COMFIT-COMMON / CCOMF / :
C"
C
C-----------------------------------------------------------------------        
C                                                                               
C   COMMON-DEFINITION FOR                 14.04.86  R.RAMCKE                    
C   COMFIT PARAMETER COMMON               09.02.87  C.KLEINWORT                 
C                                                                               
      COMMON / CCOMF / SCF1, SCF2, SCF3, SCF4,                                  
     &                 DCF1, DCF2, ITMX, IVHMN, IPRBN, CFPMIN, MODECF,          
     &                 CFSFAC             , VCWGHT  !!!   VCWGHT added 02.02.98  
C                                                                               
C----------------------------------------------------------------------- 

C
C-  IF SCF1 OR SCF2 < 0, CALCULATION OF SCF1 OR SCF2 INDIVIDUALLY FOR
C                                                        EACH TRACK
C   SCF1   : 1/SIGMA_ID**2
C   SCF2   : 1/SIGMA_VTXC**2
C   SCF3   : FACTOR FOR  1/RMS_THETA**2 (SCATTERING ANGLE R-PHI)
C   DCF1   : MAXIMUM ALLOWED RESIDUAL FOR PREFIT OF VTXC-HITS
C   DCF2   : MAXIMUM RESIDUAL FOR COMFIT
C   ITMX   : MAXIMUM NUBER OF ITERATIONS IN COMFIT
C   IVHMN  : MINIMUM NUMBER OF REMAINING VTXC-HITS AFTER ITERATION
C   IPRBN  : PATR BANK NUMBER AS INPUT FOR COMFIT
C            (AUTOMATICALLY SET BY VTXCSV BY INPUT PARAMETER NBPCFT)
C   CFPMIN : TRACKS WITH A TRANSVERSE MOMENTUM < CFPMIN WILL NOT BE
C            TAKEN FOR COMFIT
C   MODECF : MODE FLAG FOR COMFIT
C       THE TWO FOLLOWING PARAMETER CAN BE SET AUTOMATICALLY BY
C       CALLING THE SUBROUTINE VTXCSF JUST BEFORE THE CALL TO VTXCSV
C   CFSFAC : FUDGE FACTOR FOR ADDITIONAL TRACK SMEARING
C   VCWGHT : WEIGHT FOR RUN VERTEX
C
C========== D O   N   O   T   C H A N G E ==============================
C                F R O M
C"
      DATA SCF1   / -25.0 /
      DATA SCF2   / 44.0 /
      DATA SCF3   / 1.0 /
      DATA SCF4   / 0.01 /
      DATA DCF1   / 0.500 /
      DATA DCF2   / 0.600 /
      DATA ITMX   / 5 /
      DATA IVHMN  / 3 /
C-                 T O
C========== D O   N   O   T   C H A N G E ==============================
C"
      DATA IPRBN  / 10 /
      DATA CFPMIN / 0.100 /
      DATA MODECF / 0 /
      DATA CFSFAC / 1.00 /
      DATA VCWGHT / 1.00 /
C
C   END OF COMMONS FROM TO F22KLE.JVTXC.GS/GL    <<<<========
C
C
      END
