C   01/11/84 807241300  MEMBER NAME  DRAWBP   (S)           FORTRAN
C
C-----------------------------------------------------------------------
      SUBROUTINE DRAWBP( IVIEW , VTXCHR )
C-----------------------------------------------------------------------
C
C    AUTHOR:   C. BOWDERY    26/04/84 :  DRAW BEAM PIPE    (FROM JADISP)
C
C       MOD:   C. BOWDERY    27/06/84 :  VTXCHR IS AN ARGUMENT NOW
C       MOD:   J. HAGEMANN   09/10/84 :  HATCH BEAM PIPE ON SCREEN
C LAST  MOD:   J. HAGEMANN   14/02/86 :  DRAW CHAMBER IN TRUE POSITION
C                                        RELATIVE TO THE ORIGIN
C
C
C     THE BEAM PIPE DRAWN DEPENDS ON THE EVENT DATE. VTXCHR IS .TRUE.
C     IF THE NEW, SMALLER PIPE IS TO BE DRAWN.
C       IVIEW = 1  :  R PHI VIEW
C       IVIEW = 2  :  RZ VIEWS
C
C-----------------------------------------------------------------------
C
      IMPLICIT INTEGER*2 (H)
C
      LOGICAL  VTXCHR, DSPDTM
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
C   23/03/97 703231941  MEMBER NAME  MVCCAL   (PATRECSR)    SHELTRAN
C**HEADER*** MEMBER  MVCCAL         SAVED BY F22KLE  ON 87/02/09  AT 17:52
C   27/06/85 702091751  MEMBER NAME  MVCCAL   (S)           FORTRAN
C
C         THIS MACRO CONTAINS THE DECLARATION FOR THE
C         VTXC-CALIBRATION-COMMON / CVCCAL / :
C
C------------------------------------------------- 03.03.86 J.H. -------
C
      REAL*8 VCDATE
      COMMON / CVCCAL / NVFREE, VFREE(50), VCDATE,
     &      T0, VD, CSLOR, SNLOR, VROT, VDX, VZX, VDY, VZY,
     &      S0R(2,168), CVD(2,168),
     &      VIHCRR(7,2,25)
C
C     NVFREE  : NUMBER OF FREE REAL WORDS IN VFREE
C     VFREE   : REAL ARRAY FOR CONSTANTS
C     T0      : GLOBAL T0                              (. 0.1 NS .)
C     VD      : GLOBAL DRIFTVELOCITY                   (. MM/(0.1 NS) .)
C     CSLOR   : COS( LORENTZ-ANGLE )
C     SNLOR   : SIN( LORENTZ-ANGLE )
C     VROT    : ROTATION VTXC - ID                     (. RADIAN .)
C     VDX     : DISPLACEMENT VTXC - ID IN X            (. MM .)
C     VZX     : SLOPE OF " IN Z
C     VDY     : DISPLACEMENT VTXC - ID IN Y            (. MM .)
C     VZY     : SLOPE OF " IN Z
C     S0R     : DISPLACEMENT FOR EACH WIRE (#)         (. MM .)
C     CVD     : DRIFTVEL.-CORRECTURE FOR EACH WIRE (#)
C
C     VIHCRR  : LAYER-DEPENDENT CORRECTIONS FOR INHOMOGENITIES
C                                                      (. MM . )
C
C     VFREE(50) : D(VROT)/D(Z)                         (. RADIAN/MM .)
C
C     VFREE(48,49) JOBNAME OF JOB GENERATING THIS CALIBRATION
C
C     (#)     : FOR EACH DRIFTSPACESIDE
C
C
      COMMON / CHEADR / HEAD(108)
      COMMON / CGRAP2 / BCMD,DSPDTM(30),ISTVW,JTVW
      COMMON / CHATCH / IHAT
C
C-----------------  C O D E  -------------------------------------------
C
C                            DETERMINE INNER AND OUTER RADII
C
C                                RPIP(V)  = INNER RADIUS
C                                DRPIP(V) = THICKNESS OF PIPE
C
      XDEV = 0.0
      YDEV = 0.0
      IHAT = 0
      RADIN  = RPIP
      RADOUT = RPIP + DRPIP
      IF( .NOT. VTXCHR ) GO TO 111
C                            LOAD CONSTANTS FOR RELATIVE POSITION
         RADIN  = RPIPV
         RADOUT = RPIPV + DRPIPV
         IF( HEAD(18) .LE. 100 ) GO TO 111
         XDEV = VDX
         YDEV = VDY
         IHAT = 1
C
  111 GO TO ( 1 , 2 ) , IVIEW
C
C                            R PHI VIEW
C
C                            CALCULATE NUMBER OF LINES TO DRAW A CIRCLE
C
   1  DDD   = 10.24 * 25.4 / ( XMAX - XMIN )
      NN     = 25.0 * 15.0 * DDD
      IF( NN .LT. 40 ) NN = 40
C
C                            DRAW INNER AND OUTER SURFACES OF BEAM PIPE
C
      CALL PLYGON( NN, RADIN , -XDEV, YDEV, 0 )
      CALL PLYGON( NN, RADOUT, -XDEV, YDEV, 0 )
C
C                            HATCH BEAM PIPE IF CDTL 45 ON
C
      IF( DSPDTM(15) )
     +       CALL DRHATC( IVIEW, 144, RADIN, RADOUT, 0., 0. )
      IHAT = 0
      RETURN
C
C                            RZ VIEWS
C
   2  CALL DRAMOV(ZENDMI, RADIN, ZENDPL, RADIN, 0)
      CALL DRAMOV(ZENDMI,-RADIN, ZENDPL,-RADIN, 0)
      CALL DRAMOV(ZENDMI, RADOUT,ZENDPL, RADOUT,0)
      CALL DRAMOV(ZENDMI,-RADOUT,ZENDPL,-RADOUT,0)
C
C                            HATCH BEAM PIPE IF CDTL 45 ON
C
      IF( .NOT. DSPDTM(15) ) RETURN
      CALL DRHATC(IVIEW, 250, RADIN, RADOUT, ZENDMI, ZENDPL)
      CALL DRHATC(IVIEW, 250,-RADIN,-RADOUT, ZENDMI, ZENDPL)
      RETURN
      END
