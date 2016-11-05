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
     *      T0, VD, CSLOR, SNLOR, VROT, VDX, VZX, VDY, VZY,
     *      S0R(2,168), CVD(2,168),
     *      VIHCRR(7,2,25)
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
