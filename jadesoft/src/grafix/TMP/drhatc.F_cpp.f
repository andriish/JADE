C   01/11/84 807241321  MEMBER NAME  DRHATC   (S)           FORTRAN
C
C-----------------------------------------------------------------------
      SUBROUTINE DRHATC( IVIEW , NPNT, RADIN, RADOUT, ZDOWN, ZUP )
C-----------------------------------------------------------------------
C
C    AUTHOR:   J. HAGEMANN   09/10/84 :  HATCH SIMPLE SHAPES ON SCREEN
C
C LAST  MOD:   J. HAGEMANN   14/02/86 :  DRAW CHAMBER IN TRUE POSITION
C                                        RELATIVE TO THE ORIGIN
C     IVIEW = 1  :  R PHI VIEW
C     IVIEW = 2  :  RZ VIEWS
C     NPNT       :  NUMBER HATCH LINES
C     RADIN      :  INNER RADIUS OF FIGUR
C     RADOUT     :  OUTER RADIUS OF FIGUR
C     ZDOWN      :  LOWER Z-COORDINATE OF FIGUR
C     ZUP        :  UPPER Z-COORDINATE OF FIGUR
C
C-----------------------------------------------------------------------
C
      IMPLICIT INTEGER*2 (H)
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
      COMMON / CJTRIG / PI,TWOPI
      COMMON / CHEADR / HEAD(108)
      COMMON / CHATCH / IHAT
C
C-----------------  C O D E  -------------------------------------------
C
      GO TO ( 1 , 2 ) , IVIEW
C
C                            R PHI VIEW
C
    1 DLTRAD = RADOUT - RADIN
      XDEV = 0.0
      YDEV = 0.0
      IF( (HEAD(18) .LE. 100) .OR. (IHAT .EQ. 0) ) GO TO 3
         XDEV = VDX
         YDEV = VDY
    3 TANBET = TAN(PI*0.125)
      ALP    = 0.0
      DALP   = TWOPI/FLOAT(NPNT)
      DO 5  I = 1, NPNT
C
         ALP = ALP + DALP
         COSALP = COS(ALP)
         SINALP = SIN(ALP)
C
         XSTEP1 = RADIN*COSALP + XDEV
         YSTEP1 = RADIN*SINALP + YDEV
         XSTEP2 = RADOUT*COSALP + DLTRAD*TANBET*SINALP + XDEV
         YSTEP2 = RADOUT*SINALP - DLTRAD*TANBET*COSALP + YDEV
C
        CALL DRAMOV( -XSTEP1, YSTEP1, -XSTEP2, YSTEP2, 0 )
C
    5 CONTINUE
C
      RETURN
C
C                            RZ VIEWS
C
    2 DLTRAD = ABS(RADOUT - RADIN)
      TANBET = TAN(PI*0.125)
      MPNT   = NPNT
      ZSTPMN = ABS(ZUP - ZDOWN)/FLOAT(MPNT)
C
      ZSTEP1 = ZDOWN - 0.5*ZSTPMN
C
      DO 15  I = 1, NPNT
C
        ZSTEP1 = ZSTEP1 + ZSTPMN
        ZSTEP2 = ZSTEP1 + TANBET*DLTRAD
C
        CALL DRAMOV( ZSTEP1, RADOUT, ZSTEP2, RADIN, 0 )
C
   15 CONTINUE
C
      RETURN
      END
