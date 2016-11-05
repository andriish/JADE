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
#include "mvccal.for"
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
