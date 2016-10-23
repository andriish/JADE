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
#include "cgeo1.for"
#include "cgraph.for"
#include "cgeov.for"
#include "mvccal.for"
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
