C   09/03/84 406201632  MEMBER NAME  JOYS     (JADEGS)      FORTRAN
C
C-----------------------------------------------------------------------
      SUBROUTINE JOYS(N,A)
C-----------------------------------------------------------------------
C
C   AUTHOR:  J. OLSSON         ?    : HANDLES JOYS COMMAND
C
C      MOD:  C. BOWDERY     9/03/84 : TRAP DIVIDE CHECK ERROR
C      MOD:  C. BOWDERY    11/03/84 : ENSURE PICTURE ALWAYS ON SCREEN
C LAST MOD:  C. BOWDERY    12/03/84 : PROTECT AGAINST -VE MAGNIFICATION
C
C     THIS ROUTINE HANDLES THE JOYSTICK IN THE GRAPHICS PROGRAM.
C
C     JOYS             A = 0.0     USER DEFINES A BOX
C     JOYS A           A > 0.0     USER DEFINES CENTRE, A = MAGNIF.
C
C-----------------------------------------------------------------------
C
      IMPLICIT INTEGER*2 (H)
C
      LOGICAL FL18,FL22,FL24
C
#include "cgraph.for"
C
      COMMON / CWORK1 / HWORK(40),SIZEWN,RANGE,XCENT,YCENT
      COMMON / CPROJ  / XMINR,XMAXR,YMINR,YMAXR,IPRJC,FL18,FL22,FL24
C
C------------------  C O D E  ------------------------------------------
C
C                            STORE EXISTING COORDINATES IN CASE OF ERROR
C
      XMINOL = XMIN
      XMAXOL = XMAX
      YMINOL = YMIN
      YMAXOL = YMAX
C                            WAS THERE A TRAILING NON-ZERO NUMBER?
C
      IF( A .NE. 0.0 ) GO TO 3
C
C                            FLASH CURSOR ON SCREEN AND AWAIT RESULT
C
      CALL VCURSR(HWORK(1),XMIN,YMIN)
C
C                            DRAW CROSS LINES TO MARK POSITION
C
      CALL MOVEA( XMIN   , YMINOL )
      CALL DRAWA( XMIN   , YMAX   )
      CALL MOVEA( XMINOL , YMIN   )
      CALL DRAWA( XMAX   , YMIN   )
C
C                            FLASH CURSOR ON SCREEN AND AWAIT RESULT
C
      CALL VCURSR(HWORK(1),XMAX,YMAX)
C
C                            DRAW CROSS LINES TO MARK POSITION
C
      CALL MOVEA( XMAX   , YMINOL )
      CALL DRAWA( XMAX   , YMAXOL )
      CALL MOVEA( XMINOL , YMAX   )
      CALL DRAWA( XMAXOL , YMAX   )
C
C                            CHECK THAT THE USER HAS DEFINED A BOX
C                            WITH NON-ZERO DIMENSIONS
C
      IF( XMAX .NE. XMIN   .AND.   YMAX .NE. YMIN ) GO TO 1
        WRITE(JUSCRN,2)
  2     FORMAT( ' ===> ERROR:  BOX HAS ZERO AREA!'/
     +          ' ===> ACTION: COMMAND IGNORED')
        CALL PROMPT
        XMIN = XMINOL
        XMAX = XMAXOL
        YMIN = YMINOL
        YMAX = YMAXOL
        GO TO 8
C
C                            ENSURE 2ND CURSOR POSITION DRAWN
C
  1   CALL TRMOUT(80,'VIEW WILL BE RE-DRAWN^')
C
      XSIZE  = ABS(XMAX - XMIN)
      YSIZE  = ABS(YMAX - YMIN)
C
C                            ALLOW FOR MISSING TOP OF SCREEN PICTURE
C
      SIZEWN = AMAX1( XSIZE , 3.0 * YSIZE / 2.0 )
C
C                            IF SCALE DETERMINED BY YSIZE THEN CENTRE
C                            PICTURE IN X
C
      OFFSET = 0.0
      IF( SIZEWN .NE .XSIZE ) OFFSET = ( SIZEWN - XSIZE ) / 2.0
C
      IF( XMIN .LE. XMAX ) GO TO 4
      XMIN   = XMAX
  4   XMIN   = XMIN - OFFSET
      XMAX   = XMIN + SIZEWN
C
C                            IF SCALE DETERMINED BY XSIZE THEN CENTRE
C                            PICTURE IN Y
C
      OFFSET = 0.0
      IF( SIZEWN .EQ .XSIZE )
     +                OFFSET = ( 2.0 * SIZEWN / 3.0 - YSIZE ) / 2.0
C
      IF( YMIN .LE. YMAX ) GO TO 5
      YMIN   = YMAX
  5   YMIN   = YMIN - OFFSET
      YMAX   = YMIN + SIZEWN
      GO TO 6
C
C                            USER MUST PROVIDE CENTRE POINT ONLY
C
  3   IF( A .GT. 0.0 ) GO TO 30
        WRITE(JUSCRN,31)
 31     FORMAT(' ===> ERROR:  MAGNIFICATION CANNOT BE NEGATIVE'/
     +         ' ===> ACTION: COMMAND IGNORED BUT PICTURE RE-DRAWN')
        CALL PROMPT
        GO TO 8
 30   CALL VCURSR(HWORK(1),XCENT,YCENT)
      RANGE = (XMAX - XMIN)*0.5/A
C
C                            CENTRE OF SCREEN IS NOT CENTRE OF WINDOW
C                            (THE OFFSETS USE EMPIRICAL CONSTANTS)
C
      XMIN   = XCENT - RANGE
      XMAX   = XCENT + RANGE
      OFFSET = ( XMAX - XMIN ) * 0.01
      XMIN   = XMIN - OFFSET
      XMAX   = XMAX - OFFSET
C
      YMIN   = YCENT - RANGE
      YMAX   = YCENT + RANGE
      OFFSET = ( YMAX - YMIN ) * 0.1144
      YMIN   = YMIN + OFFSET
      YMAX   = YMAX + OFFSET
C
  6   CALL DWINDO(XMIN,XMAX,YMIN,YMAX)
C
C                            MAGNIFIED VIEW
C
      FL22  = .TRUE.
C
  8   XMINR = XMIN
      YMINR = YMIN
      XMAXR = XMAX
      YMAXR = YMAX
C
      RETURN
      END
