C   01/11/84 508081235  MEMBER NAME  PROJEC   (S)           FORTRAN
C
C-----------------------------------------------------------------------
      SUBROUTINE PROJEC( LAST )
C-----------------------------------------------------------------------
C
C    AUTHOR:      ?            ?    :  DRAW PROJECTIONS AT SIDE
C
C       MOD:   J. OLSSON   17/12/83 :
C       MOD:   C. BOWDERY   8/06/84 :  NEW COMMAND NUMBERS
C       MOD:   C. BOWDERY  21/09/84 :  CMD 106 RECOGNISED NOW
C       MOD:   J. HAGEMANN 19/10/84 :  FOR VIEW VC
C  LAST MOD:   C. BOWDERY   7/08/85 :  BUGS REMOVED. GENERAL TIDY UP
C
C-----------------------------------------------------------------------
C
      IMPLICIT INTEGER*2 (H)
C
      LOGICAL  LFLAG
      LOGICAL  FL18, FL22, FL24, DSPDTM
C
#include "cgraph.for"
C
      COMMON / CPROJ /XMINR,XMAXR,YMINR,YMAXR,IPRJC,FL18,FL22,FL24,KPROJ
      COMMON / CGRAP2 / BCMD,DSPDTM(30),ISTVW
C
      DIMENSION  KVIEW1(30), KVIEW2(30)
C
C                            TABLES THAT GIVES PROJECTION VIEWS FOR THE
C                            CURRENT VIEW
C
      DATA  KVIEW1
     +    / 4,5,6,  1,2,3,3,   1,2,3,3, 0,0,4,0,0, 18,17,17,4, 10 * 0 /
      DATA  KVIEW2
     +    / 8,9,10, 8,9,10,11, 4,5,6,7, 0,0,8,0,0, 19,19,18,8, 10 * 0 /
C
C------------------  C O D E  ------------------------------------------
C
C                            PRO ONLY DONE FOR RA,RB,RC,
C                                              ZXA,ZXB,ZXC,ZXD,
C                                              ZYA,ZYB,ZYC,ZYD,
C                                              CYL,VRX,VRZX,VRZY,VC
C
C                            READ KVIEW1 TABLE TO FIND FIRST PROJECTION
C
      IF( LAST .GT. 30 ) RETURN
      JVIEW = KVIEW1( LAST )
C
      IF( JVIEW .EQ. 0 ) RETURN
C
C                            SET FLAG TO INDICATE PROJECTION DRAWING
C
      NACMD = ACMD
      FL18  = .TRUE.
C
C                            DEFINE WINDOW SIZE VARIABLES
C
      ISTX  = 3095
      IADD  = 1000
      ISTY  = 2250
C
      IF( JVIEW .NE. 5 ) GO TO 2
C
        ISTX  = 3195
        IADD  =  900
C
C                            EVENT DISPLAY UNLESS LAST COMMAND WAS
C                            RES, MUR2, VRES, MUPT OR MUONS
C
   2  LFLAG = .TRUE.
      IF( LSTCMD .EQ. 45  .OR.  LSTCMD .EQ. 55  .OR.  LSTCMD .EQ. 52
     +   .OR.  LSTCMD .EQ. 105  .OR. LSTCMD .EQ. 106 ) LFLAG = .FALSE.
C
C                            CDTL 40 ON? THEN DON'T DRAW BOTTOM PROJ.
C
      IF( DSPDTM(10) ) GO TO 30
C
C                            DEFINE THE DRAWING WINDOW IN SCREEN COORDS
C                            FOR THE FIRST PROJECTION (BOTTOM RIGHT)
C
        CALL TWINDO( ISTX, ISTX+IADD, 0, IADD )
C
C                            DRAW THE PROJECTION JVIEW
C
        CALL PROJVW( JVIEW, LFLAG )
C
C                            DEFINE THE DRAWING WINDOW IN SCREEN COORDS
C                            FOR THE SECOND PROJECTION (TOP RIGHT)
C
  30  CALL TWINDO( ISTX, ISTX+IADD, ISTY, ISTY+IADD )
C
C                            USE KVIEW2 TABLE TO FIND PROJECTION VIEW
C
      JVIEW = KVIEW2( LAST )
C
C                            DRAW THE PROJECTION JVIEW
C
      CALL PROJVW( JVIEW, LFLAG )
C
C                            RESET THE DRAWING WINDOW TO FULL SIZE
C
      CALL TWINDO (0,4095,0,4095)
      LASTVW = LAST
C
      K = LAST
      IF( ISTVW .EQ. 1 ) K = 0
      CALL SETSCL( K )
C
C                            NOW WE CAN DRAW IN THE AXES BY THE PROJS.
C
      IF( DSPDTM(10) ) GO TO 50
        KPROJ = 1
        IF( NACMD .NE. 99  .AND.  LFLAG ) CALL CROAX( KVIEW1(LAST) )
C
  50  KPROJ = 2
      IF( NACMD .NE. 99  .AND.  LFLAG ) CALL CROAX( KVIEW2(LAST) )
C
C
      FL18 = .FALSE.
      IF( .NOT. FL22  .OR.  ISTVW .EQ. 1 ) RETURN
      XMIN = XMINR
      XMAX = XMAXR
      YMIN = YMINR
      YMAX = YMAXR
      CALL DWINDO (XMIN,XMAX,YMIN,YMAX)
      RETURN
C
      END
