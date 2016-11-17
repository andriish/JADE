C   01/11/84 512201616  MEMBER NAME  CROAX    (S)           FORTRAN
C
C-----------------------------------------------------------------------
      SUBROUTINE CROAX(INDEX)
C-----------------------------------------------------------------------
C
C    AUTHOR:   J. OLSSON       ?    :  DRAWS AN AXIS CROSS
C
C       MOD:   J. OLSSON   17/12/83 :
C       MOD:   C. BOWDERY   8/06/84 :  NEW COMMAND NUMBERS
C       MOD:   J. HAGEMANN 19/10/84 :  NEW INDEX=20
C  LAST MOD:   C. BOWDERY  20/12/85 :  DO NOTHING IF OPTION 48 IS ON
C
C
C     DRAWS AN AXIS CROSS WITH SYMBOLS WRITTEN, AT POSITION X1,Y1.
C     INDEX REFERS TO VIEW INDEX IN JADE GRAPHICS DISPLAY SYSTEM.
C
C-----------------------------------------------------------------------
C
      IMPLICIT INTEGER*2 (H)
C
      LOGICAL DSPDTM
      LOGICAL FL18,FL22,FL24
C
      COMMON / CGRAP2 / BCMD,DSPDTM(30),ISTVW,JTVW
      COMMON / CPROJ /XMINR,XMAXR,YMINR,YMAXR,IPRJC,FL18,FL22,FL24,KPROJ
#include "cgraph.for"
C
      DIMENSION  ISGN(4,3),IPER(4,3),HTX(4)
C
      character*2 HTX
      DATA HTX /'X ','Y ','Z ','  '/
      DATA ISGN / 1,1,1,1,1,-1,1,-1,-1,1,1,1 /
      DATA IPER / 2,1,2,2,3,2,1,3,1,3,3,1 /
      DATA FSCA1 /25./, F1,F2,F3 /3.5,1.,2.5/,FX,FY /.08,.05/,FFX /.05/
      DATA FSCA2 /15./, FX1,FY1,FY2 /.735,.08,.66/
C
C------------------  C O D E  ------------------------------------------
C
C                            IF OPTION 48 IS ON, DRAW NO AXES
C

       write(*,*)' CROAX(INDEX) ',  INDEX, DSPDTM(18) 
      IF( DSPDTM(18) ) RETURN
C
      IF( INDEX .LT. 0  .OR.  INDEX .EQ. 13  .OR.  INDEX .EQ. 15) RETURN
      IF( INDEX .EQ. 16 ) RETURN
      IF( INDEX .NE. 12 ) GO TO 143
C
C                            FORWARD DETECTOR DISPLAY
C
      XADD = XMIN + FFX*(XMAX-XMIN)
      GO TO 163
143   CONTINUE
      IF( .NOT. FL18) GO TO 160
      XADD = XMIN + FX1*(XMAX-XMIN)
      IF( KPROJ .NE. 1 ) GO TO 161
      YADD = YMIN + FY1*(YMAX-YMIN)
      GO TO 162
161   YADD = YMIN + FY2*(YMAX-YMIN)
162   FSCA = (XMAX-XMIN)*FSCA2/4096.
      GO TO 170
160   XADD = XMIN + FX*(XMAX-XMIN)
163   YADD = YMIN + FY*(YMAX-YMIN)
      FSCA = (XMAX-XMIN)*FSCA1/4096.
170   CALL CHRSIZ(4)
      IF( INDEX .EQ. 20 ) GO TO 50
      IF( INDEX .GT. 3  .AND.  INDEX .NE. 14 .AND.INDEX.NE.17) GO TO 100
C      line? 00005900
C
C                            RFI-VIEWS
C
 50   IX = 1
      GO TO 300
100   IF( INDEX .GT. 7  .AND.  INDEX .NE. 18 ) GO TO 200
C
C                            X-Z VIEWS
C
      IX = 2
      GO TO 300
C
C                            Y-Z VIEWS
C
200   IX = 3
      IF( INDEX .EQ. 12 ) IX = 1
300   ALFA = 135.
      DO 1  I = 1,3
      CALL MOVEA(XADD,YADD)
      ALFA = ALFA - 45.
      CALL RROTAT(ALFA)
      IS = ISGN(IX,I)
      CALL DRAWR(IS*FSCA*F1,0.)
      CALL MOVER(0,-FSCA*F2)
      CALL DRAWR(0,2.*FSCA*F2)
      CALL DRAWR(IS*FSCA*F3,-FSCA*F2)
      CALL DRAWR(-IS*FSCA*F3,-FSCA*F2)
      CALL MOVER(IS*(F3+1.4)*FSCA,FSCA*F2*.5)
      IS = IPER(IX,I)
1     CALL EOUTST(2,HTX(IS))
      write(*,*)'(2,HTX(IS))', HTX(IS)
      RETURN
C
      END
