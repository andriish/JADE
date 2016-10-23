C   01/11/84            MEMBER NAME  SETSCL   (S)           FORTRAN
C
C-----------------------------------------------------------------------
      SUBROUTINE SETSCL(INDEX)
C-----------------------------------------------------------------------
C
C    AUTHOR:   J. OLSSON       ?    :  SET SCALE
C
C       MOD:   J. OLSSON   15/12/83 :
C       MOD:   C. BOWDERY   8/06/84 :  NEW COMMAND NUMBERS
C  LAST MOD:   J. HAGEMANN 19/10/84 :  SET SCALE FOR SPECIAL VERTEX
C                                      CHAMBER VIEW
C
C-----------------------------------------------------------------------
C
      IMPLICIT INTEGER*2 (H)
C
#include "cgraph.for"
C
C------------------  C O D E  ------------------------------------------
C
      HNDEX=INDEX
      IF( INDEX .GT. 7 ) HNDEX = INDEX - 4
      GO TO (1,2,3,4,5,6,7,8,9,10,11,12),HNDEX
      IF( INDEX .EQ. 20 ) GO TO 16
      IF( INDEX .GE. 17 .AND. INDEX .LE. 19 ) GO TO 75
      GO TO 20
C                            VIEW RA
1     XMIN = -1400.
      YMIN = -1100.
      GO TO 999
C                            VIEW RB
2     XMIN = -2050.
      XMAX =  1950.
      YMIN = -1550.
      GO TO 999
C                            VIEW RC
3     XMIN = -4900.
      YMIN = -3350.
      GO TO 999
C                            VIEW ZXA,ZYC
    4 XMIN = -1800.
      XMAX =  1500.
      YMIN = -1300.
      GO TO 999
C                            VIEW ZXB,ZYB
    5 XMIN = -2400.
      XMAX =  2000.
      YMIN = -1700.
      GO TO 999
C                            VIEW ZXC,ZYC
    6 XMIN = -5600.
      XMAX =  4400.
      YMIN = -3900.
      GO TO 999
C                            VIEW ZXD,ZYD
    7 XMIN = -5500.
      YMIN = - 3900.
      GO TO 999
C                            VIEW FW
    8 XMIN = -1200.
      XMAX =  1200.
      YMIN = -800.
      GO TO 999
C                            VIEW RU
    9 XMIN = -1610.
      XMAX =  7890.
      YMIN = -25.
      GO TO 999
C                            VIEW CYL
10    XMIN = -1800.
      YMIN = -1300.
      GO TO 999
C                            VIEW FWMU
11    XMIN = -1610.
      XMAX =  7890.
      YMIN = -25.
      GO TO 999
C                            VIEW RZ
12    XMIN = -1250.
      XMAX =  5530.
      YMIN =  0.
      GO TO 999
C                            VIEW VC
16    XMIN = -345.
      XMAX =  345.
      YMIN = -260.
      GO TO 999
C                            VIEWS VRX,VRZX,VRZY
75    XMIN = -8.5
      XMAX =  7.0
      YMIN = -6.0
      GO TO 999
999   IF((HNDEX.LT.4.OR.HNDEX.EQ.7.OR.HNDEX.EQ.10).AND.HNDEX.NE.2)
     $ XMAX = - XMIN
      YMAX = YMIN + XMAX - XMIN
      GO TO 1000
   20 XMIN =SXIN
      XMAX = SXAX
      YMIN = SYIN
      YMAX = SYAX
 1000 CALL DWINDO(XMIN,XMAX,YMIN,YMAX)
      RETURN
C
      END
