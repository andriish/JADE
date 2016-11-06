C   24/04/85 807251625  MEMBER NAME  SETSCL   (S)           FORTRAN
C
C-----------------------------------------------------------------------
      SUBROUTINE SETSCL(INDEX)
C-----------------------------------------------------------------------
C
C    AUTHOR:   J. OLSSON       ?    :  SET SCALE
C
C       MOD:   J. OLSSON   15/12/83 :
C       MOD:   C. BOWDERY   8/06/84 :  NEW COMMAND NUMBERS
C       MOD:   J. HAGEMANN 19/10/84 :  SET SCALE FOR SPECIAL VERTEX
C                                      CHAMBER VIEW
C       MOD:   C. BOWDERY  24/04/85 :  USE CGRSCL COMMON NOW
C  LAST MOD:   J. HAGEMANN 26/10/87 :  FOR VIEW VRX
C
C
C     FOR THE GIVEN VIEW INDEX, SET VIEW SCALE USING VALUES STORED
C     IN COMMON /CGRSCL/. INDEX = 0 MEANS STANDARD VIEW SCALES.
C
C-----------------------------------------------------------------------
C
      IMPLICIT INTEGER*2 (H)
C
      LOGICAL FLVCDO
C
#include "cgraph.for"
#include "cgrscl.for"
C
      COMMON / CGVCDO / FLVCDO(20)
      COMMON / CHEADR / HEAD(108)
C
C------------------  C O D E  ------------------------------------------
C
      IF( INDEX .LT. 1  .OR.  INDEX .GT. 20 ) GO TO 1
C
        IF( INDEX.NE.17 .OR. HEAD(18).LE.100
     &                  .OR. .NOT. FLVCDO(11) ) GO TO 5
          CALL VTXCRV( INT(HEAD(18)), XRV, YRV, DXR, DYR ) ! PMF 08/11/99: add run argument HEAD(18)
          XMIN = -XRV + XMINST( INDEX )
          XMAX = -XRV + XMAXST( INDEX )
          YMIN = YRV + YMINST( INDEX )
          YMAX = YMIN + XMAX - XMIN
          GO TO 10
   5    XMIN = XMINST( INDEX )
        XMAX = XMAXST( INDEX )
        YMIN = YMINST( INDEX )
        YMAX = YMIN + XMAX - XMIN
        GO TO 10
C
C                            FOR INDEX = 0 (ESPECIALLY), SET STANDARD
C                            VIEW SCALE (AS DEFINED BY THE USER)
C
   1    XMIN = SXIN
        XMAX = SXAX
        YMIN = SYIN
        YMAX = SYAX
C
  10  CALL DWINDO( XMIN, XMAX, YMIN, YMAX )
      RETURN
C
      END
