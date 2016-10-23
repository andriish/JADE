C   12/03/84 503161539  MEMBER NAME  TAGFIT   (S)           FORTRAN
C
      SUBROUTINE TAGFIT(J,XFIT,YFIT,IDEBUG,*)
C
C TAGFIT PERFORMS THE FIT TO FIND THE DISTANCE OF THE CENTRE OF THE
C SHOWER FROM A BOUNDARY BETWEEN TWO BLOCKS BY COMPARING THE RATIO
C OF THE ENERGY IN EACH
C
C INPUT  : J       -  POINTER TO WHICH BLOCK IS TO BE COMPARED
C                     TO LARGEST BLOCK IN CLUSTER - CLUS(1)
C          IDEBUG  -  = 1  FOR DEBUG PRINT-OUT
C
C OUTPUT : XFIT    -  RESULT OF FIT IN  X - DIRECTION
C          YFIT    -  RESULT OF FIT IN  Y - DIRECTION
C
C RETURN 1 ERROR CONDITION DETECTED
C
C
#include "cwktag.for"
C
C
#include "comtag.for"
C
C
C
C
C---------------  C O D E  ---------------------------------------------
C
C
      XFIT = 0.0
      YFIT = 0.0
      SIGMA = 10.0
C
      E1 = CLUS(1,2)
      ADDRES = CLUS(1,1)
      X1 = XMAP(ADDRES)
      Y1 = YMAP(ADDRES)
      E2 = CLUS(J,2)
      IF ( (E2 + E1) .NE. 0 ) GOTO 10
C
      WRITE(6,60)
 60   FORMAT('  ERROR DETECTED IN TAGFIT E2 + E1 IS ZERO ')
      RETURN  1
C
 10   ADDRES = CLUS(J,1)
      X2 = XMAP(ADDRES)
      Y2 = YMAP(ADDRES)
      RATIO = E2/(E2 + E1)
C
C THIS IS THE CRITICAL FORMULA
C
      A = RATIO/0.5
      R = LOG(A ) /( - 0.079)
C
C WORK OUT THE X AND Y CORRECTIONS REQUIRED
C
      YDIFF = Y2 - Y1
      XDIFF = X2 - X1
CCC   YSIGN = MSIGN(YDIFF)
CCC   XSIGN = MSIGN(XDIFF)
      YSIGN =  SIGN(1.0,YDIFF)
      XSIGN =  SIGN(1.0,XDIFF)
      THETA = 1.5708 * YSIGN
      IF ( X2 .NE. X1 ) THETA = ATAN((Y2 - Y1 ) /(X2 - X1) )
      XDIFF = X2 - X1
      YDIFF = Y2 - Y1
      ARG = YDIFF ** 2 + XDIFF ** 2
      AL = (SQRT(ARG) ) /2
      XFIT = ((AL - R) * COS(THETA) )
      YFIT = ((AL - R) * SIN(THETA) )
      XFIT = XSIGN * ABS(XFIT)
      YFIT = YSIGN * ABS(YFIT)
C
C-------------------------------------------- R E T U R N --------------
C
      IF ( IDEBUG .NE. 1 ) RETURN
C
C
C
C-------------------------------------------- D E B U G ----------------
C
      WRITE(6,601) E1,E2
      WRITE(6,603) XFIT,YFIT,XDIFF,YDIFF,THETA
      WRITE(6,604) AL,R,RATIO,E1,E2
  601  FORMAT('  E1 E2 ',2X,F10.2,2X,F10.2)
  603  FORMAT(' XFIT,YFIT,YDIF,XDIF,THET,AL,R,RAT,E1,E2',5(2X,F12.2))
  604  FORMAT('  ',5(2X,F12.2) )
      RETURN
CCC   END
CCC   FUNCTION MSIGN(X)
CCC   MSIGN = - 1
CCC   IF ( X .GE. 0 ) MSIGN = 1
CCC   RETURN
      END
