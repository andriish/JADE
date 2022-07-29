C   01/11/84            MEMBER NAME  PBGRZ    (S)           FORTRAN
C
C-----------------------------------------------------------------------
      SUBROUTINE PBGRZ( INDEX, SH1, SH2, SH3 )
C-----------------------------------------------------------------------
C
C    AUTHOR:   J. OLSSON        ?     :  LG AMPLITUDES   (R-Z-VIEWS)
C
C  LAST MOD:   J. HAGEMANN   10/10/84 :  NOW OWN MEMBER (FROM EVDISP)
C
C-----------------------------------------------------------------------
C
      IMPLICIT INTEGER*2 (H)
C
#include "cdata.for"
#include "cgeo1.for"
#include "cgraph.for"
C
      COMMON /CWORK1/ R,FI,XA,COSFI,X1,Y1,YA,SINFI,X2,Y2,ZET,X3,Y3,X4,Y4
CAV      00001900
     +               ,IMW(200)
C
      COMMON /CJTRIG/ PI,TWOPI,PIHALF,PI3HALF
C
C-----------------  C O D E  -------------------------------------------
C
C     LEAD GLASS AMPLITUDES IN RZ VIEWS
C
      CALL PBGSUM(INDEX)
      call setcol('ECAL')     ! PMF 23/11/99: set colour
C DISPLAY ENDCAP SUMS
      DO 5020 IKJ=1,2
      X=ZENDMI-DEPEND+10.
      IF(IKJ.EQ.2) X=ZENDPL+10.
      Y0=-6*BLXY+10.
      DO 5020 KJI=1,12
      IRETRV=84+12*(IKJ-1)+KJI
      IESUM=IMW(IRETRV)
      IF(IESUM.LT.1) GO TO 5020
      Y=Y0+(KJI-1)*BLXY
      CALL DNUM(IESUM,X,Y,SH3,0.)
 5020 CONTINUE
C---
C---     NOW DO BARREL.
C---
      DO 34 ICC = 109,172
      IESUM = IMW(ICC)
      IF(IESUM.LT.0) GO TO 34
      ICCC = ICC - 108
      IF(ICCC.GT.32) ICCC = ICCC - 32
      X1 = ZLGMI + (ICCC-1)*BLZ
      Y1 = RLG
      X0 = X1 + 5.*SH2
      Y0 = Y1 + SH1
      FII = PI/2.
      IF(ICC.LT.141) GO TO 22
      Y1 = - RLG
      Y0 = Y1 - SH1
      X0 = X1 + SH2
      FII = 3.*PI/2.
   22 CALL DNUM(IESUM,X0,Y0,SH3,FII)
   34 CONTINUE
      call setcol(' ')     ! PMF 23/11/99: reset colour
      RETURN
      END
