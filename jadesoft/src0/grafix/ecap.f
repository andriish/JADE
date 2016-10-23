C   17/04/85 504171418  MEMBER NAME  ECAP     (S)           FORTRAN
C
C-----------------------------------------------------------------------
      SUBROUTINE ECAP( ADX, ADY )
C-----------------------------------------------------------------------
C
C
C    AUTHOR:   J. OLSSON       ?    :  DRAW LG ENDCAP DET IN RU VIEW
C
C  LAST MOD:   C. BOWDERY  17/04/85 :  COSMETIC CHANGES ONLY
C
C
C     DRAW LEADGLASS ENDCAP HARDWARE IN ROLLED OUT VIEW. ADX,ADY GIVES
C     CENTRE POINT.
C
C-----------------------------------------------------------------------
C
      IMPLICIT INTEGER*2 (H)
C
      COMMON / CWORK1 / HWORK(40)
C
#include "cgeo1.for"
#include "cgraph.for"
C
      DIMENSION HMES(36)
C
      DATA HMES/'-','Z',' ',' ',' ',' '
     +         ,'0',' ',' ',' ',' ',' '
     +         ,'+','Z',' ',' ',' ',' '
     +         ,'F','I',' ','0',' ',' '
     +         ,'F','I',' ','P','I',' '
     +         ,'F','I',' ','2','P','I'/
C
C------------------  C O D E  ------------------------------------------
C
      LLL = 0
      IF( DSPDTL(15) ) LLL = 14
      Y1  = 0.0
      Y3  = - BLXY
C
      DO  12  I = 1,6
        Y1 = Y1 + BLXY
        Y3 = Y3 + BLXY
        Y2 = Y1
        Y4 = Y3
        IF( I .NE. 1 ) GO TO 34
        X1 = 2.0*BLXY
        X2 = X1 + 4.0*BLXY + 60.0
        X3 = X1 + 60.0
        X4 = X2
        GO TO 1005
  34    IF( I .NE. 2 ) GO TO 35
        X1 = X1 - BLXY
        X2 = X2 - 60.0
        X3 = - BLXY
        X4 =   BLXY
        Y3 = 2.0*BLXY + 60.0
        Y4 = Y3
        GO TO 1005
  35    IF( ( I .NE. 3 )  .AND.  ( I .NE. 5 ) ) GO TO 36
        X2 = X2 - BLXY
  36    IF( I .EQ. 6 ) X2 = X2 - 2.0*BLXY
C
C                            LEAD GLASS ENDCAPS
C
1005    CALL DRACAP(ADX+X1,ADY+Y1,ADX+X2,ADY+Y2,ADX-X1,ADY-Y1,ADX-X2,
     +              ADY-Y2,LLL)
        CALL DRACAP(ADX+Y1,ADY+X1,ADX+Y2,ADY+X2,ADX-Y1,ADY-X1,ADX-Y2,
     +              ADY-X2,LLL)
        CALL DRAMOV(ADX+X3,ADY+Y3,ADX+X4,ADY+Y4,LLL)
        CALL DRAMOV(ADX+Y3,ADY+X3,ADX+Y4,ADY+X4,LLL)
        IF( I .NE. 1 ) GO TO 32
        CALL DRAMOV(ADX-X3,ADY+Y3,ADX-X4,ADY+Y4,LLL)
        CALL DRAMOV(ADX+Y3,ADY-X3,ADX+Y4,ADY-X4,LLL)
        GO TO 12
   32   CALL DRAMOV(ADX-Y3,ADY+X3,ADX-Y4,ADY+X4,LLL)
        CALL DRAMOV(ADX+X3,ADY-Y3,ADX+X4,ADY-Y4,LLL)
   12 CONTINUE
      RETURN
C-----------------------------------------------------------------------
      ENTRY RUTEXT(LABT,X1,Y1,SH3)
C-----------------------------------------------------------------------
C
C                            WRITE TEXT ON ROLLED OUT VIEW OF LEAD GLASS
C
      IF( ( LABT .LT. 101 )  .OR.  ( LABT .GT. 106 ) ) RETURN
C
      DO  51  IIK = 1,6
        HWORK(IIK) = HMES(6*(LABT-101)+IIK)
 51   CONTINUE
C
      ICNT = 6
      CALL HEMSYM(X1,Y1,SH3,HWORK,ICNT,0.)
      RETURN
      END
