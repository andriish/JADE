C   17/04/85 504171440  MEMBER NAME  ECAPCY   (S)           FORTRAN
C
C-----------------------------------------------------------------------
      SUBROUTINE ECAPCY( ADX, ADY, FC )
C-----------------------------------------------------------------------
C
C    AUTHOR:   J. OLSSON       ?    :  DRAW LG ENDCAP DET IN RFI VIEWS
C
C  LAST MOD:   C. BOWDERY  17/04/85 :  COSMETIC CHANGES ONLY
C
C     DRAW LEAD GLASS ENDCAP HARDWARE IN RFI VIEWS
C     FC IS A MAGNIFICATION FACTOR FOR THE USE IN CYLINDER VIEW
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
C------------------  C O D E  ------------------------------------------
C
      LLL = 14
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
 1005   CALL DRACAP((ADX+X1)*FC,(ADY+Y1)*FC,(ADX+X2)*FC,(ADY+Y2)*FC,
     +              (ADX-X1)*FC,(ADY-Y1)*FC,(ADX-X2)*FC,(ADY-Y2)*FC,LLL)
        CALL DRACAP((ADX+Y1)*FC,(ADY+X1)*FC,(ADX+Y2)*FC,(ADY+X2)*FC,
     +              (ADX-Y1)*FC,(ADY-X1)*FC,(ADX-Y2)*FC,(ADY-X2)*FC,LLL)
        CALL DRAMOV((ADX+X3)*FC,(ADY+Y3)*FC,(ADX+X4)*FC,(ADY+Y4)*FC,LLL)
        CALL DRAMOV((ADX+Y3)*FC,(ADY+X3)*FC,(ADX+Y4)*FC,(ADY+X4)*FC,LLL)
        IF( I .NE. 1 ) GO TO 32
        CALL DRAMOV((ADX-X3)*FC,(ADY+Y3)*FC,(ADX-X4)*FC,(ADY+Y4)*FC,LLL)
        CALL DRAMOV((ADX+Y3)*FC,(ADY-X3)*FC,(ADX+Y4)*FC,(ADY-X4)*FC,LLL)
        GO TO 12
  32    CALL DRAMOV((ADX-Y3)*FC,(ADY+X3)*FC,(ADX-Y4)*FC,(ADY+X4)*FC,LLL)
        CALL DRAMOV((ADX+X3)*FC,(ADY-Y3)*FC,(ADX+X4)*FC,(ADY-Y4)*FC,LLL)
   12 CONTINUE
C
      RETURN
      END
