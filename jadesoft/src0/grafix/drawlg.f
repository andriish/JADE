C   26/04/84 406081534  MEMBER NAME  DRAWLG   (S)           FORTRAN
C
C-----------------------------------------------------------------------
      SUBROUTINE DRAWLG( IVIEW )
C-----------------------------------------------------------------------
C
C    AUTHOR:   C. BOWDERY    26/04/84 :  DRAW LEAD GLASS SHOWER COUNTERS
C
C  LAST MOD:   C. BOWDERY     8/06/84 :  NEW COMMAND NUMBERS
C
C     DRAW THE LG SYSTEM FOR THE VIEW IVIEW.
C       IVIEW = 1   : NOT USED
C       IVIEW = 2   : RZ VIEWS
C       IVIEW = 3   : ROLLED OUT VIEW
C       IVIEW = 4   : PERSPECTIVE VIEW OF LG
C
C-----------------------------------------------------------------------
C
      IMPLICIT INTEGER*2 (H)
C
      COMMON / CJTRIG / PI,TWOPI
      COMMON / CHEADR / HEAD(108)
C
#include "cgeo1.for"
#include "cgeo2.for"
#include "cgraph.for"
C
      DIMENSION AXD(2)
C
C------------------  C O D E  ------------------------------------------
C
      GO TO ( 1 , 2 , 3 , 4 ) , IVIEW
C
C                            R PHI VIEW DRAWN BY JADISP
C
   1  RETURN
C
C                            Z VIEWS
C
C                            DRAW BARREL BLOCKS
C
   2  CALL DRAMOV(ZLGMI, RLG,  ZLGPL, RLG,  0)
      CALL DRAMOV(ZLGMI,-RLG,  ZLGPL,-RLG,  0)
      CALL DRAMOV(ZLGMI, OUTR2,ZLGPL, OUTR2,0)
      CALL DRAMOV(ZLGMI,-OUTR2,ZLGPL,-OUTR2,0)
      PBW = ZLGMI - BLZ
      DO  21  I = 1,33
        PBW = PBW + BLZ
        CALL DRAMOV(PBW, RLG,PBW, OUTR2,0)
        CALL DRAMOV(PBW,-RLG,PBW,-OUTR2,0)
  21  CONTINUE
C
C                            DRAW ENDCAP BLOCKS
C
      YL1   = -6.*BLXY
      HSIDE =  0
   29 CONTINUE
      XL1   = ZENDMI-DEPEND*HSIDE
      CALL DRAMOV(XL1,YL1,XL1,-YL1,0)
      XL1   = ZENDPL+DEPEND*HSIDE
      CALL DRAMOV(XL1,YL1,XL1,-YL1,0)
      HSIDE = HSIDE+1
      IF( HSIDE .LT. 2) GO TO 29
      DO  23  I = 1,13
        YL1 = (I-7)*BLXY
        CALL DRAMOV(ZENDMI,YL1,ZENDMI-DEPEND,YL1,0)
        CALL DRAMOV(ZENDPL,YL1,ZENDPL+DEPEND,YL1,0)
  23  CONTINUE
C
      RETURN
C
C                            DRAW ROLLED OUT VIEW OF BARREL AND ENDCAPS
C
C-------------- ENDCAPS ---------------------
C
   3  ADDEND = 84.0 * BLFI
      AXD(1) = ADDEND * 0.13
      AXD(2) = ADDEND * 0.87
      ADY    = 900.
      DO  31  J = 1,2
        ADX = AXD(J)
        CALL ECAP(ADX,ADY)
  31  CONTINUE
C
C                           WRITE TEXT ON ENDCAPS
C
      SH3 = 0.6*BLXY
      Y1  = ADY+BLXY
      DO  33  I = 1,2
        LABT = 101 + (I-1)*2
        X1   = AXD(I) - 0.5*BLXY
        CALL RUTEXT(LABT,X1,Y1,SH3)
  33  CONTINUE
C
      Y1   =  ADY - 0.3*BLZ
      SH3  =  0.6*BLZ
      FACT = -2.2
      DO  34  I = 1,2
        LABT = 103 + I
        IF( I .EQ. 2 ) FACT = 0.5
        DO  35  J = 1,2
          X1 = AXD(J) + FACT*BLXY
          CALL RUTEXT(LABT,X1,Y1,SH3)
  35    CONTINUE
  34  CONTINUE
C
C-------------- FORWARD DETECTOR LG SYSTEM --
C
      AXD(1) = ADDEND*.5 - 1100.
      AXD(2) = AXD(1)    + 2200.
      FC     = 1.
      IF( HEAD(18) .LT. 13000 ) GO TO 36
      AXD(1) = ADDEND*.5 -  750.
      AXD(2) = AXD(1)    + 1500.
      FC     = 2.
C
C FOR 1983-84 VERSION, MAGNIFY TAGGING APPARATUS FACTOR 2 IN RU VIEW
C
  36  LL     = 0
      IF( DSPDTL(15) ) LL = 14
      DO  37  J = 1,2
        ADX = AXD(J)
        CALL FWCAP(ADX,ADY,LL,FC,0)
  37  CONTINUE
C
C-------------- LG CYLINDER -----------------
C
      ADY = 3.*ADY + 100.
      Y1  = ADY - BLZ
      X1  = 0.
      X2  = X1 + ADDEND
      DO  38  I = 1,33
        Y1 = Y1 + BLZ
        CALL DRAMOV(X1,Y1,X2,Y1,LL)
  38  CONTINUE
      X1  = -BLFI
      Y1  = ADY
      Y2  = Y1 + 32.*BLZ
      DO  39  I = 1,85
        X1 = X1 + BLFI
        CALL DRAMOV(X1,Y1,X1,Y2,LL)
  39  CONTINUE
C
C                 WRITE TEXT ON CYLINDER MAP
C
      SH3 = 0.6 * BLZ
      Y1  = ADY - BLXY
      DO  301  I = 1,3
        LABT = 103+I
        X1   = (I-1) * (ADDEND*.5 - 1.5*BLFI)
        CALL RUTEXT(LABT,X1,Y1,SH3)
 301  CONTINUE
      X1  = -1.8 * BLFI
      DO  302  I = 1,3
        LABT = 100 + I
        Y1   = ADY + (I-1)*15.7*BLZ
        CALL RUTEXT(LABT,X1,Y1,SH3)
 302  CONTINUE
C
      RETURN
C
C                            PERSPECTIVE VIEW OF LG
C
   4  DEFIX = TWOPI / 84.0
      ZDEEP = 5800.0
      ZETMX = ZDEEP + ZLGPL
      ZETMI = ZDEEP + ZLGMI
      NECC  = ACMD
C
C                            LSTCMD = 38   IS  THE  DET COMMAND
C
      IF( LSTCMD .EQ. 38  .AND.  NECC .NE. 0 ) GO TO 45
      LLX   = 0
      IF( DSPDTL(15) ) LLX = 14
      R     = ZETMI * RLG/ZETMX
      FISX  = -DEFIX
C
C                            DRAW ROW VECTORS
C
      DO  41  IIJ = 1,84
        FISX = FISX + DEFIX
        CSFX = COS(FISX)
        SNFX = SIN(FISX)
        CALL DRAMOV(RLG*CSFX,RLG*SNFX,R*CSFX,R*SNFX,LLX)
 41   CONTINUE
      Z = ZETMX + BLZ
C
C                            DRAW POLYGONS
C
      DO  42  IIJ = 1,33
        Z = Z - BLZ
        R = Z * RLG/ZETMX
        CALL PLYGON(84,R,0.,0.,LLX)
  42  CONTINUE
      RETURN
C
C-------------- ENDCAPS ---------------------
C
  45  AXD(1) = 0.
      AXD(2) = 0.
      ADY    = 0.
      DO  47  J = 1,2
        IF( NECC .NE. J  .AND.  NECC .NE. 3 ) GO TO 47
        FACTOR = 1.
        IF( J .EQ. 2 ) FACTOR = ZETMI/ZETMX
        ADX    = AXD(J)
        CALL ECAPCY(ADX,ADY,FACTOR)
  47  CONTINUE
C
      RETURN
      END
