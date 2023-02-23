C   26/04/84 404261457  MEMBER NAME  DRAWZC   (S)           FORTRAN
C
C-----------------------------------------------------------------------
      SUBROUTINE DRAWZC( IVIEW )
C-----------------------------------------------------------------------
C
C   AUTHOR:   C. BOWDERY    26/04/84 :  DRAW Z CHAMBER
C
C
C     DRAW THE Z CHAMBER FOR THE VIEW IVIEW.
C       IVIEW = 1   : R PHI VIEW
C       IVIEW = 2   : RZ VIEWS
C       IVIEW = 3   : ROLLED OUT VIEW
C
C-----------------------------------------------------------------------
C
      IMPLICIT INTEGER*2 (H)
C
      COMMON / CJTRIG / PI,TWOPI,PIHALF,PI3HALF
#include "czgeo.for"
C
C------------------  C O D E  ------------------------------------------
C
      GO TO ( 1 , 2 , 3 ),IVIEW
C
C                            R PHI VIEW OF THE Z CHAMBER
C
  1     DEFI  = TWOPI / 24.0
        RINRZ = RZCHI / COS(0.5 * DEFI)
        RUTRZ = RZCHA / COS(0.5 * DEFI)
        BIGOF = ATAN2(100.0, RINRZ)
        WEEOF = ATAN2( 20.0, RINRZ)
C
C                            DRAW AS 4 SEPARATE QUADRANTS
C
        CALL BARSEC(    0.,    WEEOF,BIGOF,RINRZ,RUTRZ,0.,0.,6,DEFI,0.)
        CALL BARSEC(0.25*TWOPI,BIGOF,WEEOF,RINRZ,RUTRZ,0.,0.,6,DEFI,0.)
        CALL BARSEC(0.50*TWOPI,WEEOF,BIGOF,RINRZ,RUTRZ,0.,0.,6,DEFI,0.)
        CALL BARSEC(0.75*TWOPI,BIGOF,WEEOF,RINRZ,RUTRZ,0.,0.,6,DEFI,0.)
        RETURN
C
C                            RZ VIEW OF THE Z CHAMBER
C
  2   R1I    = RZCHI + ZCHA
      R2I    = RZCHI + ZCHB
      CALL RECTAN(Z1ZCH,-RZCHA,Z2ZCH,-RZCHI,0)
      CALL RECTAN(Z1ZCH, RZCHI,Z2ZCH, RZCHA,0)
C
C                            DRAW Z CHAMBER WIRES
C
      ZZZ = Z1ZCH - 0.5 * DLZZ
      DO  21  I = 1,16
        ZZZ = ZZZ + DLZZ
        CALL POINTA(ZZZ, R1I)
        CALL POINTA(ZZZ, R2I)
        CALL POINTA(ZZZ,-R1I)
        CALL POINTA(ZZZ,-R2I)
  21  CONTINUE
C
C                            DRAW Z CHAMBER WALLS
C
      ZZZ = Z1ZCH
      DO  22  I = 1,15
        ZZZ = ZZZ + DLZZ
        CALL DRAMOV(ZZZ, RZCHI,ZZZ, RZCHA,0)
        CALL DRAMOV(ZZZ,-RZCHI,ZZZ,-RZCHA,0)
  22  CONTINUE
C
      RETURN
C
C                            ROLLED OUT VIEW OF THE Z CHAMBER
C
C
C                            DRAW CHAMBER BOUNDARIES
C
   3  ADY = 1500.
      X1  =  0.
      X2  =  6. * DLZPHI
      X3  = 12. * DLZPHI
      X4  = 18. * DLZPHI
      X5  = 24. * DLZPHI
      Y1  = ADY
      Y2  = ADY + 16.*DLZZ
      CALL RECTAN(X1+ZCHDLL, Y1, X2-ZCHDL,  Y2, 0)
      CALL RECTAN(X2+ZCHDL,  Y1, X3-ZCHDLL, Y2, 0)
      CALL RECTAN(X3+ZCHDLL, Y1, X4-ZCHDL,  Y2, 0)
      CALL RECTAN(X4+ZCHDL,  Y1, X5-ZCHDLL, Y2, 0)
C
C                            DRAW CELL WALLS
C
      Y0 = Y1 - DLZZ
      DO  31  I  = 1,17
        Y0 = Y0 + DLZZ
        CALL DRAMOV(X1+ZCHDLL, Y0, X2-ZCHDL,  Y0, 0)
        CALL DRAMOV(X2+ZCHDL,  Y0, X3-ZCHDLL, Y0, 0)
        CALL DRAMOV(X3+ZCHDLL, Y0, X4-ZCHDL,  Y0, 0)
        CALL DRAMOV(X4+ZCHDL,  Y0, X5-ZCHDLL, Y0, 0)
 31   CONTINUE
C
C                            DRAW PHI-SECTOR BOUNDARIES
C
      X0 = - DLZPHI
      DO  32  I  = 1,25
        X0 = X0 + DLZPHI
        IF(MOD(I,6) .EQ. 1) GO TO 32
        CALL DRAMOV(X0,Y1,X0,Y2,14)
 32   CONTINUE
C
C                            ADD AXIS LABELLING
C
      SH3 = 0.4*DLZZ
      Y0  = ADY - 0.6*DLZPHI
      DO  33  I = 1,3
        LABT = 103+I
        X0   = (I-1) * (0.5*X5 - 0.4*DLZPHI)
        CALL RUTEXT(LABT,X0,Y0,SH3)
 33   CONTINUE
C
      X0  = -0.8*DLZPHI
      DO  34  I = 1,3
        LABT = 100+I
        Y0   = ADY + (I-1)*7.8*DLZZ
        CALL RUTEXT(LABT,X0,Y0,SH3)
 34   CONTINUE
C
      RETURN
      END
