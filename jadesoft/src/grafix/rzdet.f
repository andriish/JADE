C   12/04/84 404121125  MEMBER NAME  RZDET    (JADEGS)      FORTRAN
C
C-----------------------------------------------------------------------
      SUBROUTINE RZDET
C-----------------------------------------------------------------------
C
C   AUTHOR: S. CARTWRIGHT 11/04/84 :  RZ CHAMBERS, ROLLED OUT VIEW
C
C
C-----------------------------------------------------------------------
C
      IMPLICIT INTEGER*2 (H)
C
      COMMON / CZGEO  / RZCHI,RZCHA,NZRPSI,NZZ,Z1ZCH,Z2ZCH,ZCHA,ZCHB,
     +                  ZCHSS,ZCHDL,ZCHDLL,DLZZ,DLZPHI,DLZW1,DLZW2
C
C------------------- C O D E -------------------------------------------
C
C                            DRAW CHAMBER BOUNDARIES
C
      ADY = 1500.
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
      DO  10  I  = 1,17
        Y0 = Y0 + DLZZ
        CALL DRAMOV(X1+ZCHDLL, Y0, X2-ZCHDL,  Y0, 0)
        CALL DRAMOV(X2+ZCHDL,  Y0, X3-ZCHDLL, Y0, 0)
        CALL DRAMOV(X3+ZCHDLL, Y0, X4-ZCHDL,  Y0, 0)
        CALL DRAMOV(X4+ZCHDL,  Y0, X5-ZCHDLL, Y0, 0)
 10   CONTINUE
C
C                            DRAW PHI-SECTOR BOUNDARIES
C
      X0 = - DLZPHI
      DO  20  I  = 1,25
        X0 = X0 + DLZPHI
        IF(MOD(I,6) .EQ. 1) GO TO 20
        CALL DRAMOV(X0,Y1,X0,Y2,14)
 20   CONTINUE
C
C                            ADD AXIS LABELLING
C
      SH3 = 0.4*DLZZ
      Y0  = ADY - 0.6*DLZPHI
      DO  30  I = 1,3
        LABT = 103+I
        X0   = (I-1) * (0.5*X5 - 0.4*DLZPHI)
        CALL RUTEXT(LABT,X0,Y0,SH3)
 30   CONTINUE
C
      X0  = -0.8*DLZPHI
      DO  40  I = 1,3
        LABT = 100+I
        Y0   = ADY + (I-1)*7.8*DLZZ
        CALL RUTEXT(LABT,X0,Y0,SH3)
 40   CONTINUE
C
      RETURN
      END
