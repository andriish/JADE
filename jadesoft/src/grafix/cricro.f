C   01/11/84 411011704  MEMBER NAME  CRICRO   (S)           FORTRAN
C
C-----------------------------------------------------------------------
      SUBROUTINE CRICRO(ADX,ADY)
C-----------------------------------------------------------------------
C
C    AUTHOR:   J. OLSSON        ?     :  DRAW POINTS IN ABSOLUTE COOR-
C                                        DINATES
C  LAST MOD:   J. HAGEMANN   09/10/84 :  NOW IN OWN MEMBER (FROM EVDISP)
C
C
C       X1-Y4 ARE ABSOLUT COORDINATES, WITH RESPECT TO POINT 0.,0.,
C       DRAWING IS DONE RELATIVE TO CENTERPOINT ADX,ADY
C
C-----------------------------------------------------------------------
C
      IMPLICIT INTEGER*2 (H)
C
      COMMON /CWORK1/ DMY1(4),X1,Y1,DMY2(2),X2,Y2,ZET,X3,Y3,X4,Y4
C
C-----------------  C O D E  -------------------------------------------
C
C     DRAW CRISS CROSS IN QUADRILATERAL
      X5 = (X1 + X4)*.5 +ADX
      Y5 = (Y1 + Y4)*.5 +ADY
      X6 = (X2 + X3)*.5 +ADX
      Y6 = (Y2 + Y3)*.5 +ADY
      X7 = (X1 + X2)*.5 +ADX
      Y7 = (Y1 + Y2)*.5 +ADY
      CALL MOVEA(X1+ADX,Y1+ADY)
      CALL DRAWA(X3+ADX,Y3+ADY)
      CALL MOVEA(X2+ADX,Y2+ADY)
      CALL DRAWA(X4+ADX,Y4+ADY)
      CALL MOVEA(X5,Y5)
      CALL DRAWA(X7,Y7)
      CALL DRAWA(X6,Y6)
      X7 = (X3 + X4)*.5 +ADX
      Y7 = (Y3 + Y4)*.5 +ADY
      CALL DRAWA(X7,Y7)
      CALL DRAWA(X5,Y5)
      RETURN
      END
