C   09/04/84 404101839  MEMBER NAME  DRACAP   (S)           FORTRAN
C
C-----------------------------------------------------------------------
      SUBROUTINE DRACAP(X1,Y1,X2,Y2,X3,Y3,X4,Y4,L)
C-----------------------------------------------------------------------
C
C                            DRAW CROSS IN A QUADRILATERAL
C
      CALL DRAMOV(X1,Y1,X2,Y2,L)
      CALL DRAMOV(X3,Y3,X4,Y4,L)
      CALL DRAMOV(X3,Y1,X4,Y2,L)
      CALL DRAMOV(X1,Y3,X2,Y4,L)
C
      RETURN
      END
