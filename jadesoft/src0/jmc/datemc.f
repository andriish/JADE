C   17/11/84 712211436  MEMBER NAME  DATEMC   (S)           FORTRAN
      SUBROUTINE DATEMC( HDAT )
C-----------------------------------------------------------
C
C  VERSION OF 18/04/79  LAST MOD 09/07/79    E.ELSEN
C  STORE DATE IN HDAT.
C  SEQUENCE IS SS, MM, HH, DD, MM, YY.
C----------------------------------------------------------
C
      IMPLICIT INTEGER*2 (H)
      REAL*8 X1,X2
      DIMENSION HDAT(6)
      CALL DAY( X1, X2 )
      CALL MVC(  I1,  3, X2, 7, 1 )
      CALL MVC( I10,  3, X2, 6, 1 )
      HDAT(1) =  LAND( I10, 15 ) * 10 + LAND( I1, 15 )
      CALL MVC(  I1,  3, X2, 4, 1 )
      CALL MVC( I10,  3, X2, 3, 1 )
      HDAT(2) =  LAND( I10, 15 ) * 10 + LAND( I1, 15 )
      CALL MVC(  I1,  3, X2, 1, 1 )
      CALL MVC( I10,  3, X2, 0, 1 )
      HDAT(3) =  LAND( I10, 15 ) * 10 + LAND( I1, 15 )
      CALL MVC(  I1,  3, X1, 1, 1 )
      CALL MVC( I10,  3, X1, 0, 1 )
      HDAT(4) =  LAND( I10, 15 ) * 10 + LAND( I1, 15 )
      CALL MVC(  I1,  3, X1, 4, 1 )
      CALL MVC( I10,  3, X1, 3, 1 )
      HDAT(5) =  LAND( I10, 15 ) * 10 + LAND( I1, 15 )
      CALL MVC(  I1,  3, X1, 7, 1 )
      CALL MVC( I10,  3, X1, 6, 1 )
      HDAT(6) =  LAND( I10, 15 ) * 10 + LAND( I1, 15 ) + 1900
      RETURN
      END
