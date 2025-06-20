C   10/05/79 C9051401   MEMBER NAME  BOUNDJ   (LGSOURCE)    FORTRAN
      FUNCTION BOUND(PA0)
C
C     A. SATO  28-10-77
C     LAST MODIFICATION  08-05-79  19:00  BY Y.YANAGISAWA
C     LAST MODIFICATION  10-05-79  14:00  BY S.YAMADA
C
      COMMON /CLGDMS/ X0,RADIUS(6),RADSX0(6),THX0(4),
     1                ZEND(2),ZENDX0(2),ZWID(2),ZGAP(2),PHWID(2),
     2                ZECAP(4),ZECAPX(4),THECPX(2),
     3                ECXLST(24), ECYLST(24)
C
      DIMENSION PA0(3,3)
C
CAV To init the variable
      BOUND=0.0
      X=PA0(1,2)
      IF(X.GE.0.) GO TO 1
      BOUND=-1.
      RETURN
    1 BOUND=B-X
      IF(BOUND.LT.0.) RETURN
C
C---- CHECK SIDE BND(DISTINGUISHED BY THE BOUNDARY, SEE LGBLCK & LGSTRC)
      IF(B-65.) 3,3,2
C---- END CAP
    2 CALL BONDEC( PA0(2,2), PA0(3,2), IN)
      IF( IN ) 4,4,100
    4 BOUND = -2.
  100 RETURN
C
C---- BARREL
    3 IF(PA0(3,2).GE.ZENDX0(1).AND.PA0(3,2).LE.ZENDX0(2)) RETURN
      BOUND = -3.
      RETURN
C
C*******************************************************
C
      ENTRY BOUNDI(BIN)
      B=BIN
      RETURN
      END
