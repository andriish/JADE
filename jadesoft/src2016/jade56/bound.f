C   25/06/78            MEMBER NAME  BOUND    (LGSOURCE)    FORTRAN
      FUNCTION BOUND(PA0)
C
C     A. SATO  28-10-77
C
      DIMENSION PA0(3,3)
CAV To init the variable
      BOUND=0.0
      X=PA0(1,2)
      IF(X.GE.0.)GOTO1
      BOUND=-1.
      RETURN
1     BOUND=B-X
      RETURN
C
      ENTRY BOUNDI(BIN)
      B=BIN
      RETURN
      END
