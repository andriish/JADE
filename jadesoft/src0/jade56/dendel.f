C   25/06/78 705271434  MEMBER NAME  DENDEL   (S)           FORTRAN
      FUNCTION DENDEL(X)
C FUNCTION DENDEL CALCULATES DENSITY EFFECT CORRECTION.
C
      COMMON/DFPAR/CI,AI,RMI,X0,X1
      XT=X1-X
      IF(XT)2,1,1
 2    DENDEL=4.606*X+CI
      RETURN
 1    IF(X-X0) 3,3,4
 4    DENDEL=4.606*X+CI+AI*XT**RMI
      RETURN
 3    DENDEL=0.
      RETURN
      END
