C   10/04/84            MEMBER NAME  FWNEW    (JADEGS)      FORTRAN
      SUBROUTINE FWNEW
      IMPLICIT INTEGER*2 (H)
#include "cgeo2.for"
#include "cgeo3.for"
C
C  DRAW SIDEVIEW OF THE TAGGING APPARATUS IN 1981-82 VERSION
C
      DO 1 JZ=1,2
      IZ=-3+2*JZ
      Z0=ZMINM2
      IF(JZ.EQ.2) Z0=ZPLUM2
      Z1=Z0+IZ*BLDPFW
      X0=-3.25*FENDC
      X1=-X0
      CALL RECTAN(Z0,X0,Z1,X1,0)
      X=-3.*FENDC
      DO 1 JX=1,7
      CALL MOVEA(Z0,X)
      CALL DRAWA(Z1,X)
      X=X+FENDC
    1 CONTINUE
      RETURN
      END
