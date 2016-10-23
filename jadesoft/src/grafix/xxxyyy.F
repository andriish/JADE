C   20/12/85 512202018  MEMBER NAME  XXXYYY   (S)           FORTRAN
C
C-----------------------------------------------------------------------
      SUBROUTINE XXXYYY(XXX,YYY,SSS,INTOX)
C-----------------------------------------------------------------------
C
C     COMPUTE POSITION ON SCREEN, GIVEN INPUT IN DETECTOR COORDINATES
C
C-----------------------------------------------------------------------
C
      IMPLICIT  INTEGER*2 (H)
C
#include "cgraph.for"
C
C------------------  C O D E  ------------------------------------------
C
      IF(INTOX.EQ.1) GO TO 1
      CALL CHRSIZ(4)
      CALL CSIZE(IHO,IVE)
      SSS = IVE*(YMAX-YMIN)/6240.
      YYY = YYY - 15.*SSS
      SSS = IVE*.55
1     CALL MOVEA(XXX,YYY)
      CALL SEELOC(IHO,IVE)
      XXX = IHO
      YYY = IVE
      RETURN
      END
