C   02/06/80 104011142  MEMBER NAME  MUR2DS   (JADEGS)      FORTRAN
      SUBROUTINE MUR2DS
      IMPLICIT INTEGER*2 (H)
#include "cgraph.for"
      CALL RSDISP(LASTVW)
      IF(LASTVW.EQ.13) CALL YAMADA(0)
      IF(DSPDTL(17).AND.(LASTVW.LT.4.OR.LASTVW.EQ.14))
     $ CALL PROJEC(LASTVW)
      RETURN
      END
