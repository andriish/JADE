C   20/08/79 106101757  MEMBER NAME  OPUNIT   (JADEGS)      FORTRAN
      SUBROUTINE OPUNIT(JUNIT)
      IMPLICIT INTEGER*2 (H)
C---
C---     REPOSITIONS OUTPUT FILE JUST BEFORE EOF.
C---
#include "cgraph.for"
      NREC=0
      REWIND JUNIT
      WRITE(6,99) JUNIT
   99 FORMAT(' OPUNIT HAS REWOUND THE OUTPUT UNIT, NUMBER',I4)
    1 CONTINUE
      READ(JUNIT,ERR=1000,END=2) NDUMMY
      NREC=NREC+1
      GO TO 1
 1000 CONTINUE
      WRITE(JUSCRN,100)
      WRITE(JUSCRN,101)
      WRITE(JUSCRN,102)
  100 FORMAT(' READ ERROR REPOSITIONING THE OUTPUT FILE.')
  101 FORMAT(' SAVE EDITING RESULTS, STOP SCAN SESSION')
  102 FORMAT(' AND INVESTIGATE OUTPUT FILE IN BATCH.')
      RETURN
    2 CONTINUE
      IF(NREC.LT.1) GO TO 1000
      WRITE(6,98) NREC
   98 FORMAT(' OPUNIT FOUND',I4,' RECORDS ON THE OUTPUT FILE.')
      REWIND JUNIT
      DO 3 IREC=1,NREC
      READ(JUNIT,ERR=1000,END=1000) NDUMMY
    3 CONTINUE
      WRITE(6,97) JUNIT,NREC
   97 FORMAT
     1(' OPUNIT HAS REWOUND UNIT',I4,' AGAIN AND READ',I4,' RECORDS.')
      RETURN
      END
