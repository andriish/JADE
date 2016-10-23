C   05/11/82 602210950  MEMBER NAME  COPYLB   (JADESR)      FORTRAN
      SUBROUTINE AANAL
      IMPLICIT INTEGER*2 (H)
#include "cdata.for"
      COMMON /WRT/ NWRFL
      DATA IZAEL /0/
C
C  ***********                           ***************
C  ***********    COPY OF BOS DATA SETS  ***************
C  ***********                           ***************
C
C      ----------------    SET DATA  -------------------
C
      DATA NSCIP   /      0   /
      DATA NEND    /    200   /
C
      DATA NRUNSC  /      0   /
      DATA NRUNED  / 100000   /
C      -------------------------------------------------
      NWRFL=0
C
      IZAEL=IZAEL+1
      IRUN=HDHEAD(10)
C
      IF (IZAEL.GT.NSCIP .AND. IRUN.GT.NRUNSC) NWRFL=1
      IF (IZAEL.GT.NEND  .OR.  IRUN.GT.NRUNED) NWRFL=0
C
      IF (NWRFL.EQ.0) GOTO 30
      XRUN=IRUN
      CALL HIST(10,XRUN)
C
   30 RETURN
      END
