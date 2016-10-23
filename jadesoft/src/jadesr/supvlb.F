C   05/11/82 501081718  MEMBER NAME  SUPVLB   (JADESR)      FORTRAN
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
C-    DATA NSCIP   /      0   /
C-    DATA NEND    / 100000   /
C
C-    DATA NRUNSC  /      0   /
C-    DATA NRUNED  / 100000   /
C      -------------------------------------------------
      NWRFL=0
C
      IZAEL=IZAEL+1
      IRUN=HDHEAD(10)
C---
      IEVT=HDHEAD(11)
C---  WRITE (30) IRUN, IEVT
      IF (IZAEL.EQ.1) READ (30,END=30) IR , IE
      IF (IRUN.EQ.IR .AND. IEVT.EQ.IE) GOTO 50
   51 CONTINUE
C---
C-    IF (IZAEL.GT.NSCIP .AND. IRUN.GT.NRUNSC) NWRFL=1
C-    IF (IZAEL.GT.NEND  .OR.  IRUN.GT.NRUNED) NWRFL=0
C
      IF (NWRFL.EQ.0) GOTO 30
      XRUN=IRUN
      CALL HIST(10,XRUN)
C
   30 RETURN
C
   50 NWRFL=1
      READ (30,END=51) IR , IE
      GOTO 51
C
      END
