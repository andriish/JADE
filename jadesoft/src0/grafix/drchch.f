C   14/12/78 C9061201   MEMBER NAME  DRCHCH   (JADEGS)      FORTRAN
      SUBROUTINE DRCHCH
C---
C---     SUBROUTINE TO ALLOW CHANGING CONTENTS ON COMMON "CJDRCH"
C---                                   L.H. O'NEILL
C---                                   THRUSDAY, DECEMBER 14, 1978
C---
      IMPLICIT INTEGER*2 (H)
#include "cgraph.for"
      DIMENSION DRCH(37),IDRCH(37)
CMACRO CJDRCH
C==MACRO CJDRCH=======================================
      COMMON / CJDRCH / RDEC(4),PSIIN(3),RINCR(3),FIRSTW(3),FSENSW(3),
     * RDEPTH,SWDEPL,YSUSPN,TIMDEL(2,3),ZMAX,ZOFFS,ZRESOL,ZNORM,ZAL,
     * ZSCAL,DRIDEV,DRICOS,DRISIN,PEDES,TZERO(2)
C==ENDMACRO CJDRCH====================================
      EQUIVALENCE(IDRCH(1),RDEC(1))
      EQUIVALENCE (IDRCH(1),DRCH(1))
      COMMON/CWORK/HWORK(40)
      DATA MESS/1/
      DATA IVER/1/
C---
   50 CONTINUE
      LOOP=0
      IF(MESS.EQ.0) GO TO 1
      CALL TRMOUT(80,'OPTION TO CHANGE COMMON "CJDRCH".^')
      CALL TRMOUT(80,'TO CHANGE ITH (REAL*4) VARIABLE IN COMMON^')
      CALL TRMOUT(80,'ENTER I AND THE NEW VALUE YOU WISH.^')
      CALL TRMOUT(80,'ENTER 50 AND VALUE TO ASSIGN THIS VALUE^')
      CALL TRMOUT(80,'TO ALL DRCH(20-25).^')
      CALL TRMOUT(80,'ENTER 0 TO EXIT ROUTINE.^')
      CALL TRMOUT(80,'ENTER 100 TO SEE ALL CURRENT VALUES.^')
      CALL TRMOUT(80,'ENTER 200 TO TURN OFF (OR ON) THESE MESSAGES.^')
      CALL TRMOUT(80,'ENTER 300 TO TURN OFF (OR ON) VERIFICATION.^')
    1 CONTINUE
      CALL TONUM(IND,PAR)
      IF(IND.EQ.0) RETURN
      IF(IND.EQ.200) MESS=0
      IF(IND.EQ.300) IVER=0
      IF(IND.GT.100) GO TO 50
      IF(IND.NE.100) GO TO 2
      DO 3 I=1,37
      CALL CORE(HWORK,80)
      WRITE(JUSCRN,101) I,DRCH(I)
  101 FORMAT(I6,F12.6)
      CALL TRMOUT(18,HWORK)
    3 CONTINUE
      GO TO 50
    2 CONTINUE
      IF(IND.EQ.50) LOOP=1
      IF(IND.EQ.50) IND=20
      IF((IND.GE.1).AND.(IND.LE.37)) GO TO 4
      CALL TRMOUT(80,'NO SUCH INDEX IS AVAILABLE. PLEASE TRY AGAIN.^')
      GO TO 50
    4 CONTINUE
      IF(IVER.EQ.0) GO TO 5
      CALL CORE(HWORK,80)
      WRITE(JUSCRN,102) IND,DRCH(IND),PAR
  102 FORMAT('DRCH OF',I6,' WILL BE CHANGED FROM ',F12.6,' TO ',F12.6)
      CALL TRMOUT(63,HWORK)
      IF(LOOP.EQ.1) CALL TRMOUT(80,
     1'DRCH(21-25) WILL BE CHANGED TO THE SAME NEW VALUE.^')
      CALL TRMOUT(80,
     1'ARE YOU COMPLETELY COMFORTABLE WITH THIS PROSPECT?^')
      CALL DECIDE(IANSW)
      IF(IANSW.EQ.2) CALL TRMOUT(80,'NO ACTION TAKEN.^')
      IF(IANSW.EQ.2) CALL TRMOUT(80,
     1'RETURNING TO TOP OF ROUTINE.^')
      IF(IANSW.EQ.2) GO TO 50
    5 CONTINUE
      DRCH(IND)=PAR
      IF(LOOP.EQ.0) GO TO 6
      DO 7 I=21,25
      DRCH(I)=PAR
    7 CONTINUE
    6 CONTINUE
      CALL TRMOUT(80,'CHANGE MADE. DO YOU WISH TO MAKE MORE CHANGES?^')
      CALL DECIDE(IANSW)
      IF(IANSW.EQ.1) GO TO 50
      RETURN
      END
