C   07/06/96 606071839  MEMBER NAME  CLOC     (S4)          FORTG1
      SUBROUTINE CLOC(IND,NAME,NR)
C     BOS SUBPROGRAM =1.6=
#include "acs.for"
      COMMON/BCS/IW(1)
      IW(INAMV)=NAME
      LFDI=MOD(IABS(NAME),NPRIM)+NAMAX1
    1 LFDI=IW(LFDI+IPLST)
      IF(IW(LFDI+INAMV).NE.IW(INAMV)) GOTO 1
      IF(LFDI.EQ.0) LFDI=IBLN(IW(INAMV))
      LFDK=LFDI+1
      LFDI=IW(LFDI)
      GOTO 3
    2 LFDK=LFDI
      LFDI=IW(LFDI-1)
    3 IF(LFDI.EQ.0)        GOTO    10
      IF(IW(LFDI-2)-NR) 2,4,10
    4 IND=LFDI
  100 RETURN
   10 IND=0
      GOTO 100
      END
