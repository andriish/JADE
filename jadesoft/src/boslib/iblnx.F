C   07/06/96 606071850  MEMBER NAME  IBLNX    (S4)          FORTG1
      FUNCTION IBLN(NAME)
C     BOS SUBPROGRAM =0.3=
#include "acs.for"
      COMMON/BCS/IW(1)
      IW(INAMV)=NAME
      LFDI=MOD(IABS(NAME),NPRIM)+NAMAX1
   10 K=LFDI
      LFDI=IW(K+IPLST)
      IF(IW(LFDI+INAMV).NE.IW(INAMV)) GOTO 10
      IF(LFDI.NE.0) GOTO 90
      IF(NCI.GE.NAMAX) CALL BDMPA(1)
      NCI=NCI+1
      LFDI=NCI
      IW(INAMV+LFDI)=NAME
      IW(K+IPLST)=LFDI
   90 IBLN=LFDI
  100 RETURN
      END
