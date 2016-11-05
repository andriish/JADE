      SUBROUTINE PATREC(IDRENT)
      IMPLICIT INTEGER*2 (H)
      LOGICAL TBIT
#include "cworkpr.for"
#include "cworkeq.for"
#include "cdata.for"
      COMMON /CHEADR/ IHEADR(54)
      DIMENSION HEARR(200)
      DATA LBINIT /0/
      IF(
     - LBINIT .EQ. 0
     -)THEN
        LBINIT = 0
        IQPATR = IBLN('PATR')
        IQJHTL = IBLN('JHTL')
        IQJETC = IBLN('JETC')
        IQHEAD = IBLN('HEAD')
      ENDIF
      IPHEAD=IDATA(IQHEAD)
      IF(IPHEAD.GE.1) CALL MVCL(IHEADR(1),0,IDATA(IPHEAD-3),0,216)
      CALL PATRC1(IDRENT)
      IF(
     - NTR.GT.0
     -)THEN
      DO 13000 IMO=1,NTR
          HEARR(IMO)=IMO
13000 CONTINUE
13001 CONTINUE
        ITRK=NTR
        NTRLM = 0
        CALL KNTREL(HEARR,ITRK)
      ENDIF
      RETURN
      END
