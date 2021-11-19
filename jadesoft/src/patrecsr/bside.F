      SUBROUTINE BSIDE
      IMPLICIT INTEGER*2 (H)
#include "cworkpr.for"
#include "cjdrch.for"
#include "cdsmax.for"
#include "cpatlm.for"
#include "cworkmx.for"
      DIMENSION LSTCL(3),LFTCL(3),NCELL(3),TANDEL(3)
      EQUIVALENCE (IBCK(1),LSTCL(1)),(IBCK(4),LFTCL(1))
      EQUIVALENCE (IBCK(7),NCELL(1)),(DBCK(1),TANDEL(1))
      DATA MSKCR1 /Z'100'/
      LRCORN=0
      KT=K
      IRIFLG=0
      IWT=ILIM-1
      ICX=ICL
15000 CONTINUE
      IF(
     - IWT.LT.ILIM
     -)THEN
      DS=TRKAR(KT,10-IUDFLG)
      IW=ITRKAR(KT,9-IUDFLG)
      IKX=0
      A=TRKAR(KT,11-IUDFLG)
      ASSIGN 17001 TO IZZZ01
      GOTO 17000
17001 CONTINUE
      ICT=ICX+1
      IF(ICT.GT.LSTCL(KRING)) ICT=ICT-NCELL(KRING)
      LR1=2
      IF(
     - IPER.GE.0
     -)THEN
      IF(
     - HNTCEL(ICT+1)-HNTCEL(ICT).GT.0
     -)THEN
      ASSIGN 17003 TO IZZZ02
      GOTO 17002
17003 CONTINUE
      IF(
     - LR.EQ.1.OR.IMARK.EQ.0.OR.LR.EQ.0
     -)THEN
      CALL SIDE1
      ELSE
      IWT=ILIM
      ENDIF
      ELSE
      IWT=ILIM
      ENDIF
      ELSE
      IWT=ILIM
      ENDIF
      IF(
     - IKX.NE.0
     -)THEN
      IF(
     - LR.EQ.-1
     -)THEN
      CALL COREC
      ENDIF
      LR=-1
      IWT=HMCH(ITRKAR(IKX,9-IUDFLG)+1,KRING,2)
      IF(IUDFLG.EQ.6) IWT=-IWT
      CALL BSTORE
      IPST=0
      LR=1
      IJFLG=0
      ENDIF
      IF(
     - IKX.EQ.0
     -)THEN
      ICT=ICX-1
      IF(ICT.LT.LFTCL(KRING)) ICT=ICT+NCELL(KRING)
      LR1=1
      IF(
     - IPER.LE.0
     -)THEN
      IF(
     - HNTCEL(ICT+1)-HNTCEL(ICT).GT.0
     -)THEN
      ASSIGN 17004 TO IZZZ02
      GOTO 17002
17004 CONTINUE
      IF(
     - LR.EQ.-1.OR.IMARK.EQ.0.OR.LR.EQ.0
     -)THEN
      CALL SIDE1
      ELSE
      IWT=ILIM
      ENDIF
      ELSE
      IWT=ILIM
      ENDIF
      ELSE
      IWT=ILIM
      ENDIF
      IF(
     - IKX.NE.0
     -)THEN
      IF(
     - LR.EQ.0
     -)THEN
      HISTR(1,NTR)=-HISTR(1,NTR)
      ENDIF
      IF(
     - LR.EQ.1
     -)THEN
      CALL COREC
      ENDIF
      LR=1
      IWT=HMCH(ITRKAR(IKX,9-IUDFLG)+1,KRING,1)
      IF(IUDFLG.EQ.6) IWT=-IWT
      CALL BSTORE
      IPST=0
      IJFLG=0
      LR=-1
      ENDIF
      ELSE
      IWT=ILIM
      ENDIF
      GOTO 15000
      ENDIF
15001 CONTINUE
      RETURN
17002 CONTINUE
      IMARK=0
      KC1=HISTR(1,NTR)
      KC1=IPCL(IABS(KC1))
      KC2=HISTR(HNREL(NTR),NTR)
      KC2=IPCL(IABS(KC2))
      IF(
     - KC1.NE.KC2
     -)THEN
      IF(
     - KC1.LE.48
     -)THEN
      IF(KC1-KC2.NE.24) IMARK=1
      ELSE
      IMARK=1
      ENDIF
      ENDIF
      GOTO IZZZ02
17000 CONTINUE
      IPER=0
      IF(
     - IBKK(20).NE.0.AND.LR.NE.0.AND.NRHT(KT).GE.IBKK(19)
     -)THEN
      IPER=LR
      IF(LAND(MSKCR1,LBL(KT)).NE.0.AND.IUDFLG.EQ.3) IPER=-IPER
      ENDIF
      GOTO IZZZ01
      END
