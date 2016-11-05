      SUBROUTINE PRHTAR
      IMPLICIT INTEGER*2 (H)
#include "cdata.for"
#include "ccycp.for"
#include "cworkpr.for"
#include "cworkeq.for"
      EQUIVALENCE
     ,            (ICELL,IDWRK(1)),(NHIT,IDWRK(2)),(IRING,IDWRK(3))
#include "cjdrch.for"
      DATA LBINIT /0/
      IF(
     - LBINIT .EQ. 0
     -)THEN
        LBINIT = 0
        IQJHTL = IBLN('JHTL')
        DSD0   =-.63
        DSD1   = 1.8
        DSD2   = 4.0
        DRV0   = 0.8
        DRV1   = (DSD2 - (DSD1-DSD0)*DRV0) / (DSD2-DSD1)
      ENDIF
      DSBIN1 = TIMDEL(1,IRING)
      DSBIN2 = TIMDEL(2,IRING)
      IP0 = HPTSEC(ICELL)
      IP9 = HPTSEC(ICELL+1) - 1
      IPHL = (HPTSEC(ICELL)-HPTSEC(1))/4 + IDATA(IQJHTL) + 1
      HPHT0 = HPFREE
      HLDHT = 12
      IPHT  = HPHT0
      JHIT = 0
      NWRD2 = NWORD*2
      DO 13000 IP=IP0,IP9,4
        IPHL = IPHL + 1
      IF(
     - IDATA(IPHL).EQ.0
     -)THEN
      IF(
     - HDATA(IP+1).GT.0 .AND. HDATA(IP+2).GT.0
     -)THEN
          IWIR = HDATA(IP)
          IWIR = ISHFTR(IWIR,3)
          ILAYR = LAND(IWIR,15)
          IF(ILAYR.GE.8) DSBIN1 = DSBIN2
          DS =(HDATA(IP+3)) * DSBIN1
      IF(
     - DS.LT.4.0
     -)THEN
      IF(
     - DS.GT.DSD1
     -)THEN
              DS = (DSD1-DSD0)*DRV0 + (DS-DSD1)*DRV1
      ELSE
              DS = (DS-DSD0)*DRV0
      ENDIF
            IF(DS.LT.0.1) DS = 0.1
      ENDIF
          WRK(IPHT+2)  = AMAX1(DS,0.)
          IWRK(IPHT  ) = ILAYR
          IWRK(IPHT+1) = IP
          IWRK(IPHT+4) = 0
          IWRK(IPHT+5) = 0
          IWRK(IPHT+6) = 0
          IWRK(IPHT+7) = 0
          IPHT = IPHT + HLDHT
          JHIT = JHIT + 1
      ENDIF
      ENDIF
13000 CONTINUE
13001 CONTINUE
      NHIT = JHIT
      HPHT9 = IPHT - 1
      HPFREE= IPHT
      RETURN
      END
