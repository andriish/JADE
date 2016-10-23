      SUBROUTINE EXJHTL(IERR)
      IMPLICIT INTEGER*2 (H)
      LOGICAL TBIT
#include "cdata.for"
      IERR = 0
      IPJHTL = IDATA(IBLN('JHTL'))
      IPJETC = IDATA(IBLN('JETC'))
      IF(IPJETC.LE.0) RETURN
      IF(IPJHTL.LE.0) RETURN
      NHIT  = (HDATA(IPJETC*2+99)) / 4
16000 CONTINUE
        NWHTL = IDATA(IPJHTL)
        I0 = IPJHTL*2 + 1
        I9 = IDATA(IPJHTL)*2 + I0 - 1
        IF(NWHTL.GE.NHIT) RETURN
        NWDIFF = NHIT+1 - NWHTL
        CALL BCHM(IPJHTL,NWDIFF,IERR)
        IF(IERR.NE.0) RETURN
        IP0 =  IPJHTL*2+2
        IP1 =  IP0 + NHIT
        IP2 =  IP0 + NHIT*2
16002 CONTINUE
          LBHIT = HDATA(IP1)
      IF(
     - LBHIT.EQ.0
     -)THEN
            HDATA(IP2-1) = 0
            HDATA(IP2  ) = 0
      ELSE
            LBZ   = LAND (LBHIT, 1)
            LBB12 = ISHFTR(LBHIT,13)
            LBB12 = ISHFTL(LBB12, 9)
            LBHT1 = LAND (LBHIT,63)
            LBHT1 = LOR  (LBHT1,LBB12)
            IF(TBIT(LBHIT,25)) LBHT1 = IBITON(LBHT1,23)
            LBHT2 = ISHFTR(LBHIT, 6)
            LBHT2 = LAND (LBHT2,62)
      IF(
     - LBHT2.NE.0
     -)THEN
              LBHT2 = LOR  (LBHT2,LBZ)
              LBHT2 = LOR  (LBHT2,LBB12)
              IF(TBIT(LBHIT,19)) LBHT2 = IBITON(LBHT2,23)
      ENDIF
            HDATA(IP2-1) = LBHT1
            HDATA(IP2  ) = LBHT2
      ENDIF
        DATA NPR /0/
        NPR = NPR + 1
        IP2 = IP2 - 2
        IP1 = IP1 - 1
      IF(.NOT.(
     - IP1.LE.IP0
     -))GOTO 16002
16003 CONTINUE
        I0 = IPJHTL*2 + 1
        I9 = IDATA(IPJHTL)*2 + I0 - 1
      IPJHTL = IDATA(IPJHTL-1)
      IF(.NOT.(
     - IPJHTL.LE.0
     -))GOTO 16000
16001 CONTINUE
      RETURN
      END
