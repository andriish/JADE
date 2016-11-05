      SUBROUTINE TRHTCK(IPPATR,IPJHTL)
      IMPLICIT INTEGER*2 (H)
#include "cdata.for"
#include "ccycp.for"
#include "cjdrch.for"
#include "cworkpr.for"
#include "cworkeq.for"
      DIMENSION NCNT1(127),NCNT2(127)
      NTR1   = IDATA(IPPATR+ 2)
      IF(NTR1.LE.0) RETURN
      LPATR = IDATA(IPPATR)
      IPHL0 = IPJHTL*2 + 3
      ILDHL = IDATA(IPJHTL)*2 - 2
      IPHL9 = ILDHL + IPHL0 - 1
      I0 = IPHL0
      I9 = IPHL9
      I1 = IDATA(IBLN('PATR'))
      I2 = IDATA(IBLN('JHTL'))
      CALL SETSL(NCNT1(1),0,1016,0)
      DO 13000 I=IPHL0,IPHL9,2
        IZW1  = HDATA(I  )
        ITRK1 = LAND(ISHFTR(IZW1,1),127)
      IF(
     - ITRK1.GT.0
     -)THEN
          IZW2  = HDATA(I+1)
          ITRK2 = LAND(ISHFTR(IZW2,1),127)
      IF(
     - ITRK2.LE.0
     -)THEN
            NCNT1(ITRK1) = NCNT1(ITRK1) + 1
      ELSE
            NCNT2(ITRK1) = NCNT2(ITRK1) + 1
            NCNT2(ITRK2) = NCNT2(ITRK2) + 1
      ENDIF
      ENDIF
13000 CONTINUE
13001 CONTINUE
      MTR = 0
      IPTR9 = IDATA(IPPATR) + IPPATR
      DO 13002 ITR=1,NTR1
      IF(
     - NCNT1(ITR).LT.5 .OR. NCNT1(ITR)+NCNT2(ITR).LT.8
     -)THEN
      ASSIGN 17001 TO IZZZ01
      GOTO 17000
17001 CONTINUE
          NCNT1(ITR) =-NCNT1(ITR)
      ELSE
          MTR = MTR + 1
      ENDIF
13002 CONTINUE
13003 CONTINUE
      IDATA(IPPATR+2) = MTR
      LENG  = IDATA(IPPATR+2)*IDATA(IPPATR+3) + IDATA(IPPATR+1)
      NDIFF = LENG - IDATA(IPPATR)
      IF(NDIFF.NE.0) CALL BCHM(IPPATR,NDIFF,IRET)
      RETURN
17000 CONTINUE
        JTR = MTR + 1
        ITRDIF = ITR - JTR
      IF(
     - NCNT1(ITR).NE.0 .OR. NCNT2(ITR).NE.0
     -)THEN
      DO 13004 I=IPHL0,IPHL9,2
            IZW1  = HDATA(I  )
            ITRK1 = LAND(ISHFTR(IZW1,1),127)
            IZW2  = HDATA(I+1)
            ITRK2 = LAND(ISHFTR(IZW2,1),127)
      IF(
     - ITRK2.EQ.JTR
     -)THEN
              HDATA(I+1) = 0
              ITRK2 = 0
              IND1  = ITRK1 + ITRDIF
              NCNT1(IND1) = NCNT1(IND1) + 1
              NCNT2(IND1) = NCNT2(IND1) - 1
      ENDIF
      IF(
     - ITRK1.EQ.JTR
     -)THEN
              HDATA(I ) = HDATA(I+1)
              HDATA(I+1) = 0
              ITRK1 = ITRK2
              ITRK2 = 0
      IF(
     - ITRK1.GT.0
     -)THEN
                IND1  = ITRK1 + ITRDIF
                NCNT1(IND1) = NCNT1(IND1) + 1
                NCNT2(IND1) = NCNT2(IND1) - 1
      ENDIF
      ENDIF
            IF(ITRK1.GT.JTR) HDATA(I  ) = HDATA(I  )-2
            IF(ITRK2.GT.JTR) HDATA(I+1) = HDATA(I+1)-2
13004 CONTINUE
13005 CONTINUE
      ENDIF
        LTRBK = IDATA(IPPATR+3)
        IPTR1 = IPPATR + IDATA(IPPATR+1) + MTR*LTRBK
        IPTR2 = IPTR1 + LTRBK
        NBYTE = (IPTR9 -IPTR2 + 1) * 4
      IF(
     - NBYTE.GT.0
     -)THEN
          CALL MVCL(IDATA(IPTR1+1),0,IDATA(IPTR2+1),0,NBYTE)
      DO 13006 IP=IPTR1,IPTR9,LTRBK
            IDATA(IP+1) = IDATA(IP+1) - 1
13006 CONTINUE
13007 CONTINUE
      ENDIF
        IPTR9 = IPTR9 - LTRBK
      GOTO IZZZ01
      END
