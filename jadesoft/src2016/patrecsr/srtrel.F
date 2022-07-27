      SUBROUTINE SRTREL
      IMPLICIT INTEGER*2 (H)
      LOGICAL TBIT
#include "cdata.for"
#include "ccycp.for"
#include "cworkpr.for"
#include "cworkeq.for"
      EQUIVALENCE
     ,           (ICELL ,IDWRK(1)),(NHIT  ,IDWRK(2)),(IRING ,IDWRK(3))
     ,         , (IERRCD,IDWRK(4)),(NTRKEL,IDWRK(5))
     ,         , (ITR   ,IDWRK(7)),(ITRNG ,IDWRK(8))
#include "cpatlm.for"
      DATA MKTREL / Z'3000'/
      DATA MKLBTC / Z'FFF'/
      DATA MKLFTR / Z'30000'/, MKRGHT / Z'20000'/
      INTEGER  MKLRHT(3) 
      DATA  MKLRHT / Z'800', Z'0', Z'900'/
      DATA LBZRCR / Z'100'/
      DATA NANF /0/
      IF(NANF.EQ.0) IQPATR= IBLN('PATR')
      NANF = 1
      IPPATR = IDATA(IQPATR)
          HPFRE0 = HPFREE
          CALL PRHTAR
      IF(
     - NHIT.GE.5
     -)THEN
            CALL FLINEL
            CALL FTRKEL
      IF(
     - NTRKEL.GT.0
     -)THEN
      ASSIGN 17001 TO IZZZ01
      GOTO 17000
17001 CONTINUE
      ENDIF
            NLINUC = 0
      DO 13000 IP=HPHT0,HPHT9,HLDHT
              IF(IWRK(IP+9).EQ.0 .AND. LAND(IWRK(IP+4),7).NE.0)
     .        NLINUC = NLINUC + 1
13000 CONTINUE
13001 CONTINUE
            IDATA(IPPATR+7) = IDATA(IPPATR+7) + NLINUC
      ENDIF
          HPFREE = HPFRE0
      RETURN
17000 CONTINUE
        ICLL16 = (ICELL-1) * 16
        NTRCLL = 0
      DO 13002 IPTR = HPTR0,HPTR9,HLDTR
      IF(
     - ITR.LE.200 .AND. ITRNG.LT.128
     -)THEN
      IF(
     - LAND(IWRK(IPTR+15),MKTREL).NE.0
     -)THEN
            NTRCLL = NTRCLL + 1
            LB = LAND(IWRK(IPTR+15),MKLBTC)
            DRSP1 = WRK(IPTR+ 8)
            DRSP2 = WRK(IPTR+11)
            IF(DRSP1*DRSP2.LT.0) LB = LOR(LB,LBZRCR)
            LBLR = LAND(IWRK(IPTR+15),MKLFTR)
            IF(DRSP1.LT.0..AND.LBLR.NE.0.AND.LBLR.NE.MKLFTR)
     .         LBLR = LXOR(LBLR,MKLFTR)
            LB = LOR(LB,ISHFTR(LBLR,6))
            ITRKAR(ITR, 1) = ICELL
            ITRKAR(ITR, 2) = IWRK(IPTR+ 2)
            ITRKAR(ITR, 3) = IWRK(IPTR+ 7)
            TRKAR (ITR, 4) = ABS(DRSP1)
            DRSL1          = WRK(IPTR+ 9)
            IF(DRSP1.LT.0.)  DRSL1 =-DRSL1
            TRKAR (ITR, 5) = DRSL1
            ITRKAR(ITR, 6) = IWRK(IPTR+10)
            TRKAR (ITR, 7) = ABS(DRSP2)
            DRSL2          = WRK(IPTR+12)
            IF(DRSP2.LT.0.)  DRSL2 =-DRSL2
            TRKAR (ITR, 8) = DRSL2
            ITRKAR(ITR, 9) = LB
            ITRKAR(ITR,10) = ITRNG
            IDXLR =-1
            IF(LBLR.EQ.MKRGHT) IDXLR = 1
            IF(DRSP1.LT.0.) IDXLR =-IDXLR
            IDXLR1 = IDXLR
            ILAYZ =-1
      DO 13004 IP=HPHT0,HPHT9,HLDHT
      IF(
     - IWRK(IP+9).EQ.IPTR .OR.IWRK(IP+10).EQ.IPTR
     -)THEN
                DRSP = WRK(IP+2)
            IF(IWRK(IP+10).EQ.IPTR .AND. TBIT(IWRK(IP+4),20)) DRSP=-DRSP
                IF(DRSP.NE.0.) IDXLR1 = IDXLR
      IF(
     - DRSP*DRSP1.LT.0.
     -)THEN
                  IDXLR1 =-IDXLR
                  IF(ILAYZ.LT. 0) ILAYZ = IWRK(IP)
      ENDIF
                IPLBHT = (IWRK(IP+1) - HPTSEC(1))/2 + HPHL0
                LBHIT = ITRNG*2
                LBHIT = LOR(LBHIT,MKLRHT(IDXLR1+2))
      IF(
     - HWRK(IPLBHT).EQ.0
     -)THEN
                  HWRK(IPLBHT) = LBHIT
      ELSE
                  IF(HWRK(IPLBHT+1).EQ.0) HWRK(IPLBHT+1) = LBHIT
      ENDIF
      ENDIF
13004 CONTINUE
13005 CONTINUE
            IF(LAND(LB,LBZRCR).NE.0 .AND. ILAYZ.LT.0)
     .                                    ILAYZ = IWRK(IPTR+10) + 1
            IF(ILAYZ.LT.0) ILAYZ = 0
            ITRKAR(ITR,11) = ILAYZ
            ITR   = ITR   + 1
            ITRNG = ITRNG + 1
      ENDIF
      ENDIF
13002 CONTINUE
13003 CONTINUE
      GOTO IZZZ01
      END
