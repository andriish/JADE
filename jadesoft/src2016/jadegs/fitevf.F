      SUBROUTINE FITEVF(MODE)
      IMPLICIT INTEGER*2 (H)
#include "cdata.for"
#include "cworkpr.for"
#include "cworkeq.for"
      EQUIVALENCE
     ,          (HPCO0 ,HPWRK(17)),(HPCO9 ,HPWRK(18)),(HLDCO ,HPWRK(19))
     ,         ,(HPZR0 ,HPWRK(20)),(HPZR9 ,HPWRK(21)),(HLDZR ,HPWRK(22))
     ,         ,(ICELL ,IDWRK( 1)),(MHIT  ,IDWRK( 2)),(IRING ,IDWRK( 3))
     ,         ,(IERRCD,IDWRK( 4)),(NTRKEL,IDWRK( 5))
      DATA LBINIT /0/
      IF(
     - LBINIT.EQ.0
     -)THEN
        LBINIT = 1
        IQPATR = IBLN('PATR')
        IQJHTL = IBLN('JHTL')
      ENDIF
        IF(IDATA(IQPATR).LE.0) RETURN
      IF(
     - LAND(MODE,1) .NE. 0
     -)THEN
          IPPAT0 = IDATA(IQPATR)
          NBNK1  = IDATA(IPPAT0-2) - 1
          NWRD   = IDATA(IPPAT0)
          NBYTE  = NWRD*4
          CALL CCRE(IPPATR,'PATR',NBNK1,NWRD,IERR)
      IF(
     - IERR.NE.0
     -)THEN
 2900 FORMAT(' FITEVF(PS): CREATION OF NEW PATR-BANK RESULTED',
     ,       ' IN ERROR',I3)
            INDEX = 1
            RETURN
      ENDIF
          CALL MVCL(IDATA(IPPATR+1),0,IDATA(IPPAT0+1),0,NBYTE)
      ENDIF
      IPPATR = IDATA(IQPATR)
      IPTR1  = IDATA(IPPATR+1) + IPPATR
      LDTR   = IDATA(IPPATR+3)
      NTR    = IDATA(IPPATR+2)
      IPJHTL = IDATA(IQJHTL)
      IF(
     - NTR.GT.0
     -)THEN
      DO 13000 ITR=1,NTR
      IF(
     - ABS(ADATA(IPTR1+25)) .LT. .007
     -)THEN
            CALL REFIT(IPTR1,IPJHTL)
            IP    = HPTR0 - 1
      IF(
     - LAND(IWRK(IP+ 2),16).EQ.0
     -)THEN
              SIG   = 1000.
              SIG0  =    0.
      ELSE
              SIG   =  WRK(IP+23)
              SIG0  = ADATA(IPTR1+23)
              ANHT  = IWRK(IP+24)
              ANHT0 = IDATA(IPTR1+24)
              IF(ANHT/ANHT0.LT..8) SIG = SIG + .5
      ENDIF
      IF(
     - SIG0.GT.SIG .OR. SIG.LT..35
     -)THEN
              CALL MVC(IDATA(IPTR1+1),0,IWRK(HPTR0),0,112)
      ENDIF
      ENDIF
        IPTR1 = IPTR1 + LDTR
13000 CONTINUE
13001 CONTINUE
      ENDIF
      RETURN
      END
