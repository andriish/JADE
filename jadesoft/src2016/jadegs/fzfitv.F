      SUBROUTINE FZFITV(IPPATR,IPJHTL,NTRVTX,Z0,ERRFAC,MODEF)
      IMPLICIT INTEGER*2 (H)
#include "cdata.for"
#include "cworkpr.for"
#include "cworkeq.for"
      EQUIVALENCE
     ,          (HPCO0 ,HPWRK(17)),(HPCO9 ,HPWRK(18)),(HLDCO ,HPWRK(19))
     ,         ,(HPZR0 ,HPWRK(20)),(HPZR9 ,HPWRK(21)),(HLDZR ,HPWRK(22))
     ,         ,(ICELL ,IDWRK( 1)),(MHIT  ,IDWRK( 2)),(IRING ,IDWRK( 3))
     ,         ,(IERRCD,IDWRK( 4)),(NTRKEL,IDWRK( 5))
      NTRVTX = 0
      MODE   = MODEF
      IPTR1 = IDATA(IPPATR+1) + IPPATR
      LDTR  = IDATA(IPPATR+3)
      NTR   = IDATA(IPPATR+2)
      IF(
     - NTR.GT.0
     -)THEN
        IF(LAND(MODE,2).NE.0) CALL ZRFITO(-1,2001)
      DO 13000 ITR=1,NTR
          ITYPE =-IDATA(IPTR1+4)
      IF(
     - ITYPE .GT. 0
     -)THEN
            CALL FRFITV(IPTR1,IPJHTL,ERRFAC)
            IP    = HPTR0 - 1
            SIG   =  WRK(IP+23)
      IF(
     - LAND(IWRK(IP+ 2),32).EQ.0
     -)THEN
              SIG   = 1000.
              SIG0  =    0.
      ELSE
              SIG   =  WRK(IP+23)
              SIG0  = ADATA(IPTR1+23)
              ANHT  = IWRK(IP+24)
              IF(LAND(IWRK(IP+ 4),16).NE.0) ANHT = ANHT * 1.5
              ANHT0 = IDATA(IPTR1+24)
              IF(ANHT/ANHT0.LT..8) SIG = SIG + .5
      ENDIF
      IF(
     - SIG0.GT.SIG .OR. SIG.LT..35
     -)THEN
              NTRVTX = NTRVTX + 1
              CALL MVC(IDATA(IPTR1+1),0,IWRK(HPTR0),0,112)
              IDATA(IPTR1+4) = LOR(IDATA(IPTR1+4),ITYPE)
              INDZR = 0
      IF(
     - LAND(MODE,2) .NE. 0
     -)THEN
                 ADATA(IPTR1+17) = WRK(HPTR0+30)
                 CALL ZRFITO(INDZR,IPTR1)
      ENDIF
      IF(
     - LAND(MODE,2) .EQ. 0 .OR. INDZR.NE.0
     -)THEN
                 TGTH = ADATA(IPTR1+30)
                 Z0TR = ADATA(IPTR1+31)
                 CSTH = 1./SQRT(TGTH**2+1.)
                 SNTH = CSTH * TGTH
                 ADATA(IPTR1+ 8) = ADATA(IPTR1+ 8) * CSTH
                 ADATA(IPTR1+ 9) = ADATA(IPTR1+ 9) * CSTH
                 ADATA(IPTR1+15) = ADATA(IPTR1+15) * CSTH
                 ADATA(IPTR1+16) = ADATA(IPTR1+16) * CSTH
                 ADATA(IPTR1+17) = SNTH
                 ADATA(IPTR1+ 7) = ADATA(IPTR1+ 7) * TGTH + Z0TR
                 ADATA(IPTR1+14) = ADATA(IPTR1+14) * TGTH + Z0TR
      ENDIF
      ELSE
              IDATA(IPTR1+4) = 0
      ENDIF
      ENDIF
        IPTR1 = IPTR1 + LDTR
13000 CONTINUE
13001 CONTINUE
      IF(
     - NTRVTX.GT.0
     -)THEN
          INDZR = 1
          IF(LAND(MODE,2).NE.0) CALL ZRFITO(INDZR,Z0)
      ENDIF
      ENDIF
      RETURN
      END
