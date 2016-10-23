      SUBROUTINE FITEVR( NTRVTX, MODE )
      IMPLICIT INTEGER*2 (H)
#include "cdata.for"
#include "calibr.for"
#include "cworkpr.for"
#include "cworkeq.for"
      EQUIVALENCE
     ,          (HPCO0 ,HPWRK(17)),(HPCO9 ,HPWRK(18)),(HLDCO ,HPWRK(19))
     ,         ,(HPZR0 ,HPWRK(20)),(HPZR9 ,HPWRK(21)),(HLDZR ,HPWRK(22))
     ,         ,(ICELL ,IDWRK( 1)),(MHIT  ,IDWRK( 2)),(IRING ,IDWRK( 3))
     ,         ,(IERRCD,IDWRK( 4)),(NTRKEL,IDWRK( 5))
        DATA LBINIT /0/
        DATA NPRMES /0/
      IF(
     - LBINIT .LE.0
     -)THEN
          LBINIT = 1
          IQHEAD = IBLN('HEAD')
          IQPATR = IBLN('PATR')
          IQJHTL = IBLN('JHTL')
          IQZVTX = IBLN('ZVTX')
      ENDIF
        NITER = 0
16000 CONTINUE
        NITER = NITER + 1
          NTRVTX = 0
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
              PRINT 2900, IERR
 2900   FORMAT(' USFITO3(PS): CREATION OF NEW PATR-BANK RESULTED',
     ,         ' IN ERROR',I3)
              INDEX = 1
              RETURN
      ENDIF
            CALL MVCL(IDATA(IPPATR+1),0,IDATA(IPPAT0+1),0,NBYTE)
      ENDIF
          IPPATR = IDATA(IQPATR)
          IPTR1  = IDATA(IPPATR+1) + IPPATR
          LDTR   = IDATA(IPPATR+3)
          NTR    = IDATA(IPPATR+2)
          IF(NTR.LT.1) RETURN
          MTRV = 0
          IPV  = ICALIB(10)
          X0   = ACALIB(IPV+ 1)
          Y0   = ACALIB(IPV+ 3)
      DO 13000 ITR=1,NTR
      IF(
     - ABS(ADATA(IPTR1+25)).LT..00143 .AND.IDATA(IPTR1+24).GT.16
     -)THEN
              CALL DRTRCK(IPTR1,X0,Y0,DR0)
      IF(
     - ABS(DR0).LT.25. .OR. IDATA(IPTR1+4).EQ.1
     -)THEN
                MTRV = MTRV + 1
                IDATA(IPTR1+4) =-1
      ENDIF
      ENDIF
          IPTR1 = IPTR1 + LDTR
13000 CONTINUE
13001 CONTINUE
      IF(
     - MTRV.LE.0
     -)THEN
      GOTO 16001
      ENDIF
            Z0 = 0.
            IPZVTX = IDATA(IQZVTX)
            IF(IPZVTX.GT.0.AND.IDATA(IPZVTX+6).EQ.3) Z0=ADATA(IPZVTX+1)
            ERRFAC = 1.0
            IF(LAND(MODE,4)  .NE. 0) ERRFAC =  100.0
            IF(LAND(MODE,16) .NE. 0) ERRFAC = 1000.0
            MODEF = MODE
            CALL FZFITV(IPPATR,IDATA(IQJHTL),NTRVTX,Z0,ERRFAC,MODEF)
      IF(
     - MODEF .GE.0
     -)THEN
      GOTO 16001
      ENDIF
            IPHEAD = IDATA(IQHEAD) * 2
            NPRMES = NPRMES + 1
            IF(NPRMES.LE.10)
     ,      PRINT 2009, HDATA(IPHEAD+10),HDATA(IPHEAD+11),MODEF
 2009 FORMAT(' FITEVR(PST): ERROR IN JHTL-BANK FOR EVENT',3I6)
      IF(
     - LAND(MODE,8).EQ.0
     -)THEN
      GOTO 16001
      ENDIF
      IF(
     - NITER .GE.2
     -)THEN
      GOTO 16001
      ENDIF
      IF(
     - LAND(MODE,1).NE.0
     -)THEN
              IPPATR = IDATA(IQPATR)
              NBNK   = IDATA(IPPATR-2)
              CALL BDLS('PATR',NBNK)
      ENDIF
            IPPATR = IDATA(IQPATR)
            NBNK   = IDATA(IPPATR-2)
            CALL BDLS('PATR',NBNK)
            IPJHTL = IDATA(IQPATR)
            CALL BDLS('JHTL',NBNK)
            IND = 1
            CALL PATRCO(IND)
      IF(.NOT.(
     - NITER .GE.2
     -))GOTO 16000
16001 CONTINUE
      RETURN
      END
