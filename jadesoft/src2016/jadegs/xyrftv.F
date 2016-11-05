      SUBROUTINE XYRFTV(MODE)
      IMPLICIT INTEGER*2 (H)
#include "cdata.for"
      COMMON/XYFVT1/MODXYV
      DATA LBINIT /0/, IQPATR/0/, IQJHTL/0/
      INTEGER WEAK / Z80000 /, STRONG / Z40000 /, CONCLR,
     +        CONSTR / Z400 /
      IF(
     - LBINIT .LE.0
     -)THEN
         LBINIT = 1
         IQPATR = IBLN('PATR')
         IQJHTL = IBLN('JHTL')
         CONCLR = LCOMPL(LOR(STRONG,WEAK))
      IF(
     - LAND(MODE,16).NE.0
     -)THEN
            WRITE(6,81)
 81         FORMAT(' *** XYRFTV WITHOUT VERTEX CONSTRAINT ***')
      ELSE
      IF(
     - LAND(MODE,4).EQ.0
     -)THEN
               WRITE(6,82)
 82            FORMAT(' *** XYRFTV WITH VERTEX CONSTRAINT ***')
               WRITE(6,84)
 84            FORMAT(' VC NOT APPLIED IF VERTEX INCOMPATIBLE',
     +         ' WITH TRACK FITTED W/O VC FIRST.')
      ELSE
               WRITE(6,83)
 83            FORMAT(' *** XYRFTV WITH WEAK VERTEX CONSTRAINT ***')
               WRITE(6,84)
      ENDIF
      ENDIF
      ENDIF
      IPPAT0 = IDATA(IQPATR)
      IF(IPPAT0.LE.0 .OR. IDATA(IQJHTL).LE.0 ) RETURN
      NTR    = IDATA(IPPAT0+2)
      IF(NTR.LT.1) RETURN
      IF(
     - LAND(MODE,1) .NE. 0
     -)THEN
         NBNK1  = IDATA(IPPAT0-2) - 1
         NWRD   = IDATA(IPPAT0)
         NBYTE  = NWRD*4
         CALL CCRE(IPPATR,'PATR',NBNK1,NWRD,IERR)
      IF(
     - IERR.NE.0
     -)THEN
            PRINT 2900, IERR
 2900       FORMAT(' CREATION OF NEW PATR-BANK RESULTED IN ERROR',I3)
            RETURN
      ENDIF
         CALL MVCL(IDATA(IPPATR+1),0,IDATA(IPPAT0+1),0,NBYTE)
      ENDIF
      IF(
     - LAND(MODE,32) .NE. 0
     -)THEN
         NBNK1 = IDATA(IDATA(IQPATR)-2)
         IPJHTL = IDATA(IQJHTL)
         NWRD = IDATA(IPJHTL)
         CALL CLOC( NPJHTL, 'JHTL', NBNK1, IER )
      IF(
     - NPJHTL.LE.0
     -)THEN
           CALL CCRE(NPJHTL,'JHTL',NBNK1,NWRD,IERR)
      IF(
     - IERR.NE.0
     -)THEN
              PRINT 2910, IERR
 2910         FORMAT(' CREATION OF NEW JHTL-BANK RESULTED IN ERROR',I3)
              RETURN
      ENDIF
           CALL MVCL(IDATA(NPJHTL+1),0,IDATA(IPJHTL+1),0,NWRD*4)
      ENDIF
      ENDIF
      IPPATR = IDATA(IQPATR)
      IPTR   = IDATA(IPPATR+1) + IPPATR
      LDTR   = IDATA(IPPATR+3)
      IF(
     - LAND(MODE,16) .NE. 0
     -)THEN
         ERRFAC = 1000.0
         KIND = 0
      ELSE
      IF(
     - LAND(MODE,4)  .NE. 0
     -)THEN
            ERRFAC =  100.0
            KIND = WEAK
      ELSE
            ERRFAC = 1.0
            KIND = STRONG
      ENDIF
      ENDIF
      MODXYV=MODE
      DO 13000 ITR=1,NTR
         CALL XYRFT1(IPTR,IDATA(IQJHTL),ERRFAC,LDTR)
         IDATA(IPTR+2) = LAND(IDATA(IPTR+2),CONCLR)
      IF(
     -  LAND(IDATA(IPTR+2),CONSTR).NE.0
     -)THEN
            IDATA(IPTR+2) = LOR(IDATA(IPTR+2),KIND)
      ENDIF
         IPTR=IPTR+LDTR
13000 CONTINUE
13001 CONTINUE
      RETURN
      END
