      SUBROUTINE PATREC(IDRENT)
      IMPLICIT INTEGER*2 (H)
      LOGICAL TBIT
C----------------------------------------------
C  MACRO CWORKPR .... PATTERN RECOGNITION CWORK
C----------------------------------------------
      COMMON /CWORK/ HPLAST,HPFREE,HPWRK(30),ADWRK(600),
     ,               HPRO,HNTR,HNTCEL(98),IPCL(200),NRHT(200),
     ,               NWR1(200),DS1(200),SL1(200),
     ,               NWR2(200),DS2(200),SL2(200),
     ,               LBL(200),NTREL(200),ICRO(200),
     ,               NTR,HNREL(100),HISTR(9,100),HRES(168),
     ,               NTRLM,RLMTR(3,5),
     ,               WRK(7000)
                     DIMENSION TRKAR(200,11),ITRKAR(200,11),
     ,                         LMRTR(3,5)
                     EQUIVALENCE (IPCL(1),TRKAR(1,1),ITRKAR(1,1))
                     EQUIVALENCE (LMRTR(1,1),RLMTR(1,1))
         DIMENSION IWRK(7000),HWRK(14000),IDWRK(600),HDWRK(1200)
                     EQUIVALENCE (IWRK(1),WRK(1),HWRK(1))
                     EQUIVALENCE (IDWRK(1),ADWRK(1),HDWRK(1))
C---------- END OF MACRO CWORKPR --------------
C-------------------------------------------------------
C  MACRO CWORKEQ .... PATTERN RECOGNITION CWORK POINTERS
C-------------------------------------------------------
      EQUIVALENCE
C                POINTERS FOR FXYZ HIT ARRAY .. PRIMARY L/R SOLUTION
     +          (HPHT0,HPWRK( 1)),(HPHT9,HPWRK( 2)),(HLDHT,HPWRK( 3))
C                POINTERS FOR CWORK SINGLE TRACK PATR BANK
     +         ,(HPTR0,HPWRK( 4)),(HPTR9,HPWRK( 5)),(HLDTR,HPWRK( 6))
C                POINTERS FOR TRACK ELEMENT HIT LABEL ARRAY
     +         ,(HPHL0,HPWRK( 7)),(HPHL9,HPWRK( 8)),(HLDHL,HPWRK( 9))
C                POINTERS FOR FXYZ HIT ARRAY .. OPPOSITE L/R SOLUTION
     +         ,(HPHT0A,HPWRK(10)),(HPHT9A,HPWRK(11)),(HLDHTA,HPWRK(12))
C               POINTER LIMIT ON FXYZ HIT ARRAY
     +         ,(HPHTLM,HPWRK(13))
C               POINTERS FOR
     +         ,(HPTE0,HPWRK(14)),(HPTE9,HPWRK(15)),(HLDTE,HPWRK(16))
C-------------- END OF MACRO CWORKEQ ------------------
C----------------------------------------------------------------------
C             MACRO CDATA .... BOS COMMON.
C
C             THIS MACRO ONLY DEFINES THE IDATA/HDATA/ADATA NAMES.
C             THE ACTUAL SIZE OF /BCS/ IS FIXED ON MACRO CBCSMX
C             OR BY OTHER MEANS. A DEFAULT SIZE OF 40000 IS GIVEN HERE.
C
C----------------------------------------------------------------------
C
      COMMON /BCS/ IDATA(40000)
      DIMENSION HDATA(80000),ADATA(40000),IPNT(50)
      EQUIVALENCE (HDATA(1),IDATA(1),ADATA(1)),(IPNT(1),IDATA(55))
      EQUIVALENCE (NWORD,IPNT(50))
C
C------------------------ END OF MACRO CDATA --------------------------
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
