      SUBROUTINE BAKFIT(IB,ITYP)
      IMPLICIT INTEGER*2 (H)
C----------------------------------------------
C  MACRO CPATLM .... PATTERN RECOGNITION LIMITS
C----------------------------------------------
      COMMON /CPATLM/ PATRLM(5),FLINLM(10),TRELLM(20),ZFITLM(10),BKK(20)
     *               ,XYF(20),IGFP(20),XBKK(40),IADMIN(5),YBKK(20)
      INTEGER IXYF(20),LMPATR(5),LMFLIN(10)
      INTEGER LMTREL(20),LMZFIT(10),IBKK(20)
      DIMENSION GFP(20),IXBKK(40),IYBKK(20)
      EQUIVALENCE (PATRLM(1),LMPATR(1)),(IXBKK(1),XBKK(1)),(IYBKK(1),
     *YBKK(1))   ,(FLINLM(1),LMFLIN(1)),(TRELLM(1),LMTREL(1))
     *           ,(ZFITLM(1),LMZFIT(1)),(BKK(1),IBKK(1))
     *           ,(XYF(1),IXYF(1)),(GFP(1),IGFP(1)),(IADMIN(1),IMCERT)
     *           ,(IYBKK(20),IPPASS),(IADMIN(2),IPFAST)
C----------- END OF MACRO CPATLM --------------
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
       DIMENSION CHITR(9),HITIN(10)
       EQUIVALENCE (ADWRK(91),CHITR(1)),(HITIN(1),ADWRK(86))
       IB=0
       IREM=IXYF(1)
       IXYF(1)=LOR(IXYF(1),9)
       HPOLD=HPFREE
       IBTRK=NTR
       CALL FXYZ(IBTRK)
       HPTR0=HPFREE
       HPTR9=HPTR0+49
       HLDTR=50
      HPFREE=HPTR9+1
      IF(
     - HPFREE.LE.HPLAST
     -)THEN
      CALL XYFIT
      RES=CHITR(HNREL(NTR))
      RMS=WRK(HPTR0+22)
      NTOT=IWRK(HPTR0+23)
      NHT=HITIN(HNREL(NTR))
      IF(NHT.GT.0) RES=RES/FLOAT(NHT)
      IF(ITYP.EQ.1.AND.(RMS.GT.YBKK(2).OR.RES.GT.YBKK(3))) IB=-1
      IF(ITYP.EQ.2.AND.(RMS.GT.YBKK(4).OR.RES.GT.YBKK(5))) IB=-1
      IF(ITYP.EQ.3.AND.(RMS.GT.YBKK(6).OR.RES.GT.YBKK(7))) IB=-1
      IF(ITYP.EQ.4.AND.RMS.GT.YBKK(8)) IB=-1
      ELSE
      PRINT 33
 33   FORMAT(' +++++++++  NOT ENOUGH SPACE IN CWORK  +++++++')
      ENDIF
      IXYF(1)=IREM
      HPFREE=HPOLD
      RETURN
      END
