      SUBROUTINE KNTREL(HEARR,NUMTRK)
      IMPLICIT INTEGER*2 (H)
      COMMON/CHEADR/HEAD(17),HRUN,HEV
      COMMON/CADMIN/IEVTP,NREC,NRWRIT,NRERR
      COMMON/CBKPAT/HTRK(100)
#include "cdata.for"
#include "ccycp.for"
#include "cworkpr.for"
#include "cworkeq.for"
      EQUIVALENCE (ICELL ,IDWRK(1)),(NHIT  ,IDWRK(2))
      DIMENSION HEARR(1)
#include "cpatlm.for"
      EQUIVALENCE (IXBKK(40),IXITER),(IXBKK(39),MAXITR),(JJPR,IXBKK(38))
      EQUIVALENCE (IXBKK(37),MINHIT),(IXBKK(36),ICUT)
      INTEGER DATE(5), IDAY /0/
      DIMENSION NCNT1(127),NCNT2(127),IXREF0(127)
      DIMENSION JCLLA(20),NCLLA(20)
      DATA MSKTR1/Z'7F'/
      DATA MSKDSP/Z'2000'/
      DATA MAXTRK/100/
      DATA MKBDHT /Z'600'/
 458  FORMAT(' ',20(X,Z4))
 675  FORMAT('  ***** HIT LABEL MAY BE ZEROED **********')
 754            FORMAT('  HPFREE , HPLAST ',2I10)
 97           FORMAT(1X,30('+'),' KNTREL ERROR',I2,4I7)
 674  FORMAT('  **** NOT ENOUGH SPACE IN CWORK TO MOVE HIT LABEL *******
     $ , HPFREE, HPLAST , NO OF WORDS ',3I7)
      IF(
     - IDAY.EQ.0
     -)THEN
        CALL DAY2(DATE)
        IDAY = DATE(1)*1000 + DATE(2)
        IQJHTL = IBLN('JHTL')
        IQPATR = IBLN('PATR')
      ENDIF
      IPJHTL = IDATA(IQJHTL)
      NHITT = (HPTSEC(97) - HPTSEC(1)) / 4
      IPPATR = IDATA(IQPATR)
      NTR0  = IDATA(IPPATR+2)
      ITRBK = NTR0
      LTRBK = IDATA(IPPATR+3)
      IPTRBK = IPPATR + IDATA(IPPATR+1) + ITRBK*LTRBK
      HPFRE0 = HPFREE
      CALL SETSL(IXREF0(1),0,508,0)
      IF(
     - NTR.GT.0 .AND. NUMTRK.GT.0
     -)THEN
        LBEVTR = 0
        IEDTK  = 0
        ITR    = 0
        MAXTR0 = MIN0(MAXTRK,NUMTRK)
15000 CONTINUE
      IF(
     - IEDTK.LT.MAXTR0
     -)THEN
        IEDTK=IEDTK+1
          ITR=HEARR(IEDTK)
      IF(
     - ITRBK.GT.MAXTRK .OR. ITR.GT.MAXTRK
     -)THEN
      GOTO 15001
      ENDIF
      IF(
     - HNREL(ITR).GT.0
     -)THEN
            HPFREE = HPFRE0
            CALL FXYZ(ITR)
            NHIT = (HPHT9-HPHT0+1) / HLDHT
      IF(
     - NHIT.GT.3
     -)THEN
              HPTR0 = HPFREE
              HPTR9 = HPTR0 + 49
              HLDTR = 50
              HPFREE= HPTR9 + 1
      IF(
     - HPFREE.LE.HPLAST
     -)THEN
                IWRK(HPTR0+47)=0
                CALL XYFIT
      IF(
     - WRK(HPTR0+22).LT.GFP(2)
     -)THEN
                  RMIN = 150.
                  RMAX = 850.
      IF(
     - NTRLM.GT.0
     -)THEN
      DO 13000 I=1,NTRLM
      IF(
     - LMRTR(1,I).EQ.ITR
     -)THEN
                        RMIN = RLMTR(2,I)
                        RMAX = RLMTR(3,I)
      GOTO 13001
      ENDIF
13000 CONTINUE
13001 CONTINUE
      ENDIF
                  CALL PATROL(RMIN,RMAX)
      ENDIF
                RMSFIT = WRK(HPTR0+22)
      IF(
     - RMSFIT.GE.1000. .OR.
     ?             IWRK(HPTR0+23).LT.5 .AND. IMCERT.EQ.0
     -)THEN
                  HNREL(ITR) = 0
      ELSE
                  NHGDZ = 0
                  IDHTLB=IPJHTL*2-HPHL0+3
      DO 13002 IIP=HPHT0,HPHT9,HLDHT
                    IPHTLB=IWRK(IIP+2)
                    IPHTL=HDATA(IPHTLB+IDHTLB)
                IF(LAND(IPHTL,MKBDHT).EQ.0.AND.IPHTL.NE.0) IWRK(IIP+7)=8
                    IF(IWRK(IIP+7).LT.8) NHGDZ = NHGDZ + 1
13002 CONTINUE
13003 CONTINUE
      IF(
     - NHGDZ.LT.3
     -)THEN
                    HNREL(ITR) = 0
      ELSE
                    CALL ZRFIT
                    CRV  = ABS(WRK(HPTR0+24))
                    ZINT = ABS(WRK(HPTR0+30))
      IF(
     - IYBKK(14).NE.0 .AND. CRV.GT.YBKK(12) .AND.
     ?                 ZINT.GT.YBKK(15)
     -)THEN
                      KP=HPTR0
                      CSTH=WRK(KP+4)*WRK(KP+7)+WRK(KP+5)*WRK(KP+8)
                      CSTH=CSTH/SQRT((WRK(KP+4)**2+WRK(KP+5)**2)*
     *                          (WRK(KP+7)**2+WRK(KP+8)**2))
                      IF(CSTH.LT.YBKK(13)) HNREL(ITR) = 0
      ENDIF
      IF(
     - HNREL(ITR).GT.0
     -)THEN
                      ITRBK = ITRBK + 1
                      IXREF0(ITRBK) = ITR
      ASSIGN 17001 TO IZZZ01
      GOTO 17000
17001 CONTINUE
      IF(
     - NHTREG.LT.5
     -)THEN
                        ITRBK  = ITRBK  - 1
                        IPTRBK = IPTRBK - LTRBK
      ENDIF
      ENDIF
      ENDIF
      ENDIF
      ELSE
                KERROR = 2
                PRINT97,KERROR,NREC,HRUN,HEV,ITR
                PRINT 754,HPFREE,HPLAST
      GOTO 15001
      ENDIF
      ELSE
              KERROR = 1
              PRINT97,KERROR,NREC,HRUN,HEV,ITR
              HNREL(ITR)=0
      ENDIF
      ENDIF
      GOTO 15000
      ENDIF
15001 CONTINUE
      HPFREE=HPFRE0
      ENDIF
      NDIFF = IPTRBK - IPPATR - IDATA(IPPATR)
      CALL BCHM(IPPATR,NDIFF,IRET)
      IDATA(IPPATR+2) = ITRBK
      IF(
     - IMCERT.EQ.0 .AND. ITRBK.GT.0
     -)THEN
      ASSIGN 17003 TO IZZZ02
      GOTO 17002
17003 CONTINUE
        IDATA(IPPATR+4)=LBEVTR
        IDATA(IPPATR+ 6) = NHITUC
      ENDIF
      ITR = 0
15002 CONTINUE
      IF(
     - ITR.LT.NTR
     -)THEN
      ITR = ITR + 1
        NELM = HNREL(ITR)
      IF(
     - NELM.LE.0
     -)THEN
          NBYTE = (NTR-ITR)*2
          NTR = NTR - 1
      IF(
     - NBYTE.GT.0
     -)THEN
            CALL MVCL2(HNREL(ITR),0,HNREL(ITR+1),0,NBYTE) !PMF 28/06/99 MVCL -> MVCL2
            NBYTE = NBYTE * 9
            CALL MVCL2(HISTR(1,ITR),0,HISTR(1,ITR+1),0,NBYTE) !PMF 28/06/99 MVCL -> MVCL2
      ENDIF
          ITR = ITR - 1
      ENDIF
      GOTO 15002
      ENDIF
15003 CONTINUE
      RETURN
17000 CONTINUE
      JP     = HPTR0
      LBTRCK = 0
      IR1=0
      IR2=0
      IR3=0
      NTRKEL=HNREL(ITR)
      DO 13004 ITN=1,NTRKEL
        ITH=HISTR(ITN,ITR)
        ITH=IABS(ITH)
        IF(LAND(LBL(ITH),MSKDSP).NE.0)LBTRCK=LOR(LBTRCK,2048)
        ITH=IPCL(ITH)
      IF(
     - ITH.LE.24
     -)THEN
          IR1=1
      ELSE
      IF(
     - ITH.GT.48
     -)THEN
            IR3=1
      ELSE
            IR2=1
      ENDIF
      ENDIF
13004 CONTINUE
13005 CONTINUE
      IF(IR1.EQ.0)LBTRCK=LOR(LBTRCK,512)
      IF(IR1.NE.0.AND.IR3.NE.0.AND.IR2.EQ.0) LBTRCK=LOR(LBTRCK,1024)
      IF(CRV.GT..002) LBTRCK=LOR(LBTRCK,64)
      TGTH       = WRK(JP+29)
      Z0         = WRK(JP+30)
      CSTH       = 1. / SQRT(TGTH**2+1.)
      SNTH       = CSTH * TGTH
      WRK(JP+ 6) = WRK(JP+ 6)*TGTH + Z0
      WRK(JP+13) = WRK(JP+13)*TGTH + Z0
      WRK(JP+ 7) = WRK(JP+ 7) * CSTH
      WRK(JP+ 8) = WRK(JP+ 8) * CSTH
      WRK(JP+ 9) = SNTH
      WRK(JP+14) = WRK(JP+14) * CSTH
      WRK(JP+15) = WRK(JP+15) * CSTH
      WRK(JP+16) = SNTH
      LBEVTR=LOR(LBEVTR,LBTRCK)
      HTRK(ITR)=ITRBK
      IP0 = IPTRBK + 1
      IP9 = IPTRBK + LTRBK
      DO 13006 IP = IP0,IP9
        IDATA(IP) = 0
13006 CONTINUE
13007 CONTINUE
      IDATA(IPTRBK+ 1) = ITRBK
      IDATA(IPTRBK+ 2) = IPFAST+1
      IDATA(IPTRBK+ 3) = IDAY
      IP1 = HPTR0+3
      IP9 = IP1+29
      JP  = IPTRBK + 3
      DO 13008 IP=IP1,IP9
        JP = JP + 1
        IDATA(JP) = IWRK(IP)
13008 CONTINUE
13009 CONTINUE
      IDATA(IPTRBK+47)=IWRK(HPTR0+46)
      IDATA(IPTRBK+48)=LBTRCK
      NPCLL  = 0
      ICELL0 = -1
      NHTREG = 0
      IDHTLB = IPJHTL*2-HPHL0+3
      IP     = HPHT0
16000 CONTINUE
      IF(
     - IWRK(IP+10).GE.0 .AND. IWRK(IP+10).LE.2
     -)THEN
          LBBDHT = IWRK(IP+10)
          LBBDHT=LAND(LBBDHT,3)
          LBBDHT = ISHFTL(LBBDHT,9)
          IPHTLB = IWRK(IP+ 2)
          LBHIT0 = HWRK(IPHTLB)
          LBHIT1 = HWRK(IPHTLB+1)
          ITREL = IABS(IWRK(IP+ 8))
          JTREL = NTREL(ITREL)*2
          LBHIT = ITRBK*2
          IF(IWRK(IP+8).GT.0) LBHIT = LOR(LBHIT,256)
          IPHTL1 = IPHTLB + IDHTLB
          IPHTL2 = IPHTLB + IDHTLB +1
          LBHIT1 = HDATA(IPHTL1)
          LBHIT2 = HDATA(IPHTL1+1)
          RES=WRK(IP+13)
          RES=ABS(RES)/.2
          IRES=IFIX(RES)
          IF(IRES.GT.31) IRES=31
          IRES=ISHFTL(IRES,11)
          LBHIT=LBHIT+IRES
          LBREG = 0
      IF(
     - LAND(MSKTR1,ISHFTR(LBHIT1,1)).NE.ITRBK
     ¢      .OR.LAND(LBHIT1,MKBDHT).NE.0
     -)THEN
      IF(
     - LAND(LBHIT1,MKBDHT).NE.0
     -)THEN
      IF(
     - LBBDHT.EQ.0
     -)THEN
              LBREG = 1
              LBHIT1 = LBHIT
              IF(IWRK(IP+7).EQ.0) LBHIT1 = LOR(LBHIT1,1)
      ELSE
              ITLND=LAND(LBHIT1,MKBDHT)
      IF(
     - LBBDHT.LT.ITLND
     -)THEN
                LBHIT1 = LOR(LBHIT,LBBDHT)
                LBREG = 1
                IF(IWRK(IP+7).EQ.0) LBHIT1 = LOR(LBHIT1,1)
      ENDIF
      ENDIF
      ELSE
      IF(
     - LBHIT1.EQ.0
     -)THEN
              LBHIT1 = LOR(LBHIT,LBBDHT)
              LBREG = 1
              IF(IWRK(IP+7).EQ.0) LBHIT1 = LOR(LBHIT1,1)
      ELSE
      IF(
     - LBBDHT.EQ.0 .AND. LBHIT2.EQ.0
     -)THEN
                LBHIT2 = LOR(LBHIT,LBBDHT)
                LBREG = 1
                IF(IWRK(IP+7).EQ.0) LBHIT2 = LOR(LBHIT2,1)
      ENDIF
      ENDIF
      ENDIF
          HDATA(IPHTL1) = LBHIT1
          HDATA(IPHTL2) = LBHIT2
          NHTREG = NHTREG + 1
          ICELL = IWRK(IP+9)
      IF(
     - LBREG.NE.0
     -)THEN
      IF(
     - ICELL.EQ.ICELL0
     -)THEN
              NCLLA(JPCLL) = NCLLA(JPCLL) + 1
      ELSE
              ICELL0 = ICELL
              JPCLL = 0
      IF(
     - NPCLL.GT.1
     -)THEN
      DO 13010 I1=1,NPCLL
                  IF(ICELL.EQ.JCLLA(I1)) JPCLL = I1
13010 CONTINUE
13011 CONTINUE
      ENDIF
      IF(
     - JPCLL.EQ.0
     -)THEN
                NPCLL        = NPCLL + 1
                JPCLL        = NPCLL
                JCLLA(JPCLL) = ICELL
                NCLLA(JPCLL) = 1
      ELSE
                NCLLA(JPCLL) = NCLLA(JPCLL) + 1
      ENDIF
      ENDIF
      ENDIF
      ENDIF
      ENDIF
      IP = IP + HLDHT
      IF(.NOT.(
     - IP.GT.HPHT9
     -))GOTO 16000
16001 CONTINUE
15004 CONTINUE
      IF(
     - NPCLL.GT.6
     -)THEN
        NHTMIN = 99999
      DO 13012 I1=1,NPCLL
      IF(
     - NCLLA(I1).LT.NHTMIN
     -)THEN
            NHTMIN = NCLLA(I1)
            JPCLL = I1
      ENDIF
13012 CONTINUE
13013 CONTINUE
        JCLLA(JPCLL) = JCLLA(NPCLL)
        NCLLA(JPCLL) = NCLLA(NPCLL)
        NPCLL = NPCLL - 1
      GOTO 15004
      ENDIF
15005 CONTINUE
      IPCLL  = IPTRBK + 33
      DO 13014 I1 = 1,NPCLL
        IPCLL  = IPCLL + 1
        IDATA(IPCLL) = JCLLA(I1)
13014 CONTINUE
13015 CONTINUE
      IPTRBK = IPTRBK + LTRBK
      GOTO IZZZ01
17002 CONTINUE
      NTR1  = ITRBK
      LPATR = IDATA(IPPATR)
      IPHL0 = IPJHTL*2 + 3
      ILDHL = IDATA(IPJHTL)*2 - 2
      IPHL9 = ILDHL + IPHL0 - 1
      CALL SETSL(NCNT1(1),0,1016,0)
      NHITUC = 0
      DO 13016 I=IPHL0,IPHL9,2
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
      ELSE
          NHITUC = NHITUC + 1
      ENDIF
13016 CONTINUE
13017 CONTINUE
      MTR = 0
      LTRBK = IDATA(IPPATR+3)
      IPTR0 = IPPATR + IDATA(IPPATR+1)
      IPTR9 = (NTR1-1)*LTRBK + IPTR0
      DO 13018 ITR=1,NTR1
      IF(
     - NCNT1(ITR).GE.5 .AND. NCNT1(ITR)+NCNT2(ITR).LT.LMPATR(1)
     -)THEN
          IPTR1 = 0
          JTR = MTR + 1
      DO 13020 IP=IPTR0,IPTR9,LTRBK
      IF(
     - IDATA(IP+1).EQ.JTR
     -)THEN
              IPTR1 = IP
      GOTO 13021
      ENDIF
13020 CONTINUE
13021 CONTINUE
          IF(IPTR1.NE.0 .AND.ABS(ADATA(IPTR1+14)).LT.1000.)NCNT1(ITR)=1
      ENDIF
      IF(
     - NCNT1(ITR).LT.5
     -)THEN
      ASSIGN 17005 TO IZZZ03
      GOTO 17004
17005 CONTINUE
          NCNT1(ITR) =-NCNT1(ITR)
          JTR = IXREF0(ITR)
          HNREL(JTR) = 0
      ELSE
          MTR = MTR + 1
      ENDIF
13018 CONTINUE
13019 CONTINUE
      IDATA(IPPATR+2) = MTR
      LENG  = IDATA(IPPATR+2)*IDATA(IPPATR+3) + IDATA(IPPATR+1)
      NDIFF = LENG - IDATA(IPPATR)
      IF(NDIFF.NE.0) CALL BCHM(IPPATR,NDIFF,IRET)
      GOTO IZZZ02
17004 CONTINUE
        JTR = MTR + 1
        ITRDIF = ITR - JTR
      DO 13022 I=IPHL0,IPHL9,2
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
13022 CONTINUE
13023 CONTINUE
      DO 13024 IP=IPTR0,IPTR9,LTRBK
      IF(
     - IDATA(IP+1).EQ.JTR
     -)THEN
            IPTR1 = IP
      GOTO 13025
      ENDIF
13024 CONTINUE
13025 CONTINUE
      IF(
     - IPTR1.GT.0
     -)THEN
          IPTR2 = IPTR1 + LTRBK
          NBYTE = (IPTR9 -IPTR2 + LTRBK) * 4
          IPTR9 = IPTR9 - LTRBK
      IF(
     - NBYTE.GT.0
     -)THEN
            CALL MVCL(IDATA(IPTR1+1),0,IDATA(IPTR2+1),0,NBYTE)
      DO 13026 IP=IPTR1,IPTR9,LTRBK
              IDATA(IP+1) = IDATA(IP+1) - 1
13026 CONTINUE
13027 CONTINUE
      ENDIF
      ENDIF
      GOTO IZZZ03
      END
