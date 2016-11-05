      SUBROUTINE STTRKO(ILAYMX)
      IMPLICIT INTEGER*2 (H)
      LOGICAL TBIT
      COMMON/CHEADR/HEAD(17),HRUN,HEV
      COMMON/CADMIN/IEVTP,NREC,NRWRIT,NRERR
#include "cdata.for"
#include "ccycp.for"
#include "cjdrch.for"
#include "cworkpr.for"
#include "cworkeq.for"
      EQUIVALENCE
     ,          (HPCO0 ,HPWRK(17)),(HPCO9 ,HPWRK(18)),(HLDCO ,HPWRK(19))
     ,         ,(ICELL ,IDWRK( 1)),(MHIT  ,IDWRK( 2)),(IRING ,IDWRK( 3))
     ,         ,(IERRCD,IDWRK( 4)),(NTRKEL,IDWRK( 5))
      INTEGER DATE(5), IDAY /0/
      DATA MAXTRK/ 50/
      DATA MKBDHT /Z600/
      DATA LBINIT /0/
      IF(
     - LBINIT.EQ.0
     -)THEN
        LBINIT=1
        IQJHTL = IBLN('JHTL')
        IQPATR = IBLN('PATR')
      ENDIF
      IF(
     - IDAY.EQ.0
     -)THEN
        CALL DAY2(DATE)
        IDAY = DATE(1)*1000 + DATE(2)
      ENDIF
      IPJHTL = IDATA(IQJHTL)*2 + 2
      IPPATR = IDATA(IQPATR)
      ITRBK = IDATA(IPPATR+2)
      LTRBK = IDATA(IPPATR+3)
      IPTRBK = IPPATR + IDATA(IPPATR+1) + ITRBK*LTRBK
      IF(IPTRBK+LTRBK-IPPATR.GT.IDATA(IPPATR)) RETURN
      DO 13000 IP=HPHT0,HPHT9,HLDHT
        IWRK(IP+10) = IWRK(IP+ 7)
        IWRK(IP+ 7) = 0
        IF(IWRK(IP+10).GT.1) IWRK(IP+7) = 16
        WRK (IP+ 6) = ((WRK(IP+4)/WRK(IP+3))**2 *.5 + 1.) * WRK(IP+3)
        IP0 = IWRK(IP+1)
        IAMPL = HDATA(IP0+1)
        IAMPR = HDATA(IP0+2)
      IF(
     - IAMPR.LE.0.OR.IAMPL.LE.0
     -)THEN
          WRK (IP+5) = 0.
          IWRK(IP+7) = 16
      ELSE
          Z1 = IAMPR + IAMPL
          WRK(IP+5) = FLOAT(IAMPR-IAMPL) * ZAL*.5 / Z1
      ENDIF
13000 CONTINUE
13001 CONTINUE
      CALL ZRFIT
      ZLYMX = 1400.
      IF(
     - ILAYMX.LT.40
     -)THEN
        IZW = ILAYMX - 1 + 3
        JRING = IZW / 16
        ILAY  =-JRING*16 + IZW
        RMAX  = ILAY*RINCR(JRING+1) + FSENSW(JRING+1)
        ZLYMX = RMAX*WRK(HPTR0+29) + WRK(HPTR0+30)
      ENDIF
      IF(
     - ABS(ZLYMX)+ 50. .LT. ZMAX
     -)THEN
        RETURN
      ENDIF
      ASSIGN 17001 TO IZZZ01
      GOTO 17000
17001 CONTINUE
      IDATA(IPPATR+2) = ITRBK
      RETURN
17000 CONTINUE
      JP = HPTR0
      IWRK(HPTR0+47)=LOR(IWRK(HPTR0+47),512)
      WRK(JP+ 6) = WRK(JP+ 6)*WRK(JP+29) + WRK(JP+30)
      WRK(JP+13) = WRK(JP+13)*WRK(JP+29) + WRK(JP+30)
      WRK(JP+ 9)= WRK(JP+ 9)*WRK(JP+29)
      WRK(JP+16)= WRK(JP+16)*WRK(JP+29)
      ALGINV = 1./SQRT(WRK(JP+ 7)**2+WRK(JP+ 8)**2+WRK(JP+ 9)**2)
      WRK(JP+ 7) = WRK(JP+ 7) * ALGINV
      WRK(JP+ 8) = WRK(JP+ 8) * ALGINV
      WRK(JP+ 9) = WRK(JP+ 9) * ALGINV
      ALGINV = 1./SQRT(WRK(JP+14)**2+WRK(JP+15)**2+WRK(JP+16)**2)
      WRK(JP+14) = WRK(JP+14) * ALGINV
      WRK(JP+15) = WRK(JP+15) * ALGINV
      WRK(JP+16) = WRK(JP+16) * ALGINV
      ITRBK = ITRBK + 1
      IP0 = IPTRBK + 1
      IP9 = IPTRBK + LTRBK
      DO 13002 IP = IP0,IP9
        IDATA(IP) = 0
13002 CONTINUE
13003 CONTINUE
      IDATA(IPTRBK+ 1) = ITRBK
      IDATA(IPTRBK+ 2) = 16
      IDATA(IPTRBK+ 3) = IDAY
      IP1 = HPTR0+3
      IP9 = IP1+29
      JP  = IPTRBK + 3
      DO 13004 IP=IP1,IP9
        JP = JP + 1
        IDATA(JP) = IWRK(IP)
13004 CONTINUE
13005 CONTINUE
      IDATA(IPTRBK+47)=IWRK(HPTR0+46)
      IDATA(IPTRBK+48)=IWRK(HPTR0+47)
      IPCLL = IPTRBK + 33
      IPCLLM = IPCLL + 6
      JCELL0 = -1
      IF(
     - ITRBK.LE.MAXTRK
     -)THEN
      IPJET0 = HPTSEC(1)
      DO 13006 IP=HPHT0,HPHT9,HLDHT
        LBBDHT = IWRK(IP+10)
      IF(
     - LBBDHT.LE.1
     -)THEN
          LBBDHT = ISHFTL(LBBDHT,9)
          IPHTLB = ISHFTR((IWRK(IP+1)-IPJET0),1) + IPJHTL
          LBHIT = ITRBK*2
          IF(IWRK(IP+2).GT.0) LBHIT = LOR(LBHIT,256)
          LBHIT1 = HDATA(IPHTLB+1)
          LBHIT2 = HDATA(IPHTLB+2)
          IRES = ABS(WRK(IP+13)) * 5.
          IF(IRES.GT.31) IRES=31
          IRES=ISHFTL(IRES,11)
          LBHIT=LBHIT+IRES
      IF(
     - LAND(LBHIT1,MKBDHT).NE.0
     -)THEN
      IF(
     - LBBDHT.EQ.0
     -)THEN
              LBHIT1 = LBHIT
              LBHIT1 = LOR(LBHIT1,1)
              LBHIT2 = 0
      ELSE
              LBHIT1 = LOR(LBHIT1,1)
              LBHIT  = LOR(LBHIT ,1)
              IF(LBHIT2.EQ.0) LBHIT2 = LOR(LBHIT,LBBDHT)
      ENDIF
      ELSE
      IF(
     - LBHIT1.EQ.0
     -)THEN
              LBHIT1 = LOR(LBHIT,LBBDHT)
              IF(IWRK(IP+7).EQ.0) LBHIT1 = LOR(LBHIT1,1)
      ELSE
      IF(
     - LBBDHT.EQ.0
     -)THEN
                LBHIT2 = LOR(LBHIT,LBBDHT)
                LBHIT1 = LOR(LBHIT1,1)
                LBHIT2 = LOR(LBHIT2,1)
      ENDIF
      ENDIF
      ENDIF
          HDATA(IPHTLB+1) = LBHIT1
          HDATA(IPHTLB+2) = LBHIT2
          JCELL = IWRK(IP+9)
      IF(
     - JCELL.NE.JCELL0
     -)THEN
            JCELL0 = JCELL
            IPCLL  = IPCLL + 1
            IF(IPCLL.GT.IPCLLM) IPCLL = IPCLLM
            IDATA(IPCLL) = JCELL0
      ENDIF
      ENDIF
13006 CONTINUE
13007 CONTINUE
      DATA NPR /0/
      NPR = NPR + 1
      I0 = IPTRBK + 1
      I9 = IPTRBK + LTRBK
      ENDIF
      IPTRBK = IPTRBK + LTRBK
      GOTO IZZZ01
      END
