      SUBROUTINE PATRCO(IND)
      IMPLICIT INTEGER*2 (H)
#include "cheadr.for"
#include "cdata.for"
#include "ccycp.for"
#include "cjdrch.for"
#include "cdsmax.for"
#include "cpatlm.for"
#include "cworkpr.for"
#include "cworkeq.for"
      EQUIVALENCE
     ,           (ICELL ,IDWRK(1)),(NHIT  ,IDWRK(2)),(IRING ,IDWRK(3))
     ,         , (IERRCD,IDWRK(4)),(NTRKEL,IDWRK(5))
     ,         , (ITR   ,IDWRK(7)),(ITRNG ,IDWRK(8))
      COMMON /CLBPGM/ LBPGM(30)
      INTEGER MSKHIT(16) /1,2,4,8,16,32,64,128,Z100,Z200,Z400,Z800,
     ,                    Z1000,Z2000,Z4000,Z8000/
      DATA HPS /2HPS/
      DATA PATR /'PATR'/, LHEAD,LTRBK /8,48/
      DATA JHTL /'JHTL'/
 2991 FORMAT('0/BCS/ TOO SHORT FOR TRACK BANK; EVENT:',3I6,
     ,       ', IRET=',I2,',NTR,LENGTH=',I3,1X,I4)
 2992 FORMAT('0/CWORK/ TOO SHORT FOR PATREC; EVENT:',3I6)
 2993 FORMAT('0/BCS/ TOO SHORT FOR PATREC; EVENT:',3I6,' ,IERR=',I2)
 2994 FORMAT('0WRONG POINTER IN JETC-BANK; EVENT:',3I6,' ,POINTER:',
     ,        /,(1X,24I5))
      IF(
     - IND.EQ.2
     -)THEN
        CALL PATREC(0)
        RETURN
      ENDIF
      DATA LBINIT /0/
      IF(
     - LBINIT .EQ. 0
     -)THEN
        LBINIT = 0
        IQPATR = IBLN('PATR')
        IQJHTL = IBLN('JHTL')
        IQJETC = IBLN('JETC')
        DSD0   =-.63
        DSD1   = 1.8
        DSD2   = 4.0
        DRV0   = 0.8
        DRV1   = (DSD2 - (DSD1-DSD0)*DRV0) / (DSD2-DSD1)
      ENDIF
      NTR = 0
      IPJETC = IDATA(IQJETC)
      IF(
     - IPJETC.LE.0
     -)THEN
        IPPATR = IDATA(IQPATR)
      IF(
     - IPPATR.LE.0
     -)THEN
          NBNK = 10
          IZW = LHEAD
          CALL CCRE(IPPATR,PATR,NBNK,IZW,IERR)
          IF(IERR.NE.0) RETURN
          CALL BSAW(1,PATR)
          IDATA(IPPATR+1) = LHEAD
          IDATA(IPPATR+3) = LTRBK
      ENDIF
        RETURN
      ENDIF
      NHITJC = HDATA(IPJETC*2+99) / 4
      ASSIGN 17001 TO IZZZ01
      GOTO 17000
17001 CONTINUE
      HPFREE = 1
      HPLAST = LMPATR(5)
      ASSIGN 17003 TO IZZZ02
      GOTO 17002
17003 CONTINUE
      IF(
     - IERRFL.NE.0
     -)THEN
        I1 = IPJCA2 + 1
        I9 = I1 + 97
        WRITE(6,2994) HHEADR(17),HHEADR(18),HHEADR(19),
     ,                (HDATA(I),I=I1,I9)
        CALL BDLS(PATR,NBNK)
        CALL BDLS(JHTL,NBNK)
        RETURN
      ENDIF
      IF(
     - (HPFREE+NHITJC+96*12+10*17 - 1).LT.LMPATR(5)
     -)THEN
      ASSIGN 17005 TO IZZZ03
      GOTO 17004
17005 CONTINUE
      ELSE
        WRITE(6,2992) HHEADR(17),HHEADR(18),HHEADR(19)
        CALL BDLS(PATR,NBNK)
        CALL BDLS(JHTL,NBNK)
        RETURN
      ENDIF
      HPFRE0 = HPFREE
      HPRO = HPS
      ITR = 1
      IRINGO = 0
      JCELL=97
16000 CONTINUE
        JCELL = JCELL - 1
        MHIT = (HPTSEC(JCELL+1)-HPTSEC(JCELL)) / 4
      IF(
     - MHIT.GE.5
     -)THEN
          IRING = 3
          IF(JCELL.LE.48) IRING = 2
          IF(JCELL.LE.24) IRING = 1
          ICELL = JCELL
          IERRCD = 0
          HPFREE = HPFRE0
      ASSIGN 17007 TO IZZZ04
      GOTO 17006
17007 CONTINUE
      ENDIF
      IF(.NOT.(
     - JCELL.EQ.25
     -))GOTO 16000
16001 CONTINUE
      HPFREE = HPFRE0
      LENG  = IDATA(IPPATR+2)*IDATA(IPPATR+3) + IDATA(IPPATR+1)
      NDIFF = LENG - IDATA(IPPATR)
      CALL BCHM(IPPATR,NDIFF,IRET)
      IF(IND.NE.0) CALL PATREC(1)
      RETURN
17006 CONTINUE
        DSBIN1 = TIMDEL(1,IRING)
        DSBIN2 = TIMDEL(2,IRING)
        IP0 = HPTSEC(ICELL)
        IP9 = HPTSEC(ICELL+1) - 1
        IPHL = (HPTSEC(ICELL)-HPTSEC(1))/4 + IDATA(IQJHTL) + 1
        HPHT0 = HPFREE
        HLDHT = 12
        IPHT  = HPHT0
        LBHITW = 0
        NWRD2 = NWORD*2
        ILAY0 = -10
        MLAY  = 0
        JHIT  = 0
      DO 13000 IP=IP0,IP9,4
          IPHL = IPHL + 1
      IF(
     - IDATA(IPHL).EQ.0
     -)THEN
      IF(
     - HDATA(IP+1).GT.0 .AND. HDATA(IP+2).GT.0
     -)THEN
            IWIR = HDATA(IP)
            IWIR = ISHFTR(IWIR,3)
            ILAYR = LAND(IWIR,15)
            LBHITW = LOR(LBHITW,MSKHIT(ILAYR+1))
            IF(ILAYR.GE.8) DSBIN1 = DSBIN2
            DS =(HDATA(IP+3)) * DSBIN1
            DS = DS
      IF(
     - DS.LT.4.0
     -)THEN
      IF(
     - DS.GT.DSD1
     -)THEN
                DS = (DSD1-DSD0)*DRV0 + (DS-DSD1)*DRV1
      ELSE
                DS = (DS-DSD0)*DRV0
      ENDIF
              IF(DS.LT.0.1) DS = 0.1
      ENDIF
            WRK(IPHT+ 2)  = DS
            IWRK(IPHT  ) = ILAYR
            IWRK(IPHT+ 1) = IP
            IWRK(IPHT+ 3) = 0
            IWRK(IPHT+ 4) = 0
            IWRK(IPHT+ 5) = 0
            IWRK(IPHT+ 6) = 0
            IWRK(IPHT+ 7) = 0
            IWRK(IPHT+ 8) = 0
            IWRK(IPHT+ 9) = 0
            IWRK(IPHT+10) = 0
            IWRK(IPHT+11) = 0
            IPHT = IPHT + HLDHT
            JHIT = JHIT + 1
            IF(ILAYR.NE.ILAY0) MLAY = MLAY + 1
            ILAY0 = ILAYR
      ENDIF
      ENDIF
13000 CONTINUE
13001 CONTINUE
        NHIT = JHIT
        HPHT9 = IPHT - 1
        HPFREE= IPHT
      IF(
     - MLAY.GE.5
     -)THEN
          LBHT = LBHITW
          LBAD = 0
      DO 13002 I=1,14
      IF(
     - LAND(LBHT,7).EQ.7
     -)THEN
              LBAD = 1
      GOTO 13003
      ENDIF
            LBHT = ISHFTR(LBHT,1)
13002 CONTINUE
13003 CONTINUE
      IF(
     - LBAD.NE.0
     -)THEN
            NTRKEL = 0
            CALL FTRELO
      ENDIF
      ENDIF
        HPFREE = HPFRE0
      GOTO IZZZ04
17000 CONTINUE
        IPJHTL = IDATA(IQJHTL)
        IPPATR = IDATA(IQPATR)
        NBKPAT = 10
        IF(IPPATR.GT.0) NBKPAT = IDATA(IPPATR-2) - 1
        NBKHTL = 10
        IF(IPJHTL.GT.0) NBKHTL = IDATA(IPJHTL-2) - 1
        NBNK = MIN0(10,NBKPAT,NBKHTL)
          IZW = NHITJC + 1
          CALL CCRE(IPJHTL,JHTL,NBNK,IZW,IERR)
          LBRHTL = 0
          IF(IERR.NE.0) LBRHTL = 1
          IZW = LTRBK*30 + LHEAD
          CALL CCRE(IPPATR,PATR,NBNK,IZW,IERR)
      IF(
     - IERR.NE.0 .OR. LBRHTL.NE.0
     -)THEN
            WRITE(6,2993) HHEADR(17),HHEADR(18),HHEADR(19),IERR
            CALL BDLS(PATR,NBNK)
            CALL BDLS(JHTL,NBNK)
            RETURN
      ENDIF
          CALL BSAW(1,PATR)
          CALL BSAW(1,JHTL)
          IDATA(IPPATR+1) = LHEAD
          IDATA(IPPATR+2) = 0
          IDATA(IPPATR+3) = LTRBK
          IDATA(IPPATR+4) = IDATA(IPJHTL-2)
          IDATA(IPPATR+5) = NHITJC
          IDATA(IPPATR+6) = NHITJC
          IDATA(IPPATR+7) = NHITJC
          IDATA(IPPATR+8) = 0
      GOTO IZZZ01
17002 CONTINUE
      IPJCA2 = IPJETC*2 + 2
      IP0 = IPJCA2 + 98
      IERRFL = 0
      IPCLL = IPJCA2
      DO 13004 ICLL=1,96
        IPCLL = IPCLL + 1
        HPTSEC(ICLL) = HDATA(IPCLL) + IP0
        IF(HDATA(IPCLL+1).LT.HDATA(IPCLL)) IERRFL = 1
13004 CONTINUE
13005 CONTINUE
      HPTSEC(97) = HDATA(IPCLL+1) + IP0
      HPTSEC(98) = 0
      GOTO IZZZ02
17004 CONTINUE
      HPHL0 = HPFREE*2 - 1
      HLDHL = NHITJC*2
      HPHL9 = HPHL0 + HLDHL - 1
      NBYTHT = HLDHL*2
      ZERO = 0
      CALL SETSL(HWRK(HPHL0),0,NBYTHT,ZERO)
      HPFREE = HPFREE + NHITJC
      GOTO IZZZ03
      END
