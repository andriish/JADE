      SUBROUTINE PATRC1(IDRENT)
      IMPLICIT INTEGER*2 (H)
      LOGICAL TBIT
#include "cheadr.for"
#include "cdata.for"
#include "ccycp.for"
#include "cworkpr.for"
#include "cworkeq.for"
      EQUIVALENCE
     ,           (ICELL ,IDWRK(1)),(NHIT  ,IDWRK(2)),(IRING ,IDWRK(3))
     ,         , (IERRCD,IDWRK(4)),(NTRKEL,IDWRK(5))
     ,         , (ITR   ,IDWRK(7)),(ITRNG ,IDWRK(8))
#include "cpatlm.for"
      COMMON /CLBPGM/ LBPGM(30)
      DIMENSION HEARR(30)
      DATA HPS /2HPS/
      DATA PATR /4HPATR/, LHEAD,LTRBK /8,48/
      DATA JHTL /4HJHTL/
      DATA JETC /4HJETC/
 2009 FORMAT('0CHANGE OF POINTERS:',10I8)
 2991 FORMAT('0/BCS/ TOO SHORT FOR TRACK BANK; EVENT:',3I6,
     ,       ', IRET=',I2,',NTR,LENGTH=',I3,1X,I4)
 2992 FORMAT('0/CWORK/ TOO SHORT FOR PATREC; EVENT:',3I6)
 2993 FORMAT('0/BCS/ TOO SHORT FOR PATREC; EVENT:',3I6,' ,IERR=',I2)
 2994 FORMAT('0WRONG POINTER IN JETC-BANK; EVENT:',3I6,' ,POINTER:',
     ,        (/,1X,24I5))
      NTR = 0
      IPJETC = IDATA(IBLN(JETC))
      IF(
     - IPJETC.LE.0
     -)THEN
        IPPATR = IDATA(IBLN(PATR))
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
      JCELL=0
16000 CONTINUE
        JCELL = JCELL + 1
        HNTCEL(JCELL) = ITR
        MHIT = (HPTSEC(JCELL+1)-HPTSEC(JCELL)) / 4
      IF(
     - MHIT.GE.5
     -)THEN
          IRING = 3
          IF(JCELL.LE.48) IRING = 2
          IF(JCELL.LE.24) IRING = 1
      IF(
     - IRING.NE.IRINGO
     -)THEN
            IRINGO = IRING
            ITRNG = 1
      ENDIF
          ICELL = JCELL
          IERRCD = 0
          HPFREE = HPFRE0
          CALL SRTREL
      ENDIF
      IF(.NOT.(
     - JCELL.EQ.96
     -))GOTO 16000
16001 CONTINUE
      HNTCEL(97) = ITR
      HNTR = ITR - 1
      HPFREE = HPFRE0
      NTR = 0
      IF(HNTR.GT.0) CALL BACKTR(0,0)
      IF(
     - NTR.GT.0
     -)THEN
          LENGTR =(NTR+10)*LTRBK
          CALL BCHM(IPPATR,LENGTR,IRET)
      IF(
     - IRET.NE.0
     -)THEN
            WRITE(6,2991) HHEADR(17),HHEADR(18),HHEADR(19),
     ,                    IRET,NTR,LENGTR
            NTR = 0
            CALL BDLS(PATR,NBNK)
            CALL BDLS(JHTL,NBNK)
            RETURN
      ENDIF
          IPJETC = IDATA(IBLN(JETC))
          IP0 = IPJETC*2 + 101
      IF(
     - IP0.NE.HPTSEC(1)
     -)THEN
            PRINT 2009, IPJETC,IP0,HPTSEC(1)
      ASSIGN 17006 TO IZZZ02
      GOTO 17002
17006 CONTINUE
      ENDIF
      ENDIF
      RETURN
17000 CONTINUE
        IPJHTL = IDATA(IBLN(JHTL))
        IPPATR = IDATA(IBLN(PATR))
        NBKPAT = 10
        IF(IPPATR.GT.0) NBKPAT = IDATA(IPPATR-2) - 1
        NBKHTL = 10
        IF(IPJHTL.GT.0) NBKHTL = IDATA(IPJHTL-2) - 1
        NBNK = MIN0(10,NBKPAT,NBKHTL)
      IF(
     - IPJHTL.EQ.0 .OR. IDRENT.EQ.0
     -)THEN
          IZW = NHITJC + 1
          CALL CCRE(IPJHTL,JHTL,NBNK,IZW,IERR)
          LBRHTL = 0
          IF(IERR.NE.0) LBRHTL = 1
      ENDIF
      IF(
     - IPPATR.EQ.0 .OR. IDRENT.EQ.0
     -)THEN
          IZW = LHEAD
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
      ELSE
          CALL CMVE(IPPATR,IERR)
      ENDIF
      GOTO IZZZ01
17002 CONTINUE
      IPJCA2 = IPJETC*2 + 2
      IP0 = IPJCA2 + 98
      IERRFL = 0
      IPCLL = IPJCA2
      DO 13000 ICLL=1,96
        IPCLL = IPCLL + 1
        HPTSEC(ICLL) = HDATA(IPCLL) + IP0
        IF(HDATA(IPCLL+1).LT.HDATA(IPCLL)) IERRFL = 1
13000 CONTINUE
13001 CONTINUE
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
