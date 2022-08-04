      SUBROUTINE TRCK82(LBACC,LBTRG1,LBTRG2,IERRLG) 
      IMPLICIT INTEGER*2 (H)
C PMF 03.11.98 
      LOGICAL TBIT
      REAL LBACC
      REAL AND,OR
      EXTERNAL AND,OR
#include "cutsr1.for"
      COMMON /CREDRS/ ACCB2,ACCB4,ACCB8,ACCT2M
      COMMON /CWORK/ SUM12(12),SUMWRD(12)
      COMMON /CRESR1/ ETOT,EBAR,EC1,EC2,E7MIN,
     ,               JEMPTY,UNBAL,IACCLG
#include "cdata.for"
#include "cheadr.for"
      EQUIVALENCE (HHEADR(18),HRUN) , (HHEADR(19),HEV)
      COMMON /CWORK/ HCLWR(96)
      DATA MKLUMI /    1/, MKEHIG /   2/, MKTAGE /   4/, MKMUT1 /   8/
      DATA MKEC12 / Z'100'/, MKBEC  /Z'200'/,MKTAGB/Z'400'/,MKEB/Z'800'/
      DATA MKET12 /Z'1000'/, MKOLS /Z'2000'/,MKRAN  /Z'8000'/
      DATA MKTAG  / Z'404'/, MKENGY / Z'1F06'/
      DATA MKUNT1 /Z'40F0'/
      DATA MKMUT2 /  Z'80'/
      DATA MKTR1  /    4/, MKTR2  /   1/, MKTR3  /   2/, MKTR2C /    8/
      DATA MKAT1  / Z'F00'/,MKAT2/Z'3F'/, MKAT3/Z'C0'/,MKAT2C/Z'F000'/
      INTEGER MKATOF(4) /0,    0, Z'B037', Z'B037'/
      INTEGER MKATBG(4) /0, Z'300', Z'4308', Z'4348'/
      INTEGER MKTRBT( 7) /1,2,4,8,16,32,64/
      INTEGER NCTOF ( 7) /2,3,3,3,3,3,3/
      INTEGER ICTOF ( 7) /0,1,2,3,4,5,6/
      DATA LBINIT /0/,  NLUMI /16/, ILUMI /0/
 2001 FORMAT(' TRCK82: NO TRIG-1 BANK FOR EVENT',2I6)
 2002 FORMAT(' TRCK82: EVENT',2I6,3(2X,Z4),6I6)
 2009 FORMAT(' TRCK82: EVENT',2I6,10(2X,Z4))
 2003   FORMAT('0EROW:',20I6,/,(6X,20I6))
 2004 FORMAT(' TRCK82: NO BEAM ENERGY FOR EVENT',2I6,'EBEAM =',F8.0,
     ,       ' USE EBEAM = 17 000 GEV INSTEAD  *********************')
 2005 FORMAT(' JETC:',20I6)
 2006 FORMAT('0HCLWR:',12I6,/,(7X,12I6))
 2007 FORMAT(' TRCK82: UNKNOWN TRIGGER FOR EVENT',2I6,3(2X,Z4),
     ,       2X,8('**'))
 2008 FORMAT('0LGCT82:',2I6,2(/,1X,12F8.0),/,1X,5F8.0,I8,F8.3,I8)
      LBACC  = 0
      ACCB2  = 0
      ACCB4  = 0
      ACCB8  = 0
      ACCT2M = 0
      IF(
     - LBINIT.EQ.0
     -)THEN
        LBINIT = 1
        NPRUN  = 0
        IQJETC = IBLN('JETC')
        IQTRIG = IBLN('TRIG')
        IQLATC = IBLN('LATC')
        IQALGN = IBLN('ALGN')
        EBEAM = HHEADR(37)
      IF(
     - EBEAM.LE.6000.
     -)THEN
          PRINT 2004, HRUN,HEV,EBEAM
          EBEAM = 1700.
      ENDIF
        EB35   = EBEAM * .35
        EB50   = EBEAM * .50
        EB65   = EBEAM * .65
      ENDIF
      IPTRIG = IDATA(IQTRIG)
      IF(
     - IPTRIG.LE.0 .OR. IDATA(IPTRIG-2).NE.1
     -)THEN
        PRINT 2001, HRUN,HEV
        LBTRG1 = 0
        LBTRG2 = 0
        RETURN
      ENDIF
      LBTRG1 = HDATA(IPTRIG*2+ 8)
      LBTRP1 = HDATA(IPTRIG*2+ 9)
      LBTRG2 = HDATA(IPTRIG*2+10)
      PRINT 2007, HRUN,HEV, LBTRG1,LBTRP1,LBTRG2
      I0 = IPTRIG*2 + 1
      I9 = I0 + IDATA(IPTRIG)*2 - 1
      IF(
     - AND(LBTRG1,MKUNT1).NE.0
     -)THEN
        NPRUN = NPRUN + 1
        IF(NPRUN.LE.10) PRINT 2007, HRUN,HEV, LBTRG1,LBTRP1,LBTRG2
      ENDIF
      IF(AND(LBTRG1,MKRAN).NE.0) LBACC = OR(LBACC,1)
      IF(AND(LBTRG1,MKRAN).NE.0) ACCB2 = OR(ACCB2,MKRAN)
      IF(
     - AND(LBTRG1,MKLUMI).NE.0
     -)THEN
        ILUMI = ILUMI + 1
      IF(
     - ILUMI.EQ.NLUMI
     -)THEN
          LBACC = OR(LBACC,1)
          ILUMI = 0
          ACCB2 = OR(ACCB2,MKLUMI)
      ENDIF
      ENDIF
      IF(AND(LBTRG1,MKMUT1) .NE.0) ACCB4 = OR(ACCB4,MKMUT1)
      IF(AND(LBTRG1,MKMUT1) .NE.0) LBACC = OR(LBACC,4)
      IF(
     - AND(LBTRP1,MKMUT2).NE.0
     -)THEN
        CALL MEWT3(IMUACC)
      IF(
     - IMUACC.NE.0
     -)THEN
          LBACC = OR(LBACC,4)
          ACCB8 = OR(ACCB8,MKMUT2)
      ENDIF
      ENDIF
      IACCLG = 0
      IPALGN = IDATA(IQALGN)
      IF(AND(LBTRG1,MKENGY).NE.0 .AND. IERRLG.EQ.0 .AND. IPALGN.GT.0)
     ?  CALL LGCT82(IPALGN)
      IF(
     - IACCLG.EQ.0
     -)THEN
        EBAR = 0.
        EC1  = 0.
        EC2  = 0.
        ETOT = 0.
      ELSE
        PRINT 2008, HRUN,HEV,(SUM12(I1),I1=1,24)
     ,            , ETOT,EBAR,EC1,EC2,E7MIN, ELGLM
      ENDIF
      LBCSM = 0
      IF(
     - AND(LBTRG1,MKTAG).NE.0
     -)THEN
        CALL TAGF82(IFTG)
      IF(
     - IFTG.GT.0
     -)THEN
      IF(
     - AND(LBTRG1,MKTAGB).NE.0
     -)THEN
            LBACC = OR(LBACC,2)
            ACCB2 = OR(ACCB2,MKTAGB)
      ELSE
            ECENTR = 0.
            IF(IFTG.EQ.11) ECENTR = ETOT - EC1
            IF(IFTG.EQ.12) ECENTR = ETOT - EC2
            IF(IFTG.GT.12) ECENTR = ETOT
      IF(
     - ECENTR.GT.100. .AND. ETOT.GT.ELGLM(3)
     -)THEN
              LBACC = OR(LBACC,2)
              ACCB2 = OR(ACCB2,MKTAGE)
      ENDIF
      ENDIF
      ENDIF
      ENDIF
      IF(
     - AND(LBTRG1,MKEHIG).NE.0
     -)THEN
      IF(
     - ETOT.GT.EB65
     -)THEN
          LBACC = OR(LBACC,2)
          ACCB2 = OR(ACCB2,MKEHIG)
      ELSE
      IF(
     - ETOT.GT.ELGLM( 1)
     -)THEN
          IF(JEMPTY.LE.1) LBACC = OR(LBACC,2)
          IF(JEMPTY.LE.1) ACCB2 = OR(ACCB2,MKEHIG)
      ENDIF
      ENDIF
      ENDIF
      IF(
     - AND(LBTRG1,MKEC12).NE.0
     -)THEN
        E1 = AMIN1(EC1,EC2)
      IF(
     - E1.GT.EB35
     -)THEN
          LBACC = OR(LBACC,2)
          ACCB2 = OR(ACCB2,MKEC12)
      ELSE
          IF(E1.GT.ELGLM(6)) LBACC = OR(LBACC,4)
          IF(E1.GT.ELGLM(6)) ACCB4 = OR(ACCB4,MKEC12)
      ENDIF
      ENDIF
      IF(
     - AND(LBTRG1,MKET12).NE.0
     -)THEN
        E1       = AMIN1(EC1,EC2)
      IF(
     - E1.GT.ELGLM(8) .AND. ETOT.GT.ELGLM(2)
     -)THEN
          LBACC = OR(LBACC,4)
          ACCB4 = OR(ACCB4,MKET12)
      ENDIF
      ENDIF
      IF(
     - AND(LBTRG1,MKBEC).NE.0
     -)THEN
      IF(
     -  EBAR.GT.ELGLM(4)
     -)THEN
      IF(
     - EC1.GT. ELGLM(7) .OR. EC2.GT. ELGLM(7)
     -)THEN
      IF(
     - ETOT.GT.EB65
     -)THEN
            LBACC = OR(LBACC,2)
            ACCB2 = OR(ACCB2,MKBEC)
      ELSE
            IF(JEMPTY.LE.1) LBACC = OR(LBACC,2)
            IF(JEMPTY.LE.1) ACCB2 = OR(ACCB2,MKBEC)
      ENDIF
      ENDIF
      ENDIF
      ENDIF
      IF(
     - AND(LBTRG1,MKEB).NE.0
     -)THEN
      IF(
     - EBAR.GE.ELGLM(2)
     -)THEN
          IF(LBCSM.EQ.0) CALL LGBRCS(IRBAR)
          LBCSM = 1
          IF(IRBAR.EQ.1 .AND. JEMPTY.GT.1) IRBAR =  2
          IF(IRBAR.LE.1) LBACC = OR(LBACC,2)
          IF(IRBAR.LE.1) ACCB2 = OR(ACCB2,MKEB)
      ENDIF
      ENDIF
      PRINT 2002, HRUN,HEV, LBTRG1,LBTRG2,LBACC,JEMPTY,UNBAL,IRBAR,LBCSM
      IF(AND(LBACC,2).NE.0) RETURN
      IF(AND(LBTRG2,15).EQ.0) RETURN
      IPJETC = IDATA(IQJETC)
      IF(
     - IPJETC.LE.0
     -)THEN
        PRINT 2002, HRUN,HEV, LBTRG1,LBRG2,LBACC
        RETURN
      ENDIF
      IF(IDATA(IPJETC).LE.50) RETURN
      IPJC2 = IPJETC*2
      NHTR3 = HDATA(IPJC2+99) - HDATA(IPJC2+51)
      IF(NHTR3.LE.0) RETURN
      CALL SETS(HCLWR(1),0,192,0)
      IP0 = IPJC2 + 100 + HDATA(IPJC2+51)
      IP9 = IP0 + NHTR3 - 1
      IWIR0 =-1
      DO 13000 IP=IP0,IP9,4
        IWIR = HDATA(IP)
        IWIR = ISHFTR(IWIR,3)
      IF(
     - HDATA(IP+1).GT.80 .AND. HDATA(IP+2).GT.80
     -)THEN
          HCLL = ISHFTR(IWIR,3) - 95
          IF(IWIR.NE.IWIR0) HCLWR(HCLL) = HCLWR(HCLL) + 1
      ENDIF
        IWIR0 = IWIR
13000 CONTINUE
13001 CONTINUE
      IPLATC = IDATA(IQLATC)
      IF(IPLATC.LE.0) RETURN
      IPLT2 = IPLATC*2
      IP0 = IPLT2 + 6
      IP9 = IP0 + 5
      IPTBG = IPTRIG*2 + 5
      ITBG  = 0
      ITLST =-100
      ITOF0 = 0
      ICLL0 = 0
      NTRCK = 0
      NTRBG = 0
      NTREL = 0
16000 CONTINUE
        ITRBT = HDATA(IP0)
      IF(
     - AND(ITBG,1).EQ.0.
     -)THEN
          ITRBG = HDATA(IPTBG)
          IPTBG = IPTBG + 1
          ITBG = ITBG + 1
      ELSE
          ITRBG = ISHFTR(ITRBG,8)
          ITBG = ITBG + 1
      ENDIF
      DO 13002 JTOF=1,7
      IF(
     - AND(MKTRBT(JTOF),ITRBT) .NE. 0
     -)THEN
            ITOF = ITOF0 + JTOF
            JCTOF = (ICTOF(JTOF) + ICLL0)*2 - 1
            LBTRK = 0
      IF(
     - ITOF-ITLST.LE.2
     -)THEN
              HCLL9 = NCTOF(JTOF)*2 + JCTOF - 1
              HCLL1 =-99
              MTRK = 0
      DO 13004 ICLL=HCLL0,HCLL9
      IF(
     - HCLWR(ICLL).GE.6
     -)THEN
                  HDCLL = ICLL - HCLL1
      IF(
     - HDCLL.EQ.1
     -)THEN
                    HCLL1 = -99
      ELSE
      IF(
     - HDCLL.NE.3 .OR. TBIT(ICLL,31)
     -)THEN
                      MTRK = MTRK + 1
                      HCLL1 = ICLL
      ENDIF
      ENDIF
      ENDIF
13004 CONTINUE
13005 CONTINUE
              IF(MTRK.GE.2) LBTRK = 1
      ELSE
              NHIT5 = 0
              NHIT6 = 0
      IF(
     - JCTOF.LT.0
     -)THEN
                NHIT1 = HCLWR(95)
                NHIT2 = HCLWR(96)
                NHIT3 = HCLWR( 1)
                NHIT4 = HCLWR( 2)
                IF(NHIT2.GE.3 .AND. HCLWR(93).GE.6) NHIT2 = 8
                IF(NHIT4.GE.3 .AND. HCLWR( 3).GE.6) NHIT4 = 8
      ELSE
                NHIT1 = HCLWR(JCTOF  )
                NHIT2 = HCLWR(JCTOF+1)
                NHIT3 = HCLWR(JCTOF+2)
                NHIT4 = HCLWR(JCTOF+3)
      IF(
     - NCTOF(JTOF).EQ.3
     -)THEN
                  NHIT5 = HCLWR(JCTOF+4)
                  NHIT6 = HCLWR(JCTOF+5)
      ELSE
                  IF(NHIT2.GE.3 .AND. HCLWR(JCTOF-2).GE.6) NHIT2 = 8
                  IF(NHIT4.GE.3 .AND. HCLWR(JCTOF+4).GE.6) NHIT4 = 8
      ENDIF
      ENDIF
              IF(MAX0(NHIT1,NHIT2,NHIT3,NHIT4,NHIT5,NHIT6).GE.6) LBTRK=100045400
      ENDIF
      IF(
     - LBTRK.NE.0
     -)THEN
              NTRCK = NTRCK + 1
              IF(AND(MKTRBT(JTOF),ITRBG) .NE. 0.) NTRBG = NTRBG + 1
              ITLST = ITOF
              HCLL0 = JCTOF
      ENDIF
      ENDIF
13002 CONTINUE
13003 CONTINUE
      ICLL0 = ICLL0 + 8
      ITOF0 = ITOF0 + 7
      IP0   = IP0   + 1
      IF(.NOT.(
     - IP0.GT.IP9
     -))GOTO 16000
16001 CONTINUE
      ACCT2M = 0.
      IF(AND(LBTRG2,MKTR1 ).NE.0.) ACCT2M = OR(ACCT2M,MKAT1 )
      IF(AND(LBTRG2,MKTR2 ).NE.0.) ACCT2M = OR(ACCT2M,MKAT2 )
      IF(AND(LBTRG2,MKTR3 ).NE.0.) ACCT2M = OR(ACCT2M,MKAT3 )
      IF(AND(LBTRG2,MKTR2C).NE.0.) ACCT2M = OR(ACCT2M,MKAT2C)
      ACCT2M = AND(ACCT2M,LBTRP1)
      IMK = MIN0(NTRCK,3)
      ACCTOF = AND(ACCT2M,MKATOF(IMK+1))
      IMK = MIN0(NTRBG,3)
      ACCTBG = AND(ACCT2M,MKATBG(IMK+1))
      ACCB8  = OR(ACCB8,ACCTOF)
      ACCB8  = OR(ACCB8,ACCTBG)
      IF(ACCTOF.NE.0 .OR. ACCTBG.NE.0) LBACC = OR(LBACC,8)
      PRINT 2009, HRUN,HEV,LBTRG1,LBTRG2,LBTRP1,ACCT2M,LBACC,NTRCK,NTRBG
     ,          , ACCTOF,ACCTBG
      RETURN
      END
