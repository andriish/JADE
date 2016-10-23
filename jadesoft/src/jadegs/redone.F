      SUBROUTINE REDONE (INDREJ,LBWRT,IWRT)
      IMPLICIT INTEGER*2 (H)
#include "cdata.for"
      COMMON /CREDON/LIMHIT,LIMHT1,CRVTAG,CRVNTG
      COMMON /CIPRI/ IPRI
      COMMON /CHEADR/ HEAD(108)
      EQUIVALENCE (HRUN,HEAD(18)),(HEVENT,HEAD(19))
      DATA ETAGLM /300./
      DATA MKTAGE /Z400/, MKLGLE /Z200/, MKLUMI /Z100/, MKFWMU/Z800/
      DATA MKMUCN /Z00F/
        IQHEAD = IBLN('HEAD')
        IQTRIG = IBLN('TRIG')
        IQALGN = IBLN('ALGN')
        IQZVTX = IBLN('ZVTX')
        IQJETC = IBLN('JETC')
        IQPATR = IBLN('PATR')
        IQJHTL = IBLN('JHTL')
      IPHEAD=IDATA(IQHEAD)
      CALL MVCL(HEAD(1),0,IDATA(IPHEAD-3),0,216)
      INDREJ = 0
      LBWRT = 0
        IWRT = 0
        IFLW= 0
        IF(HEAD(23).NE.0) IFLW = 1
        LBTRBT = 0
        IPTRIG = IDATA(IQTRIG)
        IF(IPTRIG.GT.0 .AND. IDATA(IPTRIG-2).EQ.1)
     ?  LBTRBT = HDATA(IPTRIG*2+10)
        ILUMI = 0
        IF(LAND(LBTRBT,MKLUMI).NE.0) ILUMI = 1
        IMUACC=0
        IF(LAND(LBTRBT,MKFWMU).NE.0) CALL MEWT3(IMUACC)
      IF(
     - IMUACC.LE.0
     -)THEN
        CALL TRGCHK(LBTRCK,LBTRBT)
        INDREJ =  1
        IF(IPRI.GT.0.AND.LBTRCK.EQ.0) WRITE(6,4810)
4810  FORMAT(' REJECTED BY TRIGGER CHECK  ')
        IF(LBTRCK.EQ.0) RETURN
        IACC = 0
        CALL LGCUTX(IACC,ECYL,ECAMI,ECAPL,ETOT)
        IF(LAND(LBTRBT,MKLGLE).EQ.0) IACC = 0
        IFTG = 0
        IF(LAND(LBTRBT,2).NE.0) CALL TAGFLG(IFTG)
        ETAG = 0.
        IF(IFTG.NE.0. .AND. LAND(LBTRBT,MKTAGE).NE.0) ETAG = ETOT
        IF(IACC.NE.     0) IWRT = 1
        IF(IFLW.NE.     0) IWRT = IWRT + 2
      IF(
     - ETAG.GT.ETAGLM .AND. IWRT.EQ.0
     -)THEN
          ECENTR = 0.
          IF(IFTG.EQ.11) ECENTR = ETOT - ECAMI
          IF(IFTG.EQ.12) ECENTR = ETOT - ECAPL
          IF(IFTG.GT.12) ECENTR = ETOT
          IF(ECENTR.GT.100.) IWRT = IWRT + 4
      ENDIF
        INDREJ =  2
        IF(IPRI.GT.0.AND.LBTRCK.EQ.16.AND.IWRT.EQ.0) WRITE(6,4811)
4811  FORMAT(' REJECTED BY TRIGGER CHECK, WITH IWRT = 0 ,NO TRACKS ')
        IF(IWRT.EQ.0 .AND. LBTRCK.EQ.16) RETURN
      ELSE
        IWRT=1
        LBWRT=16
        INDREJ = 0
        IF(IPRI.GT.0) WRITE(6,4521)
4521  FORMAT(' ACCEPTED AS FW MU TRIGGER WITH MU TRACKS ')
        RETURN
      ENDIF
        IPJCA  = IDATA(IQJETC)
      IF(
     - IPJCA.LE.0
     -)THEN
          INDREJ =  3
        IF(IPRI.GT.0.AND.IWRT.EQ.0) WRITE(6,4812)
4812  FORMAT(' NO HITS IN INNER DET.,WITH IWRT = 0  REJECTED ')
          IF(IWRT.EQ.0) RETURN
          INDREJ = 0
          LBWRT =  1
        IF(IPRI.GT.0) WRITE(6,4813)
4813  FORMAT(' NO HITS IN INNER DET.,WITH IWRT = 1  ACCEPTED ')
          RETURN
      ENDIF
        INDREJ = 0
        LBWRT = 2
        IF(IPRI.GT.0.AND.(IWRT.EQ.1.OR.IWRT.EQ.4)) WRITE(6,4814)
4814  FORMAT('  IWRT = 1,4 ACCEPTED ')
        IF(LAND(IWRT,5).NE.0) RETURN
        LBWRT = 0
        IPJCA  = IDATA(IQJETC)
        IPJCA2 = IPJCA*2
        IPZV   = IDATA(IQZVTX)
      IF(
     - IPZV.GT.0
     -)THEN
          IFLAG  = IDATA(IPZV+6)
          ZVTX   = ADATA(IPZV+1)
          PEAK   = ADATA(IPZV+4)
          IF(IFLAG.LT.3 .AND. LBTRCK.EQ.8) IFLAG = 0
      IF(
     - IFLAG.LE.0
     -)THEN
      IF(
     - IWRT.EQ.0
     -)THEN
                INDREJ=4
                IF(IPRI.GT.0) WRITE(6,4347)
4347  FORMAT('  NO ZVTX FOUND,IWRT=0   REJECTED ')
      ELSE
                LBWRT = 14
                IF(IPRI.GT.0) WRITE(6,4387)
4387  FORMAT('  NO ZVTX FOUND,IWRT>0   ACCEPTED ')
      ENDIF
             RETURN
      ELSE
      IF(
     - ABS(ZVTX).GT.350.
     -)THEN
      IF(
     - IWRT.EQ.0
     -)THEN
                INDREJ=5
                IF(IPRI.GT.0) WRITE(6,9347)
9347  FORMAT('  ZVTX > 350,IWRT=0   REJECTED ')
      ELSE
                LBWRT = 15
                IF(IPRI.GT.0) WRITE(6,9387)
9387  FORMAT('  ZVTX > 350,IWRT>0   ACCEPTED ')
      ENDIF
             RETURN
      ENDIF
      ENDIF
      ELSE
      IF(
     - IWRT.EQ.0
     -)THEN
             INDREJ=6
        IF(IPRI.GT.0) WRITE(6,4817)
4817  FORMAT('  NO ZVTX BANK,IWRT=0   REJECTED ')
      ELSE
             LBWRT = 6
        IF(IPRI.GT.0) WRITE(6,4887)
4887  FORMAT('  NO ZVTX BANK,IWRT>0   ACCEPTED ')
      ENDIF
          RETURN
      ENDIF
        IPPATR = IDATA(IQPATR)
      IF(
     - IPPATR.LE.0
     -)THEN
      IF(
     - IWRT.EQ.0
     -)THEN
              INDREJ = 7
        IF(IPRI.GT.0) WRITE(6,4818)
4818  FORMAT('  NO PATR BANK,IWRT=0   REJECTED ')
      ELSE
              LBWRT = 7
        IF(IPRI.GT.0) WRITE(6,4888)
4888  FORMAT('  NO PATR BANK, IWRT>0  ACCEPTED ')
      ENDIF
           RETURN
      ENDIF
        NTR    = IDATA(IPPATR+2)
        LDTR   = IDATA(IPPATR+3)
        IPTR0  = IPPATR + IDATA(IPPATR+1)
        IPTR9  = IPTR0 + (NTR-1)*LDTR
      IF(
     - NTR.LE.0
     -)THEN
      IF(
     - IWRT.EQ.0
     -)THEN
             INDREJ = 8
             IF(IPRI.GT.0) WRITE(6,4819)
4819  FORMAT('  NO TRACKS, IWRT=0   REJECTED ')
      ELSE
             LBWRT = 8
             IF(IPRI.GT.0) WRITE(6,4889)
4889  FORMAT('  NO TRACKS, IWRT>0   ACCEPTED ')
      ENDIF
          RETURN
      ENDIF
      IF(
     - IFTG.NE.0
     -)THEN
          ACRV=1000.
          ZMIN = 1000.
      DO 13000 IPTR=IPTR0,IPTR9,LDTR
      IF(
     - IDATA(IPTR+24).GT.LIMHIT
     -)THEN
              CRV = ADATA(IPTR+25)
              IF(ABS(CRV).LT.ACRV) ACRV=ABS(CRV)
      IF(
     - ABS(CRV)      .LE.CRVTAG
     -)THEN
                AZV = ADATA(IPTR+31)
                IF(ABS(AZV).LT.ABS(ZMIN)) ZMIN = AZV
      ENDIF
      ENDIF
13000 CONTINUE
13001 CONTINUE
      IF(
     - ABS(ZMIN).GT.300.
     -)THEN
      IF(
     - IWRT.EQ.0
     -)THEN
               INDREJ = 9
               IF(IPRI.GT.0) WRITE(6,4829)
4829  FORMAT('  TAGGED EVENT, ZMIN > 300 MM IWRT=0   REJECTED ')
      ELSE
               LBWRT = 9
               IF(IPRI.GT.0) WRITE(6,4869)
4869  FORMAT('  TAGGED EVENT, ZMIN > 300 MM IWRT>0   ACCEPTED ')
      ENDIF
            RETURN
      ENDIF
          IWRT = IWRT + 16
          INDREJ = 0
          LBWRT = 3
        IF(IPRI.GT.0) WRITE(6,4830)
4830  FORMAT('  TAGGED EVENT, ZMIN < 300 MM  ACCEPTED ')
          RETURN
      ENDIF
        LBLONG = 0
        NE100 = 0
        ZSUM = 0.
        LBEHIG = 0
        ACRV=1000.
        ZAV=0.
        ZMIN = 1000.
        IKNT=0
      DO 13002 IPTR=IPTR0,IPTR9,LDTR
      IF(
     - IDATA(IPTR+33).GT.LIMHIT
     -)THEN
            LBLONG = 1
            CRV = ADATA(IPTR+25)
            AZV = ADATA(IPTR+31)
            IF(ABS(CRV).LT.ACRV) ACRV=ABS(CRV)
      IF(
     - ABS(CRV)      .LE.CRVNTG
     -)THEN
              LBEHIG = 1
              ZAV=ZAV+AZV
              IKNT=IKNT+1
              IF(ABS(AZV).LT.ABS(ZMIN)) ZMIN = AZV
      ELSE
      IF(
     - ABS(CRV).LT.CRVTAG.AND.ABS(AZV).LE.300.
     -)THEN
                RSQ = ADATA(IPTR+5)**2+ADATA(IPTR+6)**2
      IF(
     - RSQ.LT.250000.
     -)THEN
                   NE100 = NE100 + 1
                   ZSUM = AZV + ZSUM
      ENDIF
      ENDIF
      ENDIF
      ENDIF
13002 CONTINUE
13003 CONTINUE
        ISTAR = LBEHIG*2 + LBLONG
        IF(NE100.GE.2) ISTAR = ISTAR + 4
        IF(IKNT.GT.0) ZAV=ZAV/FLOAT(IKNT)
        IF(NE100.GE.2.AND.LBEHIG.EQ.0) ZMIN = ZSUM/FLOAT(NE100)
      IF(
     - LBLONG.EQ.0
     -)THEN
          ETOTX = ECAMI + ECAPL
          ACRV=1000.
          IKNT=0
      DO 13004 IPTR=IPTR0,IPTR9,LDTR
      IF(
     - IDATA(IPTR+24).GT.LIMHT1
     -)THEN
              LBLONG = 1
              CRV = ADATA(IPTR+25)
              IF(ABS(CRV).LT.ACRV) ACRV=ABS(CRV)
              IKNT=IKNT+1
      ENDIF
13004 CONTINUE
13005 CONTINUE
      IF(
     - LBLONG.EQ.0
     -)THEN
      IF(
     - IWRT.EQ.0
     -)THEN
               INDREJ = 10
               IF(IPRI.GT.0) WRITE(6,4831)
4831  FORMAT('  NOTAG EVENT, ONLY SHORT TRACKS  IWRT=0    REJECTED ')
      ELSE
               LBWRT = 10
               IF(IPRI.GT.0) WRITE(6,4851)
4851  FORMAT('  NOTAG EVENT, ONLY SHORT TRACKS  IWRT>0  ACCEPTED ')
      ENDIF
            RETURN
      ENDIF
          INDREJ = 0
          LBWRT = 4
        IF(IPRI.GT.0) WRITE(6,4832)
4832  FORMAT('  NOTAG EVENT, LONG TRACKS RFI, SHORT RZ, ACCEPTED ')
          IWRT = IWRT + 32
          RETURN
      ENDIF
      IF(
     - LBEHIG.EQ.0.AND.NE100.LT.2
     -)THEN
          ETOTX = ECAMI + ECAPL
      IF(
     - IWRT.EQ.0
     -)THEN
             INDREJ = 11
             IF(IPRI.GT.0) WRITE(6,4833)
4833  FORMAT(' LONG TRKS < 600 , < 2 WEAK TRACKS, IWRT=0  REJECTED')
      ELSE
             LBWRT = 11
             IF(IPRI.GT.0) WRITE(6,4873)
4873  FORMAT(' LONG TRKS < 600 , < 2 WEAK TRACKS, IWRT>0  ACCEPTED')
      ENDIF
          RETURN
      ENDIF
      IF(
     - ABS(ZMIN).GT.300.
     -)THEN
      IF(
     - IWRT.EQ.0
     -)THEN
             INDREJ = 12
             IF(IPRI.GT.0) WRITE(6,4834)
4834  FORMAT(' LONG TRKS > 600, OR<2 WEAK TR.ZMIN>300 IWRT=0 REJECTED')
      ELSE
             LBWRT = 12
             IF(IPRI.GT.0) WRITE(6,4884)
4884  FORMAT(' LONG TRKS > 600, OR<2 WEAK TR.ZMIN>300 IWRT>0 ACCEPTED')
      ENDIF
          RETURN
      ENDIF
        RMIN=10000.
      DO 13006 IPTR=IPTR0,IPTR9,LDTR
      IF(
     - IDATA(IPTR+33).GT.LIMHIT
     -)THEN
            CRV = ADATA(IPTR+25)
            AZV = ADATA(IPTR+31)
      IF(
     - ABS(CRV)      .LE.CRVTAG.AND.ABS(AZV).LE.300.
     -)THEN
              CALL PRTOCI(IPTR,DUM1,RM,DUM2,DUM3)
              IF(RM.LT.RMIN) RMIN=RM
      ENDIF
      ENDIF
13006 CONTINUE
13007 CONTINUE
      IF(
     - RMIN.GT.60.
     -)THEN
      IF(
     - IWRT.EQ.0
     -)THEN
              INDREJ = 13
              IF(IPRI.GT.0) WRITE(6,4835)
4835  FORMAT(' LONG>600,OR<2 WEAK TR.ZMN>300 IWRT=0 RMIN>60 REJECTED')
      ELSE
              LBWRT = 13
              IF(IPRI.GT.0) WRITE(6,4895)
4895  FORMAT(' LONG>600,OR<2 WEAK TR.ZMN>300 IWRT>0 RMIN>60 ACCEPTED')
      ENDIF
           RETURN
      ENDIF
        INDREJ = 0
        LBWRT  = 5
        IF(IPRI.GT.0) WRITE(6,4836)
4836  FORMAT(' LONG TR>600 OR 2 WEAK TR., ZMIN<300 RMIN<60  ACCEPTED')
        IWRT = IWRT + 64
        RETURN
      END
      BLOCK DATA
      COMMON /CREDON/LIMHIT,LIMHT1,CRVTAG,CRVNTG
      COMMON /CIPRI/IPRI
      DATA IPRI /0/
      DATA LIMHIT/12/, LIMHT1/20/
      DATA CRVTAG/.00150/, CRVNTG/.00025/
C      STOP
      END
      SUBROUTINE TRGCHK(LBTRCK,LBTRBT)
      IMPLICIT INTEGER*2 (H)
      LOGICAL TBIT
#include "cdata.for"
#include "cheadr.for"
      COMMON /CWORK/ ILGE(84),HCLWR(96)
      DATA MKTREV /Z653/, MKT2AC /Z53/, MKEHIG /Z600/
      INTEGER MKTRBT( 7) /1,2,4,8,16,32,64/
      INTEGER NCTOF ( 7) /2,3,3,3,3,3,3/
      INTEGER ICTOF ( 7) /0,1,2,3,4,5,6/
      LBTRCK = 0
      IF(LAND(LBTRBT,MKTREV).EQ.0) RETURN
      IF(LAND(LBTRBT,MKEHIG).NE.0) LBTRCK = 16
      IF(LAND(LBTRBT,MKT2AC).EQ.0) RETURN
      CALL SETSL(ILGE(1),0,528,0)
16000 CONTINUE
        IPALGN = IDATA(IBLN('ALGN'))
      IF(
     - IPALGN.LE.0
     -)THEN
      GOTO 16001
      ENDIF
      IF(
     - IDATA(IPALGN).LE. 3
     -)THEN
      GOTO 16001
      ENDIF
        IPLG2 = IPALGN*2
        NBARR = HDATA(IPLG2+ 4) - HDATA(IPLG2+ 3)
      IF(
     - NBARR.LE.0
     -)THEN
      GOTO 16001
      ENDIF
        IP0 = IPLG2 + 7
        IP9 = IP0 + NBARR - 1
      DO 13000 IP=IP0,IP9,2
          NBL  = HDATA(IP)
          IROW = ISHFTR(NBL,5)
          ILGE(IROW) = ILGE(IROW) + HDATA(IP+1)
13000 CONTINUE
13001 CONTINUE
      IF(.NOT.(
     - .TRUE.
     -))GOTO 16000
16001 CONTINUE
      IPJETC = IDATA(IBLN('JETC'))
      IF(IPJETC.LE.0) RETURN
      IF(IDATA(IPJETC).LE.50) RETURN
      IPJC2 = IPJETC*2
      NHTR3 = HDATA(IPJC2+99) - HDATA(IPJC2+51)
      IF(NHTR3.LE.0) RETURN
      IP0 = IPJC2 + 100 + HDATA(IPJC2+51)
      IP9 = IP0 + NHTR3 - 1
      IWIR0 =-1
      DO 13002 IP=IP0,IP9,4
        IWIR = HDATA(IP)
        IWIR = ISHFTR(IWIR,3)
        HCLL = ISHFTR(IWIR,3) - 95
        IF(IWIR.NE.IWIR0) HCLWR(HCLL) = HCLWR(HCLL) + 1
        IWIR0 = IWIR
13002 CONTINUE
13003 CONTINUE
      IPLATC = IDATA(IBLN('LATC'))
      IF(IPLATC.LE.0) RETURN
      IPLT2 = IPLATC*2
      IP0 = IPLT2 + 6
      IP9 = IP0 + 5
      ITLST =-100
      ITOF0 = 0
      ICLL0 = 0
      NTRCK = 0
      NTREL = 0
16002 CONTINUE
        ITRBT = HDATA(IP0)
      DO 13004 JTOF=1,7
      IF(
     - LAND(MKTRBT(JTOF),ITRBT) .NE. 0
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
      DO 13006 ICLL=HCLL0,HCLL9
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
13006 CONTINUE
13007 CONTINUE
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
              IF(MAX0(NHIT1,NHIT2,NHIT3,NHIT4,NHIT5,NHIT6).GE.6) LBTRK=100062700
      ENDIF
      IF(
     - LBTRK.NE.0
     -)THEN
              NTRCK = NTRCK + 1
              ITLST = ITOF
              HCLL0 = JCTOF
              IR = ITOF*2 - 3
      IF(
     - ITOF.EQ.1
     -)THEN
                ILGENG = ILGE(83)+ILGE(  84)+ILGE(   1)+ILGE(   2)
      ELSE
      IF(
     - ITOF.EQ.42
     -)THEN
                  ILGENG = ILGE(82)+ILGE(  83)+ILGE(  84)+ILGE(   1)
      ELSE
                  ILGENG = ILGE(IR)+ILGE(IR+1)+ILGE(IR+2)+ILGE(IR+3)
      ENDIF
                IF(ILGENG.GT.1000) NTREL = NTREL + 1
      ENDIF
      ENDIF
      ENDIF
13004 CONTINUE
13005 CONTINUE
      ICLL0 = ICLL0 + 8
      ITOF0 = ITOF0 + 7
      IP0   = IP0   + 1
      IF(.NOT.(
     - IP0.GT.IP9
     -))GOTO 16002
16003 CONTINUE
      LB1TCL = 0
      IF(
     - NTRCK.EQ.1 .AND. NTREL.EQ.0
     -)THEN
        IPJC2 = IPJETC*2
        NHTR1 = HDATA(IPJC2+27) - HDATA(IPJC2+ 3)
      IF(
     - NHTR1.GT.96 .AND. NHTR1.LT.196
     -)THEN
          CALL SETS(HCLWR(1),0,48,0)
          IP0 = IPJC2 + 100 + HDATA(IPJC2+ 3)
          IP9 = IP0 + NHTR1 - 1
          IWIR0 =-1
      DO 13008 IP=IP0,IP9,4
            IWIR = HDATA(IP)
            IWIR = ISHFTR(IWIR,3)
            HCLL = ISHFTR(IWIR,3) + 1
            IF(IWIR.NE.IWIR0) HCLWR(HCLL) = HCLWR(HCLL) + 1
            IWIR0 = IWIR
13008 CONTINUE
13009 CONTINUE
          MTRK = 0
          HCLL1 =-99
      DO 13010 ICLL=1,48
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
13010 CONTINUE
13011 CONTINUE
          IF(MTRK.EQ.2 .OR. MTRK.EQ.3) LB1TCL = 1
      ENDIF
      ENDIF
      IF(NTRCK .GE.2) LBTRCK = LBTRCK + 2
      IF(NTRCK .EQ.1 .AND. LAND(LBTRBT,2).NE.0) LBTRCK = LBTRCK + 1
      IF(NTREL .EQ.1) LBTRCK = LBTRCK + 4
      IF(LB1TCL.EQ.1) LBTRCK = LBTRCK + 8
      RETURN
      END
