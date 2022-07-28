      SUBROUTINE USER(INDEX)
      IMPLICIT INTEGER*2 (H)
#include "cdata.for"
#include "cpatlm.for"
#include "czvpar.for"
#include "cworkzv.for"
#include "cjdrch.for"
#include "cutsr1.for"
      COMMON /CTLIM/ ISECLF
      COMMON /CHEADR/ HEAD(108)
CAV      COMMON / CADMIN / IEVTP,NRREAD,NRWRIT
CAV   To assure all symbols have same size
#include "cadmin.for"
CAV

      COMMON/CALIBR/JPOINT(100),
     +HMUCAL(8370),HLGMAI(5760),HTAGSY(384),HJPULS(9216),
     +DELTA0(96,2),DELTA1(96,2),DELTA2(96,2),DELTA5(96,2),DELTA6(96,2)
      COMMON /CIGG/ IPRN,IGG(80),JIGG(80)
      LOGICAL*1 CHOP
      DATA LBSTRT/0/,ETAGLM/300./,LIMHIT/12/,LIMHT1/20/,MKFWU1/Z800/
      DATA MKTAGE/Z400/,MKLGLE/Z200/,MKLUMI/Z100/,MKFWMU/Z80/,MKT3/Z80/
 2000 FORMAT('1')
 2001 FORMAT('0TOTAL REDUCTION FACTOR ',F7.2,' % ')
 2002 FORMAT(' COUNTERS ',10I8)
 2003 FORMAT(' TOTAL COUNTS ',10I8)
 2004 FORMAT('  RUN AND EVENT HAS NO ZVTX BANK ',2I5)
 2005 FORMAT('  RUN AND EVENT HAS NO PATR BANK ',2I5)
 2006 FORMAT(' RUN AND EVENT ',2I6,' HAS RETURN1 IN LGCALB')
 2007 FORMAT(1H1/
     +1X,10('='),20X,10('=')/
     +1X,10('='),' START OF RUN ',I5,1X,10('=')/
     +1X,10('='),20X,10('='))
 2018 FORMAT(
     +1X,10('=')/
     +1X,10('='),' THIS RUN HAS BEEN CHOPPED'/
     +1X,10('='))
      IF(
     - INDEX.EQ.0
     -)THEN
        CALL R1MODE
        ISECLF=20
        IPRN = 1
        IPFAST = 0
        IQHEAD = IBLN('HEAD')
        IQTRIG = IBLN('TRIG')
        IQALGN = IBLN('ALGN')
        IQZVTX = IBLN('ZVTX')
        IQJETC = IBLN('JETC')
        IQPATR = IBLN('PATR')
        IQJHTL = IBLN('JHTL')
        IQMPRS = IBLN('MPRS')
      DO 13000 I = 1,80
         IGG(I) = 0
        JIGG(I) = 0
13000 CONTINUE
13001 CONTINUE
        CALL HBOOK1(1,'ZVERTEX (ALL)$',100,-2000.,2000.)
        CALL HBOOK1(2,'NHITS IN RING1 (ZVERTEX < 300 MMS)$',100,0.,400.)
        CALL HBOOK1(3,'NUMBER OF TRACKS$',100,0.,50.)
        CALL HBOOK1(4,'LG ENERGY SUM (ALL)  $',100,0.02,40000.)
        CALL HBOOK1(5,'LG ENDCAP ENERGY (BAD BLOCKS) $',100,0.,40000.)
        CALL HBOOK1(6,'TRACK CURVATURE (LONG TRACKS) $',100,-.001,.001)
        CALL HBOOK1(7,'ZMIN (TRACKS)  $',100,-1000.,1000.)
        CALL HBOOK1(8,'RMIN $',100,0.,300.)
        CALL HBOOK1(9,'REJECT INDEX$',100,0.,  100.)
        INDEX=INDEX+1
        RETURN
      ENDIF
      IF(
     - INDEX.EQ.100
     -)THEN
      DO 13002 I = 2,80
        JIGG(I) = JIGG(I) + IGG(I)
13002 CONTINUE
13003 CONTINUE
        PERCEN = (FLOAT(NRWRIT)/FLOAT(JIGG(4)))*100.
        WRITE(6,2001) PERCEN
        CALL STATUS
      DO 13004 I = 1,80
        IGG(I) = JIGG(I)
13004 CONTINUE
13005 CONTINUE
        CALL STATUS
        CALL HISTDO
        RETURN
      ENDIF
      IF(INDEX.GT.2 .AND. INDEX.LE.8) IGG(INDEX+2) = IGG(INDEX+2) + 1
16000 CONTINUE
      INDREJ = 0
      IZZZSE=INDEX
      GOTO 14002
14003 CONTINUE
        IGG(3) = IGG(3) + 1
        INDEX=INDEX+1
        IF(IGG(4).NE.0) CALL STATUS
      DO 13006 I = 2,80
          JIGG(I) = JIGG(I) + IGG(I)
          IGG(I) = 0
13006 CONTINUE
13007 CONTINUE
        IGG(1) = HEAD(18)
      PRINT2007,HEAD(18)
      IF(
     - LBSTRT.EQ.0
     -)THEN
          LBSTRT = 1
          ZFITLM(1) = 70.
          ZFITLM(2) = 35.
      ENDIF
        CHOP=.FALSE.
      IF(
     - NOTTOT.NE.0
     -)THEN
      DO 13008 I=1,NOTTOT
          IF(HEAD(18).EQ.NOTRUN(I)) CHOP=.TRUE.
13008 CONTINUE
13009 CONTINUE
      ENDIF
        IF(HEAD(18).LT.NRUNST) CHOP=.TRUE.
        IF(CHOP) PRINT2018
        RETURN
14004 CONTINUE
        INDEX = 1
        IF(CHOP) RETURN
        IF(HEAD(18).EQ.NRUNST .AND. HEAD(19).LT.NEVTST) RETURN
        IGG(4) = IGG(4) + 1
        IWRT = 0
        ILUMI = 0
        INDREJ =  1
      IF(
     - HEAD(19).LT.13
     -)THEN
      GOTO 16001
      ENDIF
        IFLW= 0
      IF(
     - HEAD(23).NE.0
     -)THEN
          IFLW = 1
          IGG(77) = IGG(77) + 1
      ENDIF
        IF(HEAD(23).NE.0) IFLW = 1
        IERRLG = 0
        CALL LGCALB(&ERRLGC)
        LBTRBT = 0
        IPTRIG = IDATA(IQTRIG)
        IF(IPTRIG.GT.0 .AND. IDATA(IPTRIG-2).EQ.1)
     +  LBTRBT = HDATA(IPTRIG*2+10)
        IF(LAND(LBTRBT,MKLUMI).NE.0) ILUMI = 1
        IMUACC=0
        IMU=0
        IPHEAD = IDATA(IQHEAD)
        IF(IPHEAD.GT.0) IMU=HDATA(IPHEAD*2+22)
        IF((LAND(LBTRBT,MKFWMU).NE.0.AND.LAND(IMU,MKT3).NE.0).OR.
     +   LAND(LBTRBT,MKFWU1).NE.0) CALL MEWT3(IMUACC)
      IF(
     - IMUACC.LE.0
     -)THEN
        CALL TRGCHK(LBTRCK,LBTRBT)
        INDREJ =  2
      IF(
     - LBTRCK.EQ.0
     -)THEN
      GOTO 16001
      ENDIF
        IACC = 0
        IF(IERRLG.EQ.0) CALL LGCUT(IACC,ECYL,ECAMI,ECAPL,ETOT)
        IF(LAND(LBTRBT,MKLGLE).EQ.0) IACC = 0
        IFTG = 0
        IF(LAND(LBTRBT,2).NE.0) CALL TAGFLG(IFTG)
        ETAG = 0.
        IF(IFTG.NE.0. .AND. LAND(LBTRBT,MKTAGE).NE.0) ETAG = ETOT
        AFTG = IFTG
        IF(IACC.NE.     0) IWRT = 1
        IF(IFLW.NE.     0) IWRT = IWRT + 2
      IF(
     - ETAG.GT.ETAGLM .AND. IWRT.EQ.0
     -)THEN
          ECENTR = 0.
          IF(IFTG.EQ.11) ECENTR = ETOT - ECAMI
          IF(IFTG.EQ.12) ECENTR = ETOT - ECAPL
          IF(IFTG.GT.12) ECENTR = ETOT
      IF(
     - ECENTR.GT.100.
     -)THEN
              IWRT = IWRT + 4
      ELSE
      ENDIF
      ENDIF
        INDREJ =  3
      IF(
     - IWRT.EQ.0 .AND. LBTRCK.EQ.16
     -)THEN
      GOTO 16001
      ENDIF
      ELSE
        IWRT=1
        IGG(2)=IGG(2)+1
      ENDIF
        IPJCA  = IDATA(IQJETC)
      IF(
     - IPJCA.LE.0
     -)THEN
          INDEX = 1
          INDREJ =  4
      IF(
     - IWRT.EQ.0
     -)THEN
      GOTO 16001
      ENDIF
          INDREJ =  5
          INDEX = 11
      GOTO 16001
      ENDIF
        INDREJ =  6
        INDEX = 4
        RETURN
14005 CONTINUE
        INDEX = 1
        INDREJ = 11
      IF(
     - LAND(IWRT,5).NE.0
     -)THEN
      GOTO 16001
      ENDIF
        IPJCA  = IDATA(IQJETC)
        IPJCA2 = IPJCA*2
        NHITR1 = (HDATA(IPJCA2+27) - HDATA(IPJCA2+3)) / 4
        IPZV   = IDATA(IQZVTX)
        IF(IPZV.LE.0) WRITE(6,2004) HEAD(18),HEAD(19)
      IF(
     - IPZV.GT.0
     -)THEN
          IFLAG  = IDATA(IPZV+6)
          ZVTX   = ADATA(IPZV+1)
          PEAK   = ADATA(IPZV+4)
          IF(IFLAG.LT.3 .AND. LBTRCK.EQ.8) IFLAG = 0
          IGG(IFLAG+26) = IGG(IFLAG+26) + 1
      IF(
     - IFLAG.LE.0
     -)THEN
            INDREJ = 12
      GOTO 16001
      ELSE
            CALL HF1(1,ZVTX,1.)
      IF(
     - ABS(ZVTX).LE.300.
     -)THEN
              ANHTR1 = NHITR1
              CALL HF1(2,ANHTR1,1.)
      ELSE
              INDREJ = 13
      GOTO 16001
      ENDIF
      ENDIF
      ELSE
          INDREJ = 14
      GOTO 16001
      ENDIF
        INDEX = 5
        RETURN
14006 CONTINUE
        INDEX = 1
        IPPATR = IDATA(IQPATR)
        IF(IPPATR.LE.0) WRITE(6,2005) HEAD(18),HEAD(19)
        INDREJ = 21
        IPPATR = IDATA(IQPATR)
        IF(IPPATR.LE.0) IWRT=1
      IF(
     - IPPATR.LE.0
     -)THEN
      GOTO 16001
      ENDIF
        INDEX = 1
        NTR    = IDATA(IPPATR+2)
        LDTR   = IDATA(IPPATR+3)
        IPTR0  = IPPATR + IDATA(IPPATR+1)
        IPTR9  = IPTR0 + (NTR-1)*LDTR
        ANTR = NTR
        CALL HF1(3,ANTR,1.)
      IF(
     - NTR.LE.0
     -)THEN
          INDREJ = 22
      GOTO 16001
      ENDIF
      IF(
     - IFTG.NE.0
     -)THEN
          ACRV=1000.
          ZMIN = 1000.
          IFTAGC = 0
      DO 13010 IPTR=IPTR0,IPTR9,LDTR
      IF(
     - IDATA(IPTR+24).GT.LIMHIT
     -)THEN
              CRV = ADATA(IPTR+25)
              IF(ABS(CRV).LT.ACRV) ACRV=ABS(CRV)
      IF(
     - ABS(CRV)      .LE..00150
     -)THEN
                AZV = ADATA(IPTR+31)
                IF(ABS(AZV).LT.ABS(ZMIN)) ZMIN = AZV
      ENDIF
      ENDIF
13010 CONTINUE
13011 CONTINUE
      IF(
     - ABS(ZMIN).GT.300.
     -)THEN
            INDREJ = 23
      GOTO 16001
      ENDIF
          INDEX = 11
          IWRT = IWRT + 16
          INDREJ = 24
      GOTO 16001
      ENDIF
        LBLONG = 0
        NE100  = 0
        ZSUM   = 0
        LBEHIG = 0
        ACRV=1000.
        ZAV=0.
        ZMIN = 1000.
        IKNT=0
      DO 13012 IPTR=IPTR0,IPTR9,LDTR
      IF(
     - IDATA(IPTR+33).GT.LIMHIT
     -)THEN
            LBLONG = 1
            CRV = ADATA(IPTR+25)
            AZV = ADATA(IPTR+31)
            IF(ABS(CRV).LT.ACRV) ACRV=ABS(CRV)
            CALL HF1(6,CRV,1.)
      IF(
     - ABS(CRV)      .LE..00025
     -)THEN
              LBEHIG = 1
              ZAV=ZAV+AZV
              IKNT=IKNT+1
              IF(ABS(AZV).LT.ABS(ZMIN)) ZMIN = AZV
      ELSE
      IF(
     - ABS(CRV).LT..00150 .AND.
     +           ABS(AZV).LE.300.
     -)THEN
                RSQ = ADATA(IPTR+5)**2 + ADATA(IPTR+6)**2
      IF(
     - RSQ.LT.250000.
     -)THEN
                  NE100 = NE100 + 1
                  ZSUM = AZV + ZSUM
      ENDIF
      ENDIF
      ENDIF
      ENDIF
13012 CONTINUE
13013 CONTINUE
        ISTAR = LBEHIG*2 + LBLONG
        IF(NE100.GE.2) ISTAR = ISTAR + 4
        ASTAR = ISTAR
        IF(IKNT.GT.0) ZAV=ZAV/FLOAT(IKNT)
        IF(NE100.GE.2 .AND. LBEHIG.EQ.0) ZMIN = ZSUM/FLOAT(NE100)
      IF(
     - LBLONG.EQ.0
     -)THEN
          ETOTX = ECAMI + ECAPL
          ACRV=1000.
          IKNT=0
      DO 13014 IPTR=IPTR0,IPTR9,LDTR
      IF(
     - IDATA(IPTR+24).GT.LIMHT1
     -)THEN
              LBLONG = 1
              CRV = ADATA(IPTR+25)
              IF(ABS(CRV).LT.ACRV) ACRV=ABS(CRV)
              IKNT=IKNT+1
      ENDIF
13014 CONTINUE
13015 CONTINUE
      IF(
     - LBLONG.EQ.0
     -)THEN
            INDREJ = 25
      GOTO 16001
      ENDIF
          AKNT = IKNT
          INDREJ = 26
          INDEX = 11
          IWRT = IWRT + 32
      GOTO 16001
      ENDIF
      IF(
     - LBEHIG.EQ.0 .AND. NE100.LT.2
     -)THEN
          ETOTX = ECAMI + ECAPL
          INDREJ = 27
      GOTO 16001
      ENDIF
        CALL HF1(7,ZMIN,1.)
      IF(
     - ABS(ZMIN).GT.300.
     -)THEN
          INDREJ = 28
      GOTO 16001
      ENDIF
        RMIN=10000.
      DO 13016 IPTR=IPTR0,IPTR9,LDTR
      IF(
     - IDATA(IPTR+33).GT.LIMHIT
     -)THEN
            CRV = ADATA(IPTR+25)
            AZV = ADATA(IPTR+31)
      IF(
     - ABS(CRV)      .LE..00150 .AND. ABS(AZV).LE.300.
     -)THEN
              CALL PRTOCI(IPTR,DUM1,RM,DUM2,DUM3)
              IF(RM.LT.RMIN) RMIN=RM
      ENDIF
      ENDIF
13016 CONTINUE
13017 CONTINUE
        CALL HF1(8,RMIN,1.)
        INDREJ = 29
      IF(
     - RMIN.GT.60.
     -)THEN
      GOTO 16001
      ENDIF
        INDREJ = 30
        INDEX=11
        IWRT = IWRT + 64
      GOTO 16001
14002 CONTINUE
      GOTO(
     -14003,14004,14000,14005,14006
     -), IZZZSE
14000 CONTINUE
        INDEX = INDEX + 1
14001 CONTINUE
      INDREJ = 31
      IF(.NOT.(
     - .TRUE.
     -))GOTO 16000
16001 CONTINUE
      IF(
     - INDEX.EQ.11 .OR. LAND(IWRT,5).NE.0 .AND. INDEX.NE.4
     -)THEN
      IF(
     - IDATA(IQJETC).GT.0
     -)THEN
          IPFAST = 2
          CALL INPATR
          CALL PATREC(0)
          IPPATR = IDATA(IQPATR)
          IPJHTL = IDATA(IQJHTL)
      IF(
     - IPPATR.NE.0 .AND. IDATA(IPPATR-2).EQ.10
     -)THEN
            IDATA(IPPATR-2) = 9
            IF(IPJHTL.NE.0 .AND. IDATA(IPJHTL-2).EQ.10)IDATA(IPJHTL-2)=9
CAV            00000591
      ENDIF
          CALL BDLS('PATR',10)
          CALL BDLS('JHTL',10)
          IPFAST = 0
          CALL INPATR
          ZFITLM(1) = 70.
          ZFITLM(2) = 35.
      ENDIF
        INDEX = 11
      ENDIF
      IF(INDEX.EQ.1 .AND. IWRT.NE.0) INDEX = 11
      IF(
     - INDEX.EQ.11
     -)THEN
        IPHEAD = IDATA(IQHEAD)*2
        IF(IPHEAD.GT.0) HDATA(IPHEAD+27) = IWRT
      ENDIF
      IF(
     - INDEX.EQ.1 .AND. ILUMI.NE.0
     -)THEN
        DATA NEVLUM /4/
        NEVLUM = NEVLUM - 1
      IF(
     - NEVLUM.LE.0
     -)THEN
          INDEX = 11
          NEVLUM = 4
          IPHEAD = IDATA(IQHEAD)*2
          IF(IPHEAD.GT.0) HDATA(IPHEAD+27) = 0
          IGG(80) = IGG(80) + 1
      ENDIF
      ENDIF
      IF(
     - INDEX.EQ.11
     -)THEN
        IGG(     79) = IGG(     79) + 1
        AWRT  = IWRT
        ATRCK = LBTRCK
      ENDIF
      ANDREJ = INDREJ
      CALL HF1(9,ANDREJ,1.)
      IF(INDREJ.GT.0) IGG(INDREJ+10) = IGG(INDREJ+10) + 1
      IF(IWRT  .GE.16 .AND. IWRT.LT.32) IWRT = 16
      IF(IWRT  .GE.64) IWRT = 18
      IF(IWRT  .GE.32) IWRT = 17
      IF(IWRT  .GT.0) IGG(IWRT+50) = IGG(IWRT+50) + 1
      IMPRT=IDATA(IQMPRS)
      IF(
     - IMPRT.GT.0.AND.INDEX.EQ.11
     -)THEN
      IRFLAG=HDATA(2*IMPRT+3)
      IF(LAND(IRFLAG,4).NE.0) IGG(19)=IGG(19)+1
      IF(LAND(IRFLAG,8).NE.0) IGG(20)=IGG(20)+1
      ENDIF
      RETURN
17000 CONTINUE
        IGG(78) = IGG(78) + 1
        IF(IGG(78).LT.20.AND.IPRN.GT.0) WRITE(6,2006) HEAD(18),HEAD(19)
        IERRLG = 1
      END
