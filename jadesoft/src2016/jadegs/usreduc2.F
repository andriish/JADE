C   08/07/80 301061240  MEMBER NAME  USREDUC2 (JADEGS)      FORTRAN
C   04/06/80 006061316  MEMBER NAME  USREDUC2 (JADESR)      FORTRAN
      SUBROUTINE USER(INDEX)
C---
C---     USER ROUTINE FOR CUTS AND INTERACTIVE DECISION MAKING.
C---     SPECIAL VERSION FOR SECOND DATA REDUCTION STEP
C--   J.OLSSON    09.07.79        LAST CHANGE 20.01.82
      IMPLICIT INTEGER*2 (H)
C
      COMMON // BLCOMM(27000)
#include "cgraph.for"
#include "cgeo1.for"
#include "cpatlm.for"
#include "cjdrch.for"
      COMMON /CLERGY/ ECYLM,ECYLP
      COMMON /CREDON/LIMHIT,LIMHT1,CRVTAG,CRVNTG
      COMMON /CREDTV/ CRVLIM,LMHITS,RPLIM,RPLIM1,RATLIM,COSCUT,ZVTXLM,
     $ ZVXLM1,ZVXLM2,ETOTLM,ETOTKP,ETCYKP,ETE1KP,ETE2KP,ZMLIM,FIDEL,
     $ XLM,YLM,ZLM,ERGL,ETAGLM,ETOTCT,TSUMC1,TSUMC2
      COMMON/SMPTOF/MTOF,TTOF(42),NTOF,HTOF(42)
      COMMON /CTLIM/ ISECLF
      COMMON /CHEADR/ HEAD(108)
      EQUIVALENCE (HRUN,HEAD(18)),(HEVENT,HEAD(19))
#include "cdata.for"
      COMMON /CADMIN/ IEVTP,NRREAD,NRWRIT,NRERR
      COMMON /CJTRIG/ PI,TWOPI
      COMMON /CIGG/ IPRN,IGG(80),JIGG(80)
      DIMENSION HELP(2),HELP2(2),THHELP(3)
      EQUIVALENCE (ICAMWD,HELP(1)),(HELP2(1),ICMWD2)
      DATA MSKLUM /Z102/,MSKLM1/Z100/,MKFWMU/Z800/,MKFWM1/Z80/
      DATA  HELP/0,0/,HELP2/0,0/
C--------------------------------------------
C---
C        INDEX=0   INITIAL CALL, BEFORE FIRST EVENT READ.
C              1   CALLED AT THE BEGINNING OF EACH NEW RUN.
C              2   CALLED IMMEDIATELY AFTER EVENT IS READ INTO CDATA.
C              3   LEAD GLASS ENERGIES HAVE BEEN COMPUTED.
C              4   FAST Z VERTEX RECONSTRUCTION HAS BEEN DONE.
C              5   INNER DETECTOR PATTERN RECOGNITION HAS BEEN RUN.
C              6   ENERGIES CLUSTERS IN THE LEAD GLASS HAVE BEEN FOUND.
C              7   TRACKS AND CLUSTERS HAVE BEEN ASSOCIATED.
C              8   MUON CHAMBER TRACKING HAS BEEN DONE.
C              9   MUON AND INNER DETECTOR TRACKS HAVE BEEN ASSOCIATED.
C             10   UNUSED
C---
C---     CHECK WHETHER CALL AT END OF JOB
      IF(INDEX.EQ.100) GOTO 9900
C
      GO TO (100,200,300,400,500,600,700,800,900,1000),INDEX
C---
C---     INDEX=0 INITIALIZATION.
C---
C
      DO 50  I = 1,80
      JIGG(I) = 0
50    IGG(I) = 0
      ISECLF = 15
      HRUNTP = -4
      NNREC = 1
      NMREC = 0
      EBMSTR = 0.
C  IHIST FOR HISTOGRAMS;  IRED1 FOR REDUC1;   IPRN FOR DEBUG PRINT;
C  ICALIB FOR RECALIBRATION OF LEAD GLASS, ICALJC FOR INNER DETECTOR
C  IREPAT FOR PATTERN RECOGNITION, WITH FOLLOWING OPTIONS:
C  1: SLOW FOR ALL, 2 EXISTING ONE FOR SELECT, REDO SLOW
C  3: FAST FOR ALL, REDO SLOW FOR SELECTED EVENTS;  0 FOR NO PATREC
      READ 6310,IHIST,IRED1,MAXEVS,IPRN,ICALIB,ICALJC,IREPAT
      READ 6310,NNRMIN,NNRMAX,HRUNMI,HRUNMX,IREFRM,ITMLOG,IPATRJ
6310  FORMAT(7I10)
      WRITE(6,6312) IHIST,IRED1,ICALIB,ICALJC,IREPAT,IPRN
6312  FORMAT(' IHIST IRED1 ICALIB ICALJC IREPAT IPRN ',6I8)
      WRITE(6,6313) NNRMIN,NNRMAX,MAXEVS,HRUNMI,HRUNMX
6313  FORMAT(' NNRMIN NNRMAX MAXEVS ',3I10,'   RUN LIMITS ',2I10)
      WRITE(6,6314) IREFRM,ITMLOG,IPATRJ
6314  FORMAT(' IREFRM ITMLOG IPATRJ ',3I10)
      IF(IREPAT.EQ.3) IPFAST = 0
      IF(IHIST.EQ.0) ISECLF = 2
      IF(ITMLOG.EQ.1) CALL TMLOG(10,8HUSER    )
      CALL HBOOK1(1,'ETOT ALL $',100,    0., 40000.)
      CALL HBOOK1(2,'ETOT ALL $',100,    0., 5000.)
      CALL HBOOK1(3,'ETOT FOR TAGGED EVENTS $',100,    0., 20000.)
      CALL HBOOK1(4,'ETOT FOR IAC=-1 EVENTS $',100,    0., 20000.)
      CALL HBOOK1(5,'ETOT FOR IAC.GE.0 EVENTS $',100,    0., 40000.)
      CALL HBOOK1(6,'ECYL FOR IAC.GE.0 EVENTS $',100,    0., 40000.)
      CALL HBOOK1(7,'ECAMI FOR IAC.GE.0 EVENTS $',100,    0., 40000.)
      CALL HBOOK1(8,'ECAPL FOR IAC.GE.0 EVENTS $',100,    0., 40000.)
      CALL HBOOK1(9,'ETOT FOR EVENTS WITHOUT ZVTX$',100,    0., 40000.)
      CALL HBOOK1(10,'ZVTX  $',100, -500., 500.)
      CALL HBOOK2(11,'ETOT VS ZVTX$',100,-500.,500.,50,0.,40000.)
      CALL HBOOK1(12,'ZVTX,ETOT>2 GEV $',100, -500., 500.)
      CALL HBOOK2(13,'ETOT VS ZVTX,IWRT=0$',100,-500.,500.,50,0.,10000.)
      CALL HBOOK2(14,'ETOT VS ZVTX,IWRT=1$',100,-500.,500.,50,0.,40000.)
      CALL HBOOK1(15,'NTR AFTER ZVTX REJECTS$',100,-.5,49.5)
      CALL HBOOK1(16,'ETOT FOR NTR=0 EVENTS $',100,    0., 40000.)
      CALL HBOOK1(17,'ZVTX, NTR=0 $',100, -500., 500.)
      CALL HBOOK1(18,'ETOT FOR ISTAR = 0 * 2  EVENTS$',100,0.,20000.)
      CALL HBOOK1(19,'ETOT FOR ISTAR=1 EVENTS$',100,0.,20000.)
C     CALL HBOOK1(20,'ETOT FOR ISTAR=2 EVENTS$',100,0.,20000.)
      CALL HBOOK1(21,'RATIO     $',100,0.01,1.01)
C     CALL HBOOK2(22,'RATIO VS ZVTX$',100,-500.,500.,51,0.,1.02)
C     CALL HBOOK2(23,'ICNTR VS IGDNTR$',50,0.,12.5,50,0.,12.5)
      CALL HBOOK2(24,'E VS ZVX,RATIO FAIL$',100,-500.,500.,50,0.,5000.)
      CALL HBOOK2(25,'E VS ZVX,NTR.NE.2$',100,-500.,500.,50,0.,5000.)
      CALL HBOOK2(26,'E VS ZVX,NTR=2,BF TH$',100,-500.,500.,50,0.,5000.)
      CALL HBOOK2(27,'E VS ZVX,NTR=2,AF TH$',100,-500.,500.,50,0.,5000.)
      CALL HBOOK1(28,'ABS THETA-PI TWOPRONG$',100,0.,4.)
      CALL HBOOK1(29,'T-DIFF. IN LOW-E TWOPRONGS$',100,-10.,30.)
      CALL HBOOK2(30,'EVSZVX,COSMIC 2PRNGS$',100,-500.,500.,50,0.,5000.)
      CALL HBOOK2(31,'E VS ZVX, R2 OK$',100,-500.,500.,50,0.,5000.)
      CALL HBOOK2(32,'EVSZVX,R2 OK,ICOSM=1$',100,-500.,500.,50,0.,5000.)
      CALL HBOOK2(33,'E VS ZVX, R2 NOK$',100,-500.,500.,50,0.,5000.)
      CALL HBOOK1(34,'NCLST $',100,-.5,49.5)
      CALL HBOOK1(35,'ETOT FOR NCLST.LE.1 EVENTS$',100,0.,5000.)
      CALL HBOOK1(36,'ETOT FOR HWORLD REJECTS$',100,0.,40000.)
      CALL HBOOK1(37,'ETOT FOR COLLINEAR REJECTS$',100,0.,5000.)
      CALL HBOOK2(38,'E VS ZVX,COLL. RJCTS$',100,-500.,500.,50,0.,5000.)
      CALL HBOOK1(39,'ETOT FOR COLLINEAR ACCEPTS$',100,0.,5000.)
      CALL HBOOK2(40,'E VS ZVX,COLL. ACPTS$',100,-500.,500.,50,0.,5000.)
      CALL HBOOK1(41,'NTR FOR COLLINEAR ACCEPTS$',100,-.5,49.5)
      CALL HBOOK2(42,'E VS ZV REJECTED EV$',100,-500.,500.,50,0.,5000.)
      CALL HBOOK1(43,'ETOT FOR REJECTED EVS$',100,0.,5000.)
      CALL HBOOK1(44,'REJECT FLAG AND WRITE FLAG$',100,-.25,49.75)
      CALL HBOOK2(45,'E VS ZV ACCEPTED EV$',100,-500.,500.,50,0.,40000.)
      CALL HBOOK1(46,'ZVTX     ACCEPTED EVENTS $',100, -500., 500.)
      CALL HBOOK1(47,'ETOT     ACCEPTED EVENTS$',100,0.,40000.)
      CALL HBOOK1(48,'NTR FOR  ACCEPTED EVENTS$',100,-.5,49.5)
      CALL HBOOK1(49,'COS OF COLLINEARITY ANGLE$',100,-1.,-0.7)
C     CALL HBOOK1(50,'IBM ACTION FOR REJECTS $',100,0.,100.)
C     CALL HBOOK1(51,'IBM ACTION FOR ACCEPTS $',100,0.,100.)
      CALL HBOOK1(52,'ZVTX     REJECTED EVENTS $',100, -500., 500.)
      CALL HBOOK1(53,'ETOT,TAGS WITH NTR=0 $',100,    0., 4000.)
      CALL HBOOK1(54,'TOF SUM, COLL.FEWPRONGS$',100, -50., 250.)
      CALL HBOOK2(55,'TOFSUM VS TOFDIF$',100,-10.,30.,100,-50.,250.)
      CALL HBOOK2(57,'EVSZV TRGCHK RJC$',100,-500.,500.,50,0.,8000.)
      CALL HBOOK1(61,'ZVTX LUMI TRIGGER REJECTS$',100, -500., 500.)
      CALL HBOOK1(62,'ZVTX ZVTX > LIMITS REJCTS$',100, -500., 500.)
      CALL HBOOK1(63,'ZVTX SPECIAL RATIO,ZVTX  $',100, -500., 500.)
      CALL HBOOK1(64,'ZVTX SPECIAL RATIO,ZVTX  $',100, -500., 500.)
      CALL HBOOK1(65,'ZVTX COSMIC REJECTS      $',100, -500., 500.)
      CALL HBOOK1(66,'ZVTX RATIO FAILURE FEWPR $',100, -500., 500.)
      CALL HBOOK1(67,'ZVTX LGCL ERROR,REJECTS  $',100, -500., 500.)
      CALL HBOOK1(69,'ZVTX  NCLST=1,NO ECAPS RJ$',100, -500., 500.)
      CALL HBOOK1(70,'ZVTX ALGN MISSING,REJECTS$',100, -500., 500.)
      CALL HBOOK1(71,'ZVTX HWORLD REJECTS      $',100, -500., 500.)
      CALL HBOOK1(72,'ZVTX NTR=0,IDHITS>1000   $',100, -500., 500.)
      CALL HBOOK1(73,'ZVTX NO COLLINEAR REJECTS$',100, -500., 500.)
      CALL HBOOK1(75,'ZVTX  REDUC1 REJECTS     $',100, -500., 500.)
      CALL HBOOK1(76,'ZVTX  TRGCHK REJECTS     $',100, -500., 500.)
      CALL HBOOK1(81,'ZVTX  WRITFLAG AND TRACKS$',100, -500., 500.)
      CALL HBOOK1(82,'ZVTX  NTRNE2,NE3 RATIO OK$',100, -500., 500.)
      CALL HBOOK1(83,'ZVTX  2,3 PRONGS,NONCOLL.$',100, -500., 500.)
      CALL HBOOK1(84,'ZVTX  COLL NOCOSM, RAT OK$',100, -500., 500.)
      CALL HBOOK1(85,'ZVTX  COLLIN CLUSTERS    $',100, -500., 500.)
      CALL HBOOK1(86,'ZVTX  FEW CLUST, GOOD E  $',100, -500., 500.)
      CALL HBOOK1(87,'ZVTX  NEUTRAL TAGS       $',100, -500., 500.)
      CALL HBOOK1(88,'ZVTX  1 CLUST, ECAP ERGY $',100, -500., 500.)
      CALL HBOOK1(89,'ZVTX TAGGED NTR=1 ACCEPTS$',100, -500., 500.)
      CALL HBOOK1(101,'NR OF HITS IN RZ$',100,-0.75,99.25)
      CALL HBOOK1(102,'THETA OF TRACKS$',100,-2.0,2.0)
      INDEX=INDEX+1
      RETURN
C -------------------------------------------------------------------
C *******************************************************************
C -------------------------------------------------------------------
100   CONTINUE
C                                    BEGIN OF EACH RUN
      IF(IPRN.GT.0) WRITE(6,1709) HRUN
1709  FORMAT(' RUN ',I10)
      IF(IGG(2).NE.0.AND.IPRN.EQ.2) WRITE(6,1717) (IGG(I),I=1,80)
      DO 9785 I = 2,80
      JIGG(I) = JIGG(I) + IGG(I)
 9785 IGG(I) = 0
      NRUN = HEAD(18)
C   RUN NO.
      IGG(1) = HRUN
      WRITE(JUSCRN,102) HRUN,HEVENT,HEAD(20)
102   FORMAT(' ****** RUN NR ',I5,' FIRST EVENT IS ',I8,' REC.TYPE ',I8)
      WRITE(JUSCRN,2027) IGG(80)
2027  FORMAT(' NR OF EVENTS WRITTEN SO FAR  ----- >>>> ',I10)
      IF(HRUN.GT.1400) ZFITLM(1) = 70.
      IF(HRUN.GT.1400) ZFITLM(2) = 35.
      EBM = EBEAM(HRUN)
      IF(EBM.NE.EBMSTR) GO TO 1612
      GO TO 1021
1612  EBMSTR = EBM
      MMREC = NNREC + 1
C DECIDE BEAM ENERGY AND LIMITS
      IF(EBM.GT.9000.) GO TO 1029
C ENTER HERE FOR    NEW LIMITS   FOR LOW ENERGY DATA
      WRITE(6,1671) MMREC,HRUN,HEVENT,EBM
1671  FORMAT(' LOW ENERGY DATA, RECORD,RUN EVENT ',3I8,' EBEAM ',E12.4)
C SCALE 6 GEV LIMITS TO BEAM ENERGY
      EBMFRA = EBM/6000.
      ETOTKP = 2500.*EBMFRA
      ETCYKP = 1250.*EBMFRA
      ETE1KP = 1500.*EBMFRA
      ETE2KP = 500.
      RPLIM = 40.
      RPLIM1 = 15.
      GO TO 1024
C ENTER HERE FOR    NEW LIMITS   FOR HIGH ENERGY DATA
1029  WRITE(6,1672) MMREC,HRUN,HEVENT,EBM
1672  FORMAT(' HIGH ENERGY DATA ,RECORD,RUN EVENT ',3I8,' EBEAM ',E12.4)
      EBMFRA = EBM/15000.
      ETOTKP = 7000.*EBMFRA
      ETCYKP = 3500.*EBMFRA
      ETE1KP = 4000.*EBMFRA
      ETE2KP = 500.
      RPLIM = 30.
      RPLIM1 = 10.
1024  WRITE(6,1022)
1022  FORMAT(' ZVTXLM  LMHITS  ETOTKP  ETOTCT  ETAGLM  COSCUT   ZMLIM
     $RPLIM  RPLIM1  RATLIM  CRVLIM  ZVXLM1  ZVXLM2')
      WRITE(6,1023) ZVTXLM,LMHITS,ETOTKP,ETOTCT,ETAGLM,COSCUT,ZMLIM,
     $ RPLIM,RPLIM1,RATLIM,CRVLIM,ZVXLM1,ZVXLM2
1023  FORMAT(' ',F6.1,I8,F8.1,F8.1,F8.1,F8.1,F8.1,F8.3,F8.3,F8.3,F8.5,
     $ F8.1,F8.1)
      WRITE(6,1027) ETCYKP,ETE1KP,ETE2KP,TSUMC1,TSUMC2
1027  FORMAT(' ETCYKP  ETE1KP  ETE2KP  ',3F8.1,' TSUMC1-2 ',2F8.1)
      WRITE(6,1037) LIMHIT,LIMHT1,CRVTAG,CRVNTG
1037  FORMAT(' REDUC1 LIMITS: LIMHIT LIMHT1 CRVTAG * NTG ',2I5,2E12.4)
1021  INDEX=INDEX+1
      RETURN
C -------------------------------------------------------------------
C *******************************************************************
C -------------------------------------------------------------------
200   CONTINUE
C                                                         SELECT EVENT
      IGG(2) = IGG(2) + 1
      NNREC = IGG(2) + JIGG(2)
      IF((NNREC/100)*100.EQ.NNREC) WRITE(6,2702)NNREC,HEAD(18),HEAD(19)
2702  FORMAT(' NNREC RUN EVENT ',3I8)
      IF(IREFRM.EQ.1.AND.HEAD(19).LT.13) GO TO 7171
      IF(HEAD(18).LT.HRUNMI) GO TO 7171
      IF(NNREC.LT.NNRMIN) GO TO 7171
      IF(HEAD(18).GT.HRUNMX) GO TO 12
      IF(NNREC.GT.NNRMAX) GO TO 12
      NMREC = NMREC + 1
      ZVTX = -600.
      IFLAG=-1
      LRJCT = 0
      LWRIT = 0
      CALL RDATA(IER,HEAD(18))
      IF(IER.NE.0) GO TO 1752
      IF(HEAD(20).EQ.1.OR.HEAD(20).EQ.13) GO TO 1751
      IF(HRUNTP.NE.HEAD(18)) WRITE(6,1753) HEAD(18),HEAD(20)
1753  FORMAT(' * * * * RUN NR ',I8,'   HAS RECORD TYPE ',I8)
      HRUNTP = HEAD(18)
      GO TO 1751
1752  LRJCT = 16
      IGG(70) = IGG(70) + 1
      GO TO 1
C--- EVENT NOT REAL DATA  ------------------------>>>>  * REJECT *  <<<<
C                                              ************************
C                                             ***    LRJCT = 16      ***
C                                              ************************
1751  ISTAR = 0
      NTR= 0
C   OVERFLOW MARKER
      IFLW= 0
      IF(HEAD(23).NE.0) IFLW = 1
      IF(IFLW.EQ.1) IGG(3)=IGG(3)+1
      IF(IREFRM.EQ.1) GO TO 2457
      IPPO = IDATA(IBLN('PATR'))
      IF(IPPO.GT.0) GO TO 2457
      ITRG = IDATA(IBLN('TRIG'))
      HELP(2) = HDATA(2*ITRG + IDATA(ITRG)*2)
      IF(ICAMWD.EQ.MSKLUM.OR.ICAMWD.EQ.MSKLM1) GO TO 2457
      WRITE(6,2458) HEAD(18),HEAD(19)
2458  FORMAT(' EVENT WITH NO PATR BANK AT LEVEL 2',2I8)
C
C     RECALIBRATION HERE IF REQUESTED, PARAMETER ICALIB
C
2457  IF(ICALIB.EQ.0.OR.HRUN.LT.100) GO TO 2010
      CALL BDLS('ALGN',1)
      CALL BDLS('LGCL',1)
C     CALL BGAR(IGA)
2010  IF(ICALJC.EQ.0.OR.HRUN.LT.100) GO TO 2011
C     RECALIBRATE JETC BANK
      IPNJET=IDATA(IBLN('JETC'))
      IF(IPNJET.GT.20000) WRITE(6,445) IPNJET,HRUN,HEVENT
445   FORMAT('0UNORTHODOX IPNJET = ',I10,' AT: ',2I8)
      IF(IPNJET.LT.4.OR.IPNJET.GT.20000) GO TO 1
      NUMJET=IDATA(IPNJET-2)
      IPNNEX=IDATA(IPNJET-1)
      IF(IPNNEX.GT.20000) WRITE(6,446) IPNNEX,HRUN,HEVENT
446   FORMAT('0UNORTHODOX IPNNEX = ',I10,' AT: ',2I8)
      IF(IPNNEX.LT.4.OR.IPNNEX.GT.20000) GO TO 1
      NUMNEX=IDATA(IPNNEX-2)
      CALL BDLS('JETC',NUMJET)
      IPNJET=IDATA(IBLN('JETC'))
      IF(IPNJET.GT.20000) WRITE(6,447) IPNJET,HRUN,HEVENT
447   FORMAT('0UNORTHODOX IPNJET (TWO) = ',I10,' AT: ',2I8)
      IF(IPNJET.LT.4.OR.IPNJET.GT.20000) GO TO 1
      CALL BRNM('JETC',NUMNEX,'JETC',NUMJET)
C   REDO PATTERN RECOGNITION IF REQUESTED, PARAMETER IREPAT
2011  IF((IREPAT.NE.1.AND.IREPAT.NE.3).OR.HRUN.LT.100) GO TO 2013
      IPPR =IDATA(IBLN('PATR'))
      IF(IPPR.LE.0) GO TO 2013
      NUMPPR=IDATA(IPPR-2)
      CALL BDLS('PATR',NUMPPR)
2013  INDEX=INDEX+1
      IPPR =IDATA(IBLN('PATR'))
      IF(IPPR.LE.0) IGG(77) = IGG(77) + 1
      RETURN
C -------------------------------------------------------------------
C *******************************************************************
C -------------------------------------------------------------------
300   CONTINUE
C                                           LEAD GLASS CALIBRATION DONE
CALL TRIGGER CHECK, USED IN REDUC1
      IF(IRED1.EQ.0) GO TO 3011
      LBTRBT = 0
      ITRG = IDATA(IBLN('TRIG'))
      IF(ITRG.EQ.0) GO TO 3011
      IF(IDATA(ITRG-2).NE.1) GO TO 3011
      LBTRBT = HDATA(2*ITRG  + 10)
C  SKIP TRGCHK FOR FORWARD MUON TRIGGER
      IMUACC=0
      IF(LAND(LBTRBT,MKFWMU).NE.0) CALL MEWT3(IMUACC)
      IF(LAND(LBTRBT,MKFWM1).NE.0) CALL MEWT3(IMUACC)
      IF(IMUACC.GT.0) GO TO 3011
      CALL TRGCHK(LBTRCK,LBTRBT,IPRN)
      IF(IPRN.GT.0) WRITE(6,3012) LBTRCK,LBTRBT
3012  FORMAT(' TRGCHK RETURN : LBTRCK AND LBTRBT ',I8,1X,Z4)
      IF(LBTRCK.NE.0) GO TO 3011
      IF(IREFRM.EQ.1) GO TO 3013
      IPZV = IDATA(IBLN('ZVTX'))
      IF(IPZV.LE.0) IGG(74) = IGG(74) + 1
      IF(IPZV.LE.0) GO TO 3013
      IFLAG  = IDATA(IPZV+6)
      IF(IFLAG.LT.0) IGG(75) = IGG(75) + 1
      IF(IFLAG.LT.0) GOTO 3013
      ZVTX   = ADATA(IPZV+1)
      CALL ERGTOT(ECYL,ECAMI,ECAPL)
      ETOT = ECYL + ECAMI + ECAPL
      CALL HFILL(57,ZVTX,ETOT,1.)
3013  LRJCT = 18
      IGG(73) = IGG(73) + 1
      GO TO 1
C--- TRIGGER CHECK NOT OK ------------------------>>>>  * REJECT *  <<<<
C                                              ************************
C                                             ***    LRJCT = 18      ***
C                                              ************************
3011  CALL ERGTOT(ECYL,ECAMI,ECAPL)
      ETOT = ECYL + ECAMI + ECAPL
      CALL HFILL(1,ETOT,0,1.)
      CALL HFILL(2,ETOT,0,1.)
      IPPO = IDATA(IBLN('PATR'))
      IF(IPPO.GT.0) GO TO 3532
      ITRG = IDATA(IBLN('TRIG'))
      HELP(2) = HDATA(2*ITRG + IDATA(ITRG)*2)
      IF(ICAMWD.EQ.MSKLUM.OR.ICAMWD.EQ.MSKLM1) GO TO 3532
      IF(IFLW.EQ.1.AND.ETOT.GT.500.) LWRIT = 10
      IF(LWRIT.EQ.10) IGG(71) = IGG(71) + 1
      IF(LWRIT.EQ.10) GO TO 11
C--- NO PATR BANK, HIGH ENERGY, OVERFLOW ---   ********* WRITTEN *******
C                                         --------------------------
C                                       --        LWRIT = 10        --
C                                         --------------------------
C
3532  IF(ETOT.GT.ETOTCT) GO TO 305
      IGG(4) = IGG(4) + 1
      ITRG = IDATA(IBLN('TRIG'))
      IF(ITRG.GT.0) GO TO 327
      WRITE(6,326) HEAD(18),HEAD(19)
326   FORMAT(' RUN * EVENT ',2I8,'  NO TRIGGER BANK ')
      GO TO 305
327   IF(IDATA(ITRG-2).EQ.1) GO TO 328
      WRITE(6,325) HEAD(18),HEAD(19)
325   FORMAT(' RUN * EVENT ',2I8,'  FIRST TRIGGER BANK NOT NR 1 ')
      GO TO 305
328   HELP(2) = HDATA(2*ITRG + IDATA(ITRG)*2)
      IF(ICAMWD.NE.MSKLUM.AND.ICAMWD.NE.MSKLM1) GO TO 305
      IGG(5) = IGG(5) + 1
C--- ENERGY < 100 MEV   LUMI TRIGGER ------------->>>>  * REJECT *  <<<<
C                                              ************************
C                                             ***    LRJCT = 1       ***
C                                              ************************
      LRJCT = 1
      GO TO 1
C  SET FLAG FOR ENERGY IN FORWARD TAGGING BLOCKS
C            IFTG < 11     NO ENERGY
C            IFTG = 11     ENERGY ABOVE LIMIT IN NEG. FW ARM
C            IFTG = 12     ENERGY ABOVE LIMIT IN POS. FW ARM
C            IFTG = 113,13 ENERGY ABOVE LIMIT IN BOTH FW ARMS(LUMI)
305   CALL TAGFLG(IFTG)
      IF(IFTG.GT.10) IGG(6) = IGG(6) + 1
      IF(IFTG.LT.11) IGG(7) = IGG(7) + 1
      IF(IFTG.GT.10.AND.ETOT.GT.ETAGLM) IGG(8) = IGG(8) + 1
      IF(IFTG.GT.10) CALL HFILL(3,ETOT,0,1.)
      IAC = 0
      IF(ETOT.GT.ETOTKP) IAC = 1
      IF(ECYL.GT.ETCYKP) IAC = 1
      IF(ECAMI.GT.ETE1KP.AND.ECYL.GT.ETE2KP) IAC = 1
      IF(ECAPL.GT.ETE1KP.AND.ECYL.GT.ETE2KP) IAC = 1
      IF(IAC.EQ.0) IGG(9) = IGG(9) + 1
C
C TEST HERE WHETHER ALL THE ENERGY (95 %) SITS IN ONE ENDCAP BLOCK.
C IF SO, MARK IT WITH IAC = -1
C PERFORM THIS TEST ONLY IF TOTAL ENERGY HIGHER THAN ETOTLM.
C
      IF(ETOT.LT.ETOTLM) GO TO 301
      IF(ECAMI/ETOT.GT..95.OR.ECAPL/ETOT.GT..95) GO TO 302
      GO TO 301
C CHECK WHETHER MORE THAN 95 % OF ENERGY IN ONE ENDCAP BLOCK
302   IPJ=IDATA(IBLN('ALGN'))
      IF(IPJ.LE.0) GO TO 301
      NWO=IDATA(IPJ)
      IF(NWO.LE.3) GO TO 301
      IGG(10) = IGG(10) + 1
      IPJ=2*IPJ + 8
      NWO=IPJ+2*NWO-8
      DO 303 IJK=IPJ,NWO,2
      IAD=HDATA(IJK-1)
      IF(IAD.LE.2687) GO TO 303
C TEST HERE TO EXCLUDE EDGE BLOCKS FROM THE CHECK
C     REDUCE TO NUMBERS 1 - 192
      NO = IAD - 2687
C     0 FOR -Z, 1 FOR +Z
      NE = (NO - 1)/96
C     REDUCE TO 1 - 96
      NO = NO - NE*96
C     GET QUADRANT NUMBER 0 - 3
      NQ = (NO - 1)/24
C     REDUCE TO 1 - 24
      NO = NO - NQ*24
      IF(NO.LT.5) GO TO 303
      IF(NO.GT.15.AND.NO.NE.20) GO TO 303
C BLOCK NOT AT EDGE, CHECK FURTHER:
      ETEST = HDATA(IJK)
      IF(ETEST/ETOT.LT..95) GO TO 303
      IGG(11) = IGG(11) + 1
      WRITE(6,3002) HEAD(18),HEAD(19),IAD,ETEST,ETOT
3002  FORMAT(' RU*EV '2I8,' BLOCK ',I5,' WITH ENERGY AND ETOT ',2E12.4)
      IAC = -1
      GO TO 301
303   CONTINUE
301   CONTINUE
      IF(IAC.EQ.1) IGG(12) = IGG(12) + 1
      IF(IAC.EQ.-1) CALL HFILL(4,ETOT,0,1.)
CHECK HERE IF IFLW SET, IF SO CHECK IF DUE TO MASSIVE BAD LEAD GLASS
C           OR TO PICKUP EVENT WITH MORE THAN 1200 ID HITS
      IF(IFLW.EQ.0) GO TO 3087
      IDHITS = 0
      IPJETC = IDATA(IBLN('JETC'))
      IF(IPJETC.EQ.0) GO TO 3087
      IPJETC = 2*(IPJETC+1)
      IDHITS = (HDATA(IPJETC+97) - HDATA(IPJETC+1))/4
      IF(IDHITS.GT.800.AND.IDHITS.LT.1200) GO TO 3087
CHECK FIRST IF OVERFLOW EVENT HAS > 1200 HITS AND NO E,THEN REJECT EVENT
      IF(IDHITS.LE.800) GO TO 3057
      IF(ETOT.GT.500.) GO TO 3087
      WRITE(6,3058) NNREC,HEAD(18),HEAD(19),IDHITS,ETOT
3058  FORMAT(' ** OVERFLOW ** RECORD RUN EVENT ',3I8,' WITH INNER DETECT
     $OR HITS AND ETOT = ',I6,E12.4)
C                                 ---------------->>>>  * REJECT *  <<<<
C                                              ************************
C                                             ***    LRJCT = 15      ***
C                                              ************************
      LRJCT = 15
      IGG(65) = IGG(65) + 1
      GO TO 1
3057  IPALGL = IDATA(IBLN('ALGL'))
      IPALGN = IDATA(IBLN('ALGN'))
      IF(IPALGL.GT.0.AND.IPALGN.GT.0) GO TO 3085
      WRITE(6,3084) HEAD(18),HEAD(19)
3084  FORMAT(' RUN*EV,  ALGL AND ALGN DO NOT EXIST ',2I8)
      GO TO 3087
3085  LALGL = IDATA(IPALGL)
      LALGN = IDATA(IPALGN)
      IF(LALGL.GT.10*LALGN) IFLW = 0
      IF(LALGL.GT.10*LALGN) IGG(61) = IGG(61) + 1
3087  IF(IFLW.EQ.1) IGG(62) = IGG(62) + 1
C  SET FLAG FOR LOW OR BAD ENERGY, OVERFLOW, TAGGING
      IWRT = 1
      IF(IAC.LE.0.AND.IFLW.EQ.0.AND.(IFTG.LT.11.OR.ETOT.LT.ETAGLM))
     $ IWRT = 0
      IF(IWRT.EQ.0) IGG(13) = IGG(13) + 1
      IF(IWRT.NE.0) IGG(14) = IGG(14) + 1
      IF(IWRT.NE.0.AND.IFTG.GT.10.AND.ETOT.GT.ETAGLM) IGG(63)=IGG(63)+1
      IF(IAC.NE.-1) CALL HFILL(5,ETOT,0,1.)
      IF(IAC.NE.-1) CALL HFILL(6,ECYL,0,1.)
      IF(IAC.NE.-1.AND.ECAMI.GT.0.) CALL HFILL(7,ECAMI,0,1.)
      IF(IAC.NE.-1.AND.ECAPL.GT.0.) CALL HFILL(8,ECAPL,0,1.)
      INDEX=INDEX+1
      RETURN
C -------------------------------------------------------------------
C *******************************************************************
C -------------------------------------------------------------------
400   CONTINUE
C                                                     ZVERTEX CALCULATED
      IGG(15) = IGG(15) + 1
      IPZV = IDATA(IBLN('ZVTX'))
      IF(IPZV.LE.0) GO TO 401
      IGG(16) = IGG(16) + 1
      IFLAG  = IDATA(IPZV+6)
      IF(IFLAG.LT.0) CALL HFILL(9,ETOT,0,1.)
      IF(IFLAG.LT.0) GOTO 401
      IGG(17) = IGG(17) + 1
      ZVTX   = ADATA(IPZV+1)
      CALL HFILL(10,ZVTX,0,1.)
      CALL HFILL(11,ZVTX,ETOT,1.)
      IF(ETOT.GT.2000.) CALL HFILL(12,ZVTX,0,1.)
      IF(IWRT.EQ.0) CALL HFILL(13,ZVTX,ETOT,1.)
      IF(IWRT.EQ.1) CALL HFILL(14,ZVTX,ETOT,1.)
C
C
C                                 ---------------->>>>  * REJECT *  <<<<
C                                              ************************
C                                             ***    LRJCT = 2       ***
C                                              ************************
      IF(ABS(ZVTX).GT.ZVTXLM.AND.IWRT.NE.1.AND.IFLAG.GT.1) LRJCT = 2
      IF(LRJCT.EQ.2) IGG(60) = IGG(60) + 1
      IF(LRJCT.EQ.2) GO TO 1
      IF(ABS(ZVTX).GT.ZVXLM1.AND.IFLAG.EQ.3.AND.ECAMI.LT..05*ETOT.AND.
     $ ECAPL.LT..05*ETOT) IAC = 0
      IWRT = 1
      IF(IAC.LE.0.AND.IFLW.EQ.0.AND.(IFTG.LT.11.OR.ETOT.LT.ETAGLM))
     $ IWRT = 0
      IF(ABS(ZVTX).GT.ZVTXLM) IGG(18) = IGG(18) + 1
      IGG(19) = IGG(19) + 1
401   INDEX = INDEX + 1
      RETURN
C -------------------------------------------------------------------
C *******************************************************************
C -------------------------------------------------------------------
500   CONTINUE
C                                                       PATREC PERFORMED
      IF(IRED1.EQ.0) GO TO 555
      CALL REDONE(INDRJ1,KBWRT1,KWRT,IPRN)
C                                 ---------------->>>>  * REJECT *  <<<<
C                                                    ******************
C                                                    *   LRJCT = 17   *
C                                                    ******************
      IF(IPRN.GT.0) WRITE(6,1881) INDRJ1,KBWRT1,KWRT
1881  FORMAT(' REDUC1   INDREJ,KBWRT,KWRT ',3I4)
      IF(KBWRT1.EQ.0) LRJCT = 17
      IF(LRJCT.EQ.17) IGG(72) = IGG(72) + 1
      IF(LRJCT.EQ.17) GO TO 1
C
555   IPPATR = IDATA(IBLN('PATR'))
C IF NO PATR BANK, PROCEED TO CLUSTER CHECK   (NTR = 0 AT READ EVENT)
      IF(IPPATR.EQ.0) GO TO 503
      IGG(21) = IGG(21) + 1
      LO = IDATA(IPPATR+1)
      NTR = IDATA(IPPATR+2)
      LTR = IDATA(IPPATR+3)
      IF(NTR.EQ.0) CALL HFILL(16,ETOT,0,1.)
      IF(IFLAG.GE.0.AND.NTR.EQ.0) CALL HFILL(17,ZVTX,0,1.)
C                                        ************* WRITTEN *********
      IF(NTR.EQ.0.AND.IFTG.GT.10.AND.ETOT.GT.50.) LWRIT = 7
      IF(LWRIT.EQ.7) CALL HFILL(53,ETOT,0,1.)
C                                         --------------------------
C                                       --        LWRIT = 7         --
C                                         --------------------------
      IF(LWRIT.EQ.7) IGG(59) = IGG(59) + 1
      IF(LWRIT.EQ.7) GO TO 11
C
      IF(NTR.EQ.0) GO TO 503
      CALL HFILL(15,NTR,0,1.)
      IGG(22) = IGG(22) + 1
      IF(IFTG.GT.10) GO TO 562
      IF(IFLAG.NE.3) GO TO 562
C HERE CHECK EVENTS WITH HIGH ENERGY IN CYLINDER AND ZVTX GT 200.
C  THEY ARE PASSED ON TO NORMAL TRACK CHECK
      IF(ABS(ZVTX).GT.ZVXLM2.AND.ECAMI.LT..05*ETOT.AND.ECAPL.LT..05*ETOT
     $ ) GO TO 578
      IGG(23) = IGG(23) + 1
C  WRITE  1-PRONG  TAGGED EVENTS
C                                        ************* WRITTEN *********
C                                              ------------------------
C                                             ---    LWRIT = 9       ---
C                                              ------------------------
562   IF(NTR.EQ.1.AND.IFTG.GT.10) LWRIT = 9
      IF(LWRIT.EQ.9) IGG(67) = IGG(67) + 1
      IF(LWRIT.EQ.9) GO TO 11
C NOW WRITE ALL EVENTS WITH THE WRITE FLAG IWRT SET
C                                        ************* WRITTEN *********
      IF(IWRT.EQ.1) LWRIT = 1
      IF(IWRT.EQ.1) IGG(24) = IGG(24) + 1
C                                         --------------------------
C                                       --        LWRIT = 1         --
C                                         --------------------------
      IF(IWRT.EQ.1) GO TO 11
C-
C-
578   IGG(25) = IGG(25) + 1
      IO = IPPATR
      IO = IO + LO - LTR
C  EVENTS ARE HERE DIVIDED INTO THREE CLASSES : ISTAR = 0,1,2
C    ISTAR = 0 HAS ONLY SHORT TRACKS   (NR OF ZHITS < LMHITS)
C    ISTAR = 2 HAS LONG AND WEAK TRACKS
C    ISTAR = 1 HAS AT LEAST ONE LONG FAST TRACK
C    ONLY ISTAR = 1 EVENTS UNDERGO SERIOUS TRACK CHECKS
C
      ICNTR = 0
      IGDNTR = 0
      ICNT = 0
      ISTAR = 0
      ICNTS = 0
      THESUM = 0.
501   ICNT = ICNT + 1
      IF(ICNT.GT.NTR) GO TO 502
      IO = IO + LTR
      CALL HFILL(101,IDATA(IO+33),0,1.)
      IF(IPATRJ.EQ.24.AND.IDATA(IO+33).LT.2.AND.IDATA(IO+24).GT.10)
     $ LRJCT = 99
      IF(LRJCT.EQ.99) GO TO 1
      THEX = ATAN(ADATA(IO+30))
      IF(IPATRJ.EQ.24.AND.THEX.EQ.0.) LRJCT = 99
      IF(LRJCT.EQ.99) GO TO 1
      CALL HFILL(102,THEX,0,1.)
      IF(IDATA(IO+33).LE.LMHITS.AND.IDATA(IO+24).LE.LMHITS) GO TO 5027
      IF(ISTAR.EQ.0) ISTAR = 2
      CRV = ADATA(IO+25)
      AZV = ABS(ADATA(IO+31))
      IF(ABS(CRV).GT.CRVLIM) GOTO 5027
C         COMPUTE MINIMUM DISTANCE OF PARABOLA TO ORIGIN, RFI-PLANE
      ISTAR=1
      IGDNTR = IGDNTR + 1
      CALL
     $    PARMIN(ADATA(IO+19),ADATA(IO+20),ADATA(IO+21),ADATA(IO+22),RP,
     $ IDATA(IO+18))
      IF(RP.LT.RPLIM.AND.AZV.LT.ZMLIM) ICNTR = ICNTR + 1
      THE = ATAN(ADATA(IO+30))
      THE = PI*.5 - THE
      IF(NTR.EQ.3) THHELP(ICNT) = THE
      THESUM = THESUM + THE
      IF(RP.LT.RPLIM1.AND.AZV.LT.ZMLIM) ICNTS = ICNTS + 1
      GO TO 501
5027  IF(NTR.NE.3) GO TO 501
      THE = ATAN(ADATA(IO+30))
      THE = PI*.5 - THE
      THHELP(ICNT) = THE
      GO TO 501
502   IF(ISTAR.EQ.0) IGG(26) = IGG(26) + 1
      IF(ISTAR.EQ.1) IGG(27) = IGG(27) + 1
      IF(ISTAR.EQ.2) IGG(28) = IGG(28) + 1
      IF(ISTAR.EQ.0.OR.ISTAR.EQ.2) CALL HFILL(18,ETOT,0,1.)
      IF(ISTAR.EQ.1) CALL HFILL(19,ETOT,0,1.)
C     IF(ISTAR.EQ.2) CALL HFILL(20,ETOT,0,1.)
      IF(ISTAR.EQ.1) GO TO 5035
C-----------------------------------------------------------------------
C    PROCEED TO CLUSTER CHECK      THESE EVENTS HAVE NO FAST TRACKS
      GO TO 503
C-----------------------------------------------------------------------
C
C  ONLY ISTAR = 1 EVENTS PASS THE FOLLOWING TESTS
C
C NOW COMPUTE RATIO BETWEEN TOTAL GOOD TRACKS (STAR = 1 TRACKS) AND
C TRACKS CLOSE TO THE WWP (GIVEN BY RPLIM AND ZMLIM)
C
5035  RATIO = FLOAT(ICNTR)/FLOAT(IGDNTR)
      CALL HFILL(21,RATIO,0,1.)
C     IF(IFLAG.GE.0) CALL HFILL(22,ZVTX,RATIO,1.)
C     CALL HFILL(23,IGDNTR,ICNTR,1.)
C
C  SEPARATE EVENTS WITH RATIO > RATLIM AND   EVENTS WITH RATIO < RATLIM
C
      IF(RATIO.GT.RATLIM) GO TO 517
      IF(RATIO.GT..5*RATLIM.AND.IGDNTR.GT.6) GO TO 517
C
C HERE FOR EVENTS WHICH DO NOT PASS RATIO CHECK - - - - - - - - - - -
      IGG(29) = IGG(29) + 1
      IF(IFLAG.GE.0) CALL HFILL(24,ZVTX,ETOT,1.)
C   NOW REJECT EVENTS IF
C                     OR GOOD ZVERTEX OUTSIDE LIMITS AND >3 GOOD TRACKS
C                     OR RATIO = 0.   AND >10 GOOD TRACKS
      IF(IFLAG.GT.0.AND.ABS(ZVTX).GT.ZVTXLM.AND.IGDNTR.GT.3) LRJCT = 3
      IF(LRJCT.EQ.3) GO TO 5038
      IF(RATIO.EQ.0..AND.IGDNTR.GT.10) LRJCT = 4
      IF(LRJCT.EQ.4) GO TO 5038
      IF(NTR.EQ.2) GO TO 4923
      IF(NTR.EQ.3) GO TO 517
C   OTHERWISE PROCEED TO CLUSTER CHECK
      GO TO 503
C-----------------------------------------------------------------------
C                                        >>>>>>>>>>> REJECT <<<<<<<<<
C                                              ************************
C                                             ***    LRJCT = 3 AND 4 ***
C                                              ************************
5038  IGG(32) = IGG(32) + 1
      GO TO 1
C-----------------------------------------------------------------------
C
C FOR TWOPRONG COSMIC CANDIDATES, PERFORM TOF CHECK AND
C                   PERFORM SPECIAL INTERACTION POINT TEST  (.5*RPLIM )
517   IF(NTR.NE.3) GO TO 5217
      ICOSM = 0
      ABS12 = ABS(THHELP(1)+THHELP(2)-PI)
      ABS13 = ABS(THHELP(1)+THHELP(3)-PI)
      ABS23 = ABS(THHELP(2)+THHELP(3)-PI)
      ABSM12 = ABS(THHELP(1)-THHELP(2))
      ABSM13 = ABS(THHELP(1)-THHELP(3))
      ABSM23 = ABS(THHELP(2)-THHELP(3))
      IF(IPRN.GE.2) WRITE(6,7574) ABS12,ABS13,ABS23,ABSM12,ABSM13,ABSM2300069600
7574  FORMAT(' ABS12 13 23 ABSM ',6F8.4)
      IF(ABS12.LT..25.AND.(ABSM13.LT..31.OR.ABSM23.LT..31)) GO TO 4924
      IF(ABS13.LT..25.AND.(ABSM23.LT..31.OR.ABSM12.LT..31)) GO TO 4924
      IF(ABS23.LT..25.AND.(ABSM13.LT..31.OR.ABSM12.LT..31)) GO TO 4924
      IF(RATIO.LT.RATLIM) GO TO 503
C---------------------------------------------******* WRITTEN **********
5217  IF(NTR.NE.2) LWRIT = 2
C                                         --------------------------
C                                       --        LWRIT = 2         --
C                                         --------------------------
      IF(NTR.NE.2) CALL HFILL(25,ZVTX,ETOT,1.)
      IF(LWRIT.EQ.2) IGG(34) = IGG(34) + 1
      IF(LWRIT.EQ.2) GO TO 11
      IGG(35) = IGG(35) + 1
C
4923  ICOSM = 0
C TEST ON COLLINEARITY IN THETA
      ABSTHE = ABS(THESUM-PI)
      CALL HFILL(28,ABSTHE,0,1.)
      CALL HFILL(26,ZVTX,ETOT,1.)
C---------------------------------------------******* WRITTEN **********
      IF(ABSTHE.GT..25.AND.RATIO.GT.RATLIM) LWRIT = 3
C                                         --------------------------
C                                       --        LWRIT = 3         --
C                                         --------------------------
      IF(LWRIT.EQ.3) IGG(33) = IGG(33) + 1
      IF(LWRIT.EQ.3) GO TO 11
      IF(ABSTHE.GT..25) GO TO 503
      CALL HFILL(27,ZVTX,ETOT,1.)
C
4924  IGG(36) = IGG(36) + 1
C PERFORM TOF TEST ONLY IF ETOT < 800 MEV, TO AVOID BREMS STRAHLUNG
      IF(ETOT.GT.800.) GO TO 5123
      IGG(37) = IGG(37) + 1
      ICOSM = 1
C
C SPECIALLY HARD CHECK FOR COSMIC CANDIDATES ( TWOPRONG )
CHECK TOF FOR COSMIC.  CODE TAKEN FROM NORD 50; P.DITTMANNS PROGRAM
4925  CALL TOFCHK(TDIF,TSUM,NRUN)
      NCOSM = 0
      CALL HFILL(29,TDIF,0,1.)
      CALL HFILL(54,TSUM,0,1.)
      CALL HFILL(55,TDIF,TSUM,1.)
C TIME IN NANOSECONDS
C-----------------------------------------------------------------------
C                                        >>>>>>>>>>>> REJECT <<<<<<<<<<<
      IF(TDIF.GT.COSCUT.AND.TSUM.GT.TSUMC1) NCOSM = 1
      IF(TDIF.GT.COSCUT.AND.TSUM.LT.TSUMC2) NCOSM = 1
      IF(NCOSM.EQ.1) IGG(42) = IGG(42) + 1
      IF(NCOSM.EQ.1.AND.IFLAG.GE.0) CALL HFILL(30,ZVTX,ETOT,1.)
      IF(IPATRJ.EQ.25.OR.IPATRJ.EQ.26) NCOSM = 0
      IF(NCOSM.EQ.1) LRJCT = 5
C                                              ************************
C                                             ***    LRJCT = 5       ***
C                                              ************************
      IF(NCOSM.EQ.1) GO TO 1
C-----------------------------------------------------------------------
C    NEW RATIO FOR COSMIC CANDIDATES      EXTRA STRONG DEMAND ON RPLIM
5123  IF(ICNTS.GT.0) IGG(43) = IGG(43) + 1
      IF(IFLAG.LT.0) GO TO 5031
      IF(ICNTS.GT.0.AND.IFLAG.GE.0) CALL HFILL(31,ZVTX,ETOT,1.)
C-----------------------------------------------------------------------
C---------------------------------------------******* WRITTEN **********
5031  IF(ICNTS.GT.0) LWRIT = 4
C                                         --------------------------
C                                       --        LWRIT = 4         --
C                                         --------------------------
      IF(ICNTS.GT.0.AND.ICOSM.EQ.1) CALL HFILL(32,ZVTX,ETOT,1.)
      IF(ICNTS.GT.0) GO TO 11
C-----------------------------------------------------------------------
C
C    NOW REJECT FAILING RATIO EVENTS
C--------------------------------------------->>>>>>>> REJECT <<<<<<<<<<
      IGG(44) = IGG(44) + 1
      IF(IFLAG.GE.0) CALL HFILL(33,ZVTX,ETOT,1.)
      LRJCT = 6
C                                              ************************
C                                             ***    LRJCT = 6       ***
C                                              ************************
      GO TO 1
C-----------------------------------------------------------------------
503   IGG(45) = IGG(45) + 1
      INDEX=INDEX+1
      RETURN
C -------------------------------------------------------------------
C *******************************************************************
C -------------------------------------------------------------------
600   CONTINUE
C                                    CLUSTER ANALYSIS PERFORMED
      IGG(46) = IGG(46) + 1
      IPCL = IDATA(IBLN('LGCL'))
C-----------------------------------------------------------------------
C--------------------------------------------->>>>>>>> REJECT <<<<<<<<<<
      IF (IPCL.EQ.0) LRJCT = 7
C                                              ************************
C                                             ***    LRJCT = 7       ***
C                                              ************************
      IF (IPCL.EQ.0) GO TO 1
      IGG(47) = IGG(47) + 1
      IER = IDATA(IPCL+20)
C--------------------------------------------->>>>>>>> REJECT <<<<<<<<<<
      IF(IER.NE.0) LRJCT = 8
C                                              ************************
C                                             ***    LRJCT = 8       ***
C                                              ************************
      IF(IER.NE.0) GO TO 1
      IGG(48) = IGG(48) + 1
      NCLST = IDATA(IPCL+7)
      CALL HFILL(34,NCLST,0,1.)
      IF(NCLST.LE.1) CALL HFILL(35,ETOT,0,1.)
C--------------------------------------------->>>>>>>> REJECT <<<<<<<<<<
      IF(NCLST.LE.1) LRJCT = 9
C                                              ************************
C                                             ***    LRJCT = 9       ***
C                                              ************************
      IF(LRJCT.EQ.9) GO TO 1
      IGG(49) = IGG(49) + 1
C-----------------------------------------------------------------------
C     IF(NCLST.LE.1) LWRIT = 8
C     ITRG = IDATA(IBLN('TRIG'))
C     HELP(2) = HDATA(2*ITRG + IDATA(ITRG)*2)
C     IF(LWRIT.EQ.8.AND.ICAMWD.EQ.MKFWMU) LRJCT = 14
C     IF(LWRIT.EQ.8.AND.ICAMWD.EQ.MKFWMU) LWRIT = 0
C                                         --------------------------
C                                       --        LWRIT = 8         --
C                                         --------------------------
C     IF(LWRIT.EQ.8) GO TO 11
C     IGG(50) = IGG(50) + 1
C-----------------------------------------------------------------------
      EBM = EBEAM(HRUN)
      IF(NTR.NE.0) GO TO 519
      IGG(66) = IGG(66) + 1
      IF(IWRT.NE.0) IGG(68) = IGG(68) + 1
      IF(IWRT.NE.0.AND.IFTG.GT.10.AND.ETOT.GT.ETAGLM) IGG(69)=IGG(69)+1
C                ENTER HERE FOR EVENTS WITH NO TRACKS
      IALGN = IDATA(IBLN('ALGN'))
      IF(IALGN.EQ.0) IGG(51) = IGG(51) + 1
C--------------------------------------------->>>>>>>> REJECT <<<<<<<<<<
      IF(IALGN.EQ.0) LRJCT = 10
C                                              ************************
C                                             ***    LRJCT = 10      ***
C                                              ************************
      IF(IALGN.EQ.0) GO TO 1
C- NEUTRAL EVENTS ARE NOW CHECKED FOR MOMENTUM BALANCE -----------------
      CALL HWORLD(IALGN,JEMPTY,UNBAL)
C--------------------------------------------->>>>>>>> REJECT <<<<<<<<<<
      IF(JEMPTY.GT.1.OR.UNBAL.LT..05) LRJCT = 11
      IF(LRJCT.EQ.11) IGG(52) = IGG(52) + 1
      IF(LRJCT.EQ.11) CALL HFILL(36,ETOT,0,1.)
C                                              ************************
C                                             ***    LRJCT = 11      ***
C                                              ************************
      IF(LRJCT.EQ.11) GO TO 1
C-----------------------------------------------------------------------
C
      IDHITS = 0
      IPJETC = IDATA(IBLN('JETC'))
      IF(IPJETC.EQ.0) GO TO 519
      IPJETC = 2*(IPJETC+1)
      IDHITS = (HDATA(IPJETC+97) - HDATA(IPJETC+1))/4
5191  IF(IDHITS.GT.1000) IGG(53) = IGG(53) + 1
C-----------------------------------------------------------------------
C--------------------------------------------->>>>>>>> REJECT <<<<<<<<<<
C  NO TRACKS BUT MORE THAN 1000 HITS IN I.D.
      IF(IDHITS.GT.1000) LRJCT = 12
C                                              ************************
C                                             ***    LRJCT = 12      ***
C                                              ************************
      IF(IDHITS.GT.1000) GO TO 1
C-----------------------------------------------------------------------
C  WRITE HERE NEUTRAL EVENTS WITH ETOT < 3.*EBM AND < 10 CLUSTERS
C---------------------------------------------******** WRITTEN *********
      IF(ETOT.LT.3.*EBM.AND.NCLST.LT.10) LWRIT = 6
      ITRG = IDATA(IBLN('TRIG'))
      HELP(2) = HDATA(2*ITRG + IDATA(ITRG)*2)
      IF(LWRIT.EQ.6.AND.(ICAMWD.EQ.MKFWMU.OR.ICAMWD.EQ.MKFWM1))
     $ LWRIT = 0
C                                         --------------------------
C                                       --        LWRIT = 6         --
C                                         --------------------------
      IF(LWRIT.EQ.6) IGG(58) = IGG(58) + 1
      IF(LWRIT.EQ.6) GO TO 11
C--
519   IP3 = IDATA(IPCL+3)
      NPWCL = IDATA(IPCL+25)
      MARK = 0
      NNCL = NCLST - 1
C                          SEARCH HERE FOR COLLINEAR CLUSTERS
      DO 610  I = 1,NNCL
      IB1 = IPCL + IP3 + (I-1)*NPWCL - 1
      IF(ADATA(IB1+2).LT.ERGL) GO TO 610
      JPRT1 = IDATA(IB1+1)
      NN = I + 1
      DO 611  J = NN,NCLST
      IB2 = IPCL + IP3 + (J-1)*NPWCL - 1
      JPRT2 = IDATA(IB2+1)
      IF(JPRT1+JPRT2.NE.0) GO TO 611
      IF(ADATA(IB2+2).LT.ERGL) GO TO 611
      IF(JPRT1.NE.0) GO TO 612
C
C  CHECK OF TWO CLUSTERS IN BARREL
C
      FI1 = ADATA(IB1+4)
      FI2 = ADATA(IB2+4)
      ZE1 = ADATA(IB1+5)
      ZE2 = ADATA(IB2+5)
      ABF = ABS(FI2-FI1)
      ABZ = ABS(ZE1+ZE2)
      IF(ABZ.GT.ZLM) GO TO 611
      IF(ABF.LT.PI-FIDEL.OR.ABF.GT.PI+FIDEL) GO TO 611
C TOTALLY NEUTRAL COLLINEARS MUST HAVE MORE THAN 7 % OF ETOT
      IF(NTR.EQ.0.AND.ADATA(IB1+2)+ADATA(IB2+2).LT..00007*ETOT)GO TO 61100090800
      MARK = 1
      IGG(54) = IGG(54) + 1
      THE1 = ATAN2(ZE1,RLG)
      THE2 = ATAN2(ZE2,RLG)
      THE1 = PI*.5 - THE1
      THE2 = PI*.5 - THE2
      GO TO 619
C
C  CHECK OF TWO CLUSTERS IN ENDCAPS
C
612   X1 = ADATA(IB1+4)
      X2 = ADATA(IB2+4)
      Y1 = ADATA(IB1+5)
      Y2 = ADATA(IB2+5)
      ABX = ABS(X1+X2)
      ABY = ABS(Y1+Y2)
      IF(ABX.GT.XLM) GO TO 611
      IF(ABY.GT.YLM) GO TO 611
C TOTALLY NEUTRAL COLLINEARS MUST HAVE MORE THAN 7 % OF ETOT
      IF(NTR.EQ.0.AND.ADATA(IB1+2)+ADATA(IB2+2).LT..00007*ETOT)GO TO 61100092800
      MARK = 1
      FI1 = ATAN2(Y1,X1)
      IF(FI1.LT.0.) FI1 = FI1 + TWOPI
      FI2 = ATAN2(Y2,X2)
      IF(FI2.LT.0.) FI2 = FI2 + TWOPI
      RR = SQRT(X1*X1 + Y1*Y1)
      THE1 = ATAN2(RR,ZENDPL)
      IF(JPRT1.LT.0) THE1 = PI - THE1
      RR = SQRT(X2*X2 + Y2*Y2)
      THE2 = ATAN2(RR,ZENDPL)
      IF(JPRT2.LT.0) THE2 = PI - THE2
      IGG(55) = IGG(55) + 1
      GO TO 619
C
611   CONTINUE
C
610   CONTINUE
      IGG(56) = IGG(56) + 1
      CALL HFILL(37,ETOT,0,1.)
      IF(IFLAG.GE.0) CALL HFILL(38,ZVTX,ETOT,1.)
C--------------------------------------------->>>>>>>> REJECT <<<<<<<<<<
      LRJCT = 13
C                                              ************************
C                                             ***    LRJCT = 13      ***
C                                              ************************
      GO TO 1
C
C
619   IGG(64) = IGG(64) + 1
      ICOSM = 0
C IF COLLINEARS FOUND IN FEWPRONG EVENT AND LOW ENERGY, CHECK TOF.
      IF(NTR.GT.2.AND.NTR.LT.8.AND.ETOT.LT.700..AND.ISTAR.EQ.1)
     $ GO TO 4925
      IGG(57) = IGG(57) + 1
      CALL HFILL(39,ETOT,0,1.)
      IF(IFLAG.GE.0) CALL HFILL(40,ZVTX,ETOT,1.)
      IF(NTR.GT.0) CALL HFILL(41,NTR,0,1.)
      COSS = SIN(THE1)*SIN(THE2)*(COS(FI1)*COS(FI2)+SIN(FI1)*SIN(FI2))
     $  + COS(THE1)*COS(THE2)
      CALL HFILL(49,COSS,0,1.)
C---------------------------------------------******** WRITTEN *********
      LWRIT = 5
C                                         --------------------------
C                                       --        LWRIT = 5         --
C                                         --------------------------
      GO TO 11
C
C
C -------------------------------------------------------------------
C *******************************************************************
C -------------------------------------------------------------------
700   CONTINUE
      INDEX=INDEX+1
      RETURN
C -------------------------------------------------------------------
800   CONTINUE
      INDEX=INDEX+1
      RETURN
C -------------------------------------------------------------------
900   CONTINUE
      INDEX=INDEX+1
      RETURN
C -------------------------------------------------------------------
1000  CONTINUE
      INDEX=INDEX+1
      RETURN
C -------------------------------------------------------------------
C---     END OF JOB: FINAL CALCULATIONS + PRINTOUT
9900  CONTINUE
      DO 9781 I = 2,80
 9781 JIGG(I) = JIGG(I) + IGG(I)
      PERCEN = FLOAT(JIGG(2) - JIGG(70))
      PERCEN = FLOAT(NRWRIT)/PERCEN*100.
      WRITE(6,1101) PERCEN
1101  FORMAT(' TOTAL REDUCTION FACTOR ',F7.2,' % ')
      WRITE(6,1717) (IGG(I),I=1,80)
 1717 FORMAT(' COUNTERS ',10I8)
C     CALL STATR2
      WRITE(6,1718) (JIGG(I),I=1,80)
 1718 FORMAT(' TOTAL COUNTS ',10I8)
      DO 1719  I = 1,80
 1719 IGG(I) = JIGG(I)
C     IF(IPRN.GE.2) IPRN = 1
      CALL STATR2
      IF(IHIST.EQ.1) CALL HISTDO
      IF(ITMLOG.EQ.1) CALL TMLOG(0)
      RETURN
C---
C---     RETURNS FOR STEERING ANALYSIS TO DESIRED NEXT STEP.
C---     'GO TO 1' MEANS REJECT EVENT AND GO TO NEXT EVENT.
C---     'GO TO 11' MEANS ACCEPT EVENT, WRITE IT AND GO TO NEXT EVENT
C---
1     IF(IREFRM.EQ.1.AND.(LRJCT.EQ.1.OR.LRJCT.EQ.15.OR.LRJCT.EQ.18))
     $  GO TO 7172
      IF(IFLAG.GE.0) CALL HFILL(42,ZVTX,ETOT,1.)
      IF(IFLAG.GE.0) CALL HFILL(52,ZVTX,0,1.)
      IF(IFLAG.GE.0.AND.LRJCT.EQ.1) CALL HFILL(61,ZVTX,0,1.)
      IF(IFLAG.GE.0.AND.LRJCT.EQ.2) CALL HFILL(62,ZVTX,0,1.)
      IF(IFLAG.GE.0.AND.LRJCT.EQ.3) CALL HFILL(63,ZVTX,0,1.)
      IF(IFLAG.GE.0.AND.LRJCT.EQ.4) CALL HFILL(64,ZVTX,0,1.)
      IF(IFLAG.GE.0.AND.LRJCT.EQ.5) CALL HFILL(65,ZVTX,0,1.)
      IF(IFLAG.GE.0.AND.LRJCT.EQ.6) CALL HFILL(66,ZVTX,0,1.)
      IF(IFLAG.GE.0.AND.(LRJCT.EQ.7.OR.LRJCT.EQ.8))
     $ CALL HFILL(67,ZVTX,0,1.)
      IF(IFLAG.GE.0.AND.LRJCT.EQ.9) CALL HFILL(69,ZVTX,0,1.)
      IF(IFLAG.GE.0.AND.LRJCT.EQ.10) CALL HFILL(70,ZVTX,0,1.)
      IF(IFLAG.GE.0.AND.LRJCT.EQ.11) CALL HFILL(71,ZVTX,0,1.)
      IF(IFLAG.GE.0.AND.LRJCT.EQ.12) CALL HFILL(72,ZVTX,0,1.)
      IF(IFLAG.GE.0.AND.LRJCT.EQ.13) CALL HFILL(73,ZVTX,0,1.)
      IF(IFLAG.GE.0.AND.LRJCT.EQ.17) CALL HFILL(75,ZVTX,0,1.)
      IF(IFLAG.GE.0.AND.LRJCT.EQ.18) CALL HFILL(76,ZVTX,0,1.)
C     LHEAD = HEAD(35)
C     CALL HFILL(50,LHEAD,0.,1.)
      CALL HFILL(43,ETOT,0.,1.)
7172  CALL HFILL(44,LRJCT,0,1.)
      IGG(79) = IGG(79) + 1
      IF(IPRN.GT.0)
     $ WRITE(6,7739) HEAD(18),HEAD(19),NNREC,LRJCT
7739  FORMAT('  RUN*EV  ',2I8,30X,' REC AND CODE ',2I5,' >>> RJCT <<< ')
      IF(IPATRJ.EQ.11) GO TO 1111
      IF(IPATRJ.EQ.22.AND.LRJCT.EQ.8) GO TO 1111
      IF(IPATRJ.EQ.23.AND.LRJCT.EQ.17) GO TO 1111
      IF(IPATRJ.EQ.23.AND.LRJCT.EQ.18) GO TO 1111
      IF(IPATRJ.EQ.24.AND.LRJCT.EQ.99) GO TO 1111
      IF(IPATRJ.EQ.26.AND.LRJCT.EQ.11) GO TO 1111
      IF(IPATRJ.EQ.27.AND.LRJCT.EQ.5) GO TO 1111
7171  INDEX = 1
      RETURN
2     INDEX = 2
      RETURN
3     INDEX = 3
      RETURN
4     INDEX = 4
      RETURN
5     INDEX = 5
      RETURN
6     INDEX = 6
      RETURN
7     INDEX = 7
      RETURN
8     INDEX = 8
      RETURN
9     INDEX = 9
      RETURN
10    INDEX = 10
      RETURN
11    IGG(80) = IGG(80) + 1
      IF(IPRN.GT.0) WRITE(6,7738) HEAD(18),HEAD(19),NNREC,LWRIT
C     LHEAD = HEAD(35)
C     CALL HFILL(51,LHEAD,0.,1.)
      IF(IFLAG.GE.0) CALL HFILL(45,ZVTX,ETOT,1.)
      IF(IFLAG.GE.0) CALL HFILL(46,ZVTX,0.,1.)
      IF(IFLAG.GE.0.AND.LWRIT.EQ.1) CALL HFILL(81,ZVTX,0,1.)
      IF(IFLAG.GE.0.AND.LWRIT.EQ.2) CALL HFILL(82,ZVTX,0,1.)
      IF(IFLAG.GE.0.AND.LWRIT.EQ.3) CALL HFILL(83,ZVTX,0,1.)
      IF(IFLAG.GE.0.AND.LWRIT.EQ.4) CALL HFILL(84,ZVTX,0,1.)
      IF(IFLAG.GE.0.AND.LWRIT.EQ.5) CALL HFILL(85,ZVTX,0,1.)
      IF(IFLAG.GE.0.AND.LWRIT.EQ.6) CALL HFILL(86,ZVTX,0,1.)
      IF(IFLAG.GE.0.AND.LWRIT.EQ.7) CALL HFILL(87,ZVTX,0,1.)
      IF(IFLAG.GE.0.AND.LWRIT.EQ.8) CALL HFILL(88,ZVTX,0,1.)
      IF(IFLAG.GE.0.AND.LWRIT.EQ.9) CALL HFILL(89,ZVTX,0,1.)
      CALL HFILL(47,ETOT,0.,1.)
      CALL HFILL(48,NTR,0.,1.)
      LWRIT = LWRIT + 30
      CALL HFILL(44,LWRIT,0.,1.)
      IF(IGG(80)+JIGG(80).GT.MAXEVS) GO TO 12
      IF(IPATRJ.EQ.11) GO TO 7171
      IF(IPATRJ.EQ.22) GO TO 7171
      IF(IPATRJ.EQ.23) GO TO 7171
      IF(IPATRJ.EQ.24) GO TO 7171
      IF(IPATRJ.EQ.26) GO TO 7171
      IF(IPATRJ.EQ.27) GO TO 7171
      IF(IPATRJ.EQ.28.AND.LWRIT.NE.34) GO TO 7171
7738  FORMAT('  RUN*EV  ',2I8,40X,' REC AND CODE ',2I5,'*** WRIT *** ')
      IF((IREPAT.NE.2.AND.IREPAT.NE.3).OR.HRUN.LT.100) GO TO 1111
      IPPR =IDATA(IBLN('PATR'))
      IF(IPPR.LE.0) GO TO 1174
      NUMPPR=IDATA(IPPR-2)
      CALL BDLS('PATR',NUMPPR)
1174  CALL BGAR(IBAGA)
      IPFAST = 2
      CALL INPATR
      CALL PATREC(0)
      IPFAST = 0
      CALL INPATR
      ZFITLM(1) = 70.
      ZFITLM(2) = 35.
1111  INDEX = 11
      RETURN
12    INDEX = 12
      RETURN
      END
      SUBROUTINE STATR2
      COMMON /CIGG/ IPRN,IGG(80)
C  STATUS FOR DATA REDUCTION STEP 2
      DATA ETAGLM /100./ ,RATLIM /.20/ ,ZVTXLM /350./,ETOTCT/100./
      WRITE(6,1) IGG(1)
  1    FORMAT(' ',25X,'++++++++   STATISTICS FOR RUN',I7,'         +++++
     +++++++++')
      WRITE(6,2) IGG(2),IGG(3),IGG(62)
  2   FORMAT('   EVENTS READ ',I8,'   OVERFLOW EVENTS',I8,'   OVERFLOW E
     $VENTS AFTER LEAD GLASS AND PICKUP CHECK ',I8)
      WRITE(6,222) IGG(70)
222   FORMAT('   EVENTS REJECTED AS NOT BEING REAL DATA ---->>',I8)
      EVS=FLOAT(IGG(2)-IGG(70))
      IF(EVS.EQ.0.) GO TO 2347
      GFAIL=FLOAT(IGG(73))/EVS*100.
      WRITE(6,765) IGG(73),GFAIL
765   FORMAT(' ',I10,' EVENTS FAILING TRIGGER CHECK IN REDUC1  ',F10.2
     +,'%','               >>>>> REJECTED <<<<<')
      GFAIL=FLOAT(IGG(65))/EVS*100.
      WRITE(6,665) IGG(65),GFAIL
665   FORMAT(' ',I10,' EVENTS WITH > 1200 IDHITS, < 500 MEV    ',F10.2
     +,'%','               >>>>> REJECTED <<<<<')
      GFAIL=FLOAT(IGG(5))/EVS*100.
      WRITE(6,5) IGG(5),ETOTCT,GFAIL
 5    FORMAT(' ',I10,' LUMI   WITH TOTAL ENERGY < ',F10.2,'     ',F10.2
     +,'%','               >>>>> REJECTED <<<<<')
      IVAR = IGG(60)
      GFAIL=FLOAT(IVAR)/EVS*100.
      WRITE(6,56) IVAR,ZVTXLM,GFAIL
 56   FORMAT(' ',I10,' EVENTS WITH ZVTX  > ',F10.2,' AND IWRT=0 ',F10.2
     +,'%','               >>>>> REJECTED <<<<<')
      IVAR = IGG(72)
      GFAIL=FLOAT(IVAR)/EVS*100.
      WRITE(6,132) IVAR,GFAIL
 132   FORMAT(' ',I10,' EVENTS REJECTED BY NEW REDUC1 STEP       ',F10.200115500
     +,'%','               >>>>> REJECTED <<<<<')
      IVAR = IGG(32)
      GFAIL=FLOAT(IVAR)/EVS*100.
      WRITE(6,57) IVAR,GFAIL
 57   FORMAT(' ',I10,' EVENTS WITH BAD RATIO, SPECIAL CONDITIONS ',F10.200116000
     +,'%','               >>>>> REJECTED <<<<<')
      IVAR = IGG(42)
      GFAIL=FLOAT(IVAR)/EVS*100.
      WRITE(6,58) IVAR,GFAIL
 58   FORMAT(' ',I10,' EVENTS LABELLED AS COSMICS                ',F10.200116500
     +,'%','               >>>>> REJECTED <<<<<')
      IVAR = IGG(44)
      GFAIL=FLOAT(IVAR)/EVS*100.
      WRITE(6,59) IVAR,GFAIL
 59   FORMAT(' ',I10,' EVENTS FAILING RATIO AND WITH FEW PRONGS  ',F10.200117000
     +,'%','               >>>>> REJECTED <<<<<')
      IVAR = IGG(46) - IGG(47)
      GFAIL=FLOAT(IVAR)/EVS*100.
      WRITE(6,60) IVAR,GFAIL
 60   FORMAT(' ',I10,' EVENTS WITH NO BANK LGCL                  ',F10.200117500
     +,'%','               >>>>> REJECTED <<<<<')
      IVAR = IGG(47) - IGG(48)
      GFAIL=FLOAT(IVAR)/EVS*100.
      WRITE(6,61) IVAR,GFAIL
 61   FORMAT(' ',I10,' EVENTS WITH NONZERO ERRORCODE IN LGCL     ',F10.200118000
     +,'%','               >>>>> REJECTED <<<<<')
      IVAR = IGG(48) - IGG(49)
      GFAIL=FLOAT(IVAR)/EVS*100.
      WRITE(6,62) IVAR,GFAIL
 62   FORMAT(' ',I10,' EVENTS WITH < 2 CLUSTERS,NO GOOD TRACKS   ',F10.200118500
     +,'%','               >>>>> REJECTED <<<<<')
      IVAR = IGG(51)
      GFAIL=FLOAT(IVAR)/EVS*100.
      WRITE(6,63) IVAR,GFAIL
 63   FORMAT(' ',I10,' EVENTS WITH NO BANK ALGN                  ',F10.200119000
     +,'%','               >>>>> REJECTED <<<<<')
      IVAR = IGG(52)
      GFAIL=FLOAT(IVAR)/EVS*100.
      WRITE(6,64) IVAR,GFAIL
 64   FORMAT(' ',I10,' EVENTS WITH NTR=0, HWORLD IMBALANCE       ',F10.200119500
     +,'%','               >>>>> REJECTED <<<<<')
      IVAR = IGG(53)
      GFAIL=FLOAT(IVAR)/EVS*100.
      WRITE(6,65) IVAR,GFAIL
 65   FORMAT(' ',I10,' EVENTS WITH NTR=0, > 1000 IDHITS          ',F10.200120000
     +,'%','               >>>>> REJECTED <<<<<')
      IVAR = IGG(56)
      GFAIL=FLOAT(IVAR)/EVS*100.
      WRITE(6,66) IVAR,GFAIL
 66   FORMAT(' ',I10,' EVENTS WITH NO COLLINEARS FOUND           ',F10.200120500
     +,'%','               >>>>> REJECTED <<<<<')
      WRITE(6,1119)
1119  FORMAT(' ',90(1H-))
      EVSW = IGG(80)
      GOOD=EVSW/EVS*100.
      WRITE(6,86) IGG(80),GOOD
 86   FORMAT(' ',I10,' EVENTS WRITTEN                            ',F10.200121200
     +,'%','            OF ALL READ EVENTS')
      IF(IGG(80).LE.0) GO TO 2347
      IVAR = IGG(24)
      GOOD=FLOAT(IVAR)/EVSW*100.
      WRITE(6,7) IVAR,GOOD
 7    FORMAT(' ',I10,' EVENTS WITH WRITE FLAG SET, NTR > 0       ',F10.200121800
     $,'%','                     ****** WRITTEN *****')
      IVAR = IGG(71)
      GOOD=FLOAT(IVAR)/EVSW*100.
      WRITE(6,1161) IVAR,GOOD
1161   FORMAT(' ',I10,' EVENTS WITH ETOT>500, OVERFLOW, NO PATR  ',F10.200122300
     $,'%','                     ****** WRITTEN *****')
      IVAR = IGG(67)
      GOOD=FLOAT(IVAR)/EVSW*100.
      WRITE(6,161) IVAR,GOOD
 161   FORMAT(' ',I10,' EVENTS WITH NTR=1 AND TAGFLAG SET        ',F10.200122800
     $,'%','                     ****** WRITTEN *****')
      IVAR = IGG(34)
      GOOD=FLOAT(IVAR)/EVSW*100.
      WRITE(6,71) IVAR,GOOD
 71   FORMAT(' ',I10,' EVENTS WITH GOOD RATIO, NTR > 3           ',F10.200123300
     $,'%','                     ****** WRITTEN *****')
      IVAR = IGG(33)
      GOOD=FLOAT(IVAR)/EVSW*100.
      WRITE(6,72) IVAR,GOOD
 72   FORMAT(' ',I10,' EVENTS WITH GOOD RATIO, NTR=2,3 SUMTHE<PI ',F10.200123800
     $,'%','                     ****** WRITTEN *****')
      IVAR = IGG(43)
      GOOD=FLOAT(IVAR)/EVSW*100.
      WRITE(6,73) IVAR,GOOD
 73   FORMAT(' ',I10,' EVENTS WITH RATIO OK, FEWPRONG,NO COSMICS ',F10.200124300
     $,'%','                     ****** WRITTEN *****')
      IVAR = IGG(57)
      GOOD=FLOAT(IVAR)/EVSW*100.
      WRITE(6,74) IVAR,GOOD
 74   FORMAT(' ',I10,' EVENTS WITH COLLINEARS FOUND              ',F10.200124800
     $,'%','                     ****** WRITTEN *****')
      IVAR = IGG(58)
      GOOD=FLOAT(IVAR)/EVSW*100.
      WRITE(6,741) IVAR,GOOD
 741  FORMAT(' ',I10,' EVENTS WITH ETOT<3*EBM, NCLST<10, NTR=0   ',F10.200125300
     $,'%','                     ****** WRITTEN *****')
      IVAR = IGG(59)
      GOOD=FLOAT(IVAR)/EVSW*100.
      WRITE(6,742) IVAR,GOOD
 742  FORMAT(' ',I10,' EVENTS WITH TAGFLAG SET AND NTR=0         ',F10.200125800
     $,'%','                     ****** WRITTEN *****')
2347  RETURN
      END
      SUBROUTINE TOFCHK(TDIF,TSUM,NRUN)
      IMPLICIT INTEGER*2 (H)
      COMMON/SMPTOF/MTOF,TTOF(42),NTOF,HTOF(42)
      TDIF = 0.
      TSUM = 0.
      CALL TOFSMP(NRUN)
      IF(NTOF.LE.0) GO TO 5123
      NRS = 0
      DO 32 JK = 1,21
      IF(HTOF(JK).EQ.0.OR.ABS(TTOF(JK)).EQ.0.) GO TO 32
      IF(TTOF(JK).EQ.-20.) GO TO 32
      TOF1 = TTOF(JK)
      JK1 = JK + 18
      NRT = 0
      TOF2 = 0.
      DO 33 IBT = 1,5
      JK2 = JK1 + IBT
      IF(JK2.GT.42) JK2 = 42
      IF(HTOF(JK2).EQ.0.OR.ABS(TTOF(JK2)).EQ.0.) GO TO 33
      IF(TTOF(JK2).EQ.-20.) GO TO 33
      NRT = NRT + 1
      TOF2 = TOF2 + TTOF(JK2)
33    CONTINUE
      IF(NRT.EQ.0) GO TO 32
      TOF2 = TOF2/NRT
      NRS = NRS + 1
      TDIF = TDIF + TOF1 - TOF2
      TSUM = TSUM + TOF1 + TOF2
   32 CONTINUE
      IF(NRS.EQ.0) GO TO 5123
      TDIF = - TDIF / NRS
      TSUM = TSUM / NRS
5123  RETURN
      END
      BLOCK DATA
      COMMON /CREDTV/ CRVLIM,LMHITS,RPLIM,RPLIM1,RATLIM,COSCUT,ZVTXLM,
     $ ZVXLM1,ZVXLM2,ETOTLM,ETOTKP,ETCYKP,ETE1KP,ETE2KP,ZMLIM,FIDEL,
     $ XLM,YLM,ZLM,ERGL,ETAGLM,ETOTCT,TSUMC1,TSUMC2
      DATA ERGL /.200/
      DATA ETAGLM /100./
      DATA ETOTCT /100./
      DATA CRVLIM/.00135/, LMHITS/16/
      DATA RPLIM /30./
      DATA RPLIM1/10./
      DATA RATLIM /.20/
      DATA ZMLIM /350./
      DATA FIDEL /.200/
      DATA ZLM /500./
      DATA XLM /350./
      DATA YLM /350./
      DATA COSCUT /5.5/
      DATA ZVTXLM /350./
      DATA ZVXLM1 /500./
      DATA ZVXLM2 /200./
      DATA  ETOTLM /5000./
      DATA  ETOTKP /7000./
      DATA  ETCYKP /3500./
      DATA  ETE1KP /4000./
      DATA  ETE2KP /500./
      DATA  TSUMC1 /30./
      DATA  TSUMC2 /-20./
      END
