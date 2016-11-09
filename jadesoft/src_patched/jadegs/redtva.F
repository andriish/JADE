C   18/07/80 301061237  MEMBER NAME  REDTVA   (JADEGS)      FORTRAN
      SUBROUTINE REDTVA(LWRIT,LRJCT,IWRT,IFTG,IAC,IFLW,IPRN)
C---
C---     USER ROUTINE FOR CUTS AND INTERACTIVE DECISION MAKING.
C---     SPECIAL VERSION FOR SECOND DATA REDUCTION STEP
C---     *****  THIS IS THE GRAPHICS VERSION **********
C--  IPRN IS THE PRINT INDEX
C--   J.OLSSON    09.07.79        LAST CHANGE 07.08.81
      IMPLICIT INTEGER*2 (H)
C
      COMMON /CREDTV/ CRVLIM,LMHITS,RPLIM,RPLIM1,RATLIM,COSCUT,ZVTXLM,
     $ ZVXLM1,ZVXLM2,ETOTLM,ETOTKP,ETCYKP,ETE1KP,ETE2KP,ZMLIM,FIDEL,
     $ XLM,YLM,ZLM,ERGL,ETAGLM,ETOTCT,TSUMC1,TSUMC2
      COMMON/SMPTOF/MTOF,TTOF(42),NTOF,HTOF(42)
      COMMON /CHEADR/ HEAD(108)
      EQUIVALENCE (HRUN,HEAD(18)),(HEVENT,HEAD(19))
#include "cdata.for"
#include "cgeo1.for"
      COMMON /CJTRIG/ PI,TWOPI
      DIMENSION HELP(2),HELP2(2),THHELP(3)
      EQUIVALENCE (ICAMWD,HELP(1)),(HELP2(1),ICMWD2)
      DATA MSKLUM /Z102/,HRUNTP/-4/,EBMSTR/0./,MKFWMU /Z800/,MKFWM1/Z80/
      DATA MSKLM1 /Z100/,HRUNTP/-4/,EBMSTR/0./,MKFWMU /Z800/,MKFWM1/Z80/
      DATA  HELP/0,0/,HELP2/0,0/,NRUNOL/-1/,IENT /0/
C--------------------------------------------
C---
C  IHIST FOR HISTOGRAMS;   IPRN FOR DEBUG PRINT;
C  ICALIB FOR RECALIBRATION OF LEAD GLASS, ICALJC FOR INNER DETECTOR
C  IREPAT FOR PATTERN RECOGNITION, WITH FOLLOWING OPTIONS:
C  1: SLOW FOR ALL, 2 EXISTING ONE FOR SELECT, REDO SLOW
C  3: FAST FOR ALL, REDO SLOW FOR SELECTED EVENTS;  0 FOR NO PATREC
C                                    BEGIN OF EACH RUN
      IENT = IENT + 1
      IF(IENT.NE.1) GO TO 7575
      PI=ARCOS(-1.)
      TWOPI = 2.*PI
      IQHEAD = IBLN('HEAD')
7575  IPHEAD=IDATA(IQHEAD)
      CALL MVCL(HEAD(1),0,IDATA(IPHEAD-3),0,216)
      NRUN = HEAD(18)
      IF(NRUN.EQ.NRUNOL) GO TO 1021
      NRUNOL = NRUN
      IF(IPRN.GT.0) WRITE(6,102) HRUN,HEVENT,HEAD(20)
102   FORMAT(' ****** RUN ',I5,' EVENT ',I8,' REC.TYPE ',I8)
      EBM = EBEAM(HRUN)
      IF(EBM.GT.2.*EBMSTR) GO TO 1612
      IF(EBM.LT..5*EBMSTR) GO TO 1612
      GO TO 1021
1612  EBMSTR = EBM
C DECIDE BEAM ENERGY AND LIMITS
      IF(EBM.GT.7000.) GO TO 1029
C ENTER HERE FOR    NEW LIMITS   FOR 12 GEV DATA
      WRITE(6,1671) HRUN,HEVENT,EBM
1671  FORMAT(' LOW ENERGY DATA, RUN EVENT ',2I8,' EBEAM ',E12.4)
      ETOTKP = 2500.
      ETCYKP = 1250.
      ETE1KP = 1500.
      ETE2KP = 500.
      RPLIM = 40.
      RPLIM1 = 15.
      GO TO 1024
C ENTER HERE FOR    NEW LIMITS   FOR HIGH ENERGY DATA
1029  WRITE(6,1672) HRUN,HEVENT,EBM
1672  FORMAT(' HIGH ENERGY DATA ,RUN EVENT ',2I8,' EBEAM ',E12.4)
      ETOTKP = 7000.
      ETCYKP = 3500.
      ETE1KP = 4000.
      ETE2KP = 500.
      RPLIM = 30.
      RPLIM1 = 10.
1024  CONTINUE
C1024  WRITE(6,1022)
C1022  FORMAT(' ZVTXLM  LMHITS  ETOTKP  ETOTCT  ETAGLM  COSCUT   ZMLIM
C     $RPLIM  RPLIM1  RATLIM  CRVLIM  ZVXLM1  ZVXLM2')
C     WRITE(6,1023) ZVTXLM,LMHITS,ETOTKP,ETOTCT,ETAGLM,COSCUT,ZMLIM,
C    $ RPLIM,RPLIM1,RATLIM,CRVLIM,ZVXLM1,ZVXLM2
C1023  FORMAT(' ',F6.1,I8,F8.1,F8.1,F8.1,F8.1,F8.1,F8.3,F8.3,F8.3,F8.5,
C    $ F8.1,F8.1)
C      WRITE(6,1027) ETCYKP,ETE1KP,ETE2KP,TSUMC1,TSUMC2
C1027  FORMAT(' ETCYKP  ETE1KP  ETE2KP  ',3F8.1,' TSUMC1-2 ',2F8.1)
C                                                         SELECT EVENT
1021  ZVTX = -600.
      IFLAG=-1
      LRJCT = 0
      LWRIT = 0
      CALL RDATA(IER,HEAD(18))
      IF(IER.NE.0) GO TO 1752
      IF(HEAD(20).EQ.1.OR.HEAD(20).EQ.13) GO TO 1751
      IF(HEAD(20).GE.64) GO TO 1752
      IF(HRUNTP.NE.HRUN) WRITE(6,1753) HRUN,HEAD(20)
1753  FORMAT(' * * * * RUN NR ',I8,'   HAS RECORD TYPE ',I8)
      HRUNTP = HRUN
      GO TO 1751
1752  LRJCT = 16
      IF(IPRN.GT.0) WRITE(6,2490) HRUN,HEVENT,HEAD(20)
2490  FORMAT(' REJECTED AS NOT BEING DATA, RUN&EVENT ',2I8,' TYPE ',I8)
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
C
C     RECALIBRATION HERE IF REQUESTED, PARAMETER ICALIB
C
C                                           LEAD GLASS CALIBRATION DONE
CALL TRIGGER CHECK, USED IN REDUC1
C     LBTRBT = 0
C     IPTRIG = IDATA(IBLN('TRIG'))
C     IF(IPTRIG.EQ.0) GO TO 3011
C     IF(IDATA(IPTRIG-2).NE.1) GO TO 3011
C     LBTRBT = HDATA(2*IPTRIG  + 10)
C     CALL TRGCHK(LBTRCK,LBTRBT,IPRN)
C     IF(IPRN.GT.0) WRITE(6,3012) LBTRCK,LBTRBT
C3012  FORMAT(' TRGCHK RETURN : LBTRCK AND LBTRBT ',I8,1X,Z4)
C      IF(LBTRCK.NE.0) GO TO 3011
C      LRJCT = 18
C      IF(IPRN.GT.0) WRITE(6,3211) HRUN,HEVENT
C3211  FORMAT(' REJECTED BY THE TRIGGER CHECK, RUN EVENT ',2I8)
C      GO TO 1
C--- TRIGGER CHECK NOT OK ------------------------>>>>  * REJECT *  <<<<
C                                              ************************
C                                             ***    LRJCT = 18      ***
C                                              ************************
3011  CALL ERGTOT(ECYL,ECAMI,ECAPL)
      ETOT = ECYL + ECAMI + ECAPL
      IF(ETOT.GT.ETOTCT) GO TO 305
C THIS CHECK IS TO SEPARATE EVENTS OUT FOR LUMI CHECK
      ITRG = IDATA(IBLN('TRIG'))
      IF(ITRG.GT.0) GO TO 327
      WRITE(6,326) HEAD(18),HEAD(19)
326   FORMAT(' RUN & EVENT ',2I8,'  NO TRIGGER BANK ')
      GO TO 305
327   IF(IDATA(ITRG-2).EQ.1) GO TO 328
      WRITE(6,325) HEAD(18),HEAD(19)
325   FORMAT(' RUN & EVENT ',2I8,'  FIRST TRIGGER BANK NOT NR 1 ')
      GO TO 305
328   HELP(2) = HDATA(2*ITRG + IDATA(ITRG)*2)
      IF(ICAMWD.NE.MSKLUM.AND.ICAMWD.NE.MSKLM1) GO TO 305
C--- ENERGY < 100 MEV   LUMI TRIGGER ------------->>>>  * REJECT *  <<<<
C                                              ************************
C                                             ***    LRJCT = 1       ***
C                                              ************************
      LRJCT = 1
      IF(IPRN.GT.0) WRITE(6,3501) HRUN,HEVENT
3501  FORMAT(' REJECTED AS LUMI TRIGGER   RUN EVENT ',2I8)
      GO TO 1
C  SET FLAG FOR ENERGY IN FORWARD TAGGING BLOCKS
C            IFTG < 11     NO ENERGY
C            IFTG = 11     ENERGY ABOVE LIMIT IN NEG. FW ARM
C            IFTG = 12     ENERGY ABOVE LIMIT IN POS. FW ARM
C            IFTG = 113,13 ENERGY ABOVE LIMIT IN BOTH FW ARMS(LUMI)
305   CALL TAGFLG(IFTG)
      IAC = 0
      IF(ETOT.GT.ETOTKP) IAC = 1
      IF(ECYL.GT.ETCYKP) IAC = 1
      IF(ECAMI.GT.ETE1KP.AND.ECYL.GT.ETE2KP) IAC = 1
      IF(ECAPL.GT.ETE1KP.AND.ECYL.GT.ETE2KP) IAC = 1
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
      WRITE(6,3002) HEAD(18),HEAD(19),IAD,ETEST,ETOT
3002  FORMAT(' RU&EV '2I8,' BLOCK',I5,' WITH ENERGY&ETOT ',2E12.4)
      IAC = -1
      GO TO 301
303   CONTINUE
301   CONTINUE
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
      WRITE(6,3058) HEAD(18),HEAD(19),IDHITS,ETOT
3058  FORMAT(' ** OVERFLOW RUN EVENT ',2I8,' I.D. HITS & ETOT ',I6,
     $ E12.4)
C                                 ---------------->>>>  * REJECT *  <<<<
C                                              ************************
C                                             ***    LRJCT = 15      ***
C                                              ************************
      LRJCT = 15
      GO TO 1
3057  IPALGL = IDATA(IBLN('ALGL'))
      IPALGN = IDATA(IBLN('ALGN'))
      IF(IPALGL.GT.0.AND.IPALGN.GT.0) GO TO 3085
      WRITE(6,3084) HEAD(18),HEAD(19)
3084  FORMAT(' RUN&EV,  ALGL AND ALGN DO NOT EXIST ',2I8)
      GO TO 3087
3085  LALGL = IDATA(IPALGL)
      LALGN = IDATA(IPALGN)
      IF(LALGL.GT.10*LALGN) IFLW = 0
      IF(LALGL.GT.10*LALGN.AND.IPRN.GT.0) WRITE(6,4430) HRUN,HEVENT
4430  FORMAT(' RUN EVENT RESET FOR OVERFLOW IN PBG ',2I8)
C  SET FLAG FOR LOW OR BAD ENERGY, OVERFLOW, TAGGING
3087  IWRT = 1
      IF(IAC.LE.0.AND.IFLW.EQ.0.AND.(IFTG.LT.11.OR.ETOT.LT.ETAGLM))
     $ IWRT = 0
C                                                     ZVERTEX CALCULATED
      IPZV = IDATA(IBLN('ZVTX'))
      IF(IPZV.LE.0) GO TO 401
      IFLAG  = IDATA(IPZV+6)
      IF(IFLAG.LT.0) GOTO 401
      ZVTX   = ADATA(IPZV+1)
C                                 ---------------->>>>  * REJECT *  <<<<
C                                              ************************
C                                             ***    LRJCT = 2       ***
C                                              ************************
      IF(ABS(ZVTX).GT.ZVTXLM.AND.IWRT.NE.1.AND.IFLAG.GT.1) LRJCT = 2
      IF(IPRN.GT.0.AND.LRJCT.EQ.2) WRITE(6,1484) HRUN,HEVENT
1484  FORMAT(' REJECTED FOR BAD ZVERTEX, NO WRITE FLAG  ',2I8)
      IF(LRJCT.EQ.2) GO TO 1
      IF(ABS(ZVTX).GT.ZVXLM1.AND.IFLAG.EQ.3.AND.ECAMI.LT..05*ETOT.AND.
     $ ECAPL.LT..05*ETOT) IAC = 0
      IWRT = 1
      IF(IAC.LE.0.AND.IFLW.EQ.0.AND.(IFTG.LT.11.OR.ETOT.LT.ETAGLM))
     $ IWRT = 0
C                                                       PATREC PERFORMED
C401   CALL REDONE(INDRJ1,KBWRT1,KWRT)
C                                 ---------------->>>>  * REJECT *  <<<<
C                                                    ******************
C                                                    *   LRJCT = 17   *
C                                                    ******************
C      IF(IPRN.GT.0) WRITE(6,1881) INDRJ1,KBWRT1,KWRT
C1881  FORMAT(' REDUC1   INDREJ,KBWRT,KWRT ',3I4)
C      IF(KBWRT1.EQ.0) LRJCT = 17
C      IF(LRJCT.EQ.17.AND.IPRN.GT.0) WRITE(6,5090) HRUN,HEVENT
C5090  FORMAT(' REJECTED BY REDUC1  RUN EVENT ',2I8)
C      IF(LRJCT.EQ.17) GO TO 1
C
401   IPPATR = IDATA(IBLN('PATR'))
C IF NO PATR BANK, PROCEED TO CLUSTER CHECK   (NTR = 0 AT READ EVENT)
      IF(IPPATR.EQ.0) GO TO 503
      LO = IDATA(IPPATR+1)
      NTR = IDATA(IPPATR+2)
      LTR = IDATA(IPPATR+3)
C                                        ************* WRITTEN *********
      IF(NTR.EQ.0.AND.IFTG.GT.10.AND.ETOT.GT.50.) LWRIT = 7
      IF(IPRN.GT.0.AND.LWRIT.EQ.7) WRITE(6,5230) HRUN,HEVENT
5230  FORMAT('  NEUTRAL TAGGED EVENT, ENERGY > 50 MEV ',2I8)
C                                         --------------------------
C                                       --        LWRIT = 7         --
C                                         --------------------------
      IF(LWRIT.EQ.7) GO TO 11
C
      IF(NTR.EQ.0) GO TO 503
      IF(IFTG.GT.10) GO TO 562
      IF(IFLAG.NE.3) GO TO 562
C HERE CHECK EVENTS WITH HIGH ENERGY IN CYLINDER AND ZVTX GT 200.
C  THEY ARE PASSED ON TO NORMAL TRACK CHECK
      IF(ABS(ZVTX).GT.ZVXLM2.AND.ECAMI.LT..05*ETOT.AND.ECAPL.LT..05*ETOT
     $ ) GO TO 578
C  WRITE  1-PRONG  TAGGED EVENTS
C                                        ************* WRITTEN *********
C                                              ------------------------
C                                             ---    LWRIT = 9       ---
C                                              ------------------------
562   IF(NTR.EQ.1.AND.IFTG.GT.10) LWRIT = 9
      IF(LWRIT.EQ.9.AND.IPRN.GT.0) WRITE(6,5460) HRUN,HEVENT
5460  FORMAT(' ONE PRONG TAGGED EVENT   RUN EVENT ',2I8)
      IF(LWRIT.EQ.9) GO TO 11
C NOW WRITE ALL EVENTS WITH THE WRITE FLAG IWRT SET
C                                        ************* WRITTEN *********
      IF(IWRT.EQ.1) LWRIT = 1
      IF(LWRIT.EQ.1.AND.IPRN.GT.0) WRITE(6,5461) HRUN,HEVENT
5461  FORMAT(' EVENT WITH WRITE FLAG    RUN EVENT ',2I8)
C                                         --------------------------
C                                       --        LWRIT = 1         --
C                                         --------------------------
      IF(IWRT.EQ.1) GO TO 11
C-
C-
578   IO = IPPATR
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
502   IF(IPRN.GT.0) WRITE(6,5990) ISTAR
5990  FORMAT(' ISTAR = ',I7)
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
C
C  SEPARATE EVENTS WITH RATIO > RATLIM AND   EVENTS WITH RATIO < RATLIM
C
      IF(RATIO.GT.RATLIM) GO TO 517
      IF(RATIO.GT..5*RATLIM.AND.IGDNTR.GT.6) GO TO 517
C
C HERE FOR EVENTS WHICH DO NOT PASS RATIO CHECK - - - - - - - - - - -
C   NOW REJECT EVENTS IF
C                     OR GOOD ZVERTEX OUTSIDE LIMITS AND >3 GOOD TRACKS
C                     OR RATIO = 0.   AND >10 GOOD TRACKS
      IF(IFLAG.GT.0.AND.ABS(ZVTX).GT.ZVTXLM.AND.IGDNTR.GT.3) LRJCT = 3
      IF(LRJCT.EQ.3.AND.IPRN.GT.0) WRITE(6,6330) HRUN,HEVENT
6330  FORMAT(' REJECTED: BAD RATIO,ZV>LIM, IGDNTR>3  RUN EV. ',2I8)
      IF(LRJCT.EQ.3) GO TO 5038
      IF(RATIO.EQ.0..AND.IGDNTR.GT.10) LRJCT = 4
      IF(LRJCT.EQ.4.AND.IPRN.GT.0) WRITE(6,6331) HRUN,HEVENT
6331  FORMAT(' REJECTED: RATIO=0.,IGDNTR>10,  RUN EV. ',2I8)
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
5038  GO TO 1
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
      IF(IPRN.GE.2) WRITE(6,7574) ABS12,ABS13,ABS23,ABSM12,ABSM13,ABSM2300040500
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
      IF(LWRIT.EQ.2.AND.IPRN.GT.0) WRITE(6,6710) HRUN,HEVENT
6710  FORMAT(' WRITTEN AS GOOD RATIO NOT FEWPRONG  RUN EVENT ',2I8)
      IF(LWRIT.EQ.2) GO TO 11
C
4923  ICOSM = 0
C TEST ON COLLINEARITY IN THETA
      ABSTHE = ABS(THESUM-PI)
C---------------------------------------------******* WRITTEN **********
      IF(ABSTHE.GT..25.AND.RATIO.GT.RATLIM) LWRIT = 3
C                                         --------------------------
C                                       --        LWRIT = 3         --
C                                         --------------------------
      IF(LWRIT.EQ.3.AND.IPRN.GT.0) WRITE(6,6711) HRUN,HEVENT
6711  FORMAT(' WRITTEN AS GOOD RATIO  NONCOLL.   RUN EVENT ',2I8)
      IF(LWRIT.EQ.3) GO TO 11
      IF(ABSTHE.GT..25) GO TO 503
C
C PERFORM TOF TEST ONLY IF ETOT < 800 MEV, TO AVOID BREMS STRAHLUNG
4924  IF(ETOT.GT.800.) GO TO 5123
      ICOSM = 1
C
C SPECIALLY HARD CHECK FOR COSMIC CANDIDATES ( TWOPRONG )
CHECK TOF FOR COSMIC.  CODE TAKEN FROM NORD 50; P.DITTMANNS PROGRAM
4925  CALL TOFCHK(TDIF,TSUM,NRUN)
      NCOSM = 0
C TIME IN NANOSECONDS
C-----------------------------------------------------------------------
C                                        >>>>>>>>>>>> REJECT <<<<<<<<<<<
      IF(TDIF.GT.COSCUT.AND.TSUM.GT.TSUMC1) NCOSM = 1
      IF(TDIF.GT.COSCUT.AND.TSUM.LT.TSUMC2) NCOSM = 1
      IF(NCOSM.EQ.1) LRJCT = 5
C                                              ************************
C                                             ***    LRJCT = 5       ***
C                                              ************************
      IF(NCOSM.EQ.1.AND.IPRN.GT.0) WRITE(6,7131) HRUN,HEVENT
7131  FORMAT(' REJECTED AS COSMIC COLLINEAR ',2I8)
      IF(NCOSM.EQ.1) GO TO 1
C-----------------------------------------------------------------------
C    NEW RATIO FOR COSMIC CANDIDATES      EXTRA STRONG DEMAND ON RPLIM
5123  IF(IFLAG.LT.0) GO TO 5031
C-----------------------------------------------------------------------
C---------------------------------------------******* WRITTEN **********
5031  IF(ICNTS.GT.0) LWRIT = 4
C                                         --------------------------
C                                       --        LWRIT = 4         --
C                                         --------------------------
      IF(ICNTS.GT.0.AND.IPRN.GT.0) WRITE(6,7132) HRUN,HEVENT
7132  FORMAT(' WRITTEN AS NO COSMIC, GOOD RATIO ',2I8)
      IF(ICNTS.GT.0) GO TO 11
C-----------------------------------------------------------------------
C    NOW REJECT FAILING RATIO EVENTS
C--------------------------------------------->>>>>>>> REJECT <<<<<<<<<<
      LRJCT = 6
      IF(IPRN.GT.0) WRITE(6,7133) HRUN,HEVENT
7133  FORMAT(' REJECTED AS NO COSMIC BAD RATIO ',2I8)
C                                              ************************
C                                             ***    LRJCT = 6       ***
C                                              ************************
      GO TO 1
C-----------------------------------------------------------------------
C                                    CLUSTER ANALYSIS PERFORMED
503   IPCL = IDATA(IBLN('LGCL'))
C-----------------------------------------------------------------------
C--------------------------------------------->>>>>>>> REJECT <<<<<<<<<<
      IF (IPCL.EQ.0) LRJCT = 7
C                                              ************************
C                                             ***    LRJCT = 7       ***
C                                              ************************
      IF(IPCL.EQ.0.AND.IPRN.GT.0) WRITE(6,7134) HRUN,HEVENT
7134  FORMAT(' REJECTED NO CLUSTER BANK    ',2I8)
      IF (IPCL.EQ.0) GO TO 1
      IER = IDATA(IPCL+20)
C--------------------------------------------->>>>>>>> REJECT <<<<<<<<<<
      IF(IER.NE.0) LRJCT = 8
C                                              ************************
C                                             ***    LRJCT = 8       ***
C                                              ************************
      IF(IER.NE.0.AND.IPRN.GT.0) WRITE(6,7135) HRUN,HEVENT
7135  FORMAT(' REJECTED ERROR FLAG IN CLUSTER BANK  ',2I8)
      IF(IER.NE.0) GO TO 1
      NCLST = IDATA(IPCL+7)
C--------------------------------------------->>>>>>>> REJECT <<<<<<<<<<
      IF(NCLST.LE.1) LRJCT = 9
      IF(LRJCT.EQ.9.AND.IPRN.GT.0) WRITE(6,7136) HRUN,HEVENT
7136  FORMAT(' REJECTED NCLST LE 1 ',2I8)
C                                              ************************
C                                             ***    LRJCT = 9       ***
C                                              ************************
      IF(LRJCT.EQ.9) GO TO 1
C-----------------------------------------------------------------------
C     IF(NCLST.LE.1) LWRIT = 8
C     IF(LWRIT.EQ.8.AND.IPRN.GT.0) WRITE(6,7137) HRUN,HEVENT
C7137  FORMAT(' WRITTEN  NCLST LE 1, ECAPS>50        ',2I8)
C                                         --------------------------
C                                       --        LWRIT = 8         --
C                                         --------------------------
C     IF(LWRIT.EQ.8) GO TO 1
C-----------------------------------------------------------------------
      EBM = EBEAM(HRUN)
      IF(NTR.NE.0) GO TO 519
C                ENTER HERE FOR EVENTS WITH NO TRACKS
      IALGN = IDATA(IBLN('ALGN'))
C--------------------------------------------->>>>>>>> REJECT <<<<<<<<<<
      IF(IALGN.EQ.0) LRJCT = 10
      IF(IALGN.EQ.0.AND.IPRN.GT.0) WRITE(6,7138) HRUN,HEVENT
7138  FORMAT(' REJECTED NO ALGN BANK     ',2I8)
C                                              ************************
C                                             ***    LRJCT = 10      ***
C                                              ************************
      IF(IALGN.EQ.0) GO TO 1
C- NEUTRAL EVENTS ARE NOW CHECKED FOR MOMENTUM BALANCE -----------------
      CALL HWORLD(IALGN,JEMPTY,UNBAL)
C--------------------------------------------->>>>>>>> REJECT <<<<<<<<<<
      IF(JEMPTY.GT.1.OR.UNBAL.LT..05) LRJCT = 11
      IF(LRJCT.EQ.11.AND.IPRN.GT.0) WRITE(6,7139) HRUN,HEVENT
7139  FORMAT(' REJECTED HWORLD IMBALANCE ',2I8)
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
C-----------------------------------------------------------------------
C--------------------------------------------->>>>>>>> REJECT <<<<<<<<<<
C  NO TRACKS BUT MORE THAN 1000 HITS IN I.D.
5191  IF(IDHITS.GT.1000) LRJCT = 12
      IF(LRJCT.EQ.12.AND.IPRN.GT.0) WRITE(6,7140) HRUN,HEVENT
7140  FORMAT(' REJECTED NEUTRAL WITH IDHITS>1000 ',2I8)
C                                              ************************
C                                             ***    LRJCT = 12      ***
C                                              ************************
      IF(IDHITS.GT.1000) GO TO 1
C-----------------------------------------------------------------------
C  WRITE HERE NEUTRAL EVENTS WITH ETOT < 3.*EBM AND < 10 CLUSTERS
C---------------------------------------------******** WRITTEN *********
      IF(ETOT.LT.3.*EBM.AND.NCLST.LT.10) LWRIT = 6
      IF(LWRIT.EQ.6.AND.(ICAMWD.EQ.MKFWMU.OR.ICAMWD.EQ.MKFWM1))
     * LWRIT = 0
      IF(LWRIT.EQ.6.AND.IPRN.GT.0) WRITE(6,7141) HRUN,HEVENT
7141  FORMAT(' WRITTEN NTR=0, ETOT<3*EBM NCLST<10 ',2I8)
C                                         --------------------------
C                                       --        LWRIT = 6         --
C                                         --------------------------
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
      IF(NTR.EQ.0.AND.ADATA(IB1+2)+ADATA(IB2+2).LT..00007*ETOT)GO TO 611
CAV      00059400
      MARK = 1
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
      IF(NTR.EQ.0.AND.ADATA(IB1+2)+ADATA(IB2+2).LT..00007*ETOT)GO TO 611
CAV      00061300
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
      GO TO 619
C
611   CONTINUE
C
610   CONTINUE
C--------------------------------------------->>>>>>>> REJECT <<<<<<<<<<
      LRJCT = 13
      IF(LRJCT.EQ.13.AND.IPRN.GT.0) WRITE(6,7142) HRUN,HEVENT
7142  FORMAT(' REJECTED, NO COLLINEARS FOUND      ',2I8)
C                                              ************************
C                                             ***    LRJCT = 13      ***
C                                              ************************
      GO TO 1
C
C
619   ICOSM = 0
C IF COLLINEARS FOUND IN FEWPRONG EVENT AND LOW ENERGY, CHECK TOF.
      IF(NTR.GT.2.AND.NTR.LT.8.AND.ETOT.LT.700..AND.ISTAR.EQ.1)
     $ GO TO 4925
      COSS = SIN(THE1)*SIN(THE2)*(COS(FI1)*COS(FI2)+SIN(FI1)*SIN(FI2))
     $  + COS(THE1)*COS(THE2)
C---------------------------------------------******** WRITTEN *********
      LWRIT = 5
      IF(LWRIT.EQ.5.AND.IPRN.GT.0) WRITE(6,7143) HRUN,HEVENT
7143  FORMAT(' WRITTEN, COLLINEARS FOUND      ',2I8)
C                                         --------------------------
C                                       --        LWRIT = 5         --
C                                         --------------------------
      GO TO 11
C---
C---     RETURNS FOR STEERING ANALYSIS TO DESIRED NEXT STEP.
C---     'GO TO 1' MEANS REJECT EVENT AND GO TO NEXT EVENT.
C---     'GO TO 11' MEANS ACCEPT EVENT, WRITE IT AND GO TO NEXT EVENT
C---
1     IF(IPRN.GT.0)
     $ WRITE(6,7739) HEAD(18),HEAD(19),LRJCT
7739  FORMAT('  RUN&EV ',2I8,'         CODE ',I5,' >>> RJCT <<< ')
      RETURN
11    IF(IPRN.GT.0) WRITE(6,7738) HEAD(18),HEAD(19),LWRIT
7738  FORMAT('  RUN&EV  ',2I8,' CODE ',I5,'*** WRIT *** ')
      RETURN
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
