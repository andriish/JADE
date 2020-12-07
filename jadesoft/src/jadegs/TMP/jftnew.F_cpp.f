      SUBROUTINE JFTNEW(IPTR,IPJHTL,WRK,LHIT,IPRES,INDEX,XO,YO,
     +CALCST,DSTORW)
      IMPLICIT INTEGER*2 (H)
      DIMENSION WRK(200)
      EQUIVALENCE (ZWZ,IZW)
C----------------------------------------------------------------------
C             MACRO CDATA .... BOS COMMON.
C
C             THIS MACRO ONLY DEFINES THE IDATA/HDATA/ADATA NAMES.
C             THE ACTUAL SIZE OF /BCS/ IS FIXED ON MACRO CBCSMX
C             OR BY OTHER MEANS. A DEFAULT SIZE OF 40000 IS GIVEN HERE.
C
C----------------------------------------------------------------------
C
      COMMON /BCS/ IDATA(40000)
      DIMENSION HDATA(80000),ADATA(40000),IPNT(50)
      EQUIVALENCE (HDATA(1),IDATA(1),ADATA(1)),(IPNT(1),IDATA(55))
      EQUIVALENCE (NWORD,IPNT(50))
C
C------------------------ END OF MACRO CDATA --------------------------
C-----------------------------------------------------------------------
C                            MACRO CJDRCH .... JET CHAMBER CONSTANTS.
C-----------------------------------------------------------------------
C
      COMMON / CJDRCH / RDEC(4),PSIIN(3),RINCR(3),FIRSTW(3),FSENSW(3),
     +                  RDEPTH,SWDEPL,YSUSPN,TIMDEL(2,3),ZMAX,ZOFFS,
     +                  ZRESOL,ZNORM,ZAL,ZSCAL,DRIDEV,DRICOS,DRISIN,
     +                  PEDES,TZERO(3),DRIROT(96,2),SINDRI(96,2),
     +                  COSDRI(96,2),DRIVEL(96,2),T0FIX(3),
     +                  ABERR(8), DUMJDC(20)
C
C      BLOCK DATA SET TO MC VALUES, KALIBR WILL SET REAL DATA VALUES
C--->  A CHANGE OF THIS COMMON MUST BE DONE SIMULTANEOUSLY WITH  <----
C--->  A CHANGE OF THE BLOCK DATA                                <----
C
C--------------------------- END OF MACRO CJDRCH -----------------------
C
      COMMON/CCMZCT/ DIMPCT, ZCUTV, ZCUTVV, IZVCST(5), ZCHWW
      DIMENSION IZCHMB(3,2),AZCHMB(3,2)
      COMMON/JSCALD/ JESCAL,JESKEY,JESDRW
      COMMON/XYFVT1/MODXYV,NOVL
      COMMON /CFLMAS/ AFLMAS
      DIMENSION CALCST(96,27),DSTORW(5,64,24)
      REAL GG(4,8) / 1.16872E-1,-2.58070E-1, 1.32006E-1, 286.,
     +              -1.04290E-1, 8.84550E-2,-1.96380E-2, 286.,
     +               3.24418E-2,-7.46600E-2, 2.36765E-2, 496.,
     +              -5.75000E-2, 4.36000E-2,-1.10000E-2, 496.,
     +               3.12761E-2,-1.12856E-1, 6.26170E-2, 707.,
     +               5.44166E-1,-8.74361E-1, 3.05246E-1, 707.,
     +               3.54954E-1,-6.67922E-1, 2.74681E-1, 707.,
     +              -2.26062E-1, 3.02971E-1,-1.15400E-1, 707./
      REAL GGF(4,8)/         0.,         0.,         0., 286.,
     +                       0.,         0.,         0., 286.,
     +                       0.,         0.,         0., 496.,
     +                       0.,         0.,         0., 496.,
     +                       0.,         0.,         0., 707.,
     +               7.52764E-1,-1.10923E00, 3.47826E-1, 707.,
     +               2.52785E-1,-4.63690E-1, 1.75837E-1, 707.,
     +                       0.,         0.,         0., 707./
      REAL THL(4)/-52.,-52.,-4.,960./,
     +     THU(4)/-4.,-4.,960.,1570./,
     +     A2(4)/-52.,-1.97924E-5,-20.5739,0./,
     +     A3(4)/-9.224,700.,6.E-5,2.E-10/,
     +     A4(4)/1.497,1320.,-8.31E-6,-2.73E-10/
      REAL B1(4,4)/
     + 0.62000E+03,   0.87300E+03,  -0.41000E+03,   0.15800E+04,
     + 0.62000E+03,   0.87300E+03,  -0.41000E+03,   0.15800E+04,
     + 0.57030E+03,   0.89000E+03,  -0.38000E+03,   0.15800E+04,
     + 0.64300E+03,   0.90200E+03,  -0.41200E+03,   0.15800E+04/
      DIMENSION Q(5,4)
      DATA Q/14.5142,3.2743E-2,-6.E-6,0.,0.,
     +       4.46445E1,-8.87962E-2,1.29605E-4,9.02461E-9,-5.85976E-11,
     +       4.52471E1,-8.94577E-2,1.39668E-4,1.05065E-8,-7.46739E-11,
     +       18.256,3.46596E-2,-1.26438E-5,0.,0./
      DIMENSION P(5,4)
      DATA P/-.955408,1.62185E-3,-8.22074E-7,0.,0.,
     +       -.1736,1.41338E-3,-1.14314E-5,1.96957E-8,-7.93752E-12,
     +       -.173,2.2942E-4,-2.4298E-6,0.,0.,
     +       -1.0475,1.92375E-3,-1.2E-6,0.,0./
      REAL OMERIN(3)/2*.130900,.0654498/,ALORIN(3)/3*.34/,
     +     RR1(3)/211.,421.,632./,WIRDIS/10./,SMAXW(2,64),SM01(2,64),
     +     ANG375/.0654498/,
     +     FLTIM1/.028/,FLTIM2/.0363/,FLTIM3/1222.9/,ELFRCZ/.231/,
     +     AVFRMX/.78742/,EPS/1.E-4/,TANLOR/.365028/,SINLOR/.342898/,
     +     COSLOR/.939373/,PIVALU/3.141593/,
     +     PARVD(3)/.59562E-2,.59482E-2,.59421E-2/
      DATA JESOLD/-1/,LIMPRT/0/,KIMPRT/0/,LIMPR1/3/,KIMPR1/0/
***PMF      DIMENSION IRESAR(13),RESAR(13),HRESAR(13)
      DIMENSION IRESAR(14),RESAR(14),HRESAR(14)
      EQUIVALENCE (IRESAR(1),RESAR(1),HRESAR(1))
      INTEGER MKLRT1 /Z1000000/, MKLRT2 /Z100/
      INDX=INDEX
      IF(INDX.EQ.4) INDX=1
      IF(
     - JESCAL.NE.JESOLD
     -)THEN
         JESOLD=JESCAL
      IF(
     - KIMPR1.LT.LIMPR1
     -)THEN
            KIMPR1=KIMPR1+1
            PRINT 720, JESCAL
720      FORMAT(' **** NEW ID CALIBRATION IN EFFECT IN JFETCH AFTER ',
     +   'RUN', I7,/,6X,'BIT 256 IS SET IN THE PROGRAM IDENTIFIER ',
     +   'WORD OF THE PATR BANK',////)
      ENDIF
      ENDIF
      DATA LBINIT /0/,IQJETC/0/,IQHEAD/0/
      IF(
     - LBINIT .EQ. 0
     -)THEN
         LBINIT = 1
         IQJETC = IBLN('JETC')
         IQHEAD = IBLN('HEAD')
         A5=1./.6726
      DO 13000 J=1,8
            GG(1,J)=GG(1,J)*A5
            GG(2,J)=GG(2,J)*A5*.5/GG(4,J)
            GG(3,J)=GG(3,J)*A5*.333333/GG(4,J)**2
      IF(
     - J.EQ.6.OR.J.EQ.7
     -)THEN
               GGF(1,J)=GGF(1,J)*A5
               GGF(2,J)=GGF(2,J)*A5*.5/GGF(4,J)
               GGF(3,J)=GGF(3,J)*A5*.333333/GGF(4,J)**2
      ENDIF
13000 CONTINUE
13001 CONTINUE
      DO 13002 I=1,64
      IF(
     - I.LE.16
     -)THEN
               IRIN=1
               IW=I
      ELSE
      IF(
     - I.LE.32
     -)THEN
                  IRIN=2
                  IW=I-16
      ELSE
                  IRIN=3
                  IW=I-32
                  IF(IW.GT.16) IW=IW-16
      ENDIF
      ENDIF
            R=RR1(IRIN)+(IW-1)*WIRDIS
            SMAXW(1,I)=1.05*R*SIN(OMERIN(IRIN))/COS(ALORIN(IRIN)
     +      -OMERIN(IRIN))
            SMAXW(2,I)=1.05*R*SIN(OMERIN(IRIN))/COS(ALORIN(IRIN)
     +      +OMERIN(IRIN))
            SM01(1,I)=.7*SMAXW(1,I)
            SM01(2,I)=.7*SMAXW(2,I)
      IF(
     - IW.LT.3.OR.IW.GT.14
     -)THEN
               SM01(1,I)=.45*SMAXW(1,I)
               SM01(2,I)=.45*SMAXW(2,I)
      ENDIF
13002 CONTINUE
13003 CONTINUE
        PRINT 7777
 7777   FORMAT(' JFTNEW : VERSION FROM 13/01/88 CALLED!',/,
     *         ' +++++++++++++++++++++++++++++++++++++++++++++++++++++')
      ENDIF
      LHBIT  = LHIT*4
      IPCO = 1
      IPHEAD = IDATA(IQHEAD)*2
      NRUN = HDATA(IPHEAD+10)
      IF(
     - NRUN.LT.24200
     -)THEN
         FREQR=1.0127
         FLTIM2=.0363
      ELSE
         FREQR=1.
         FLTIM2=0.
      ENDIF
      NEVT = HDATA(IPHEAD+11)
      ITRK = IDATA(IPTR+1)
      IDATA(IPTR+2) = LOR(IDATA(IPTR+2),256)
      IPJETC = IDATA(IQJETC)
      IP0    = 2*IPJETC + 100
         IPRAW=IPJETC
15000 CONTINUE
      IF(
     - IDATA(IPRAW -1).GT.0
     -)THEN
            IPRAW =IDATA(IPRAW -1)
      GOTO 15000
      ENDIF
15001 CONTINUE
         IPRAW2=2*IPRAW -2*IPJETC
      ZVERT = ADATA(IPTR+31)
      TGTH = ADATA(IPTR+30)
      CSTH = 1./SQRT(TGTH**2 + 1.)
      SNTH  = CSTH * TGTH
         XHCS = (ADATA(IPTR+12) + ADATA(IPTR+5)) * .5
         YHCS = (ADATA(IPTR+13) + ADATA(IPTR+6)) * .5
         XX    =  ADATA(IPTR+12) - ADATA(IPTR+5)
         YY    =  ADATA(IPTR+13) - ADATA(IPTR+6)
         RR    = SQRT(XX**2+YY**2)
      IF(
     - RR.LT.10.
     -)THEN
            IPRES=IPCO
            RETURN
      ENDIF
         CSROT = XX / RR
         SNROT = YY / RR
         ALCS=.5*RR
         SINFIC=SNROT
         COSFIC=CSROT
         IF(COSFIC.GT.1.) COSFIC=1.
         IF(COSFIC.LT.-1.) COSFIC=-1.
         FIC=ACOS(COSFIC)
         IF(SINFIC.LT.0.) FIC=2.*PIVALU-FIC
         BCS1X=-XHCS*COSFIC-YHCS*SINFIC
         BCS1Y= XHCS*SINFIC-YHCS*COSFIC
         CURVXY=ADATA(IPTR+25)
         IF(ABS(CURVXY).LT.1.E-8) CURVXY = SIGN(1.E-8,CURVXY)
         IF(ABS(CURVXY*ALCS).GT..966) CURVXY=SIGN(.966/ALCS,CURVXY)
         CTGTH=TGTH
         IF(ABS(CTGTH).GT.2.) CTGTH=SIGN(2.,CTGTH)
         CPR0=CURVXY/SQRT(1.-(CURVXY*ALCS)**2)
         VCRS=1./CPR0
         CURN1=CURVXY*PARVD(2)
      IZZZSE=INDX
      GOTO 14002
14003 CONTINUE
        XT = 0.
        YT = 0.
        CSROT = 1.
        SNROT = 0.
        XOT   = 0.
      GOTO 14001
14004 CONTINUE
        XT    = XHCS
        YT    = YHCS
        XOT   = 0.
      GOTO 14001
14005 CONTINUE
        XT    = (ADATA(IPTR+12) + XO) * .5
        YT    = (ADATA(IPTR+13) + YO) * .5
        XX    =  ADATA(IPTR+12) - XO
        YY    =  ADATA(IPTR+13) - YO
        RR    = SQRT(XX**2+YY**2)
        CSROT = XX / RR
        SNROT = YY / RR
        XOT   = -.5*RR
      GOTO 14001
14002 CONTINUE
      GOTO(
     -14003,14004,14005
     -), IZZZSE
14000 CONTINUE
        RETURN
14001 CONTINUE
      NOVL = 0
      IPC0 = IPTR + 34
      IPC9 = IPC0 +  5
      DO 13004 IPC = IPC0,IPC9
         JCELL = IDATA(IPC)
      IF(
     - JCELL.GT. 0 .AND. JCELL.LE.96
     -)THEN
            JRING = 1
            IF(JCELL.GT.24) JRING = 2
            IF(JCELL.GT.48) JRING = 3
      ASSIGN 17001 TO IZZZ01
      GOTO 17000
17001 CONTINUE
      ENDIF
13004 CONTINUE
13005 CONTINUE
      IF(
     - INDEX.EQ.4 .AND. ZCHWW.GT..1 .AND. ZCHWW.LT.2000.
     -)THEN
         CALL ZCFTNW(NRUN,NEVT,ITRK,TGTH,ZVERT,NZHIT,IZCHMB,AZCHMB)
      IF(
     - NZHIT.GT.0
     -)THEN
      DO 13006 J=1,NZHIT
               HRESAR( 1) = 100+IZCHMB(1,J)
               HRESAR( 2) = IZCHMB(2,J)
               HRESAR( 3) = 0
               HRESAR( 4) = 0
               HRESAR( 5) = 1
               HRESAR( 6) = 101
               XX=AZCHMB(1,J)
               YY=AZCHMB(2,J)
               ZZ=AZCHMB(3,J)
               RESAR ( 4) = XX
               RESAR ( 5) = YY
               RESAR ( 6) = ZZ
               UX=XX-ADATA(IPTR+5)
               UY=YY-ADATA(IPTR+6)
               UU=SQRT(UX**2+UY**2)
            IF(ABS(CURVXY*UU).GT.1.E-5) UU=2.*SIN(.5*CURVXY*UU)/CURVXY
               IF(UX*ADATA(IPTR+8)+UY*ADATA(IPTR+9).LT.0.) UU=-UU
               RESAR ( 7) = UU
               RESAR ( 8) = ZCHWW
               IF(NRUN.LT.24200) RESAR(8)=RESAR(8)*.6
               CALL MVC(WRK(IPCO),0,RESAR(1),0,LHBIT)
               IPCO = IPCO + LHIT
13006 CONTINUE
13007 CONTINUE
      ENDIF
      ENDIF
      IPRES = IPCO
      IF(
     - INDEX.LT.4
     -)THEN
         WRK (IPRES   ) = XT
         WRK (IPRES+ 1) = YT
         WRK (IPRES+ 2) = CSROT
         WRK (IPRES+ 3) = SNROT
         WRK (IPRES+ 9) = XOT
         WRK (IPRES+10) = 0.
         WRK (IPRES+11) = CSTH
         WRK (IPRES+12) = SNTH
      ENDIF
      RETURN
17000 CONTINUE
         KRING=JRING
         IF(JRING.EQ.3 .AND. JCELL-(JCELL/2)*2.NE.1) KRING=4
      IF(
     - JCELL.LE.24
     -)THEN
            ISEG=JCELL
      ELSE
      IF(
     - JCELL.LE.48
     -)THEN
               ISEG=JCELL-24
      ELSE
               ISEG=(JCELL-47)/2
      ENDIF
      ENDIF
         FISEGM=((ISEG-1)*4+2)*ANG375
         ACEL1=CALCST(JCELL,18)
         ZETCEL=2.*SIN(.5*ACEL1*CURVXY)/CURVXY*TGTH+ZVERT
         IF(ABS(ZETCEL).GT.1000.) ZETCEL=SIGN(1000.,ZETCEL)
         ACEL1=   ACEL1        +ZETCEL*CALCST(JCELL,19)
         BCEL1=CALCST(JCELL,20)+ZETCEL*CALCST(JCELL,21)
         OCEL1=CALCST(JCELL,22)+ZETCEL*CALCST(JCELL,23)
         FIIC=FISEGM+OCEL1-FIC
         ACEL2=CALCST(JCELL,19)*CTGTH*WIRDIS
         BCEL2=CALCST(JCELL,21)*CTGTH*WIRDIS
         OCEL2=CALCST(JCELL,23)*CTGTH*WIRDIS
         ROT1X=COS(FIIC)
         ROT1Y=SIN(FIIC)
         ROT2X=SIN(FIC-FISEGM)
         ROT2Y=COS(FIC-FISEGM)
         BCS1XC=BCS1X+ACEL1*ROT2Y+BCEL1*ROT2X
         BCS1YC=BCS1Y-ACEL1*ROT2X+BCEL1*ROT2Y
        JHIT = 0
        NHIT   = 0
        NHGOOD = 0
        ILAYL =-99
        IPCO = IPCO - LHIT
        IPCLL  = 2*IPJETC + 2 + JCELL
        IP     = HDATA(IPCLL  ) + IP0
        IP9    = HDATA(IPCLL+1) + IP0
        IPHL   = IPJHTL + 2 + HDATA(IPCLL)/4
15002 CONTINUE
      IF(
     - IP.LT.IP9
     -)THEN
          LB   = IDATA(IPHL)
          ITR1 = LAND(ISHFTR(LB,17),127)
          ITR2 = LAND(ISHFTR(LB, 1),127)
      IF(
     - ITR1.EQ.ITRK .OR. ITR2.EQ.ITRK
     -)THEN
            L0GOOD = 0
      IF(
     - ITR1.NE.0 .AND. ITR2.NE.0
     -)THEN
               NOVL   = NOVL + 1
               L0GOOD = 11
               ID1    = LAND(ISHFTR(LB,27), 31)
               ID2    = LAND(ISHFTR(LB,11), 31)
      IF(
     - ITR1 .EQ. ITRK
     -)THEN
                  IF( ID1 .LT. ID2 ) L0GOOD = 2
                  IF( ID1 .GT. ID2 ) L0GOOD = 12
      ELSE
                  IF( ID2 .LT. ID1 ) L0GOOD = 2
                  IF( ID2 .GT. ID1 ) L0GOOD = 12
      ENDIF
      ENDIF
            LBLR = 0
            IF(ITR1.EQ.ITRK) LBLR = LAND(LB,MKLRT1)
            IF(ITR2.EQ.ITRK) LBLR = LAND(LB,MKLRT2)
            LBSIDE =-1
            IF(LBLR.NE.0) LBSIDE = 1
            LBLR = LBSIDE
            IWIR = HDATA(IP)
            IWIR = ISHFTR(IWIR,3)
            ILAY = LAND(IWIR,15)
            IAMPL = HDATA(IP+1)
            IAMPR = HDATA(IP+2)
            CALL AMPS2Z( IP,IPJETC,ZZ,WW,LZGOOD)
            IW=ILAY+1
            IODD=1
            IF(IW-(IW/2)*2.EQ.0) IODD=-1
            RHIT=ACEL1+(IW-8.5)*WIRDIS
            FLPATH=2.*SIN(.5*RHIT*CURVXY)/CURVXY
            ZHIT=FLPATH*TGTH+ZVERT
            IF(ABS(ZHIT).GT.1200.) ZHIT=SIGN(1200.,ZHIT)
            FLPATH=SQRT(FLPATH**2+ZHIT**2)
            TDRIFT=HDATA(IP+3+IPRAW2)
      IF(
     - NRUN.LT.24200
     -)THEN
               TDRIFT=TDRIFT*64.+32.
               IF(NRUN.GE.19050.AND.NRUN.LE.20274) TDRIFT=TDRIFT+20.
               IF(NRUN.GE. 3300.AND.NRUN.LE. 3550) TDRIFT=TDRIFT-90.
      ELSE
      IF(
     - NRUN.LE.24698
     -)THEN
                  TDRIFT=TDRIFT-5.
                  IF(NRUN.LT.24405) TDRIFT=TDRIFT+153.
                  IF(NRUN.GE.24227.AND.NRUN.LE.24232) TDRIFT=TDRIFT+147.
                  IF(NRUN.GE.24233.AND.NRUN.LE.24245) TDRIFT=TDRIFT+297.
      ENDIF
      ENDIF
            AMRAWL=HDATA(IP+1+IPRAW2)*8.
            AMRAWR=HDATA(IP+2+IPRAW2)*8.
      ASSIGN 17003 TO IZZZ02
      GOTO 17002
17003 CONTINUE
            IF(JRING.EQ.3) TDRIFT=TDRIFT*FREQR
            BKGS=ABS(HDATA(IPHEAD+30)*.001)
            IF(BKGS.LT.3.) BKGS=4.8
            AMOMGV=.02998E-3*BKGS*SQRT(1.+CTGTH**2)/CURVXY
            CPERV=SQRT(1.+(AFLMAS/AMOMGV)**2)
            TDRIFT=TDRIFT-FLTIM1*FLPATH*CPERV-FLTIM2*(FLTIM3-ABS(ZHIT))
            TDRIFT=TDRIFT-CALCST(JCELL,IW)
            TSTG=CALCST(JCELL,17)*(1.-ELFRCZ*(ZHIT/1200.)**2)
     +      *AVFRMX*SINLOR*PARVD(JRING)/WIRDIS
            XK=IW-IODD*TSTG-8.5
            ROT3X=XK*WIRDIS*(ROT1X-ROT1Y*XK*OCEL2)
            ROT3Y=XK*WIRDIS*(ROT1Y+ROT1X*XK*OCEL2)
            XWPR=ROT3X+BCS1XC+XK*( ACEL2*ROT2Y+BCEL2*ROT2X)
            YWPR=ROT3Y+BCS1YC+XK*(-ACEL2*ROT2X+BCEL2*ROT2Y)
            NLRSOL = 1
            IF(TDRIFT.LT.300.) NLRSOL = 2
            ILRSOL = 0
16000 CONTINUE
            ILRSOL = ILRSOL + 1
            LBGOOD = L0GOOD
      IF(
     - NLRSOL.EQ.1 .AND. LBSIDE.LT.0  .OR.
     ?           NLRSOL.EQ.2 .AND. ILRSOL.EQ.1
     -)THEN
                LBSIDE =-1
      ASSIGN 17005 TO IZZZ03
      GOTO 17004
17005 CONTINUE
      ELSE
                LBSIDE = 1
      ASSIGN 17006 TO IZZZ03
      GOTO 17004
17006 CONTINUE
      ENDIF
              IF(LBSIDE.NE.LBLR) LBGOOD = LBGOOD + 1
      IF(
     - ILAY.NE.ILAYL .OR. LBGDL.LE.1.AND.LBGOOD.LE.2
     -)THEN
                LBREG = 1
                JHIT = JHIT + 1
                IPCO = IPCO + LHIT
      ELSE
                LBREG = 0
                ZWZ = WRK(IPCO+10)
                IF(LBGOOD.LT.IZW) LBREG = 1
      ENDIF
      IF(
     - LBREG.NE.0
     -)THEN
                NHIT   = NHIT   + 1
                IF(LBGOOD.LE.2) NHGOOD = NHGOOD + 1
      IF(
     - INDEX.NE.4
     -)THEN
                   IRESAR( 1) = ILAY
                   IRESAR( 2) = IP
                   IRESAR( 3) = LBSIDE
                   RESAR ( 4) = XX
                   RESAR ( 5) = YY
                   RESAR ( 6) = ZZ
                   RESAR ( 7) = XX - XOT
                   IF(INDX.EQ.1) RESAR ( 7) = SQRT(XX**2 + YY**2)
                   IRESAR( 8) = LZGOOD
                   RESAR ( 9) = DSC
                   IRESAR(10) = JCELL
                   IRESAR(11) = LBGOOD
                   RESAR (12) = TANBET
                   IRESAR(13) = JRING
                   RESAR (14) = 0.
      ELSE
                   HRESAR( 1) = JCELL
                   HRESAR( 2) = ILAY
                   HRESAR( 3) = LZGOOD
                   HRESAR( 4) = LBGOOD
                   HRESAR( 5) = 1
                   HRESAR( 6) = IP-2*IPJETC
                   RESAR ( 4) = XX
                   RESAR ( 5) = YY
                   RESAR ( 6) = ZZ
                   UX=XX-ADATA(IPTR+5)
                   UY=YY-ADATA(IPTR+6)
                   UU=SQRT(UX**2+UY**2)
            IF(ABS(CURVXY*UU).GT.1.E-5) UU=2.*SIN(.5*CURVXY*UU)/CURVXY
                   IF(UX*ADATA(IPTR+8)+UY*ADATA(IPTR+9).LT.0.) UU=-UU
                   RESAR ( 7) = UU
                   RESAR ( 8) = WW
      ENDIF
                CALL MVC(WRK(IPCO),0,RESAR(1),0,LHBIT)
                ILAYL = ILAY
                LBGDL = LBGOOD
      ENDIF
      IF(.NOT.(
     - ILRSOL.GE.NLRSOL
     -))GOTO 16000
16001 CONTINUE
      ENDIF
        IPHL = IPHL + 1
        IP   = IP   + 4
      GOTO 15002
      ENDIF
15003 CONTINUE
        IPCO = IPCO + LHIT
      GOTO IZZZ01
17004 CONTINUE
      IF(
     - LBSIDE.LT.0
     -)THEN
            AG2= CALCST(JCELL,26)
            VDP=-CALCST(JCELL,24)
      ELSE
            AG2= CALCST(JCELL,27)
            VDP= CALCST(JCELL,25)
      ENDIF
         AG2=AG2-FIIC-XK*OCEL2
         SINAG2=SIN(AG2)
         COSAG2=COS(AG2)
         F=XWPR*SINAG2+(YWPR+VCRS)*COSAG2
         G=(ALCS-XWPR)*(ALCS+XWPR)-YWPR*(YWPR+2.*VCRS)
      IF(
     - G.GT.-.98*F**2
     -)THEN
      ASSIGN 17008 TO IZZZ04
      GOTO 17007
17008 CONTINUE
            DISTWC=SQTVAL
            XPR=XWPR+DISTWC*SINAG2
            DYPDXP=1.-(CURVXY*XPR)**2
      IF(
     - DYPDXP.LT..02
     -)THEN
      IF(
     - KIMPRT.LT.LIMPRT
     -)THEN
                  KIMPRT=KIMPRT+1
                  PRINT 675,CURVXY,ALCS,XPR,NRUN,NEVT,ITRK
675      FORMAT(' *** ERROR IN JFTNEW *** CURVATURE, HALF TRACK LENGTH',
     +   ' X IN TR C.S.',/,8X,3E15.5,'   TRACK',I9,I6,I4)
                  PRINT 676, CPR0,VCRS,XHCS,YHCS,FIC,TGTH,
     +            XWPR,YWPR,AG2,F,G,DISTWC,DYPDXP,
     +            KRING,JCELL,ISEG,FISEGM,IW,IODD,ILRSOL,LBSIDE
676               FORMAT(/,' CPR0,VCRS,XHCS,YHCS,FIC,TGTH,',/,
     +            ' XWPR,YWPR,AG2,F,G,DISTWC,DYPDXP,'/,
     +            ' KRING,JCELL,ISEG,FISEGM,IW,IODD,ILRSOL,LBSIDE',/,
     +            1X,6E15.5,/,1X,7E15.5,/,1X,3I4,E15.5,4I6,////)
      ENDIF
               DYPDXP=.02
      ENDIF
            DYPDXP=-XPR*CURVXY/SQRT(DYPDXP)
            C=1.-DYPDXP*SINAG2/COSAG2
            IF(ABS(C).LT..001) C=SIGN(.001,C)
            TANBET=(DYPDXP+SINAG2/COSAG2)/C
            AMU=WIRDIS/PARVD(JRING)*COSLOR
            DELTA=AMU*(TANBET-TANLOR)
            IF(ABS(DELTA).GT.1800.) DELTA=SIGN(1800.,DELTA)
      ELSE
      IF(
     - KIMPRT.LT.LIMPRT
     -)THEN
               KIMPRT=KIMPRT+1
               PRINT 674,CURVXY,ALCS,NRUN,NEVT,ITRK
674      FORMAT(' *** ERROR IN JFTNEW *** CURVATURE, HALF TRACK LENGTH',
     +   /,8X,2E15.5,'   TRACK',I9,I6,I4)
               PRINT 677, CPR0,VCRS,XHCS,YHCS,FIC,TGTH,
     +         XWPR,YWPR,AG2,F,G,
     +         KRING,JCELL,ISEG,FISEGM,IW,IODD,ILRSOL,LBSIDE
677            FORMAT(/,' CPR0,VCRS,XHCS,YHCS,FIC,TGTH,',/,
     +         ' XWPR,YWPR,AG2,F,G,'/,
     +         ' KRING,JCELL,ISEG,FISEGM,IW,IODD,ILRSOL,LBSIDE',/,
     +         1X,6E15.5,/,1X,5E15.5,/,1X,3I4,E15.5,4I6,////)
      ENDIF
            TANBET=TANLOR
            DELTA=0.
      ENDIF
      ASSIGN 17010 TO IZZZ05
      GOTO 17009
17010 CONTINUE
      ASSIGN 17012 TO IZZZ06
      GOTO 17011
17012 CONTINUE
      ASSIGN 17014 TO IZZZ07
      GOTO 17013
17014 CONTINUE
         Y1=VDP*TCORR
      IF(
     - JESDRW.GT.0
     -)THEN
      ASSIGN 17016 TO IZZZ08
      GOTO 17015
17016 CONTINUE
      ENDIF
         DSC=ABS(Y1)
         XX=XWPR+Y1*SINAG2
         YY=YWPR+Y1*COSAG2
      IF(
     - INDX.NE.2
     -)THEN
            A= XX*COSFIC-YY*SINFIC+XHCS
            YY=XX*SINFIC+YY*COSFIC+YHCS
            XX=A
      IF(
     - INDX.EQ.3
     -)THEN
               A = (XX-XT)*CSROT+(YY-YT)*SNROT
               YY=-(XX-XT)*SNROT+(YY-YT)*CSROT
               XX=A
      ENDIF
      ENDIF
      GOTO IZZZ03
17002 CONTINUE
      A=AMRAWL
      IF(AMRAWR.GT.A) A=AMRAWR
      IF(A.LT.10.) A=10.
      IF(
     - NRUN.GE. 24200
     -)THEN
      IF(
     - A.GT.1800.
     -)THEN
            TSLEW=-1.449+1.19097E-3*(A-2000.)
      ELSE
            TSLEW=-2.100+2.11521E-3*(A-1600.)-8.50349E-12*(1600.-A)**4
      ENDIF
      ELSE
      IF(
     - A.GT.5000.
     -)THEN
            TSLEW=-50.+5.80000E-3*(A-5000.)
      ELSE
      IF(
     - A.LT.300.
     -)THEN
               TSLEW=-200.
      ELSE
         TSLEW=-4472.05*A**(-5.23557E-1-6.42692E-3*(ALOG(A)-7.77529)**2)
               IF(NRUN.GE.20275.AND.A.LT.1500.)
     +         TSLEW=TSLEW-(A-1500.)**2*2.26664E-5
      ENDIF
            IF(A.LT.650. .AND.(NRUN.GE.20275 .OR.
     +      NRUN.GE.13000 .AND. NRUN.LE. 14599) )
     +      TSLEW=TSLEW+116.6-1.79687E-1*A
            IF(A.LT.800. .AND. NRUN.GE.11473 .AND. NRUN.LE.12554)
     +      TSLEW=TSLEW+139.4-1.74800E-1*A
      ENDIF
      ENDIF
      TDRIFT=TDRIFT+TSLEW
      GOTO IZZZ02
17009 CONTINUE
      ACTG=ABS(CTGTH)
      IF(
     - NRUN.GE. 24200
     -)THEN
         ZTSLW=0.
      IF(
     - KRING.EQ.4
     -)THEN
      IF(
     - LBSIDE.LT.0
     -)THEN
               ZTSLW=-19.43-14.5942*ACTG+19.8951*ACTG**2
      IF(
     - ACTG.LT..42
     -)THEN
                  ZTSLW=ZTSLW+5.1921+3.216*ACTG-82.49*ACTG**2
      ELSE
                  ZTSLW=ZTSLW-24.66+48.9578*ACTG-22.7265*ACTG**2
      ENDIF
      ELSE
               ZTSLW=5.918-5.45559*ACTG-2.12*ACTG**2
      ENDIF
      ENDIF
      IF(
     - KRING.EQ.3
     -)THEN
      IF(
     - LBSIDE.LT.0
     -)THEN
               ZTSLW=-.937-8.66313*ACTG+9.8988*ACTG**2
      ELSE
               ZTSLW= 2.46- 3.8375*ACTG-14.5671*ACTG**2
      ENDIF
      ENDIF
         J=2*KRING
         IF(LBSIDE.LT.0) J=J-1
      IF(
     - J.EQ.6.OR.J.EQ.7
     -)THEN
            AZ=ABS(ZHIT)
            BZ=ACTG*GGF(4,J)
            ZTSLW=ZTSLW+(AZ-BZ)*(GGF(1,J)+GGF(2,J)*(AZ+BZ)+GGF(3,J)
     +      *(AZ*(AZ+BZ)+BZ**2))
      ENDIF
      ELSE
      IF(
     - KRING.EQ.1
     -)THEN
            ZTSLW=13.46-14.03*ACTG
      ELSE
      IF(
     - KRING.EQ.2
     -)THEN
               ZTSLW=15.23-31.278*ACTG+7.54731*ACTG**2
      ELSE
               ZTSLW=20.86-48.672*ACTG+13.663*ACTG**2
      ENDIF
      ENDIF
      IF(
     - KRING.EQ.1
     -)THEN
      IF(
     - LBSIDE.LT.0
     -)THEN
      IF(
     - ACTG.LT..37
     -)THEN
                  T1=10.30-26.1*ACTG
      ELSE
                  T1=3.88-18.5345*ABS(ACTG-.5427)
      ENDIF
               T1=T1-0.30+ 0.77*ACTG-0.7648*ACTG**2
      ELSE
      IF(
     - ACTG.LT..37
     -)THEN
                  T1=7.50-26.3*ACTG
      ELSE
                  T1=-1.95+6.84118*ABS(ACTG-.4)
      ENDIF
               T1=T1+3.21-14.10*ACTG+10.73*ACTG**2
      ENDIF
            ZTSLW=ZTSLW+T1
      ENDIF
      IF(
     - KRING.EQ.2
     -)THEN
      IF(
     - LBSIDE.LT.0
     -)THEN
      IF(
     - ACTG.LT..40
     -)THEN
                  T1=8.787-19.675*ACTG
      ELSE
                  T1=4.091-18.133*ABS(ACTG-.56)
      ENDIF
      ELSE
      IF(
     - ACTG.LT..48
     -)THEN
                  T1=1.983-12.667*ACTG
      ELSE
                  T1=-4.1125+13.574*ABS(ACTG-.5)
      ENDIF
      ENDIF
            ZTSLW=ZTSLW+T1
      ENDIF
      IF(
     - KRING.EQ.3
     -)THEN
            T1=8.336-12.3519*ACTG
      IF(
     - LBSIDE.LT.0
     -)THEN
               T1=T1-2.68+18.09*ACTG-15.95*ACTG**2
      ELSE
               T1=T1+1.20-14.489*ACTG+14.623*ACTG**2
               IF(NRUN.GE.8712.AND.NRUN.LE.9999)
     +         T1=T1+14.20-8.5415*ACTG-10.6000*ACTG**2
               IF(NRUN.GE.7592.AND.NRUN.LE.8711)
     +         T1=T1+4.16+17.97*ACTG-24.33*ACTG**2
      ENDIF
            ZTSLW=ZTSLW+T1
      ENDIF
      IF(
     - KRING.EQ.4
     -)THEN
      IF(
     - ACTG.LT..56
     -)THEN
               T1=5.58-15.183*ACTG
      ELSE
               T1=-3.30+8.1613*(ACTG-.5)
      ENDIF
      IF(
     - LBSIDE.LT.0
     -)THEN
               T1=T1+3.12-21.16*ACTG+18.90*ACTG**2
               IF(NRUN.GE.11038.AND.NRUN.LE.12554)
     +         T1=T1+.39+29.1785*ACTG-30.4402*ACTG**2
               IF(NRUN.GE.8712.AND.NRUN.LE.9999)
     +         T1=T1+7.30+13.8*ACTG-23.20*ACTG**2
               IF(NRUN.GE.7592.AND.NRUN.LE.8711)
     +         T1=T1+0.16+ 9.08*ACTG- 9.60*ACTG**2
               IF(NRUN.GE.6185.AND.NRUN.LE.7591)
     +         T1=T1-16.6+41.18*ACTG-20.60*ACTG**2
      ELSE
               T1=T1-0.62+12.25*ACTG-10.52*ACTG**2
      ENDIF
            ZTSLW=ZTSLW+T1
      ENDIF
         J=2*KRING
         IF(LBSIDE.LT.0) J=J-1
         AZ=ABS(ZHIT)
         BZ=ACTG*GG(4,J)
         ZTSLW=ZTSLW+(AZ-BZ)*(GG(1,J)+GG(2,J)*(AZ+BZ)+GG(3,J)
     +   *(AZ*(AZ+BZ)+BZ**2))
      IF(
     - KRING.GE.3
     -)THEN
            ZTSLW=ZTSLW+1.70E-2/.6727*(ZHIT-CTGTH*GG(4,J))
      ENDIF
         IF(KRING.GE.3) ZTSLW=ZTSLW*FREQR
      ENDIF
      TCORR=TDRIFT+ZTSLW
      GOTO IZZZ05
17011 CONTINUE
      IF(
     - TCORR.LT.THU(4)
     -)THEN
         TCOR=TCORR
      IF(
     - TCOR.GT.THL(4)
     -)THEN
            TCOR=TCOR+A4(1)+A4(3)*(TCOR-A4(2))**2+A4(4)*(TCOR-A4(2))**4
      ELSE
      IF(
     - TCOR.GT.THL(3)
     -)THEN
               TCOR=TCOR+A3(1)+A3(3)*(TCOR-A3(2))**2
     +         +A3(4)*(TCOR-A3(2))**4
      ELSE
      IF(
     - TCOR.GT.THL(2)
     -)THEN
                  TCOR=TCOR-A2(1)+A2(2)*((TCOR-A2(3))**4-
     +            (A2(1)-A2(3))**4)
      ELSE
                  TCOR=TCOR-A2(1)
      ENDIF
      ENDIF
      ENDIF
         IF(TCOR.GT.0..AND.TCOR.LT.120.) TCOR=TCOR-8.E-2*(TCOR-120.)
         TCORR=TCOR
      ENDIF
      GOTO IZZZ06
17013 CONTINUE
      ASSIGN 17018 TO IZZZ09
      GOTO 17017
17018 CONTINUE
         STGTC=CALCST(JCELL,17)*IODD*LBSIDE*STGCOR
      IF(
     - DELTA.GT.B1(3,KRING)
     -)THEN
            A12=B1(2,KRING)
      ELSE
            A12=B1(1,KRING)
      ENDIF
         CSGINV=SQRT(1.+((DELTA-B1(3,KRING))/B1(4,KRING))**2)
      IF(
     - TCORR.GT.A12
     -)THEN
               TANGCC=A12*(CSGINV-1.)
      ELSE
               TANGCC=TCORR*(CSGINV-1.)
      ENDIF
         TCORR=TCORR+TANGCC+STGTC
      GOTO IZZZ07
17017 CONTINUE
      D=DELTA
      Z=ZHIT
      ASSIGN 17020 TO IZZZ10
      GOTO 17019
17020 CONTINUE
      ASSIGN 17022 TO IZZZ11
      GOTO 17021
17022 CONTINUE
      U=(Z/1200.)**2
      STGCOR=STGDZ0*(1.+U*FRACZD)
      IF(STGCOR.LT..2) STGCOR=.2
      GOTO IZZZ09
17019 CONTINUE
      IF(
     - D.LT.-600.
     -)THEN
            I=1
      ELSE
      IF(
     - D.LT.0.
     -)THEN
               I=2
      ELSE
      IF(
     - D.LT.600.
     -)THEN
                  I=3
      ELSE
                  I=4
      ENDIF
      ENDIF
      ENDIF
         STGDZ0=Q(1,I)+ABS(D)*(Q(2,I)+Q(4,I)*D**2)+Q(3,I)*D**2
     +   +Q(5,I)*D**4
         STGDZ0=STGDZ0/44.9444
      GOTO IZZZ10
17021 CONTINUE
      IF(
     - D.LT.-500.
     -)THEN
            I=1
      ELSE
      IF(
     - D.LT.0.
     -)THEN
               I=2
      ELSE
      IF(
     - D.LT.400.
     -)THEN
                  I=3
      ELSE
                  I=4
      ENDIF
      ENDIF
      ENDIF
         FRACZD=P(1,I)+ABS(D)*(P(2,I)+P(4,I)*D**2)+P(3,I)*D**2
     +   +P(5,I)*D**4
      GOTO IZZZ11
17015 CONTINUE
      IWR=(KRING-1)*16+IW
      IF(
     - Y1.GT.0.
     -)THEN
         IND=2
      ELSE
         IND=1
      ENDIF
      SMAX=SMAXW(IND,IWR)
      SM0=SM01(IND,IWR)
      Y1COR=DSTORW(2*IND,IWR,ISEG)*Y1**2    +   DSTORW(5,IWR,ISEG)
      S0=ABS(Y1)-SM0
      IF(S0.GT.0.) Y1COR=Y1COR+DSTORW(2*IND-1,IWR,ISEG)*S0**2
      IF(
     - IW.EQ.1 .OR. IW.EQ.16
     -)THEN
         X=ABS(Y1/SMAX)
      IF(
     - IWR.EQ.1
     -)THEN
      IF(
     - IND.EQ.2
     -)THEN
      IF(
     - X.LT..52
     -)THEN
                  T=-.07*(1.-((2.*X-.52)/.52)**2)
      ELSE
      IF(
     - X.LT.1.
     -)THEN
                     T=.05*(1.-((2.*X-1.37)/.33)**2)
      ELSE
                     T=-.13
      ENDIF
      ENDIF
      ELSE
      IF(
     - X.LT..52
     -)THEN
                  T=-.05*(1.-((2.*X-.52)/.52)**2)
      ELSE
      IF(
     - X.LT.1.
     -)THEN
                     T=.035*(1.-((2.*X-1.34)/.30)**2)
      ELSE
                     T=-.13
      ENDIF
      ENDIF
      ENDIF
      ENDIF
      IF(
     - IWR.EQ.16
     -)THEN
      IF(
     - IND.EQ.2
     -)THEN
      IF(
     - X.LT..42
     -)THEN
                  T=.075*(1.-((2.*X-.42)/.42)**2)
      ELSE
      IF(
     - X.LT..67
     -)THEN
                     T=-.06*(1.-((2.*X-1.09)/.25)**2)
      ELSE
      IF(
     - X.LT..9
     -)THEN
                        T= .06*(1.-((2.*X-1.53)/.19)**2)
      ELSE
                        T=-.080
      ENDIF
      ENDIF
      ENDIF
      ELSE
      IF(
     - X.LT..50
     -)THEN
                  T= .05*(1.-((2.*X-.50)/.50)**2)
      ELSE
      IF(
     - X.LT..75
     -)THEN
                     T=-.02*(1.-((2.*X-1.25)/.25)**2)
      ELSE
      IF(
     - X.LT.1.
     -)THEN
                        T=.025*(1.-((2.*X-1.70)/.20)**2)
      ELSE
                        T=-.03
      ENDIF
      ENDIF
      ENDIF
      ENDIF
      ENDIF
      IF(
     - IWR.EQ.17
     -)THEN
      IF(
     - IND.EQ.2
     -)THEN
      IF(
     - X.LT..40
     -)THEN
                  T=-.085*(1.-((2.*X-.40)/.40)**2)
      ELSE
      IF(
     - X.LT..62
     -)THEN
                     T= .05*(1.-((2.*X-1.02)/.22)**2)
      ELSE
      IF(
     - X.LT.1.
     -)THEN
                        T=-.04*(1.-((2.*X-1.47)/.23)**2)
      ELSE
                        T= .170
      ENDIF
      ENDIF
      ENDIF
      ELSE
      IF(
     - X.LT..37
     -)THEN
                  T=-.10*(1.-((2.*X-.37)/.37)**2)
      ELSE
      IF(
     - X.LT..60
     -)THEN
                     T= .06*(1.-((2.*X- .97)/.23)**2)
      ELSE
      IF(
     - X.LT..72
     -)THEN
                        T=-.03*(1.-((2.*X-1.32)/.12)**2)
      ELSE
      IF(
     - X.LT..9
     -)THEN
                           T= .03*(1.-((2.*X-1.58)/.14)**2)
      ELSE
                           T=-.07
      ENDIF
      ENDIF
      ENDIF
      ENDIF
      ENDIF
      ENDIF
      IF(
     - IWR.EQ.32
     -)THEN
      IF(
     - IND.EQ.2
     -)THEN
      IF(
     - X.LT..27
     -)THEN
                  T=.120*(1.-((2.*X-.27)/.27)**2)
      ELSE
      IF(
     - X.LT..46
     -)THEN
                     T=-.08*(1.-((2.*X- .73)/.19)**2)
      ELSE
      IF(
     - X.LT..64
     -)THEN
                        T= .055*(1.-((2.*X-1.10)/.18)**2)
      ELSE
                        T=0.
      ENDIF
      ENDIF
      ENDIF
      ELSE
      IF(
     - X.LT..43
     -)THEN
                  T= .05*(1.-((2.*X-.43)/.43)**2)
      ELSE
      IF(
     - X.LT..67
     -)THEN
                     T=-.025*(1.-((2.*X-1.10)/.24)**2)
      ELSE
      IF(
     - X.LT.1.
     -)THEN
                        T=.020*(1.-((2.*X-1.55)/.21)**2)
      ELSE
                        T=-.070
      ENDIF
      ENDIF
      ENDIF
      ENDIF
      ENDIF
      IF(
     - IWR.EQ.33
     -)THEN
      IF(
     - IND.EQ.2
     -)THEN
      IF(
     - X.LT..42
     -)THEN
                  T=-.09*(1.-((2.*X-.42)/.42)**2)
      ELSE
      IF(
     - X.LT..68
     -)THEN
                     T= .06*(1.-((2.*X-1.10)/.26)**2)
      ELSE
      IF(
     - X.LT..95
     -)THEN
                        T=-.055*(1.-((2.*X-1.54)/.18)**2)
      ELSE
                        T=.170
      ENDIF
      ENDIF
      ENDIF
      ELSE
      IF(
     - X.LT..44
     -)THEN
                  T=-.11*(1.-((2.*X-.44)/.44)**2)
      ELSE
      IF(
     - X.LT..68
     -)THEN
                     T= .075*(1.-((2.*X-1.12)/.24)**2)
      ELSE
      IF(
     - X.LT..9
     -)THEN
                        T=-.05*(1.-((2.*X-1.53)/.17)**2)
      ELSE
                        T= .080
      ENDIF
      ENDIF
      ENDIF
      ENDIF
      ENDIF
      IF(
     - IWR.EQ.48
     -)THEN
      IF(
     - IND.EQ.2
     -)THEN
      IF(
     - X.LT..34
     -)THEN
                  T= .08*(1.-((2.*X-.34)/.34)**2)
      ELSE
      IF(
     - X.LT..85
     -)THEN
                     T=-.035*(1.-((2.*X- .99)/.31)**2)
      ELSE
                     T=.150
      ENDIF
      ENDIF
      ELSE
      IF(
     - X.LT..30
     -)THEN
                  T=.035*(1.-((2.*X-.30)/.30)**2)
      ELSE
                  T=-.035*(1.-((2.*X-1.10)/.50)**2)
      ENDIF
      ENDIF
      ENDIF
      IF(
     - IWR.EQ.49
     -)THEN
      IF(
     - IND.EQ.2
     -)THEN
      IF(
     - X.LT..42
     -)THEN
                  T=-.08*(1.-((2.*X-.42)/.42)**2)
      ELSE
      IF(
     - X.LT..70
     -)THEN
                     T=.018*(1.-((2.*X-1.07)/.23)**2)
      ELSE
                     T=.035
      ENDIF
      ENDIF
      ELSE
      IF(
     - X.LT..50
     -)THEN
                  T=-.09*(1.-((2.*X-.50)/.50)**2)
      ELSE
      IF(
     - X.LT..85
     -)THEN
                     T= .080*(1.-((2.*X-1.30)/.30)**2)
      ELSE
                     T=-.060
      ENDIF
      ENDIF
      ENDIF
      ENDIF
      IF(
     - IWR.EQ.64
     -)THEN
      IF(
     - IND.EQ.2
     -)THEN
      IF(
     - X.LT..35
     -)THEN
                  T= .09*(1.-((2.*X-.35)/.35)**2)
      ELSE
      IF(
     - X.LT..64
     -)THEN
                     T=-.07*(1.-((2.*X- .99)/.29)**2)
      ELSE
      IF(
     - X.LT..85
     -)THEN
                        T= .05*(1.-((2.*X-1.44)/.16)**2)
      ELSE
                        T=-.09
      ENDIF
      ENDIF
      ENDIF
      ELSE
      IF(
     - X.LT..40
     -)THEN
                  T= .09*(1.-((2.*X-.40)/.40)**2)
      ELSE
                  T=0.
      ENDIF
      ENDIF
      ENDIF
         Y1COR=Y1COR+T
      ENDIF
      Y1=Y1+Y1COR
      GOTO IZZZ08
17007 CONTINUE
      S=G/F
      U=-S/F
      S=-.5*S
      IF(
     - ABS(U).GT..3
     -)THEN
      IF(
     - U.LT..98
     -)THEN
            SQTVAL=F*(SQRT(1.-U)-1.)
      ELSE
            SQTVAL=0.
      ENDIF
      ELSE
         VAL=-S*(1.+.25*U+.125*U**2)
         QQ=S*U**3/12.8
         N=5
15004 CONTINUE
      IF(
     - ABS(QQ).GT.EPS .AND.N.LT.15
     -)THEN
           VAL=VAL-QQ
           QQ=QQ*U*(1.-1.5/N)
           N=N+1
      GOTO 15004
      ENDIF
15005 CONTINUE
         SQTVAL=VAL
      ENDIF
      GOTO IZZZ04
      END
