      SUBROUTINE USER(INDEX)
      IMPLICIT INTEGER*2 (H)
      REAL LBACC
      LOGICAL TBIT
      EXTERNAL OR,AND
      REAL OR,AND
#include "cdata.for"
#include "cpatlm.for"
#include "czvpar.for"
#include "cworkzv.for"
#include "cjdrch.for"
#include "cheadr.for"
#include "ciouni.for"
#include "cutsr1.for"
      COMMON/CGBOOK/PLOT(116,13)
      COMMON /CREDRS/ LBAB2,LBAB4,LBAB8,LBAT2M,ARES(40)
                      INTEGER IRES(40)
                      EQUIVALENCE (ARES(1),IRES(1))
      COMMON /CTLIM/ ISECLF
      COMMON /CADMIN/ IEVTP,NRREAD,NRWRIT
      COMMON/CALIBR/JPOINT(100),
     +HMUCAL(8370),HLGMAI(5760),HTAGSY(384),HJPULS(9216),
     +DELTA0(96,2),DELTA1(96,2),DELTA2(96,2),DELTA5(96,2),DELTA6(96,2)
      COMMON /CIGG/ IPRN,IGG(80),JIGG(80)
      LOGICAL*1 CHOP
      DATA LBSTRT/0/, LIMHIT/12/,MKMUT1/8/,LIMHFW/8/
 2000 FORMAT('1')
 2004 FORMAT('  RUN AND EVENT HAS NO ZVTX BANK ',2I5)
 2006 FORMAT(' RUN AND EVENT ',2I6,' HAS RETURN1 IN LGCALB')
 2010 FORMAT(' ***** ERROR IN USRED82 (INFORM PST), EVENT ',3I6)
      IF(
     - INDEX.EQ.0
     -)THEN
        CALL DBASE(0,IDBCHP)
        NRUN = 63
        NCALI = 1
        LUNITA(1) = 22
        LUNITA(2) = 0
        CALL R1MODE
        ISECLF= 25
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
        CALL HLIMIT(6000)
        CALL HBOOK1(111,'TRIGGER BITS (INPUT)      $', 50,0.,   50.)
        CALL HBOOK1(112,'TRIGGER BITS (TRCK-ACC)   $',100,0.,  100.)
        CALL HBOOK1(113,'TRIGGER BITS ACCEPTED     $',100,0.,  100.)
        CALL HBOOK1(1,'ZVERTEX (ALL)$',100,-2000.,2000.)
        CALL HBOOK1(2,'NHITS IN RING1 (ZVERTEX < 300 MMS)$', 50,0.,200.)
        CALL HBOOK1(3,'NUMBER OF TRACKS$',100,0.,50.)
        CALL HBOOK1(4,'LG ENERGY SUM (ALL)  $',100,0.02,40000.)
        CALL HBOOK1(5,'LG ENDCAP ENERGY (BAD BLOCKS) $',100,0.,40000.)
        CALL HBOOK1(  6,'TRACK CURVATURE           $', 50,  0., .010)
        CALL HBOOK1(7,'ZMIN (TRACKS)                 $', 50,  0.,1000.)
        CALL HBOOK1(8,'RMIN                          $', 50,  0., 300.)
        CALL HBOOK1(9,'     ACCEPTED TRACKS          $', 50,  0.,  25.)
        CALL HBOOK1(10,'REJECT INDEX                 $', 50,  0.,  50.)
        CALL HBLACK(0)
        PRINT2000
        INDEX=INDEX+1
        IF(.TRUE.) RETURN
      ENDIF
      IF(
     - INDEX.EQ.100
     -)THEN
      DO 13002 I = 2,80
        JIGG(I) = JIGG(I) + IGG(I)
13002 CONTINUE
13003 CONTINUE
        CALL STAT82
        CALL DBASE(3,IDBCHP)
      DO 13004 I = 1,80
        IGG(I) = JIGG(I)
13004 CONTINUE
13005 CONTINUE
        CALL STAT82
        CALL H1EVLI(    0)
        CALL HISTDO
        IF(.TRUE.) RETURN
      ENDIF
      IF(INDEX.GT.2 .AND. INDEX.LE.5) IGG(INDEX+2) = IGG(INDEX+2) + 1
16000 CONTINUE
      INDREJ = 0
      IZZZSE=INDEX
      GOTO 14002
14003 CONTINUE
      IF(
     - HHEADR(18).GT.NRUN
     -)THEN
              NRUN=HHEADR(18)
      IF(
     - NRUN.NE.IGG(1)
     -)THEN
                   CALL DBASE(2,IDBCHP)
                   IGG(3) = IGG(3) + 1
                   IF(IGG(4).NE.0) CALL STAT82
                   IGG(1) = NRUN
      DO 13006 I = 2,80
                        JIGG(I) = JIGG(I) + IGG(I)
                        IGG(I) = 0
13006 CONTINUE
13007 CONTINUE
                   PRINT 2007,IGG(1)
 2007              FORMAT(1H0/
     +             1X,10('='),20X,10('=')/
     +             1X,10('='),' START OF RUN ',I5,1X,10('=')/
     +             1X,10('='),20X,10('='))
      IF(
     - LBSTRT.EQ.0
     -)THEN
                        LBSTRT = 1
                        ZFITLM(1) = 70.
                        ZFITLM(2) = 35.
      ENDIF
                   CHOP=.FALSE.
                   IF(IGG(1).LT.NRUNST) CHOP=.TRUE.
      IF(
     - NOTTOT.NE.0
     -)THEN
      DO 13008 I=1,NOTTOT
                             IF(IGG(1).EQ.NOTRUN(I)) CHOP=.TRUE.
13008 CONTINUE
13009 CONTINUE
      ENDIF
                   CHOP = CHOP.OR.IDBCHP.NE.0
                   IF(CHOP) PRINT 2018,IGG(1)
 2018              FORMAT(
     +             1X,10('=')/
     +             1X,10('='),' RUN',I6, ' AXED'/
     +             1X,10('='))
      ENDIF
              INDEX=INDEX+1
              IF(.TRUE.) RETURN
      ELSE
      IF(
     - HHEADR(18).LT.NRUN .OR. HHEADR(18).EQ.63
     -)THEN
                   CHOP = .TRUE.
                   PRINT 2011, HHEADR(18)
 2011              FORMAT(' USRED82 .. RUN',I15,' RECORD ENCOUNTERED')
                   CALL DBASE(10,IDBCHP)
                   IF(.TRUE.) RETURN
      ELSE
                   CHOP = .FALSE.
                   PRINT 2020, NRUN
 2020              FORMAT(' USRED82 - CONTINUING WITH RUN',I15)
                   INDEX = 2
                   IF(.TRUE.) RETURN
      ENDIF
      ENDIF
      GOTO 14001
14004 CONTINUE
        INDEX = 1
        IF(HHEADR(18).EQ.NRUNST .AND. HHEADR(19).LT.NEVTST) RETURN
        IF(HHEADR(18).EQ.27987 .AND. HHEADR(19).EQ.5301) RETURN
        IF(HHEADR(18).EQ.27989 .AND. HHEADR(19).EQ.4893) RETURN
        IF(HHEADR(18).EQ.27989 .AND. HHEADR(19).EQ.5555) RETURN
        IF(HHEADR(18).EQ.27989 .AND. HHEADR(19).EQ.5965) RETURN
        IF(HHEADR(18).EQ.27990 .AND. HHEADR(19).EQ.  91) RETURN
        IF(CHOP) RETURN
        CALL DBASE(1,IDBCHP)
        IGG(4) = IGG(4) + 1
        INDREJ =  1
      IF(
     - HHEADR(20).GE.64
     -)THEN
      GOTO 16001
      ENDIF
        IERRLG = 0
        CALL LGCALB(&ERRLGC)
        CALL TRCK82(LBACC,LBTRB1,LBTRB2,IERRLG)
        LBTRBT = LBTRB1
        TRBIT = 0.
      DO 13010 I=1,32
          TRBIT = TRBIT + 1.
      IF(
     - TBIT(LBTRBT,31)
     -)THEN
            CALL HF1(111,TRBIT,1.)
      ENDIF
          LBTRBT = ISHFTR(LBTRBT,1)
          IF(I.EQ.16) LBTRBT = LBAT2M
13010 CONTINUE
13011 CONTINUE
        LBTRBT = LBAB2
        TRBIT = 0.
      DO 13012 I=1,48
          TRBIT = TRBIT + 1.
      IF(
     - TBIT(LBTRBT,31)
     -)THEN
            CALL HF1(112,TRBIT,1.)
      ENDIF
          LBTRBT = ISHFTR(LBTRBT,1)
          IF(I.EQ.32) LBTRBT = LBAB4
          IF(I.EQ.16) LBTRBT = LBAB8
13012 CONTINUE
13013 CONTINUE
        INDEX  =  4
        IF(AND(LBACC,2).NE.0) RETURN
        INDEX  =  4
        IF(AND(LBACC,4).NE.0) RETURN
        INDEX  =  4
        IF(AND(LBACC,8).NE.0) RETURN
        INDREJ =  4
        INDEX  = 11
      IF(
     - AND(LBACC,1).NE.0
     -)THEN
      GOTO 16001
      ENDIF
        INDREJ =  5
        INDEX  =  1
      GOTO 16001
14005 CONTINUE
        INDEX  = 11
        INDREJ = 11
      IF(
     - AND(LBACC,2).NE.0
     -)THEN
      GOTO 16001
      ENDIF
        INDEX  = 5
        IF(AND(LBACC,4).NE.0) RETURN
        INDEX = 1
        IPJCA  = IDATA(IQJETC)
        IPJCA2 = IPJCA*2
        NHITR1 = (HDATA(IPJCA2+27) - HDATA(IPJCA2+3)) / 4
        IPZV   = IDATA(IQZVTX)
        IF(IPZV.LE.0) WRITE(6,2004) HHEADR(18),HHEADR(19)
      IF(
     - IPZV.GT.0
     -)THEN
          IFLAG  = IDATA(IPZV+6)
          ZVTX   = ADATA(IPZV+1)
          PEAK   = ADATA(IPZV+4)
          IGG(IFLAG+27) = IGG(IFLAG+27) + 1
      IF(
     - IFLAG.LT.3
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
        IF(IPPATR.LE.0) PRINT2005,HHEADR(18),HHEADR(19)
 2005   FORMAT('  RUN AND EVENT HAS NO PATR BANK ',2I5)
        INDREJ = 21
        IPPATR = IDATA(IQPATR)
        IF(IPPATR.LE.0) LBACC = OR(LBACC,32)
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
     - AND(LBAB4,MKMUT1).NE.0
     -)THEN
          FACC = 0
          TRAC = 0
          LBSHRT = 0
          IF(AND(LBACC,4).NE.0) LBSHRT = 1
          XO = 0.
          YO = 0.
          ZMIN  = 100000.
          RMIN0 = 100000.
      DO 13014 IPTR=IPTR0,IPTR9,LDTR
      IF(
     - IDATA(IPTR+24).GT.LIMHFW
     -)THEN
              FACC = OR(FACC,1)
              CRV = ABS(ADATA(IPTR+25))
              CALL HF1(6,CRV,1.)
      IF(
     - CRV .LE..00150
     -)THEN
                FACC = OR(FACC,2)
                AZV = ABS(ADATA(IPTR+31))
                ZMIN = AMIN1(AZV,ZMIN)
      IF(
     - AZV .LT. 300.  .OR.  LBSHRT .NE. 0
     -)THEN
                  FACC = OR(FACC,4)
                  CALL DRTRCK(IPTR,XO,YO,RMIN)
                  RMIN = ABS(RMIN)
                  RMIN0 = AMIN1(RMIN0,RMIN)
      IF(
     - ABS(RMIN).LT.   50.
     -)THEN
                    TRAC = TRAC + 1.
                    FACC = OR(FACC,8)
      ENDIF
      ENDIF
      ENDIF
      ENDIF
13014 CONTINUE
13015 CONTINUE
      ELSE
          FACC = 0
          TRAC = 0
          LBSHRT = 0
          IF(AND(LBACC,4).NE.0) LBSHRT = 1
          XO = 0.
          YO = 0.
          ZMIN  = 100000.
          RMIN0 = 100000.
      DO 13016 IPTR=IPTR0,IPTR9,LDTR
      IF(
     - IDATA(IPTR+24).GT.LIMHIT
     -)THEN
              FACC = OR(FACC,1)
              CRV = ABS(ADATA(IPTR+25))
              CALL HF1(6,CRV,1.)
      IF(
     - CRV .LE..00150
     -)THEN
                FACC = OR(FACC,2)
                AZV = ABS(ADATA(IPTR+31))
                ZMIN = AMIN1(AZV,ZMIN)
      IF(
     - AZV .LT. 300.  .OR.  LBSHRT .NE. 0
     -)THEN
                  FACC = OR(FACC,4)
                  CALL DRTRCK(IPTR,XO,YO,RMIN)
                  RMIN = ABS(RMIN)
                  RMIN0 = AMIN1(RMIN0,RMIN)
      IF(
     - ABS(RMIN).LT.   50.
     -)THEN
                    IF(TRAC.GT.0.) FACC = OR(FACC,8)
                    TRAC = TRAC + 1.
      ENDIF
      ENDIF
      ENDIF
      ENDIF
13016 CONTINUE
13017 CONTINUE
      ENDIF
        CALL HF1(7,ZMIN ,1.)
        CALL HF1(8,RMIN0,1.)
        IF(TRAC.GT. 8.) TRAC =  8.
        IF(AND(LBACC,4).NE.0) TRAC = TRAC + 10.
        CALL HF1(9,TRAC,1.)
      IF(
     - AND(FACC,8).EQ.0
     -)THEN
          INDREJ = 23
          IF(AND(FACC,1).NE.0) INDREJ = 24
          IF(AND(FACC,2).NE.0) INDREJ = 25
          IF(AND(FACC,4).NE.0) INDREJ = 26
          INDEX = 1
      GOTO 16001
      ENDIF
        INDREJ = 30
        INDEX  = 11
      GOTO 16001
14002 CONTINUE
      GOTO(
     -14003,14004,14000,14005,14006
     -), IZZZSE
14000 CONTINUE
        PRINT 2010, HHEADR(18),HHEADR(19),INDEX
        INDEX = 1
        RETURN
14001 CONTINUE
      IF(.NOT.(
     - .TRUE.
     -))GOTO 16000
16001 CONTINUE
      IF(
     - INDEX.EQ.11 .AND. AND(LBACC,14).NE.0
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
CAV            00058800
      ENDIF
          CALL BDLS('PATR',10)
          CALL BDLS('JHTL',10)
          IPFAST = 0
          CALL INPATR
          ZFITLM(1) = 70.
          ZFITLM(2) = 35.
      ENDIF
      ENDIF
      IWRT = 0
      IF(
     - INDEX.EQ.11
     -)THEN
        IPHEAD = IDATA(IQHEAD)*2
        IF(AND(LBACC, 2).NE.0) IWRT = 1
        IF(AND(LBACC, 4).NE.0) IWRT = IWRT + 8
        IF(AND(LBACC, 8).NE.0) IWRT = IWRT +16
        IF(IPHEAD.GT.0) HDATA(IPHEAD+27) = IWRT
        LBTRBT = LBAB2
        TRBIT = 0.
      DO 13018 I=1,48
          TRBIT = TRBIT + 1.
          IF(TBIT(LBTRBT,31)) CALL HF1(113,TRBIT,1.)
          LBTRBT = ISHFTR(LBTRBT,1)
          IF(I.EQ.32) LBTRBT = LBAB4
          IF(I.EQ.16) LBTRBT = LBAB8
13018 CONTINUE
13019 CONTINUE
      ENDIF
      IF(INDEX.EQ.11) IGG( 10) = IGG( 10) + 1
      ANDREJ = INDREJ
      CALL HF1(10,ANDREJ,1.)
      IF(INDREJ.GT.0) IGG(INDREJ+10) = IGG(INDREJ+10) + 1
      IF(IWRT  .GT.0) IGG(IWRT+50) = IGG(IWRT+50) + 1
      IMPRT=IDATA(IQMPRS)
      IF(
     - IMPRT.GT.0.AND.INDEX.EQ.11
     -)THEN
        IRFLAG=HDATA(2*IMPRT+3)
        IF(AND(IRFLAG,4).NE.0) IGG(49)=IGG(49)+1
        IF(AND(IRFLAG,8).NE.0) IGG(50)=IGG(50)+1
      ENDIF
      RETURN
17000 CONTINUE
        IGG( 9) = IGG( 9) + 1
        IF(IGG( 9).LT.20.AND.IPRN.GT.0) PRINT2006,HHEADR(18),HHEADR(19)
        IERRLG = 1
      END
