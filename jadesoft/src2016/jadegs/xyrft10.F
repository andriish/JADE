      SUBROUTINE XYRFT1(IPTR,IPJHTL,ERRFAC,LDTR)
      IMPLICIT INTEGER*2 (H)
      REAL*8   S0D,S1D,S2D,S3D,S4D,S8D,DETD
      COMMON/XYFVT1/MODXYV
#include "cdata.for"
#include "cgeo1.for"
#include "calibr.for"
#include "cworkpr.for"
#include "cworkeq.for"
      EQUIVALENCE
     ,          (HPCO0 ,HPWRK(17)),(HPCO9 ,HPWRK(18)),(HLDCO ,HPWRK(19))
     ,         ,(ICELL ,IDWRK( 1)),(MHIT  ,IDWRK( 2)),(IRING ,IDWRK( 3))
     ,         ,(IERRCD,IDWRK( 4)),(NTRKEL,IDWRK( 5))
#include "cpatlm.for"
#include "cjdrch.for"
#include "cdsmax.for"
      INTEGER DATE(5), IDAY /0/
      INTEGER NCHECK(5)/5*8/
      REAL RCHECK(12,2,5)/
     +   0.,  .004,  .01,  .045,  .06,  .1,  1.,  6., 4*0.,
     +   0.,  .65,   .79,  .86,   .89, .91, .94,  1., 4*1.,
     +   0.,  .004,  .01,  .045,  .06,  .1,  1.,  6., 4*0.,
     +   0.,  .65,   .79,  .86,   .89, .91, .94,  1., 4*1.,
     +   0.,  .004,  .01,  .045,  .06,  .1,  1.,  6., 4*0.,
     +   0.,  .65,   .79,  .86,   .89, .91, .94,  1., 4*1.,
     +   0.,  .004,  .01,  .045,  .06,  .1,  1.,  6., 4*0.,
     +   0.,  .65,   .79,  .86,   .89, .91, .94,  1., 4*1.,
     +   0.,  .004,  .01,  .045,  .06,  .1,  1.,  6., 4*0.,
     +   0.,  .65,   .79,  .86,   .89, .91, .94,  1., 4*1./
      REAL RESCUT/8./,CKAPP/.966/
      DIMENSION ISORT1(71),ISORT2(3,71),IRESHT(71),ISORT3(91)
     +,ISORT4(2,91)
      DATA KPRT1/0/,NPRT1/50/,IQJETC/0/,IQHEAD/0/
      DATA MASK1/Z2FFFFFF/,MASK2/ZFFFF02FF/,MASK3/ZFFFFF1FF/
      DATA LBINIT /0/
      IF(
     - LBINIT .EQ. 0
     -)THEN
         LBINIT = 1
         IQJETC = IBLN('JETC')
         IQHEAD = IBLN('HEAD')
         CALL DAY2(DATE)
         IDAY = DATE(1)*1000 + DATE(2)
         RESMS = .020**2/2. * .16 * (1. + ALOG10(.16) /9.) * 155.45**2
         WRITE(6,137)
 137     FORMAT(/,' *** XYRFT1 ***  A NEW R-PHI FITTING ROUTINE',/,
     +            '                 TEST VERSION 3. (J. SPITZER)',/
     +     ' BIT 512 OR 1024 IS SET IN THE PROGRAM IDENTIFIER WORD',/,
     +' IN CASE VERTEX CONSTRAINT WAS NOT OR WAS USED RESP.',/,
     +' COVARIANCE MATRIX IS PROVIDED WITH BIT 2048 SWITCHED ON',/,
     +' IF THE PATR BANK ALREADY HAS THE LARGER LENGTH, THE NUMBER',/,
     +' OF HITS USED IN THE FIT IS AT LEAST 10 AND THE FIT CONVERGED.',
     +/)
      ENDIF
      IPHEAD = IDATA(IQHEAD)*2
      NRUN = HDATA(IPHEAD+10)
      NEV  = HDATA(IPHEAD+11)
      ITRK = IDATA(IPTR+1)
      HPFREE = 1
      IPV    = ICALIB(10)
      XO     = ACALIB(IPV+ 1)
      YO     = ACALIB(IPV+ 3)
      CURVXY=ADATA(IPTR+25)
      IF(ABS(CURVXY).LT.1.E-9) CURVXY = SIGN(1.E-9,CURVXY)
      DDR0=DISTXY(ADATA(IPTR+5),ADATA(IPTR+6),ADATA(IPTR+8),
     +ADATA(IPTR+9),1./CURVXY,XO,YO,XP,YP,FI)
      FV     = ERRFAC
      IF(FV .LT. .50) FV = .50
      IF(
     - NRUN.LT.24200
     -)THEN
         SRESO=.160
      ELSE
         SRESO=.100
      ENDIF
      SIGMIN=(SRESO/1.6)**2
      PTRANS = ABS(0.0299792458*BKGAUS/CURVXY) * .001
      RESV=.0100+.25*SIN(FI)**2+RESMS/PTRANS**2
      WGHT0  = (SRESO/FV)**2 / RESV
      INDBIT=512
      XX    =  ADATA(IPTR+12) - ADATA(IPTR+5)
      YY    =  ADATA(IPTR+13) - ADATA(IPTR+6)
      RR    = 0.5*SQRT(XX**2+YY**2)
      IF(
     - RR.LT.10.
     -)THEN
      IF(
     - KPRT1.LT.NPRT1
     -)THEN
            KPRT1=KPRT1+1
            WRITE(6,848) NRUN,NEV,ITRK,RR
848         FORMAT(' ******** RUN,EV,TRACK',I8,I6,I3,/,
     +      ' HALF DISTANCE OF FIRST AND LAST POINTS',E14.3,
     +      ', XYRFT1 DOES NOT ATTEMPT R-PHI FIT')
      ENDIF
         RETURN
      ENDIF
      HPCO0  = HPFREE
      LHIT   = 14
      INDFET = 2
      CALL JFETCH(IPTR,IPJHTL,WRK(HPCO0),LHIT,IPRES,INDFET,XO,YO)
      HLDCO  = LHIT
      HPCO9  = IPRES - 1
      HPAR0  = IPRES
      HLDPA  = 20
      HPAR9  = HPAR0 + HLDPA - 1
      HPFREE = HPAR9 + 1
      XT     = WRK (IPRES   )
      YT     = WRK (IPRES+ 1)
      CSROT  = WRK (IPRES+ 2)
      SNROT  = WRK (IPRES+ 3)
      XOR    =- XT*CSROT -  YT*SNROT
      YOR    =  XT*SNROT -  YT*CSROT
      X0     = (XO-XT)*CSROT+(YO-YT)*SNROT
      Y0     =-(XO-XT)*SNROT+(YO-YT)*CSROT
      ZVERT = ADATA(IPTR+31)
      TGTH = ADATA(IPTR+30)
      CSTH   = WRK (IPRES+11)
      SNTH   = WRK (IPRES+12)
      IWRK(IPRES+10)=0
      SIG=ADATA(IPTR+23)**2
      IF(SIG.LT.1.E-5) SIG=1.E-5
      SIG11=(.5*ADATA(IPTR+26))**2/SIG
      XHCUT=RR+200.
      XREGA= 100000.
      XREGB=-100000.
      NHALL=0
      NHWIR=0
      NHPOT=0
      IPCO=HPCO0
16000 CONTINUE
         NHWIR=NHWIR+1
         IF(NHWIR.GT.70) RETURN
         ISORT1(NHWIR)=NHWIR
         ISORT2(1,NHWIR)=IPCO
         ISORT2(3,NHWIR)=0
         IW0=IWRK(IPCO)
         ICL0=IWRK(IPCO+9)
         LFL=0
15000 CONTINUE
      IF(
     - IPCO.LE.HPCO9
     -)THEN
            IW9=IWRK(IPCO)
            ICL9=IWRK(IPCO+9)
      IF(
     - IW9.EQ.IW0 .AND. ICL9.EQ.ICL0
     -)THEN
               NHALL=NHALL+1
               IF(NHALL.GT.90) RETURN
               IF(ISORT2(3,NHWIR).EQ.0) ISORT2(2,NHWIR)=NHALL
               ISORT2(3,NHWIR)=ISORT2(3,NHWIR)+1
               XA=WRK(IPCO+3)
               LBGOOD=IWRK(IPCO+10)
      IF(
     - LBGOOD.LE.2
     -)THEN
      IF(
     - ABS(XA).GT.XHCUT
     -)THEN
                     ISORT3(NHALL)=-1
      ELSE
                     ISORT3(NHALL)= 1
                     LFL=1
                     IF(XA.LT.XREGA) XREGA=XA
                     IF(XA.GT.XREGB) XREGB=XA
      ENDIF
                  IWRK(IPCO+10)=1
      ELSE
                  ISORT3(NHALL)=-1
      ENDIF
               IPCO=IPCO+HLDCO
      ELSE
      GOTO 15001
      ENDIF
      GOTO 15000
      ENDIF
15001 CONTINUE
      IF(
     - LFL.EQ.1
     -)THEN
            NHPOT=NHPOT+1
      ELSE
            ISORT3(ISORT2(2,NHWIR))=-2
      ENDIF
      IF(.NOT.(
     - IPCO.GT.HPCO9
     -))GOTO 16000
16001 CONTINUE
      IF(NHPOT.LT.4) RETURN
      XHF=.5*(XREGA+XREGB)
      RRPL=.5*(XREGB-XREGA)
      RRMI=RRPL
      IF(
     - ABS(XHF).GT.RR
     -)THEN
         RRPL=RRPL+XHF-SIGN(RR,XHF)
         RRMI=RRMI-XHF+SIGN(RR,XHF)
         XHF=SIGN(RR,XHF)
      ENDIF
      LCHC=0
      IF(
     - ABS(CURVXY*RR).GT.CKAPP
     -)THEN
         LCHC=1
         CURVXY=SIGN(CKAPP/RR,CURVXY)
      ENDIF
      IF(
     - ABS(CURVXY)*RR .GT. .04
     -)THEN
         LFTYP=1
         P1=CURVXY/SQRT(1.-(CURVXY*RR)**2)
         AAH=(RR-XHF)*(RR+XHF)
         PAR3=SAGCIR(1.,P1,AAH,SAGPR,1.E-4)
         PAR2=-P1/(1.+PAR3*P1)*XHF
         CSI2GM=1.+PAR2**2
         PAR1=CURVXY*SQRT(CSI2GM)
      ELSE
         LFTYP=2
         PAR3=.5*CURVXY*(RR-XHF)*(RR+XHF)
         PAR2=-CURVXY*XHF
         CSI2GM=1.+PAR2**2
         PAR1=-0.5*CURVXY*SQRT(CSI2GM)*CSI2GM
      ENDIF
      XMIN=PAR2/PAR1-CKAPP/ABS(CURVXY)
      XMAX=PAR2/PAR1+CKAPP/ABS(CURVXY)
      DISCUT=400.
      KFLIP=1
      ASSIGN 17001 TO IZZZ01
      GOTO 17000
17001 CONTINUE
      IF(NHFIT.LT.4) RETURN
      IF(
     - NHFIT.LT.6
     -)THEN
      ASSIGN 17003 TO IZZZ02
      GOTO 17002
17003 CONTINUE
         RETURN
      ENDIF
      IF(
     - ABS(DA).GT.1.5 .OR. ABS(DB).GT..1 .OR. LCHC.EQ.1
     -)THEN
      IF(
     - LFTYP.EQ.2
     -)THEN
            LFTYP=1
            PAR1=CUROUT*SQRT(CSI2GM)
            XMIN=PAR2/PAR1-CKAPP/ABS(CUROUT)
            XMAX=PAR2/PAR1+CKAPP/ABS(CUROUT)
      ENDIF
      ASSIGN 17005 TO IZZZ03
      GOTO 17004
17005 CONTINUE
      ENDIF
      IF(NHFIT.LT.NHPOT/2) RETURN
      NHFTLS=NHFIT
      PAR1LS=PAR1
      PAR2LS=PAR2
      PAR3LS=PAR3
      CSI2LS=CSI2GM
      CURLST=CUROUT
      XMAXLS=XMAX
      XMINLS=XMIN
      SIG11L=SIG11
      SIGLST=SIG
      NHWIRV=NHWIR+1
      ISORT1(NHWIRV)=NHWIRV
      ISORT2(1,NHWIRV)=-200
      INDMAX=NHFIT/8+1
      IF(INDMAX.GT.8) INDMAX=8
      INDFIT=0
15002 CONTINUE
      IF(
     - INDFIT.LT.INDMAX
     -)THEN
         INDFIT=INDFIT+1
      ASSIGN 17007 TO IZZZ04
      GOTO 17006
17007 CONTINUE
      IF(
     - LNOCON.EQ.1
     -)THEN
            NHFIT=NHFTLS
            PAR1=PAR1LS
            PAR2=PAR2LS
            PAR3=PAR3LS
            CSI2GM=CSI2LS
            CUROUT=CURLST
            XMAX=XMAXLS
            XMIN=XMINLS
            SIG11=SIG11L
            SIG=SIGLST
      ASSIGN 17008 TO IZZZ02
      GOTO 17002
17008 CONTINUE
            RETURN
      ENDIF
      IF(
     - SIG.LT.SIGMIN
     -)THEN
      GOTO 15003
      ENDIF
      IF(
     - INDFIT.GE.2
     -)THEN
      ASSIGN 17010 TO IZZZ05
      GOTO 17009
17010 CONTINUE
      IF(
     - LSTOP.EQ.1
     -)THEN
               INDFIT=INDFIT-1
               NHFIT=NHFTLS
               PAR1=PAR1LS
               PAR2=PAR2LS
               PAR3=PAR3LS
               CSI2GM=CSI2LS
               CUROUT=CURLST
               XMAX=XMAXLS
               XMIN=XMINLS
               SIG11=SIG11L
               SIG=SIGLST
               KFLIP=3-KFLIP
      GOTO 15003
      ENDIF
      ENDIF
      IF(
     - INDFIT.EQ.INDMAX
     -)THEN
      GOTO 15003
      ENDIF
         NHFTLS=NHFIT
         PAR1LS=PAR1
         PAR2LS=PAR2
         PAR3LS=PAR3
         CSI2LS=CSI2GM
         CURLST=CUROUT
         XMAXLS=XMAX
         XMINLS=XMIN
         SIG11L=SIG11
         SIGLST=SIG
      ASSIGN 17012 TO IZZZ06
      GOTO 17011
17012 CONTINUE
      GOTO 15002
      ENDIF
15003 CONTINUE
      X0R=X0-XHF
      IF(
     - FV.LT.200. .AND. X0R.GT.XMIN .AND. X0R.LT.XMAX
     -)THEN
      IF(
     - LFTYP.EQ.2
     -)THEN
            DVCHI2=((PAR1*X0R+PAR2)*X0R+PAR3-Y0)**2*WGHT0
      ELSE
            AAH=-X0R**2*CSI2GM
            FDBPR=1./(1.+PAR1*X0R*PAR2)
            SAG=SAGCIR(FDBPR,PAR1,AAH,SAGPR,1.E-4)
            DVCHI2=(SAG+PAR2*X0R+PAR3-Y0)**2*WGHT0
      ENDIF
      IF(
     - DVCHI2 .LT.  9.*SIG
     -)THEN
            ISORT2(1,NHWIRV)=-100
      ASSIGN 17013 TO IZZZ04
      GOTO 17006
17013 CONTINUE
            IF(LNOCON.EQ.0) INDBIT=1024
      ENDIF
      ENDIF
      ASSIGN 17014 TO IZZZ02
      GOTO 17002
17014 CONTINUE
      RETURN
17006 CONTINUE
      LNOCON=0
      KFLIP=3-KFLIP
      KITER=0
15004 CONTINUE
      IF(
     - KITER .LT. 3-LFTYP
     -)THEN
         KITER=KITER+1
         X0R=X0-XHF
      IF(
     - ISORT2(1,NHWIRV).EQ.-100 .AND. X0R.GT.XMIN .AND. X0R.LT.XMAX
     -)THEN
      IF(
     - LFTYP.EQ.2
     -)THEN
               DYDP1=X0R**2
               DYDP2=X0R
               DYRES=Y0
      ELSE
               AAH=-X0R**2
               FDBPR=1./(1.+PAR1*X0R*PAR2)
               SAG=SAGCIR(FDBPR,PAR1,AAH*CSI2GM,SAGPR,1.E-4)
               CC1=FDBPR/(1.+SAG*PAR1*FDBPR)
               DYDP1=CC1*SAGPR
               DYDP2=X0R+PAR1*CC1*(AAH*PAR2-SAG*X0R)
               DYRES=Y0-SAG-PAR2*X0R-PAR3
      ENDIF
            S0 = WGHT0
            S1=DYDP2*WGHT0
            S2=DYDP1*WGHT0
            S3=S2*DYDP2
            S4=S2*DYDP1
            S8=S1*DYDP2
            S7=DYRES*WGHT0
            S6=S7*DYDP2
            S5=S7*DYDP1
      ELSE
            S0 = 0.
            S1 = 0.
            S2 = 0.
            S3 = 0.
            S4 = 0.
            S8 = 0.
            S7 = 0.
            S6 = 0.
            S5 = 0.
      ENDIF
         S00=S0
      DO 13000 IHWIR=1,NHWIR
            IH=ISORT2(2,IHWIR)
            NNH=ISORT2(3,IHWIR)
      DO 13002 JNH=1,NNH
               ISORT4(KFLIP,IH+JNH-1)=0
13002 CONTINUE
13003 CONTINUE
      IF(
     - ISORT3(IH).EQ.1 .OR. ISORT3(IH).EQ.-1.AND.NNH.GT.1
     -)THEN
               RESMIN=10000.
      DO 13004 JNH=1,NNH
                  JH=IH+JNH-1
      IF(
     - ISORT3(JH).EQ.1
     -)THEN
                     IPCO=ISORT2(1,IHWIR)+(JNH-1)*HLDCO
                     XA = WRK(IPCO+3)
                     XAR=XA-XHF
      IF(
     - XAR.GT.XMIN.AND.XAR.LT.XMAX
     -)THEN
                        YA = WRK(IPCO+4)
      IF(
     - LFTYP.EQ.2
     -)THEN
                           DYDP1A=XAR**2
                           DYDP2A=XAR
                           DYRESA=YA
                           DF0=ABS(YA-((PAR1*XAR+PAR2)*XAR+PAR3))
      ELSE
                           AAH=-XAR**2
                           FDBPR=1./(1.+PAR1*XAR*PAR2)
                           SAG=SAGCIR(FDBPR,PAR1,AAH*CSI2GM,SAGPR,1.E-4)
                           CC1=FDBPR/(1.+SAG*PAR1*FDBPR)
                           DYDP1A=CC1*SAGPR
                           DYDP2A=XAR+PAR1*CC1*(AAH*PAR2-SAG*XAR)
                           DYRESA=YA-SAG-PAR2*XAR-PAR3
                           DF0=ABS(DYRESA)
      ENDIF
      ELSE
                        DF0=15000.
      ENDIF
      IF(
     - DF0.LT.RESMIN
     -)THEN
                        RESMIN=DF0
                        DYDP1=DYDP1A
                        DYDP2=DYDP2A
                        DYRES=DYRESA
                        JHUSE=JH
      ENDIF
      ENDIF
13004 CONTINUE
13005 CONTINUE
      IF(
     - RESMIN.LT.RESCUT
     -)THEN
                  S0=S0+1.
                  S1=S1+DYDP2
                  S2=S2+DYDP1
                  S3=S3+DYDP1*DYDP2
                  S4=S4+DYDP1**2
                  S8=S8+DYDP2**2
                  S7=S7+DYRES
                  S6=S6+DYRES*DYDP2
                  S5=S5+DYRES*DYDP1
                  ISORT4(KFLIP,JHUSE)=1
      ELSE
                  ISORT3(IH)=-2
      ENDIF
      ENDIF
13000 CONTINUE
13001 CONTINUE
         NHF1=S0-S00+.1
      IF(
     - NHF1.LT.6 .OR. NHF1.LT.NHPOT/2
     -)THEN
            LNOCON=1
      GOTO 15005
      ENDIF
         NHFIT=NHF1
         DEG   = S0 - S00 - 3.
         DET = (S8*S0-S1*S1)*S4 + (S2*S1-S3*S0)*S3 + (S3*S1-S2*S8)*S2
         SIG11 = (S8*S0 - S1*S1)/DET
         F1 = 1. / S4
         XX12 = S3*F1
         XX13 = S2*F1
         YY1  = S5*F1
         XX22 = S8 - S3*XX12
         XX23 = S1 - S3*XX13
         YY2  = S6 - S3*YY1
         XX32 = S1 - S2*XX12
         XX33 = S0 - S2*XX13
         YY3  = S7 - S2*YY1
      IF(
     - XX22.GT.XX32
     -)THEN
            XX23 = XX23 / XX22
            YY2  = YY2  / XX22
            PARR3 = (YY3 - XX32*YY2) / (XX33 - XX32*XX23)
            PARR2 = YY2 - XX23*PARR3
      ELSE
            XX33 = XX33 / XX32
            YY3  = YY3  / XX32
            PARR3 = (YY2 - XX22*YY3) / (XX23 - XX22*XX33)
            PARR2 = YY3 - XX33*PARR3
      ENDIF
         PARR1 = YY1 - XX12*PARR2 - XX13*PARR3
      IF(
     - LFTYP.EQ.2
     -)THEN
            PAR1=PARR1
            PAR2=PARR2
            PAR3=PARR3
            IF(ABS(PAR1).LT.1.E-10) PAR1 = SIGN(1.E-10,PAR1)
            CSI2GM=PAR2**2+1.
            CUROUT =-PAR1 * 2./ (SQRT(CSI2GM)*CSI2GM)
      ELSE
            PAR1=PAR1+PARR1
            PAR2=PAR2+PARR2
            PAR3=PAR3+PARR3
            IF(ABS(PAR1).LT.1.E-10) PAR1 = SIGN(1.E-10,PAR1)
            CSI2GM=PAR2**2+1.
            CUROUT=PAR1/SQRT(CSI2GM)
            XMIN=PAR2/PAR1-CKAPP/ABS(CUROUT)
            XMAX=PAR2/PAR1+CKAPP/ABS(CUROUT)
      ENDIF
      GOTO 15004
      ENDIF
15005 CONTINUE
      IF(
     - LNOCON.EQ.0
     -)THEN
         CHISQ = 0.
         NHF1=0
      DO 13006 IHWIR=1,NHWIR
            IRESHT(IHWIR)=-1
            IH=ISORT2(2,IHWIR)
            NNH=ISORT2(3,IHWIR)
      IF(
     - ISORT3(IH).GE.0 .OR. ISORT3(IH).EQ.-1.AND.NNH.GT.1
     -)THEN
               RESMIN=10000.
      DO 13008 JNH=1,NNH
                  JH=IH+JNH-1
      IF(
     - ISORT3(JH).GE.0
     -)THEN
                     IFLG=ISORT3(JH)
                     IPCO=ISORT2(1,IHWIR)+(JNH-1)*HLDCO
                     XR= WRK(IPCO+3)-XHF
      IF(
     - XR.GT.XMIN.AND.XR.LT.XMAX
     -)THEN
                        Y = WRK(IPCO+4)
      IF(
     - LFTYP.EQ.2
     -)THEN
                           DF0=ABS(Y-(PAR1*XR+PAR2)*XR-PAR3)
      ELSE
                           AAH=-XR**2*CSI2GM
                           FDBPR=1./(1.+PAR1*XR*PAR2)
                           SAG=SAGCIR(FDBPR,PAR1,AAH,SAGPR,1.E-4)
                           DF0=ABS(Y-SAG-PAR2*XR-PAR3)
      ENDIF
                        IF(DF0.LT.RESMIN) RESMIN=DF0
      ENDIF
      ENDIF
13008 CONTINUE
13009 CONTINUE
      IF(
     - RESMIN.LT.5000.
     -)THEN
                  IRESHT(IHWIR)=RESMIN*1.E6
      IF(
     - IFLG.EQ.1
     -)THEN
                     CHISQ=CHISQ+RESMIN**2
                     NHF1=NHF1+1
      ENDIF
      ENDIF
      ENDIF
13006 CONTINUE
13007 CONTINUE
         IF(NHF1.LT.NHFIT-3) LNOCON=1
         SIG    =      CHISQ  / DEG
      ENDIF
      GOTO IZZZ04
17011 CONTINUE
         CALL SHELL9(IRESHT,ISORT1,NHWIR)
         KOMIT=0
      DO 13010 J1=1,NHWIR
            IHWIR=ISORT1(NHWIR+1-J1)
            IPCO=ISORT2(1,IHWIR)
      IF(
     - IPCO.NE.-100 .AND. IPCO.NE.-200
     -)THEN
               NNH=ISORT2(3,IHWIR)
               IH=ISORT2(2,IHWIR)
               LFLG=0
      DO 13012 JNH=1,NNH
                  IHA=IH+JNH-1
                  IQA=ISORT3(IHA)
      IF(
     - IQA.GT.-1
     -)THEN
      IF(
     - LFLG.EQ.0
     -)THEN
                        LFLG=1
                        KOMIT=KOMIT+1
      ENDIF
      IF(
     - KOMIT.LE.INDFIT
     -)THEN
                        ISORT3(IHA)=0
      ELSE
                        ISORT3(IHA)=1
      ENDIF
      ENDIF
13012 CONTINUE
13013 CONTINUE
      ENDIF
13010 CONTINUE
13011 CONTINUE
      GOTO IZZZ06
17009 CONTINUE
      IF(
     - INDFIT.LE.6
     -)THEN
            INDCK=INDFIT-1
      ELSE
            INDCK=5
      ENDIF
         ICHCK=NCHECK(INDCK)
15006 CONTINUE
      IF(
     - SIGLST.LT.RCHECK(ICHCK,1,INDCK)
     -)THEN
            ICHCK=ICHCK-1
      GOTO 15006
      ENDIF
15007 CONTINUE
         IF(ICHCK.LT.1) ICHCK=1
      IF(
     - SIG/SIGLST.GT.RCHECK(ICHCK,2,INDCK)
     -)THEN
            LSTOP=1
      ELSE
            LSTOP=0
      ENDIF
      GOTO IZZZ05
17002 CONTINUE
         XST=XHF+XMIN
         IF(XST.LT.XREGA) XST=XREGA
         XSTR=XST-XHF
         XEN=XHF+XMAX
         IF(XEN.GT.XREGB) XEN=XREGB
         XENR=XEN-XHF
      IF(
     - LFTYP.EQ.2
     -)THEN
            YST  = (PAR1 *XSTR+ PAR2 )*XSTR+ PAR3
            YEN  = (PAR1 *XENR+ PAR2 )*XENR+ PAR3
            TGST = PAR1*XSTR*2.+ PAR2
            TGEN = PAR1*XENR*2.+ PAR2
      ELSE
            AAH=-XSTR**2
            FDBPR=1./(1.+PAR1*XSTR*PAR2)
            SAG=SAGCIR(FDBPR,PAR1,AAH*CSI2GM,SAGPR,1.E-4)
            YST=SAG+PAR2*XSTR+PAR3
         TGST=PAR2-PAR1*FDBPR/(1.+SAG*PAR1*FDBPR)*(SAG*PAR2+XSTR*CSI2GM)
            AAH=-XENR**2
            FDBPR=1./(1.+PAR1*XENR*PAR2)
            SAG=SAGCIR(FDBPR,PAR1,AAH*CSI2GM,SAGPR,1.E-4)
            YEN=SAG+PAR2*XENR+PAR3
         TGEN=PAR2-PAR1*FDBPR/(1.+SAG*PAR1*FDBPR)*(SAG*PAR2+XENR*CSI2GM)
      ENDIF
         DXST = 1./SQRT(TGST**2+1.)
         DYST = DXST * TGST
         DXEN = 1./SQRT(TGEN**2+1.)
         DYEN = DXEN * TGEN
         HPTR0 = HPFREE
         CALL MVC(IWRK(HPTR0),0,IDATA(IPTR+1),0,4*LDTR)
         IP    = HPTR0 - 1
         IWRK(IP+2) = LAND(IWRK(IP+2),MASK3)
         IWRK(IP+2) = LOR(IWRK(IP+2),INDBIT)
         IWRK(IP+ 3) = IDAY
         WRK (IP+ 5) = XST *CSROT - YST *SNROT + XT
         WRK (IP+ 6) = XST *SNROT + YST *CSROT + YT
         WRK (IP+ 7) = SQRT(WRK(IP+ 5)**2 + WRK(IP+ 6)**2)*TGTH+ZVERT
         DXSTJ       =  DXST*CSROT - DYST*SNROT
         DYSTJ       =  DXST*SNROT + DYST*CSROT
         WRK (IP+ 8) =  DXSTJ*CSTH
         WRK (IP+ 9) =  DYSTJ*CSTH
         WRK (IP+12) = XEN *CSROT - YEN *SNROT + XT
         WRK (IP+13) = XEN *SNROT + YEN *CSROT + YT
         WRK (IP+14) = SQRT(WRK(IP+12)**2 + WRK(IP+13)**2)*TGTH+ZVERT
         WRK (IP+15) = (DXEN*CSROT - DYEN*SNROT)*CSTH
         WRK (IP+16) = (DXEN*SNROT + DYEN*CSROT)*CSTH
         IWRK(IP+24) = NHFIT
         WRK (IP+25) = CUROUT
         WRK (IP+27) = CUROUT
         WRK (IP+28) = CUROUT
         WRK (IP+23) = SQRT(SIG)
         WRK (IP+26) = SQRT(SIG*SIG11/CSI2GM)
         IF(LFTYP.EQ.2) WRK(IP+26)=WRK(IP+26)*2./CSI2GM
         IWRK(IP+18) = 1
         PAR1=CUROUT*SQRT(CSI2GM)
         SIGNC=SIGN(1.,CUROUT)
         ACURV=ABS(CUROUT)
         A=((XHF-XOR)*PAR2-PAR3+YOR)/SQRT(CSI2GM)
         B=(XHF-XOR+(PAR3-YOR)*PAR2)/SQRT(CSI2GM)
         FDBPR=1./ACURV+SIGNC*A
      IF(
     - FDBPR.LT.100.
     -)THEN
            DIMP=-1./ACURV+SQRT(FDBPR**2+B**2)
      ELSE
            DIMP=SIGNC*A+SAGCIR(1./(1.+CUROUT*A),ACURV,B**2,SP,1.E-4)
      ENDIF
         FDBPR=1.+DIMP*ACURV
      IF(
     - FDBPR.LT.ACURV*1.E-3
     -)THEN
            WRK(IP+19)=ACURV
            WRK(IP+20)=DIMP
            WRK(IP+21)=0.
      ELSE
            FDBPR=SIGNC/FDBPR
            SGPFI=(PAR2*CSROT+SNROT)/SQRT(CSI2GM)
            CGPFI=(CSROT-PAR2*SNROT)/SQRT(CSI2GM)
            COSALP=(CUROUT*(XHF*CSROT-PAR3*SNROT+XT)+SGPFI)*FDBPR
            SINALP=(CUROUT*(XHF*SNROT+PAR3*CSROT+YT)-CGPFI)*FDBPR
            WRK(IP+19)=ACURV
            WRK(IP+20)=DIMP
            WRK(IP+21)=ATAN2(SINALP,COSALP)
      IF(
     - LDTR.GE.55.AND.NHFIT.GE.10.AND.LNOCON.EQ.0
     -)THEN
               LCOVAR=1
      ELSE
               LCOVAR=0
      ENDIF
      IF(
     - LAND(MODXYV,32) .NE. 0
     -)THEN
               LJHTLU=1
      ELSE
               LJHTLU=0
      ENDIF
      ASSIGN 17016 TO IZZZ07
      GOTO 17015
17016 CONTINUE
            IWRK(IPRES+10)=1
      ENDIF
         CALL MVC(IDATA(IPTR+1),0,IWRK(HPTR0),0,4*LDTR)
      GOTO IZZZ02
17000 CONTINUE
         S0 = 0.
         S1 = 0.
         S2 = 0.
         S3 = 0.
         S4 = 0.
      DO 13014 IHWIR=1,NHWIR
            IH=ISORT2(2,IHWIR)
            NNH=ISORT2(3,IHWIR)
      DO 13016 JNH=1,NNH
               ISORT4(KFLIP,IH+JNH-1)=0
13016 CONTINUE
13017 CONTINUE
      IF(
     - ISORT3(IH).EQ.1 .OR. ISORT3(IH).EQ.-1.AND.NNH.GT.1
     -)THEN
               RESMIN=10000.
      DO 13018 JNH=1,NNH
                  JH=IH+JNH-1
      IF(
     - ISORT3(JH).EQ.1
     -)THEN
                     IPCO=ISORT2(1,IHWIR)+(JNH-1)*HLDCO
                     XA = WRK(IPCO+3)
                     XAR=XA-XHF
      IF(
     - XAR.GT.XMIN .AND. XAR.LT.XMAX
     -)THEN
                        YA = WRK(IPCO+4)
      IF(
     - LFTYP.EQ.2
     -)THEN
                           DYRESA=YA-(PAR1*XAR+PAR2)*XAR-PAR3
      ELSE
                           AAH=-XAR**2*CSI2GM
                           FDBPR=1./(1.+PAR1*XAR*PAR2)
                           SAG=SAGCIR(FDBPR,PAR1,AAH,SAGPR,1.E-4)
                           DYRESA=YA-SAG-PAR2*XAR-PAR3
      ENDIF
                        DF0=ABS(DYRESA)
      ELSE
                        DF0=15000.
      ENDIF
      IF(
     - DF0.LT.RESMIN
     -)THEN
                        RESMIN=DF0
                        XR=XAR
                        DYRES=DYRESA
                        JHUSE=JH
      ENDIF
      ENDIF
13018 CONTINUE
13019 CONTINUE
      IF(
     - RESMIN.LT.DISCUT
     -)THEN
                  S0=S0+1.
                  S1=S1+XR
                  S2=S2+XR**2
                  S3=S3+DYRES
                  S4=S4+DYRES*XR
                  ISORT4(KFLIP,JHUSE)=1
      ENDIF
      ENDIF
13014 CONTINUE
13015 CONTINUE
         NHFIT=S0+.1
      IF(
     - NHFIT.GE.4
     -)THEN
            S12=S1/S2
            S42=S4/S2
            DA=(S3-S1*S42)/(S0-S1*S12)
            DB=S42-S12*DA
            PAR3=PAR3+DA
            PAR2=PAR2+DB
            CSI2GM=1.+PAR2**2
      IF(
     - LFTYP.EQ.2
     -)THEN
               CUROUT =-PAR1 * 2./ (SQRT(CSI2GM)*CSI2GM)
      ELSE
               CUROUT=PAR1/SQRT(CSI2GM)
               XMIN=PAR2/PAR1-CKAPP/ABS(CUROUT)
               XMAX=PAR2/PAR1+CKAPP/ABS(CUROUT)
      ENDIF
      ENDIF
      GOTO IZZZ01
17004 CONTINUE
      ISTV1=0
      DISCUT=400.
15008 CONTINUE
      IF(
     - ISTV1.LT.10
     -)THEN
         ISTV1=ISTV1+1
         S0 = 0.
         S1 = 0.
         S2 = 0.
         S3 = 0.
         S4 = 0.
         S8 = 0.
         S7 = 0.
         S6 = 0.
         S5 = 0.
         KFLIP=3-KFLIP
      DO 13020 IHWIR=1,NHWIR
            IH=ISORT2(2,IHWIR)
            NNH=ISORT2(3,IHWIR)
      DO 13022 JNH=1,NNH
               ISORT4(KFLIP,IH+JNH-1)=0
13022 CONTINUE
13023 CONTINUE
      IF(
     - ISORT3(IH).EQ.1 .OR. ISORT3(IH).EQ.-1.AND.NNH.GT.1
     -)THEN
               RESMIN=10000.
      DO 13024 JNH=1,NNH
                  JH=IH+JNH-1
      IF(
     - ISORT3(JH).EQ.1
     -)THEN
                     IPCO=ISORT2(1,IHWIR)+(JNH-1)*HLDCO
                     XA = WRK(IPCO+3)
                     XAR= WRK(IPCO+3)-XHF
      IF(
     - XAR.GT.XMIN.AND.XAR.LT.XMAX
     -)THEN
                        YA = WRK(IPCO+4)
                        AAH=-XAR**2
                        FDBPR=1./(1.+PAR1*XAR*PAR2)
                        SAG=SAGCIR(FDBPR,PAR1,AAH*CSI2GM,SAGPR,1.E-4)
                        DYRESA=YA-SAG-PAR2*XAR-PAR3
                        DF0=ABS(DYRESA)
      ELSE
                        DF0=15000.
      ENDIF
      IF(
     - DF0.LT.RESMIN
     -)THEN
                        RESMIN=DF0
                        X=XAR
                        DYRES=DYRESA
                        JHUSE=JH
      ENDIF
      ENDIF
13024 CONTINUE
13025 CONTINUE
      IF(
     - RESMIN.LT.DISCUT
     -)THEN
                  S0=S0+1.
                  S1=S1+X
                  S2=S2+X**2
                  S3=S3+X**3
                  S4=S4+X**4
                  S8=S8+X**2
                  S7=S7+DYRES
                  S6=S6+DYRES*X
                  S5=S5+DYRES*X**2
                  ISORT4(KFLIP,JHUSE)=1
      ENDIF
      ENDIF
13020 CONTINUE
13021 CONTINUE
         NHFIT=S0+.1
         IF(NHFIT.LT.5) RETURN
         F1 = 1. / S4
         XX12 = S3*F1
         XX13 = S2*F1
         YY1  = S5*F1
         XX22 = S8 - S3*XX12
         XX23 = S1 - S3*XX13
         YY2  = S6 - S3*YY1
         XX32 = S1 - S2*XX12
         XX33 = S0 - S2*XX13
         YY3  = S7 - S2*YY1
      IF(
     - XX22.GT.XX32
     -)THEN
            XX23 = XX23 / XX22
            YY2  = YY2  / XX22
            PARR3 = (YY3 - XX32*YY2) / (XX33 - XX32*XX23)
            PARR2 = YY2 - XX23*PARR3
      ELSE
            XX33 = XX33 / XX32
            YY3  = YY3  / XX32
            PARR3 = (YY2 - XX22*YY3) / (XX23 - XX22*XX33)
            PARR2 = YY3 - XX33*PARR3
      ENDIF
         PARR1 = YY1 - XX12*PARR2 - XX13*PARR3
         XAR=-.7*RRMI
         IF(XAR.LT..8*XMIN) XAR=.8*XMIN
         XBR= .7*RRPL
         IF(XBR.GT..8*XMAX) XBR=.8*XMAX
      IF(
     - -XAR.LT.XBR
     -)THEN
            XBR=-XAR
      ELSE
            XAR=-XBR
      ENDIF
         AAH=-XAR**2*CSI2GM
         FDBPR=1./(1.+PAR1*XAR*PAR2)
         SAG=SAGCIR(FDBPR,PAR1,AAH,SAGPR,1.E-4)
         YA=SAG+PAR2*XAR+PAR3 + (PARR1*XAR+PARR2)*XAR+PARR3
         AAH=-XBR**2*CSI2GM
         FDBPR=1./(1.+PAR1*XBR*PAR2)
         SAG=SAGCIR(FDBPR,PAR1,AAH,SAGPR,1.E-4)
         YB=SAG+PAR2*XBR+PAR3 + (PARR1*XBR+PARR2)*XBR+PARR3
         YC=PAR3+PARR3
         P2=(YB-YA)/(2.*XBR)
         C2=1.+P2**2
         SAG=YC-.5*(YA+YB)
      IF(
     - SAG**2.GT.(CKAPP*XBR)**2*C2
     -)THEN
            SAG=SIGN(CKAPP*XBR*SQRT(C2),SAG)
            YC=.5*(YA+YB)+SAG
      ENDIF
         PAR3=YC
         P1=2.*SAG/(C2*XBR**2-SAG**2)
         CUROUT=P1/SQRT(C2*(1.+(P1*XBR)**2))
         IF(ABS(CUROUT).LT.1.E-8) CUROUT= SIGN(1.E-8,CUROUT)
         PAR2=P2/(1.+SAG*P1)
         CSI2GM=1.+PAR2**2
         PAR1=CUROUT*SQRT(CSI2GM)
         XMIN=PAR2/PAR1-CKAPP/ABS(CUROUT)
         XMAX=PAR2/PAR1+CKAPP/ABS(CUROUT)
         DISCUT=.5*DISCUT
         IF(DISCUT.LT.10.) DISCUT=10.
      GOTO 15008
      ENDIF
15009 CONTINUE
      GOTO IZZZ03
17015 CONTINUE
         SAMFI=SINALP*CSROT-COSALP*SNROT
         CAMFI=COSALP*CSROT+SINALP*SNROT
         IP00=2*IDATA(IQJETC)+100
      DO 13026 IHWIR=1,NHWIR
            IH=ISORT2(2,IHWIR)
            NNH=ISORT2(3,IHWIR)
      DO 13028 JNH=1,NNH
               JH=IH+JNH-1
      IF(
     - ISORT4(KFLIP,JH).NE.1
     -)THEN
                  IPCO=ISORT2(1,IHWIR)+(JNH-1)*HLDCO
                  X=WRK(IPCO+3)
                  Y=WRK(IPCO+4)
                  A=(X-XOR)*CAMFI+(Y-YOR)*SAMFI
                  B=(X-XOR)*SAMFI-(Y-YOR)*CAMFI
                  FDBPR=1./ACURV+DIMP-A
      IF(
     - FDBPR.LT.100.
     -)THEN
                     SAG=SQRT(FDBPR**2+B**2)
                     RESA=SAG-1./ACURV
      ELSE
                     FDBPR=1./(1.+ACURV*(DIMP-A))
                     SAG=SAGCIR(FDBPR,ACURV,B**2,SP,1.E-4)
                     RESA=DIMP-A+SAG
      ENDIF
                  WRK(IPCO+13)=RESA
      IF(
     - LJHTLU.EQ.1
     -)THEN
                     IP    =IWRK(IPCO+1)
                     LBSIDE=IWRK(IPCO+2)
                     IPHL=IPJHTL+2+(IP-IP00)/4
                     LB=IDATA(IPHL)
                     IDST=ABS(RESA)*5.
                     IF(IDST.GT.31) IDST=31
                     IDST=ISHFTL(IDST,11)
                     IDST=LOR(IDST,1024)
                     IF(LBSIDE.EQ.1) IDST=LOR(IDST,256)
                     ITR1=LAND(ISHFTR(LB,17),127)
      IF(
     - ITR1.EQ.ITRK
     -)THEN
                        IDATA(IPHL)=LOR(LAND(LB,MASK1),ISHFTL(IDST,16))
      ELSE
                        IDATA(IPHL)=LOR(LAND(LB,MASK2),      IDST    )
      ENDIF
      ENDIF
      ENDIF
13028 CONTINUE
13029 CONTINUE
13026 CONTINUE
13027 CONTINUE
         CHISQ=0.
         NHF1=0
         S0D= 0.D0
         S1D= 0.D0
         S2D= 0.D0
         S3D= 0.D0
         S4D= 0.D0
         S8D= 0.D0
      DO 13030 IHWIR=1,NHWIR
            IH=ISORT2(2,IHWIR)
            NNH=ISORT2(3,IHWIR)
      DO 13032 JNH=1,NNH
               JH=IH+JNH-1
      IF(
     - ISORT4(KFLIP,JH).EQ.1
     -)THEN
                  IPCO=ISORT2(1,IHWIR)+(JNH-1)*HLDCO
                  X=WRK(IPCO+3)
                  Y=WRK(IPCO+4)
                  A=(X-XOR)*CAMFI+(Y-YOR)*SAMFI
                  B=(X-XOR)*SAMFI-(Y-YOR)*CAMFI
                  FDBPR=1./ACURV+DIMP-A
      IF(
     - FDBPR.LT.100.
     -)THEN
                     SAG=SQRT(FDBPR**2+B**2)
                     RESA=SAG-1./ACURV
                     DRDP2=FDBPR/SAG
                     DRDP1=B*(DRDP2+A/SAG)
                     DRDP3=1./ACURV**2*(1.-DRDP2)
      ELSE
                     FDBPR=1./(1.+ACURV*(DIMP-A))
                     SAG=SAGCIR(FDBPR,ACURV,B**2,SP,1.E-4)
                     DRDP2=1./(1.+SAG*FDBPR*ACURV)
                     DRDP1=DRDP2*B*(1.+A*FDBPR*ACURV)
                     DRDP3=DRDP2*FDBPR*SP
                     RESA=DIMP-A+SAG
      ENDIF
                  NHF1=NHF1+1
                  CHISQ=CHISQ+RESA**2
                  S0D=S0D+DRDP3**2
                  S1D=S1D+DRDP3*DRDP2
                  S2D=S2D+DRDP1*DRDP3
                  S3D=S3D+DRDP1*DRDP2
                  S4D=S4D+DRDP1**2
                  S8D=S8D+DRDP2**2
                  IWRK(IPCO+10)=0
                  WRK(IPCO+13)=RESA
      IF(
     - LJHTLU.EQ.1
     -)THEN
                     IP    =IWRK(IPCO+1)
                     LBSIDE=IWRK(IPCO+2)
                     IPHL=IPJHTL+2+(IP-IP00)/4
                     LB=IDATA(IPHL)
                     IDST=ABS(RESA)*5.
                     IF(IDST.GT.31) IDST=31
                     IDST=ISHFTL(IDST,11)
                     IF(LBSIDE.EQ.1) IDST=LOR(IDST,256)
                     ITR1=LAND(ISHFTR(LB,17),127)
      IF(
     - ITR1.EQ.ITRK
     -)THEN
                        IDATA(IPHL)=LOR(LAND(LB,MASK1),ISHFTL(IDST,16))
      ELSE
                        IDATA(IPHL)=LOR(LAND(LB,MASK2),      IDST    )
      ENDIF
      ENDIF
      ENDIF
13032 CONTINUE
13033 CONTINUE
13030 CONTINUE
13031 CONTINUE
      IF(
     - LCOVAR.EQ.1
     -)THEN
      IF(
     - NHF1.LT.10.OR.NHF1.NE.NHFIT
     -)THEN
               PRINT 6781,NRUN,NEV,ITRK,NHFIT,NHF1
6781           FORMAT(' RUN,EV,TRK,NHFIT,NHF1',I7,I6,I3,2I5)
      ELSE
               DETD=(S8D*S0D-S1D*S1D)*S4D+
     +        (S2D*S1D-S3D*S0D)*S3D+(S3D*S1D-S2D*S8D)*S2D
               FACT=CHISQ/(NHFIT-3)/DETD
               IWRK(IP+2) =LOR(IWRK(IP+2),2048)
               WRK(IP+49)=CHISQ/.115**2
               WRK(IP+50)=(S8*S0-S1**2)*FACT
               WRK(IP+51)=(S1*S2-S0*S3)*FACT
               WRK(IP+52)=(S4*S0-S2**2)*FACT
               WRK(IP+53)=(S3*S1-S8*S2)*FACT
               WRK(IP+54)=(S2*S3-S1*S4)*FACT
               WRK(IP+55)=(S8*S4-S3**2)*FACT
      ENDIF
      ENDIF
      GOTO IZZZ07
      END
