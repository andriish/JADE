      SUBROUTINE ZSRFT1(IPTR,LDTR,IPCO0,IPRES,LHIT,IQUAL,IOPT)
      IMPLICIT INTEGER*2 (H)
      LOGICAL LIDHIT
#include "cdata.for"
#include "calibr.for"
      COMMON/CWORK/NDIWRK,WRK(200)
      DIMENSION IWRK(200),HWRK(400)
      EQUIVALENCE (WRK(1),IWRK(1),HWRK(1))
      COMMON / CBIN   / TIME(6),ZOF,ZRS,ZL,ZSC,EPSI(3),DOUB(3),IRN(3),
     +                BINDL8(6),RJITT, DLRSLN(3), DLZSLN(3)
      INTEGER DATE(5), IDAY /0/
      REAL RESCUT/600./
      DIMENSION ISORT1(71),ISORT2(3,71),IRESHT(71),ISORT3(91)
     +,ISORT4(2,91),KSORT3(91),KSZSRT(91,2)
      DATA IQHEAD/0/,MASK4/ZFFFFCFFF/
      DATA LBINIT /0/
      IF(
     - LBINIT .EQ. 0
     -)THEN
         LBINIT = 1
         IQHEAD = IBLN('HEAD')
         CALL DAY2(DATE)
         IDAY = DATE(1)*1000 + DATE(2)
         WRITE(6,137)
 137     FORMAT(/,' *** ZSRFT1 ***  (J.SPITZER) VERSION OF 20/5/88',
     +   ' SMOOTH CUTOFF PROCEDURE IMPLEMENTED')
      ENDIF
      IPHEAD = IDATA(IQHEAD)*2
      NRUN = HDATA(IPHEAD+10)
      NEV  = HDATA(IPHEAD+11)
      ITRK = IDATA(IPTR+1)
      IF(
     - LDATYP(DUMMY) .EQ. 1
     -)THEN
         SRESO=24.
         STPFAC=.85
      ELSE
         SRESO=32.
         STPFAC=.92
      ENDIF
      IF( NRUN.LT.100 ) SRESO = SQRT(ZRS**2+1.)
      SIGMIN=(SRESO/1.6)**2
      SIGFAC=(.14/30.)**2
      PAR1=ADATA(IPTR+30)
      PAR2=ADATA(IPTR+7)
      NHALL=0
      NHWIR=0
      NHPOT=0
      NHPOTT=0
      IPCO=IPCO0
      IPCO9=IPRES-LHIT
16000 CONTINUE
         NHWIR=NHWIR+1
      IF(
     - NHWIR.GT.70
     -)THEN
            IQUAL=-1
            RETURN
      ENDIF
         ISORT1(NHWIR)=NHWIR
         ISORT2(1,NHWIR)=IPCO
         ISORT2(3,NHWIR)=0
         IW0=IWRK(IPCO)
         LFL=0
         LFL1=0
         LFL2=0
15000 CONTINUE
      IF(
     - IPCO.LE.IPCO9
     -)THEN
            IW9=IWRK(IPCO)
      IF(
     - IW9.EQ.IW0
     -)THEN
               NHALL=NHALL+1
      IF(
     - NHALL.GT.90
     -)THEN
                  IQUAL=-2
                  RETURN
      ENDIF
               IF(ISORT2(3,NHWIR).EQ.0) ISORT2(2,NHWIR)=NHALL
               ISORT2(3,NHWIR)=ISORT2(3,NHWIR)+1
               KSORT3(NHALL)= NHALL
               LZGOOD=HWRK(2*IPCO+1)
      IF(
     - LZGOOD.NE.0
     -)THEN
                  ISORT3(NHALL)=-1
                  KSZSRT(NHALL,1)= 100000
      ELSE
                  ISORT3(NHALL)= 1
      IF(
     - LFL2.EQ.0
     -)THEN
                     KSZSRT(NHALL,1)= WRK(IPCO+6)
                     KSZSRT(NHALL,2)= WRK(IPCO+5)
                     LFL2=1
      ELSE
                     KSZSRT(NHALL,1)= 100000
      ENDIF
                  LFL=1
      IF(
     - ABS(WRK(IPCO+5)-PAR1*WRK(IPCO+6)-PAR2).LT.RESCUT
     -)THEN
      IF(
     - LFL1.EQ.0
     -)THEN
                        LFL1=1
                        NHPOTT=NHPOTT+1
      ENDIF
      ENDIF
      ENDIF
               IPCO=IPCO+LHIT
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
     - IPCO.GT.IPCO9
     -))GOTO 16000
16001 CONTINUE
      IF(
     - NHPOT.LT.2
     -)THEN
         IQUAL=-3
         RETURN
      ENDIF
      KFLIP=2
      NHFIT=NHPOTT
      LFOUND=-1
      IF(
     - NHPOTT.LT.6.OR.NHPOTT.LT.NHPOT*.75
     -)THEN
      ASSIGN 17001 TO IZZZ01
      GOTO 17000
17001 CONTINUE
      ENDIF
      INDMAX=NHFIT/4+1
      IF(INDMAX.GT.13) INDMAX=13
      INDFIT=0
15002 CONTINUE
      IF(
     - INDFIT.LT.INDMAX
     -)THEN
         INDFIT=INDFIT+1
      ASSIGN 17003 TO IZZZ02
      GOTO 17002
17003 CONTINUE
      IF(
     - LNOCON.EQ.1
     -)THEN
            IQUAL=-4
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
      ASSIGN 17005 TO IZZZ03
      GOTO 17004
17005 CONTINUE
      IF(
     - LSTOP.EQ.1
     -)THEN
               INDFIT=INDFIT-1
               KFLIP=3-KFLIP
               NHFIT=NHFTLS
               PAR1=PAR1LS
               PAR2=PAR2LS
               SIG=SIGLST
               S0=S0LS
               S1=S1LS
               S2=S2LS
               S3=S3LS
               S4=S4LS
               SHID = SHIDLS
               S0ID = S0IDLS
      GOTO 15003
      ENDIF
      ENDIF
      IF(
     - INDFIT.EQ.INDMAX.OR.NHFIT.EQ.2
     -)THEN
      GOTO 15003
      ENDIF
         NHFTLS=NHFIT
         PAR1LS=PAR1
         PAR2LS=PAR2
         SIGLST=SIG
         S0LS=S0
         S1LS=S1
         S2LS=S2
         S3LS=S3
         S4LS=S4
         SHIDLS = SHID
         S0IDLS = S0ID
      ASSIGN 17007 TO IZZZ04
      GOTO 17006
17007 CONTINUE
      GOTO 15002
      ENDIF
15003 CONTINUE
      ASSIGN 17009 TO IZZZ05
      GOTO 17008
17009 CONTINUE
      IQUAL=1
      RETURN
17002 CONTINUE
      LNOCON=0
      KFLIP=3-KFLIP
      NHF1=0
      SHID = 0.
      S0ID = 0.
      S0 = 0.
      S1 = 0.
      S2 = 0.
      S3 = 0.
      S4 = 0.
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
            LIDHIT = .TRUE.
      DO 13004 JNH=1,NNH
               JH=IH+JNH-1
      IF(
     - ISORT3(JH).EQ.1
     -)THEN
                  IPCO=ISORT2(1,IHWIR)+(JNH-1)*LHIT
                  SA = WRK(IPCO+6)
                  ZA = WRK(IPCO+5)
                  WA = WRK(IPCO+7)
                  DZRESA=ZA-PAR1*SA-PAR2
                  DF0=ABS(DZRESA)
      IF(
     - DF0.LT.RESMIN
     -)THEN
                     RESMIN=DF0
                     S=SA
                     W=WA
                     DZRES=DZRESA
                     JHUSE=JH
                     LIDHIT = HWRK(IPCO*2-1).LT.97
      ENDIF
      ENDIF
13004 CONTINUE
13005 CONTINUE
      IF(
     - RESMIN.LT.RESCUT
     -)THEN
               NHF1=NHF1+1
               S0=S0+W
               S1=S1+S*W
               S2=S2+S**2*W
               S3=S3+DZRES*W
               S4=S4+DZRES*S*W
      IF(
     - LIDHIT
     -)THEN
                  SHID = SHID + 1.
                  S0ID = S0ID + W
      ENDIF
               ISORT4(KFLIP,JHUSE)=1
      ELSE
               ISORT3(IH)=-2
      ENDIF
      ENDIF
13000 CONTINUE
13001 CONTINUE
      IF(
     - NHF1.LT.2 .OR. S2.LT.1.
     -)THEN
         LNOCON=1
      ELSE
         NHFIT=NHF1
         F1 = 1. / S2
         XX12 = S1*F1
         YY1  = S4*F1
         PARR2=(S3-S1*YY1)/(S0-S1*XX12)
         PAR1=YY1-PARR2*XX12+PAR1
         PAR2=PAR2+PARR2
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
                     IPCO=ISORT2(1,IHWIR)+(JNH-1)*LHIT
                     SA = WRK(IPCO+6)
                     ZA = WRK(IPCO+5)
                     WA = WRK(IPCO+7)
                     DF0=ABS(ZA-PAR1*SA-PAR2)
      IF(
     - DF0.LT.RESMIN
     -)THEN
                        RESMIN=DF0
                        W=WA
      ENDIF
      ENDIF
13008 CONTINUE
13009 CONTINUE
      IF(
     - RESMIN.LT.8000.
     -)THEN
                  RESMIN=RESMIN*SQRT(W)
                  IRESHT(IHWIR)=RESMIN*1.E4
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
      IF(
     - NHF1.LT.2
     -)THEN
            LNOCON=1
      ELSE
      IF(
     - NHF1.EQ.2
     -)THEN
               SIG=1.E-5
      ELSE
               SIG=CHISQ/(NHF1-2)
      ENDIF
      ENDIF
         NHFIT=NHF1
      ENDIF
      GOTO IZZZ02
17006 CONTINUE
         CALL SHELL9(IRESHT,ISORT1,NHWIR)
         KOMIT=0
      DO 13010 J1=1,NHWIR
            IHWIR=ISORT1(NHWIR+1-J1)
            IPCO=ISORT2(1,IHWIR)
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
13010 CONTINUE
13011 CONTINUE
      GOTO IZZZ04
17004 CONTINUE
         SIGCHK = SIGLST*SIGFAC
      IF(
     - SIGCHK .LT. 0.002
     -)THEN
            RCHKPR = 0.0
      ELSE
            RCHKPR = 1.139 + 0.08862*ALOG(SIGCHK)
      ENDIF
      IF(
     - SIG/SIGLST*STPFAC.GT.RCHKPR
     -)THEN
            LSTOP=1
      ELSE
            LSTOP=0
      ENDIF
      GOTO IZZZ03
17008 CONTINUE
      CTGTH=PAR1
      CSTH = 1./SQRT(CTGTH**2 + 1.)
      SNTH  = CSTH * CTGTH
      IF( S0ID.GT.0. ) SIG = SIG*SHID/S0ID
         IFREE=NDIWRK-100
         CALL MVCL(IWRK(IFREE),0,IDATA(IPTR+1),0,4*LDTR)
         IP    = IFREE - 1
         IWRK(IP+ 2) = LAND(IWRK(IP+2),MASK4)
         IWRK(IP+ 2) = LOR(IWRK(IP+2),4096)
         WRK (IP+ 7) = PAR2
         A=SQRT(WRK(IP+8)**2+WRK(IP+9)**2)
         WRK (IP+ 8) = WRK (IP+ 8)/A*CSTH
         WRK (IP+ 9) = WRK (IP+ 9)/A*CSTH
         WRK (IP+10) = SNTH
         CURVXY=WRK(IP+25)
         IF(ABS(CURVXY).LT.1.E-9) CURVXY = SIGN(1.E-9,CURVXY)
         UU=SQRT((WRK(IP+12)-WRK(IP+5))**2+(WRK(IP+13)-WRK(IP+6))**2)
         ARGARG = .5*CURVXY*UU
         IF(ABS(ARGARG).GT.1.) ARGARG = SIGN(1.,ARGARG)
         IF(ABS(CURVXY*UU).GT.1.E-5) UU=2.*ARSIN(ARGARG)/CURVXY
         WRK (IP+14) = PAR2+UU*PAR1
         A=SQRT(WRK(IP+15)**2+WRK(IP+16)**2)
         WRK (IP+15) = WRK (IP+15)/A*CSTH
         WRK (IP+16) = WRK (IP+16)/A*CSTH
         WRK (IP+17) = SNTH
         IWRK(IP+33) = NHFIT
         WRK (IP+32) = SQRT(SIG)
         IWRK(IP+29) = 2
         WRK (IP+30) = PAR1
         DDR0=DISTXY(ADATA(IPTR+5),ADATA(IPTR+6),ADATA(IPTR+8),
     +   ADATA(IPTR+9),1./CURVXY,0.,0.,XP,YP,FI)
         UU=SQRT((XP-WRK(IP+5))**2+(YP-WRK(IP+6))**2)
         ARGARG = .5*CURVXY*UU
         IF(ABS(ARGARG).GT.1.) ARGARG = SIGN(1.,ARGARG)
         IF(ABS(CURVXY*UU).GT.1.E-5) UU=2.*ARSIN(ARGARG)/CURVXY
         WRK (IP+31) = PAR2-PAR1*UU
      IF(
     - LDTR.GE.59 .AND. NHFIT.GE.4 .AND. LNOCON.EQ.0
     -)THEN
            DET=S0*S2-S1**2
            FACT=SIG/DET
            WRK(IP+56)=SIG*(NHFIT-2)/20.**2
            WRK(IP+57)=(S2+2.*UU*S1+UU**2*S0)*FACT
            WRK(IP+58)=-(UU*S0+S1)*FACT
            WRK(IP+59)=S0*FACT
      ENDIF
         LFL=0
      DO 13014 IHWIR=1,NHWIR
            IH=ISORT2(2,IHWIR)
            NNH=ISORT2(3,IHWIR)
      DO 13016 JNH=1,NNH
               IPCO=ISORT2(1,IHWIR)+(JNH-1)*LHIT
               HWRK(2*IPCO+3)=ISORT4(KFLIP,IH+JNH-1)
               IF(HWRK(2*IPCO-1).GT.100.AND.HWRK(2*IPCO+3).EQ.1) LFL=1
13016 CONTINUE
13017 CONTINUE
13014 CONTINUE
13015 CONTINUE
         IF(LFL.EQ.1) IWRK(IP+ 2) = LOR(IWRK(IP+2),16384)
         CALL MVCL(IDATA(IPTR+1),0,IWRK(IFREE),0,4*LDTR)
      IF(
     - IOPT.GT.8
     -)THEN
            CALL CCRE(NPZSPD,'ZSPD',ITRK,5*NHALL+1,IERR)
      IF(
     - IERR.EQ.0
     -)THEN
               CALL BSAW(1,'ZSPD')
               NPZSP1=NPZSPD+1
               IDATA(NPZSP1)=5
      DO 13018 IHWIR=1,NHWIR
                  IH=ISORT2(2,IHWIR)
                  NNH=ISORT2(3,IHWIR)
      DO 13020 JNH=1,NNH
                     IPCO=ISORT2(1,IHWIR)+(JNH-1)*LHIT
                     IFL=ISORT4(KFLIP,IH+JNH-1)
      IF(
     - IFL.EQ.1
     -)THEN
                        IFL=0
      ELSE
                        IFL=16
      ENDIF
                     IDATA(NPZSP1+1)=HWRK(2*IPCO+4)
                     ADATA(NPZSP1+2)=WRK(IPCO+6)
                     ADATA(NPZSP1+3)=WRK(IPCO+5)
                     IDATA(NPZSP1+4)=IFL
                     ADATA(NPZSP1+5)=WRK(IPCO+7)
                     NPZSP1=NPZSP1+5
13020 CONTINUE
13021 CONTINUE
13018 CONTINUE
13019 CONTINUE
      ENDIF
      ENDIF
      GOTO IZZZ05
17000 CONTINUE
         CALL SHELL9(KSZSRT(1,1),KSORT3,NHALL)
         NH9=NHALL
15004 CONTINUE
      IF(
     - KSZSRT(KSORT3(NH9),1).GT.99999
     -)THEN
            NH9=NH9-1
      GOTO 15004
      ENDIF
15005 CONTINUE
         NH99=NH9
         NH1=1
         LFOUND=0
         NHMAX=0
         CI2MAX=1.E10
15006 CONTINUE
      IF(
     - NH9.GT.NH1
     -)THEN
      DO 13022 KK=1,3
      IF(
     - KK.EQ.1
     -)THEN
                  JH1=NH1
                  JH9=NH9
      ELSE
      IF(
     - KK.EQ.2
     -)THEN
                     JH9=JH9-1
      ELSE
                     JH9=JH9+1
                     JH1=JH1+1
      ENDIF
      ENDIF
      IF(
     - JH9.GT.JH1
     -)THEN
                  AS1=KSZSRT(KSORT3(JH1),1)
                  AS9=KSZSRT(KSORT3(JH9),1)
      IF(
     - AS9.LT.AS1+5.
     -)THEN
      GOTO 15007
      ENDIF
                  AZ1=KSZSRT(KSORT3(JH1),2)
                  AZ9=KSZSRT(KSORT3(JH9),2)
                  PAR1=(AZ9-AZ1)/(AS9-AS1)
                  PAR2=AZ1-PAR1*AS1
                  NHFIT=0
                  CI2=0.
      DO 13024 I=1,NH99
                     DZ=ABS(KSZSRT(KSORT3(I),2)-PAR1*KSZSRT(KSORT3(I),1)
     +               -PAR2)
      IF(
     - DZ.LT.RESCUT
     -)THEN
                        NHFIT=NHFIT+1
                        CI2=CI2+DZ**2
      ENDIF
13024 CONTINUE
13025 CONTINUE
      IF(
     - NHFIT.GT.NHPOT*.75
     -)THEN
                     LFOUND=1
      GOTO 15007
      ENDIF
      IF(
     - NHFIT.GT.NHMAX .OR.NHFIT.EQ.NHMAX.AND.CI2.LT.CI2MAX
     -)THEN
                     NHMAX=NHFIT
                     P1MAX=PAR1
                     P2MAX=PAR2
                     CI2MAX=CI2
      ENDIF
      ENDIF
13022 CONTINUE
13023 CONTINUE
            NH1=NH1+1
            NH9=NH9-1
      GOTO 15006
      ENDIF
15007 CONTINUE
      IF(
     - LFOUND.EQ.0 .AND. NHMAX.GT.0
     -)THEN
            NHFIT=NHMAX
            PAR1=P1MAX
            PAR2=P2MAX
      ENDIF
      GOTO IZZZ01
      END
