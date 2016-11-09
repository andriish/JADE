      SUBROUTINE ZSRFTV(MODE,IOPT)
      IMPLICIT INTEGER*2 (H)
      COMMON/CZSSTE/ NSETZS,NTSTZS(5),KTSTZS(100,2,5),XYSTZS(2,5),
     +SQCHZS(3,5)
      COMMON/CCMZCT/ DIMPCT, ZCUTV, ZCUTVV, IZVCST(5)
#include "cdata.for"
#include "calibr.for"
      COMMON/CWORK/NDIWRK,WRK(20000)
      DIMENSION IWRK(20000),HWRK(40000)
      EQUIVALENCE (WRK(1),IWRK(1),HWRK(1))
      INTEGER ITRREQ(100)
      REAL CORTRC(2,100)
      DATA LBINIT /0/, IQPATR/0/, IQJHTL/0/, IQHEAD/0/, IQJETC/0/
      DATA NPRLIM/50/,KPRLIM/0/
      KOPT=IOPT
      IF(KOPT.GT.8) KOPT=KOPT-8
      IF(
     - KOPT.NE.1 .AND. KOPT.NE.2 .AND. KOPT.NE.4
     -)THEN
      IF(
     - KPRLIM.LT.NPRLIM
     -)THEN
            KPRLIM=KPRLIM+1
            WRITE(6,100) IOPT
100         FORMAT(' **** ZSRFTV CALLED WITH INVALID OPTION :',I8)
      ENDIF
         RETURN
      ENDIF
      IF(
     - KOPT.EQ.4
     -)THEN
      IF(
     - NTSTZS(1).EQ.1
     -)THEN
            KTC=KTSTZS(1,1,1)
      IF(
     - KTC.LT.1 .OR. KTC.GT.100
     -)THEN
               LINCON=1
      ELSE
               LINCON=0
               NTRREQ=1
               ITRREQ(1)=KTC
               KTSTZS(1,2,1)=0
      ENDIF
      ELSE
      IF(
     - NSETZS.GE.1.AND.NSETZS.LE.5
     -)THEN
               NTRREQ=0
               LINCON=0
      DO 13000 ISET=1,NSETZS
                  SQCHZS(1,ISET)=0.
                  SQCHZS(2,ISET)=0.
                  SQCHZS(3,ISET)=0.
                  NTSETI=NTSTZS(ISET)
      IF(
     - NTSETI.LT.2 .OR. NTSETI.GT.100
     -)THEN
                     LINCON=1
      GOTO 13001
      ENDIF
      DO 13002 JT=1,NTSETI
                     KTC=KTSTZS(JT,1,ISET)
      IF(
     - KTC.LT.1 .OR. KTC.GT.100
     -)THEN
                        LINCON=1
      GOTO 13003
      ENDIF
                     KTSTZS(JT,2,ISET)=0
                     LPRES=0
      IF(
     - NTRREQ.GT.0
     -)THEN
      DO 13004 J=1,NTRREQ
      IF(
     - ITRREQ(J).EQ.KTC
     -)THEN
                              LPRES=1
      GOTO 13005
      ENDIF
13004 CONTINUE
13005 CONTINUE
      ENDIF
      IF(
     - LPRES.EQ.0
     -)THEN
      IF(
     - NTRREQ.EQ.100
     -)THEN
                           LINCON=1
      GOTO 13003
      ENDIF
                        NTRREQ=NTRREQ+1
                        ITRREQ(NTRREQ)=KTC
      ENDIF
13002 CONTINUE
13003 CONTINUE
      IF(
     - LINCON.NE.0
     -)THEN
      GOTO 13001
      ENDIF
13000 CONTINUE
13001 CONTINUE
      ELSE
               LINCON=1
      ENDIF
      ENDIF
      IF(
     - LINCON.NE.0
     -)THEN
      IF(
     - KPRLIM.LT.NPRLIM
     -)THEN
               KPRLIM=KPRLIM+1
               WRITE(6,200)
200            FORMAT(' **** ZSRFTV: INVALID REQUEST IN /CZSSTE/')
      ENDIF
            RETURN
      ENDIF
      ELSE
         SQCHZS(1,1)=0.
         SQCHZS(2,1)=0.
         SQCHZS(3,1)=0.
      DO 13006 J=1,100
            KTSTZS(J,2,1)=0
13006 CONTINUE
13007 CONTINUE
      ENDIF
      IF(
     - LBINIT .LE.0
     -)THEN
         LBINIT = 1
         IQPATR = IBLN('PATR')
         IQJHTL = IBLN('JHTL')
         IQHEAD = IBLN('HEAD')
         IQJETC = IBLN('JETC')
      ENDIF
      IF(IDATA(IQPATR).LE.0) RETURN
      IF(
     - MODE.EQ.1
     -)THEN
         IPPAT0 = IDATA(IQPATR)
         NBNK1  = IDATA(IPPAT0-2) - 1
         NWRD   = IDATA(IPPAT0)
         NBYTE  = NWRD*4
         CALL CCRE(IPPATR,'PATR',NBNK1,NWRD,IERR)
      IF(
     - IERR.NE.0
     -)THEN
            PRINT 2900, IERR
 2900       FORMAT(' CREATION OF NEW PATR-BANK RESULTED IN ERROR',I3)
            RETURN
      ENDIF
         CALL MVCL(IDATA(IPPATR+1),0,IDATA(IPPAT0+1),0,NBYTE)
      ENDIF
      IPPATR = IDATA(IQPATR)
      IPTR   = IDATA(IPPATR+1) + IPPATR
      LDTR   = IDATA(IPPATR+3)
      NTR    = IDATA(IPPATR+2)
      IF(NTR.LT.1) RETURN
      IF(
     - NTR.GT.100
     -)THEN
      IF(
     - KPRLIM.LT.NPRLIM
     -)THEN
            KPRLIM=KPRLIM+1
            WRITE(6,300) NTR
300         FORMAT(' **** ZSRFTV : NUMBER OF TRACKS IN PATR BANK :',
     +      I4,'. FIRST 100 WILL BE CONSIDERED.')
      ENDIF
         NTR=100
      ENDIF
      CALL JRECAL(IERR)
      IF(
     - IERR.NE.0
     -)THEN
         PRINT 6784,IERR
 6784    FORMAT(' *** ERROR IN JRECAL',I3)
         RETURN
      ENDIF
      IPJETC = IDATA(IQJETC)
      IPJHTL = IDATA(IQJHTL)
      MODEZ  = 1
      CALL ZSFIT(IPJETC,IDATA(IPJETC-1),IPJHTL,IPPATR,MODEZ,5) ! PMF 03/12/99: 6th argument added
      NDIWRK=20000
      NTRKS=0
      IDTR2=1
      LDTR2=11
      LHIT=8
      LTRREC=0
      IHIT1=NTR*LDTR2+1
      DO 13008 ITR=1,NTR
      IF(
     - IHIT1 .GT. NDIWRK-200-(70*LHIT+LTRREC)
     -)THEN
      GOTO 13009
      ENDIF
      IF(
     - KOPT.NE.4
     -)THEN
            LFIT=1
      ELSE
            LFIT=0
      DO 13010 J=1,NTRREQ
      IF(
     - ITRREQ(J).EQ.ITR
     -)THEN
                  LFIT=1
      GOTO 13011
      ENDIF
13010 CONTINUE
13011 CONTINUE
      ENDIF
      IF(
     - LFIT.EQ.1
     -)THEN
            INDFET = 4
            CALL JFETCH(IPTR,IPJHTL,WRK(IHIT1),LHIT,IPRES,INDFET,XD,YD)
            NHIT=(IPRES-1)/LHIT
      IF(
     - NHIT.GT.1
     -)THEN
               IPRES=IHIT1+IPRES-1
               CALL ZSRFT1(IPTR,LDTR,IHIT1,IPRES,LHIT,IQUAL,IOPT)
      IF(
     - KOPT.EQ.4
     -)THEN
      IF(
     - NTSTZS(1).EQ.1
     -)THEN
                     KTSTZS(1,2,1)=IQUAL
                     RETURN
      ELSE
      DO 13012 ISET=1,NSETZS
                        NTSETI=NTSTZS(ISET)
      DO 13014 JT=1,NTSETI
                     IF(KTSTZS(JT,1,ISET).EQ.ITR)KTSTZS(JT,2,ISET)=IQUAL
13014 CONTINUE
13015 CONTINUE
13012 CONTINUE
13013 CONTINUE
      ENDIF
      ELSE
                  KTSTZS(ITR,2,1)=IQUAL
      ENDIF
      IF(
     - IQUAL.GT.0 .AND. KOPT.NE.1
     -)THEN
                  NTRKS=NTRKS+1
                  IWRK(IDTR2  )=ITR
                  IWRK(IDTR2+1)=IPTR
                  IF(KOPT.EQ.2) IQUAL=2
                  IWRK(IDTR2+2)=IQUAL
                  IWRK(IDTR2+3)=IHIT1
                  IWRK(IDTR2+4)=IPRES
                  IHIT1=IPRES+LTRREC
                  IDTR2=IDTR2+LDTR2
      ENDIF
      ENDIF
      ENDIF
         IPTR=IPTR+LDTR
13008 CONTINUE
13009 CONTINUE
      IF(KOPT.EQ.1.OR.NTRKS.LT.2) RETURN
      IF(
     - KOPT.EQ.2
     -)THEN
         IRUN=HDATA( 2*IDATA(IQHEAD) + 10)
      IF(
     -  IRUN.GE.100
     -)THEN
            IPV    = ICALIB(10)
            XCOMM  = ACALIB(IPV+ 1)
            YCOMM  = ACALIB(IPV+ 3)
      ELSE
            XCOMM  = 0.
            YCOMM  = 0.
      ENDIF
         IVNEED=IZVCST(1)
      ASSIGN 17001 TO IZZZ01
      GOTO 17000
17001 CONTINUE
      ENDIF
      IF(
     - KOPT.EQ.4
     -)THEN
      DO 13016 ISET=1,NSETZS
            NTSETI=NTSTZS(ISET)
            NTRFIT=0
            IDTR2=1
      DO 13018 JTR=1,NTRKS
               ITR=IWRK(IDTR2)
               IWRK(IDTR2+2)=1
      DO 13020 JT=1,NTSETI
      IF(
     - KTSTZS(JT,1,ISET).EQ.ITR
     -)THEN
                     NTRFIT=NTRFIT+1
                     IWRK(IDTR2+2)=2
      GOTO 13021
      ENDIF
13020 CONTINUE
13021 CONTINUE
               IDTR2=IDTR2+LDTR2
13018 CONTINUE
13019 CONTINUE
      IF(
     - NTRFIT.GE.2
     -)THEN
               XCOMM=XYSTZS(1,ISET)
               YCOMM=XYSTZS(2,ISET)
               IVNEED=IZVCST(ISET)
      ASSIGN 17002 TO IZZZ01
      GOTO 17000
17002 CONTINUE
      ENDIF
13016 CONTINUE
13017 CONTINUE
      ENDIF
      RETURN
17000 CONTINUE
         ZCUT=ZCUTV
         IF(KOPT.EQ.4) ZCUT=ZCUTVV
         IDTR2=1
         NTRFIT=0
         ZCOMM=0.
      DO 13022 JTR=1,NTRKS
      IF(
     - IWRK(IDTR2+2).EQ.2
     -)THEN
               IPTR=IWRK(IDTR2+1)
               CURVXY=ADATA(IPTR+25)
               IF(ABS(CURVXY).LT.1.E-9) CURVXY = SIGN(1.E-9,CURVXY)
               DDR0=DISTXY(ADATA(IPTR+5),ADATA(IPTR+6),ADATA(IPTR+8),
     +         ADATA(IPTR+9),1./CURVXY,XCOMM,YCOMM,CORTRC(1,JTR),
     +         CORTRC(2,JTR),FI)
      IF(
     - ABS(DDR0).LT.DIMPCT
     -)THEN
                  WRK(IDTR2+5)=COS(FI)
                  WRK(IDTR2+6)=SIN(FI)
                  DDR0=DISTXY(ADATA(IPTR+5),ADATA(IPTR+6),ADATA(IPTR+8),
     +            ADATA(IPTR+9),1./CURVXY,0.,0.,XP,YP,FI)
                  UU=SQRT((CORTRC(1,JTR)-XP)**2+(CORTRC(2,JTR)-YP)**2)
                  ARGARG = .5*CURVXY*UU
                  IF( ABS(ARGARG).GT.1.) ARGARG = SIGN( 1., ARGARG )
                  IF(ABS(CURVXY*UU).GT.1.E-5)
     +            UU=2.*ARSIN(ARGARG)/CURVXY
                  ZCOMM1=ADATA(IPTR+31)+ADATA(IPTR+30)*UU
      IF(
     - ABS(ZCOMM1) .LT. ZCUT
     -)THEN
                     WRK(IDTR2+7)=ZCOMM1
                     WRK(IDTR2+10)=CURVXY
                     ZCOMM=ZCOMM+ZCOMM1
                     NTRFIT=NTRFIT+1
      ELSE
                     IWRK(IDTR2+2)=1
      ENDIF
      ELSE
                  IWRK(IDTR2+2)=1
      ENDIF
      ENDIF
            IDTR2=IDTR2+LDTR2
13022 CONTINUE
13023 CONTINUE
      IF(
     - NTRFIT.GE.2
     -)THEN
            ZCOMM=ZCOMM/NTRFIT
            NHTOT=0
      IF(
     - IVNEED.EQ.1
     -)THEN
               S0=(20./10.)**2
               IF( LDATYP(DUMMY).EQ.2 ) S0=S0*4.
               S3=-ZCOMM*S0
               S7=ZCOMM**2*S0
      ELSE
               S0=0.
               S3=0.
               S7=0.
      ENDIF
            S5=0.
            S6=0.
            IDTR2=1
      DO 13024 JTR=1,NTRKS
      IF(
     - IWRK(IDTR2+2).EQ.2
     -)THEN
                     IPTR=IWRK(IDTR2+1)
                     CURVXY=WRK(IDTR2+10)
                     CTGTH=ADATA(IPTR+30)
                     S1=0.
                     S2=0.
                     S4=0.
                     IPCO =IWRK(IDTR2+3)
                     IPCO9=IWRK(IDTR2+4)-LHIT
      DO 13026 IP=IPCO,IPCO9,LHIT
      IF(
     - HWRK(2*IP+3).EQ.1
     -)THEN
                           UX=WRK(IP+3)-CORTRC(1,JTR)
                           UY=WRK(IP+4)-CORTRC(2,JTR)
                           UU=SQRT(UX**2+UY**2)
                           ARGARG = .5*CURVXY*UU
                           IF( ABS(ARGARG).GT.1.) ARGARG=SIGN(1.,ARGARG)
                           IF(ABS(CURVXY*UU).GT.1.E-5)
     +                     UU=2.*ARSIN(ARGARG)/CURVXY
                           IF(UX*WRK(IDTR2+5)+UY*WRK(IDTR2+6).LT.0.)
     +                     UU=-UU
                           DZ=WRK(IP+5)-ZCOMM-CTGTH*UU
                           W=WRK(IP+7)
                           NHTOT=NHTOT+1
                           S0=S0+W
                           S3=S3+DZ*W
                           S1=S1+UU*W
                           S2=S2+UU**2*W
                           S4=S4+DZ*UU*W
                           S7=S7+DZ**2*W
      ENDIF
13026 CONTINUE
13027 CONTINUE
                     WRK(IDTR2+7)=S4
                     WRK(IDTR2+8)=S1
                     WRK(IDTR2+9)=S2
                     S5=S5+S1*S4/S2
                     S6=S6+S1*S1/S2
      ENDIF
               IDTR2=IDTR2+LDTR2
13024 CONTINUE
13025 CONTINUE
               DZCOMM=(S3-S5)/(S0-S6)
               ZCOMMR=ZCOMM+DZCOMM
               ISETOP=1
               IF(KOPT.EQ.4) ISETOP=ISET
               SQCHZS(2,ISETOP)=NHTOT
               SQCHZS(3,ISETOP)=ZCOMMR
               SQCHZS(1,ISETOP)=S7+DZCOMM*(DZCOMM*S0-2.*S3)
               IDTR2=1
      DO 13028 JTR=1,NTRKS
      IF(
     - IWRK(IDTR2+2).EQ.2
     -)THEN
                     IPTR=IWRK(IDTR2+1)
      ASSIGN 17004 TO IZZZ02
      GOTO 17003
17004 CONTINUE
                     ITR=IWRK(IDTR2)
      IF(
     - KOPT.EQ.4
     -)THEN
      DO 13030 JT=1,NTSETI
                         IF(KTSTZS(JT,1,ISET).EQ.ITR)KTSTZS(JT,2,ISET)=2
CAV                         00059400
13030 CONTINUE
13031 CONTINUE
      ELSE
                        KTSTZS(ITR,2,1)=2
      ENDIF
      ENDIF
                  IDTR2=IDTR2+LDTR2
13028 CONTINUE
13029 CONTINUE
               IF(SQCHZS(1,ISETOP).LT.1.E-5) SQCHZS(1,ISETOP)=1.E-5
               SQCHZS(1,ISETOP)=SQRT(SQCHZS(1,ISETOP)/(NHTOT-NTRFIT-1))
      ENDIF
      GOTO IZZZ01
17003 CONTINUE
      DCTGTH=(WRK(IDTR2+7)-DZCOMM*WRK(IDTR2+8))/WRK(IDTR2+9)
      SQCHZS(1,ISETOP)=SQCHZS(1,ISETOP)+DCTGTH*(DCTGTH*WRK(IDTR2+9)+
     +2.*DZCOMM*WRK(IDTR2+8)-2.*WRK(IDTR2+7))
      CTGTH=ADATA(IPTR+30)+DCTGTH
      CSTH = 1./SQRT(CTGTH**2 + 1.)
      SNTH  = CSTH * CTGTH
      CURVXY=WRK(IDTR2+10)
         IFREE=NDIWRK-100
         CALL MVCL(IWRK(IFREE),0,IDATA(IPTR+1),0,4*LDTR)
         IP    = IFREE - 1
         IWRK(IP+ 2) = LOR(IWRK(IP+2),8192)
         UX=WRK(IP+5)-CORTRC(1,JTR)
         UY=WRK(IP+6)-CORTRC(2,JTR)
         UU=SQRT(UX**2+UY**2)
         ARGARG = .5*CURVXY*UU
         IF( ABS(ARGARG).GT.1.) ARGARG = SIGN( 1., ARGARG )
         IF(ABS(CURVXY*UU).GT.1.E-5) UU=2.*ARSIN(ARGARG)/CURVXY
         IF(UX*WRK(IDTR2+5)+UY*WRK(IDTR2+6).LT.0.) UU=-UU
         WRK (IP+ 7) = ZCOMMR+CTGTH*UU
         A=SQRT(WRK(IP+8)**2+WRK(IP+9)**2)
         WRK (IP+ 8) = WRK (IP+ 8)/A*CSTH
         WRK (IP+ 9) = WRK (IP+ 9)/A*CSTH
         WRK (IP+10) = SNTH
         UX=WRK(IP+12)-CORTRC(1,JTR)
         UY=WRK(IP+13)-CORTRC(2,JTR)
         UU=SQRT(UX**2+UY**2)
         ARGARG = .5*CURVXY*UU
         IF( ABS(ARGARG).GT.1.) ARGARG = SIGN( 1., ARGARG )
         IF(ABS(CURVXY*UU).GT.1.E-5) UU=2.*ARSIN(ARGARG)/CURVXY
         IF(UX*WRK(IDTR2+5)+UY*WRK(IDTR2+6).LT.0.) UU=-UU
         WRK (IP+14) = ZCOMMR+CTGTH*UU
         A=SQRT(WRK(IP+15)**2+WRK(IP+16)**2)
         WRK (IP+15) = WRK (IP+15)/A*CSTH
         WRK (IP+16) = WRK (IP+16)/A*CSTH
         WRK (IP+17) = SNTH
         IWRK(IP+29) = 2
         WRK (IP+30) = CTGTH
         DDR0=DISTXY(ADATA(IPTR+5),ADATA(IPTR+6),ADATA(IPTR+8),
     +   ADATA(IPTR+9),1./CURVXY,0.,0.,XP,YP,FI)
         UX=XP-CORTRC(1,JTR)
         UY=YP-CORTRC(2,JTR)
         UU=SQRT(UX**2+UY**2)
         ARGARG = .5*CURVXY*UU
         IF( ABS(ARGARG).GT.1.) ARGARG = SIGN( 1., ARGARG )
         IF(ABS(CURVXY*UU).GT.1.E-5) UU=2.*ARSIN(ARGARG)/CURVXY
         IF(UX*WRK(IDTR2+5)+UY*WRK(IDTR2+6).LT.0.) UU=-UU
         WRK (IP+31) = ZCOMMR+CTGTH*UU
         CALL MVCL(IDATA(IPTR+1),0,IWRK(IFREE),0,4*LDTR)
      GOTO IZZZ02
      END
