C   22/03/97            MEMBER NAME  JEOSUM3  (JADEGS)      FORTRAN
C   28/04/87 706161853  MEMBER NAME  AMPS2Z0  (JADEGS)      SHELTRAN
      SUBROUTINE AMPS2Z( IP, NPJETC, Z, W, IFLAG )
C-----------------------------------------------------------
C-     COPIED FROM F22ELS.ZSLIB.S    7.5.87   --------------
C-----------------------------------------------------------
C  VERSION OF 21/04/87                             E ELSEN
C  BLOCK DATA MOVED TO JADEBD  LAST MOD 16/06/87   E ELSEN
C  Convert the amplitudes stored in HW(IP+1) and HW(IP+2)
C  into Z and calculate the weight W associated with this
C  measurement.
C  NPJETC = IW(IBLN('JETC'))
C  Flag IFLAG is 0 if the hit passes some quality criteria
C  Weighting will only work if ZSFIT has been called. In that
C  case CZSCAL has been initialised ( ZALPDI=1400 )
C  Note the different effective wire length that is used
C  for the two calibrations.
C-----------------------------------------------------------
      IMPLICIT INTEGER*2 (H)
C                                           from zsfit
C                                           zal name conflict
C     COMMON /CZSCAL/ IPVERS,ZAL,RESFAC,SECH(5)
      COMMON /CZSCAL/ IPVERS, ZALPDI, RESFAC, SECH(5)
#include "czsprm.for"
C
C
#include "cjdrch.for"
C
      COMMON / BCS / HW(1)
C
      REAL EXTRMZ / 1250. /, ZALDEF / 1400. /
C                                           ROUGHLY 3.6MM
      INTEGER IDTMAX / 600 /
C
      Z = 0.
      W = 1.
      IFLAG = 16
C
      AL = HW(IP+2)/8.
      AR = HW(IP+1)/8.
C                                          check whether CZSCAL has been
C                                          initialised
      IF  AL.GT.0. .AND. AR.GT.0.
      THEN
        IF  ZALPDI .EQ. ZALDEF
        THEN
          Z = ZALPDI*(AL-AR)/(AL+AR)
          IF NZSPRD .LE. 2
          THEN
            W = ( AZSSAV(NZSPRD) /
     *              ( AZSRS0(NZSPRD)+
     *                AZSRSA(NZSPRD)*SQRT(AL**2+AR**2)/(AL+AR)**2
     *              )
     *          )**2
          ELSE
            W = (AZSSAV(NZSPRD)/
     *             (AZSRS0(NZSPRD)+AZSRSA(NZSPRD)/(AL+AR)))**2
          CIF
        ELSE
          Z = .5*ZAL*(AL-AR)/(AL+AR)
        CIF
        IF  ABS( Z ) .LT. EXTRMZ
        THEN
          IF NZSPRD.GT.2
          THEN
C                                           ANY CLOSE HIT?
            ISEC = 0
            NP = IP - 4
            IDT = HW(NP+3)-HW(IP+3)
            IF( NP.GT.NPJETC*2+100 .AND. HW(NP)/8 .EQ. HW(IP)/8.AND.
     *          IABS(IDT).LT.IDTMAX ) ISEC = ISEC + 1
            NP = IP + 4
            IDT = HW(NP+3)-HW(IP+3)
            IF( NP.LT.NPJETC*2+100+HW(NPJETC*2+99) .AND.
     *          HW(NP)/8 .EQ. HW(IP)/8.AND.
     *          IABS(IDT).LT.IDTMAX ) ISEC = ISEC + 1
            IF( ISEC .EQ. 0 ) IFLAG = 0
          ELSE
             IFLAG = 0
          CIF
        CIF
      CIF
      RETURN
      END
C   28/04/87 706201702  MEMBER NAME  AMPS2S1  (JADEGS)      SHELTRAN
      SUBROUTINE AMPS2Z( IP, NPJETC, Z, W, IFLAG )
C-----------------------------------------------------------
C-     COPIED FROM F22ELS.ZSLIB.S   20.6.87   --------------
C-----------------------------------------------------------
C  VERSION OF 21/04/87         LAST MOD 05/06/87   E ELSEN
C  Convert the amplitudes stored in HW(IP+1) and HW(IP+2)
C  into Z and calculate the weight W associated with this
C  measurement.
C  NPJETC = IW(IBLN('JETC'))
C  Flag IFLAG is 0 if the hit passes some quality criteria
C  Weighting will only work if ZSFIT has been called. In that
C  case CZSCAL has been initialised ( ZALPDI=1400 )
C  Note the different effective wire length that is used
C  for the two calibrations.
C-----------------------------------------------------------
      IMPLICIT INTEGER*2 (H)
C                                           from zsfit
C                                           zal name conflict
C     COMMON /CZSCAL/ IPVERS,ZAL,RESFAC,SECH(5)
      COMMON /CZSCAL/ IPVERS, ZALPDI, RESFAC, SECH(5)
#include "czsprm.for"
C
C
#include "cjdrch.for"
C
      COMMON / BCS / HW(1)
C
      REAL EXTRMZ / 1250. /, ZALDEF / 1400. /
C                                           ROUGHLY 3.6MM
      INTEGER IDTMAX / 600 /
C
      Z = 0.
      W = 1.
      IFLAG = 16
C
      AL = HW(IP+2)/8.
      AR = HW(IP+1)/8.
C                                          check whether CZSCAL has been
C                                          initialised
      IF  AL.GT.0. .AND. AR.GT.0.
      THEN
        IF  ZALPDI .EQ. ZALDEF
        THEN
          Z = ZALPDI*(AL-AR)/(AL+AR)
          IF NZSPRD .LE. 2
          THEN
            W = ( AZSSAV(NZSPRD) /
     *              ( AZSRS0(NZSPRD)+
     *                AZSRSA(NZSPRD)*SQRT(AL**2+AR**2)/(AL+AR)**2
     *              )
     *          )**2
          ELSE
            W = (AZSSAV(NZSPRD)/
     *             (AZSRS0(NZSPRD)+AZSRSA(NZSPRD)/(AL+AR)))**2
          CIF
        ELSE
          Z = .5*ZAL*(AL-AR)/(AL+AR)
        CIF
        IF  ABS( Z ) .LT. EXTRMZ
        THEN
          IF NZSPRD.GT.2
          THEN
C                                           ANY CLOSE HIT?
            ISEC = 0
            NP = IP - 4
            IDT = HW(NP+3)-HW(IP+3)
            IF( NP.GT.NPJETC*2+100 .AND. HW(NP)/8 .EQ. HW(IP)/8.AND.
     *          IABS(IDT).LT.IDTMAX ) ISEC = ISEC + 1
            NP = IP + 4
            IDT = HW(NP+3)-HW(IP+3)
            IF( NP.LT.NPJETC*2+100+HW(NPJETC*2+99) .AND.
     *          HW(NP)/8 .EQ. HW(IP)/8.AND.
     *          IABS(IDT).LT.IDTMAX ) ISEC = ISEC + 1
            IF( ISEC .EQ. 0 ) IFLAG = 0
          ELSE
             IFLAG = 0
          CIF
        CIF
C                                           OVERFLOW HITS
        IF(IFLAG.EQ.0 .AND.
     *     ( HW(IP+1).EQ. 32760 .OR. HW(IP+2).EQ. 32760 ) ) IFLAG = 32
      CIF
      RETURN
      END
C   08/09/82 306161127  MEMBER NAME  FRFITV   (JADEGS)      SHELTRAN
C   16/08/82 209081051  MEMBER NAME  ORFRFITV (FITSR)       SHELTRAN
C   18/02/81 208111542  MEMBER NAME  REFITV   (JETCALSR)    SHELTRAN
      SUBROUTINE FRFITV(IPTR,IPJHTL,ERRFAC)
C
C        REFIT TRACK ITRK IN 'PATR'-BANK USING ORIGIN
C                   ONLY INTERMEDIATE VALUES STORED
C                   FOR POSITION + DIRECTION AT 1. AND LAST HIT
C                   THIS ROUTINE IS ONLY USED WITH SUBSEQUENT ZRFIT
C                   USE REFITV IF ONLY R-PHI-FIT WANTED
C        P. STEFFEN                    22/08/80
C
      IMPLICIT INTEGER*2 (H)
      LOGICAL DEADCL
C
#include "cdata.for"
C
#include "cgeo1.for"
C
#include "calibr.for"
C
#include "cworkpr.for"
#include "cworkeq.for"
C
      EQUIVALENCE
     ,          (HPCO0 ,HPWRK(17)),(HPCO9 ,HPWRK(18)),(HLDCO ,HPWRK(19))
     ,         ,(ICELL ,IDWRK( 1)),(MHIT  ,IDWRK( 2)),(IRING ,IDWRK( 3))
     ,         ,(IERRCD,IDWRK( 4)),(NTRKEL,IDWRK( 5))
C
#include "cpatlm.for"
C
#include "cjdrch.for"
#include "cdsmax.for"
C
      INTEGER DATE(5), IDAY /0/
      DIMENSION ITRCLL(6), NCNCK(24), NHTRNG(3)
C
N     JET-CHAMBER AND VERTEX RESOLUTION
      DATA RESJ0 /.200/, RESV0 /.300/
C
N     MASK FOR L/R BIT IN HIT LABEL
      INTEGER MKLRT1 /Z1000000/, MKLRT2 /Z100/
C
N     MASK FOR TRACKS AT CELL WALL
      INTEGER MKBDCL(3) /Z10,Z20,Z40/
      INTEGER MKDDCL(3) /Z01,Z02,Z04/
C
C     IF(IDATA(IPTR+1).LT. 4) RETURN
C     I0 = IPTR + 1
C     I9 = IPTR + 48
C     PRINT 2001, (IDATA(I1),I1=I0,I9)
C     I0 = IPJHTL*2 + 1
C     I9 = I0 + IDATA(IPJHTL)*2 - 1
C     PRINT 2000, IPJHTL,I0,I9,(HDATA(I1),I1=I0,I9)
C     IPJETC = IDATA(IBLN('JETC'))
C     I0 = IPJETC*2 + 1
C     I9 = I0 + 109
C     PRINT 2000, IPJETC,I0,I9,(HDATA(I1),I1=I0,I9)
C2000 FORMAT('0REFIT:',3I8,/,(20(1X,Z4)))
C2001 FORMAT(1H0,2I3,I8,2(I4,3F6.1,3F6.3),
C    ,     /,14X,I3,4E13.5,F6.2,I3,4E13.5,
C    ,     /,14X,I3,2F8.3,F6.1,I3,10X,6I3,8I6,2X,Z4)
C2002 FORMAT('0FETCH:',2I3,2I5,12F9.5)
C2003 FORMAT('0ROTATION:',12F10.5)
C2004 FORMAT('0CIRC.CENTRE:',2I3, F10.5,2F10.0,F8.1,2F8.1)
C2005 FORMAT('0TRACK:',I6,/,(1X,3I6,4F8.1,I4,F6.2,2I4,F8.3,I6,F8.1))
C2006 FORMAT(1X,I6,5F8.2,F12.1,5F8.2)
C2007 FORMAT(' FETCH:',I3,9F8.4,F10.5,F6.0)
C2008 FORMAT(' FIT:',2I3,F8.2,F5.0,F10.6,F7.3,F5.1,F6.3,F5.1)
C2009 FORMAT(' JHTL:',I8,1X,Z8,3I5)
C2010 FORMAT(' HIT:',I6,12F8.2)
C2011 FORMAT('0ABERR:',10F10.6)
C2012 FORMAT('0ERROR:',10E13.6)
C2014 FORMAT('0FIT-BANK:',5F8.3,5X,5F8.3,5X,F8.5,2F8.1)
C2016 FORMAT('0ITRCLL =',6I8,/,(9X,6F8.3))
C2107 FORMAT(' SIGLM:',10F8.3)
C
N     INITIALIZATION
      DATA LBINIT /0/
      IF LBINIT .EQ. 0
      THEN
        LBINIT = 1
        PERFORM INIT
      CIF
C
N     RESERVE SPACE IN CWORK
      HPFREE = 1
      HPFRE1 = HPFREE
      HPCO0  = HPFREE
      HLDCO  = 14
      HPFREE = HLDCO*100 + HPCO0
      HPCO9 = HPFREE - 1
C
N     GET RUN #
      IPHEAD = IDATA(IQHEAD)*2
      NRUN = HDATA(IPHEAD+10)
C
N     COPY TRACK BANK
      HPTR0 = HPFREE
      CALL MVC(IWRK(HPTR0),0,IDATA(IPTR+1),0,192)
      IWRK(HPTR0+1) = 0
C
N     TRACK #
      ITRK = IDATA(IPTR+1)
C
N     CENTRE OF CIRCLE (USED FOR ANGULAR CORRECTION)
      IF IDATA(IPTR+18).EQ.1
      THEN
N       CIRCLE PARAMETERS
        ALFA  = ADATA(IPTR+21)
        CRV   = ADATA(IPTR+19)
        IF(ABS(CRV).LT.1.E-8) CRV = SIGN(1.E-8,CRV)
        RAD   =  1./CRV + ADATA(IPTR+20)
        XCIRC = COS(ALFA) * RAD
        YCIRC = SIN(ALFA) * RAD
        CHARGE = SIGN(1.,ADATA(IPTR+25))
      ELSE
N       PARABOLA PARAMETERS
        CRV   = ADATA(IPTR+22)*2.
        IF(ABS(CRV).LT.1.E-8) CRV = SIGN(1.E-8,CRV)
        ALFA  = ADATA(IPTR+19)
        XCIRC =-SIN(ALFA)/CRV + ADATA(IPTR+20)
        YCIRC = COS(ALFA)/CRV + ADATA(IPTR+21)
        CHARGE =-SIGN(1.,ADATA(IPTR+22))
      CIF
C
N     ZVERT, THETA + DIR. COSINES
      ZVERT = ADATA(IPTR+31)
      TGTH = ADATA(IPTR+30)
      CSTHI = SQRT(TGTH**2 + 1.)
      CSTH  = 1. / CSTHI
      SNTH  = CSTH * TGTH
C     PRINT 2004,ITRK,IDATA(IPTR+18),ALFA,XCIRC,YCIRC,ZVERT,TGTH,CSTHI
C
N     GET X-Y-VERTEX AND DETERMINE ERROR
      IPV    = ICALIB(10)
      XO     = ACALIB(IPV+ 1)
      YO     = ACALIB(IPV+ 3)
C     I0 = IPV + 1
C     I9 = IPV + 6
C     PRINT 2029, XO,YO,(ACALIB(I1),I1=I0,I9)
      PTRANS = ABS(0.0299792458*BKGAUS/ADATA(IPTR+25)) * .001
      RESV   = RESV0**2 + RESMS / PTRANS**2
      WGHT0  = RESJ0**2 / RESV
      F1     = ERRFAC
      IF(F1 .LT. .10) F1 = .10
      WGHT0  = WGHT0 / F1**2
C     PRINT 2029, XO,YO,WGHT0,F1,RESV,RESMS,PTRANS
C2029 FORMAT(' VERTEX',9E13.5)
C     PRINT 2011,ABERR
C
N     ROTATION ANGLE (USING LAST POINT OF TRACK)
      XT    = (ADATA(IPTR+12) + XO) * .5
      YT    = (ADATA(IPTR+13) + YO) * .5
      XX    =  ADATA(IPTR+12) - XO
      YY    =  ADATA(IPTR+13) - YO
      RR    = SQRT(XX**2+YY**2)
      CSROT = XX / RR
      SNROT = YY / RR
      XOT = XO - XT
      YOT = YO - YT
      X0   = XOT*CSROT + YOT*SNROT
      Y0   =-XOT*SNROT + YOT*CSROT
      XOR  =- XT*CSROT -  YT*SNROT
      YOR  =  XT*SNROT -  YT*CSROT
C     PRINT 2003, CSROT,SNROT,XX,YY,XT,YT,X0,Y0,XO,YO,WGHT0
C
N     FILL CELL ARRAY
      PERFORM SELCLL
C
N     LOOP OVER ALL CELLS + FETCH HITS
      KCLL = 0
      NHIT = 0
      IPCO = HPCO0
C
N     LOOP OVER RINGS
      JRING = 0
N     TRACKS AT CELL WALLS
      LBCELL = 0
      REPEAT
      JRING = JRING + 1
        NHTRNG(JRING) = 0
        NHRNG = 0
        NCLL = 0
        REPEAT
        NCLL = NCLL + 1
        KCLL = KCLL + 1
          JCELL = ITRCLL(KCLL)
          IF JCELL.NE.0
          THEN
            PERFORM FETCH
            NHRNG = NHRNG + JHIT
          CIF
        UNTIL NCLL.EQ.2
N       SET LABEL FOR TRACK AT CELL BOUND.
        IF(JCELL.NE.0) LBCELL = LOR(MKBDCL(JRING),LBCELL)
      UNTIL KCLL.EQ.6
      HPCO9 = IPCO - 1
C     PRINT 2005, LBCELL,(WRK(I),I=HPCO0,HPCO9)
C
N     1. PARABOLA FIT
N     LAST RING INCLUDED IN FIT
      JRINGL = 3
      PERFORM FPARA0
C
N     RELABEL HITS
      ALBLM1 = 0.6
      ALBLM2 = 3.0
      PERFORM LABEL
C     PRINT 2005, LBCELL,(WRK(I),I=HPCO0,HPCO9)
C
N     REFIT PARABOLA
      PERFORM FPARA0
C
N     RELABEL HITS
      PERFORM LABEL
C     PRINT 2005, LBCELL,(WRK(I),I=HPCO0,HPCO9)
C
N     SET UP FIT-BANK
      IF SIG.LT.1.
      THEN
        PERFORM FITBNK
      CIF
C
N     CHECK IF BAD FIT AND LOW MOMENTUM
      IF ABS(PAR1).GT..00030 .AND. NHTRNG(1)+NHTRNG(2).GT.16
      THEN
        ALBLM1 = 1.5
        ALBLM2 = 3.0
        PERFORM LABEL
        JRINGL = 2
        PERFORM FPARA0
        ALBLM1 = 0.6
        PERFORM LABEL
        PERFORM FPARA0
        PERFORM LABEL
        IF SIG.LT..10
        THEN
          PERFORM FITBK1
          IWRK(IP+ 4) = 32
        CIF
      CIF
      IF ABS(PAR1).GT..00150 .AND. NHTRNG(1)+NHTRNG(2).GT.9
      THEN
        ALBLM1 = 1.5
        ALBLM2 = 3.0
        PERFORM LABEL
        JRINGL = 1
        NHTFIT = NHTRNG(1)
        IF NHTFIT.LE.5
        THEN
          NHTFIT = NHTFIT + NHTRNG(2)
          JRINGL = 2
        CIF
        IF NHTFIT.GT.9
        THEN
          PERFORM FPARA0
          ALBLM1 = 0.6
          PERFORM LABEL
          PERFORM FPARA0
          PERFORM LABEL
          IF SIG.LT..10
          THEN
            PERFORM FITBK1
            IWRK(IP+ 4) = 48
          CIF
        CIF
      CIF
C
      HPFREE = HPFRE1
      RETURN
C
N     *************************
N     *      F P A R A 0      *
N     *************************
C
C
N     PARABOLA FIT THROUG ORIGIN
      PROC FPARA0
C
N     GET EQUATIONS
N     WEIGHT ORIGIN AS POINT OF PARABOLA
      S0 = WGHT0
      S1 = X0*WGHT0
      S2 = S1*X0
      S3 = S2*X0
      S4 = S3*X0
      S7 = Y0 * WGHT0
      S6 = S7*X0
      S5 = S6*X0
      IPCO = HPCO0
      REPEAT
       IF IWRK(IPCO+ 10).EQ.0 .AND. IWRK(IPCO+12).LE.JRINGL
       THEN
          X = WRK(IPCO+3)
          Y = WRK(IPCO+4)
          X2 = X**2
          S1 = S1 + X
          S2 = S2 + X2
          S3 = S3 + X*X2
          S4 = S4 + X2**2
          S5 = S5 + Y*X2
          S6 = S6 + Y*X
          S7 = S7 + Y
          S0 = S0 + 1.
        CIF
      IPCO = IPCO + HLDCO
      UNTIL IPCO.GT.HPCO9
      IF S0.LT.2.5
      THEN
        SIG = 1000.
      ELSE
C
N       SOLVE EQUATIONS FOR PARABOLA FIT
        F1 = 1. / S4
        XX12 = S3*F1
        XX13 = S2*F1
        YY1  = S5*F1
        XX22 = S2 - S3*XX12
        XX23 = S1 - S3*XX13
        YY2  = S6 - S3*YY1
        XX32 = S1 - S2*XX12
        XX33 = S0 - S2*XX13
        YY3  = S7 - S2*YY1
        IF XX22.GT.XX32
        THEN
          XX23 = XX23 / XX22
          YY2  = YY2  / XX22
          PAR3 = (YY3 - XX32*YY2) / (XX33 - XX32*XX23)
          PAR2 = YY2 - XX23*PAR3
        ELSE
          XX33 = XX33 / XX32
          YY3  = YY3  / XX32
          PAR3 = (YY2 - XX22*YY3) / (XX23 - XX22*XX33)
          PAR2 = YY3 - XX33*PAR3
        CIF
        PAR1 = YY1 - XX12*PAR2 - XX13*PAR3
        DEG = S0 - WGHT0 - 2.
C
C
N       CALC. CHISQ + SOLVE L/R AMBIGUITY
        CHISQ = 0.
        DCHIM1 = 0.
        IHITM1 = 0
        XST    = 999999.
        XEN    =-999999.
        IPCO = HPCO0
        REPEAT
         IF IWRK(IPCO+ 10).EQ.0 .AND. IWRK(IPCO+12).LE.JRINGL
         THEN
            X = WRK(IPCO+3)
            IF(X.LT.XST) XST = X
            IF(X.GT.XEN) XEN = X
            Y = WRK(IPCO+4)
            F = (PAR1 *X + PAR2 )*X + PAR3
            DCHI = Y - F
            WRK(IPCO+13) = DCHI
N           SUM FOR RMS
            CHISQ = CHISQ + DCHI**2
N           KEEP BIGGEST RMS
C           IF ABS(DCHI).GE.DCHIM1
C           THEN
C             DCHIM1 = ABS(DCHI)
C             IHITM1 = IPCO
C           CIF
C     PRINT 2006, IPCO,X,Y,F,DCHI,CHISQ
          CIF
        IPCO = IPCO + HLDCO
        UNTIL IPCO.GT.HPCO9
        SIG    =      CHISQ  / DEG
C     PRINT 2008, ITRK,XST,SIG,DEG,PAR1,PAR2,PAR3,WGHT0,Y0
C
N       SET LIMIT FOR SIGMA
        SIGLM = TRELLM(16)**2
      CIF
C
      CPROC
C
N     *************************
N     *      S E L C L L      *
N     *************************
C
C
N     SELECT CELLS CONTAINING TRACK
      PROC SELCLL
C
        FOR I=1,6
          ITRCLL(I) = 0
        CFOR
        IPC0 = IPTR + 34
        IPC9 = IPC0 +  5
        ICELL = 0
        FOR IPC = IPC0,IPC9
          JCELL = IDATA(IPC)
          IF JCELL.GT. 0 .AND. JCELL.LE.96
          THEN
            JRING = 1
            IF(JCELL.GT.24) JRING = 2
            IF(JCELL.GT.48) JRING = 3
            JPC = JRING*2 - 1
            IF ITRCLL(JPC).EQ.0
            THEN
              ITRCLL(JPC) = JCELL
            ELSE
              IF(ITRCLL(JPC).NE.JCELL) ITRCLL(JPC+1) = JCELL
            CIF
            ICELL = JCELL
            IRING = JRING
          CIF
        CFOR
C
C     PRINT 2016, ITRCLL
      CPROC
C
N     *************************
N     *      F E T C H        *
N     *************************
C
C
N     FETCH HITS IN CELL
      PROC FETCH
C
N       DIR. OF SENSEW. + DRIFTSP.
        IF JRING.NE.3
        THEN
          IC1 = JCELL
          IF(IC1.GT.24) IC1 = IC1 - 24
          CSROT0 = DIRWR1(IC1,1)
          SNROT0 = DIRWR1(IC1,2)
        ELSE
          IC1 = JCELL - 48
          CSROT0 = DIRWR3(IC1,1)
          SNROT0 = DIRWR3(IC1,2)
        CIF
        DRICS  = TRMATC(JCELL,2)
        DRISN  = TRMATS(JCELL,2)
        DRITG  = DRISN/DRICS
        DRISNF = DRISN * .05
C
N       LOAD RADIUS AND WIRE SPACING
        R0 = FSENSW(JRING)
        DR = RINCR (JRING)
C
N       ANGLE OF TRACK IN RING
        R1   = DR*7.5 + R0
        DX   = R1 * CSROT0 - XCIRC
        DY   = R1 * SNROT0 - YCIRC
        RR   = SQRT(DX**2 + DY**2) * CHARGE
        CSB  = DX / RR
        SNB  = DY / RR
        TGB  = CSB/SNB
C
N       SET DRIFT SPACE BIN
        DSBIN1 = DRIVEL(JCELL,1)
        IF(NRUN.GT.100) DS0 = T0FIX(JRING)*DSBIN1*64.
        IF(NRUN.LE.100) DS0 = DSBIN1*.5
N       ANGLE(TRACK,DRIFT DIRECT.)
        TANBET = ABS((TGB-DRITG)/(TGB*DRITG+1.))
C     PRINT 2007, JCELL,CSROT0,SNROT0,DRICS,DRISN,CSB,SNB,CHARGE,TANBET,
C    ,            DSBIN1,DS0
N       CORRECTION CONSTANTS FOR JCELL
C
        IPJCOR = ICALIB(5) + JCELL
        CCST01 = ACALIB(IPJCOR    ) * TANBET
        CCST02 = ACALIB(IPJCOR+ 96) * TANBET
        CCST11 = ACALIB(IPJCOR+192)
        CCST12 = ACALIB(IPJCOR+288)
        CCST21 = ACALIB(IPJCOR+384)
        CCST22 = ACALIB(IPJCOR+480)
        CCST51 = ACALIB(IPJCOR+576) * 10.
        CCST52 = ACALIB(IPJCOR+672) / 121.15
        CCST61 = ACALIB(IPJCOR+768) * 10.
        CCST62 = ACALIB(IPJCOR+864) / 121.15
C     PRINT 2002, JRING,JCELL,IP,IPJCOR,CCST01,CCST02,CCST11,CCST12,
C    ,            CCST21,CCST22,CCST51,CCST52,CCST61,CCST62
N       COUNTER FOR NUMBER OF HITS FOUND
        JHIT = 0
        NHIT   = 0
        NHGOOD = 0
N       PRESET LAST LAYER
        ILAYL =-99
N       LOOP OVER ALL HITS OF CELL
        IPCO = IPCO - HLDCO
        IPJETC = IDATA(IQJETC)*2
        IP0    = IPJETC + 100
        IPCLL  = IPJETC + 2 + JCELL
        IP     = HDATA(IPCLL  ) + IP0
        IP9    = HDATA(IPCLL+1) + IP0
        IPHL   = IPJHTL + 2 + HDATA(IPCLL)/4
C     PRINT 2002, JRING,JCELL,IP,IP9,TGB,SNB,CSB,DRISN,DRICS
        WHILE IP.LT.IP9
C
N         CHECK TRACK # OF HIT LABEL
          LB   = IDATA(IPHL)
          ITR1 = LAND(SHFTR(LB,17),127)
          ITR2 = LAND(SHFTR(LB, 1),127)
C     PRINT 2009, IPHL,LB,ITR1,ITR2,ITRK
          IF ITR1.EQ.ITRK .OR. ITR2.EQ.ITRK
          THEN
C
N           L/R FROM HIT LABEL
            LBLR = 0
            IF(ITR1.EQ.ITRK) LBLR = LAND(LB,MKLRT1)
            IF(ITR2.EQ.ITRK) LBLR = LAND(LB,MKLRT2)
            LBSIDE =-1
            IF(LBLR.NE.0) LBSIDE = 1
            LBLR = LBSIDE
C
            IWIR = HDATA(IP)
            IWIR = SHFTR(IWIR,3)
N           LAYER NUMBER WITHIN RING 3
            ILAY = LAND(IWIR,15)
N           AMPLITUDES
            IAMPL = HDATA(IP+1)
            IAMPR = HDATA(IP+2)
N           DRIFT SPACE
            DS =(HDATA(IP+3)) * DSBIN1
C     DATA NPRHT /0/
C     NPRHT = NPRHT + 1
C     IF(NPRHT.LE.25) PRINT 2019, IWIR,ILAY,JCELL,HDATA(IP+3),DS,DSBIN1
C2019 FORMAT(' HIT ',4I6,F6.1)
            X1   = ILAY * DR + R0
            Z1   = X1*TGTH + ZVERT
N           CORRECTION FOR TOF + PROPAG. ALONG WIRE
            DDS = (1222.9-ABS(Z1))*ABERR(1) + ABERR(6)*R1*CSTHI
            DSC = DS - DDS + DS0
            Y1   = SWDEPL
            IF(LAND(ILAY,1).NE.0) Y1 =-Y1
            Y1   = (7-ILAY)*(CCST52*Z1+CCST51) - CCST62*Z1-CCST61 + Y1
            X    = X1*CSROT0 - Y1*SNROT0
            Y    = X1*SNROT0 + Y1*CSROT0
            IF DS.LE.DRC
            THEN
              IF DS.LT.4.0
              THEN
                IF DS.GT.DSD1
                THEN
                  DSC = (DSD1-DSD0)*DRV0 + (DS-DSD1)*DRV1
                ELSE
                  DSC = (DS-DSD0)*DRV0
                CIF
                IF(DSC.LT.0.1) DSC = 0.1
              CIF
              DXR  = DSC * CSB
              DYR  = DSC * SNB
              DXL =-DXR
              DYL =-DYR
            ELSE
C
N             EDGE WIRE FIELD DISTORTION
              IF ILAY.LT. 3
              THEN
                DILAY =-(ILAY- 3)**2
                DSCL  = (DILAY*CCST11 + 1.) * DSC
                DSCR  = (DILAY*CCST12 + 1.) * DSC
              ELSE
              IF ILAY.GT.12
              THEN
                DILAY =-(ILAY-12)**2
                DSCL  = (DILAY*CCST21 + 1.) * DSC
                DSCR  = (DILAY*CCST22 + 1.) * DSC
              ELSE
                DSCL = DSC
                DSCR = DSC
              CIF
              CIF
C
N             FIELD DISTORTIONS AT LARGE DRIFT TIMES
              IF DSC.GT.ABERR(7)
              THEN
                DWIR  = ILAY - 7.5
                DWIRC = DSC*DRISNF
                DWIRL = DWIR + DWIRC
                DWIRR = DWIR - DWIRC
                DSCL  = (DSCL-ABERR(7))*DWIRL*CCST01 + DSCL
                DSCR  =-(DSCR-ABERR(7))*DWIRR*CCST02 + DSCR
              CIF
              DXR  = (DSCR-DRC)*DRISN + DRC*CSB
              DYR  = (DSCR-DRC)*DRICS + DRC*SNB
              DXL  =-(DSCL-DRC)*DRISN - DRC*CSB
              DYL  =-(DSCL-DRC)*DRICS - DRC*SNB
            CIF
C     PRINT 2010, ILAY,DS,DSC,DSCL,DSCR,XL,XR,X,Y,DXL,DXR,DYL,DYR
            XL   = DXL + X - XT
            YL   = DYL + Y - YT
            XXL  = XL*CSROT + YL*SNROT
            YYL  =-XL*SNROT + YL*CSROT
            XR   = DXR + X - XT
            YR   = DYR + Y - YT
            XXR  = XR*CSROT + YR*SNROT
            YYR  =-XR*SNROT + YR*CSROT
C
N           CALCULATE Z COORDINATE
            IF IAMPR.LE.0.OR.IAMPL.LE.0
            THEN
              ZZ     = 0.
              LZGOOD = 16
            ELSE
              ZZ = IAMPR + IAMPL
              ZZ = FLOAT(IAMPR-IAMPL) * ZAL*.5 / ZZ
              LZGOOD = 0
              IF(ABS(ZZ).GT.1250.) LZGOOD = 16
            CIF
N           SET ARRAY
C     PRINT 2010, ILAY,DS,XXL,YYL,X1,Z1,XXR,YYR,Y1
C
N           CHECK IF LEFT + RIGHT SOLUTION POSSIBLE
            NLRSOL = 1
            IF(DS.LT.2.0) NLRSOL = 2
C
N           LOOP OVER LEFT +/OR RIGHT SOLUTION
            ILRSOL = 0
            REPEAT
            ILRSOL = ILRSOL + 1
C
N             SELECT SIDE
              IF NLRSOL.EQ.1 .AND. LBSIDE.LT.0  .OR.
     ?           NLRSOL.EQ.2 .AND. ILRSOL.EQ.1
              THEN
N               LEFT SIDE
                LBSIDE =-1
                XX  = XXL
                YY  = YYL
              ELSE
N               RIGHT SIDE
                LBSIDE = 1
                XX  = XXR
                YY  = YYR
              CIF
C
N             HIT QUALITY:
              LBGOOD = 0
              IF(LBSIDE.NE.LBLR) LBGOOD = 1
N             NEW LAYER?
              IF ILAY.NE.ILAYL .OR. LBGDL.LE.1.AND.LBGOOD.LE.1
              THEN
                LBREG = 1
N               INCREASE HIT COUNTER
                JHIT = JHIT + 1
                IPCO = IPCO + HLDCO
              ELSE
N               2 HITS IN SAME LAYER, SELECT CLOSEST
                LBREG = 0
                IF(LBGOOD.LT.IWRK(IPCO+10)) LBREG = 1
              CIF
N             REGISTER NEW HIT?
              IF LBREG.NE.0
              THEN
                NHIT   = NHIT   + 1
                IF(LBGOOD.LE.1) NHGOOD = NHGOOD + 1
                IWRK(IPCO   ) = ILAY
                IWRK(IPCO+ 1) = IP
                IWRK(IPCO+ 2) = LBSIDE
                WRK (IPCO+ 3) = XX
                WRK (IPCO+ 4) = YY
                WRK (IPCO+ 5) = ZZ
                WRK (IPCO+ 6) = XX - X0
                IWRK(IPCO+ 7) = LZGOOD
                WRK (IPCO+ 8) = DS
                IWRK(IPCO+ 9) = JCELL
                IWRK(IPCO+10) = LBGOOD
                WRK (IPCO+11) = TGB
                IWRK(IPCO+12) = JRING
                WRK (IPCO+13) = 0.
                ILAYL = ILAY
                LBGDL = LBGOOD
                NHTRNG(JRING) = NHTRNG(JRING) + 1
              CIF
C
            UNTIL ILRSOL.GE.NLRSOL
C
          CIF
C
        IPHL = IPHL + 1
        IP   = IP   + 4
        CWHILE
N       SET IPCO TO 1. FREE LOCATION
        IPCO = IPCO + HLDCO
C
N     MASK FOR TRACKS AT CELL WALL + IN DEAD CELLS
C
N       SET LABEL FOR DEAD CELL
        IF NHIT.LE.2
        THEN
          IF DEADCL(JCELL,NRUN)
          THEN
            LBCELL = LOR(LBCELL,MKDDCL(JRING))
            JHIT = 16
            NHIT = 16
C     PRINT 2019, JCELL,JRING,NRUN,LBCELL
          CIF
        CIF
C
      CPROC
C
N     *************************
N     *      F I T B N K      *
N     *************************
C
C
N     SET UP FIT-BANK
      PROC FITBNK
C
N     START + END POINTS
      YST  = (PAR1 *XST + PAR2 )*XST + PAR3
      YEN  = (PAR1 *XEN + PAR2 )*XEN + PAR3
N     DIRECTION AT START + END POINT
      TGST = PAR1*XST*2 + PAR2
      DXST = 1./SQRT(TGST**2+1.)
      DYST = DXST * TGST
      TGEN = PAR1*XEN*2 + PAR2
      DXEN = 1./SQRT(TGEN**2+1.)
      DYEN = DXEN * TGEN
N     MIN. OF PARABOLA
      XMIN = -PAR2*.5 / PAR1
      YMIN = (PAR1*XMIN + PAR2)*XMIN + PAR3
C
N     CURVATURE + ERROR
      CURV =-PAR1 * 2.
      DET = (S2*S0-S1*S1)*S4 + (S2*S1-S3*S0)*S3 + (S3*S1-S2*S2)*S2
      SIG11 = (S2*S0 - S1*S1)/DET
      SIG22 = (S4*S0 - S2*S2)/DET
      SIG33 = (S4*S2 - S3*S3)/DET
      SIG12 = (S3*S0 - S2*S1)/DET
      SIG13 = (S3*S1 - S2*S2)/DET
      SIG23 = (S4*S1 - S3*S2)/DET
C     PRINT 2012, DET,SIG11,SIG22,SIG33,SIG12,SIG13,SIG23,SIG
C
C
C     PRINT 2014, XST,YST,DXST,DYST,TGST,XEN,YEN,DXEN,DYEN,TGEN,CURV
C
N     FILL FIT-BANK
      IP    = HPTR0 - 1
      IWRK(IP+ 1) = ITRK
      IWRK(IP+ 2) = 32
      IWRK(IP+ 3) = IDAY
      IWRK(IP+ 4) = 16
      WRK (IP+ 5) = XST *CSROT - YST *SNROT + XT
      WRK (IP+ 6) = XST *SNROT + YST *CSROT + YT
      WRK (IP+ 7) = XST - X0
      WRK (IP+ 8) = (DXST*CSROT - DYST*SNROT)
      WRK (IP+ 9) = (DXST*SNROT + DYST*CSROT)
      WRK (IP+10) = SNTH
      IWRK(IP+11) = 0
      WRK (IP+12) = XEN *CSROT - YEN *SNROT + XT
      WRK (IP+13) = XEN *SNROT + YEN *CSROT + YT
      WRK (IP+14) = XEN - X0
      WRK (IP+15) = (DXEN*CSROT - DYEN*SNROT)
      WRK (IP+16) = (DXEN*SNROT + DYEN*CSROT)
      WRK (IP+17) = SNTH
      IWRK(IP+18) = 2
      WRK (IP+19) = ATAN2(SNROT,CSROT)
      WRK (IP+20) = XMIN*CSROT - YMIN*SNROT + XT
      WRK (IP+21) = XMIN*SNROT + YMIN*CSROT + YT
      WRK (IP+22) = PAR1
C     IF(SIG  .LT.0) PRINT 2021,WRK(IP+1),S0,SIG
C2021 FORMAT(' -VE SQRT:',I4,5E13.5)
      WRK (IP+23) = SQRT(SIG)
      IWRK(IP+24) = S0 + .001
      WRK (IP+25) = CURV
C     IF(SIG11.LT.0) PRINT 2021,WRK(IP+1),S0,SIG,SIG11
      WRK (IP+26) = SQRT(SIG*SIG11) * 2.
      WRK (IP+27) = CURV
      WRK (IP+28) = CURV
      WRK (IP+31) = XOR - X0
C     I0 = IP+ 1
C     I9 = IP+48
C     PRINT 2001,(WRK(I1),I1=I0,I9)
      CPROC
C
N     *************************
N     *      F I T B K 1      *
N     *************************
C
C
N     CHANGE FIT BANK (1.POINT)
      PROC FITBK1
C
N     START POINT
      YST  = (PAR1 *XST + PAR2 )*XST + PAR3
N     DIRECTION AT START POINT
      TGST = PAR1*XST*2 + PAR2
      DXST = 1./SQRT(TGST**2+1.)
      DYST = DXST * TGST
N     MIN. OF PARABOLA
      XMIN = -PAR2*.5 / PAR1
      YMIN = (PAR1*XMIN + PAR2)*XMIN + PAR3
C
N     CURVATURE + ERROR
      CURV =-PAR1 * 2.
      DET = (S2*S0-S1*S1)*S4 + (S2*S1-S3*S0)*S3 + (S3*S1-S2*S2)*S2
      SIG11 = (S2*S0 - S1*S1)/DET
      SIG22 = (S4*S0 - S2*S2)/DET
      SIG33 = (S4*S2 - S3*S3)/DET
      SIG12 = (S3*S0 - S2*S1)/DET
      SIG13 = (S3*S1 - S2*S2)/DET
      SIG23 = (S4*S1 - S3*S2)/DET
C     PRINT 2012, DET,SIG11,SIG22,SIG33,SIG12,SIG13,SIG23,SIG
C
C     PRINT 2014, XST,YST,DXST,DYST,TGST,XEN,YEN,DXEN,DYEN,TGEN,CURV,
C    ,            XMIN,YMIN
C
N     FILL FIT-BANK
      IP    = HPTR0 - 1
      WRK (IP+ 5) = XST *CSROT - YST *SNROT + XT
      WRK (IP+ 6) = XST *SNROT + YST *CSROT + YT
      WRK (IP+ 7) = XST - X0
      WRK (IP+ 8) = (DXST*CSROT - DYST*SNROT)
      WRK (IP+ 9) = (DXST*SNROT + DYST*CSROT)
      WRK (IP+10) = SNTH
      IWRK(IP+18) = 2
      WRK (IP+19) = ATAN2(SNROT,CSROT)
      WRK (IP+20) = XMIN*CSROT - YMIN*SNROT + XT
      WRK (IP+21) = XMIN*SNROT + YMIN*CSROT + YT
      WRK (IP+22) = PAR1
C     IF(SIG  .LT.0) PRINT 2022,WRK(IP+1),S0,SIG
C2022 FORMAT(' -VE SQRT(1):',I4,5E13.5)
      WRK (IP+23) = SQRT(SIG)
      IWRK(IP+24) = S0 + .001
      WRK (IP+25) = CURV
C     IF(SIG11.LT.0) PRINT 2022,WRK(IP+1),S0,SIG,SIG11
      WRK (IP+26) = SQRT(SIG*SIG11) * 2.
      WRK (IP+27) = CURV
C     I0 = IP+ 1
C     I9 = IP+48
C     PRINT 2001,(WRK(I1),I1=I0,I9)
      CPROC
C
C
N     *************************
N     *      L A B E L        *
N     *************************
C
C
N     LABEL USED HITS
      PROC LABEL
C
N       PRESET LAST HIT POINTER
        IWL = -999
C
N       PRESET LAST HIT POINTER
        IWL = -999
        FOR IP = HPCO0,HPCO9,HLDCO
          IW0 = IWRK(IP)
          X   = WRK(IP+3)
          Y   = WRK(IP+4)
          F   = (PAR1*X + PAR2)*X + PAR3
          DF  = F - Y
N         SELECT CLOSEST HIT
          LBGOOD = 4
          IF(ABS(DF).LT.ALBLM2) LBGOOD = 1
          IF(ABS(DF).LT.ALBLM1) LBGOOD = 0
          IWRK(IP+ 10) = LBGOOD
          WRK (IP+13) = DF
C
N         CHECK IF 2 HITS FROM SAME WIRE
          IF IWL.EQ.IW0
          THEN
N           SELECT CLOSEST HIT
            IF ABS(DFL).LT.ABS(DF)
            THEN
              IWRK(IP +10) = 16
            ELSE
              IWRK(IPL+10) = 16
            CIF
          CIF
N         STORE LAST POINTERS + DF
          IWL = IW0
          IPL = IP
          DFL = DF
        CFOR
C
      CPROC
C
C
N     *************************
N     *      I N I T          *
N     *************************
C
C
N     INITIALIZE CONSTANTS
      PROC INIT
C
        IQJETC = IBLN('JETC')
        IQHEAD = IBLN('HEAD')
C
        CALL DAY2(DATE)
        IDAY = DATE(1)*1000 + DATE(2)
C
N       MULT. SCATTERING CONSTANTS
        RESMS = .020**2/2. * .16 * (1. + ALOG10(.16) / 9) * 155.45**2
C
N       RADIUS AROUND WIRE FOR CORR. OF DRIFTSPACE
        DRC = RINCR(1)*.5 * DRICOS
C       CONST. FOR VAR. OF DRIFT VEL.
        IPHEAD = IDATA(IQHEAD)*2
        NRUN = HDATA(IPHEAD+10)
        IF NRUN.LE.100
        THEN
          DSD0   = .0
          DSD1   = 5.0
          DSD2   = 5.0
          DRV0   = 1.0
          DRV1   = 1.0
        ELSE
          DSD0   =-.63
          DSD1   = 1.8
          DSD2   = 4.0
          DRV0   = 0.8
          DRV1   = (DSD2 - (DSD1-DSD0)*DRV0) / (DSD2-DSD1)
        CIF
      CPROC
C
      END
C   09/06/83 707281320  MEMBER NAME  JFETCH0  (ZS)          SHELTRAN
      SUBROUTINE JFETCH(IPTR,IPJHTL,WRK,LHIT,IPRES,INDEX,/XO/,/YO/)
C
C        P. STEFFEN                         83/03/28
C
C        MODIFIED TO CALL NEW JFETCH WHEN APPLICABLE   J.SPITZER
C                                                       86/04/30
C    Z-CHAMBER COORDINATES ARE FETCHED IN CASE OF
C    ZS-FIT (INDEX=4)
C                                            15/7/87  J.S.
C        FETCH HITS FOR TRACK 'IPTR' IN PATR-BANK
C        CALCULATE COORDINATE INCLUDING ALL CORRECTIONS
C        USE SPECIAL LAYER DEPENDENT POS. + VD   ***************
C        STORE COORDINATES IN WRK(I1),I1=1,LHIT*NHIT
C
C        INDEX = 1 : COORDINATES IN REAL SPACE
C        INDEX = 2 : X-AXIS THROUGH 1. + LAST POINT
C        INDEX = 3 : X-AXIS THROUGH (XO,YO) + LAST POINT
C
C        INDEX = 4 : NEW FOR S-Z FITS    J. SPITZER 22/4/87
C                    COORDINATES IN REAL SPACE
C
      IMPLICIT INTEGER*2 (H)
C
      DIMENSION WRK(200)
      EQUIVALENCE (ZWZ,IZW)
C---------------------------------
      COMMON/JSCALD/ JESCAL,JESKEY
C---------------------------------
#include "cdata.for"
C
#include "calibr.for"
C
C
#include "cjdrch.for"
#include "cdsmax.for"
C
      COMMON/CCMZCT/ DIMPCT, ZCUTV, ZCUTVV, IZVCST(5), ZCHWW
C ONLY ZCHWW WHICH IS THE WEIGHT FOR Z-CHAMBER HITS IS USED HERE
C  ARRAYS FOR Z-CHAMBER INFORMATION
      DIMENSION IZCHMB(3,2),AZCHMB(3,2)
C
      DIMENSION IRESAR(13),RESAR(13),HRESAR(26)
      EQUIVALENCE (IRESAR(1),RESAR(1),HRESAR(1))
C
N     CONSTANTS FOR ANGULAR CORRECTION
      DATA NCOAR / 15/, DTGB / .15/
      REAL TGCOAR(15) /-99.,-.45, 12*0., 99./
      REAL T0COAR(60) / .000, .000, .000, .000, .000,
     ,     .000, .000,-.020,-.060,-.130,-.030, .100, .200, .200, .200,
     ,                  .000, .000, .010, .110, .100,
     ,     .075, .050, .025, .005, .015, .065, .060, .060, .060, .060,
     ,                  .190, .190, .180, .165, .140,
     ,     .120, .100, .075, .050, .010,-.050,-.075,-.035, .000, .000,
     ,                  .110, .110, .115, .140, .135,
     ,     .085, .045, .030, .040, .050, .055, .055, .055, .055, .055/
      REAL SLCOAR(60) / 60*0./
C
N     MASK FOR L/R BIT IN HIT LABEL
      INTEGER MKLRT1 /Z1000000/, MKLRT2 /Z100/
C
C-----------------------------------------------------------------------
C      --- NEW CALIBRATION WHEN AVAILABLE AND IF REQUESTED   J.S. ---
      IF JESCAL .GT. 0   .AND.   JESKEY .EQ. 54321
      THEN
         IP8=ICALIB(13)+6
         IP9=ICALIB(13)+2598
         CALL JFTNEW(IPTR,IPJHTL,WRK,LHIT,IPRES,INDEX,XO,YO,
     +   ACALIB(IP8),ACALIB(IP9))
         RETURN
      CIF
C
      INDX=INDEX
      IF(INDX.EQ.4) INDX=1
C-----------------------------------------------------------------------
C
C     IF(IDATA(IPTR+1).LT. 4) RETURN
C     I0 = IPTR + 1
C     I9 = IPTR + 48
C     PRINT 2001, (IDATA(I1),I1=I0,I9)
C     I0 = IPJHTL*2 + 1
C     I9 = I0 + IDATA(IPJHTL)*2 - 1
C     IPJETC = IDATA(IBLN('JETC'))
C     I0 = IPJETC*2 + 1
C     I9 = I0 + 109
C2001 FORMAT(1H0,2I3,I8,2(I4,3F6.1,3F6.3),
C    ,     /,14X,I3,4E13.5,F6.2,I3,4E13.5,
C    ,     /,14X,I3,2F8.3,F6.1,I3,10X,6I3,8I6,2X,Z4)
C2002 FORMAT('0FETCH:',2I3,2I5,12F9.5)
C2003 FORMAT('0ROTATION:',12F10.5)
C2004 FORMAT('0CIRC.CENTRE:',2I3, F10.5,2F10.0,F8.1,2F8.1)
C2005 FORMAT('0TRACK:',I6,/,(1X,3I6,4F9.3,I4,F9.3,2I4,F8.3,I6,3F8.2))
C2007 FORMAT(' FETCH:',I3,8F8.5,F10.7,F6.3)
C2009 FORMAT(' JHTL:',I8,1X,Z8,3I5,I8)
C2010 FORMAT(' HIT:',I6,12F8.2)
C2011 FORMAT('0ABERR:',10F10.6)
C2016 FORMAT('0ITRCLL =',6I8,/,(9X,6F8.3))
C
C2901 FORMAT('0JFETCH(PST) CALLED WITH WRONG INDEX:',I6)
C
C
N     INITIALIZATION
      DATA LBINIT /0/
      IF LBINIT .EQ. 0
      THEN
        LBINIT = 1
        NCALL = 0
        PERFORM INIT
      CIF
      NCALL = NCALL + 1
C
CCC   RESERVE SPACE IN CWORK
CCC   HPCO0  = 1
CCCCC LHIT   = MAX0(LHIT,14)
      LHBIT  = LHIT*4
CCC   HPFREE = LHIT*100 + HPCO0
CCC   HPCO9  = 0
      IPCO = 1
C
N     GET RUN #
      IPHEAD = IDATA(IQHEAD)*2
      NRUN = HDATA(IPHEAD+10)
      NEVT = HDATA(IPHEAD+11)
C
N     TRACK #
      ITRK = IDATA(IPTR+1)
C
N     CENTRE OF CIRCLE (USED FOR ANGULAR CORRECTION)
      IF IDATA(IPTR+18).EQ.1
      THEN
N       CIRCLE PARAMETERS
        ALFA  = ADATA(IPTR+21)
        CRV   = ADATA(IPTR+19)
        IF(ABS(CRV).LT.1.E-8) CRV = SIGN(1.E-8,CRV)
        RAD   =  1./ABS(CRV) + ADATA(IPTR+20)
        XCIRC = COS(ALFA) * RAD
        YCIRC = SIN(ALFA) * RAD
        CHARGE = SIGN(1.,ADATA(IPTR+25))
      ELSE
N       PARABOLA PARAMETERS
        CRV   = ADATA(IPTR+22)*2.
        IF(ABS(CRV).LT.1.E-8) CRV = SIGN(1.E-8,CRV)
        ALFA  = ADATA(IPTR+19)
        XCIRC =-SIN(ALFA)/CRV + ADATA(IPTR+20)
        YCIRC = COS(ALFA)/CRV + ADATA(IPTR+21)
        CHARGE =-SIGN(1.,ADATA(IPTR+22))
      CIF
C
N     ZVERT, THETA + DIR. COSINES
      ZVERT = ADATA(IPTR+31)
      TGTH = ADATA(IPTR+30)
      CSTHI = SQRT(TGTH**2 + 1.)
      CSTH  = 1. / CSTHI
      SNTH  = CSTH * TGTH
C     PRINT 2004,ITRK,IDATA(IPTR+18),ALFA,XCIRC,YCIRC,ZVERT,TGTH,CSTHI
C
C     PRINT 2011,ABERR
C
N     ROTATION ANGLE (USING LAST POINT OF TRACK)
      SELECT INDX
      CASE 1
        XT = 0.
        YT = 0.
        CSROT = 1.
        SNROT = 0.
        XOT   = 0.
        YOT   = 0.
      CASE 2
        XT    = (ADATA(IPTR+12) + ADATA(IPTR+5)) * .5
        YT    = (ADATA(IPTR+13) + ADATA(IPTR+6)) * .5
        XX    =  ADATA(IPTR+12) - ADATA(IPTR+5)
        YY    =  ADATA(IPTR+13) - ADATA(IPTR+6)
        RR    = SQRT(XX**2+YY**2)
        IF RR.LT.10.
        THEN
           IPRES=IPCO
           RETURN
        CIF
        CSROT = XX / RR
        SNROT = YY / RR
        XX    = XO - XT
        YY    = YO - YT
        XOT   = 0.
        YOT   = 0.
      CASE 3
        XT    = (ADATA(IPTR+12) + XO) * .5
        YT    = (ADATA(IPTR+13) + YO) * .5
        XX    =  ADATA(IPTR+12) - XO
        YY    =  ADATA(IPTR+13) - YO
        RR    = SQRT(XX**2+YY**2)
        CSROT = XX / RR
        SNROT = YY / RR
        XX    = XO - XT
        YY    = YO - YT
        XOT   = XX*CSROT + YY*SNROT
        YOT   =-XX*SNROT + YY*CSROT
      OTHER
C
N       ILLEGAL INDEX
C       PRINT 2901,INDEX
        RETURN
C
      CSELECT
C
C
N     SELECT CELLS CONTAINING TRACK
C
      IPC0 = IPTR + 34
      IPC9 = IPC0 +  5
      FOR IPC = IPC0,IPC9
         JCELL = IDATA(IPC)
         IF JCELL.GT. 0 .AND. JCELL.LE.96
         THEN
            JRING = 1
            IF(JCELL.GT.24) JRING = 2
            IF(JCELL.GT.48) JRING = 3
            PERFORM FETCH
         CIF
      CFOR
C
CCC   HPCO9 = IPCO - 1
CCC   PRINT 2005, LBCELL,(WRK(I),I=1,HPCO9)
C
C
C FETCH Z-CHAMBER HITS IN CASE Z-S FITS (INDEX=4)
      IF INDEX.EQ.4 .AND. ZCHWW.GT..1 .AND. ZCHWW.LT.2000.
      THEN
         CALL ZCFTNW(NRUN,NEVT,ITRK,TGTH,ZVERT,NZHIT,IZCHMB,AZCHMB)
         IF NZHIT.GT.0
         THEN
            FOR J=1,NZHIT
               HRESAR( 1) = 100+IZCHMB(1,J)
               HRESAR( 2) = IZCHMB(2,J)
               HRESAR( 3) = 0
               HRESAR( 4) = 0
               HRESAR( 5) = 1
C              HRESAR( 6) = IP-2*IPJETC
               HRESAR( 6) = 101
               XX=AZCHMB(1,J)
               YY=AZCHMB(2,J)
               ZZ=AZCHMB(3,J)
               RESAR ( 4) = XX
               RESAR ( 5) = YY
               RESAR ( 6) = ZZ
C CALCULATE TRACK LENGTH IN R-PHI FROM FIRST POINT ON TRACK
               UX=XX-ADATA(IPTR+5)
               UY=YY-ADATA(IPTR+6)
               UU=SQRT(UX**2+UY**2)
            IF(ABS(CURVXY*UU).GT.1.E-5) UU=2.*SIN(.5*CURVXY*UU)/CURVXY
               IF(UX*ADATA(IPTR+8)+UY*ADATA(IPTR+9).LT.0.) UU=-UU
               RESAR ( 7) = UU
C              RESAR ( 8) = WW
               RESAR ( 8) = ZCHWW
               IF(NRUN.LT.24200) RESAR(8)=RESAR(8)*.6
               CALL MVC(WRK(IPCO),0,RESAR(1),0,LHBIT)
               IPCO = IPCO + LHIT
            CFOR
         CIF
      CIF
C
C
N     STORE RESULTS
      IPRES = IPCO
      IF INDEX.NE.4
      THEN
         WRK (IPRES   ) = XT
         WRK (IPRES+ 1) = YT
         WRK (IPRES+ 2) = CSROT
         WRK (IPRES+ 3) = SNROT
         WRK (IPRES+ 9) = XOT
         WRK (IPRES+10) = YOT
         WRK (IPRES+11) = CSTH
         WRK (IPRES+12) = SNTH
      CIF
C
C     PRINT 2003, CSROT,SNROT,XT,YT
C
C
      RETURN
C
C
N     *************************
N     *      F E T C H        *
N     *************************
C
C
N     FETCH HITS IN CELL
      PROC FETCH
C
N       DIR. OF SENSEW. + DRIFTSP.
        IF JRING.NE.3
        THEN
          IC1 = JCELL
          IF(IC1.GT.24) IC1 = IC1 - 24
          CSROT0 = DIRWR1(IC1,1)
          SNROT0 = DIRWR1(IC1,2)
        ELSE
          IC1 = JCELL - 48
          CSROT0 = DIRWR3(IC1,1)
          SNROT0 = DIRWR3(IC1,2)
        CIF
        DRICS  = TRMATC(JCELL,2)
        DRISN  = TRMATS(JCELL,2)
        DRITG  = DRISN/DRICS
        DRISNF = DRISN * .05
C
N       LOAD RADIUS AND WIRE SPACING
        R0 = FSENSW(JRING)
        DR = RINCR (JRING)
C
N       ANGLE OF TRACK IN RING
        R1   = DR*7.5 + R0
        DX   = R1 * CSROT0 - XCIRC
        DY   = R1 * SNROT0 - YCIRC
        RR   = SQRT(DX**2 + DY**2) * CHARGE
        CSB  = DX / RR
        SNB  = DY / RR
        TGB  = CSB/SNB
C
N       SET DRIFT SPACE BIN
        DSBIN1 = DRIVEL(JCELL,1)
C
N       ANGLE(TRACK,DRIFT DIRECT.)
        TANBET = (DRITG - TGB) / (TGB*DRITG + 1.)
C
N       DIFFERENT CORRECTION CONST. FOR MC + DATA
        IF NRUN.LE.100
        THEN
N         MC
          DS0 = DSBIN1*.5
          T0CORR = 0.
        ELSE
N         DATA
          DS0 = T0FIX(JRING)*DSBIN1*64.
          FOR I1=1,NCOAR
            IDX = I1
            IF(TANBET.LT.TGCOAR(IDX)) XFOR
          CFOR
          KRNG = JRING
          IF(KRNG.EQ.3 .AND. AND(JCELL,1).EQ.0) KRNG = 4
          IBIN = (KRNG-1)*NCOAR  + IDX
          T0CORR = (TANBET-TGCOAR(IDX)) * SLCOAR(IBIN) + T0COAR(IBIN)
        CIF
C     IF(NCALL.LE.2)
C    ,PRINT 2007, JCELL,CSROT0,SNROT0,DRICS,DRISN,CSB,SNB,CHARGE,TANBET,
C    ,            DSBIN1,DS0
C     IF(NCALL.LE.8)
C    ,PRINT 2093, JCELL,KRNG,IDX,IBIN,T0CORR,TGCOAR(IDX),T0COAR(IBIN),
C    ,            SLCOAR(IBIN),TANBET
C2093 FORMAT('0ANG.CORR.:',4I4,8F8.3)
N       CORRECTION CONSTANTS FOR JCELL
C
        IPJCOR = ICALIB(5) + JCELL
        CCST01 = ACALIB(IPJCOR     ) * ABS(TANBET)
        CCST02 = ACALIB(IPJCOR+  96) * ABS(TANBET)
        CCST11 = ACALIB(IPJCOR+ 192)
        CCST12 = ACALIB(IPJCOR+ 288)
        CCST21 = ACALIB(IPJCOR+ 384)
        CCST22 = ACALIB(IPJCOR+ 480)
        CCST51 = ACALIB(IPJCOR+ 576) * 10.
        CCST52 = ACALIB(IPJCOR+ 672) / 121.15
        CCST61 = ACALIB(IPJCOR+ 768) * 10.
        CCST62 = ACALIB(IPJCOR+ 864) / 121.15
        CCST81 = ACALIB(IPJCOR+1152)
C     IF(NCALL.LE.2)
C    ,PRINT 2002, JRING,JCELL,IP,IPCLLC,CCST01,CCST02,CCST11,CCST12,
C    ,  CCST21,CCST22,CCST51,CCST52,CCST61,CCST62,ACALIB(IPDY),CCST81
N       COUNTER FOR NUMBER OF HITS FOUND
        JHIT = 0
        NHIT   = 0
        NHGOOD = 0
N       PRESET LAST LAYER
        ILAYL =-99
N       LOOP OVER ALL HITS OF CELL
        IPCO = IPCO - LHIT
        IPJET4 = IDATA(IQJETC)
        IPJETC = IDATA(IQJETC)*2
        IP0    = IPJETC + 100
        IPCLL  = IPJETC + 2 + JCELL
        IP     = HDATA(IPCLL  ) + IP0
        IP9    = HDATA(IPCLL+1) + IP0
        IPHL   = IPJHTL + 2 + HDATA(IPCLL)/4
C     PRINT 2002, JRING,JCELL,IP,IP9,TGB,SNB,CSB,DRISN,DRICS
        WHILE IP.LT.IP9
C
N         CHECK TRACK # OF HIT LABEL
          LB   = IDATA(IPHL)
          ITR1 = LAND(SHFTR(LB,17),127)
          ITR2 = LAND(SHFTR(LB, 1),127)
C     PRINT 2009, IPHL,LB,ITR1,ITR2,ITRK,IP
          IF ITR1.EQ.ITRK .OR. ITR2.EQ.ITRK
          THEN
C
N           SET LBGOOD = 2 IF HIT ASSOCIATED WITH 2 TRACKS
            L0GOOD = 0
            IF(ITR1.NE.0 .AND. ITR2.NE.0) L0GOOD = 2
C
N           L/R FROM HIT LABEL
            LBLR = 0
            IF(ITR1.EQ.ITRK) LBLR = LAND(LB,MKLRT1)
            IF(ITR2.EQ.ITRK) LBLR = LAND(LB,MKLRT2)
            LBSIDE =-1
            IF(LBLR.NE.0) LBSIDE = 1
            LBLR = LBSIDE
C
            IWIR = HDATA(IP)
            IWIR = SHFTR(IWIR,3)
N           LAYER NUMBER WITHIN RING 3
            ILAY = LAND(IWIR,15)
N           AMPLITUDES
            IAMPL = HDATA(IP+1)
            IAMPR = HDATA(IP+2)
N           DRIFT SPACE
            DS =(HDATA(IP+3)) * DSBIN1
            X1   = ILAY * DR + R0
            Z1   = X1*TGTH + ZVERT
N           CORRECTION FOR TOF + PROPAG. ALONG WIRE
            DDS = (1222.9-ABS(Z1))*ABERR(1) + ABERR(6)*R1*CSTHI
N           CORRECTION FOR GRAVITATION
            DGR = ((Z1/1222.9)**2 - 1.) * .075
            DSC =  DS - DDS + DS0
C     DATA NPRHT /0/
C     NPRHT = NPRHT + 1
C     IF(NPRHT.LE.50) PRINT 2019, IWIR,ILAY,JCELL,HDATA(IP+3),DS,DSBIN1,
C    ,                DSC,DDS,DS0,ACALIB(IPVD+ILAY)
C2019 FORMAT(' HIT ',4I6,F7.3,5E13.5)
            Y1   = SWDEPL
            IF(LAND(ILAY,1).NE.0) Y1 =-Y1
            Y1   = (7.5-ILAY)*(CCST52*Z1+CCST51) - CCST62*Z1-CCST61 + Y100367700
            X    = X1*CSROT0 - Y1*SNROT0
            Y    = X1*SNROT0 + Y1*CSROT0 - DGR
            IF DSC.LE.DRC
            THEN
              IF DSC.LT.DSD2
              THEN
                IF DSC.LT.DSD1
                THEN
                  DSC = DSC + DDS1 + (DSC-DSD1)*DRV1
                ELSE
                  DSC = DSC + DDS2 + (DSC-DSD2)*DRV2
                CIF
N               ANGULAR CORRECTION
C               DSC = DSC/DSD2 * T0CORR + DSC
                IF(DSC.LT.0.1) DSC = 0.1
              ELSE
C
N               ANGULAR CORRECTION
C               DSC = DSC + T0CORR
                DSC = (DSC-DSD2)/(DRC-DSD2) * T0CORR + DSC
              CIF
C             DSC = DSC + DSOFF
              DXR  = DSC * CSB
              DYR  = DSC * SNB
              DXL =-DXR
              DYL =-DYR
            ELSE
C
N             ANGULAR CORRECTION
              DSC = DSC + T0CORR
C             DSC = DSC + DSOFF
C
N             EDGE WIRE FIELD DISTORTION
              IF ILAY.LT. 3
              THEN
                DILAY =-(ILAY- 3)**2
                DSCL  = (DILAY*CCST11 + 1.) * DSC * (1. - CCST81)
                DSCR  = (DILAY*CCST12 + 1.) * DSC * (1. + CCST81)
              ELSE
              IF ILAY.GT.12
              THEN
                DILAY =-(ILAY-12)**2
                DSCL  = (DILAY*CCST21 + 1.) * DSC * (1. - CCST81)
                DSCR  = (DILAY*CCST22 + 1.) * DSC * (1. + CCST81)
              ELSE
                DSCL = DSC * (1. - CCST81)
                DSCR = DSC * (1. + CCST81)
              CIF
              CIF
C
N             FIELD DISTORTIONS AT LARGE DRIFT TIMES
              IF DSC.GT.ABERR(7)
              THEN
                DWIR  = ILAY - 7.5
                DWIRC = DSC*DRISNF
                DWIRL = DWIR + DWIRC
                DWIRR = DWIR - DWIRC
                DSCL  = (DSCL-ABERR(7))*DWIRL*CCST01 + DSCL
                DSCR  =-(DSCR-ABERR(7))*DWIRR*CCST02 + DSCR
              CIF
              DXR  = (DSCR-DRC)*DRISN + DRC*CSB
              DYR  = (DSCR-DRC)*DRICS + DRC*SNB
              DXL  =-(DSCL-DRC)*DRISN - DRC*CSB
              DYL  =-(DSCL-DRC)*DRICS - DRC*SNB
            CIF
C     PRINT 2010, ILAY,DS,DSC,DSCL,DSCR,XL,XR,X,Y,DXL,DXR,DYL,DYR
            XL   = DXL + X - XT
            YL   = DYL + Y - YT
            XXL  = XL*CSROT + YL*SNROT
            YYL  =-XL*SNROT + YL*CSROT
            XR   = DXR + X - XT
            YR   = DYR + Y - YT
            XXR  = XR*CSROT + YR*SNROT
            YYR  =-XR*SNROT + YR*CSROT
C
N           CALCULATE Z COORDINATE
C           IF IAMPR.LE.0.OR.IAMPL.LE.0
C           THEN
C             ZZ     = 0.
C             LZGOOD = 16
C           ELSE
C             ZZ = IAMPR + IAMPL
C             ZZ = FLOAT(IAMPR-IAMPL) * ZAL*.5 / ZZ
C             LZGOOD = 0
C             IF(ABS(ZZ).GT.1250.) LZGOOD = 16
C           CIF
            CALL AMPS2Z( IP,IPJET4,ZZ,WW,LZGOOD)
C
N           SET ARRAY
C     PRINT 2010, ILAY,DS,XXL,YYL,X1,Z1,XXR,YYR,Y1
C
N           CHECK IF LEFT + RIGHT SOLUTION POSSIBLE
            NLRSOL = 1
            IF(DSC.LT.2.0) NLRSOL = 2
C
N           LOOP OVER LEFT +/OR RIGHT SOLUTION
            ILRSOL = 0
            REPEAT
            ILRSOL = ILRSOL + 1
            LBGOOD = L0GOOD
C
N             SELECT SIDE
              IF NLRSOL.EQ.1 .AND. LBSIDE.LT.0  .OR.
     ?           NLRSOL.EQ.2 .AND. ILRSOL.EQ.1
              THEN
N               LEFT SIDE
                LBSIDE =-1
                XX  = XXL
                YY  = YYL
              ELSE
N               RIGHT SIDE
                LBSIDE = 1
                XX  = XXR
                YY  = YYR
              CIF
C
N             HIT QUALITY:
              IF(LBSIDE.NE.LBLR) LBGOOD = LBGOOD + 1
N             NEW LAYER?
              IF ILAY.NE.ILAYL .OR. LBGDL.LE.1.AND.LBGOOD.LE.2
              THEN
                LBREG = 1
N               INCREASE HIT COUNTER
                JHIT = JHIT + 1
                IPCO = IPCO + LHIT
              ELSE
N               2 HITS IN SAME LAYER, SELECT CLOSEST
                LBREG = 0
                ZWZ = WRK(IPCO+10)
                IF(LBGOOD.LT.IZW) LBREG = 1
              CIF
N             REGISTER NEW HIT?
              IF LBREG.NE.0
              THEN
                NHIT   = NHIT   + 1
                IF(LBGOOD.LE.2) NHGOOD = NHGOOD + 1
                IF INDEX.NE.4
                THEN
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
                   HRESAR( 6) = IP-IPJETC
                   RESAR ( 4) = XX
                   RESAR ( 5) = YY
                   RESAR ( 6) = ZZ
C CALCULATE TRACK LENGTH IN R-PHI FROM FIRST POINT ON TRACK
                   UX=XX-ADATA(IPTR+5)
                   UY=YY-ADATA(IPTR+6)
                   UU=SQRT(UX**2+UY**2)
                   CURVXY=ADATA(IPTR+25)
                   IF(ABS(CURVXY).LT.1.E-8) CURVXY = SIGN(1.E-8,CURVXY)
            IF(ABS(CURVXY*UU).GT.1.E-5) UU=2.*SIN(.5*CURVXY*UU)/CURVXY
                   IF(UX*ADATA(IPTR+8)+UY*ADATA(IPTR+9).LT.0.) UU=-UU
                   RESAR ( 7) = UU
                   RESAR ( 8) = WW
                CIF
C     PRINT 2005, LHIT,(RESAR(I1),I1=1,13)
                CALL MVC(WRK(IPCO),0,RESAR(1),0,LHBIT)
CCCCC           IF LHIT.GT.14
CCCCC           THEN
CCCCC             I0 = IPCO+14
CCCCC             I9 = IPCO+LHIT - 1
CCCCC             FOR I1=I0,I9
CCCCC               WRK(I1) = 0.
CCCCC             CFOR
CCCCC           CIF
                ILAYL = ILAY
                LBGDL = LBGOOD
              CIF
C
            UNTIL ILRSOL.GE.NLRSOL
C
          CIF
C
        IPHL = IPHL + 1
        IP   = IP   + 4
        CWHILE
N       SET IPCO TO 1. FREE LOCATION
        IPCO = IPCO + LHIT
C
C
      CPROC
C
C
N     *************************
N     *      I N I T          *
N     *************************
C
C
N     INITIALIZE CONSTANTS
      PROC INIT
C
        IQJETC = IBLN('JETC')
        IQHEAD = IBLN('HEAD')
C
N       RADIUS AROUND WIRE FOR CORR. OF DRIFTSPACE
        DRC = RINCR(1)*.5 * DRICOS
C       CONST. FOR VAR. OF DRIFT VEL.
N     GET RUN #
        IPHEAD = IDATA(IQHEAD)*2
        NRUN = HDATA(IPHEAD+10)
        IF NRUN.LE.100
        THEN
          DSD0   = .0
          DSD1   = .0
          DSD2   = .0
          DDS0   = .0
          DDS1   = .0
          DDS2   = .0
          DRV1   = .0
          DRV2   = .0
        ELSE
          DSD0   =-0.400
          DSD1   = 0.300
          DSD2   = 2.500
          DDS0   = 0.720
          DDS1   = 0.330
          DDS2   = 0.0
          DRV1   = (DDS0-DDS1) / (DSD0-DSD1)
          DRV2   = (DDS1-DDS2) / (DSD1-DSD2)
        CIF
C     PRINT 2091, DSD0,DDS0,DSD1,DDS1,DSD2,DDS2,DRV1,DRV2,DRC
C2091 FORMAT(' DSD,DDS=',3(F9.3,F7.3),F11.5,F9.5,F9.3,F8.3)
C
N       INITIALIZE ANGULAR CORRECTION CONSTANTS
        I9 = NCOAR - 1
        FOR I1=2,I9
          IF(I1.GT.2) TGCOAR(I1   ) = TGCOAR(I1- 1) + DTGB
          SLCOAR(I1   ) = (T0COAR(I1   )-T0COAR(I1- 1)) / DTGB
          SLCOAR(I1+15) = (T0COAR(I1+15)-T0COAR(I1+14)) / DTGB
          SLCOAR(I1+30) = (T0COAR(I1+30)-T0COAR(I1+29)) / DTGB
          SLCOAR(I1+45) = (T0COAR(I1+45)-T0COAR(I1+44)) / DTGB
        CFOR
C     PRINT 2092, TGCOAR,T0COAR,SLCOAR
C2092 FORMAT('0ANG.CORR.:',15F8.3,/,(11X,15F8.3))
      CPROC
C
      END
C   09/06/83 707281313  MEMBER NAME  JFTNEW0  (ZS)          SHELTRAN
      SUBROUTINE JFTNEW(IPTR,IPJHTL,WRK,LHIT,IPRES,INDEX,/XO/,/YO/,
     +CALCST,DSTORW)
C
C        J. SPITZER                           /10/86
C    UPDATED 4.4.87, FROM F11SPI.JADECAL.S
C    INCLUDE POSSIBILITY OF SUPPLYING PARTICLE MASS FOR
C    FLIGHT TIME CORRECTION                   1/6/87  J.S.
C
C    Z-CHAMBER COORDINATES ARE FETCHED IN CASE OF
C    ZS-FIT (INDEX=4)
C                                            15/7/87  J.S.
C
C        FETCH HITS FOR TRACK 'IPTR' IN PATR-BANK
C        CALCULATE COORDINATE INCLUDING ALL CORRECTIONS
C        STORE COORDINATES IN WRK(I1),I1=1,LHIT*NHIT
C
C        INDEX = 1 : COORDINATES IN REAL SPACE
C        INDEX = 2 : X-AXIS THROUGH 1. + LAST POINT
C        INDEX = 3 : X-AXIS THROUGH (XO,YO) + LAST POINT
C
C        INDEX = 4 : NEW FOR S-Z FITS    J. SPITZER 22/4/87
C                    COORDINATES IN REAL SPACE
C
      IMPLICIT INTEGER*2 (H)
C
      DIMENSION WRK(200)
      EQUIVALENCE (ZWZ,IZW)
C
#include "cdata.for"
#include "cjdrch.for"
C
      COMMON/CCMZCT/ DIMPCT, ZCUTV, ZCUTVV, IZVCST(5), ZCHWW
C ONLY ZCHWW WHICH IS THE WEIGHT FOR Z-CHAMBER HITS IS USED HERE
C  ARRAYS FOR Z-CHAMBER INFORMATION
      DIMENSION IZCHMB(3,2),AZCHMB(3,2)
C
N     * CALIBRATION CONSTANTS
      COMMON/JSCALD/ JESCAL,JESKEY,JESDRW
C
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
C
      REAL B1(4,4)/
     + 0.62000E+03,   0.87300E+03,  -0.41000E+03,   0.15800E+04,
     + 0.62000E+03,   0.87300E+03,  -0.41000E+03,   0.15800E+04,
     + 0.57030E+03,   0.89000E+03,  -0.38000E+03,   0.15800E+04,
     + 0.64300E+03,   0.90200E+03,  -0.41200E+03,   0.15800E+04/
C
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
C
      REAL OMERIN(3)/2*.130900,.0654498/,ALORIN(3)/3*.34/,
     +     RR1(3)/211.,421.,632./,WIRDIS/10./,SMAXW(2,64),SM01(2,64),
     +     ANG375/.0654498/,
     +     FLTIM1/.028/,FLTIM2/.0363/,FLTIM3/1222.9/,ELFRCZ/.231/,
     +     AVFRMX/.78742/,EPS/1.E-4/,TANLOR/.365028/,SINLOR/.342898/,
     +     COSLOR/.939373/,PIVALU/3.141593/,
     +     PARVD(3)/.59562E-2,.59482E-2,.59421E-2/
C
      DATA JESOLD/-1/,LIMPRT/0/,KIMPRT/0/,LIMPR1/3/,KIMPR1/0/
C
      DIMENSION IRESAR(13),RESAR(13),HRESAR(13)
      EQUIVALENCE (IRESAR(1),RESAR(1),HRESAR(1))
C
C
N     MASK FOR L/R BIT IN HIT LABEL
      INTEGER MKLRT1 /Z1000000/, MKLRT2 /Z100/
C
      INDX=INDEX
      IF(INDX.EQ.4) INDX=1
      IF JESCAL.NE.JESOLD
      THEN
         JESOLD=JESCAL
         IF KIMPR1.LT.LIMPR1
         THEN
            KIMPR1=KIMPR1+1
            PRINT 720, JESCAL
720      FORMAT(' **** NEW ID CALIBRATION IN EFFECT IN JFETCH AFTER ',
     +   'RUN', I7,/,6X,'BIT 256 IS SET IN THE PROGRAM IDENTIFIER ',
     +   'WORD OF THE PATR BANK',////)
         CIF
      CIF
C
N     INITIALIZATION
      DATA LBINIT /0/,IQJETC/0/,IQHEAD/0/
      IF LBINIT .EQ. 0
      THEN
         LBINIT = 1
         IQJETC = IBLN('JETC')
         IQHEAD = IBLN('HEAD')
C
         A5=1./.6726
         FOR J=1,8
            GG(1,J)=GG(1,J)*A5
            GG(2,J)=GG(2,J)*A5*.5/GG(4,J)
            GG(3,J)=GG(3,J)*A5*.333333/GG(4,J)**2
            IF J.EQ.6.OR.J.EQ.7
            THEN
               GGF(1,J)=GGF(1,J)*A5
               GGF(2,J)=GGF(2,J)*A5*.5/GGF(4,J)
               GGF(3,J)=GGF(3,J)*A5*.333333/GGF(4,J)**2
            CIF
         CFOR
C
         FOR I=1,64
            IF I.LE.16
            THEN
               IRIN=1
               IW=I
            ELSE
               IF I.LE.32
               THEN
                  IRIN=2
                  IW=I-16
               ELSE
                  IRIN=3
                  IW=I-32
                  IF(IW.GT.16) IW=IW-16
               CIF
            CIF
            R=RR1(IRIN)+(IW-1)*WIRDIS
            SMAXW(1,I)=1.05*R*SIN(OMERIN(IRIN))/COS(ALORIN(IRIN)
     +      -OMERIN(IRIN))
            SMAXW(2,I)=1.05*R*SIN(OMERIN(IRIN))/COS(ALORIN(IRIN)
     +      +OMERIN(IRIN))
            SM01(1,I)=.7*SMAXW(1,I)
            SM01(2,I)=.7*SMAXW(2,I)
            IF IW.LT.3.OR.IW.GT.14
            THEN
               SM01(1,I)=.45*SMAXW(1,I)
               SM01(2,I)=.45*SMAXW(2,I)
            CIF
         CFOR
      CIF
C
      LHBIT  = LHIT*4
      IPCO = 1
C
N     GET RUN #
      IPHEAD = IDATA(IQHEAD)*2
      NRUN = HDATA(IPHEAD+10)
      IF NRUN.LT.24200
      THEN
         FREQR=1.0127
         FLTIM2=.0363
      ELSE
         FREQR=1.
         FLTIM2=0.
      CIF
      NEVT = HDATA(IPHEAD+11)
C
N     TRACK #
      ITRK = IDATA(IPTR+1)
N     SET FLAG FOR NEW CALIBRATION
      IDATA(IPTR+2) = LOR(IDATA(IPTR+2),256)
C
N     POINTER TO CALIBRATED JETC BANK
      IPJETC = IDATA(IQJETC)
      IP0    = 2*IPJETC + 100
C++++++++
N     LOCATE RAW JETC BANK
         IPRAW=IPJETC
         WHILE IDATA(IPRAW -1).GT.0
            IPRAW =IDATA(IPRAW -1)
         CWHILE
         IPRAW2=2*IPRAW -2*IPJETC
C========
C
N     ZVERT, THETA + DIR. COSINES
      ZVERT = ADATA(IPTR+31)
      TGTH = ADATA(IPTR+30)
      CSTH = 1./SQRT(TGTH**2 + 1.)
      SNTH  = CSTH * TGTH
C
C
C++++++++
         XHCS = (ADATA(IPTR+12) + ADATA(IPTR+5)) * .5
         YHCS = (ADATA(IPTR+13) + ADATA(IPTR+6)) * .5
         XX    =  ADATA(IPTR+12) - ADATA(IPTR+5)
         YY    =  ADATA(IPTR+13) - ADATA(IPTR+6)
         RR    = SQRT(XX**2+YY**2)
         IF RR.LT.10.
         THEN
            IPRES=IPCO
            RETURN
         CIF
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
C   FOLLOWING PARAMETERS ARE USED FOR CORRECTIONS ONLY
         IF(ABS(CURVXY*ALCS).GT..966) CURVXY=SIGN(.966/ALCS,CURVXY)
         CTGTH=TGTH
         IF(ABS(CTGTH).GT.2.) CTGTH=SIGN(2.,CTGTH)
C
         CPR0=CURVXY/SQRT(1.-(CURVXY*ALCS)**2)
         VCRS=1./CPR0
         CURN1=CURVXY*PARVD(2)
C========
C
N     ROTATION ANGLE (USING LAST POINT OF TRACK)
      SELECT INDX
      CASE 1
        XT = 0.
        YT = 0.
        CSROT = 1.
        SNROT = 0.
        XOT   = 0.
      CASE 2
        XT    = XHCS
        YT    = YHCS
        XOT   = 0.
      CASE 3
        XT    = (ADATA(IPTR+12) + XO) * .5
        YT    = (ADATA(IPTR+13) + YO) * .5
        XX    =  ADATA(IPTR+12) - XO
        YY    =  ADATA(IPTR+13) - YO
        RR    = SQRT(XX**2+YY**2)
        CSROT = XX / RR
        SNROT = YY / RR
C++
        XOT   = -.5*RR
      OTHER
N       ILLEGAL INDEX
        RETURN
      CSELECT
C
C
C
N     SELECT CELLS CONTAINING TRACK
C
      IPC0 = IPTR + 34
      IPC9 = IPC0 +  5
      FOR IPC = IPC0,IPC9
         JCELL = IDATA(IPC)
         IF JCELL.GT. 0 .AND. JCELL.LE.96
         THEN
            JRING = 1
            IF(JCELL.GT.24) JRING = 2
            IF(JCELL.GT.48) JRING = 3
            PERFORM FETCH
         CIF
      CFOR
C
C FETCH Z-CHAMBER HITS IN CASE Z-S FITS (INDEX=4)
      IF INDEX.EQ.4 .AND. ZCHWW.GT..1 .AND. ZCHWW.LT.2000.
      THEN
         CALL ZCFTNW(NRUN,NEVT,ITRK,TGTH,ZVERT,NZHIT,IZCHMB,AZCHMB)
         IF NZHIT.GT.0
         THEN
            FOR J=1,NZHIT
               HRESAR( 1) = 100+IZCHMB(1,J)
               HRESAR( 2) = IZCHMB(2,J)
               HRESAR( 3) = 0
               HRESAR( 4) = 0
               HRESAR( 5) = 1
C              HRESAR( 6) = IP-2*IPJETC
               HRESAR( 6) = 101
               XX=AZCHMB(1,J)
               YY=AZCHMB(2,J)
               ZZ=AZCHMB(3,J)
               RESAR ( 4) = XX
               RESAR ( 5) = YY
               RESAR ( 6) = ZZ
C CALCULATE TRACK LENGTH IN R-PHI FROM FIRST POINT ON TRACK
               UX=XX-ADATA(IPTR+5)
               UY=YY-ADATA(IPTR+6)
               UU=SQRT(UX**2+UY**2)
            IF(ABS(CURVXY*UU).GT.1.E-5) UU=2.*SIN(.5*CURVXY*UU)/CURVXY
               IF(UX*ADATA(IPTR+8)+UY*ADATA(IPTR+9).LT.0.) UU=-UU
               RESAR ( 7) = UU
C              RESAR ( 8) = WW
               RESAR ( 8) = ZCHWW
               IF(NRUN.LT.24200) RESAR(8)=RESAR(8)*.6
               CALL MVC(WRK(IPCO),0,RESAR(1),0,LHBIT)
               IPCO = IPCO + LHIT
            CFOR
         CIF
      CIF
C
N     STORE RESULTS
      IPRES = IPCO
      IF INDEX.LT.4
      THEN
         WRK (IPRES   ) = XT
         WRK (IPRES+ 1) = YT
         WRK (IPRES+ 2) = CSROT
         WRK (IPRES+ 3) = SNROT
         WRK (IPRES+ 9) = XOT
         WRK (IPRES+10) = 0.
         WRK (IPRES+11) = CSTH
         WRK (IPRES+12) = SNTH
      CIF
C
      RETURN
C
N     *************************
N     *      F E T C H        *
N     *************************
C
C
N     FETCH HITS IN CELL
      PROC FETCH
C
C+++++++
         KRING=JRING
         IF(JRING.EQ.3 .AND. JCELL-(JCELL/2)*2.NE.1) KRING=4
         IF JCELL.LE.24
         THEN
            ISEG=JCELL
         ELSE
            IF JCELL.LE.48
            THEN
               ISEG=JCELL-24
            ELSE
               ISEG=(JCELL-47)/2
            CIF
         CIF
         FISEGM=((ISEG-1)*4+2)*ANG375
N    MIDDLE OF CELL WITHIN SEGMENT
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
C=========
C
N       COUNTER FOR NUMBER OF HITS FOUND
        JHIT = 0
        NHIT   = 0
        NHGOOD = 0
N       PRESET LAST LAYER
        ILAYL =-99
N       LOOP OVER ALL HITS OF CELL
        IPCO = IPCO - LHIT
        IPCLL  = 2*IPJETC + 2 + JCELL
        IP     = HDATA(IPCLL  ) + IP0
        IP9    = HDATA(IPCLL+1) + IP0
        IPHL   = IPJHTL + 2 + HDATA(IPCLL)/4
        WHILE IP.LT.IP9
C
N         CHECK TRACK # OF HIT LABEL
          LB   = IDATA(IPHL)
          ITR1 = LAND(SHFTR(LB,17),127)
          ITR2 = LAND(SHFTR(LB, 1),127)
          IF ITR1.EQ.ITRK .OR. ITR2.EQ.ITRK
          THEN
C
N           SET LBGOOD = 2 IF HIT ASSOCIATED WITH 2 TRACKS
            L0GOOD = 0
            IF(ITR1.NE.0 .AND. ITR2.NE.0) L0GOOD = 2
C
N           L/R FROM HIT LABEL
            LBLR = 0
            IF(ITR1.EQ.ITRK) LBLR = LAND(LB,MKLRT1)
            IF(ITR2.EQ.ITRK) LBLR = LAND(LB,MKLRT2)
            LBSIDE =-1
            IF(LBLR.NE.0) LBSIDE = 1
            LBLR = LBSIDE
C
            IWIR = HDATA(IP)
            IWIR = SHFTR(IWIR,3)
N           LAYER NUMBER WITHIN RING 3
            ILAY = LAND(IWIR,15)
N           AMPLITUDES
            IAMPL = HDATA(IP+1)
            IAMPR = HDATA(IP+2)
N           CALCULATE Z COORDINATE
C           IF IAMPR.LE.0.OR.IAMPL.LE.0
C           THEN
C             ZZ     = 0.
C             LZGOOD = 16
C           ELSE
C             ZZ = IAMPR + IAMPL
C             ZZ = FLOAT(IAMPR-IAMPL) * ZAL*.5 / ZZ
C             LZGOOD = 0
C             IF(ABS(ZZ).GT.1250.) LZGOOD = 16
C           CIF
            CALL AMPS2Z( IP,IPJETC,ZZ,WW,LZGOOD)
C
C+++++++
N     WIRE NUMBER WITHIN CELL 1..16
            IW=ILAY+1
            IODD=1
            IF(IW-(IW/2)*2.EQ.0) IODD=-1
            RHIT=ACEL1+(IW-8.5)*WIRDIS
            FLPATH=2.*SIN(.5*RHIT*CURVXY)/CURVXY
            ZHIT=FLPATH*TGTH+ZVERT
            IF(ABS(ZHIT).GT.1200.) ZHIT=SIGN(1200.,ZHIT)
            FLPATH=SQRT(FLPATH**2+ZHIT**2)
C
N     DRIFT TIME (FROM RAW BANK) + CORRECTIONS
            TDRIFT=HDATA(IP+3+IPRAW2)
            IF NRUN.LT.24200
            THEN
               TDRIFT=TDRIFT*64.+32.
               IF(NRUN.GE.19050.AND.NRUN.LE.20274) TDRIFT=TDRIFT+20.
               IF(NRUN.GE. 3300.AND.NRUN.LE. 3550) TDRIFT=TDRIFT-90.
            ELSE
               IF NRUN.LE.24698
               THEN
                  TDRIFT=TDRIFT-5.
                  IF(NRUN.LT.24405) TDRIFT=TDRIFT+153.
                  IF(NRUN.GE.24227.AND.NRUN.LE.24232) TDRIFT=TDRIFT+147.
                  IF(NRUN.GE.24233.AND.NRUN.LE.24245) TDRIFT=TDRIFT+297.
               CIF
            CIF
            AMRAWL=HDATA(IP+1+IPRAW2)*8.
            AMRAWR=HDATA(IP+2+IPRAW2)*8.
N     SLEWING CORRECTION
            PERFORM SLWCOR
N     CLOCK FREQUENCY
            IF(JRING.EQ.3) TDRIFT=TDRIFT*FREQR
N     FLIGHT AND PROPAGATION TIME
            BKGS=ABS(HDATA(IPHEAD+30)*.001)
            IF(BKGS.LT.3.) BKGS=4.8
            AMOMGV=.02998E-3*BKGS*SQRT(1.+CTGTH**2)/CURVXY
            CPERV=SQRT(1.+(AFLMAS/AMOMGV)**2)
            TDRIFT=TDRIFT-FLTIM1*FLPATH*CPERV-FLTIM2*(FLTIM3-ABS(ZHIT))
N     T0
            TDRIFT=TDRIFT-CALCST(JCELL,IW)
N     * CALCULATE "WIRE NUMBER CORRECTION"
            TSTG=CALCST(JCELL,17)*(1.-ELFRCZ*(ZHIT/1200.)**2)
     +      *AVFRMX*SINLOR*PARVD(JRING)/WIRDIS
            XK=IW-IODD*TSTG-8.5
            ROT3X=XK*WIRDIS*(ROT1X-ROT1Y*XK*OCEL2)
            ROT3Y=XK*WIRDIS*(ROT1Y+ROT1X*XK*OCEL2)
N     * WIRE IN THE "CIRCLE" SYSTEM
            XWPR=ROT3X+BCS1XC+XK*( ACEL2*ROT2Y+BCEL2*ROT2X)
            YWPR=ROT3Y+BCS1YC+XK*(-ACEL2*ROT2X+BCEL2*ROT2Y)
C========
N           CHECK IF LEFT + RIGHT SOLUTION POSSIBLE
            NLRSOL = 1
C++
            IF(TDRIFT.LT.300.) NLRSOL = 2
C
N           LOOP OVER LEFT +/OR RIGHT SOLUTION
            ILRSOL = 0
            REPEAT
            ILRSOL = ILRSOL + 1
            LBGOOD = L0GOOD
C
N             SELECT SIDE
              IF NLRSOL.EQ.1 .AND. LBSIDE.LT.0  .OR.
     ?           NLRSOL.EQ.2 .AND. ILRSOL.EQ.1
              THEN
N               LEFT SIDE
                LBSIDE =-1
C++
                PERFORM GETCOR
              ELSE
N               RIGHT SIDE
                LBSIDE = 1
C++
                PERFORM GETCOR
              CIF
C
N             HIT QUALITY:
              IF(LBSIDE.NE.LBLR) LBGOOD = LBGOOD + 1
N             NEW LAYER?
              IF ILAY.NE.ILAYL .OR. LBGDL.LE.1.AND.LBGOOD.LE.2
              THEN
                LBREG = 1
N               INCREASE HIT COUNTER
                JHIT = JHIT + 1
                IPCO = IPCO + LHIT
              ELSE
N               2 HITS IN SAME LAYER, SELECT CLOSEST
                LBREG = 0
                ZWZ = WRK(IPCO+10)
                IF(LBGOOD.LT.IZW) LBREG = 1
              CIF
N             REGISTER NEW HIT?
              IF LBREG.NE.0
              THEN
                NHIT   = NHIT   + 1
                IF(LBGOOD.LE.2) NHGOOD = NHGOOD + 1
                IF INDEX.NE.4
                THEN
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
C CALCULATE TRACK LENGTH IN R-PHI FROM FIRST POINT ON TRACK
                   UX=XX-ADATA(IPTR+5)
                   UY=YY-ADATA(IPTR+6)
                   UU=SQRT(UX**2+UY**2)
            IF(ABS(CURVXY*UU).GT.1.E-5) UU=2.*SIN(.5*CURVXY*UU)/CURVXY
                   IF(UX*ADATA(IPTR+8)+UY*ADATA(IPTR+9).LT.0.) UU=-UU
                   RESAR ( 7) = UU
                   RESAR ( 8) = WW
                CIF
                CALL MVC(WRK(IPCO),0,RESAR(1),0,LHBIT)
                ILAYL = ILAY
                LBGDL = LBGOOD
              CIF
C
            UNTIL ILRSOL.GE.NLRSOL
C
          CIF
C
        IPHL = IPHL + 1
        IP   = IP   + 4
        CWHILE
N       SET IPCO TO 1. FREE LOCATION
        IPCO = IPCO + LHIT
C
      CPROC
C
N     *************************
N     *      G E T C O R      *
N     *************************
C
      PROC GETCOR
         IF LBSIDE.LT.0
         THEN
            AG2= CALCST(JCELL,26)
            VDP=-CALCST(JCELL,24)
         ELSE
            AG2= CALCST(JCELL,27)
            VDP= CALCST(JCELL,25)
         CIF
         AG2=AG2-FIIC-XK*OCEL2
         SINAG2=SIN(AG2)
         COSAG2=COS(AG2)
C CALCULATE DELTA=SIGNED CHANGE OF DRIFT TIME PRO WIRE SPACING
         F=XWPR*SINAG2+(YWPR+VCRS)*COSAG2
         G=(ALCS-XWPR)*(ALCS+XWPR)-YWPR*(YWPR+2.*VCRS)
         IF G.GT.-.98*F**2
         THEN
N     * WIRE CIRCLE DISTANCE ALONG DRIFT DIR.
            PERFORM CALSQT
            DISTWC=SQTVAL
            XPR=XWPR+DISTWC*SINAG2
            DYPDXP=1.-(CURVXY*XPR)**2
            IF DYPDXP.LT..02
            THEN
               IF KIMPRT.LT.LIMPRT
               THEN
                  KIMPRT=KIMPRT+1
                  PRINT 675,CURVXY,ALCS,XPR,NRUN,NEVT,ITRK
675      FORMAT(' *** ERROR IN JFTNEW *** CURVATURE, HALF TRACK LENGTH',
     +   ' X IN TR C.S.',/,8X,3E15.5,'   TRACK',I9,I6,I4)
C
                  PRINT 676, CPR0,VCRS,XHCS,YHCS,FIC,TGTH,
     +            XWPR,YWPR,AG2,F,G,DISTWC,DYPDXP,
     +            KRING,JCELL,ISEG,FISEGM,IW,IODD,ILRSOL,LBSIDE
676               FORMAT(/,' CPR0,VCRS,XHCS,YHCS,FIC,TGTH,',/,
     +            ' XWPR,YWPR,AG2,F,G,DISTWC,DYPDXP,'/,
     +            ' KRING,JCELL,ISEG,FISEGM,IW,IODD,ILRSOL,LBSIDE',/,
     +            1X,6E15.5,/,1X,7E15.5,/,1X,3I4,E15.5,4I6,////)
               CIF
               DYPDXP=.02
            CIF
            DYPDXP=-XPR*CURVXY/SQRT(DYPDXP)
            C=1.-DYPDXP*SINAG2/COSAG2
            IF(ABS(C).LT..001) C=SIGN(.001,C)
            TANBET=(DYPDXP+SINAG2/COSAG2)/C
            AMU=WIRDIS/PARVD(JRING)*COSLOR
            DELTA=AMU*(TANBET-TANLOR)
            IF(ABS(DELTA).GT.1800.) DELTA=SIGN(1800.,DELTA)
         ELSE
C           HIT CAN NOT BE ON THE TRACK. FOR ANGLE DEPENDENT
C           CORRECTIONS A TRACK PARALELL WITH THE WIRE PLANE
C           WILL BE ASSUMED
            IF KIMPRT.LT.LIMPRT
            THEN
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
            CIF
            TANBET=TANLOR
            DELTA=0.
         CIF
C
N     *  Z AND THETA DEPENDENT SLEWING
         PERFORM SLWZTH
N     * CLOSE WIRE CORRECTION
         PERFORM CLWCOR
N     * CORRECT FOR STAGGERING AND TRACK ANGLE FI
         PERFORM STGANG
C
N     * DISTANCE FROM WIRE
         Y1=VDP*TCORR
N     * DISTORTIONS
         IF(JESDRW.GT.0) PERFORM DSTRTN
         DSC=ABS(Y1)
         XX=XWPR+Y1*SINAG2
         YY=YWPR+Y1*COSAG2
         IF INDX.NE.2
         THEN
            A= XX*COSFIC-YY*SINFIC+XHCS
            YY=XX*SINFIC+YY*COSFIC+YHCS
            XX=A
            IF INDX.EQ.3
            THEN
               A = (XX-XT)*CSROT+(YY-YT)*SNROT
               YY=-(XX-XT)*SNROT+(YY-YT)*CSROT
               XX=A
            CIF
         CIF
      CPROC
C
N     *************************
N     *      S L W C O R      *
N     *************************
C
      PROC SLWCOR
      A=AMRAWL
      IF(AMRAWR.GT.A) A=AMRAWR
      IF(A.LT.10.) A=10.
N    * SLEWING FOR RAW AMPLITUDES
      IF NRUN.GE. 24200
      THEN
         IF A.GT.1800.
         THEN
            TSLEW=-1.449+1.19097E-3*(A-2000.)
         ELSE
            TSLEW=-2.100+2.11521E-3*(A-1600.)-8.50349E-12*(1600.-A)**4
         CIF
      ELSE
         IF A.GT.5000.
         THEN
            TSLEW=-50.+5.80000E-3*(A-5000.)
         ELSE
            IF A.LT.300.
            THEN
               TSLEW=-200.
            ELSE
         TSLEW=-4472.05*A**(-5.23557E-1-6.42692E-3*(ALOG(A)-7.77529)**2)
               IF(NRUN.GE.20275.AND.A.LT.1500.)
     +         TSLEW=TSLEW-(A-1500.)**2*2.26664E-5
            CIF
            IF(A.LT.650. .AND.(NRUN.GE.20275 .OR.
     +      NRUN.GE.13000 .AND. NRUN.LE. 14599) )
     +      TSLEW=TSLEW+116.6-1.79687E-1*A
            IF(A.LT.800. .AND. NRUN.GE.11473 .AND. NRUN.LE.12554)
     +      TSLEW=TSLEW+139.4-1.74800E-1*A
         CIF
      CIF
      TDRIFT=TDRIFT+TSLEW
      CPROC
C
N     *************************
N     *      S L W Z T H      *
N     *************************
C
      PROC SLWZTH
      ACTG=ABS(CTGTH)
      IF NRUN.GE. 24200
      THEN
         ZTSLW=0.
         IF KRING.EQ.4
         THEN
            IF LBSIDE.LT.0
            THEN
               ZTSLW=-19.43-14.5942*ACTG+19.8951*ACTG**2
               IF ACTG.LT..42
               THEN
                  ZTSLW=ZTSLW+5.1921+3.216*ACTG-82.49*ACTG**2
               ELSE
                  ZTSLW=ZTSLW-24.66+48.9578*ACTG-22.7265*ACTG**2
               CIF
            ELSE
               ZTSLW=5.918-5.45559*ACTG-2.12*ACTG**2
            CIF
         CIF
         IF KRING.EQ.3
         THEN
            IF LBSIDE.LT.0
            THEN
               ZTSLW=-.937-8.66313*ACTG+9.8988*ACTG**2
            ELSE
               ZTSLW= 2.46- 3.8375*ACTG-14.5671*ACTG**2
            CIF
         CIF
         J=2*KRING
         IF(LBSIDE.LT.0) J=J-1
         IF J.EQ.6.OR.J.EQ.7
         THEN
            AZ=ABS(ZHIT)
            BZ=ACTG*GGF(4,J)
            ZTSLW=ZTSLW+(AZ-BZ)*(GGF(1,J)+GGF(2,J)*(AZ+BZ)+GGF(3,J)
     +      *(AZ*(AZ+BZ)+BZ**2))
         CIF
      ELSE
         IF KRING.EQ.1
         THEN
            ZTSLW=13.46-14.03*ACTG
         ELSE
            IF KRING.EQ.2
            THEN
               ZTSLW=15.23-31.278*ACTG+7.54731*ACTG**2
            ELSE
               ZTSLW=20.86-48.672*ACTG+13.663*ACTG**2
            CIF
         CIF
C
         IF KRING.EQ.1
         THEN
            IF LBSIDE.LT.0
            THEN
               IF ACTG.LT..37
               THEN
                  T1=10.30-26.1*ACTG
               ELSE
                  T1=3.88-18.5345*ABS(ACTG-.5427)
               CIF
               T1=T1-0.30+ 0.77*ACTG-0.7648*ACTG**2
            ELSE
               IF ACTG.LT..37
               THEN
                  T1=7.50-26.3*ACTG
               ELSE
                  T1=-1.95+6.84118*ABS(ACTG-.4)
               CIF
               T1=T1+3.21-14.10*ACTG+10.73*ACTG**2
            CIF
            ZTSLW=ZTSLW+T1
         CIF
         IF KRING.EQ.2
         THEN
            IF LBSIDE.LT.0
            THEN
               IF ACTG.LT..40
               THEN
                  T1=8.787-19.675*ACTG
               ELSE
                  T1=4.091-18.133*ABS(ACTG-.56)
               CIF
            ELSE
               IF ACTG.LT..48
               THEN
                  T1=1.983-12.667*ACTG
               ELSE
                  T1=-4.1125+13.574*ABS(ACTG-.5)
               CIF
            CIF
            ZTSLW=ZTSLW+T1
         CIF
         IF KRING.EQ.3
         THEN
            T1=8.336-12.3519*ACTG
            IF LBSIDE.LT.0
            THEN
               T1=T1-2.68+18.09*ACTG-15.95*ACTG**2
            ELSE
               T1=T1+1.20-14.489*ACTG+14.623*ACTG**2
               IF(NRUN.GE.8712.AND.NRUN.LE.9999)
     +         T1=T1+14.20-8.5415*ACTG-10.6000*ACTG**2
               IF(NRUN.GE.7592.AND.NRUN.LE.8711)
     +         T1=T1+4.16+17.97*ACTG-24.33*ACTG**2
            CIF
            ZTSLW=ZTSLW+T1
         CIF
         IF KRING.EQ.4
         THEN
            IF ACTG.LT..56
            THEN
               T1=5.58-15.183*ACTG
            ELSE
               T1=-3.30+8.1613*(ACTG-.5)
            CIF
            IF LBSIDE.LT.0
            THEN
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
            CIF
            ZTSLW=ZTSLW+T1
         CIF
C
         J=2*KRING
         IF(LBSIDE.LT.0) J=J-1
         AZ=ABS(ZHIT)
         BZ=ACTG*GG(4,J)
         ZTSLW=ZTSLW+(AZ-BZ)*(GG(1,J)+GG(2,J)*(AZ+BZ)+GG(3,J)
     +   *(AZ*(AZ+BZ)+BZ**2))
         IF KRING.GE.3
         THEN
            ZTSLW=ZTSLW+1.70E-2/.6727*(ZHIT-CTGTH*GG(4,J))
         CIF
C
         IF(KRING.GE.3) ZTSLW=ZTSLW*FREQR
      CIF
      TCORR=TDRIFT+ZTSLW
      CPROC
C
N     *************************
N     *      C L W C O R      *
N     *************************
C
      PROC CLWCOR
C     APPLY CLOSE WIRE CORRECTION
      IF TCORR.LT.THU(4)
      THEN
         TCOR=TCORR
         IF TCOR.GT.THL(4)
         THEN
            TCOR=TCOR+A4(1)+A4(3)*(TCOR-A4(2))**2+A4(4)*(TCOR-A4(2))**4
         ELSE
            IF TCOR.GT.THL(3)
            THEN
               TCOR=TCOR+A3(1)+A3(3)*(TCOR-A3(2))**2
     +         +A3(4)*(TCOR-A3(2))**4
            ELSE
               IF TCOR.GT.THL(2)
               THEN
                  TCOR=TCOR-A2(1)+A2(2)*((TCOR-A2(3))**4-
     +            (A2(1)-A2(3))**4)
               ELSE
                  TCOR=TCOR-A2(1)
               CIF
            CIF
         CIF
         IF(TCOR.GT.0..AND.TCOR.LT.120.) TCOR=TCOR-8.E-2*(TCOR-120.)
         TCORR=TCOR
      CIF
      CPROC
C
N     *************************
N     *      S T G A N G      *
N     *************************
C
      PROC STGANG
         PERFORM STGFIZ
         STGTC=CALCST(JCELL,17)*IODD*LBSIDE*STGCOR
C
         IF DELTA.GT.B1(3,KRING)
         THEN
            A12=B1(2,KRING)
         ELSE
            A12=B1(1,KRING)
         CIF
         CSGINV=SQRT(1.+((DELTA-B1(3,KRING))/B1(4,KRING))**2)
         IF TCORR.GT.A12
         THEN
C           Y=A12*CURN1*LBSIDE
C           IF ABS(Y).GT.1.E-5
C           THEN
C!!            TANGCC=A12*(-SQTVAL(1./(Y*CSGINV),1.-2./Y,1.,1.E-4)-1.)
C           ELSE
               TANGCC=A12*(CSGINV-1.)
C           CIF
         ELSE
C           Y=TCORR*CURN1*LBSIDE
C           IF ABS(Y).GT.1.E-5
C           THEN
C !!           TANGCC=TCORR*(SQTVAL(1./(Y*CSGINV),1.-2./Y,1.,1.E-4)+1.)
C           ELSE
               TANGCC=TCORR*(CSGINV-1.)
C           CIF
         CIF
         TCORR=TCORR+TANGCC+STGTC
      CPROC
C
N     *************************
N     *      S T G F I Z      *
N     *************************
C
      PROC STGFIZ
C
      D=DELTA
      Z=ZHIT
      PERFORM STGZ0
      PERFORM FRACT
      U=(Z/1200.)**2
      STGCOR=STGDZ0*(1.+U*FRACZD)
      IF(STGCOR.LT..2) STGCOR=.2
      CPROC
C-----------------------------------------------------------------------
      PROC STGZ0
N     * CALCULATE D DEPENDENT STAG FRAC AT Z=0
         IF D.LT.-600.
         THEN
            I=1
         ELSE
            IF D.LT.0.
            THEN
               I=2
            ELSE
               IF D.LT.600.
               THEN
                  I=3
               ELSE
                  I=4
               CIF
            CIF
         CIF
         STGDZ0=Q(1,I)+ABS(D)*(Q(2,I)+Q(4,I)*D**2)+Q(3,I)*D**2
     +   +Q(5,I)*D**4
         STGDZ0=STGDZ0/44.9444
      CPROC
C-----------------------------------------------------------------------
      PROC FRACT
N     * CALCULATE D DEPENDENT EL.STAT.FRACTION
         IF D.LT.-500.
         THEN
            I=1
         ELSE
            IF D.LT.0.
            THEN
               I=2
            ELSE
               IF D.LT.400.
               THEN
                  I=3
               ELSE
                  I=4
               CIF
            CIF
         CIF
         FRACZD=P(1,I)+ABS(D)*(P(2,I)+P(4,I)*D**2)+P(3,I)*D**2
     +   +P(5,I)*D**4
      CPROC
C
N     *************************
N     *      D S T R T N      *
N     *************************
C
      PROC DSTRTN
C
      IWR=(KRING-1)*16+IW
      IF Y1.GT.0.
      THEN
         IND=2
      ELSE
         IND=1
      CIF
      SMAX=SMAXW(IND,IWR)
      SM0=SM01(IND,IWR)
      Y1COR=DSTORW(2*IND,IWR,ISEG)*Y1**2    +   DSTORW(5,IWR,ISEG)
      S0=ABS(Y1)-SM0
      IF(S0.GT.0.) Y1COR=Y1COR+DSTORW(2*IND-1,IWR,ISEG)*S0**2
      IF IW.EQ.1 .OR. IW.EQ.16
      THEN
         X=ABS(Y1/SMAX)
         IF IWR.EQ.1
         THEN
            IF IND.EQ.2
            THEN
               IF X.LT..52
               THEN
                  T=-.07*(1.-((2.*X-.52)/.52)**2)
               ELSE
                  IF X.LT.1.
                  THEN
                     T=.05*(1.-((2.*X-1.37)/.33)**2)
                  ELSE
                     T=-.13
                  CIF
               CIF
            ELSE
               IF X.LT..52
               THEN
                  T=-.05*(1.-((2.*X-.52)/.52)**2)
               ELSE
                  IF X.LT.1.
                  THEN
                     T=.035*(1.-((2.*X-1.34)/.30)**2)
                  ELSE
                     T=-.13
                  CIF
               CIF
            CIF
         CIF
         IF IWR.EQ.16
         THEN
            IF IND.EQ.2
            THEN
               IF X.LT..42
               THEN
                  T=.075*(1.-((2.*X-.42)/.42)**2)
               ELSE
                  IF X.LT..67
                  THEN
                     T=-.06*(1.-((2.*X-1.09)/.25)**2)
                  ELSE
                     IF X.LT..9
                     THEN
                        T= .06*(1.-((2.*X-1.53)/.19)**2)
                     ELSE
                        T=-.080
                     CIF
                  CIF
               CIF
            ELSE
               IF X.LT..50
               THEN
                  T= .05*(1.-((2.*X-.50)/.50)**2)
               ELSE
                  IF X.LT..75
                  THEN
                     T=-.02*(1.-((2.*X-1.25)/.25)**2)
                  ELSE
                     IF X.LT.1.
                     THEN
                        T=.025*(1.-((2.*X-1.70)/.20)**2)
                     ELSE
                        T=-.03
                     CIF
                  CIF
               CIF
            CIF
         CIF
         IF IWR.EQ.17
         THEN
            IF IND.EQ.2
            THEN
               IF X.LT..40
               THEN
                  T=-.085*(1.-((2.*X-.40)/.40)**2)
               ELSE
                  IF X.LT..62
                  THEN
                     T= .05*(1.-((2.*X-1.02)/.22)**2)
                  ELSE
                     IF X.LT.1.
                     THEN
                        T=-.04*(1.-((2.*X-1.47)/.23)**2)
                     ELSE
                        T= .170
                     CIF
                  CIF
               CIF
            ELSE
               IF X.LT..37
               THEN
                  T=-.10*(1.-((2.*X-.37)/.37)**2)
               ELSE
                  IF X.LT..60
                  THEN
                     T= .06*(1.-((2.*X- .97)/.23)**2)
                  ELSE
                     IF X.LT..72
                     THEN
                        T=-.03*(1.-((2.*X-1.32)/.12)**2)
                     ELSE
                        IF X.LT..9
                        THEN
                           T= .03*(1.-((2.*X-1.58)/.14)**2)
                        ELSE
                           T=-.07
                        CIF
                     CIF
                  CIF
               CIF
            CIF
         CIF
         IF IWR.EQ.32
         THEN
            IF IND.EQ.2
            THEN
               IF X.LT..27
               THEN
                  T=.120*(1.-((2.*X-.27)/.27)**2)
               ELSE
                  IF X.LT..46
                  THEN
                     T=-.08*(1.-((2.*X- .73)/.19)**2)
                  ELSE
                     IF X.LT..64
                     THEN
                        T= .055*(1.-((2.*X-1.10)/.18)**2)
                     ELSE
                        T=0.
                     CIF
                  CIF
               CIF
            ELSE
               IF X.LT..43
               THEN
                  T= .05*(1.-((2.*X-.43)/.43)**2)
               ELSE
                  IF X.LT..67
                  THEN
                     T=-.025*(1.-((2.*X-1.10)/.24)**2)
                  ELSE
                     IF X.LT.1.
                     THEN
                        T=.020*(1.-((2.*X-1.55)/.21)**2)
                     ELSE
                        T=-.070
                     CIF
                  CIF
               CIF
            CIF
         CIF
         IF IWR.EQ.33
         THEN
            IF IND.EQ.2
            THEN
               IF X.LT..42
               THEN
                  T=-.09*(1.-((2.*X-.42)/.42)**2)
               ELSE
                  IF X.LT..68
                  THEN
                     T= .06*(1.-((2.*X-1.10)/.26)**2)
                  ELSE
                     IF X.LT..95
                     THEN
                        T=-.055*(1.-((2.*X-1.54)/.18)**2)
                     ELSE
                        T=.170
                     CIF
                  CIF
               CIF
            ELSE
               IF X.LT..44
               THEN
                  T=-.11*(1.-((2.*X-.44)/.44)**2)
               ELSE
                  IF X.LT..68
                  THEN
                     T= .075*(1.-((2.*X-1.12)/.24)**2)
                  ELSE
                     IF X.LT..9
                     THEN
                        T=-.05*(1.-((2.*X-1.53)/.17)**2)
                     ELSE
                        T= .080
                     CIF
                  CIF
               CIF
            CIF
         CIF
         IF IWR.EQ.48
         THEN
            IF IND.EQ.2
            THEN
               IF X.LT..34
               THEN
                  T= .08*(1.-((2.*X-.34)/.34)**2)
               ELSE
                  IF X.LT..85
                  THEN
                     T=-.035*(1.-((2.*X- .99)/.31)**2)
                  ELSE
                     T=.150
                  CIF
               CIF
            ELSE
               IF X.LT..30
               THEN
                  T=.035*(1.-((2.*X-.30)/.30)**2)
               ELSE
                  T=-.035*(1.-((2.*X-1.10)/.50)**2)
               CIF
            CIF
         CIF
         IF IWR.EQ.49
         THEN
            IF IND.EQ.2
            THEN
               IF X.LT..42
               THEN
                  T=-.08*(1.-((2.*X-.42)/.42)**2)
               ELSE
                  IF X.LT..70
                  THEN
                     T=.018*(1.-((2.*X-1.07)/.23)**2)
                  ELSE
                     T=.035
                  CIF
               CIF
            ELSE
               IF X.LT..50
               THEN
                  T=-.09*(1.-((2.*X-.50)/.50)**2)
               ELSE
                  IF X.LT..85
                  THEN
                     T= .080*(1.-((2.*X-1.30)/.30)**2)
                  ELSE
                     T=-.060
                  CIF
               CIF
            CIF
         CIF
         IF IWR.EQ.64
         THEN
            IF IND.EQ.2
            THEN
               IF X.LT..35
               THEN
                  T= .09*(1.-((2.*X-.35)/.35)**2)
               ELSE
                  IF X.LT..64
                  THEN
                     T=-.07*(1.-((2.*X- .99)/.29)**2)
                  ELSE
                     IF X.LT..85
                     THEN
                        T= .05*(1.-((2.*X-1.44)/.16)**2)
                     ELSE
                        T=-.09
                     CIF
                  CIF
               CIF
            ELSE
               IF X.LT..40
               THEN
                  T= .09*(1.-((2.*X-.40)/.40)**2)
               ELSE
                  T=0.
               CIF
            CIF
         CIF
         Y1COR=Y1COR+T
      CIF
      Y1=Y1+Y1COR
      CPROC
C
CCCCCCCCCCCCCCCC
C     FUNCTION SQTVAL(F,G,AL,EPS)
C
C     CALCULATE SQTVAL=F*(SQRT(1+G*AL**2/F**2)-1).
C     TO ACHIEVE GOOD PRECISION, FOR LARGE F THE TAYLOR EXPANSION
C     IS USED UPTO AT MOST 15 TERMS
C     EPS IS THE REQUIRED ABSOLUTE PRECISION
C
      PROC CALSQT
      S=G/F
      U=-S/F
      S=-.5*S
      IF ABS(U).GT..3
      THEN
         IF U.LT..98
         THEN
            SQTVAL=F*(SQRT(1.-U)-1.)
         ELSE
            SQTVAL=0.
C           PRINT 100,F,G,AL
C100        FORMAT(1X,' SQTVAL',3E16.7)
         CIF
      ELSE
         VAL=-S*(1.+.25*U+.125*U**2)
         QQ=S*U**3/12.8
         N=5
         WHILE ABS(QQ).GT.EPS .AND.N.LT.15
           VAL=VAL-QQ
           QQ=QQ*U*(1.-1.5/N)
           N=N+1
         CWHILE
         SQTVAL=VAL
      CIF
      CPROC
      END
C   20/08/82 306161129  MEMBER NAME  REFITV   (JADEGS)      SHELTRAN
C   13/08/82 208201533  MEMBER NAME  ORREFITV (FITSR)       SHELTRAN
C   18/02/81 208111542  MEMBER NAME  REFITV   (JETCALSR)    SHELTRAN
      SUBROUTINE REFITV(IPTR,IPJHTL,ERRFAC)
C
C        REFIT TRACK ITRK IN 'PATR'-BANK USING ORIGIN
C        P. STEFFEN                    22/08/80
C
      IMPLICIT INTEGER*2 (H)
      LOGICAL DEADCL
C
#include "cdata.for"
C
#include "cgeo1.for"
C
#include "calibr.for"
C
#include "cworkpr.for"
#include "cworkeq.for"
C
      EQUIVALENCE
     ,          (HPCO0 ,HPWRK(17)),(HPCO9 ,HPWRK(18)),(HLDCO ,HPWRK(19))
     ,         ,(ICELL ,IDWRK( 1)),(MHIT  ,IDWRK( 2)),(IRING ,IDWRK( 3))
     ,         ,(IERRCD,IDWRK( 4)),(NTRKEL,IDWRK( 5))
C
#include "cpatlm.for"
C
#include "cjdrch.for"
#include "cdsmax.for"
C
      INTEGER DATE(5), IDAY /0/
      DIMENSION ITRCLL(6), NCNCK(24), NHTRNG(3)
C
N     JET-CHAMBER AND VERTEX RESOLUTION
      DATA RESJ0 /.200/, RESV0 /.300/
C
N     MASK FOR L/R BIT IN HIT LABEL
      INTEGER MKLRT1 /Z1000000/, MKLRT2 /Z100/
C
N     MASK FOR TRACKS AT CELL WALL
      INTEGER MKBDCL(3) /Z10,Z20,Z40/
      INTEGER MKDDCL(3) /Z01,Z02,Z04/
C
C     IF(IDATA(IPTR+1).LT. 4) RETURN
C     I0 = IPTR + 1
C     I9 = IPTR + 48
C     PRINT 2001, (IDATA(I1),I1=I0,I9)
C     I0 = IPJHTL*2 + 1
C     I9 = I0 + IDATA(IPJHTL)*2 - 1
C     PRINT 2000, IPJHTL,I0,I9,(HDATA(I1),I1=I0,I9)
C     IPJETC = IDATA(IBLN('JETC'))
C     I0 = IPJETC*2 + 1
C     I9 = I0 + 109
C     PRINT 2000, IPJETC,I0,I9,(HDATA(I1),I1=I0,I9)
C2000 FORMAT('0REFIT:',3I8,/,(20(1X,Z4)))
C2001 FORMAT(1H0,2I3,I8,2(I4,3F6.1,3F6.3),
C    ,     /,14X,I3,4E13.5,F6.2,I3,4E13.5,
C    ,     /,14X,I3,2F8.3,F6.1,I3,10X,6I3,8I6,2X,Z4)
C2002 FORMAT('0FETCH:',2I3,2I5,12F9.5)
C2003 FORMAT('0ROTATION:',12F10.5)
C2004 FORMAT('0CIRC.CENTRE:',2I3, F10.5,2F10.0,F8.1,2F8.1)
C2005 FORMAT('0TRACK:',I6,/,(1X,3I6,4F8.1,I4,F6.2,2I4,F8.3,I6,F8.1))
C2006 FORMAT(1X,I6,5F8.2,F12.1,5F8.2)
C2007 FORMAT(' FETCH:',I3,9F8.4,F10.5,F6.0)
C2008 FORMAT(' FIT:',2I3,F8.2,F5.0,F10.6,F7.3,F5.1,F6.3,F5.1)
C2009 FORMAT(' JHTL:',I8,1X,Z8,3I5)
C2010 FORMAT(' HIT:',I6,12F8.2)
C2011 FORMAT('0ABERR:',10F10.6)
C2012 FORMAT('0ERROR:',10E13.6)
C2014 FORMAT('0FIT-BANK:',5F8.3,5X,5F8.3,5X,F8.5,2F8.1)
C2016 FORMAT('0ITRCLL =',6I8,/,(9X,6F8.3))
C2107 FORMAT(' SIGLM:',10F8.3)
C
N     INITIALIZATION
      DATA LBINIT /0/
      IF LBINIT .EQ. 0
      THEN
        LBINIT = 1
        PERFORM INIT
      CIF
C
N     RESERVE SPACE IN CWORK
      HPFREE = 1
      HPFRE1 = HPFREE
      HPCO0  = HPFREE
      HLDCO  = 14
      HPFREE = HLDCO*100 + HPCO0
      HPCO9 = HPFREE - 1
C
N     GET RUN #
      IPHEAD = IDATA(IQHEAD)*2
      NRUN = HDATA(IPHEAD+10)
C
N     COPY TRACK BANK
      HPTR0 = HPFREE
      CALL MVC(IWRK(HPTR0),0,IDATA(IPTR+1),0,192)
C
N     TRACK #
      ITRK = IDATA(IPTR+1)
C
N     CENTRE OF CIRCLE (USED FOR ANGULAR CORRECTION)
      IF IDATA(IPTR+18).EQ.1
      THEN
N       CIRCLE PARAMETERS
        ALFA  = ADATA(IPTR+21)
        CRV   = ADATA(IPTR+19)
        IF(ABS(CRV).LT.1.E-8) CRV = SIGN(1.E-8,CRV)
        RAD   =  1./CRV + ADATA(IPTR+20)
        XCIRC = COS(ALFA) * RAD
        YCIRC = SIN(ALFA) * RAD
        CHARGE = SIGN(1.,ADATA(IPTR+25))
      ELSE
N       PARABOLA PARAMETERS
        CRV   = ADATA(IPTR+22)*2.
        IF(ABS(CRV).LT.1.E-8) CRV = SIGN(1.E-8,CRV)
        ALFA  = ADATA(IPTR+19)
        XCIRC =-SIN(ALFA)/CRV + ADATA(IPTR+20)
        YCIRC = COS(ALFA)/CRV + ADATA(IPTR+21)
        CHARGE =-SIGN(1.,ADATA(IPTR+22))
      CIF
C
N     ZVERT, THETA + DIR. COSINES
      ZVERT = ADATA(IPTR+31)
      TGTH = ADATA(IPTR+30)
      CSTHI = SQRT(TGTH**2 + 1.)
      CSTH  = 1. / CSTHI
      SNTH  = CSTH * TGTH
C     PRINT 2004,ITRK,IDATA(IPTR+18),ALFA,XCIRC,YCIRC,ZVERT,TGTH,CSTHI
C
N     GET X-Y-VERTEX AND DETERMINE ERROR
      IPV    = ICALIB(10)
      XO     = ACALIB(IPV+ 1)
      YO     = ACALIB(IPV+ 3)
C     I0 = IPV + 1
C     I9 = IPV + 6
C     PRINT 2029, XO,YO,(ACALIB(I1),I1=I0,I9)
      PTRANS = ABS(0.0299792458*BKGAUS/ADATA(IPTR+25)) * .001
      RESV   = RESV0**2 + RESMS / PTRANS**2
      WGHT0  = RESJ0**2 / RESV
      F1     = ERRFAC
      IF(F1 .LT. .10) F1 = .10
      WGHT0  = WGHT0 / F1**2
C     PRINT 2029, XO,YO,WGHT0,F1,RESV,RESMS,PTRANS
C2029 FORMAT(' VERTEX',9E13.5)
C     PRINT 2011,ABERR
C
N     ROTATION ANGLE (USING LAST POINT OF TRACK)
      XT    = (ADATA(IPTR+12) + XO) * .5
      YT    = (ADATA(IPTR+13) + YO) * .5
      XX    =  ADATA(IPTR+12) - XO
      YY    =  ADATA(IPTR+13) - YO
      RR    = SQRT(XX**2+YY**2)
      CSROT = XX / RR
      SNROT = YY / RR
      XOT = XO - XT
      YOT = YO - YT
      X0   = XOT*CSROT + YOT*SNROT
      Y0   =-XOT*SNROT + YOT*CSROT
      XOR  =- XT*CSROT -  YT*SNROT
      YOR  =  XT*SNROT -  YT*CSROT
C     PRINT 2003, CSROT,SNROT,XX,YY,XT,YT,X0,Y0,XO,YO,WGHT0
C
N     FILL CELL ARRAY
      PERFORM SELCLL
C
N     LOOP OVER ALL CELLS + FETCH HITS
      KCLL = 0
      NHIT = 0
      IPCO = HPCO0
C
N     LOOP OVER RINGS
      JRING = 0
N     TRACKS AT CELL WALLS
      LBCELL = 0
      REPEAT
      JRING = JRING + 1
        NHTRNG(JRING) = 0
        NHRNG = 0
        NCLL = 0
        REPEAT
        NCLL = NCLL + 1
        KCLL = KCLL + 1
          JCELL = ITRCLL(KCLL)
          IF JCELL.NE.0
          THEN
            PERFORM FETCH
            NHRNG = NHRNG + JHIT
          CIF
        UNTIL NCLL.EQ.2
N       SET LABEL FOR TRACK AT CELL BOUND.
        IF(JCELL.NE.0) LBCELL = LOR(MKBDCL(JRING),LBCELL)
      UNTIL KCLL.EQ.6
      HPCO9 = IPCO - 1
C     PRINT 2005, LBCELL,(WRK(I),I=HPCO0,HPCO9)
C
N     1. PARABOLA FIT
N     LAST RING INCLUDED IN FIT
      JRINGL = 3
      PERFORM FPARA0
C
N     RELABEL HITS
      ALBLM1 = 0.6
      ALBLM2 = 3.0
      PERFORM LABEL
C     PRINT 2005, LBCELL,(WRK(I),I=HPCO0,HPCO9)
C
N     REFIT PARABOLA
      PERFORM FPARA0
C
N     RELABEL HITS
      PERFORM LABEL
C     PRINT 2005, LBCELL,(WRK(I),I=HPCO0,HPCO9)
C
N     SET UP FIT-BANK
      IF SIG.LT.1.
      THEN
        PERFORM FITBNK
      CIF
C
N     CHECK IF BAD FIT AND LOW MOMENTUM
      IF ABS(PAR1).GT..00030 .AND. NHTRNG(1)+NHTRNG(2).GT.16
      THEN
        ALBLM1 = 1.5
        ALBLM2 = 3.0
        PERFORM LABEL
        JRINGL = 2
        PERFORM FPARA0
        ALBLM1 = 0.6
        PERFORM LABEL
        PERFORM FPARA0
        PERFORM LABEL
        IF SIG.LT..10
        THEN
          PERFORM FITBK1
          IWRK(IP+ 4) = 32
        CIF
      CIF
      IF ABS(PAR1).GT..00150 .AND. NHTRNG(1)+NHTRNG(2).GT.9
      THEN
        ALBLM1 = 1.5
        ALBLM2 = 3.0
        PERFORM LABEL
        JRINGL = 1
        NHTFIT = NHTRNG(1)
        IF NHTFIT.LE.5
        THEN
          NHTFIT = NHTFIT + NHTRNG(2)
          JRINGL = 2
        CIF
        IF NHTFIT.GT.9
        THEN
          PERFORM FPARA0
          ALBLM1 = 0.6
          PERFORM LABEL
          PERFORM FPARA0
          PERFORM LABEL
          IF SIG.LT..10
          THEN
            PERFORM FITBK1
            IWRK(IP+ 4) = 48
          CIF
        CIF
      CIF
C
      HPFREE = HPFRE1
      RETURN
C
N     *************************
N     *      F P A R A 0      *
N     *************************
C
C
N     PARABOLA FIT THROUG ORIGIN
      PROC FPARA0
C
N     GET EQUATIONS
N     WEIGHT ORIGIN AS POINT OF PARABOLA
      S0 = WGHT0
      S1 = X0*WGHT0
      S2 = S1*X0
      S3 = S2*X0
      S4 = S3*X0
      S7 = Y0 * WGHT0
      S6 = S7*X0
      S5 = S6*X0
      IPCO = HPCO0
      REPEAT
       IF IWRK(IPCO+ 10).EQ.0 .AND. IWRK(IPCO+12).LE.JRINGL
       THEN
          X = WRK(IPCO+3)
          Y = WRK(IPCO+4)
          X2 = X**2
          S1 = S1 + X
          S2 = S2 + X2
          S3 = S3 + X*X2
          S4 = S4 + X2**2
          S5 = S5 + Y*X2
          S6 = S6 + Y*X
          S7 = S7 + Y
          S0 = S0 + 1.
        CIF
      IPCO = IPCO + HLDCO
      UNTIL IPCO.GT.HPCO9
      IF S0.LT.2.5
      THEN
        SIG = 1000.
      ELSE
C
N       SOLVE EQUATIONS FOR PARABOLA FIT
        F1 = 1. / S4
        XX12 = S3*F1
        XX13 = S2*F1
        YY1  = S5*F1
        XX22 = S2 - S3*XX12
        XX23 = S1 - S3*XX13
        YY2  = S6 - S3*YY1
        XX32 = S1 - S2*XX12
        XX33 = S0 - S2*XX13
        YY3  = S7 - S2*YY1
        IF XX22.GT.XX32
        THEN
          XX23 = XX23 / XX22
          YY2  = YY2  / XX22
          PAR3 = (YY3 - XX32*YY2) / (XX33 - XX32*XX23)
          PAR2 = YY2 - XX23*PAR3
        ELSE
          XX33 = XX33 / XX32
          YY3  = YY3  / XX32
          PAR3 = (YY2 - XX22*YY3) / (XX23 - XX22*XX33)
          PAR2 = YY3 - XX33*PAR3
        CIF
        PAR1 = YY1 - XX12*PAR2 - XX13*PAR3
        DEG = S0 - WGHT0 - 2.
C
C
N       CALC. CHISQ + SOLVE L/R AMBIGUITY
        CHISQ = 0.
        DCHIM1 = 0.
        IHITM1 = 0
        XST = 999999.
        XEN =-999999.
        IPCO = HPCO0
        REPEAT
         IF IWRK(IPCO+ 10).EQ.0 .AND. IWRK(IPCO+12).LE.JRINGL
         THEN
            X = WRK(IPCO+3)
            IF(X.LT.XST) XST = X
            IF(X.GT.XEN) XEN = X
            Y = WRK(IPCO+4)
            F = (PAR1 *X + PAR2 )*X + PAR3
            DCHI = Y - F
            WRK(IPCO+13) = DCHI
N           SUM FOR RMS
            CHISQ = CHISQ + DCHI**2
N           KEEP BIGGEST RMS
C           IF ABS(DCHI).GE.DCHIM1
C           THEN
C             DCHIM1 = ABS(DCHI)
C             IHITM1 = IPCO
C           CIF
C     PRINT 2006, IPCO,X,Y,F,DCHI,CHISQ
          CIF
        IPCO = IPCO + HLDCO
        UNTIL IPCO.GT.HPCO9
        SIG    =      CHISQ  / DEG
C     PRINT 2008, JRINGL,IWRK(IHEND),SIG,DEG,PAR1,PAR2,PAR3,WGHT0,Y0
C
N       SET LIMIT FOR SIGMA
        SIGLM = TRELLM(16)**2
      CIF
C
      CPROC
C
N     *************************
N     *      S E L C L L      *
N     *************************
C
C
N     SELECT CELLS CONTAINING TRACK
      PROC SELCLL
C
        FOR I=1,6
          ITRCLL(I) = 0
        CFOR
        IPC0 = IPTR + 34
        IPC9 = IPC0 +  5
        ICELL = 0
        FOR IPC = IPC0,IPC9
          JCELL = IDATA(IPC)
          IF JCELL.GT. 0 .AND. JCELL.LE.96
          THEN
            JRING = 1
            IF(JCELL.GT.24) JRING = 2
            IF(JCELL.GT.48) JRING = 3
            JPC = JRING*2 - 1
            IF ITRCLL(JPC).EQ.0
            THEN
              ITRCLL(JPC) = JCELL
            ELSE
              IF(ITRCLL(JPC).NE.JCELL) ITRCLL(JPC+1) = JCELL
            CIF
            ICELL = JCELL
            IRING = JRING
          CIF
        CFOR
C
C     PRINT 2016, ITRCLL
      CPROC
C
N     *************************
N     *      F E T C H        *
N     *************************
C
C
N     FETCH HITS IN CELL
      PROC FETCH
C
N       DIR. OF SENSEW. + DRIFTSP.
        IF JRING.NE.3
        THEN
          IC1 = JCELL
          IF(IC1.GT.24) IC1 = IC1 - 24
          CSROT0 = DIRWR1(IC1,1)
          SNROT0 = DIRWR1(IC1,2)
        ELSE
          IC1 = JCELL - 48
          CSROT0 = DIRWR3(IC1,1)
          SNROT0 = DIRWR3(IC1,2)
        CIF
        DRICS  = TRMATC(JCELL,2)
        DRISN  = TRMATS(JCELL,2)
        DRITG  = DRISN/DRICS
        DRISNF = DRISN * .05
C
N       LOAD RADIUS AND WIRE SPACING
        R0 = FSENSW(JRING)
        DR = RINCR (JRING)
C
N       ANGLE OF TRACK IN RING
        R1   = DR*7.5 + R0
        DX   = R1 * CSROT0 - XCIRC
        DY   = R1 * SNROT0 - YCIRC
        RR   = SQRT(DX**2 + DY**2) * CHARGE
        CSB  = DX / RR
        SNB  = DY / RR
        TGB  = CSB/SNB
C
N       SET DRIFT SPACE BIN
        DSBIN1 = DRIVEL(JCELL,1)
        IF(NRUN.GT.100) DS0 = T0FIX(JRING)*DSBIN1*64.
        IF(NRUN.LE.100) DS0 = DSBIN1*.5
N       ANGLE(TRACK,DRIFT DIRECT.)
        TANBET = ABS((TGB-DRITG)/(TGB*DRITG+1.))
C     PRINT 2007, JCELL,CSROT0,SNROT0,DRICS,DRISN,CSB,SNB,CHARGE,TANBET,
C    ,            DSBIN1,DS0
N       CORRECTION CONSTANTS FOR JCELL
C
        IPJCOR = ICALIB(5) + JCELL
        CCST01 = ACALIB(IPJCOR    ) * TANBET
        CCST02 = ACALIB(IPJCOR+ 96) * TANBET
        CCST11 = ACALIB(IPJCOR+192)
        CCST12 = ACALIB(IPJCOR+288)
        CCST21 = ACALIB(IPJCOR+384)
        CCST22 = ACALIB(IPJCOR+480)
        CCST51 = ACALIB(IPJCOR+576) * 10.
        CCST52 = ACALIB(IPJCOR+672) / 121.15
        CCST61 = ACALIB(IPJCOR+768) * 10.
        CCST62 = ACALIB(IPJCOR+864) / 121.15
C     PRINT 2002, JRING,JCELL,IP,IPJCOR,CCST01,CCST02,CCST11,CCST12,
C    ,            CCST21,CCST22,CCST51,CCST52,CCST61,CCST62
N       COUNTER FOR NUMBER OF HITS FOUND
        JHIT = 0
        NHIT   = 0
        NHGOOD = 0
N       PRESET LAST LAYER
        ILAYL =-99
N       LOOP OVER ALL HITS OF CELL
        IPCO = IPCO - HLDCO
        IPJETC = IDATA(IQJETC)*2
        IP0    = IPJETC + 100
        IPCLL  = IPJETC + 2 + JCELL
        IP     = HDATA(IPCLL  ) + IP0
        IP9    = HDATA(IPCLL+1) + IP0
        IPHL   = IPJHTL + 2 + HDATA(IPCLL)/4
C     PRINT 2002, JRING,JCELL,IP,IP9,TGB,SNB,CSB,DRISN,DRICS
        WHILE IP.LT.IP9
C
N         CHECK TRACK # OF HIT LABEL
          LB   = IDATA(IPHL)
          ITR1 = LAND(SHFTR(LB,17),127)
          ITR2 = LAND(SHFTR(LB, 1),127)
C     PRINT 2009, IPHL,LB,ITR1,ITR2,ITRK
          IF ITR1.EQ.ITRK .OR. ITR2.EQ.ITRK
          THEN
C
N           L/R FROM HIT LABEL
            LBLR = 0
            IF(ITR1.EQ.ITRK) LBLR = LAND(LB,MKLRT1)
            IF(ITR2.EQ.ITRK) LBLR = LAND(LB,MKLRT2)
            LBSIDE =-1
            IF(LBLR.NE.0) LBSIDE = 1
            LBLR = LBSIDE
C
            IWIR = HDATA(IP)
            IWIR = SHFTR(IWIR,3)
N           LAYER NUMBER WITHIN RING 3
            ILAY = LAND(IWIR,15)
N           AMPLITUDES
            IAMPL = HDATA(IP+1)
            IAMPR = HDATA(IP+2)
N           DRIFT SPACE
            DS =(HDATA(IP+3)) * DSBIN1
C     DATA NPRHT /0/
C     NPRHT = NPRHT + 1
C     IF(NPRHT.LE.25) PRINT 2019, IWIR,ILAY,JCELL,HDATA(IP+3),DS,DSBIN1
C2019 FORMAT(' HIT ',4I6,F6.1)
            X1   = ILAY * DR + R0
            Z1   = X1*TGTH + ZVERT
N           CORRECTION FOR TOF + PROPAG. ALONG WIRE
            DDS = (1222.9-ABS(Z1))*ABERR(1) + ABERR(6)*R1*CSTHI
            DSC = DS - DDS + DS0
            Y1   = SWDEPL
            IF(LAND(ILAY,1).NE.0) Y1 =-Y1
            Y1   = (7-ILAY)*(CCST52*Z1+CCST51) - CCST62*Z1-CCST61 + Y1
            X    = X1*CSROT0 - Y1*SNROT0
            Y    = X1*SNROT0 + Y1*CSROT0
            IF DS.LE.DRC
            THEN
              IF DS.LT.4.0
              THEN
                IF DS.GT.DSD1
                THEN
                  DSC = (DSD1-DSD0)*DRV0 + (DS-DSD1)*DRV1
                ELSE
                  DSC = (DS-DSD0)*DRV0
                CIF
                IF(DSC.LT.0.1) DSC = 0.1
              CIF
              DXR  = DSC * CSB
              DYR  = DSC * SNB
              DXL =-DXR
              DYL =-DYR
            ELSE
C
N             EDGE WIRE FIELD DISTORTION
              IF ILAY.LT. 3
              THEN
                DILAY =-(ILAY- 3)**2
                DSCL  = (DILAY*CCST11 + 1.) * DSC
                DSCR  = (DILAY*CCST12 + 1.) * DSC
              ELSE
              IF ILAY.GT.12
              THEN
                DILAY =-(ILAY-12)**2
                DSCL  = (DILAY*CCST21 + 1.) * DSC
                DSCR  = (DILAY*CCST22 + 1.) * DSC
              ELSE
                DSCL = DSC
                DSCR = DSC
              CIF
              CIF
C
N             FIELD DISTORTIONS AT LARGE DRIFT TIMES
              IF DSC.GT.ABERR(7)
              THEN
                DWIR  = ILAY - 7.5
                DWIRC = DSC*DRISNF
                DWIRL = DWIR + DWIRC
                DWIRR = DWIR - DWIRC
                DSCL  = (DSCL-ABERR(7))*DWIRL*CCST01 + DSCL
                DSCR  =-(DSCR-ABERR(7))*DWIRR*CCST02 + DSCR
              CIF
              DXR  = (DSCR-DRC)*DRISN + DRC*CSB
              DYR  = (DSCR-DRC)*DRICS + DRC*SNB
              DXL  =-(DSCL-DRC)*DRISN - DRC*CSB
              DYL  =-(DSCL-DRC)*DRICS - DRC*SNB
            CIF
C     PRINT 2010, ILAY,DS,DSC,DSCL,DSCR,XL,XR,X,Y,DXL,DXR,DYL,DYR
            XL   = DXL + X - XT
            YL   = DYL + Y - YT
            XXL  = XL*CSROT + YL*SNROT
            YYL  =-XL*SNROT + YL*CSROT
            XR   = DXR + X - XT
            YR   = DYR + Y - YT
            XXR  = XR*CSROT + YR*SNROT
            YYR  =-XR*SNROT + YR*CSROT
C
N           CALCULATE Z COORDINATE
            IF IAMPR.LE.0.OR.IAMPL.LE.0
            THEN
              ZZ     = 0.
              LZGOOD = 16
            ELSE
              ZZ = IAMPR + IAMPL
              ZZ = FLOAT(IAMPR-IAMPL) * ZAL*.5 / ZZ
              LZGOOD = 0
              IF(ABS(ZZ).GT.1250.) LZGOOD = 16
            CIF
N           SET ARRAY
C     PRINT 2010, ILAY,DS,XXL,YYL,X1,Z1,XXR,YYR,Y1
C
N           CHECK IF LEFT + RIGHT SOLUTION POSSIBLE
            NLRSOL = 1
            IF(DS.LT.2.0) NLRSOL = 2
C
N           LOOP OVER LEFT +/OR RIGHT SOLUTION
            ILRSOL = 0
            REPEAT
            ILRSOL = ILRSOL + 1
C
N             SELECT SIDE
              IF NLRSOL.EQ.1 .AND. LBSIDE.LT.0  .OR.
     ?           NLRSOL.EQ.2 .AND. ILRSOL.EQ.1
              THEN
N               LEFT SIDE
                LBSIDE =-1
                XX  = XXL
                YY  = YYL
              ELSE
N               RIGHT SIDE
                LBSIDE = 1
                XX  = XXR
                YY  = YYR
              CIF
C
N             HIT QUALITY:
              LBGOOD = 0
              IF(LBSIDE.NE.LBLR) LBGOOD = 1
N             NEW LAYER?
              IF ILAY.NE.ILAYL .OR. LBGDL.LE.1.AND.LBGOOD.LE.1
              THEN
                LBREG = 1
N               INCREASE HIT COUNTER
                JHIT = JHIT + 1
                IPCO = IPCO + HLDCO
              ELSE
N               2 HITS IN SAME LAYER, SELECT CLOSEST
                LBREG = 0
                IF(LBGOOD.LT.IWRK(IPCO+10)) LBREG = 1
              CIF
N             REGISTER NEW HIT?
              IF LBREG.NE.0
              THEN
                NHIT   = NHIT   + 1
                IF(LBGOOD.LE.1) NHGOOD = NHGOOD + 1
                IWRK(IPCO   ) = ILAY
                IWRK(IPCO+ 1) = IP
                IWRK(IPCO+ 2) = LBSIDE
                WRK (IPCO+ 3) = XX
                WRK (IPCO+ 4) = YY
                WRK (IPCO+ 5) = ZZ
                WRK (IPCO+ 6) = XX - X0
                IWRK(IPCO+ 7) = LZGOOD
                WRK (IPCO+ 8) = DS
                IWRK(IPCO+ 9) = JCELL
                IWRK(IPCO+10) = LBGOOD
                WRK (IPCO+11) = TGB
                IWRK(IPCO+12) = JRING
                WRK (IPCO+13) = 0.
                ILAYL = ILAY
                LBGDL = LBGOOD
                NHTRNG(JRING) = NHTRNG(JRING) + 1
              CIF
C
            UNTIL ILRSOL.GE.NLRSOL
C
          CIF
C
        IPHL = IPHL + 1
        IP   = IP   + 4
        CWHILE
N       SET IPCO TO 1. FREE LOCATION
        IPCO = IPCO + HLDCO
C
N     MASK FOR TRACKS AT CELL WALL + IN DEAD CELLS
C
N       SET LABEL FOR DEAD CELL
        IF NHIT.LE.2
        THEN
          IF DEADCL(JCELL,NRUN)
          THEN
            LBCELL = LOR(LBCELL,MKDDCL(JRING))
            JHIT = 16
            NHIT = 16
C     PRINT 2019, JCELL,JRING,NRUN,LBCELL
          CIF
        CIF
C
      CPROC
C
N     *************************
N     *      F I T B N K      *
N     *************************
C
C
N     SET UP FIT-BANK
      PROC FITBNK
C
N     START + END POINTS
      YST  = (PAR1 *XST + PAR2 )*XST + PAR3
      YEN  = (PAR1 *XEN + PAR2 )*XEN + PAR3
N     DIRECTION AT START + END POINT
      TGST = PAR1*XST*2 + PAR2
      DXST = 1./SQRT(TGST**2+1.)
      DYST = DXST * TGST
      TGEN = PAR1*XEN*2 + PAR2
      DXEN = 1./SQRT(TGEN**2+1.)
      DYEN = DXEN * TGEN
N     MIN. OF PARABOLA
      XMIN = -PAR2*.5 / PAR1
      YMIN = (PAR1*XMIN + PAR2)*XMIN + PAR3
C
N     CURVATURE + ERROR
      CURV =-PAR1 * 2.
      DET = (S2*S0-S1*S1)*S4 + (S2*S1-S3*S0)*S3 + (S3*S1-S2*S2)*S2
      SIG11 = (S2*S0 - S1*S1)/DET
      SIG22 = (S4*S0 - S2*S2)/DET
      SIG33 = (S4*S2 - S3*S3)/DET
      SIG12 = (S3*S0 - S2*S1)/DET
      SIG13 = (S3*S1 - S2*S2)/DET
      SIG23 = (S4*S1 - S3*S2)/DET
C     PRINT 2012, DET,SIG11,SIG22,SIG33,SIG12,SIG13,SIG23,SIG
C
C
C     PRINT 2014, XST,YST,DXST,DYST,TGST,XEN,YEN,DXEN,DYEN,TGEN,CURV
C
N     FILL FIT-BANK
      IP    = HPTR0 - 1
      IWRK(IP+ 1) = ITRK
      IWRK(IP+ 2) = 32
      IWRK(IP+ 3) = IDAY
      IWRK(IP+ 4) = 16
      WRK (IP+ 5) = XST *CSROT - YST *SNROT + XT
      WRK (IP+ 6) = XST *SNROT + YST *CSROT + YT
      WRK (IP+ 7) = SQRT(WRK(IP+ 5)**2 + WRK(IP+ 6)**2) * TGTH + ZVERT
      WRK (IP+ 8) = (DXST*CSROT - DYST*SNROT)*CSTH
      WRK (IP+ 9) = (DXST*SNROT + DYST*CSROT)*CSTH
      WRK (IP+10) = SNTH
      IWRK(IP+11) = 0
      WRK (IP+12) = XEN *CSROT - YEN *SNROT + XT
      WRK (IP+13) = XEN *SNROT + YEN *CSROT + YT
      WRK (IP+14) = SQRT(WRK(IP+12)**2 + WRK(IP+13)**2) * TGTH + ZVERT
      WRK (IP+15) = (DXEN*CSROT - DYEN*SNROT)*CSTH
      WRK (IP+16) = (DXEN*SNROT + DYEN*CSROT)*CSTH
      WRK (IP+17) = SNTH
      IWRK(IP+18) = 2
      WRK (IP+19) = ATAN2(SNROT,CSROT)
      WRK (IP+20) = XMIN*CSROT - YMIN*SNROT + XT
      WRK (IP+21) = XMIN*SNROT + YMIN*CSROT + YT
      WRK (IP+22) = PAR1
C     IF(SIG  .LT.0) PRINT 2021,WRK(IP+1),S0,SIG
C2021 FORMAT(' -VE SQRT:',I4,5E13.5)
      WRK (IP+23) = SQRT(SIG)
      IWRK(IP+24) = S0 + .001
      WRK (IP+25) = CURV
C     IF(SIG11.LT.0) PRINT 2021,WRK(IP+1),S0,SIG,SIG11
      WRK (IP+26) = SQRT(SIG*SIG11) * 2.
      WRK (IP+27) = CURV
      WRK (IP+28) = CURV
C     I0 = IP+ 1
C     I9 = IP+48
C     PRINT 2001,(WRK(I1),I1=I0,I9)
      CPROC
C
N     *************************
N     *      F I T B K 1      *
N     *************************
C
C
N     CHANGE FIT BANK (1.POINT)
      PROC FITBK1
C
N     START POINT
      YST  = (PAR1 *XST + PAR2 )*XST + PAR3
N     DIRECTION AT START POINT
      TGST = PAR1*XST*2 + PAR2
      DXST = 1./SQRT(TGST**2+1.)
      DYST = DXST * TGST
N     MIN. OF PARABOLA
      XMIN = -PAR2*.5 / PAR1
      YMIN = (PAR1*XMIN + PAR2)*XMIN + PAR3
C
N     CURVATURE + ERROR
      CURV =-PAR1 * 2.
      DET = (S2*S0-S1*S1)*S4 + (S2*S1-S3*S0)*S3 + (S3*S1-S2*S2)*S2
      SIG11 = (S2*S0 - S1*S1)/DET
      SIG22 = (S4*S0 - S2*S2)/DET
      SIG33 = (S4*S2 - S3*S3)/DET
      SIG12 = (S3*S0 - S2*S1)/DET
      SIG13 = (S3*S1 - S2*S2)/DET
      SIG23 = (S4*S1 - S3*S2)/DET
C     PRINT 2012, DET,SIG11,SIG22,SIG33,SIG12,SIG13,SIG23,SIG
C
C     PRINT 2014, XST,YST,DXST,DYST,TGST,XEN,YEN,DXEN,DYEN,TGEN,CURV,
C    ,            XMIN,YMIN
C
N     FILL FIT-BANK
      IP    = HPTR0 - 1
      WRK (IP+ 5) = XST *CSROT - YST *SNROT + XT
      WRK (IP+ 6) = XST *SNROT + YST *CSROT + YT
      WRK (IP+ 7) = SQRT(WRK(IP+ 5)**2 + WRK(IP+ 6)**2) * TGTH + ZVERT
      WRK (IP+ 8) = (DXST*CSROT - DYST*SNROT)*CSTH
      WRK (IP+ 9) = (DXST*SNROT + DYST*CSROT)*CSTH
      WRK (IP+10) = SNTH
      IWRK(IP+18) = 2
      WRK (IP+19) = ATAN2(SNROT,CSROT)
      WRK (IP+20) = XMIN*CSROT - YMIN*SNROT + XT
      WRK (IP+21) = XMIN*SNROT + YMIN*CSROT + YT
      WRK (IP+22) = PAR1
C     IF(SIG  .LT.0) PRINT 2022,WRK(IP+1),S0,SIG
C2022 FORMAT(' -VE SQRT(1):',I4,5E13.5)
      WRK (IP+23) = SQRT(SIG)
      IWRK(IP+24) = S0 + .001
      WRK (IP+25) = CURV
C     IF(SIG11.LT.0) PRINT 2022,WRK(IP+1),S0,SIG,SIG11
      WRK (IP+26) = SQRT(SIG*SIG11) * 2.
      WRK (IP+27) = CURV
C     I0 = IP+ 1
C     I9 = IP+48
C     PRINT 2001,(WRK(I1),I1=I0,I9)
      CPROC
C
C
N     *************************
N     *      L A B E L        *
N     *************************
C
C
N     LABEL USED HITS
      PROC LABEL
C
N       PRESET LAST HIT POINTER
        IWL = -999
        FOR IP = HPCO0,HPCO9,HLDCO
          IW0 = IWRK(IP)
          X   = WRK(IP+3)
          Y   = WRK(IP+4)
          F   = (PAR1*X + PAR2)*X + PAR3
          DF  = F - Y
N         SELECT CLOSEST HIT
          LBGOOD = 4
          IF(ABS(DF).LT.ALBLM2) LBGOOD = 1
          IF(ABS(DF).LT.ALBLM1) LBGOOD = 0
          IWRK(IP+ 10) = LBGOOD
          WRK (IP+13) = DF
C
N         CHECK IF 2 HITS FROM SAME WIRE
          IF IWL.EQ.IW0
          THEN
N           SELECT CLOSEST HIT
            IF ABS(DFL).LT.ABS(DF)
            THEN
              IWRK(IP +10) = 16
            ELSE
              IWRK(IPL+10) = 16
            CIF
          CIF
N         STORE LAST POINTERS + DF
          IWL = IW0
          IPL = IP
          DFL = DF
        CFOR
C
      CPROC
C
C
N     *************************
N     *      I N I T          *
N     *************************
C
C
N     INITIALIZE CONSTANTS
      PROC INIT
C
        IQJETC = IBLN('JETC')
        IQHEAD = IBLN('HEAD')
C
        CALL DAY2(DATE)
        IDAY = DATE(1)*1000 + DATE(2)
C
N       MULT. SCATTERING CONSTANTS
        RESMS = .020**2/2. * .16 * (1. + ALOG10(.16) / 9) * 155.45**2
C
N       RADIUS AROUND WIRE FOR CORR. OF DRIFTSPACE
        DRC = RINCR(1)*.5 * DRICOS
C       CONST. FOR VAR. OF DRIFT VEL.
        IPHEAD = IDATA(IQHEAD)*2
        NRUN = HDATA(IPHEAD+10)
        IF NRUN.LE.100
        THEN
          DSD0   = .0
          DSD1   = 5.0
          DSD2   = 5.0
          DRV0   = 1.0
          DRV1   = 1.0
        ELSE
          DSD0   =-.63
          DSD1   = 1.8
          DSD2   = 4.0
          DRV0   = 0.8
          DRV1   = (DSD2 - (DSD1-DSD0)*DRV0) / (DSD2-DSD1)
        CIF
      CPROC
C
      END
C   21/09/82 306161128  MEMBER NAME  REFIT    (JADEGS)      SHELTRAN
C   19/08/82 209211032  MEMBER NAME  ORREFIT  (FITSR)       SHELTRAN
C   18/02/81 208111542  MEMBER NAME  REFITV   (JETCALSR)    SHELTRAN
      SUBROUTINE REFIT(IPTR,IPJHTL)
C
C        REFIT TRACK ITRK IN 'PATR'-BANK
C        P. STEFFEN                    80/08/19
C
      IMPLICIT INTEGER*2 (H)
      LOGICAL DEADCL
C
#include "cdata.for"
C
#include "calibr.for"
C
#include "cworkpr.for"
#include "cworkeq.for"
C
      EQUIVALENCE
     ,          (HPCO0 ,HPWRK(17)),(HPCO9 ,HPWRK(18)),(HLDCO ,HPWRK(19))
     ,         ,(ICELL ,IDWRK( 1)),(MHIT  ,IDWRK( 2)),(IRING ,IDWRK( 3))
     ,         ,(IERRCD,IDWRK( 4)),(NTRKEL,IDWRK( 5))
C
#include "cjdrch.for"
#include "cdsmax.for"
C
      INTEGER DATE(5), IDAY /0/
      DIMENSION ITRCLL(6), NCNCK(24), NHTRNG(3)
C
N     MASK FOR L/R BIT IN HIT LABEL
      INTEGER MKLRT1 /Z1000000/, MKLRT2 /Z100/
C
N     MASK FOR TRACKS AT CELL WALL
      INTEGER MKBDCL(3) /Z10,Z20,Z40/
      INTEGER MKDDCL(3) /Z01,Z02,Z04/
C
C     IF(IDATA(IPTR+1).LT. 4) RETURN
C     I0 = IPTR + 1
C     I9 = IPTR + 48
C     PRINT 2001, (IDATA(I1),I1=I0,I9)
C     I0 = IPJHTL*2 + 1
C     I9 = I0 + IDATA(IPJHTL)*2 - 1
C     PRINT 2000, IPJHTL,I0,I9,(HDATA(I1),I1=I0,I9)
C     IPJETC = IDATA(IBLN('JETC'))
C     I0 = IPJETC*2 + 1
C     I9 = I0 + 109
C     PRINT 2000, IPJETC,I0,I9,(HDATA(I1),I1=I0,I9)
C2000 FORMAT('0REFIT:',3I8,/,(20(1X,Z4)))
C2001 FORMAT(1H0,2I3,I8,2(I4,3F6.1,3F6.3),
C    ,     /,14X,I3,4E13.5,F6.2,I3,4E13.5,
C    ,     /,14X,I3,2F8.3,F6.1,I3,10X,6I3,8I6,2X,Z4)
C2002 FORMAT('0FETCH:',2I3,2I5,12F9.5)
C2003 FORMAT('0ROTATION:',12F10.5)
C2004 FORMAT('0CIRC.CENTRE:',2I3, F10.5,2F10.0,F8.1,2F8.1)
C2005 FORMAT('0TRACK:',I6,/,(1X,3I6,4F8.1,I4,F6.2,2I4,F8.3,I6,F8.1))
C2006 FORMAT(1X,I6,5F8.2,F12.1,5F8.2)
C2007 FORMAT(' FETCH:',I3,9F8.4,F10.5,F6.0)
C2008 FORMAT(' FIT:',2I3,F8.2,F5.0,F10.6,F7.3,F5.1,F6.3,F5.1)
C2009 FORMAT(' JHTL:',I8,1X,Z8,3I5)
C2010 FORMAT(' HIT:',I6,12F8.2)
C2011 FORMAT('0ABERR:',10F10.6)
C2012 FORMAT('0ERROR:',10E13.6)
C2014 FORMAT('0FIT-BANK:',5F8.3,5X,5F8.3,5X,F8.5,2F8.1)
C2016 FORMAT('0ITRCLL =',6I8,/,(9X,6F8.3))
C
N     INITIALIZATION
      DATA LBINIT /0/
      IF LBINIT .EQ. 0
      THEN
        LBINIT = 1
        PERFORM INIT
      CIF
C
N     RESERVE SPACE IN CWORK
      HPFREE = 1
      HPFRE1 = HPFREE
      HPCO0  = HPFREE
      HLDCO  = 14
      HPFREE = HLDCO*100 + HPCO0
      HPCO9 = HPFREE - 1
C
N     GET RUN #
      IPHEAD = IDATA(IQHEAD)*2
      NRUN = HDATA(IPHEAD+10)
C
N     COPY TRACK BANK
      HPTR0 = HPFREE
      CALL MVC(IWRK(HPTR0),0,IDATA(IPTR+1),0,192)
C
N     TRACK #
      ITRK = IDATA(IPTR+1)
C
N     CENTRE OF CIRCLE (USED FOR ANGULAR CORRECTION)
      IF IDATA(IPTR+18).EQ.1
      THEN
N       CIRCLE PARAMETERS
        ALFA  = ADATA(IPTR+21)
        CRV   = ADATA(IPTR+19)
        IF(ABS(CRV).LT.1.E-8) CRV = SIGN(1.E-8,CRV)
        RAD   =  1./CRV + ADATA(IPTR+20)
        XCIRC = COS(ALFA) * RAD
        YCIRC = SIN(ALFA) * RAD
        CHARGE = SIGN(1.,ADATA(IPTR+25))
      ELSE
N       PARABOLA PARAMETERS
        CRV   = ADATA(IPTR+22)*2.
        IF(ABS(CRV).LT.1.E-8) CRV = SIGN(1.E-8,CRV)
        ALFA  = ADATA(IPTR+19)
        XCIRC =-SIN(ALFA)/CRV + ADATA(IPTR+20)
        YCIRC = COS(ALFA)/CRV + ADATA(IPTR+21)
        CHARGE =-SIGN(1.,ADATA(IPTR+22))
      CIF
C
N     ZVERT, THETA + DIR. COSINES
      ZVERT = ADATA(IPTR+31)
      TGTH = ADATA(IPTR+30)
      CSTHI = SQRT(TGTH**2 + 1.)
      CSTH  = 1. / CSTHI
      SNTH  = CSTH * TGTH
C     PRINT 2004,ITRK,IDATA(IPTR+18),ALFA,XCIRC,YCIRC,ZVERT,TGTH,CSTHI
C
C     PRINT 2029, XO,YO,WGHT0,F1,RESV,RESMS,PTRANS
C2029 FORMAT(' VERTEX',9E13.5)
C     PRINT 2011,ABERR
C
N     ROTATION ANGLE (USING LAST POINT OF TRACK)
      XT    = (ADATA(IPTR+12) + ADATA(IPTR+5)) * .5
      YT    = (ADATA(IPTR+13) + ADATA(IPTR+6)) * .5
      XX    =  ADATA(IPTR+12) - ADATA(IPTR+5)
      YY    =  ADATA(IPTR+13) - ADATA(IPTR+6)
      RR    = SQRT(XX**2+YY**2)
      CSROT = XX / RR
      SNROT = YY / RR
      XOR  =- XT*CSROT -  YT*SNROT
      YOR  =  XT*SNROT -  YT*CSROT
C     PRINT 2003, CSROT,SNROT,XX,YY,XT,YT
C
N     FILL CELL ARRAY
      PERFORM SELCLL
C
N     LOOP OVER ALL CELLS + FETCH HITS
      KCLL = 0
      NHIT = 0
      IPCO = HPCO0
C
N     LOOP OVER RINGS
      JRING = 0
N     TRACKS AT CELL WALLS
      LBCELL = 0
      REPEAT
      JRING = JRING + 1
        NHTRNG(JRING) = 0
        NHRNG = 0
        NCLL = 0
        REPEAT
        NCLL = NCLL + 1
        KCLL = KCLL + 1
          JCELL = ITRCLL(KCLL)
          IF JCELL.NE.0
          THEN
            PERFORM FETCH
            NHRNG = NHRNG + JHIT
          CIF
        UNTIL NCLL.EQ.2
N       SET LABEL FOR TRACK AT CELL BOUND.
        IF(JCELL.NE.0) LBCELL = LOR(MKBDCL(JRING),LBCELL)
      UNTIL KCLL.EQ.6
      HPCO9 = IPCO - 1
C     PRINT 2005, LBCELL,(WRK(I),I=HPCO0,HPCO9)
C
C
      REPEAT
C
C
C
C
N       1. PARABOLA FIT
N       LAST RING INCLUDED IN FIT
        JRINGL = 3
        PERFORM FPARA0
C
N       RELABEL HITS
        ALBLM1 = 0.6
        ALBLM2 = 3.0
        PERFORM LABEL
C       PRINT 2005, LBCELL,(WRK(I),I=HPCO0,HPCO9)
C
N       REFIT PARABOLA
        PERFORM FPARA0
C
N       RELABEL HITS
        PERFORM LABEL
C       IF(ITRK.EQ.17) PRINT 2005, LBCELL,(WRK(I),I=HPCO0,HPCO9)
C
N       SET UP FIT-BANK
        IF SIG.LT.1.
        THEN
          PERFORM FITBNK
        CIF
C
N       STOP IF SIG < .10
        IF(SIG.LT..10) XREPEAT
C
N       STOP IF HIGH MOMENTUM
        IF(ABS(PAR1).LT..00030) XREPEAT
C
N       STOP IF NOT ENOUGH HITS IN R1 + R2
        IF(NHTRNG(1)+NHTRNG(2).LE.16) XREPEAT
C
N       CONTINUE + FIT ONLY R1 + R2
        ALBLM1 = 2.0
        ALBLM2 = 3.0
        PERFORM LABEL
        JRINGL = 2
        PERFORM FPARA0
        ALBLM1 = 1.0
        PERFORM LABEL
        PERFORM FPARA0
        PERFORM LABEL
        IF SIG.LT..20
        THEN
          PERFORM FITBK1
        CIF
C       IF(ITRK.EQ.17) PRINT 2005, LBCELL,(WRK(I),I=HPCO0,HPCO9)
C
N       STOP IF GOOD FIT
        IF(SIG.LT..20) XREPEAT
C
N       STOP IF NOT LOW MOMENTUM
        IF(ABS(PAR1).LT..00150) XREPEAT
C
N       STOP IF NOT ENOUGH HITS IN R1
        IF(NHTRNG(1).LE.9) XREPEAT
C
N       CONTINUE + FIT IN R1 ONLY
        ALBLM1 = 2.0
        ALBLM2 = 3.0
        PERFORM LABEL
        JRINGL = 1
        NHTFIT = NHTRNG(1)
        IF NHTFIT.LE.5
        THEN
          NHTFIT = NHTFIT + NHTRNG(2)
          JRINGL = 2
        CIF
        IF NHTFIT.GT.9
        THEN
          PERFORM FPARA0
          ALBLM1 = 1.0
          PERFORM LABEL
          PERFORM FPARA0
          PERFORM LABEL
          IF SIG.LT..20
          THEN
            PERFORM FITBK1
          CIF
        CIF
C       IF(ITRK.EQ.17) PRINT 2005, LBCELL,(WRK(I),I=HPCO0,HPCO9)
C
C
      UNTIL .TRUE.
C
C
      HPFREE = HPFRE1
      RETURN
C
N     *************************
N     *      F P A R A 0      *
N     *************************
C
C
N     PARABOLA FIT THROUG ORIGIN
      PROC FPARA0
C
N     GET EQUATIONS
N     WEIGHT ORIGIN AS POINT OF PARABOLA
      S0 = 0.
      S1 = 0.
      S2 = 0.
      S3 = 0.
      S4 = 0.
      S7 = 0.
      S6 = 0.
      S5 = 0.
      IPCO = HPCO0
      REPEAT
       IF IWRK(IPCO+ 10).EQ.0 .AND. IWRK(IPCO+12).LE.JRINGL
       THEN
          X = WRK(IPCO+3)
          Y = WRK(IPCO+4)
          X2 = X**2
          S1 = S1 + X
          S2 = S2 + X2
          S3 = S3 + X*X2
          S4 = S4 + X2**2
          S5 = S5 + Y*X2
          S6 = S6 + Y*X
          S7 = S7 + Y
          S0 = S0 + 1.
        CIF
      IPCO = IPCO + HLDCO
      UNTIL IPCO.GT.HPCO9
      IF S0.LT.3.5
      THEN
        SIG = 1000.
      ELSE
C
N       SOLVE EQUATIONS FOR PARABOLA FIT
        F1 = 1. / S4
        XX12 = S3*F1
        XX13 = S2*F1
        YY1  = S5*F1
        XX22 = S2 - S3*XX12
        XX23 = S1 - S3*XX13
        YY2  = S6 - S3*YY1
        XX32 = S1 - S2*XX12
        XX33 = S0 - S2*XX13
        YY3  = S7 - S2*YY1
        IF XX22.GT.XX32
        THEN
          XX23 = XX23 / XX22
          YY2  = YY2  / XX22
          PAR3 = (YY3 - XX32*YY2) / (XX33 - XX32*XX23)
          PAR2 = YY2 - XX23*PAR3
        ELSE
          XX33 = XX33 / XX32
          YY3  = YY3  / XX32
          PAR3 = (YY2 - XX22*YY3) / (XX23 - XX22*XX33)
          PAR2 = YY3 - XX33*PAR3
        CIF
        PAR1 = YY1 - XX12*PAR2 - XX13*PAR3
        DEG = S0 - 3.
C
C
N       CALC. CHISQ + SOLVE L/R AMBIGUITY
        CHISQ = 0.
        DCHIM1 = 0.
        IHITM1 = 0
        IHSTRT = 0
        IPCO = HPCO0
        REPEAT
         IF IWRK(IPCO+ 10).EQ.0 .AND. IWRK(IPCO+12).LE.JRINGL
         THEN
            IF(IHSTRT.EQ.0) IHSTRT = IPCO
            IHEND = IPCO
            X = WRK(IPCO+3)
            Y = WRK(IPCO+4)
            F = (PAR1 *X + PAR2 )*X + PAR3
            DCHI = Y - F
            WRK(IPCO+13) = DCHI
N           SUM FOR RMS
            CHISQ = CHISQ + DCHI**2
N           KEEP BIGGEST RMS
C           IF ABS(DCHI).GE.DCHIM1
C           THEN
C             DCHIM1 = ABS(DCHI)
C             IHITM1 = IPCO
C           CIF
C     PRINT 2006, IPCO,X,Y,F,DCHI,CHISQ
          CIF
        IPCO = IPCO + HLDCO
        UNTIL IPCO.GT.HPCO9
        SIG    =      CHISQ  / DEG
C     PRINT 2008, JRINGL,IWRK(IHEND),SIG,DEG,PAR1,PAR2,PAR3,WGHT0,Y0
C     PRINT 2012, S0,S1,S2,S3,S4,S5,S6,S7
C
      CIF
C
      CPROC
C
N     *************************
N     *      S E L C L L      *
N     *************************
C
C
N     SELECT CELLS CONTAINING TRACK
      PROC SELCLL
C
        FOR I=1,6
          ITRCLL(I) = 0
        CFOR
        IPC0 = IPTR + 34
        IPC9 = IPC0 +  5
        ICELL = 0
        FOR IPC = IPC0,IPC9
          JCELL = IDATA(IPC)
          IF JCELL.GT. 0 .AND. JCELL.LE.96
          THEN
            JRING = 1
            IF(JCELL.GT.24) JRING = 2
            IF(JCELL.GT.48) JRING = 3
            JPC = JRING*2 - 1
            IF ITRCLL(JPC).EQ.0
            THEN
              ITRCLL(JPC) = JCELL
            ELSE
              IF(ITRCLL(JPC).NE.JCELL) ITRCLL(JPC+1) = JCELL
            CIF
            ICELL = JCELL
            IRING = JRING
          CIF
        CFOR
C
C     PRINT 2016, ITRCLL
      CPROC
C
N     *************************
N     *      F E T C H        *
N     *************************
C
C
N     FETCH HITS IN CELL
      PROC FETCH
C
N       DIR. OF SENSEW. + DRIFTSP.
        IF JRING.NE.3
        THEN
          IC1 = JCELL
          IF(IC1.GT.24) IC1 = IC1 - 24
          CSROT0 = DIRWR1(IC1,1)
          SNROT0 = DIRWR1(IC1,2)
        ELSE
          IC1 = JCELL - 48
          CSROT0 = DIRWR3(IC1,1)
          SNROT0 = DIRWR3(IC1,2)
        CIF
        DRICS  = TRMATC(JCELL,2)
        DRISN  = TRMATS(JCELL,2)
        DRITG  = DRISN/DRICS
        DRISNF = DRISN * .05
C
N       LOAD RADIUS AND WIRE SPACING
        R0 = FSENSW(JRING)
        DR = RINCR (JRING)
C
N       ANGLE OF TRACK IN RING
        R1   = DR*7.5 + R0
        DX   = R1 * CSROT0 - XCIRC
        DY   = R1 * SNROT0 - YCIRC
        RR   = SQRT(DX**2 + DY**2) * CHARGE
        CSB  = DX / RR
        SNB  = DY / RR
        TGB  = CSB/SNB
C
N       SET DRIFT SPACE BIN
        DSBIN1 = DRIVEL(JCELL,1)
        IF(NRUN.GT.100) DS0 = T0FIX(JRING)*DSBIN1*64.
        IF(NRUN.LE.100) DS0 = DSBIN1*.5
N       ANGLE(TRACK,DRIFT DIRECT.)
        TANBET = ABS((TGB-DRITG)/(TGB*DRITG+1.))
C     PRINT 2007, JCELL,CSROT0,SNROT0,DRICS,DRISN,CSB,SNB,CHARGE,TANBET,
C    ,            DSBIN1,DS0
N       CORRECTION CONSTANTS FOR JCELL
C
        IPJCOR = ICALIB(5) + JCELL
        CCST01 = ACALIB(IPJCOR    ) * TANBET
        CCST02 = ACALIB(IPJCOR+ 96) * TANBET
        CCST11 = ACALIB(IPJCOR+192)
        CCST12 = ACALIB(IPJCOR+288)
        CCST21 = ACALIB(IPJCOR+384)
        CCST22 = ACALIB(IPJCOR+480)
        CCST51 = ACALIB(IPJCOR+576) * 10.
        CCST52 = ACALIB(IPJCOR+672) / 121.15
        CCST61 = ACALIB(IPJCOR+768) * 10.
        CCST62 = ACALIB(IPJCOR+864) / 121.15
C     PRINT 2002, JRING,JCELL,IP,IPJCOR,CCST01,CCST02,CCST11,CCST12,
C    ,            CCST21,CCST22,CCST51,CCST52,CCST61,CCST62
N       COUNTER FOR NUMBER OF HITS FOUND
        JHIT = 0
        NHIT   = 0
        NHGOOD = 0
N       PRESET LAST LAYER
        ILAYL =-99
N       LOOP OVER ALL HITS OF CELL
        IPCO = IPCO - HLDCO
        IPJETC = IDATA(IQJETC)*2
        IP0    = IPJETC + 100
        IPCLL  = IPJETC + 2 + JCELL
        IP     = HDATA(IPCLL  ) + IP0
        IP9    = HDATA(IPCLL+1) + IP0
        IPHL   = IPJHTL + 2 + HDATA(IPCLL)/4
C     PRINT 2002, JRING,JCELL,IP,IP9,TGB,SNB,CSB,DRISN,DRICS
        WHILE IP.LT.IP9
C
N         CHECK TRACK # OF HIT LABEL
          LB   = IDATA(IPHL)
          ITR1 = LAND(SHFTR(LB,17),127)
          ITR2 = LAND(SHFTR(LB, 1),127)
C     PRINT 2009, IPHL,LB,ITR1,ITR2,ITRK
          IF ITR1.EQ.ITRK .OR. ITR2.EQ.ITRK
          THEN
C
N           L/R FROM HIT LABEL
            LBLR = 0
            IF(ITR1.EQ.ITRK) LBLR = LAND(LB,MKLRT1)
            IF(ITR2.EQ.ITRK) LBLR = LAND(LB,MKLRT2)
            LBSIDE =-1
            IF(LBLR.NE.0) LBSIDE = 1
            LBLR = LBSIDE
C
            IWIR = HDATA(IP)
            IWIR = SHFTR(IWIR,3)
N           LAYER NUMBER WITHIN RING 3
            ILAY = LAND(IWIR,15)
N           AMPLITUDES
            IAMPL = HDATA(IP+1)
            IAMPR = HDATA(IP+2)
N           DRIFT SPACE
            DS =(HDATA(IP+3)) * DSBIN1
C     DATA NPRHT /0/
C     NPRHT = NPRHT + 1
C     IF(NPRHT.LE.25) PRINT 2019, IWIR,ILAY,JCELL,HDATA(IP+3),DS,DSBIN1
C2019 FORMAT(' HIT ',4I6,F6.1)
            X1   = ILAY * DR + R0
            Z1   = X1*TGTH + ZVERT
N           CORRECTION FOR TOF + PROPAG. ALONG WIRE
            DDS = (1222.9-ABS(Z1))*ABERR(1) + ABERR(6)*R1*CSTHI
            DSC = DS - DDS + DS0
            Y1   = SWDEPL
            IF(LAND(ILAY,1).NE.0) Y1 =-Y1
            Y1   = (7-ILAY)*(CCST52*Z1+CCST51) - CCST62*Z1-CCST61 + Y1
            X    = X1*CSROT0 - Y1*SNROT0
            Y    = X1*SNROT0 + Y1*CSROT0
            IF DS.LE.DRC
            THEN
              IF DS.LT.4.0
              THEN
                IF DS.GT.DSD1
                THEN
                  DSC = (DSD1-DSD0)*DRV0 + (DS-DSD1)*DRV1
                ELSE
                  DSC = (DS-DSD0)*DRV0
                CIF
                IF(DSC.LT.0.1) DSC = 0.1
              CIF
              DXR  = DSC * CSB
              DYR  = DSC * SNB
              DXL =-DXR
              DYL =-DYR
            ELSE
C
N             EDGE WIRE FIELD DISTORTION
              IF ILAY.LT. 3
              THEN
                DILAY =-(ILAY- 3)**2
                DSCL  = (DILAY*CCST11 + 1.) * DSC
                DSCR  = (DILAY*CCST12 + 1.) * DSC
              ELSE
              IF ILAY.GT.12
              THEN
                DILAY =-(ILAY-12)**2
                DSCL  = (DILAY*CCST21 + 1.) * DSC
                DSCR  = (DILAY*CCST22 + 1.) * DSC
              ELSE
                DSCL = DSC
                DSCR = DSC
              CIF
              CIF
C
N             FIELD DISTORTIONS AT LARGE DRIFT TIMES
              IF DSC.GT.ABERR(7)
              THEN
                DWIR  = ILAY - 7.5
                DWIRC = DSC*DRISNF
                DWIRL = DWIR + DWIRC
                DWIRR = DWIR - DWIRC
                DSCL  = (DSCL-ABERR(7))*DWIRL*CCST01 + DSCL
                DSCR  =-(DSCR-ABERR(7))*DWIRR*CCST02 + DSCR
              CIF
              DXR  = (DSCR-DRC)*DRISN + DRC*CSB
              DYR  = (DSCR-DRC)*DRICS + DRC*SNB
              DXL  =-(DSCL-DRC)*DRISN - DRC*CSB
              DYL  =-(DSCL-DRC)*DRICS - DRC*SNB
            CIF
C     PRINT 2010, ILAY,DS,DSC,DSCL,DSCR,XL,XR,X,Y,DXL,DXR,DYL,DYR
            XL   = DXL + X - XT
            YL   = DYL + Y - YT
            XXL  = XL*CSROT + YL*SNROT
            YYL  =-XL*SNROT + YL*CSROT
            XR   = DXR + X - XT
            YR   = DYR + Y - YT
            XXR  = XR*CSROT + YR*SNROT
            YYR  =-XR*SNROT + YR*CSROT
C
N           CALCULATE Z COORDINATE
            IF IAMPR.LE.0.OR.IAMPL.LE.0
            THEN
              ZZ     = 0.
              LZGOOD = 16
            ELSE
              ZZ = IAMPR + IAMPL
              ZZ = FLOAT(IAMPR-IAMPL) * ZAL*.5 / ZZ
              LZGOOD = 0
              IF(ABS(ZZ).GT.1250.) LZGOOD = 16
            CIF
N           SET ARRAY
C     PRINT 2010, ILAY,DS,XXL,YYL,X1,Z1,XXR,YYR,Y1
C
N           CHECK IF LEFT + RIGHT SOLUTION POSSIBLE
            NLRSOL = 1
            IF(DS.LT.2.0) NLRSOL = 2
C
N           LOOP OVER LEFT +/OR RIGHT SOLUTION
            ILRSOL = 0
            REPEAT
            ILRSOL = ILRSOL + 1
C
N             SELECT SIDE
              IF NLRSOL.EQ.1 .AND. LBSIDE.LT.0  .OR.
     ?           NLRSOL.EQ.2 .AND. ILRSOL.EQ.1
              THEN
N               LEFT SIDE
                LBSIDE =-1
                XX  = XXL
                YY  = YYL
              ELSE
N               RIGHT SIDE
                LBSIDE = 1
                XX  = XXR
                YY  = YYR
              CIF
C
N             HIT QUALITY:
              LBGOOD = 0
              IF(LBSIDE.NE.LBLR) LBGOOD = 1
N             NEW LAYER?
              IF ILAY.NE.ILAYL .OR. LBGDL.LE.1.AND.LBGOOD.LE.1
              THEN
                LBREG = 1
N               INCREASE HIT COUNTER
                JHIT = JHIT + 1
                IPCO = IPCO + HLDCO
              ELSE
N               2 HITS IN SAME LAYER, SELECT CLOSEST
                LBREG = 0
                IF(LBGOOD.LT.IWRK(IPCO+10)) LBREG = 1
              CIF
N             REGISTER NEW HIT?
              IF LBREG.NE.0
              THEN
                NHIT   = NHIT   + 1
                IF(LBGOOD.LE.1) NHGOOD = NHGOOD + 1
                IWRK(IPCO   ) = ILAY
                IWRK(IPCO+ 1) = IP
                IWRK(IPCO+ 2) = LBSIDE
                WRK (IPCO+ 3) = XX
                WRK (IPCO+ 4) = YY
                WRK (IPCO+ 5) = ZZ
                WRK (IPCO+ 6) = XX
                IWRK(IPCO+ 7) = LZGOOD
                WRK (IPCO+ 8) = DS
                IWRK(IPCO+ 9) = JCELL
                IWRK(IPCO+10) = LBGOOD
                WRK (IPCO+11) = TGB
                IWRK(IPCO+12) = JRING
                WRK (IPCO+13) = 0.
                ILAYL = ILAY
                LBGDL = LBGOOD
                NHTRNG(JRING) = NHTRNG(JRING) + 1
              CIF
C
            UNTIL ILRSOL.GE.NLRSOL
C
          CIF
C
        IPHL = IPHL + 1
        IP   = IP   + 4
        CWHILE
N       SET IPCO TO 1. FREE LOCATION
        IPCO = IPCO + HLDCO
C
N     MASK FOR TRACKS AT CELL WALL + IN DEAD CELLS
C
N       SET LABEL FOR DEAD CELL
        IF NHIT.LE.2
        THEN
          IF DEADCL(JCELL,NRUN)
          THEN
            LBCELL = LOR(LBCELL,MKDDCL(JRING))
            JHIT = 16
            NHIT = 16
C     PRINT 2019, JCELL,JRING,NRUN,LBCELL
          CIF
        CIF
C
      CPROC
C
N     *************************
N     *      F I T B N K      *
N     *************************
C
C
N     SET UP FIT-BANK
      PROC FITBNK
C
N     START + END POINTS
      XST  = WRK(IHSTRT+ 3)
      YST  = (PAR1 *XST + PAR2 )*XST + PAR3
      XEN  = WRK(IHEND + 3)
      YEN  = (PAR1 *XEN + PAR2 )*XEN + PAR3
N     DIRECTION AT START + END POINT
      TGST = PAR1*XST*2 + PAR2
      DXST = 1./SQRT(TGST**2+1.)
      DYST = DXST * TGST
      TGEN = PAR1*XEN*2 + PAR2
      DXEN = 1./SQRT(TGEN**2+1.)
      DYEN = DXEN * TGEN
N     MIN. OF PARABOLA
      XMIN = -PAR2*.5 / PAR1
      YMIN = (PAR1*XMIN + PAR2)*XMIN + PAR3
C
N     CURVATURE + ERROR
      CURV =-PAR1 * 2.
      DET = (S2*S0-S1*S1)*S4 + (S2*S1-S3*S0)*S3 + (S3*S1-S2*S2)*S2
      SIG11 = (S2*S0 - S1*S1)/DET
      SIG22 = (S4*S0 - S2*S2)/DET
      SIG33 = (S4*S2 - S3*S3)/DET
      SIG12 = (S3*S0 - S2*S1)/DET
      SIG13 = (S3*S1 - S2*S2)/DET
      SIG23 = (S4*S1 - S3*S2)/DET
C     PRINT 2012, DET,SIG11,SIG22,SIG33,SIG12,SIG13,SIG23,SIG
C
C
C     PRINT 2014, XST,YST,DXST,DYST,TGST,XEN,YEN,DXEN,DYEN,TGEN,CURV
C
N     FILL FIT-BANK
      IP    = HPTR0 - 1
      IWRK(IP+ 1) = ITRK
      IWRK(IP+ 2) = 16
      IWRK(IP+ 3) = IDAY
      IWRK(IP+ 4) =  0
      WRK (IP+ 5) = XST *CSROT - YST *SNROT + XT
      WRK (IP+ 6) = XST *SNROT + YST *CSROT + YT
      WRK (IP+ 7) = SQRT(WRK(IP+ 5)**2 + WRK(IP+ 6)**2) * TGTH + ZVERT
      WRK (IP+ 8) = (DXST*CSROT - DYST*SNROT)*CSTH
      WRK (IP+ 9) = (DXST*SNROT + DYST*CSROT)*CSTH
      WRK (IP+10) = SNTH
      IWRK(IP+11) = 0
      WRK (IP+12) = XEN *CSROT - YEN *SNROT + XT
      WRK (IP+13) = XEN *SNROT + YEN *CSROT + YT
      WRK (IP+14) = SQRT(WRK(IP+12)**2 + WRK(IP+13)**2) * TGTH + ZVERT
      WRK (IP+15) = (DXEN*CSROT - DYEN*SNROT)*CSTH
      WRK (IP+16) = (DXEN*SNROT + DYEN*CSROT)*CSTH
      WRK (IP+17) = SNTH
      IWRK(IP+18) = 2
      WRK (IP+19) = ATAN2(SNROT,CSROT)
      WRK (IP+20) = XMIN*CSROT - YMIN*SNROT + XT
      WRK (IP+21) = XMIN*SNROT + YMIN*CSROT + YT
      WRK (IP+22) = PAR1
      IF(SIG  .LT.0) PRINT 2021,WRK(IP+1),S0,SIG
 2021 FORMAT(' REFIT(PST): -VE SQRT:',I4,5E13.5)
      WRK (IP+23) = SIG
      IF(SIG  .GT.0) WRK(IP+23) = SQRT(SIG)
      IWRK(IP+24) = S0 + .001
      WRK (IP+25) = CURV
      IF(SIG11.LT.0) PRINT 2021,WRK(IP+1),S0,SIG,SIG11
      WRK (IP+26) = SIG*SIG11
      IF(WRK(IP+26) .GT. 0) WRK(IP+26) = SQRT(WRK(IP+26))*2.
      WRK (IP+27) = CURV
      WRK (IP+28) = CURV
      I0 = IP+ 1
      I9 = IP+48
C     PRINT 2001,(WRK(I1),I1=I0,I9)
      CPROC
C
N     *************************
N     *      F I T B K 1      *
N     *************************
C
C
N     CHANGE FIT BANK (1.POINT)
      PROC FITBK1
C
N     START POINT
      XST  = WRK(IHSTRT+ 3)
      YST  = (PAR1 *XST + PAR2 )*XST + PAR3
N     DIRECTION AT START POINT
      TGST = PAR1*XST*2 + PAR2
      DXST = 1./SQRT(TGST**2+1.)
      DYST = DXST * TGST
N     MIN. OF PARABOLA
      XMIN = -PAR2*.5 / PAR1
      YMIN = (PAR1*XMIN + PAR2)*XMIN + PAR3
C
N     CURVATURE + ERROR
      CURV =-PAR1 * 2.
      DET = (S2*S0-S1*S1)*S4 + (S2*S1-S3*S0)*S3 + (S3*S1-S2*S2)*S2
      SIG11 = (S2*S0 - S1*S1)/DET
      SIG22 = (S4*S0 - S2*S2)/DET
      SIG33 = (S4*S2 - S3*S3)/DET
      SIG12 = (S3*S0 - S2*S1)/DET
      SIG13 = (S3*S1 - S2*S2)/DET
      SIG23 = (S4*S1 - S3*S2)/DET
C     PRINT 2012, DET,SIG11,SIG22,SIG33,SIG12,SIG13,SIG23,SIG
C
C     PRINT 2014, XST,YST,DXST,DYST,TGST,XEN,YEN,DXEN,DYEN,TGEN,CURV,
C    ,            XMIN,YMIN
C
N     FILL FIT-BANK
      IP    = HPTR0 - 1
      WRK (IP+ 5) = XST *CSROT - YST *SNROT + XT
      WRK (IP+ 6) = XST *SNROT + YST *CSROT + YT
      WRK (IP+ 7) = SQRT(WRK(IP+ 5)**2 + WRK(IP+ 6)**2) * TGTH + ZVERT
      WRK (IP+ 8) = (DXST*CSROT - DYST*SNROT)*CSTH
      WRK (IP+ 9) = (DXST*SNROT + DYST*CSROT)*CSTH
      WRK (IP+10) = SNTH
      IWRK(IP+18) = 2
      WRK (IP+19) = ATAN2(SNROT,CSROT)
      WRK (IP+20) = XMIN*CSROT - YMIN*SNROT + XT
      WRK (IP+21) = XMIN*SNROT + YMIN*CSROT + YT
      WRK (IP+22) = PAR1
      IF(SIG  .LT.0) PRINT 2022,WRK(IP+1),S0,SIG
 2022 FORMAT(' REFIT(PST): -VE SQRT(1):',I4,5E13.5)
      WRK (IP+23) = SIG
      IF(SIG  .GT.0) WRK(IP+23) = SQRT(SIG)
      IWRK(IP+24) = S0 + .001
      WRK (IP+25) = CURV
      IF(SIG11.LT.0) PRINT 2022,WRK(IP+1),S0,SIG,SIG11
      WRK (IP+26) = SIG*SIG11
      IF(WRK(IP+26) .GT. 0) WRK(IP+26) = SQRT(WRK(IP+26))*2.
      WRK (IP+27) = CURV
C     I0 = IP+ 1
C     I9 = IP+48
C     PRINT 2001,(WRK(I1),I1=I0,I9)
      CPROC
C
C
N     *************************
N     *      L A B E L        *
N     *************************
C
C
N     LABEL USED HITS
      PROC LABEL
C
N       PRESET LAST HIT POINTER
        IWL = -999
        FOR IP = HPCO0,HPCO9,HLDCO
          IW0 = IWRK(IP)
          X   = WRK(IP+3)
          Y   = WRK(IP+4)
          F   = (PAR1*X + PAR2)*X + PAR3
          DF  = F - Y
N         SELECT CLOSEST HIT
          LBGOOD = 4
          IF(ABS(DF).LT.ALBLM2) LBGOOD = 1
          IF(ABS(DF).LT.ALBLM1) LBGOOD = 0
          IWRK(IP+ 10) = LBGOOD
          WRK (IP+13) = DF
C
N         CHECK IF 2 HITS FROM SAME WIRE
          IF IWL.EQ.IW0
          THEN
N           SELECT CLOSEST HIT
            IF ABS(DFL).LT.ABS(DF)
            THEN
              IWRK(IP +10) = 16
            ELSE
              IWRK(IPL+10) = 16
            CIF
          CIF
N         STORE LAST POINTERS + DF
          IWL = IW0
          IPL = IP
          DFL = DF
        CFOR
C
      CPROC
C
C
N     *************************
N     *      I N I T          *
N     *************************
C
C
N     INITIALIZE CONSTANTS
      PROC INIT
C
        IQJETC = IBLN('JETC')
        IQHEAD = IBLN('HEAD')
C
        CALL DAY2(DATE)
        IDAY = DATE(1)*1000 + DATE(2)
C
N       MULT. SCATTERING CONSTANTS
        RESMS = .020**2/2. * .16 * (1. + ALOG10(.16) / 9) * 155.45**2
C
N       RADIUS AROUND WIRE FOR CORR. OF DRIFTSPACE
        DRC = RINCR(1)*.5 * DRICOS
C       CONST. FOR VAR. OF DRIFT VEL.
        IPHEAD = IDATA(IQHEAD)*2
        NRUN = HDATA(IPHEAD+10)
        IF NRUN.LE.100
        THEN
          DSD0   = .0
          DSD1   = 5.0
          DSD2   = 5.0
          DRV0   = 1.0
          DRV1   = 1.0
        ELSE
          DSD0   =-.63
          DSD1   = 1.8
          DSD2   = 4.0
          DRV0   = 0.8
          DRV1   = (DSD2 - (DSD1-DSD0)*DRV0) / (DSD2-DSD1)
        CIF
      CPROC
C
      END
C   09/06/83 801112018  MEMBER NAME  XYRFTV0  (JADEGS)      SHELTRAN
      SUBROUTINE XYRFTV(MODE)
C-----------------------------------------------------------------------
C                                   J. SPITZER 13/3/87
C        FIT ALL TRACKS WITH OR WITHOUT CONSTRAINT TO RUN VERTEX
C        INPUT :
C        MODE   = 0 : OVERWRITE OLD PATR-BANK WITH NEW RESULTS
C        MODE   = 1 : CREATE NEW PATR-BANK WITH NEW RESULTS
C        MODE   + 2 : NOT USED
C        MODE   + 4 : VERTEX WEAKLY CONSTRAINED (ERRFAC = 100.0)
C        MODE   + 8 : NOT USED
C        MODE   +16 : NO VERTEX CONSTRAINT (ERRFAC = 1000.0 )
C        MODE   +32 : UPDATE OR CREATE JHTL IN PARALLEL WITH PATR.
C                     IF A NEW PATR IS TO BE CREATED OR THE OLD
C                       PATR IS TO BE OVERWRITTEN AND THERE IS NO
C                       JHTL WITH THE SAME NUMBER A NEW JHTL WILL
C                       BE CREATED.
C                     OTHERWISE THE OLD JHTL IS OVERWRITTEN.
C-----------------------------------------------------------------------
C
      IMPLICIT INTEGER*2 (H)
C
#include "cdata.for"
C
      COMMON/XYFVT1/MODXYV
C
      DATA LBINIT /0/, IQPATR/0/, IQJHTL/0/
C
N     INITIALIZATION
      IF LBINIT .LE.0
      THEN
         LBINIT = 1
         IQPATR = IBLN('PATR')
         IQJHTL = IBLN('JHTL')
         IF LAND(MODE,16).NE.0
         THEN
            WRITE(6,81)
 81         FORMAT(' *** XYRFTV WITHOUT VERTEX CONSTRAINT ***')
         ELSE
            IF LAND(MODE,4).EQ.0
            THEN
               WRITE(6,82)
 82            FORMAT(' *** XYRFTV WITH VERTEX CONSTRAINT ***')
               WRITE(6,84)
 84            FORMAT(' VC NOT APPLIED IF VERTEX INCOMPATIBLE',
     +         ' WITH TRACK FITTED W/O VC FIRST.')
            ELSE
               WRITE(6,83)
 83            FORMAT(' *** XYRFTV WITH WEAK VERTEX CONSTRAINT ***')
               WRITE(6,84)
            CIF
         CIF
      CIF
C
C
N     CHECK IF PATR- AND JHTL-BANK
      IPPAT0 = IDATA(IQPATR)
      IF(IPPAT0.LE.0 .OR. IDATA(IQJHTL).LE.0 ) RETURN
C
      NTR    = IDATA(IPPAT0+2)
C
N     CHECK IF 1 TRACK
      IF(NTR.LT.1) RETURN
C
N     CREATE NEW PATR BANK
      IF LAND(MODE,1) .NE. 0
      THEN
         NBNK1  = IDATA(IPPAT0-2) - 1
         NWRD   = IDATA(IPPAT0)
         NBYTE  = NWRD*4
         CALL CCRE(IPPATR,'PATR',NBNK1,NWRD,IERR)
         IF IERR.NE.0
         THEN
            PRINT 2900, IERR
 2900       FORMAT(' CREATION OF NEW PATR-BANK RESULTED IN ERROR',I3)
            RETURN
         CIF
N        COPY CONTENTS OF 'PATR'-BANK
         CALL MVCL(IDATA(IPPATR+1),0,IDATA(IPPAT0+1),0,NBYTE)
      CIF
C
N     UPDATE JHTL BANK
      IF LAND(MODE,32) .NE. 0
      THEN
         NBNK1 = IDATA(IDATA(IQPATR)-2)
         IPJHTL = IDATA(IQJHTL)
         NWRD = IDATA(IPJHTL)
         CALL CLOC( NPJHTL, 'JHTL', NBNK1, IER )
         IF NPJHTL.LE.0
         THEN
           CALL CCRE(NPJHTL,'JHTL',NBNK1,NWRD,IERR)
           IF IERR.NE.0
           THEN
              PRINT 2910, IERR
 2910         FORMAT(' CREATION OF NEW JHTL-BANK RESULTED IN ERROR',I3)
              RETURN
           CIF
N        COPY CONTENTS OF 'JHTL'-BANK
           CALL MVCL(IDATA(NPJHTL+1),0,IDATA(IPJHTL+1),0,NWRD*4)
         CIF
      CIF
C
      IPPATR = IDATA(IQPATR)
      IPTR   = IDATA(IPPATR+1) + IPPATR
      LDTR   = IDATA(IPPATR+3)
C
      ERRFAC = 1.0
      IF(LAND(MODE,4)  .NE. 0) ERRFAC =  100.0
      IF(LAND(MODE,16) .NE. 0) ERRFAC = 1000.0
C
      MODXYV=MODE
      FOR ITR=1,NTR
N        R-PHI FIT
         CALL XYRFT1(IPTR,IDATA(IQJHTL),ERRFAC,LDTR)
         IPTR=IPTR+LDTR
      CFOR
C
      RETURN
      END
C   09/06/83 801121949  MEMBER NAME  XYRFT10  (JADEGS)      SHELTRAN
      SUBROUTINE XYRFT1(IPTR,IPJHTL,ERRFAC,LDTR)
C
C        REFIT TRACK ITRK IN 'PATR'-BANK (WITH A VERTEX
C        CONSTRAINT OF STRENGTH 1/ERRFAC IF ERRFAC<200.
C        VERTEX OMITTED IF INCOMPATIBLE.)
C        PARABOLA FIT IF |OLD CURVATURE * HALF TRACK LENGTH| < .04
C        CIRCLE FIT OTHERWISE
C
C    TEST VERSION 3.     (TESTED TO SOME EXTENT)
C
C                                J. SPITZER  25/3/87
C
C    EXTENDED TO GIVE COVARIANCE MATRIX FOR FIT PARAMETERS
C    CIRCLE PARAMETERS ARE SET EVEN IF PARABOLA FIT WAS PERFORMED
C                                      J.S.  2/4/87
C
C    DOUBLE PRECISION TO CALCULATE DETERMINANT FOR COVARIANCE MATRIX
C                                      J.S.  5/6/87
C
C    MODIFIED TO UPDATE JHTL BANK UPON REQUEST (MADE IN MODXYV)
C    + SMALL MODIFICATIONS IN THE WORK COMMON FOR THE
C      VERTEX CHAMBER GROUP
C                                      J.S.  5/1/88
C
      IMPLICIT INTEGER*2 (H)
      REAL*8   S0D,S1D,S2D,S3D,S4D,S8D,DETD
C
      COMMON/XYFVT1/MODXYV
C
C
#include "cdata.for"
C
#include "cgeo1.for"
C
#include "calibr.for"
C
#include "cworkpr.for"
#include "cworkeq.for"
C
      EQUIVALENCE
     ,          (HPCO0 ,HPWRK(17)),(HPCO9 ,HPWRK(18)),(HLDCO ,HPWRK(19))
     ,         ,(ICELL ,IDWRK( 1)),(MHIT  ,IDWRK( 2)),(IRING ,IDWRK( 3))
     ,         ,(IERRCD,IDWRK( 4)),(NTRKEL,IDWRK( 5))
C
#include "cpatlm.for"
C
#include "cjdrch.for"
#include "cdsmax.for"
      INTEGER DATE(5), IDAY /0/
C-----------------------------
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
C
C
      REAL RESCUT/8./,CKAPP/.966/
      DIMENSION ISORT1(71),ISORT2(3,71),IRESHT(71),ISORT3(91)
     +,ISORT4(2,91)
      DATA KPRT1/0/,NPRT1/50/,IQJETC/0/,IQHEAD/0/
      DATA MASK1/Z2FFFFFF/,MASK2/ZFFFF02FF/,MASK3/ZFFFFF1FF/
C
C
C
N     INITIALIZATION
      DATA LBINIT /0/
      IF LBINIT .EQ. 0
      THEN
         LBINIT = 1
C
         IQJETC = IBLN('JETC')
         IQHEAD = IBLN('HEAD')
C
         CALL DAY2(DATE)
         IDAY = DATE(1)*1000 + DATE(2)
C
N       MULT. SCATTERING CONSTANTS
         RESMS = .020**2/2. * .16 * (1. + ALOG10(.16) /9.) * 155.45**2
C
         WRITE(6,137)
 137     FORMAT(/,' *** XYRFT1 ***  A NEW R-PHI FITTING ROUTINE',/,
     +            '                 TEST VERSION 3. (J. SPITZER)',/
     +     ' BIT 512 OR 1024 IS SET IN THE PROGRAM IDENTIFIER WORD',/,
     +' IN CASE VERTEX CONSTRAINT WAS NOT OR WAS USED RESP.',/,
     +' COVARIANCE MATRIX IS PROVIDED WITH BIT 2048 SWITCHED ON',/,
     +' IF THE PATR BANK ALREADY HAS THE LARGER LENGTH, THE NUMBER',/,
     +' OF HITS USED IN THE FIT IS AT LEAST 10 AND THE FIT CONVERGED.',
     +/)
      CIF
C
C
N     GET RUN #
      IPHEAD = IDATA(IQHEAD)*2
      NRUN = HDATA(IPHEAD+10)
      NEV  = HDATA(IPHEAD+11)
C
N     TRACK #
      ITRK = IDATA(IPTR+1)
C
C
N     RESERVE SPACE IN CWORK
      HPFREE = 1
C
C=======================================================================
N     GET X-Y-VERTEX AND STRENGTH OF VC
      IPV    = ICALIB(10)
      XO     = ACALIB(IPV+ 1)
      YO     = ACALIB(IPV+ 3)
      CURVXY=ADATA(IPTR+25)
      IF(ABS(CURVXY).LT.1.E-9) CURVXY = SIGN(1.E-9,CURVXY)
      DDR0=DISTXY(ADATA(IPTR+5),ADATA(IPTR+6),ADATA(IPTR+8),
     +ADATA(IPTR+9),1./CURVXY,XO,YO,XP,YP,FI)
      FV     = ERRFAC
      IF(FV .LT. .50) FV = .50
      IF NRUN.LT.24200
      THEN
         SRESO=.160
      ELSE
         SRESO=.100
      CIF
      SIGMIN=(SRESO/1.6)**2
      PTRANS = ABS(0.0299792458*BKGAUS/CURVXY) * .001
      RESV=.0100+.25*SIN(FI)**2+RESMS/PTRANS**2
      WGHT0  = (SRESO/FV)**2 / RESV
      INDBIT=512
C=======================================================================
N     HALF DISTANCE BETWEEN FIRST AND LAST POINTS ON TRACK
      XX    =  ADATA(IPTR+12) - ADATA(IPTR+5)
      YY    =  ADATA(IPTR+13) - ADATA(IPTR+6)
      RR    = 0.5*SQRT(XX**2+YY**2)
C
      IF RR.LT.10.
      THEN
         IF KPRT1.LT.NPRT1
         THEN
            KPRT1=KPRT1+1
            WRITE(6,848) NRUN,NEV,ITRK,RR
848         FORMAT(' ******** RUN,EV,TRACK',I8,I6,I3,/,
     +      ' HALF DISTANCE OF FIRST AND LAST POINTS',E14.3,
     +      ', XYRFT1 DOES NOT ATTEMPT R-PHI FIT')
         CIF
         RETURN
      CIF
C
C
N     FETCH HITS, CALCULATE COORDINATES, AND
N     FILL ARRAY IN /CWORK/
      HPCO0  = HPFREE
      LHIT   = 14
C     ORIGIN HALFWAY BETWEEN FIRST AND LAST POINTS ON TRACK
      INDFET = 2
      CALL JFETCH(IPTR,IPJHTL,WRK(HPCO0),LHIT,IPRES,INDFET,XO,YO)
C
C
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
N     JADE ORIGIN IN THE FIT SYSTEM
      XOR    =- XT*CSROT -  YT*SNROT
      YOR    =  XT*SNROT -  YT*CSROT
N     VERTEX IN THE FIT SYSTEM
      X0     = (XO-XT)*CSROT+(YO-YT)*SNROT
      Y0     =-(XO-XT)*SNROT+(YO-YT)*CSROT
C
N     ZVERT, THETA
      ZVERT = ADATA(IPTR+31)
      TGTH = ADATA(IPTR+30)
      CSTH   = WRK (IPRES+11)
      SNTH   = WRK (IPRES+12)
C
      IWRK(IPRES+10)=0
C
C ORIGINAL CHI2 AND CURVATURE ERROR IN PATR BANK
      SIG=ADATA(IPTR+23)**2
      IF(SIG.LT.1.E-5) SIG=1.E-5
      SIG11=(.5*ADATA(IPTR+26))**2/SIG
C
C-----------------------------------------------------------------------
C
C     GYMNASTICS FOR PRIVATE HIT QUALIFICATION
C     AND FOR HANDLING MORE HITS ON SAME WIRE
C
C
C TRY TO RECOVER HITS POSSIBLY LOST BY EARLIER FIT
      XHCUT=RR+200.
C
      XREGA= 100000.
      XREGB=-100000.
      NHALL=0
      NHWIR=0
      NHPOT=0
      IPCO=HPCO0
      REPEAT
         NHWIR=NHWIR+1
         IF(NHWIR.GT.70) RETURN
         ISORT1(NHWIR)=NHWIR
         ISORT2(1,NHWIR)=IPCO
         ISORT2(3,NHWIR)=0
         IW0=IWRK(IPCO)
         ICL0=IWRK(IPCO+9)
         LFL=0
         WHILE IPCO.LE.HPCO9
            IW9=IWRK(IPCO)
            ICL9=IWRK(IPCO+9)
            IF IW9.EQ.IW0 .AND. ICL9.EQ.ICL0
            THEN
N        HIT ON THE SAME WIRE
               NHALL=NHALL+1
               IF(NHALL.GT.90) RETURN
               IF(ISORT2(3,NHWIR).EQ.0) ISORT2(2,NHWIR)=NHALL
               ISORT2(3,NHWIR)=ISORT2(3,NHWIR)+1
C
               XA=WRK(IPCO+3)
               LBGOOD=IWRK(IPCO+10)
               IF LBGOOD.LE.2
               THEN
                  IF ABS(XA).GT.XHCUT
                  THEN
                     ISORT3(NHALL)=-1
                  ELSE
                     ISORT3(NHALL)= 1
                     LFL=1
                     IF(XA.LT.XREGA) XREGA=XA
                     IF(XA.GT.XREGB) XREGB=XA
                  CIF
C
                  IWRK(IPCO+10)=1
               ELSE
                  ISORT3(NHALL)=-1
               CIF
C
               IPCO=IPCO+HLDCO
            ELSE
               XWHILE
            CIF
         CWHILE
         IF LFL.EQ.1
         THEN
            NHPOT=NHPOT+1
         ELSE
            ISORT3(ISORT2(2,NHWIR))=-2
         CIF
      UNTIL IPCO.GT.HPCO9
C-----------------------------------------------------------------------
C
C IF LESS THAN 4 HITS SURVIVE NOTHING WILL BE DONE
      IF(NHPOT.LT.4) RETURN
      XHF=.5*(XREGA+XREGB)
      RRPL=.5*(XREGB-XREGA)
      RRMI=RRPL
      IF ABS(XHF).GT.RR
      THEN
         RRPL=RRPL+XHF-SIGN(RR,XHF)
         RRMI=RRMI-XHF+SIGN(RR,XHF)
         XHF=SIGN(RR,XHF)
      CIF
C-----------------------------------------------------------------------
C  STARTING VALUES OF FIT PARAMETERS
C
      LCHC=0
C  CHANGE START VALUE IF CURVATURE INCONSISTENT WITH FIRST & LAST POINTS
      IF ABS(CURVXY*RR).GT.CKAPP
      THEN
         LCHC=1
         CURVXY=SIGN(CKAPP/RR,CURVXY)
      CIF
      IF ABS(CURVXY)*RR .GT. .04
      THEN
C      CIRCLE FIT
         LFTYP=1
         P1=CURVXY/SQRT(1.-(CURVXY*RR)**2)
         AAH=(RR-XHF)*(RR+XHF)
         PAR3=SAGCIR(1.,P1,AAH,SAGPR,1.E-4)
         PAR2=-P1/(1.+PAR3*P1)*XHF
         CSI2GM=1.+PAR2**2
         PAR1=CURVXY*SQRT(CSI2GM)
      ELSE
C      PARABOLA FIT
         LFTYP=2
         PAR3=.5*CURVXY*(RR-XHF)*(RR+XHF)
         PAR2=-CURVXY*XHF
         CSI2GM=1.+PAR2**2
         PAR1=-0.5*CURVXY*SQRT(CSI2GM)*CSI2GM
      CIF
      XMIN=PAR2/PAR1-CKAPP/ABS(CURVXY)
      XMAX=PAR2/PAR1+CKAPP/ABS(CURVXY)
C
C-----------------------------------------------------------------------
C  FIT SHIFT AND ROTATION ONLY (CURVATURE KEPT FIXED)
      DISCUT=400.
      KFLIP=1
      PERFORM SHFROT
      IF(NHFIT.LT.4) RETURN
      IF NHFIT.LT.6
      THEN
C FIT OF SHIFT AND ROTATION HAS ONLY BEEN PERFORMED
C CHI2 AND CURVATURE ERROR REMAIN THE OLD VALUES IN THE PATR BANK
         PERFORM FITBNK
         RETURN
      CIF
C
C-----------------------------------------------------------------------
      IF ABS(DA).GT.1.5 .OR. ABS(DB).GT..1 .OR. LCHC.EQ.1
      THEN
C SHIFT TO ORIGINAL FIT TOO BIG OR CHANGE OF CURVATURE FOR START
C TRY TO FIND CORRECT STARTING VALUES, TAKE CIRCLE IN ANY CASE
         IF LFTYP.EQ.2
         THEN
            LFTYP=1
            PAR1=CUROUT*SQRT(CSI2GM)
            XMIN=PAR2/PAR1-CKAPP/ABS(CUROUT)
            XMAX=PAR2/PAR1+CKAPP/ABS(CUROUT)
         CIF
         PERFORM STVCIR
      CIF
C
C
C IF TOO MANY HITS THROWN AWAY EVEN WITH THE VERY LOOSE RESIDUAL CUTS
C DO NOT DARE TO ACCEPT/START THE FIT
C
      IF(NHFIT.LT.NHPOT/2) RETURN
C
C
N      SAVE CURRENT (START) VALUES
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
C
C
N      VERTEX
      NHWIRV=NHWIR+1
      ISORT1(NHWIRV)=NHWIRV
      ISORT2(1,NHWIRV)=-200
C
C
C=======================================================================
C
N     FIRST ITERATE WITHOUT VERTEX CONSTRAINT
      INDMAX=NHFIT/8+1
      IF(INDMAX.GT.8) INDMAX=8
      INDFIT=0
      WHILE INDFIT.LT.INDMAX
         INDFIT=INDFIT+1
N    PARABOLA OR CIRCLE FIT
         PERFORM FTCURV
         IF LNOCON.EQ.1
         THEN
C NO CONVERGENCE AS INDICATED BY LOSS OF TOO MANY HITS
C STILL RETAIN THE EARLIER FIT
C IF IT IS THE ONE OBTAINED IN PROC STVCIR,
C CHI2 AND CURVATURE ERROR REMAIN THE OLD VALUES IN THE PATR BANK
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
C
C DO NOT ATTEMPT VERTEX CONSTRAINT
            PERFORM FITBNK
            RETURN
         CIF
         IF(SIG.LT.SIGMIN) XWHILE
         IF INDFIT.GE.2
         THEN
            PERFORM LLSTOP
            IF LSTOP.EQ.1
            THEN
C      PREVIOUS FIT ACCEPTED, RESTORE ITS RESULTS
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
               XWHILE
            CIF
         CIF
         IF(INDFIT.EQ.INDMAX) XWHILE
N      SAVE FIT RESULTS
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
N      HIT CLEANING
         PERFORM HITCLN
      CWHILE
C
C            ======  VERTEX CONSTRAINT  =======
      X0R=X0-XHF
      IF FV.LT.200. .AND. X0R.GT.XMIN .AND. X0R.LT.XMAX
      THEN
C        VERTEX CONSTRAINT (WEEK OR STRONG) HAS BEEN REQUESTED
C        ROUGH CHECK IF RUN VERTEX CONSISTENT WITH THE TRACK
         IF LFTYP.EQ.2
         THEN
            DVCHI2=((PAR1*X0R+PAR2)*X0R+PAR3-Y0)**2*WGHT0
         ELSE
            AAH=-X0R**2*CSI2GM
            FDBPR=1./(1.+PAR1*X0R*PAR2)
            SAG=SAGCIR(FDBPR,PAR1,AAH,SAGPR,1.E-4)
            DVCHI2=(SAG+PAR2*X0R+PAR3-Y0)**2*WGHT0
         CIF
         IF DVCHI2 .LT.  9.*SIG
         THEN
            ISORT2(1,NHWIRV)=-100
            PERFORM FTCURV
            IF(LNOCON.EQ.0) INDBIT=1024
         CIF
      CIF
C
N     SET UP PATR-BANK
      PERFORM FITBNK
      RETURN
C=======================================================================
C
N     *************************
N     *      F T C U R V      *
N     *************************
C
C
N      PARABOLA OR CIRCLE FIT
      PROC FTCURV
C
      LNOCON=0
N     GET EQUATIONS
N     WEIGHT VERTEX AS POINT OF PARABOLA
      KFLIP=3-KFLIP
      KITER=0
      WHILE KITER .LT. 3-LFTYP
         KITER=KITER+1
         X0R=X0-XHF
         IF ISORT2(1,NHWIRV).EQ.-100 .AND. X0R.GT.XMIN .AND. X0R.LT.XMAX
         THEN
N      VERTEX INCLUDED
            IF LFTYP.EQ.2
            THEN
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
            CIF
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
N      VERTEX OMITTED
            S0 = 0.
            S1 = 0.
            S2 = 0.
            S3 = 0.
            S4 = 0.
            S8 = 0.
            S7 = 0.
            S6 = 0.
            S5 = 0.
         CIF
         S00=S0
         FOR IHWIR=1,NHWIR
            IH=ISORT2(2,IHWIR)
            NNH=ISORT2(3,IHWIR)
            FOR JNH=1,NNH
               ISORT4(KFLIP,IH+JNH-1)=0
            CFOR
            IF ISORT3(IH).EQ.1 .OR. ISORT3(IH).EQ.-1.AND.NNH.GT.1
            THEN
               RESMIN=10000.
               FOR JNH=1,NNH
                  JH=IH+JNH-1
                  IF ISORT3(JH).EQ.1
                  THEN
                     IPCO=ISORT2(1,IHWIR)+(JNH-1)*HLDCO
                     XA = WRK(IPCO+3)
                     XAR=XA-XHF
                     IF XAR.GT.XMIN.AND.XAR.LT.XMAX
                     THEN
                        YA = WRK(IPCO+4)
                        IF LFTYP.EQ.2
                        THEN
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
                        CIF
                     ELSE
                        DF0=15000.
                     CIF
                     IF DF0.LT.RESMIN
                     THEN
                        RESMIN=DF0
                        DYDP1=DYDP1A
                        DYDP2=DYDP2A
                        DYRES=DYRESA
                        JHUSE=JH
                     CIF
                  CIF
               CFOR
               IF RESMIN.LT.RESCUT
               THEN
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
               CIF
            CIF
         CFOR
         NHF1=S0-S00+.1
         IF NHF1.LT.6 .OR. NHF1.LT.NHPOT/2
         THEN
            LNOCON=1
            XWHILE
         CIF
         NHFIT=NHF1
         DEG   = S0 - S00 - 3.
N     CURVATURE ERROR
         DET = (S8*S0-S1*S1)*S4 + (S2*S1-S3*S0)*S3 + (S3*S1-S2*S8)*S2
         SIG11 = (S8*S0 - S1*S1)/DET
C
N        SOLVE EQUATIONS
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
         IF XX22.GT.XX32
         THEN
            XX23 = XX23 / XX22
            YY2  = YY2  / XX22
            PARR3 = (YY3 - XX32*YY2) / (XX33 - XX32*XX23)
            PARR2 = YY2 - XX23*PARR3
         ELSE
            XX33 = XX33 / XX32
            YY3  = YY3  / XX32
            PARR3 = (YY2 - XX22*YY3) / (XX23 - XX22*XX33)
            PARR2 = YY3 - XX33*PARR3
         CIF
         PARR1 = YY1 - XX12*PARR2 - XX13*PARR3
C
         IF LFTYP.EQ.2
         THEN
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
         CIF
      CWHILE
C  END ITERATION DONE IN CASE OF CIRCLE FIT ONLY
C
C
      IF LNOCON.EQ.0
      THEN
N     CALC. CHISQ + SOLVE L/R AMBIGUITY
         CHISQ = 0.
         NHF1=0
         FOR IHWIR=1,NHWIR
            IRESHT(IHWIR)=-1
            IH=ISORT2(2,IHWIR)
            NNH=ISORT2(3,IHWIR)
            IF ISORT3(IH).GE.0 .OR. ISORT3(IH).EQ.-1.AND.NNH.GT.1
            THEN
               RESMIN=10000.
               FOR JNH=1,NNH
                  JH=IH+JNH-1
                  IF ISORT3(JH).GE.0
                  THEN
                     IFLG=ISORT3(JH)
                     IPCO=ISORT2(1,IHWIR)+(JNH-1)*HLDCO
                     XR= WRK(IPCO+3)-XHF
                     IF XR.GT.XMIN.AND.XR.LT.XMAX
                     THEN
                        Y = WRK(IPCO+4)
                        IF LFTYP.EQ.2
                        THEN
                           DF0=ABS(Y-(PAR1*XR+PAR2)*XR-PAR3)
                        ELSE
                           AAH=-XR**2*CSI2GM
                           FDBPR=1./(1.+PAR1*XR*PAR2)
                           SAG=SAGCIR(FDBPR,PAR1,AAH,SAGPR,1.E-4)
                           DF0=ABS(Y-SAG-PAR2*XR-PAR3)
                        CIF
                        IF(DF0.LT.RESMIN) RESMIN=DF0
                     CIF
                  CIF
               CFOR
               IF RESMIN.LT.5000.
               THEN
                  IRESHT(IHWIR)=RESMIN*1.E6
                  IF IFLG.EQ.1
                  THEN
                     CHISQ=CHISQ+RESMIN**2
                     NHF1=NHF1+1
                  CIF
               CIF
            CIF
         CFOR
         IF(NHF1.LT.NHFIT-3) LNOCON=1
         SIG    =      CHISQ  / DEG
      CIF
      CPROC
C=======================================================================
      PROC HITCLN
C      LABEL HITS NOT TO BE USED IN THE NEXT ITRATION
N       SQRT(CHI2) OF VERTEX
C  VERTEX NOT INCLUDED IN THE ITERATIVE PART
C  IN THE CURRENT VERSION
CV       X0R=X0-XHF
CV       IF LFTYP.EQ.2
CV       THEN
CV          DFVERT=((PAR1*X0R+PAR2)*X0R+PAR3-Y0)*SQRT(WGHT0)
CV       ELSE
CV          AAH=-X0R**2*CSI2GM
CV          FDBPR=1./(1.+PAR1*X0R*PAR2)
CV          SAG=SAGCIR(FDBPR,PAR1,AAH,SAGPR,1.E-4)
CV          DFVERT=(SAG+PAR2*X0R+PAR3-Y0)*SQRT(WGHT0)
CV       CIF
CV       IRESHT(NHWIRV)=ABS(DFVERT)*1.E6
C-------------------------------------------------------------
C
N       SORT HITS ACCORDING TO RESIDUALS
C  EXCLUDE THE INDFIT LARGEST RESIDUAL HITS,
C  RESTORE THE OTHERS (EXLUDED FOR EVER HITS NOT COUNTED)
C
CV       CALL SHELL9(IRESHT,ISORT1,NHWIRV)
         CALL SHELL9(IRESHT,ISORT1,NHWIR)
         KOMIT=0
CV       FOR J1=1,NHWIRV
         FOR J1=1,NHWIR
CV          IHWIR=ISORT1(NHWIRV+1-J1)
            IHWIR=ISORT1(NHWIR+1-J1)
            IPCO=ISORT2(1,IHWIR)
            IF IPCO.NE.-100 .AND. IPCO.NE.-200
            THEN
N     HIT, NOT VERTEX
               NNH=ISORT2(3,IHWIR)
               IH=ISORT2(2,IHWIR)
               LFLG=0
               FOR JNH=1,NNH
                  IHA=IH+JNH-1
                  IQA=ISORT3(IHA)
                  IF IQA.GT.-1
                  THEN
                     IF LFLG.EQ.0
                     THEN
                        LFLG=1
                        KOMIT=KOMIT+1
                     CIF
                     IF KOMIT.LE.INDFIT
                     THEN
                        ISORT3(IHA)=0
                     ELSE
                        ISORT3(IHA)=1
                     CIF
                  CIF
               CFOR
CV          ELSE
C   VERTEX;   DOES NOT OCCOUR IN THE CURRENT VERSION
CV             KOMIT=KOMIT+1
CV             IF(KOMIT.LE.INDFIT) WGHT0=WGHT0*.01
            CIF
         CFOR
      CPROC
C=======================================================================
      PROC LLSTOP
         IF INDFIT.LE.6
         THEN
            INDCK=INDFIT-1
         ELSE
            INDCK=5
         CIF
         ICHCK=NCHECK(INDCK)
         WHILE SIGLST.LT.RCHECK(ICHCK,1,INDCK)
            ICHCK=ICHCK-1
         CWHILE
         IF(ICHCK.LT.1) ICHCK=1
         IF SIG/SIGLST.GT.RCHECK(ICHCK,2,INDCK)
         THEN
            LSTOP=1
         ELSE
            LSTOP=0
         CIF
      CPROC
C=======================================================================
C
C
N     *************************
N     *      F I T B N K      *
N     *************************
C
C
N     SET UP FIT-BANK
      PROC FITBNK
C
N     START + END POINTS
         XST=XHF+XMIN
         IF(XST.LT.XREGA) XST=XREGA
         XSTR=XST-XHF
         XEN=XHF+XMAX
         IF(XEN.GT.XREGB) XEN=XREGB
         XENR=XEN-XHF
         IF LFTYP.EQ.2
         THEN
            YST  = (PAR1 *XSTR+ PAR2 )*XSTR+ PAR3
            YEN  = (PAR1 *XENR+ PAR2 )*XENR+ PAR3
N     DIRECTION AT START + END POINT
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
         CIF
         DXST = 1./SQRT(TGST**2+1.)
         DYST = DXST * TGST
         DXEN = 1./SQRT(TGEN**2+1.)
         DYEN = DXEN * TGEN
C
C
C
N     COPY TRACK BANK
         HPTR0 = HPFREE
         CALL MVC(IWRK(HPTR0),0,IDATA(IPTR+1),0,4*LDTR)
C
N     FILL FIT-BANK
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
C
         WRK (IP+23) = SQRT(SIG)
         WRK (IP+26) = SQRT(SIG*SIG11/CSI2GM)
         IF(LFTYP.EQ.2) WRK(IP+26)=WRK(IP+26)*2./CSI2GM
C
C        IWRK(IP+18) = LFTYP
C EVEN IF PARABOLA FIT WAS DONE, CIRCLE PARAMETERS ARE STORED
         IWRK(IP+18) = 1
C
         PAR1=CUROUT*SQRT(CSI2GM)
         SIGNC=SIGN(1.,CUROUT)
         ACURV=ABS(CUROUT)
         A=((XHF-XOR)*PAR2-PAR3+YOR)/SQRT(CSI2GM)
         B=(XHF-XOR+(PAR3-YOR)*PAR2)/SQRT(CSI2GM)
         FDBPR=1./ACURV+SIGNC*A
         IF FDBPR.LT.100.
         THEN
            DIMP=-1./ACURV+SQRT(FDBPR**2+B**2)
         ELSE
            DIMP=SIGNC*A+SAGCIR(1./(1.+CUROUT*A),ACURV,B**2,SP,1.E-4)
         CIF
         FDBPR=1.+DIMP*ACURV
         IF FDBPR.LT.ACURV*1.E-3
         THEN
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
C
            IF LDTR.GE.55.AND.NHFIT.GE.10.AND.LNOCON.EQ.0
            THEN
               LCOVAR=1
            ELSE
               LCOVAR=0
            CIF
            IF LAND(MODXYV,32) .NE. 0
            THEN
               LJHTLU=1
            ELSE
               LJHTLU=0
            CIF
C           CALCULATE COVARIANCE MATRIX AND/OR UPDATE JHTL BANK
            PERFORM COVAR
C
            IWRK(IPRES+10)=1
C
         CIF
C
N     PUT RESULT INTO PATR-BANK
         CALL MVC(IDATA(IPTR+1),0,IWRK(HPTR0),0,4*LDTR)
C
C
      CPROC
C-----------------------------------------------------------------------
      PROC SHFROT
C  FIT SHIFT AND ROTATION ONLY (CURVATURE KEPT FIXED)
         S0 = 0.
         S1 = 0.
         S2 = 0.
         S3 = 0.
         S4 = 0.
         FOR IHWIR=1,NHWIR
            IH=ISORT2(2,IHWIR)
            NNH=ISORT2(3,IHWIR)
            FOR JNH=1,NNH
               ISORT4(KFLIP,IH+JNH-1)=0
            CFOR
            IF ISORT3(IH).EQ.1 .OR. ISORT3(IH).EQ.-1.AND.NNH.GT.1
            THEN
               RESMIN=10000.
               FOR JNH=1,NNH
                  JH=IH+JNH-1
                  IF ISORT3(JH).EQ.1
                  THEN
                     IPCO=ISORT2(1,IHWIR)+(JNH-1)*HLDCO
                     XA = WRK(IPCO+3)
                     XAR=XA-XHF
                     IF XAR.GT.XMIN .AND. XAR.LT.XMAX
                     THEN
                        YA = WRK(IPCO+4)
                        IF LFTYP.EQ.2
                        THEN
                           DYRESA=YA-(PAR1*XAR+PAR2)*XAR-PAR3
                        ELSE
                           AAH=-XAR**2*CSI2GM
                           FDBPR=1./(1.+PAR1*XAR*PAR2)
                           SAG=SAGCIR(FDBPR,PAR1,AAH,SAGPR,1.E-4)
                           DYRESA=YA-SAG-PAR2*XAR-PAR3
                        CIF
                        DF0=ABS(DYRESA)
                     ELSE
                        DF0=15000.
                     CIF
                     IF DF0.LT.RESMIN
                     THEN
                        RESMIN=DF0
                        XR=XAR
                        DYRES=DYRESA
                        JHUSE=JH
                     CIF
                  CIF
               CFOR
               IF RESMIN.LT.DISCUT
               THEN
                  S0=S0+1.
                  S1=S1+XR
                  S2=S2+XR**2
                  S3=S3+DYRES
                  S4=S4+DYRES*XR
                  ISORT4(KFLIP,JHUSE)=1
               CIF
            CIF
         CFOR
         NHFIT=S0+.1
         IF NHFIT.GE.4
         THEN
            S12=S1/S2
            S42=S4/S2
            DA=(S3-S1*S42)/(S0-S1*S12)
            DB=S42-S12*DA
            PAR3=PAR3+DA
            PAR2=PAR2+DB
            CSI2GM=1.+PAR2**2
            IF LFTYP.EQ.2
            THEN
               CUROUT =-PAR1 * 2./ (SQRT(CSI2GM)*CSI2GM)
            ELSE
               CUROUT=PAR1/SQRT(CSI2GM)
               XMIN=PAR2/PAR1-CKAPP/ABS(CUROUT)
               XMAX=PAR2/PAR1+CKAPP/ABS(CUROUT)
            CIF
         CIF
      CPROC
C=======================================================================
      PROC STVCIR
C  TRY TO FIND STARTING VALUES FOR CIRCLE FIT
C  THIS PART IS EXECUTED FOR ONLY A VERY SMALL FRACTION OF THE TRACKS
C  JUST LOOP UNTIL 10, NO STOP CONDITION CHECKED
      ISTV1=0
      DISCUT=400.
      WHILE ISTV1.LT.10
         ISTV1=ISTV1+1
C  FIT PARABOLA P1*X**2+P2*X+P3 TO RESIDUALS & MODIFY CIRCLE PARAMETERS
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
         FOR IHWIR=1,NHWIR
            IH=ISORT2(2,IHWIR)
            NNH=ISORT2(3,IHWIR)
            FOR JNH=1,NNH
               ISORT4(KFLIP,IH+JNH-1)=0
            CFOR
            IF ISORT3(IH).EQ.1 .OR. ISORT3(IH).EQ.-1.AND.NNH.GT.1
            THEN
               RESMIN=10000.
               FOR JNH=1,NNH
                  JH=IH+JNH-1
                  IF ISORT3(JH).EQ.1
                  THEN
                     IPCO=ISORT2(1,IHWIR)+(JNH-1)*HLDCO
                     XA = WRK(IPCO+3)
                     XAR= WRK(IPCO+3)-XHF
                     IF XAR.GT.XMIN.AND.XAR.LT.XMAX
                     THEN
                        YA = WRK(IPCO+4)
                        AAH=-XAR**2
                        FDBPR=1./(1.+PAR1*XAR*PAR2)
                        SAG=SAGCIR(FDBPR,PAR1,AAH*CSI2GM,SAGPR,1.E-4)
                        DYRESA=YA-SAG-PAR2*XAR-PAR3
                        DF0=ABS(DYRESA)
                     ELSE
                        DF0=15000.
                     CIF
                     IF DF0.LT.RESMIN
                     THEN
                        RESMIN=DF0
                        X=XAR
                        DYRES=DYRESA
                        JHUSE=JH
                     CIF
                  CIF
               CFOR
               IF RESMIN.LT.DISCUT
               THEN
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
               CIF
            CIF
         CFOR
         NHFIT=S0+.1
C
C
         IF(NHFIT.LT.5) RETURN
C
C
N        SOLVE EQUATIONS
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
         IF XX22.GT.XX32
         THEN
            XX23 = XX23 / XX22
            YY2  = YY2  / XX22
            PARR3 = (YY3 - XX32*YY2) / (XX33 - XX32*XX23)
            PARR2 = YY2 - XX23*PARR3
         ELSE
            XX33 = XX33 / XX32
            YY3  = YY3  / XX32
            PARR3 = (YY2 - XX22*YY3) / (XX23 - XX22*XX33)
            PARR2 = YY3 - XX33*PARR3
         CIF
         PARR1 = YY1 - XX12*PARR2 - XX13*PARR3
C
         XAR=-.7*RRMI
         IF(XAR.LT..8*XMIN) XAR=.8*XMIN
         XBR= .7*RRPL
         IF(XBR.GT..8*XMAX) XBR=.8*XMAX
         IF -XAR.LT.XBR
         THEN
            XBR=-XAR
         ELSE
            XAR=-XBR
         CIF
C
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
         IF SAG**2.GT.(CKAPP*XBR)**2*C2
         THEN
            SAG=SIGN(CKAPP*XBR*SQRT(C2),SAG)
            YC=.5*(YA+YB)+SAG
         CIF
         PAR3=YC
         P1=2.*SAG/(C2*XBR**2-SAG**2)
         CUROUT=P1/SQRT(C2*(1.+(P1*XBR)**2))
         IF(ABS(CUROUT).LT.1.E-8) CUROUT= SIGN(1.E-8,CUROUT)
         PAR2=P2/(1.+SAG*P1)
         CSI2GM=1.+PAR2**2
         PAR1=CUROUT*SQRT(CSI2GM)
         XMIN=PAR2/PAR1-CKAPP/ABS(CUROUT)
         XMAX=PAR2/PAR1+CKAPP/ABS(CUROUT)
C
         DISCUT=.5*DISCUT
         IF(DISCUT.LT.10.) DISCUT=10.
      CWHILE
      CPROC
C-----------------------------------------------------------------------
      PROC COVAR
C
C      CALCULATE COVARIANCE MATRIX AND/OR UPDATE JHTL BANK
C
         SAMFI=SINALP*CSROT-COSALP*SNROT
         CAMFI=COSALP*CSROT+SINALP*SNROT
C
C        UPDATE OF JHTL FOR HITS   N O T   USED IN THE FIT
         IP00=2*IDATA(IQJETC)+100
         FOR IHWIR=1,NHWIR
            IH=ISORT2(2,IHWIR)
            NNH=ISORT2(3,IHWIR)
            FOR JNH=1,NNH
               JH=IH+JNH-1
               IF ISORT4(KFLIP,JH).NE.1
               THEN
                  IPCO=ISORT2(1,IHWIR)+(JNH-1)*HLDCO
                  X=WRK(IPCO+3)
                  Y=WRK(IPCO+4)
                  A=(X-XOR)*CAMFI+(Y-YOR)*SAMFI
                  B=(X-XOR)*SAMFI-(Y-YOR)*CAMFI
                  FDBPR=1./ACURV+DIMP-A
                  IF FDBPR.LT.100.
                  THEN
                     SAG=SQRT(FDBPR**2+B**2)
                     RESA=SAG-1./ACURV
                  ELSE
                     FDBPR=1./(1.+ACURV*(DIMP-A))
                     SAG=SAGCIR(FDBPR,ACURV,B**2,SP,1.E-4)
                     RESA=DIMP-A+SAG
                  CIF
C
C                 IWRK(IPCO+10)=1
                  WRK(IPCO+13)=RESA
                  IF LJHTLU.EQ.1
                  THEN
                     IP    =IWRK(IPCO+1)
                     LBSIDE=IWRK(IPCO+2)
                     IPHL=IPJHTL+2+(IP-IP00)/4
                     LB=IDATA(IPHL)
                     IDST=ABS(RESA)*5.
                     IF(IDST.GT.31) IDST=31
                     IDST=SHFTL(IDST,11)
                     IDST=LOR(IDST,1024)
                     IF(LBSIDE.EQ.1) IDST=LOR(IDST,256)
                     ITR1=LAND(SHFTR(LB,17),127)
                     IF ITR1.EQ.ITRK
                     THEN
                        IDATA(IPHL)=LOR(LAND(LB,MASK1),SHFTL(IDST,16))
                     ELSE
                        IDATA(IPHL)=LOR(LAND(LB,MASK2),      IDST    )
                     CIF
                  CIF
               CIF
            CFOR
         CFOR
C
         CHISQ=0.
C
N      VERTEX OMITTED
         NHF1=0
         S0D= 0.D0
         S1D= 0.D0
         S2D= 0.D0
         S3D= 0.D0
         S4D= 0.D0
         S8D= 0.D0
         FOR IHWIR=1,NHWIR
            IH=ISORT2(2,IHWIR)
            NNH=ISORT2(3,IHWIR)
            FOR JNH=1,NNH
               JH=IH+JNH-1
               IF ISORT4(KFLIP,JH).EQ.1
               THEN
                  IPCO=ISORT2(1,IHWIR)+(JNH-1)*HLDCO
                  X=WRK(IPCO+3)
                  Y=WRK(IPCO+4)
                  A=(X-XOR)*CAMFI+(Y-YOR)*SAMFI
                  B=(X-XOR)*SAMFI-(Y-YOR)*CAMFI
                  FDBPR=1./ACURV+DIMP-A
                  IF FDBPR.LT.100.
                  THEN
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
                  CIF
C
                  NHF1=NHF1+1
                  CHISQ=CHISQ+RESA**2
                  S0D=S0D+DRDP3**2
                  S1D=S1D+DRDP3*DRDP2
                  S2D=S2D+DRDP1*DRDP3
                  S3D=S3D+DRDP1*DRDP2
                  S4D=S4D+DRDP1**2
                  S8D=S8D+DRDP2**2
C
                  IWRK(IPCO+10)=0
                  WRK(IPCO+13)=RESA
                  IF LJHTLU.EQ.1
                  THEN
                     IP    =IWRK(IPCO+1)
                     LBSIDE=IWRK(IPCO+2)
                     IPHL=IPJHTL+2+(IP-IP00)/4
                     LB=IDATA(IPHL)
                     IDST=ABS(RESA)*5.
                     IF(IDST.GT.31) IDST=31
                     IDST=SHFTL(IDST,11)
                     IF(LBSIDE.EQ.1) IDST=LOR(IDST,256)
                     ITR1=LAND(SHFTR(LB,17),127)
                     IF ITR1.EQ.ITRK
                     THEN
                        IDATA(IPHL)=LOR(LAND(LB,MASK1),SHFTL(IDST,16))
                     ELSE
                        IDATA(IPHL)=LOR(LAND(LB,MASK2),      IDST    )
                     CIF
                  CIF
               CIF
            CFOR
         CFOR
         IF LCOVAR.EQ.1
         THEN
            IF NHF1.LT.10.OR.NHF1.NE.NHFIT
            THEN
               PRINT 6781,NRUN,NEV,ITRK,NHFIT,NHF1
6781           FORMAT(' RUN,EV,TRK,NHFIT,NHF1',I7,I6,I3,2I5)
            ELSE
               DETD=(S8D*S0D-S1D*S1D)*S4D+
     +        (S2D*S1D-S3D*S0D)*S3D+(S3D*S1D-S2D*S8D)*S2D
               FACT=CHISQ/(NHFIT-3)/DETD
C
               IWRK(IP+2) =LOR(IWRK(IP+2),2048)
               WRK(IP+49)=CHISQ/.115**2
               WRK(IP+50)=(S8*S0-S1**2)*FACT
               WRK(IP+51)=(S1*S2-S0*S3)*FACT
               WRK(IP+52)=(S4*S0-S2**2)*FACT
               WRK(IP+53)=(S3*S1-S8*S2)*FACT
               WRK(IP+54)=(S2*S3-S1*S4)*FACT
               WRK(IP+55)=(S8*S4-S3**2)*FACT
            CIF
         CIF
      CPROC
      END
C   09/06/83 802221536  MEMBER NAME  XYRFT11  (JADEGS)      SHELTRAN
      SUBROUTINE XYRFT1(IPTR,IPJHTL,ERRFAC,LDTR)
C
C        REFIT TRACK ITRK IN 'PATR'-BANK (WITH A VERTEX
C        CONSTRAINT OF STRENGTH 1/ERRFAC IF ERRFAC<200.
C        VERTEX OMITTED IF INCOMPATIBLE.)
C        PARABOLA FIT IF |OLD CURVATURE * HALF TRACK LENGTH| < .04
C        CIRCLE FIT OTHERWISE
C
C    TEST VERSION 3.     (TESTED TO SOME EXTENT)
C    22.2.88   MVC CHANGED TO MVCL (256 BYTES NOT ENOUGH!)  J.H./J.O.
C
C                                J. SPITZER  25/3/87
C
C    EXTENDED TO GIVE COVARIANCE MATRIX FOR FIT PARAMETERS
C    CIRCLE PARAMETERS ARE SET EVEN IF PARABOLA FIT WAS PERFORMED
C                                      J.S.  2/4/87
C
C    DOUBLE PRECISION TO CALCULATE DETERMINANT FOR COVARIANCE MATRIX
C                                      J.S.  5/6/87
C
C    MODIFIED TO UPDATE JHTL BANK UPON REQUEST (MADE IN MODXYV)
C    + SMALL MODIFICATIONS IN THE WORK COMMON FOR THE
C      VERTEX CHAMBER GROUP
C                                      J.S.  5/1/88
C
      IMPLICIT INTEGER*2 (H)
      REAL*8   S0D,S1D,S2D,S3D,S4D,S8D,DETD
C
      COMMON/XYFVT1/MODXYV
C
C
#include "cdata.for"
C
#include "cgeo1.for"
C
#include "calibr.for"
C
#include "cworkpr.for"
#include "cworkeq.for"
C
      EQUIVALENCE
     ,          (HPCO0 ,HPWRK(17)),(HPCO9 ,HPWRK(18)),(HLDCO ,HPWRK(19))
     ,         ,(ICELL ,IDWRK( 1)),(MHIT  ,IDWRK( 2)),(IRING ,IDWRK( 3))
     ,         ,(IERRCD,IDWRK( 4)),(NTRKEL,IDWRK( 5))
C
#include "cpatlm.for"
C
#include "cjdrch.for"
#include "cdsmax.for"
      INTEGER DATE(5), IDAY /0/
C-----------------------------
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
C
C
      REAL RESCUT/8./,CKAPP/.966/
      DIMENSION ISORT1(71),ISORT2(3,71),IRESHT(71),ISORT3(91)
     +,ISORT4(2,91)
      DATA KPRT1/0/,NPRT1/50/,IQJETC/0/,IQHEAD/0/
      DATA MASK1/Z2FFFFFF/,MASK2/ZFFFF02FF/,MASK3/ZFFFFF1FF/
C
C
C
N     INITIALIZATION
      DATA LBINIT /0/
      IF LBINIT .EQ. 0
      THEN
         LBINIT = 1
C
         IQJETC = IBLN('JETC')
         IQHEAD = IBLN('HEAD')
C
         CALL DAY2(DATE)
         IDAY = DATE(1)*1000 + DATE(2)
C
N       MULT. SCATTERING CONSTANTS
         RESMS = .020**2/2. * .16 * (1. + ALOG10(.16) /9.) * 155.45**2
C
         WRITE(6,137)
 137     FORMAT(/,' *** XYRFT1 ***  A NEW R-PHI FITTING ROUTINE',/,
     +            '                 TEST VERSION 3. (J. SPITZER)',/
     +     ' BIT 512 OR 1024 IS SET IN THE PROGRAM IDENTIFIER WORD',/,
     +' IN CASE VERTEX CONSTRAINT WAS NOT OR WAS USED RESP.',/,
     +' COVARIANCE MATRIX IS PROVIDED WITH BIT 2048 SWITCHED ON',/,
     +' IF THE PATR BANK ALREADY HAS THE LARGER LENGTH, THE NUMBER',/,
     +' OF HITS USED IN THE FIT IS AT LEAST 10 AND THE FIT CONVERGED.',
     +/)
      CIF
C
C
N     GET RUN #
      IPHEAD = IDATA(IQHEAD)*2
      NRUN = HDATA(IPHEAD+10)
      NEV  = HDATA(IPHEAD+11)
C
N     TRACK #
      ITRK = IDATA(IPTR+1)
C
C
N     RESERVE SPACE IN CWORK
      HPFREE = 1
C
C=======================================================================
N     GET X-Y-VERTEX AND STRENGTH OF VC
      IPV    = ICALIB(10)
      XO     = ACALIB(IPV+ 1)
      YO     = ACALIB(IPV+ 3)
      CURVXY=ADATA(IPTR+25)
      IF(ABS(CURVXY).LT.1.E-9) CURVXY = SIGN(1.E-9,CURVXY)
      DDR0=DISTXY(ADATA(IPTR+5),ADATA(IPTR+6),ADATA(IPTR+8),
     +ADATA(IPTR+9),1./CURVXY,XO,YO,XP,YP,FI)
      FV     = ERRFAC
      IF(FV .LT. .50) FV = .50
      IF NRUN.LT.24200
      THEN
         SRESO=.160
      ELSE
         SRESO=.100
      CIF
      SIGMIN=(SRESO/1.6)**2
      PTRANS = ABS(0.0299792458*BKGAUS/CURVXY) * .001
      RESV=.0100+.25*SIN(FI)**2+RESMS/PTRANS**2
      WGHT0  = (SRESO/FV)**2 / RESV
      INDBIT=512
C=======================================================================
N     HALF DISTANCE BETWEEN FIRST AND LAST POINTS ON TRACK
      XX    =  ADATA(IPTR+12) - ADATA(IPTR+5)
      YY    =  ADATA(IPTR+13) - ADATA(IPTR+6)
      RR    = 0.5*SQRT(XX**2+YY**2)
C
      IF RR.LT.10.
      THEN
         IF KPRT1.LT.NPRT1
         THEN
            KPRT1=KPRT1+1
            WRITE(6,848) NRUN,NEV,ITRK,RR
848         FORMAT(' ******** RUN,EV,TRACK',I8,I6,I3,/,
     +      ' HALF DISTANCE OF FIRST AND LAST POINTS',E14.3,
     +      ', XYRFT1 DOES NOT ATTEMPT R-PHI FIT')
         CIF
         RETURN
      CIF
C
C
N     FETCH HITS, CALCULATE COORDINATES, AND
N     FILL ARRAY IN /CWORK/
      HPCO0  = HPFREE
      LHIT   = 14
C     ORIGIN HALFWAY BETWEEN FIRST AND LAST POINTS ON TRACK
      INDFET = 2
      CALL JFETCH(IPTR,IPJHTL,WRK(HPCO0),LHIT,IPRES,INDFET,XO,YO)
C
C
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
N     JADE ORIGIN IN THE FIT SYSTEM
      XOR    =- XT*CSROT -  YT*SNROT
      YOR    =  XT*SNROT -  YT*CSROT
N     VERTEX IN THE FIT SYSTEM
      X0     = (XO-XT)*CSROT+(YO-YT)*SNROT
      Y0     =-(XO-XT)*SNROT+(YO-YT)*CSROT
C
N     ZVERT, THETA
      ZVERT = ADATA(IPTR+31)
      TGTH = ADATA(IPTR+30)
      CSTH   = WRK (IPRES+11)
      SNTH   = WRK (IPRES+12)
C
      IWRK(IPRES+10)=0
C
C ORIGINAL CHI2 AND CURVATURE ERROR IN PATR BANK
      SIG=ADATA(IPTR+23)**2
      IF(SIG.LT.1.E-5) SIG=1.E-5
      SIG11=(.5*ADATA(IPTR+26))**2/SIG
C
C-----------------------------------------------------------------------
C
C     GYMNASTICS FOR PRIVATE HIT QUALIFICATION
C     AND FOR HANDLING MORE HITS ON SAME WIRE
C
C
C TRY TO RECOVER HITS POSSIBLY LOST BY EARLIER FIT
      XHCUT=RR+200.
C
      XREGA= 100000.
      XREGB=-100000.
      NHALL=0
      NHWIR=0
      NHPOT=0
      IPCO=HPCO0
      REPEAT
         NHWIR=NHWIR+1
         IF(NHWIR.GT.70) RETURN
         ISORT1(NHWIR)=NHWIR
         ISORT2(1,NHWIR)=IPCO
         ISORT2(3,NHWIR)=0
         IW0=IWRK(IPCO)
         ICL0=IWRK(IPCO+9)
         LFL=0
         WHILE IPCO.LE.HPCO9
            IW9=IWRK(IPCO)
            ICL9=IWRK(IPCO+9)
            IF IW9.EQ.IW0 .AND. ICL9.EQ.ICL0
            THEN
N        HIT ON THE SAME WIRE
               NHALL=NHALL+1
               IF(NHALL.GT.90) RETURN
               IF(ISORT2(3,NHWIR).EQ.0) ISORT2(2,NHWIR)=NHALL
               ISORT2(3,NHWIR)=ISORT2(3,NHWIR)+1
C
               XA=WRK(IPCO+3)
               LBGOOD=IWRK(IPCO+10)
               IF LBGOOD.LE.2
               THEN
                  IF ABS(XA).GT.XHCUT
                  THEN
                     ISORT3(NHALL)=-1
                  ELSE
                     ISORT3(NHALL)= 1
                     LFL=1
                     IF(XA.LT.XREGA) XREGA=XA
                     IF(XA.GT.XREGB) XREGB=XA
                  CIF
C
                  IWRK(IPCO+10)=1
               ELSE
                  ISORT3(NHALL)=-1
               CIF
C
               IPCO=IPCO+HLDCO
            ELSE
               XWHILE
            CIF
         CWHILE
         IF LFL.EQ.1
         THEN
            NHPOT=NHPOT+1
         ELSE
            ISORT3(ISORT2(2,NHWIR))=-2
         CIF
      UNTIL IPCO.GT.HPCO9
C-----------------------------------------------------------------------
C
C IF LESS THAN 4 HITS SURVIVE NOTHING WILL BE DONE
      IF(NHPOT.LT.4) RETURN
      XHF=.5*(XREGA+XREGB)
      RRPL=.5*(XREGB-XREGA)
      RRMI=RRPL
      IF ABS(XHF).GT.RR
      THEN
         RRPL=RRPL+XHF-SIGN(RR,XHF)
         RRMI=RRMI-XHF+SIGN(RR,XHF)
         XHF=SIGN(RR,XHF)
      CIF
C-----------------------------------------------------------------------
C  STARTING VALUES OF FIT PARAMETERS
C
      LCHC=0
C  CHANGE START VALUE IF CURVATURE INCONSISTENT WITH FIRST & LAST POINTS
      IF ABS(CURVXY*RR).GT.CKAPP
      THEN
         LCHC=1
         CURVXY=SIGN(CKAPP/RR,CURVXY)
      CIF
      IF ABS(CURVXY)*RR .GT. .04
      THEN
C      CIRCLE FIT
         LFTYP=1
         P1=CURVXY/SQRT(1.-(CURVXY*RR)**2)
         AAH=(RR-XHF)*(RR+XHF)
         PAR3=SAGCIR(1.,P1,AAH,SAGPR,1.E-4)
         PAR2=-P1/(1.+PAR3*P1)*XHF
         CSI2GM=1.+PAR2**2
         PAR1=CURVXY*SQRT(CSI2GM)
      ELSE
C      PARABOLA FIT
         LFTYP=2
         PAR3=.5*CURVXY*(RR-XHF)*(RR+XHF)
         PAR2=-CURVXY*XHF
         CSI2GM=1.+PAR2**2
         PAR1=-0.5*CURVXY*SQRT(CSI2GM)*CSI2GM
      CIF
      XMIN=PAR2/PAR1-CKAPP/ABS(CURVXY)
      XMAX=PAR2/PAR1+CKAPP/ABS(CURVXY)
C
C-----------------------------------------------------------------------
C  FIT SHIFT AND ROTATION ONLY (CURVATURE KEPT FIXED)
      DISCUT=400.
      KFLIP=1
      PERFORM SHFROT
      IF(NHFIT.LT.4) RETURN
      IF NHFIT.LT.6
      THEN
C FIT OF SHIFT AND ROTATION HAS ONLY BEEN PERFORMED
C CHI2 AND CURVATURE ERROR REMAIN THE OLD VALUES IN THE PATR BANK
         PERFORM FITBNK
         RETURN
      CIF
C
C-----------------------------------------------------------------------
      IF ABS(DA).GT.1.5 .OR. ABS(DB).GT..1 .OR. LCHC.EQ.1
      THEN
C SHIFT TO ORIGINAL FIT TOO BIG OR CHANGE OF CURVATURE FOR START
C TRY TO FIND CORRECT STARTING VALUES, TAKE CIRCLE IN ANY CASE
         IF LFTYP.EQ.2
         THEN
            LFTYP=1
            PAR1=CUROUT*SQRT(CSI2GM)
            XMIN=PAR2/PAR1-CKAPP/ABS(CUROUT)
            XMAX=PAR2/PAR1+CKAPP/ABS(CUROUT)
         CIF
         PERFORM STVCIR
      CIF
C
C
C IF TOO MANY HITS THROWN AWAY EVEN WITH THE VERY LOOSE RESIDUAL CUTS
C DO NOT DARE TO ACCEPT/START THE FIT
C
      IF(NHFIT.LT.NHPOT/2) RETURN
C
C
N      SAVE CURRENT (START) VALUES
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
C
C
N      VERTEX
      NHWIRV=NHWIR+1
      ISORT1(NHWIRV)=NHWIRV
      ISORT2(1,NHWIRV)=-200
C
C
C=======================================================================
C
N     FIRST ITERATE WITHOUT VERTEX CONSTRAINT
      INDMAX=NHFIT/8+1
      IF(INDMAX.GT.8) INDMAX=8
      INDFIT=0
      WHILE INDFIT.LT.INDMAX
         INDFIT=INDFIT+1
N    PARABOLA OR CIRCLE FIT
         PERFORM FTCURV
         IF LNOCON.EQ.1
         THEN
C NO CONVERGENCE AS INDICATED BY LOSS OF TOO MANY HITS
C STILL RETAIN THE EARLIER FIT
C IF IT IS THE ONE OBTAINED IN PROC STVCIR,
C CHI2 AND CURVATURE ERROR REMAIN THE OLD VALUES IN THE PATR BANK
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
C
C DO NOT ATTEMPT VERTEX CONSTRAINT
            PERFORM FITBNK
            RETURN
         CIF
         IF(SIG.LT.SIGMIN) XWHILE
         IF INDFIT.GE.2
         THEN
            PERFORM LLSTOP
            IF LSTOP.EQ.1
            THEN
C      PREVIOUS FIT ACCEPTED, RESTORE ITS RESULTS
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
               XWHILE
            CIF
         CIF
         IF(INDFIT.EQ.INDMAX) XWHILE
N      SAVE FIT RESULTS
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
N      HIT CLEANING
         PERFORM HITCLN
      CWHILE
C
C            ======  VERTEX CONSTRAINT  =======
      X0R=X0-XHF
      IF FV.LT.200. .AND. X0R.GT.XMIN .AND. X0R.LT.XMAX
      THEN
C        VERTEX CONSTRAINT (WEEK OR STRONG) HAS BEEN REQUESTED
C        ROUGH CHECK IF RUN VERTEX CONSISTENT WITH THE TRACK
         IF LFTYP.EQ.2
         THEN
            DVCHI2=((PAR1*X0R+PAR2)*X0R+PAR3-Y0)**2*WGHT0
         ELSE
            AAH=-X0R**2*CSI2GM
            FDBPR=1./(1.+PAR1*X0R*PAR2)
            SAG=SAGCIR(FDBPR,PAR1,AAH,SAGPR,1.E-4)
            DVCHI2=(SAG+PAR2*X0R+PAR3-Y0)**2*WGHT0
         CIF
         IF DVCHI2 .LT.  9.*SIG
         THEN
            ISORT2(1,NHWIRV)=-100
            PERFORM FTCURV
            IF(LNOCON.EQ.0) INDBIT=1024
         CIF
      CIF
C
N     SET UP PATR-BANK
      PERFORM FITBNK
      RETURN
C=======================================================================
C
N     *************************
N     *      F T C U R V      *
N     *************************
C
C
N      PARABOLA OR CIRCLE FIT
      PROC FTCURV
C
      LNOCON=0
N     GET EQUATIONS
N     WEIGHT VERTEX AS POINT OF PARABOLA
      KFLIP=3-KFLIP
      KITER=0
      WHILE KITER .LT. 3-LFTYP
         KITER=KITER+1
         X0R=X0-XHF
         IF ISORT2(1,NHWIRV).EQ.-100 .AND. X0R.GT.XMIN .AND. X0R.LT.XMAX
         THEN
N      VERTEX INCLUDED
            IF LFTYP.EQ.2
            THEN
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
            CIF
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
N      VERTEX OMITTED
            S0 = 0.
            S1 = 0.
            S2 = 0.
            S3 = 0.
            S4 = 0.
            S8 = 0.
            S7 = 0.
            S6 = 0.
            S5 = 0.
         CIF
         S00=S0
         FOR IHWIR=1,NHWIR
            IH=ISORT2(2,IHWIR)
            NNH=ISORT2(3,IHWIR)
            FOR JNH=1,NNH
               ISORT4(KFLIP,IH+JNH-1)=0
            CFOR
            IF ISORT3(IH).EQ.1 .OR. ISORT3(IH).EQ.-1.AND.NNH.GT.1
            THEN
               RESMIN=10000.
               FOR JNH=1,NNH
                  JH=IH+JNH-1
                  IF ISORT3(JH).EQ.1
                  THEN
                     IPCO=ISORT2(1,IHWIR)+(JNH-1)*HLDCO
                     XA = WRK(IPCO+3)
                     XAR=XA-XHF
                     IF XAR.GT.XMIN.AND.XAR.LT.XMAX
                     THEN
                        YA = WRK(IPCO+4)
                        IF LFTYP.EQ.2
                        THEN
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
                        CIF
                     ELSE
                        DF0=15000.
                     CIF
                     IF DF0.LT.RESMIN
                     THEN
                        RESMIN=DF0
                        DYDP1=DYDP1A
                        DYDP2=DYDP2A
                        DYRES=DYRESA
                        JHUSE=JH
                     CIF
                  CIF
               CFOR
               IF RESMIN.LT.RESCUT
               THEN
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
               CIF
            CIF
         CFOR
         NHF1=S0-S00+.1
         IF NHF1.LT.6 .OR. NHF1.LT.NHPOT/2
         THEN
            LNOCON=1
            XWHILE
         CIF
         NHFIT=NHF1
         DEG   = S0 - S00 - 3.
N     CURVATURE ERROR
         DET = (S8*S0-S1*S1)*S4 + (S2*S1-S3*S0)*S3 + (S3*S1-S2*S8)*S2
         SIG11 = (S8*S0 - S1*S1)/DET
C
N        SOLVE EQUATIONS
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
         IF XX22.GT.XX32
         THEN
            XX23 = XX23 / XX22
            YY2  = YY2  / XX22
            PARR3 = (YY3 - XX32*YY2) / (XX33 - XX32*XX23)
            PARR2 = YY2 - XX23*PARR3
         ELSE
            XX33 = XX33 / XX32
            YY3  = YY3  / XX32
            PARR3 = (YY2 - XX22*YY3) / (XX23 - XX22*XX33)
            PARR2 = YY3 - XX33*PARR3
         CIF
         PARR1 = YY1 - XX12*PARR2 - XX13*PARR3
C
         IF LFTYP.EQ.2
         THEN
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
         CIF
      CWHILE
C  END ITERATION DONE IN CASE OF CIRCLE FIT ONLY
C
C
      IF LNOCON.EQ.0
      THEN
N     CALC. CHISQ + SOLVE L/R AMBIGUITY
         CHISQ = 0.
         NHF1=0
         FOR IHWIR=1,NHWIR
            IRESHT(IHWIR)=-1
            IH=ISORT2(2,IHWIR)
            NNH=ISORT2(3,IHWIR)
            IF ISORT3(IH).GE.0 .OR. ISORT3(IH).EQ.-1.AND.NNH.GT.1
            THEN
               RESMIN=10000.
               FOR JNH=1,NNH
                  JH=IH+JNH-1
                  IF ISORT3(JH).GE.0
                  THEN
                     IFLG=ISORT3(JH)
                     IPCO=ISORT2(1,IHWIR)+(JNH-1)*HLDCO
                     XR= WRK(IPCO+3)-XHF
                     IF XR.GT.XMIN.AND.XR.LT.XMAX
                     THEN
                        Y = WRK(IPCO+4)
                        IF LFTYP.EQ.2
                        THEN
                           DF0=ABS(Y-(PAR1*XR+PAR2)*XR-PAR3)
                        ELSE
                           AAH=-XR**2*CSI2GM
                           FDBPR=1./(1.+PAR1*XR*PAR2)
                           SAG=SAGCIR(FDBPR,PAR1,AAH,SAGPR,1.E-4)
                           DF0=ABS(Y-SAG-PAR2*XR-PAR3)
                        CIF
                        IF(DF0.LT.RESMIN) RESMIN=DF0
                     CIF
                  CIF
               CFOR
               IF RESMIN.LT.5000.
               THEN
                  IRESHT(IHWIR)=RESMIN*1.E6
                  IF IFLG.EQ.1
                  THEN
                     CHISQ=CHISQ+RESMIN**2
                     NHF1=NHF1+1
                  CIF
               CIF
            CIF
         CFOR
         IF(NHF1.LT.NHFIT-3) LNOCON=1
         SIG    =      CHISQ  / DEG
      CIF
      CPROC
C=======================================================================
      PROC HITCLN
C      LABEL HITS NOT TO BE USED IN THE NEXT ITRATION
N       SQRT(CHI2) OF VERTEX
C  VERTEX NOT INCLUDED IN THE ITERATIVE PART
C  IN THE CURRENT VERSION
CV       X0R=X0-XHF
CV       IF LFTYP.EQ.2
CV       THEN
CV          DFVERT=((PAR1*X0R+PAR2)*X0R+PAR3-Y0)*SQRT(WGHT0)
CV       ELSE
CV          AAH=-X0R**2*CSI2GM
CV          FDBPR=1./(1.+PAR1*X0R*PAR2)
CV          SAG=SAGCIR(FDBPR,PAR1,AAH,SAGPR,1.E-4)
CV          DFVERT=(SAG+PAR2*X0R+PAR3-Y0)*SQRT(WGHT0)
CV       CIF
CV       IRESHT(NHWIRV)=ABS(DFVERT)*1.E6
C-------------------------------------------------------------
C
N       SORT HITS ACCORDING TO RESIDUALS
C  EXCLUDE THE INDFIT LARGEST RESIDUAL HITS,
C  RESTORE THE OTHERS (EXLUDED FOR EVER HITS NOT COUNTED)
C
CV       CALL SHELL9(IRESHT,ISORT1,NHWIRV)
         CALL SHELL9(IRESHT,ISORT1,NHWIR)
         KOMIT=0
CV       FOR J1=1,NHWIRV
         FOR J1=1,NHWIR
CV          IHWIR=ISORT1(NHWIRV+1-J1)
            IHWIR=ISORT1(NHWIR+1-J1)
            IPCO=ISORT2(1,IHWIR)
            IF IPCO.NE.-100 .AND. IPCO.NE.-200
            THEN
N     HIT, NOT VERTEX
               NNH=ISORT2(3,IHWIR)
               IH=ISORT2(2,IHWIR)
               LFLG=0
               FOR JNH=1,NNH
                  IHA=IH+JNH-1
                  IQA=ISORT3(IHA)
                  IF IQA.GT.-1
                  THEN
                     IF LFLG.EQ.0
                     THEN
                        LFLG=1
                        KOMIT=KOMIT+1
                     CIF
                     IF KOMIT.LE.INDFIT
                     THEN
                        ISORT3(IHA)=0
                     ELSE
                        ISORT3(IHA)=1
                     CIF
                  CIF
               CFOR
CV          ELSE
C   VERTEX;   DOES NOT OCCOUR IN THE CURRENT VERSION
CV             KOMIT=KOMIT+1
CV             IF(KOMIT.LE.INDFIT) WGHT0=WGHT0*.01
            CIF
         CFOR
      CPROC
C=======================================================================
      PROC LLSTOP
         IF INDFIT.LE.6
         THEN
            INDCK=INDFIT-1
         ELSE
            INDCK=5
         CIF
         ICHCK=NCHECK(INDCK)
         WHILE SIGLST.LT.RCHECK(ICHCK,1,INDCK)
            ICHCK=ICHCK-1
         CWHILE
         IF(ICHCK.LT.1) ICHCK=1
         IF SIG/SIGLST.GT.RCHECK(ICHCK,2,INDCK)
         THEN
            LSTOP=1
         ELSE
            LSTOP=0
         CIF
      CPROC
C=======================================================================
C
C
N     *************************
N     *      F I T B N K      *
N     *************************
C
C
N     SET UP FIT-BANK
      PROC FITBNK
C
N     START + END POINTS
         XST=XHF+XMIN
         IF(XST.LT.XREGA) XST=XREGA
         XSTR=XST-XHF
         XEN=XHF+XMAX
         IF(XEN.GT.XREGB) XEN=XREGB
         XENR=XEN-XHF
         IF LFTYP.EQ.2
         THEN
            YST  = (PAR1 *XSTR+ PAR2 )*XSTR+ PAR3
            YEN  = (PAR1 *XENR+ PAR2 )*XENR+ PAR3
N     DIRECTION AT START + END POINT
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
         CIF
         DXST = 1./SQRT(TGST**2+1.)
         DYST = DXST * TGST
         DXEN = 1./SQRT(TGEN**2+1.)
         DYEN = DXEN * TGEN
C
C
C
N     COPY TRACK BANK
         HPTR0 = HPFREE
         CALL MVCL(IWRK(HPTR0),0,IDATA(IPTR+1),0,4*LDTR)
C
N     FILL FIT-BANK
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
C
         WRK (IP+23) = SQRT(SIG)
         WRK (IP+26) = SQRT(SIG*SIG11/CSI2GM)
         IF(LFTYP.EQ.2) WRK(IP+26)=WRK(IP+26)*2./CSI2GM
C
C        IWRK(IP+18) = LFTYP
C EVEN IF PARABOLA FIT WAS DONE, CIRCLE PARAMETERS ARE STORED
         IWRK(IP+18) = 1
C
         PAR1=CUROUT*SQRT(CSI2GM)
         SIGNC=SIGN(1.,CUROUT)
         ACURV=ABS(CUROUT)
         A=((XHF-XOR)*PAR2-PAR3+YOR)/SQRT(CSI2GM)
         B=(XHF-XOR+(PAR3-YOR)*PAR2)/SQRT(CSI2GM)
         FDBPR=1./ACURV+SIGNC*A
         IF FDBPR.LT.100.
         THEN
            DIMP=-1./ACURV+SQRT(FDBPR**2+B**2)
         ELSE
            DIMP=SIGNC*A+SAGCIR(1./(1.+CUROUT*A),ACURV,B**2,SP,1.E-4)
         CIF
         FDBPR=1.+DIMP*ACURV
         IF FDBPR.LT.ACURV*1.E-3
         THEN
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
C
            IF LDTR.GE.55.AND.NHFIT.GE.10.AND.LNOCON.EQ.0
            THEN
               LCOVAR=1
            ELSE
               LCOVAR=0
            CIF
            IF LAND(MODXYV,32) .NE. 0
            THEN
               LJHTLU=1
            ELSE
               LJHTLU=0
            CIF
C           CALCULATE COVARIANCE MATRIX AND/OR UPDATE JHTL BANK
            PERFORM COVAR
C
            IWRK(IPRES+10)=1
C
         CIF
C
N     PUT RESULT INTO PATR-BANK
         CALL MVCL(IDATA(IPTR+1),0,IWRK(HPTR0),0,4*LDTR)
C
C
      CPROC
C-----------------------------------------------------------------------
      PROC SHFROT
C  FIT SHIFT AND ROTATION ONLY (CURVATURE KEPT FIXED)
         S0 = 0.
         S1 = 0.
         S2 = 0.
         S3 = 0.
         S4 = 0.
         FOR IHWIR=1,NHWIR
            IH=ISORT2(2,IHWIR)
            NNH=ISORT2(3,IHWIR)
            FOR JNH=1,NNH
               ISORT4(KFLIP,IH+JNH-1)=0
            CFOR
            IF ISORT3(IH).EQ.1 .OR. ISORT3(IH).EQ.-1.AND.NNH.GT.1
            THEN
               RESMIN=10000.
               FOR JNH=1,NNH
                  JH=IH+JNH-1
                  IF ISORT3(JH).EQ.1
                  THEN
                     IPCO=ISORT2(1,IHWIR)+(JNH-1)*HLDCO
                     XA = WRK(IPCO+3)
                     XAR=XA-XHF
                     IF XAR.GT.XMIN .AND. XAR.LT.XMAX
                     THEN
                        YA = WRK(IPCO+4)
                        IF LFTYP.EQ.2
                        THEN
                           DYRESA=YA-(PAR1*XAR+PAR2)*XAR-PAR3
                        ELSE
                           AAH=-XAR**2*CSI2GM
                           FDBPR=1./(1.+PAR1*XAR*PAR2)
                           SAG=SAGCIR(FDBPR,PAR1,AAH,SAGPR,1.E-4)
                           DYRESA=YA-SAG-PAR2*XAR-PAR3
                        CIF
                        DF0=ABS(DYRESA)
                     ELSE
                        DF0=15000.
                     CIF
                     IF DF0.LT.RESMIN
                     THEN
                        RESMIN=DF0
                        XR=XAR
                        DYRES=DYRESA
                        JHUSE=JH
                     CIF
                  CIF
               CFOR
               IF RESMIN.LT.DISCUT
               THEN
                  S0=S0+1.
                  S1=S1+XR
                  S2=S2+XR**2
                  S3=S3+DYRES
                  S4=S4+DYRES*XR
                  ISORT4(KFLIP,JHUSE)=1
               CIF
            CIF
         CFOR
         NHFIT=S0+.1
         IF NHFIT.GE.4
         THEN
            S12=S1/S2
            S42=S4/S2
            DA=(S3-S1*S42)/(S0-S1*S12)
            DB=S42-S12*DA
            PAR3=PAR3+DA
            PAR2=PAR2+DB
            CSI2GM=1.+PAR2**2
            IF LFTYP.EQ.2
            THEN
               CUROUT =-PAR1 * 2./ (SQRT(CSI2GM)*CSI2GM)
            ELSE
               CUROUT=PAR1/SQRT(CSI2GM)
               XMIN=PAR2/PAR1-CKAPP/ABS(CUROUT)
               XMAX=PAR2/PAR1+CKAPP/ABS(CUROUT)
            CIF
         CIF
      CPROC
C=======================================================================
      PROC STVCIR
C  TRY TO FIND STARTING VALUES FOR CIRCLE FIT
C  THIS PART IS EXECUTED FOR ONLY A VERY SMALL FRACTION OF THE TRACKS
C  JUST LOOP UNTIL 10, NO STOP CONDITION CHECKED
      ISTV1=0
      DISCUT=400.
      WHILE ISTV1.LT.10
         ISTV1=ISTV1+1
C  FIT PARABOLA P1*X**2+P2*X+P3 TO RESIDUALS & MODIFY CIRCLE PARAMETERS
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
         FOR IHWIR=1,NHWIR
            IH=ISORT2(2,IHWIR)
            NNH=ISORT2(3,IHWIR)
            FOR JNH=1,NNH
               ISORT4(KFLIP,IH+JNH-1)=0
            CFOR
            IF ISORT3(IH).EQ.1 .OR. ISORT3(IH).EQ.-1.AND.NNH.GT.1
            THEN
               RESMIN=10000.
               FOR JNH=1,NNH
                  JH=IH+JNH-1
                  IF ISORT3(JH).EQ.1
                  THEN
                     IPCO=ISORT2(1,IHWIR)+(JNH-1)*HLDCO
                     XA = WRK(IPCO+3)
                     XAR= WRK(IPCO+3)-XHF
                     IF XAR.GT.XMIN.AND.XAR.LT.XMAX
                     THEN
                        YA = WRK(IPCO+4)
                        AAH=-XAR**2
                        FDBPR=1./(1.+PAR1*XAR*PAR2)
                        SAG=SAGCIR(FDBPR,PAR1,AAH*CSI2GM,SAGPR,1.E-4)
                        DYRESA=YA-SAG-PAR2*XAR-PAR3
                        DF0=ABS(DYRESA)
                     ELSE
                        DF0=15000.
                     CIF
                     IF DF0.LT.RESMIN
                     THEN
                        RESMIN=DF0
                        X=XAR
                        DYRES=DYRESA
                        JHUSE=JH
                     CIF
                  CIF
               CFOR
               IF RESMIN.LT.DISCUT
               THEN
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
               CIF
            CIF
         CFOR
         NHFIT=S0+.1
C
C
         IF(NHFIT.LT.5) RETURN
C
C
N        SOLVE EQUATIONS
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
         IF XX22.GT.XX32
         THEN
            XX23 = XX23 / XX22
            YY2  = YY2  / XX22
            PARR3 = (YY3 - XX32*YY2) / (XX33 - XX32*XX23)
            PARR2 = YY2 - XX23*PARR3
         ELSE
            XX33 = XX33 / XX32
            YY3  = YY3  / XX32
            PARR3 = (YY2 - XX22*YY3) / (XX23 - XX22*XX33)
            PARR2 = YY3 - XX33*PARR3
         CIF
         PARR1 = YY1 - XX12*PARR2 - XX13*PARR3
C
         XAR=-.7*RRMI
         IF(XAR.LT..8*XMIN) XAR=.8*XMIN
         XBR= .7*RRPL
         IF(XBR.GT..8*XMAX) XBR=.8*XMAX
         IF -XAR.LT.XBR
         THEN
            XBR=-XAR
         ELSE
            XAR=-XBR
         CIF
C
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
         IF SAG**2.GT.(CKAPP*XBR)**2*C2
         THEN
            SAG=SIGN(CKAPP*XBR*SQRT(C2),SAG)
            YC=.5*(YA+YB)+SAG
         CIF
         PAR3=YC
         P1=2.*SAG/(C2*XBR**2-SAG**2)
         CUROUT=P1/SQRT(C2*(1.+(P1*XBR)**2))
         IF(ABS(CUROUT).LT.1.E-8) CUROUT= SIGN(1.E-8,CUROUT)
         PAR2=P2/(1.+SAG*P1)
         CSI2GM=1.+PAR2**2
         PAR1=CUROUT*SQRT(CSI2GM)
         XMIN=PAR2/PAR1-CKAPP/ABS(CUROUT)
         XMAX=PAR2/PAR1+CKAPP/ABS(CUROUT)
C
         DISCUT=.5*DISCUT
         IF(DISCUT.LT.10.) DISCUT=10.
      CWHILE
      CPROC
C-----------------------------------------------------------------------
      PROC COVAR
C
C      CALCULATE COVARIANCE MATRIX AND/OR UPDATE JHTL BANK
C
         SAMFI=SINALP*CSROT-COSALP*SNROT
         CAMFI=COSALP*CSROT+SINALP*SNROT
C
C        UPDATE OF JHTL FOR HITS   N O T   USED IN THE FIT
         IP00=2*IDATA(IQJETC)+100
         FOR IHWIR=1,NHWIR
            IH=ISORT2(2,IHWIR)
            NNH=ISORT2(3,IHWIR)
            FOR JNH=1,NNH
               JH=IH+JNH-1
               IF ISORT4(KFLIP,JH).NE.1
               THEN
                  IPCO=ISORT2(1,IHWIR)+(JNH-1)*HLDCO
                  X=WRK(IPCO+3)
                  Y=WRK(IPCO+4)
                  A=(X-XOR)*CAMFI+(Y-YOR)*SAMFI
                  B=(X-XOR)*SAMFI-(Y-YOR)*CAMFI
                  FDBPR=1./ACURV+DIMP-A
                  IF FDBPR.LT.100.
                  THEN
                     SAG=SQRT(FDBPR**2+B**2)
                     RESA=SAG-1./ACURV
                  ELSE
                     FDBPR=1./(1.+ACURV*(DIMP-A))
                     SAG=SAGCIR(FDBPR,ACURV,B**2,SP,1.E-4)
                     RESA=DIMP-A+SAG
                  CIF
C
C                 IWRK(IPCO+10)=1
                  WRK(IPCO+13)=RESA
                  IF LJHTLU.EQ.1
                  THEN
                     IP1   =IWRK(IPCO+1)
                     LBSIDE=IWRK(IPCO+2)
                     IPHL=IPJHTL+2+(IP1-IP00)/4
                     LB=IDATA(IPHL)
                     IDST=ABS(RESA)*5.
                     IF(IDST.GT.31) IDST=31
                     IDST=SHFTL(IDST,11)
                     IDST=LOR(IDST,1024)
                     IF(LBSIDE.EQ.1) IDST=LOR(IDST,256)
                     ITR1=LAND(SHFTR(LB,17),127)
                     IF ITR1.EQ.ITRK
                     THEN
                        IDATA(IPHL)=LOR(LAND(LB,MASK1),SHFTL(IDST,16))
                     ELSE
                        IDATA(IPHL)=LOR(LAND(LB,MASK2),      IDST    )
                     CIF
                  CIF
               CIF
            CFOR
         CFOR
C
         CHISQ=0.
C
N      VERTEX OMITTED
         NHF1=0
         S0D= 0.D0
         S1D= 0.D0
         S2D= 0.D0
         S3D= 0.D0
         S4D= 0.D0
         S8D= 0.D0
         FOR IHWIR=1,NHWIR
            IH=ISORT2(2,IHWIR)
            NNH=ISORT2(3,IHWIR)
            FOR JNH=1,NNH
               JH=IH+JNH-1
               IF ISORT4(KFLIP,JH).EQ.1
               THEN
                  IPCO=ISORT2(1,IHWIR)+(JNH-1)*HLDCO
                  X=WRK(IPCO+3)
                  Y=WRK(IPCO+4)
                  A=(X-XOR)*CAMFI+(Y-YOR)*SAMFI
                  B=(X-XOR)*SAMFI-(Y-YOR)*CAMFI
                  FDBPR=1./ACURV+DIMP-A
                  IF FDBPR.LT.100.
                  THEN
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
                  CIF
C
                  NHF1=NHF1+1
                  CHISQ=CHISQ+RESA**2
                  S0D=S0D+DRDP3**2
                  S1D=S1D+DRDP3*DRDP2
                  S2D=S2D+DRDP1*DRDP3
                  S3D=S3D+DRDP1*DRDP2
                  S4D=S4D+DRDP1**2
                  S8D=S8D+DRDP2**2
C
                  IWRK(IPCO+10)=0
                  WRK(IPCO+13)=RESA
                  IF LJHTLU.EQ.1
                  THEN
                     IP1   =IWRK(IPCO+1)
                     LBSIDE=IWRK(IPCO+2)
                     IPHL=IPJHTL+2+(IP1-IP00)/4
                     LB=IDATA(IPHL)
                     IDST=ABS(RESA)*5.
                     IF(IDST.GT.31) IDST=31
                     IDST=SHFTL(IDST,11)
                     IF(LBSIDE.EQ.1) IDST=LOR(IDST,256)
                     ITR1=LAND(SHFTR(LB,17),127)
                     IF ITR1.EQ.ITRK
                     THEN
                        IDATA(IPHL)=LOR(LAND(LB,MASK1),SHFTL(IDST,16))
                     ELSE
                        IDATA(IPHL)=LOR(LAND(LB,MASK2),      IDST    )
                     CIF
                  CIF
               CIF
            CFOR
         CFOR
         IF LCOVAR.EQ.1
         THEN
            IF NHF1.LT.10.OR.NHF1.NE.NHFIT
            THEN
               PRINT 6781,NRUN,NEV,ITRK,NHFIT,NHF1
6781           FORMAT(' RUN,EV,TRK,NHFIT,NHF1',I7,I6,I3,2I5)
            ELSE
               DETD=(S8D*S0D-S1D*S1D)*S4D+
     +        (S2D*S1D-S3D*S0D)*S3D+(S3D*S1D-S2D*S8D)*S2D
               FACT=CHISQ/(NHFIT-3)/DETD
C
               IWRK(IP+2) =LOR(IWRK(IP+2),2048)
               WRK(IP+49)=CHISQ/.115**2
               WRK(IP+50)=(S8*S0-S1**2)*FACT
               WRK(IP+51)=(S1*S2-S0*S3)*FACT
               WRK(IP+52)=(S4*S0-S2**2)*FACT
               WRK(IP+53)=(S3*S1-S8*S2)*FACT
               WRK(IP+54)=(S2*S3-S1*S4)*FACT
               WRK(IP+55)=(S8*S4-S3**2)*FACT
            CIF
         CIF
      CPROC
      END
C   09/06/83 708122254  MEMBER NAME  ZCFTNW0  (JADEGS)      SHELTRAN
      SUBROUTINE ZCFTNW(NRUN,NEVT,ITRK,TGTH,ZVERT,NZHIT,IZCHMB,AZCHMB)
C             J.SPITZER    15/7/87
C      PRINT STATEMENT FOR USE OF Z-CHAMBER HITS IN FIT  12.8.87  J.S.
C    PASS Z-CHAMBER DATA FOR A SINGLE TRACK TO JFTNEW (OR JFETCH).
C    AT REQUESTING DATA FOR THE FIRST TRACK IN THE EVENT,
C    SUSAN'S ZCDATA IS CALLED WHICH CALCULATES THE COORDINATES
C    FOR THE Z-CH HITS AND PERFORMS THE TRACK-HIT ASSOCIATION.
C    THE Z-COORDINATES ARE CORRECTED IN THIS ROUTINE IN ORDER
C    TO MATCH THE JET-CHAMBER EXTRAPOLATIONS ON THE AVERAGE.
C
      DIMENSION IZCHMB(3,2),AZCHMB(3,2)
      DATA NRUNLS/-99/,NEVTLS/-99/,JZERR/0/
C
      COMMON/CZDATA/JZDATA(64),IZDATA(3,8,64),RZDATA(3,8,64)
C*****              JZDATA(NW)        NUMBER OF HITS ON WIRE NW
C                   IZDATA(*,         ADC1,ADC2,TDC
C                            NH,      HIT NUMBER (1--8)
C                               NW)   WIRE NUMBER (1--64)
C                   RZDATA(*,         Z-DZ,Z+DZ,PHI
C*****                       NH,NW)   HIT NO, WIRE NO.
      COMMON/CZTRK /RZTRK(5,100),IZTRK(3,2,100)
C*****              RZTRK(*,NTR)      ZI1,PHI1,ZI2,PHI2,TANTH
C                                         FOR TRACK NUMBER NTR
C                   IZTRK(*,          IM(=1,-DZ,=2,+DZ),NH,NW
C                           IL,       LAYER(=1,INNER,=2,OUTER)
C*****                         NTR)   TRACKNO
C
      IF NEVT.NE.NEVTLS .OR. NRUN.NE.NRUNLS
      THEN
       IF NEVTLS.LT.0
       THEN
        WRITE(6,1111)
1111  FORMAT(' Z-CHAMBER HITS ARE USED IN FITTING WITH ZSRFTV ')
       CIF
         NEVTLS=NEVT
         NRUNLS=NRUN
         CALL ZCDATA(NTPAT,NZHT,NASS1,NASS2,JZERR)
      CIF
C
      NZHIT=0
      IF(JZERR.NE.0) RETURN
      IMIR=IZTRK(1,1,ITRK)
      IF IMIR.GT.0
      THEN
         IHT=IZTRK(2,1,ITRK)
         IWR=IZTRK(3,1,ITRK)
         PHI=RZTRK(2,ITRK)
         RR=(RZTRK(1,ITRK)-ZVERT)/TGTH
         XX=RR*COS(PHI)
         YY=RR*SIN(PHI)
         ZZ=RZDATA(IMIR,IHT,IWR)-13.77
         IF XX.LT.0.
         THEN
            IF ZZ.LT.-270.
            THEN
               ZZ=ZZ-10.9-1.0414E-2*(ZZ+270.)+7.93616E-6*(ZZ+270.)**2
            ELSE
               ZZ=ZZ-10.9+1.98198E-2*(ZZ+270.)-5.13690E-6*(ZZ+270.)**2
            CIF
            IF NRUN.LT.24200
            THEN
               IF ZZ.LT.-270.
               THEN
                  ZZ=ZZ+9.2+5.25E-3*(ZZ+270.)
               ELSE
                  ZZ=ZZ+9.2-1.49E-2*(ZZ+270.)+5.1E-6*(ZZ+270.)**2
               CIF
               ZZ=ZZ-4.7E-2+9.87E-5*ZZ
            CIF
         ELSE
            IF ZZ.LT.-270.
            THEN
               ZZ=ZZ-2.995-3.42774E-3*(ZZ+270.)+1.3112E-5*(ZZ+270.)**2
            ELSE
               ZZ=ZZ-2.995+7.2565E-3*(ZZ+270.)
            CIF
            IF NRUN.GE.24200
            THEN
               ZZ=ZZ+1.-2.8E-3*ZZ
               ZZ=ZZ-.076+2.67E-4*ZZ
            ELSE
               IF NRUN.GE.20275
               THEN
                  IF ZZ.LT.0.
                  THEN
                     ZZ=ZZ-8.09E-3*ZZ-4.5E-6*ZZ**2
                  ELSE
                     ZZ=ZZ-8.09E-3*ZZ+5.85E-6*ZZ**2
                  CIF
               ELSE
                  IF ZZ.LT.480.
                  THEN
                     ZZ=ZZ+7.75-1.367E-2*ZZ-1.309E-5*ZZ**2
                  ELSE
                     ZZ=ZZ-1.34+2.556E-2*(ZZ-500.)-4.25E-5*(ZZ-500.)**2
                  CIF
               CIF
               ZZ=ZZ-.085+1.59E-4*ZZ
            CIF
         CIF
         NZHIT=NZHIT+1
         IZCHMB(1,NZHIT)=IWR
         IZCHMB(2,NZHIT)=1
         IZCHMB(3,NZHIT)=IMI
         AZCHMB(1,NZHIT)=XX
         AZCHMB(2,NZHIT)=YY
         AZCHMB(3,NZHIT)=ZZ
      CIF
      IMIR=IZTRK(1,2,ITRK)
      IF IMIR.GT.0
      THEN
         IHT=IZTRK(2,2,ITRK)
         IWR=IZTRK(3,2,ITRK)
         PHI=RZTRK(4,ITRK)
         RR=(RZTRK(3,ITRK)-ZVERT)/TGTH
         XX=RR*COS(PHI)
         YY=RR*SIN(PHI)
         ZZ=RZDATA(IMIR,IHT,IWR)-14.06
         IF XX.LT.0.
         THEN
            IF ZZ.LT.-270.
            THEN
               ZZ=ZZ-10.9-1.0414E-2*(ZZ+270.)+7.93616E-6*(ZZ+270.)**2
            ELSE
               ZZ=ZZ-10.9+1.98198E-2*(ZZ+270.)-5.13690E-6*(ZZ+270.)**2
            CIF
            IF NRUN.LT.24200
            THEN
               IF ZZ.LT.-270.
               THEN
                  ZZ=ZZ+9.2+5.25E-3*(ZZ+270.)
               ELSE
                  ZZ=ZZ+9.2-1.49E-2*(ZZ+270.)+5.1E-6*(ZZ+270.)**2
               CIF
               ZZ=ZZ+4.7E-2-9.87E-5*ZZ
            CIF
         ELSE
            IF ZZ.LT.-270.
            THEN
               ZZ=ZZ-2.995-3.42774E-3*(ZZ+270.)+1.3112E-5*(ZZ+270.)**2
            ELSE
               ZZ=ZZ-2.995+7.2565E-3*(ZZ+270.)
            CIF
            IF NRUN.GE.24200
            THEN
               ZZ=ZZ+1.-2.8E-3*ZZ
               ZZ=ZZ+.076-2.67E-4*ZZ
            ELSE
               IF NRUN.GE.20275
               THEN
                  IF ZZ.LT.0.
                  THEN
                     ZZ=ZZ-8.09E-3*ZZ-4.5E-6*ZZ**2
                  ELSE
                     ZZ=ZZ-8.09E-3*ZZ+5.85E-6*ZZ**2
                  CIF
               ELSE
                  IF ZZ.LT.480.
                  THEN
                     ZZ=ZZ+7.75-1.367E-2*ZZ-1.309E-5*ZZ**2
                  ELSE
                     ZZ=ZZ-1.34+2.556E-2*(ZZ-500.)-4.25E-5*(ZZ-500.)**2
                  CIF
               CIF
               ZZ=ZZ+.085-1.59E-4*ZZ
            CIF
         CIF
         NZHIT=NZHIT+1
         IZCHMB(1,NZHIT)=IWR
         IZCHMB(2,NZHIT)=2
         IZCHMB(3,NZHIT)=IMI
         AZCHMB(1,NZHIT)=XX
         AZCHMB(2,NZHIT)=YY
         AZCHMB(3,NZHIT)=ZZ
      CIF
      RETURN
      END
C   09/06/83 803181311  MEMBER NAME  ZSRFTV0  (JADEGS)      SHELTRAN
C   09/06/83 803181238  MEMBER NAME  ZSRFTV   (S)           SHELTRAN
      SUBROUTINE ZSRFTV(MODE,IOPT)
C-----------------------------------------------------------------------
C                                   J. SPITZER 22/4/87
C                    UPDATED TO GIVE COMMON Z    1/6/87  J.S.
C    18.3.88   PROPER RUN NUMBER HANDLING USING LDATYP      E E
C    22.2.88   MVC CHANGED TO MVCL (256 BYTES NOT ENOUGH!)  J.H./J.O.
C
C       A GENERAL S-Z FIT ROUTINE
C       S = TRACK LENGTH ALONG THE CIRCLE COUNTED FROM THE
C           FIRST POINT IN THE DIRECTION OF THE LAST ONE
C
C      MODE   = 0 : OVERWRITE OLD PATR-BANK WITH NEW RESULTS
C      MODE   = 1 : CREATE NEW PATR-BANK WITH NEW RESULTS
C
C      IOPT =   1 : S-Z FIT SEPARATELY FOR ALL TRACKS
C      IOPT =   2 : S-Z FIT SEPARATELY FOR ALL TRACKS AND SUBSEQUENTLY
C                   A COMMON S-Z FIT FOR THOSE ONES WHICH
C                   EXTRAPOLATE WITHIN 15 MM TO THE RUN VERTEX IN R-PHI
C                   AND HAVE  | Z(R=0) | < 800 MM
C      *****************************************************************
C      *  THE FOLLOWIG OPTION NEEDS FILLING OF A COMMON IN ADDITION !!!*
C      *****************************************************************
C      IOPT =   4 : COMMON S-Z FITS FOR USER SPECIFIED (UPTO 5) SETS
C                   OF TRACKS WITH USER SPECIFIED COMMON (X,Y) POINTS
C                   IN R-PHI (OR SINGLE TRACK S-Z FIT, SEE LATER).
C                   THE SINGLE TRACK S-Z FITS ARE ONLY DONE FOR THOSE
C                   TRACKS WHICH APPEAR IN THE SETS.
C                   IF A TRACK IS NOT WITHIN 15 MM TO THE SPECIFIED
C                   (X,Y) POINT OR | Z(X,Y) | > 1600 MM, IT WILL BE
C                   DISCARDED.
C
C    TRACK SELECTION PARAMETERS FOR COMMON Z FIT ARE IN /CCMZCT/
C
C    ( IOPT <= IOPT+8 CREATES A SPECIAL BANK IN ADDITION )
C
C-----------------------------------------------------------------------
C
      IMPLICIT INTEGER*2 (H)
C
C-----------------------------------------------------------------------
C *****************************************************************
C *  THE COMMON TO BE FILLED BY THE USER IF IOPT=4 IS REQUESTED   *
C *****************************************************************
C
      COMMON/CZSSTE/ NSETZS,NTSTZS(5),KTSTZS(100,2,5),XYSTZS(2,5),
     +SQCHZS(3,5)
C
C NSETZS : NUMBER OF TRACK SETS FOR WHICH COMMON FIT IS TO BE DONE
C NTSTZS(1..NSETZS) : NUMBER OF TRACKS IN EACH SET
C KTSTZS(1..,1,I) TRACK SEQUENCE NUMBERS FOR SET "I"
C XYSTZS(1,I), XYSTZS(2,I) : COMMON X,Y FOR SET "I"
C
C FOR SINGLE TRACK FIT PLEASE SET     NSETZS=1, NTSTZS(1)=1,
C                                     KTSTZS(1,1,1)=ITRK (TR. SEQ. NR.)
C                              NOTHING ELSE NEEDS TO BE SET.
C
C  ***********
C  * ON EXIT *
C  ***********
C
C  KTSTZS(.,2,.)  IS A FLAG THAT TELLS YOU:
C       2 : TRACK WAS USED IN A COMMON Z-FIT
C       1 : SINGLE TRACK FIT WAS SUCCESSFUL BUT
C           THE TRACK WAS NOT USED IN A COMMON FIT
C       0 : TRACK WAS NOT CONSIDERED FOR REFIT
C      <0 : SINGLE TRACK FIT FAILED (TOO FEW USABLE HITS IN GENERAL)
C
C  THIS INFORMATION IS PROVIDED FOR IOPT=1 AND 2 AS WELL BUT
C  WITHOUT FILLING  KTSTZS(J,1,.)=J
C
C  SQCHZS(1,.) : SQUARE ROOT OF {SUM OF WEIGHTED RESIDUAL SQUARES
C                DIVIDED BY THE NUMBER OF DEGREES OF FREEDOM (LATTER=
C                NHITS-NTRACKS-1)} FOR COMMON FIT (.)
C                OR 0.0 IF NO COMMON FIT WAS PERFORMED.
C                THE (DIFFERENT FOR DL8 AND FADC) NORMALIZATION
C                TO GET THE CHI**2/DOF. IS LEFT FOR THE USER
C  SQCHZS(2,.) : NUMBER OF HITS USED IN THE COMMON FIT OR 0.0; REAL !!!
C  SQCHZS(3,.) : COMMON Z AT THE COMMON (X,Y)    (OR 0.0)
C
      COMMON/CCMZCT/ DIMPCT, ZCUTV, ZCUTVV, IZVCST(5)
C
C THIS COMMON IS BLOCK DATA SET IN   JADEBD
C
C     BLOCK DATA
C     COMMON/CCMZCT/ DIMPCT, ZCUTV, ZCUTVV, IZVCST(5)
C     DATA  DIMPCT /15./, ZCUTV /800./, ZCUTVV /1600./, IZVCST/5*0/
C     END
C=======================================================================
C
#include "cdata.for"
#include "calibr.for"
C
      COMMON/CWORK/NDIWRK,WRK(20000)
      DIMENSION IWRK(20000),HWRK(40000)
      EQUIVALENCE (WRK(1),IWRK(1),HWRK(1))
C
      INTEGER ITRREQ(100)
      DATA LBINIT /0/, IQPATR/0/, IQJHTL/0/, IQHEAD/0/, IQJETC/0/
      DATA NPRLIM/50/,KPRLIM/0/
C=======================================================================
C
      KOPT=IOPT
      IF(KOPT.GT.8) KOPT=KOPT-8
      IF KOPT.NE.1 .AND. KOPT.NE.2 .AND. KOPT.NE.4
      THEN
         IF KPRLIM.LT.NPRLIM
         THEN
            KPRLIM=KPRLIM+1
            WRITE(6,100) IOPT
100         FORMAT(' **** ZSRFTV CALLED WITH INVALID OPTION :',I8)
         CIF
         RETURN
      CIF
C
C
      IF KOPT.EQ.4
      THEN
C PRODUCE ARRAY OF ALL TRACKS REQUESTED; CHECK CONSISTENCY OF REQUEST
         IF NTSTZS(1).EQ.1
         THEN
            KTC=KTSTZS(1,1,1)
            IF KTC.LT.1 .OR. KTC.GT.100
            THEN
               LINCON=1
            ELSE
               LINCON=0
               NTRREQ=1
               ITRREQ(1)=KTC
               KTSTZS(1,2,1)=0
            CIF
         ELSE
            IF NSETZS.GE.1.AND.NSETZS.LE.5
            THEN
               NTRREQ=0
               LINCON=0
               FOR ISET=1,NSETZS
                  SQCHZS(1,ISET)=0.
                  SQCHZS(2,ISET)=0.
                  SQCHZS(3,ISET)=0.
                  NTSETI=NTSTZS(ISET)
                  IF NTSETI.LT.2 .OR. NTSETI.GT.100
                  THEN
                     LINCON=1
                     XFOR
                  CIF
                  FOR JT=1,NTSETI
                     KTC=KTSTZS(JT,1,ISET)
                     IF KTC.LT.1 .OR. KTC.GT.100
                     THEN
                        LINCON=1
                        XFOR
                     CIF
                     KTSTZS(JT,2,ISET)=0
                     LPRES=0
                     IF NTRREQ.GT.0
                     THEN
                        FOR J=1,NTRREQ
                           IF ITRREQ(J).EQ.KTC
                           THEN
                              LPRES=1
                              XFOR
                           CIF
                        CFOR
                     CIF
                     IF LPRES.EQ.0
                     THEN
                        IF NTRREQ.EQ.100
                        THEN
                           LINCON=1
                           XFOR
                        CIF
                        NTRREQ=NTRREQ+1
                        ITRREQ(NTRREQ)=KTC
                     CIF
                  CFOR
                  IF(LINCON.NE.0) XFOR
               CFOR
            ELSE
               LINCON=1
            CIF
         CIF
         IF LINCON.NE.0
         THEN
            IF KPRLIM.LT.NPRLIM
            THEN
               KPRLIM=KPRLIM+1
               WRITE(6,200)
200            FORMAT(' **** ZSRFTV: INVALID REQUEST IN /CZSSTE/')
            CIF
            RETURN
         CIF
      ELSE
         SQCHZS(1,1)=0.
         SQCHZS(2,1)=0.
         SQCHZS(3,1)=0.
         FOR J=1,100
            KTSTZS(J,2,1)=0
         CFOR
      CIF
C-----------------------------------------------------------------------
N     INITIALIZATION
      IF LBINIT .LE.0
      THEN
         LBINIT = 1
         IQPATR = IBLN('PATR')
         IQJHTL = IBLN('JHTL')
         IQHEAD = IBLN('HEAD')
         IQJETC = IBLN('JETC')
      CIF
C
C-----------------------------------------------------------------------
C
N     CHECK IF PATR-BANK
      IF(IDATA(IQPATR).LE.0) RETURN
C-----------------------------------------------------------------------
C
C     CREATE NEW PATR BANK IF REQUESTED
      IF MODE.EQ.1
      THEN
         IPPAT0 = IDATA(IQPATR)
         NBNK1  = IDATA(IPPAT0-2) - 1
         NWRD   = IDATA(IPPAT0)
         NBYTE  = NWRD*4
         CALL CCRE(IPPATR,'PATR',NBNK1,NWRD,IERR)
         IF IERR.NE.0
         THEN
            PRINT 2900, IERR
 2900       FORMAT(' CREATION OF NEW PATR-BANK RESULTED IN ERROR',I3)
            RETURN
         CIF
N        COPY CONTENTS OF 'PATR'-BANK
         CALL MVCL(IDATA(IPPATR+1),0,IDATA(IPPAT0+1),0,NBYTE)
      CIF
C-----------------------------------------------------------------------
C
      IPPATR = IDATA(IQPATR)
      IPTR   = IDATA(IPPATR+1) + IPPATR
      LDTR   = IDATA(IPPATR+3)
      NTR    = IDATA(IPPATR+2)
C
N     CHECK IF 1 TRACK
      IF(NTR.LT.1) RETURN
C
      IF NTR.GT.100
      THEN
         IF KPRLIM.LT.NPRLIM
         THEN
            KPRLIM=KPRLIM+1
            WRITE(6,300) NTR
300         FORMAT(' **** ZSRFTV : NUMBER OF TRACKS IN PATR BANK :',
     +      I4,'. FIRST 100 WILL BE CONSIDERED.')
         CIF
         NTR=100
      CIF
C
C-----------------------------------------------------------------------
C
C GET LATEST AMPLITUDE CALIBRATION
      CALL JRECAL(IERR)
      IF IERR.NE.0
      THEN
         PRINT 6784,IERR
 6784    FORMAT(' *** EROR IN JRECAL',I3)
         RETURN
      CIF
C-----------------------------------------------------------------------
C
N     RECALIBRATE Z-COORDINATES
      IPJETC = IDATA(IQJETC)
      IPJHTL = IDATA(IQJHTL)
C
C MODEZ=1 MEANS    CALIBRATION ONLY
C
      MODEZ  = 1
      CALL ZSFIT(IPJETC,IDATA(IPJETC-1),IPJHTL,IPPATR,MODEZ)
C
C
C=======================================================================
C
C     COLLECTION OF HIT DATA IN /CWORK/ AND SINGLE TRACK FITS
C
C DIMENSION OF WRK(.)
      NDIWRK=20000
C NUMBER OF TRACKS STORED IN /CWORK/
      NTRKS=0
C POINTER TO TRACK DATA THAT STORES THE STRUCTURE OF WRK(.) ETC.
      IDTR2=1
C LENGTH OF ABOVE DATA PRO TRACK
      LDTR2=11
C LENGTH OF HIT AND SUBSEQUENT TRACK DATA IN /CWORK/
      LHIT=8
      LTRREC=0
C POINTER TO FIRST HIT OF TRACK IN /CWORK/
      IHIT1=NTR*LDTR2+1
C
C
      FOR ITR=1,NTR
C NO SPACE TO STORE MORE TRACKS
         IF(IHIT1 .GT. NDIWRK-200-(70*LHIT+LTRREC)) XFOR
C CHECK IF TRACK CONSIDERED FOR REFIT
         IF KOPT.NE.4
         THEN
            LFIT=1
         ELSE
            LFIT=0
            FOR J=1,NTRREQ
               IF ITRREQ(J).EQ.ITR
               THEN
                  LFIT=1
                  XFOR
               CIF
            CFOR
         CIF
         IF LFIT.EQ.1
         THEN
            INDFET = 4
            CALL JFETCH(IPTR,IPJHTL,WRK(IHIT1),LHIT,IPRES,INDFET,XD,YD)
            NHIT=(IPRES-1)/LHIT
            IF NHIT.GT.1
            THEN
C OTHERWISE TRACK IS NOT CONSIDERED FOR REFIT
               IPRES=IHIT1+IPRES-1
C ----------------------------------------------------------------------
C S-Z FIT FOR SINGLE TRACK; MARK USED HITS
C IOPT IS PASSED ONLY TO INDICATE WHETHER SPECIAL BANK IS TO BE
C CREATED (IF IOPT>8)
               CALL ZSRFT1(IPTR,LDTR,IHIT1,IPRES,LHIT,IQUAL,IOPT)
C-----------------------------------------------------------------------
C
C SET SINGLE TRACK FIT FLAG IN   KTSTZS(.,2,.)
               IF KOPT.EQ.4
               THEN
                  IF NTSTZS(1).EQ.1
                  THEN
C  S-Z FIT OF A SINGLE TRACK WAS REQUESTED
                     KTSTZS(1,2,1)=IQUAL
                     RETURN
                  ELSE
                     FOR ISET=1,NSETZS
                        NTSETI=NTSTZS(ISET)
                        FOR JT=1,NTSETI
                     IF(KTSTZS(JT,1,ISET).EQ.ITR)KTSTZS(JT,2,ISET)=IQUAL
                        CFOR
                     CFOR
                  CIF
               ELSE
                  KTSTZS(ITR,2,1)=IQUAL
               CIF
C-----------------------------------------------------------------------
               IF IQUAL.GT.0 .AND. KOPT.NE.1
               THEN
C STORE TRACK FOR SUBSEQUENT COMMON S-Z FIT
                  NTRKS=NTRKS+1
                  IWRK(IDTR2  )=ITR
                  IWRK(IDTR2+1)=IPTR
C
                  IF(KOPT.EQ.2) IQUAL=2
C
                  IWRK(IDTR2+2)=IQUAL
                  IWRK(IDTR2+3)=IHIT1
                  IWRK(IDTR2+4)=IPRES
C
                  IHIT1=IPRES+LTRREC
                  IDTR2=IDTR2+LDTR2
               CIF
            CIF
         CIF
         IPTR=IPTR+LDTR
      CFOR
C
C NO COMMON FIT IS REQUESTED OR POSSIBLE
C
      IF(KOPT.EQ.1.OR.NTRKS.LT.2) RETURN
C
C=======================================================================
C
C     COMMON S-Z FIT TO RUN VERTEX
C
C
      IF KOPT.EQ.2
      THEN
         IRUN=HDATA( 2*IDATA(IQHEAD) + 10)
         IF  IRUN.GE.100
         THEN
            IPV    = ICALIB(10)
            XCOMM  = ACALIB(IPV+ 1)
            YCOMM  = ACALIB(IPV+ 3)
         ELSE
            XCOMM  = 0.
            YCOMM  = 0.
         CIF
         IVNEED=IZVCST(1)
C
         PERFORM COMMZS
C
      CIF
C
C=======================================================================
C
C     COMMON S-Z FIT FOR USER SPECIFIED TRACK SETS
C
C
      IF KOPT.EQ.4
      THEN
         FOR ISET=1,NSETZS
            NTSETI=NTSTZS(ISET)
            NTRFIT=0
            IDTR2=1
            FOR JTR=1,NTRKS
               ITR=IWRK(IDTR2)
               IWRK(IDTR2+2)=1
               FOR JT=1,NTSETI
                  IF KTSTZS(JT,1,ISET).EQ.ITR
                  THEN
                     NTRFIT=NTRFIT+1
                     IWRK(IDTR2+2)=2
                     XFOR
                  CIF
               CFOR
               IDTR2=IDTR2+LDTR2
            CFOR
            IF NTRFIT.GE.2
            THEN
               XCOMM=XYSTZS(1,ISET)
               YCOMM=XYSTZS(2,ISET)
               IVNEED=IZVCST(ISET)
               PERFORM COMMZS
            CIF
         CFOR
      CIF
C
      RETURN
C
C
C=======================================================================
C
C
C  CODE FOR THE COMMON S-Z FIT
C  HITS ARE USED   IFF   MARKED AS USED IN ZSRFT1
C
      PROC COMMZS
C
C STARTING VALUE FOR COMMON Z AND
C CHECK IF TRACK CONSISTENT WITH THE COMMON POINT IN R-PHI
C AND IF Z AT COMMON POINT IS WITHIN LIMITS
C
         ZCUT=ZCUTV
         IF(KOPT.EQ.4) ZCUT=ZCUTVV
         IDTR2=1
         NTRFIT=0
         ZCOMM=0.
         FOR JTR=1,NTRKS
            IF IWRK(IDTR2+2).EQ.2
            THEN
C TRACK WAS REQUESTED
               IPTR=IWRK(IDTR2+1)
C CALCULATE DISTANCE OF COMMON POINT TO CIRCLE IN R-PHI
               CURVXY=ADATA(IPTR+25)
               IF(ABS(CURVXY).LT.1.E-9) CURVXY = SIGN(1.E-9,CURVXY)
               DDR0=DISTXY(ADATA(IPTR+5),ADATA(IPTR+6),ADATA(IPTR+8),
     +         ADATA(IPTR+9),1./CURVXY,XCOMM,YCOMM,XP,YP,FI)
               IF ABS(DDR0).LT.DIMPCT
               THEN
C CIRCLE CLOSE ENOUGH TO COMMON POINT
C TRACK DIRECTION AT COMMON POINT
                  WRK(IDTR2+5)=COS(FI)
                  WRK(IDTR2+6)=SIN(FI)
C CALCULATE Z OF TRACK AT THE COMMON POINT
                  DDR0=DISTXY(ADATA(IPTR+5),ADATA(IPTR+6),ADATA(IPTR+8),
     +            ADATA(IPTR+9),1./CURVXY,0.,0.,XP,YP,FI)
                  UU=SQRT((XCOMM-XP)**2+(YCOMM-YP)**2)
                  IF(ABS(CURVXY*UU).GT.1.E-5)
     +            UU=2.*SIN(.5*CURVXY*UU)/CURVXY
                  ZCOMM1=ADATA(IPTR+31)+ADATA(IPTR+30)*UU
                  IF ABS(ZCOMM1) .LT. ZCUT
                  THEN
                     WRK(IDTR2+7)=ZCOMM1
                     WRK(IDTR2+10)=CURVXY
                     ZCOMM=ZCOMM+ZCOMM1
                     NTRFIT=NTRFIT+1
                  ELSE
                     IWRK(IDTR2+2)=1
                  CIF
               ELSE
                  IWRK(IDTR2+2)=1
               CIF
            CIF
            IDTR2=IDTR2+LDTR2
         CFOR
C
C STARTING VALUE OF COMMON Z; NO CHECK IF Z OF TRACK CONSISTENT WITH IT
C COLLECT SUMS FOR COMMON Z FIT
         IF NTRFIT.GE.2
         THEN
            ZCOMM=ZCOMM/NTRFIT
COMIT       DZLIM=400.
COMIT       NTRFIT=0
            NHTOT=0
            IF IVNEED.EQ.1
            THEN
C VERTEX CONSTRAINT OF 10 MM ON THE COMMON Z
               S0=(20./10.)**2
               IF( LDATYP(DUMMY).EQ.2 ) S0=S0*4.
               S3=-ZCOMM*S0
               S7=ZCOMM**2*S0
            ELSE
               S0=0.
               S3=0.
               S7=0.
            CIF
            S5=0.
            S6=0.
            IDTR2=1
            FOR JTR=1,NTRKS
               IF IWRK(IDTR2+2).EQ.2
               THEN
C TRACK REQUESTED AND SURVIVED THE R-PHI DISTANCE AND Z CUTS
COMIT             IF ABS(WRK(IDTR2+7)-ZCOMM).LT.DZLIM
COMIT             THEN
C TRACK CLOSE ENOUGH IN Z
COMIT                NTRFIT=NTRFIT+1
                     IPTR=IWRK(IDTR2+1)
                     CURVXY=WRK(IDTR2+10)
                     CTGTH=ADATA(IPTR+30)
                     S1=0.
                     S2=0.
                     S4=0.
C LOOP OVER HITS
                     IPCO =IWRK(IDTR2+3)
                     IPCO9=IWRK(IDTR2+4)-LHIT
                     FOR IP=IPCO,IPCO9,LHIT
                        IF HWRK(2*IP+3).EQ.1
                        THEN
C HIT WAS USED IN THE SINGLE TRACK FIT
C CALCULATE TRACK LENGTH IN R-PHI COUNTED FROM COMMON POINT
                           UX=WRK(IP+3)-XCOMM
                           UY=WRK(IP+4)-YCOMM
                           UU=SQRT(UX**2+UY**2)
                           IF(ABS(CURVXY*UU).GT.1.E-5)
     +                     UU=2.*SIN(.5*CURVXY*UU)/CURVXY
                           IF(UX*WRK(IDTR2+5)+UY*WRK(IDTR2+6).LT.0.)
     +                     UU=-UU
C RESIDUAL TO LINE WITH START PARAMETERS
                           DZ=WRK(IP+5)-ZCOMM-CTGTH*UU
                           W=WRK(IP+7)
                           NHTOT=NHTOT+1
                           S0=S0+W
                           S3=S3+DZ*W
                           S1=S1+UU*W
                           S2=S2+UU**2*W
                           S4=S4+DZ*UU*W
                           S7=S7+DZ**2*W
                        CIF
                     CFOR
                     WRK(IDTR2+7)=S4
                     WRK(IDTR2+8)=S1
                     WRK(IDTR2+9)=S2
                     S5=S5+S1*S4/S2
                     S6=S6+S1*S1/S2
COMIT             ELSE
COMIT                IWRK(IDTR2+2)=1
COMIT             CIF
               CIF
               IDTR2=IDTR2+LDTR2
            CFOR
C
C RESULTS OF COMMON FIT; FILL 'PATR' BANK
COMIT       IF NTRFIT.GE.2
COMIT       THEN
               DZCOMM=(S3-S5)/(S0-S6)
               ZCOMMR=ZCOMM+DZCOMM
C
               ISETOP=1
               IF(KOPT.EQ.4) ISETOP=ISET
               SQCHZS(2,ISETOP)=NHTOT
               SQCHZS(3,ISETOP)=ZCOMMR
               SQCHZS(1,ISETOP)=S7+DZCOMM*(DZCOMM*S0-2.*S3)
C
               IDTR2=1
               FOR JTR=1,NTRKS
                  IF IWRK(IDTR2+2).EQ.2
                  THEN
                     IPTR=IWRK(IDTR2+1)
                     PERFORM FITBNK
C
C SET SINGLE TRACK FIT FLAG IN   KTSTZS(.,2,.)
                     ITR=IWRK(IDTR2)
                     IF KOPT.EQ.4
                     THEN
                        FOR JT=1,NTSETI
                         IF(KTSTZS(JT,1,ISET).EQ.ITR)KTSTZS(JT,2,ISET)=201556500
                        CFOR
                     ELSE
                        KTSTZS(ITR,2,1)=2
                     CIF
C
                  CIF
                  IDTR2=IDTR2+LDTR2
               CFOR
C
               IF(SQCHZS(1,ISETOP).LT.1.E-5) SQCHZS(1,ISETOP)=1.E-5
               SQCHZS(1,ISETOP)=SQRT(SQCHZS(1,ISETOP)/(NHTOT-NTRFIT-1))
C
COMIT       CIF
         CIF
      CPROC
C=======================================================================
C
C
N     *************************
N     *      F I T B N K      *
N     *************************
C
C
N     SET UP FIT-BANK
      PROC FITBNK
C
      DCTGTH=(WRK(IDTR2+7)-DZCOMM*WRK(IDTR2+8))/WRK(IDTR2+9)
C
      SQCHZS(1,ISETOP)=SQCHZS(1,ISETOP)+DCTGTH*(DCTGTH*WRK(IDTR2+9)+
     +2.*DZCOMM*WRK(IDTR2+8)-2.*WRK(IDTR2+7))
C
      CTGTH=ADATA(IPTR+30)+DCTGTH
      CSTH = 1./SQRT(CTGTH**2 + 1.)
      SNTH  = CSTH * CTGTH
      CURVXY=WRK(IDTR2+10)
C
C
C
N     COPY TRACK BANK
         IFREE=NDIWRK-100
         CALL MVCL(IWRK(IFREE),0,IDATA(IPTR+1),0,4*LDTR)
C
N     FILL FIT-BANK
         IP    = IFREE - 1
         IWRK(IP+ 2) = LOR(IWRK(IP+2),8192)
C FIRST POINT ON TRACK
         UX=WRK(IP+5)-XCOMM
         UY=WRK(IP+6)-YCOMM
         UU=SQRT(UX**2+UY**2)
         IF(ABS(CURVXY*UU).GT.1.E-5) UU=2.*SIN(.5*CURVXY*UU)/CURVXY
         IF(UX*WRK(IDTR2+5)+UY*WRK(IDTR2+6).LT.0.) UU=-UU
         WRK (IP+ 7) = ZCOMMR+CTGTH*UU
         A=SQRT(WRK(IP+8)**2+WRK(IP+9)**2)
         WRK (IP+ 8) = WRK (IP+ 8)/A*CSTH
         WRK (IP+ 9) = WRK (IP+ 9)/A*CSTH
         WRK (IP+10) = SNTH
C LAST POINT ON TRACK
         UX=WRK(IP+12)-XCOMM
         UY=WRK(IP+13)-YCOMM
         UU=SQRT(UX**2+UY**2)
         IF(ABS(CURVXY*UU).GT.1.E-5) UU=2.*SIN(.5*CURVXY*UU)/CURVXY
         IF(UX*WRK(IDTR2+5)+UY*WRK(IDTR2+6).LT.0.) UU=-UU
         WRK (IP+14) = ZCOMMR+CTGTH*UU
         A=SQRT(WRK(IP+15)**2+WRK(IP+16)**2)
         WRK (IP+15) = WRK (IP+15)/A*CSTH
         WRK (IP+16) = WRK (IP+16)/A*CSTH
         WRK (IP+17) = SNTH
C STORE COMMON FIT PARAMETERS
         IWRK(IP+29) = 2
         WRK (IP+30) = CTGTH
C GET CLOSEST POINT (XP,YP) TO ORIGIN
         DDR0=DISTXY(ADATA(IPTR+5),ADATA(IPTR+6),ADATA(IPTR+8),
     +   ADATA(IPTR+9),1./CURVXY,0.,0.,XP,YP,FI)
C CALCULATE TRACK LENGTH ALONG CIRCLE FROM FIRST POINT TO (XP,YP)
         UX=XP-XCOMM
         UY=YP-YCOMM
         UU=SQRT(UX**2+UY**2)
         IF(ABS(CURVXY*UU).GT.1.E-5) UU=2.*SIN(.5*CURVXY*UU)/CURVXY
         IF(UX*WRK(IDTR2+5)+UY*WRK(IDTR2+6).LT.0.) UU=-UU
         WRK (IP+31) = ZCOMMR+CTGTH*UU
N     PUT RESULT INTO PATR-BANK
         CALL MVCL(IDATA(IPTR+1),0,IWRK(IFREE),0,4*LDTR)
      CPROC
      END
C   09/06/83 802221541  MEMBER NAME  ZSRFTV1  (JADEGS)      SHELTRAN
      SUBROUTINE ZSRFTV(MODE,IOPT)
C-----------------------------------------------------------------------
C                                   J. SPITZER 22/4/87
C                    UPDATED TO GIVE COMMON Z    1/6/87  J.S.
C    22.2.88   MVC CHANGED TO MVCL (256 BYTES NOT ENOUGH!)  J.H./J.O.
C
C       A GENERAL S-Z FIT ROUTINE
C       S = TRACK LENGTH ALONG THE CIRCLE COUNTED FROM THE
C           FIRST POINT IN THE DIRECTION OF THE LAST ONE
C
C      MODE   = 0 : OVERWRITE OLD PATR-BANK WITH NEW RESULTS
C      MODE   = 1 : CREATE NEW PATR-BANK WITH NEW RESULTS
C
C      IOPT =   1 : S-Z FIT SEPARATELY FOR ALL TRACKS
C      IOPT =   2 : S-Z FIT SEPARATELY FOR ALL TRACKS AND SUBSEQUENTLY
C                   A COMMON S-Z FIT FOR THOSE ONES WHICH
C                   EXTRAPOLATE WITHIN 15 MM TO THE RUN VERTEX IN R-PHI
C                   AND HAVE  | Z(R=0) | < 800 MM
C      *****************************************************************
C      *  THE FOLLOWIG OPTION NEEDS FILLING OF A COMMON IN ADDITION !!!*
C      *****************************************************************
C      IOPT =   4 : COMMON S-Z FITS FOR USER SPECIFIED (UPTO 5) SETS
C                   OF TRACKS WITH USER SPECIFIED COMMON (X,Y) POINTS
C                   IN R-PHI (OR SINGLE TRACK S-Z FIT, SEE LATER).
C                   THE SINGLE TRACK S-Z FITS ARE ONLY DONE FOR THOSE
C                   TRACKS WHICH APPEAR IN THE SETS.
C                   IF A TRACK IS NOT WITHIN 15 MM TO THE SPECIFIED
C                   (X,Y) POINT OR | Z(X,Y) | > 1600 MM, IT WILL BE
C                   DISCARDED.
C
C    TRACK SELECTION PARAMETERS FOR COMMON Z FIT ARE IN /CCMZCT/
C
C    ( IOPT <= IOPT+8 CREATES A SPECIAL BANK IN ADDITION )
C
C-----------------------------------------------------------------------
C
      IMPLICIT INTEGER*2 (H)
C
C-----------------------------------------------------------------------
C *****************************************************************
C *  THE COMMON TO BE FILLED BY THE USER IF IOPT=4 IS REQUESTED   *
C *****************************************************************
C
      COMMON/CZSSTE/ NSETZS,NTSTZS(5),KTSTZS(100,2,5),XYSTZS(2,5),
     +SQCHZS(3,5)
C
C NSETZS : NUMBER OF TRACK SETS FOR WHICH COMMON FIT IS TO BE DONE
C NTSTZS(1..NSETZS) : NUMBER OF TRACKS IN EACH SET
C KTSTZS(1..,1,I) TRACK SEQUENCE NUMBERS FOR SET "I"
C XYSTZS(1,I), XYSTZS(2,I) : COMMON X,Y FOR SET "I"
C
C FOR SINGLE TRACK FIT PLEASE SET     NSETZS=1, NTSTZS(1)=1,
C                                     KTSTZS(1,1,1)=ITRK (TR. SEQ. NR.)
C                              NOTHING ELSE NEEDS TO BE SET.
C
C  ***********
C  * ON EXIT *
C  ***********
C
C  KTSTZS(.,2,.)  IS A FLAG THAT TELLS YOU:
C       2 : TRACK WAS USED IN A COMMON Z-FIT
C       1 : SINGLE TRACK FIT WAS SUCCESSFUL BUT
C           THE TRACK WAS NOT USED IN A COMMON FIT
C       0 : TRACK WAS NOT CONSIDERED FOR REFIT
C      <0 : SINGLE TRACK FIT FAILED (TOO FEW USABLE HITS IN GENERAL)
C
C  THIS INFORMATION IS PROVIDED FOR IOPT=1 AND 2 AS WELL BUT
C  WITHOUT FILLING  KTSTZS(J,1,.)=J
C
C  SQCHZS(1,.) : SQUARE ROOT OF {SUM OF WEIGHTED RESIDUAL SQUARES
C                DIVIDED BY THE NUMBER OF DEGREES OF FREEDOM (LATTER=
C                NHITS-NTRACKS-1)} FOR COMMON FIT (.)
C                OR 0.0 IF NO COMMON FIT WAS PERFORMED.
C                THE (DIFFERENT FOR DL8 AND FADC) NORMALIZATION
C                TO GET THE CHI**2/DOF. IS LEFT FOR THE USER
C  SQCHZS(2,.) : NUMBER OF HITS USED IN THE COMMON FIT OR 0.0; REAL !!!
C  SQCHZS(3,.) : COMMON Z AT THE COMMON (X,Y)    (OR 0.0)
C
      COMMON/CCMZCT/ DIMPCT, ZCUTV, ZCUTVV, IZVCST(5)
C
C THIS COMMON IS BLOCK DATA SET IN   JADEBD
C
C     BLOCK DATA
C     COMMON/CCMZCT/ DIMPCT, ZCUTV, ZCUTVV, IZVCST(5)
C     DATA  DIMPCT /15./, ZCUTV /800./, ZCUTVV /1600./, IZVCST/5*0/
C     END
C=======================================================================
C
#include "cdata.for"
#include "calibr.for"
C
      COMMON/CWORK/NDIWRK,WRK(20000)
      DIMENSION IWRK(20000),HWRK(40000)
      EQUIVALENCE (WRK(1),IWRK(1),HWRK(1))
C
      INTEGER ITRREQ(100)
      DATA LBINIT /0/, IQPATR/0/, IQJHTL/0/, IQHEAD/0/, IQJETC/0/
      DATA NPRLIM/50/,KPRLIM/0/
C=======================================================================
C
      KOPT=IOPT
      IF(KOPT.GT.8) KOPT=KOPT-8
      IF KOPT.NE.1 .AND. KOPT.NE.2 .AND. KOPT.NE.4
      THEN
         IF KPRLIM.LT.NPRLIM
         THEN
            KPRLIM=KPRLIM+1
            WRITE(6,100) IOPT
100         FORMAT(' **** ZSRFTV CALLED WITH INVALID OPTION :',I8)
         CIF
         RETURN
      CIF
C
C
      IF KOPT.EQ.4
      THEN
C PRODUCE ARRAY OF ALL TRACKS REQUESTED; CHECK CONSISTENCY OF REQUEST
         IF NTSTZS(1).EQ.1
         THEN
            KTC=KTSTZS(1,1,1)
            IF KTC.LT.1 .OR. KTC.GT.100
            THEN
               LINCON=1
            ELSE
               LINCON=0
               NTRREQ=1
               ITRREQ(1)=KTC
               KTSTZS(1,2,1)=0
            CIF
         ELSE
            IF NSETZS.GE.1.AND.NSETZS.LE.5
            THEN
               NTRREQ=0
               LINCON=0
               FOR ISET=1,NSETZS
                  SQCHZS(1,ISET)=0.
                  SQCHZS(2,ISET)=0.
                  SQCHZS(3,ISET)=0.
                  NTSETI=NTSTZS(ISET)
                  IF NTSETI.LT.2 .OR. NTSETI.GT.100
                  THEN
                     LINCON=1
                     XFOR
                  CIF
                  FOR JT=1,NTSETI
                     KTC=KTSTZS(JT,1,ISET)
                     IF KTC.LT.1 .OR. KTC.GT.100
                     THEN
                        LINCON=1
                        XFOR
                     CIF
                     KTSTZS(JT,2,ISET)=0
                     LPRES=0
                     IF NTRREQ.GT.0
                     THEN
                        FOR J=1,NTRREQ
                           IF ITRREQ(J).EQ.KTC
                           THEN
                              LPRES=1
                              XFOR
                           CIF
                        CFOR
                     CIF
                     IF LPRES.EQ.0
                     THEN
                        IF NTRREQ.EQ.100
                        THEN
                           LINCON=1
                           XFOR
                        CIF
                        NTRREQ=NTRREQ+1
                        ITRREQ(NTRREQ)=KTC
                     CIF
                  CFOR
                  IF(LINCON.NE.0) XFOR
               CFOR
            ELSE
               LINCON=1
            CIF
         CIF
         IF LINCON.NE.0
         THEN
            IF KPRLIM.LT.NPRLIM
            THEN
               KPRLIM=KPRLIM+1
               WRITE(6,200)
200            FORMAT(' **** ZSRFTV: INVALID REQUEST IN /CZSSTE/')
            CIF
            RETURN
         CIF
      ELSE
         SQCHZS(1,1)=0.
         SQCHZS(2,1)=0.
         SQCHZS(3,1)=0.
         FOR J=1,100
            KTSTZS(J,2,1)=0
         CFOR
      CIF
C-----------------------------------------------------------------------
N     INITIALIZATION
      IF LBINIT .LE.0
      THEN
         LBINIT = 1
         IQPATR = IBLN('PATR')
         IQJHTL = IBLN('JHTL')
         IQHEAD = IBLN('HEAD')
         IQJETC = IBLN('JETC')
      CIF
C
C-----------------------------------------------------------------------
C
N     CHECK IF PATR-BANK
      IF(IDATA(IQPATR).LE.0) RETURN
C-----------------------------------------------------------------------
C
C     CREATE NEW PATR BANK IF REQUESTED
      IF MODE.EQ.1
      THEN
         IPPAT0 = IDATA(IQPATR)
         NBNK1  = IDATA(IPPAT0-2) - 1
         NWRD   = IDATA(IPPAT0)
         NBYTE  = NWRD*4
         CALL CCRE(IPPATR,'PATR',NBNK1,NWRD,IERR)
         IF IERR.NE.0
         THEN
            PRINT 2900, IERR
 2900       FORMAT(' CREATION OF NEW PATR-BANK RESULTED IN ERROR',I3)
            RETURN
         CIF
N        COPY CONTENTS OF 'PATR'-BANK
         CALL MVCL(IDATA(IPPATR+1),0,IDATA(IPPAT0+1),0,NBYTE)
      CIF
C-----------------------------------------------------------------------
C
      IPPATR = IDATA(IQPATR)
      IPTR   = IDATA(IPPATR+1) + IPPATR
      LDTR   = IDATA(IPPATR+3)
      NTR    = IDATA(IPPATR+2)
C
N     CHECK IF 1 TRACK
      IF(NTR.LT.1) RETURN
C
      IF NTR.GT.100
      THEN
         IF KPRLIM.LT.NPRLIM
         THEN
            KPRLIM=KPRLIM+1
            WRITE(6,300) NTR
300         FORMAT(' **** ZSRFTV : NUMBER OF TRACKS IN PATR BANK :',
     +      I4,'. FIRST 100 WILL BE CONSIDERED.')
         CIF
         NTR=100
      CIF
C
C-----------------------------------------------------------------------
C
C GET LATEST AMPLITUDE CALIBRATION
      CALL JRECAL(IERR)
      IF IERR.NE.0
      THEN
         PRINT 6784,IERR
 6784    FORMAT(' *** EROR IN JRECAL',I3)
         RETURN
      CIF
C-----------------------------------------------------------------------
C
N     RECALIBRATE Z-COORDINATES
      IPJETC = IDATA(IQJETC)
      IPJHTL = IDATA(IQJHTL)
C
C MODEZ=1 MEANS    CALIBRATION ONLY
C
      MODEZ  = 1
      CALL ZSFIT(IPJETC,IDATA(IPJETC-1),IPJHTL,IPPATR,MODEZ)
C
C
C=======================================================================
C
C     COLLECTION OF HIT DATA IN /CWORK/ AND SINGLE TRACK FITS
C
C DIMENSION OF WRK(.)
      NDIWRK=20000
C NUMBER OF TRACKS STORED IN /CWORK/
      NTRKS=0
C POINTER TO TRACK DATA THAT STORES THE STRUCTURE OF WRK(.) ETC.
      IDTR2=1
C LENGTH OF ABOVE DATA PRO TRACK
      LDTR2=11
C LENGTH OF HIT AND SUBSEQUENT TRACK DATA IN /CWORK/
      LHIT=8
      LTRREC=0
C POINTER TO FIRST HIT OF TRACK IN /CWORK/
      IHIT1=NTR*LDTR2+1
C
C
      FOR ITR=1,NTR
C NO SPACE TO STORE MORE TRACKS
         IF(IHIT1 .GT. NDIWRK-200-(70*LHIT+LTRREC)) XFOR
C CHECK IF TRACK CONSIDERED FOR REFIT
         IF KOPT.NE.4
         THEN
            LFIT=1
         ELSE
            LFIT=0
            FOR J=1,NTRREQ
               IF ITRREQ(J).EQ.ITR
               THEN
                  LFIT=1
                  XFOR
               CIF
            CFOR
         CIF
         IF LFIT.EQ.1
         THEN
            INDFET = 4
            CALL JFETCH(IPTR,IPJHTL,WRK(IHIT1),LHIT,IPRES,INDFET,XD,YD)
            NHIT=(IPRES-1)/LHIT
            IF NHIT.GT.1
            THEN
C OTHERWISE TRACK IS NOT CONSIDERED FOR REFIT
               IPRES=IHIT1+IPRES-1
C ----------------------------------------------------------------------
C S-Z FIT FOR SINGLE TRACK; MARK USED HITS
C IOPT IS PASSED ONLY TO INDICATE WHETHER SPECIAL BANK IS TO BE
C CREATED (IF IOPT>8)
               CALL ZSRFT1(IPTR,LDTR,IHIT1,IPRES,LHIT,IQUAL,IOPT)
C-----------------------------------------------------------------------
C
C SET SINGLE TRACK FIT FLAG IN   KTSTZS(.,2,.)
               IF KOPT.EQ.4
               THEN
                  IF NTSTZS(1).EQ.1
                  THEN
C  S-Z FIT OF A SINGLE TRACK WAS REQUESTED
                     KTSTZS(1,2,1)=IQUAL
                     RETURN
                  ELSE
                     FOR ISET=1,NSETZS
                        NTSETI=NTSTZS(ISET)
                        FOR JT=1,NTSETI
                     IF(KTSTZS(JT,1,ISET).EQ.ITR)KTSTZS(JT,2,ISET)=IQUAL
                        CFOR
                     CFOR
                  CIF
               ELSE
                  KTSTZS(ITR,2,1)=IQUAL
               CIF
C-----------------------------------------------------------------------
               IF IQUAL.GT.0 .AND. KOPT.NE.1
               THEN
C STORE TRACK FOR SUBSEQUENT COMMON S-Z FIT
                  NTRKS=NTRKS+1
                  IWRK(IDTR2  )=ITR
                  IWRK(IDTR2+1)=IPTR
C
                  IF(KOPT.EQ.2) IQUAL=2
C
                  IWRK(IDTR2+2)=IQUAL
                  IWRK(IDTR2+3)=IHIT1
                  IWRK(IDTR2+4)=IPRES
C
                  IHIT1=IPRES+LTRREC
                  IDTR2=IDTR2+LDTR2
               CIF
            CIF
         CIF
         IPTR=IPTR+LDTR
      CFOR
C
C NO COMMON FIT IS REQUESTED OR POSSIBLE
C
      IF(KOPT.EQ.1.OR.NTRKS.LT.2) RETURN
C
C=======================================================================
C
C     COMMON S-Z FIT TO RUN VERTEX
C
C
      IF KOPT.EQ.2
      THEN
         IPV    = ICALIB(10)
         XCOMM  = ACALIB(IPV+ 1)
         YCOMM  = ACALIB(IPV+ 3)
         IVNEED=IZVCST(1)
C
         PERFORM COMMZS
C
      CIF
C
C=======================================================================
C
C     COMMON S-Z FIT FOR USER SPECIFIED TRACK SETS
C
C
      IF KOPT.EQ.4
      THEN
         FOR ISET=1,NSETZS
            NTSETI=NTSTZS(ISET)
            NTRFIT=0
            IDTR2=1
            FOR JTR=1,NTRKS
               ITR=IWRK(IDTR2)
               IWRK(IDTR2+2)=1
               FOR JT=1,NTSETI
                  IF KTSTZS(JT,1,ISET).EQ.ITR
                  THEN
                     NTRFIT=NTRFIT+1
                     IWRK(IDTR2+2)=2
                     XFOR
                  CIF
               CFOR
               IDTR2=IDTR2+LDTR2
            CFOR
            IF NTRFIT.GE.2
            THEN
               XCOMM=XYSTZS(1,ISET)
               YCOMM=XYSTZS(2,ISET)
               IVNEED=IZVCST(ISET)
               PERFORM COMMZS
            CIF
         CFOR
      CIF
C
      RETURN
C
C
C=======================================================================
C
C
C  CODE FOR THE COMMON S-Z FIT
C  HITS ARE USED   IFF   MARKED AS USED IN ZSRFT1
C
      PROC COMMZS
C
C STARTING VALUE FOR COMMON Z AND
C CHECK IF TRACK CONSISTENT WITH THE COMMON POINT IN R-PHI
C AND IF Z AT COMMON POINT IS WITHIN LIMITS
C
         ZCUT=ZCUTV
         IF(KOPT.EQ.4) ZCUT=ZCUTVV
         IDTR2=1
         NTRFIT=0
         ZCOMM=0.
         FOR JTR=1,NTRKS
            IF IWRK(IDTR2+2).EQ.2
            THEN
C TRACK WAS REQUESTED
               IPTR=IWRK(IDTR2+1)
C CALCULATE DISTANCE OF COMMON POINT TO CIRCLE IN R-PHI
               CURVXY=ADATA(IPTR+25)
               IF(ABS(CURVXY).LT.1.E-9) CURVXY = SIGN(1.E-9,CURVXY)
               DDR0=DISTXY(ADATA(IPTR+5),ADATA(IPTR+6),ADATA(IPTR+8),
     +         ADATA(IPTR+9),1./CURVXY,XCOMM,YCOMM,XP,YP,FI)
               IF ABS(DDR0).LT.DIMPCT
               THEN
C CIRCLE CLOSE ENOUGH TO COMMON POINT
C TRACK DIRECTION AT COMMON POINT
                  WRK(IDTR2+5)=COS(FI)
                  WRK(IDTR2+6)=SIN(FI)
C CALCULATE Z OF TRACK AT THE COMMON POINT
                  DDR0=DISTXY(ADATA(IPTR+5),ADATA(IPTR+6),ADATA(IPTR+8),
     +            ADATA(IPTR+9),1./CURVXY,0.,0.,XP,YP,FI)
                  UU=SQRT((XCOMM-XP)**2+(YCOMM-YP)**2)
                  IF(ABS(CURVXY*UU).GT.1.E-5)
     +            UU=2.*SIN(.5*CURVXY*UU)/CURVXY
                  ZCOMM1=ADATA(IPTR+31)+ADATA(IPTR+30)*UU
                  IF ABS(ZCOMM1) .LT. ZCUT
                  THEN
                     WRK(IDTR2+7)=ZCOMM1
                     WRK(IDTR2+10)=CURVXY
                     ZCOMM=ZCOMM+ZCOMM1
                     NTRFIT=NTRFIT+1
                  ELSE
                     IWRK(IDTR2+2)=1
                  CIF
               ELSE
                  IWRK(IDTR2+2)=1
               CIF
            CIF
            IDTR2=IDTR2+LDTR2
         CFOR
C
C STARTING VALUE OF COMMON Z; NO CHECK IF Z OF TRACK CONSISTENT WITH IT
C COLLECT SUMS FOR COMMON Z FIT
         IF NTRFIT.GE.2
         THEN
            ZCOMM=ZCOMM/NTRFIT
COMIT       DZLIM=400.
COMIT       NTRFIT=0
            NHTOT=0
            IF IVNEED.EQ.1
            THEN
C VERTEX CONSTRAINT OF 10 MM ON THE COMMON Z
               IRUN=HDATA( 2*IDATA(IQHEAD) + 10)
               S0=(20./10.)**2
               IF(IRUN.GE.24200) S0=S0*4.
               S3=-ZCOMM*S0
               S7=ZCOMM**2*S0
            ELSE
               S0=0.
               S3=0.
               S7=0.
            CIF
            S5=0.
            S6=0.
            IDTR2=1
            FOR JTR=1,NTRKS
               IF IWRK(IDTR2+2).EQ.2
               THEN
C TRACK REQUESTED AND SURVIVED THE R-PHI DISTANCE AND Z CUTS
COMIT             IF ABS(WRK(IDTR2+7)-ZCOMM).LT.DZLIM
COMIT             THEN
C TRACK CLOSE ENOUGH IN Z
COMIT                NTRFIT=NTRFIT+1
                     IPTR=IWRK(IDTR2+1)
                     CURVXY=WRK(IDTR2+10)
                     CTGTH=ADATA(IPTR+30)
                     S1=0.
                     S2=0.
                     S4=0.
C LOOP OVER HITS
                     IPCO =IWRK(IDTR2+3)
                     IPCO9=IWRK(IDTR2+4)-LHIT
                     FOR IP=IPCO,IPCO9,LHIT
                        IF HWRK(2*IP+3).EQ.1
                        THEN
C HIT WAS USED IN THE SINGLE TRACK FIT
C CALCULATE TRACK LENGTH IN R-PHI COUNTED FROM COMMON POINT
                           UX=WRK(IP+3)-XCOMM
                           UY=WRK(IP+4)-YCOMM
                           UU=SQRT(UX**2+UY**2)
                           IF(ABS(CURVXY*UU).GT.1.E-5)
     +                     UU=2.*SIN(.5*CURVXY*UU)/CURVXY
                           IF(UX*WRK(IDTR2+5)+UY*WRK(IDTR2+6).LT.0.)
     +                     UU=-UU
C RESIDUAL TO LINE WITH START PARAMETERS
                           DZ=WRK(IP+5)-ZCOMM-CTGTH*UU
                           W=WRK(IP+7)
                           NHTOT=NHTOT+1
                           S0=S0+W
                           S3=S3+DZ*W
                           S1=S1+UU*W
                           S2=S2+UU**2*W
                           S4=S4+DZ*UU*W
                           S7=S7+DZ**2*W
                        CIF
                     CFOR
                     WRK(IDTR2+7)=S4
                     WRK(IDTR2+8)=S1
                     WRK(IDTR2+9)=S2
                     S5=S5+S1*S4/S2
                     S6=S6+S1*S1/S2
COMIT             ELSE
COMIT                IWRK(IDTR2+2)=1
COMIT             CIF
               CIF
               IDTR2=IDTR2+LDTR2
            CFOR
C
C RESULTS OF COMMON FIT; FILL 'PATR' BANK
COMIT       IF NTRFIT.GE.2
COMIT       THEN
               DZCOMM=(S3-S5)/(S0-S6)
               ZCOMMR=ZCOMM+DZCOMM
C
               ISETOP=1
               IF(KOPT.EQ.4) ISETOP=ISET
               SQCHZS(2,ISETOP)=NHTOT
               SQCHZS(3,ISETOP)=ZCOMMR
               SQCHZS(1,ISETOP)=S7+DZCOMM*(DZCOMM*S0-2.*S3)
C
               IDTR2=1
               FOR JTR=1,NTRKS
                  IF IWRK(IDTR2+2).EQ.2
                  THEN
                     IPTR=IWRK(IDTR2+1)
                     PERFORM FITBNK
C
C SET SINGLE TRACK FIT FLAG IN   KTSTZS(.,2,.)
                     ITR=IWRK(IDTR2)
                     IF KOPT.EQ.4
                     THEN
                        FOR JT=1,NTSETI
                         IF(KTSTZS(JT,1,ISET).EQ.ITR)KTSTZS(JT,2,ISET)=201623400
                        CFOR
                     ELSE
                        KTSTZS(ITR,2,1)=2
                     CIF
C
                  CIF
                  IDTR2=IDTR2+LDTR2
               CFOR
C
               IF(SQCHZS(1,ISETOP).LT.1.E-5) SQCHZS(1,ISETOP)=1.E-5
               SQCHZS(1,ISETOP)=SQRT(SQCHZS(1,ISETOP)/(NHTOT-NTRFIT-1))
C
COMIT       CIF
         CIF
      CPROC
C=======================================================================
C
C
N     *************************
N     *      F I T B N K      *
N     *************************
C
C
N     SET UP FIT-BANK
      PROC FITBNK
C
      DCTGTH=(WRK(IDTR2+7)-DZCOMM*WRK(IDTR2+8))/WRK(IDTR2+9)
C
      SQCHZS(1,ISETOP)=SQCHZS(1,ISETOP)+DCTGTH*(DCTGTH*WRK(IDTR2+9)+
     +2.*DZCOMM*WRK(IDTR2+8)-2.*WRK(IDTR2+7))
C
      CTGTH=ADATA(IPTR+30)+DCTGTH
      CSTH = 1./SQRT(CTGTH**2 + 1.)
      SNTH  = CSTH * CTGTH
      CURVXY=WRK(IDTR2+10)
C
C
C
N     COPY TRACK BANK
         IFREE=NDIWRK-100
         CALL MVCL(IWRK(IFREE),0,IDATA(IPTR+1),0,4*LDTR)
C
N     FILL FIT-BANK
         IP    = IFREE - 1
         IWRK(IP+ 2) = LOR(IWRK(IP+2),8192)
C FIRST POINT ON TRACK
         UX=WRK(IP+5)-XCOMM
         UY=WRK(IP+6)-YCOMM
         UU=SQRT(UX**2+UY**2)
         IF(ABS(CURVXY*UU).GT.1.E-5) UU=2.*SIN(.5*CURVXY*UU)/CURVXY
         IF(UX*WRK(IDTR2+5)+UY*WRK(IDTR2+6).LT.0.) UU=-UU
         WRK (IP+ 7) = ZCOMMR+CTGTH*UU
         A=SQRT(WRK(IP+8)**2+WRK(IP+9)**2)
         WRK (IP+ 8) = WRK (IP+ 8)/A*CSTH
         WRK (IP+ 9) = WRK (IP+ 9)/A*CSTH
         WRK (IP+10) = SNTH
C LAST POINT ON TRACK
         UX=WRK(IP+12)-XCOMM
         UY=WRK(IP+13)-YCOMM
         UU=SQRT(UX**2+UY**2)
         IF(ABS(CURVXY*UU).GT.1.E-5) UU=2.*SIN(.5*CURVXY*UU)/CURVXY
         IF(UX*WRK(IDTR2+5)+UY*WRK(IDTR2+6).LT.0.) UU=-UU
         WRK (IP+14) = ZCOMMR+CTGTH*UU
         A=SQRT(WRK(IP+15)**2+WRK(IP+16)**2)
         WRK (IP+15) = WRK (IP+15)/A*CSTH
         WRK (IP+16) = WRK (IP+16)/A*CSTH
         WRK (IP+17) = SNTH
C STORE COMMON FIT PARAMETERS
         IWRK(IP+29) = 2
         WRK (IP+30) = CTGTH
C GET CLOSEST POINT (XP,YP) TO ORIGIN
         DDR0=DISTXY(ADATA(IPTR+5),ADATA(IPTR+6),ADATA(IPTR+8),
     +   ADATA(IPTR+9),1./CURVXY,0.,0.,XP,YP,FI)
C CALCULATE TRACK LENGTH ALONG CIRCLE FROM FIRST POINT TO (XP,YP)
         UX=XP-XCOMM
         UY=YP-YCOMM
         UU=SQRT(UX**2+UY**2)
         IF(ABS(CURVXY*UU).GT.1.E-5) UU=2.*SIN(.5*CURVXY*UU)/CURVXY
         IF(UX*WRK(IDTR2+5)+UY*WRK(IDTR2+6).LT.0.) UU=-UU
         WRK (IP+31) = ZCOMMR+CTGTH*UU
N     PUT RESULT INTO PATR-BANK
         CALL MVCL(IDATA(IPTR+1),0,IWRK(IFREE),0,4*LDTR)
      CPROC
      END
C   09/06/83 803251954  MEMBER NAME  ZSRFTV2  (JADEGS)      SHELTRAN
C   09/06/83 803181238  MEMBER NAME  ZSRFTV   (S)           SHELTRAN
      SUBROUTINE ZSRFTV(MODE,IOPT)
C-----------------------------------------------------------------------
C                                   J. SPITZER 22/4/87
C                    UPDATED TO GIVE COMMON Z    1/6/87  J.S.
C    18.3.88   PROPER RUN NUMBER HANDLING USING LDATYP      E E
C    22.2.88   MVC CHANGED TO MVCL (256 BYTES NOT ENOUGH!)  J.H./J.O.
C
C       A GENERAL S-Z FIT ROUTINE
C       S = TRACK LENGTH ALONG THE CIRCLE COUNTED FROM THE
C           FIRST POINT IN THE DIRECTION OF THE LAST ONE
C
C      MODE   = 0 : OVERWRITE OLD PATR-BANK WITH NEW RESULTS
C      MODE   = 1 : CREATE NEW PATR-BANK WITH NEW RESULTS
C
C      IOPT =   1 : S-Z FIT SEPARATELY FOR ALL TRACKS
C      IOPT =   2 : S-Z FIT SEPARATELY FOR ALL TRACKS AND SUBSEQUENTLY
C                   A COMMON S-Z FIT FOR THOSE ONES WHICH
C                   EXTRAPOLATE WITHIN 15 MM TO THE RUN VERTEX IN R-PHI
C                   AND HAVE  | Z(R=0) | < 800 MM
C      *****************************************************************
C      *  THE FOLLOWIG OPTION NEEDS FILLING OF A COMMON IN ADDITION !!!*
C      *****************************************************************
C      IOPT =   4 : COMMON S-Z FITS FOR USER SPECIFIED (UPTO 5) SETS
C                   OF TRACKS WITH USER SPECIFIED COMMON (X,Y) POINTS
C                   IN R-PHI (OR SINGLE TRACK S-Z FIT, SEE LATER).
C                   THE SINGLE TRACK S-Z FITS ARE ONLY DONE FOR THOSE
C                   TRACKS WHICH APPEAR IN THE SETS.
C                   IF A TRACK IS NOT WITHIN 15 MM TO THE SPECIFIED
C                   (X,Y) POINT OR | Z(X,Y) | > 1600 MM, IT WILL BE
C                   DISCARDED.
C
C    TRACK SELECTION PARAMETERS FOR COMMON Z FIT ARE IN /CCMZCT/
C
C    ( IOPT <= IOPT+8 CREATES A SPECIAL BANK IN ADDITION )
C
C-----------------------------------------------------------------------
C
      IMPLICIT INTEGER*2 (H)
C
C-----------------------------------------------------------------------
C *****************************************************************
C *  THE COMMON TO BE FILLED BY THE USER IF IOPT=4 IS REQUESTED   *
C *****************************************************************
C
      COMMON/CZSSTE/ NSETZS,NTSTZS(5),KTSTZS(100,2,5),XYSTZS(2,5),
     +SQCHZS(3,5)
C
C NSETZS : NUMBER OF TRACK SETS FOR WHICH COMMON FIT IS TO BE DONE
C NTSTZS(1..NSETZS) : NUMBER OF TRACKS IN EACH SET
C KTSTZS(1..,1,I) TRACK SEQUENCE NUMBERS FOR SET "I"
C XYSTZS(1,I), XYSTZS(2,I) : COMMON X,Y FOR SET "I"
C
C FOR SINGLE TRACK FIT PLEASE SET     NSETZS=1, NTSTZS(1)=1,
C                                     KTSTZS(1,1,1)=ITRK (TR. SEQ. NR.)
C                              NOTHING ELSE NEEDS TO BE SET.
C
C  ***********
C  * ON EXIT *
C  ***********
C
C  KTSTZS(.,2,.)  IS A FLAG THAT TELLS YOU:
C       2 : TRACK WAS USED IN A COMMON Z-FIT
C       1 : SINGLE TRACK FIT WAS SUCCESSFUL BUT
C           THE TRACK WAS NOT USED IN A COMMON FIT
C       0 : TRACK WAS NOT CONSIDERED FOR REFIT
C      <0 : SINGLE TRACK FIT FAILED (TOO FEW USABLE HITS IN GENERAL)
C
C  THIS INFORMATION IS PROVIDED FOR IOPT=1 AND 2 AS WELL BUT
C  WITHOUT FILLING  KTSTZS(J,1,.)=J
C
C  SQCHZS(1,.) : SQUARE ROOT OF {SUM OF WEIGHTED RESIDUAL SQUARES
C                DIVIDED BY THE NUMBER OF DEGREES OF FREEDOM (LATTER=
C                NHITS-NTRACKS-1)} FOR COMMON FIT (.)
C                OR 0.0 IF NO COMMON FIT WAS PERFORMED.
C                THE (DIFFERENT FOR DL8 AND FADC) NORMALIZATION
C                TO GET THE CHI**2/DOF. IS LEFT FOR THE USER
C  SQCHZS(2,.) : NUMBER OF HITS USED IN THE COMMON FIT OR 0.0; REAL !!!
C  SQCHZS(3,.) : COMMON Z AT THE COMMON (X,Y)    (OR 0.0)
C
      COMMON/CCMZCT/ DIMPCT, ZCUTV, ZCUTVV, IZVCST(5)
C
C THIS COMMON IS BLOCK DATA SET IN   JADEBD
C
C     BLOCK DATA
C     COMMON/CCMZCT/ DIMPCT, ZCUTV, ZCUTVV, IZVCST(5)
C     DATA  DIMPCT /15./, ZCUTV /800./, ZCUTVV /1600./, IZVCST/5*0/
C     END
C=======================================================================
C
#include "cdata.for"
#include "calibr.for"
C
      COMMON/CWORK/NDIWRK,WRK(20000)
      DIMENSION IWRK(20000),HWRK(40000)
      EQUIVALENCE (WRK(1),IWRK(1),HWRK(1))
C
      INTEGER ITRREQ(100)
      REAL CORTRC(2,100)
      DATA LBINIT /0/, IQPATR/0/, IQJHTL/0/, IQHEAD/0/, IQJETC/0/
      DATA NPRLIM/50/,KPRLIM/0/
C=======================================================================
C
      KOPT=IOPT
      IF(KOPT.GT.8) KOPT=KOPT-8
      IF KOPT.NE.1 .AND. KOPT.NE.2 .AND. KOPT.NE.4
      THEN
         IF KPRLIM.LT.NPRLIM
         THEN
            KPRLIM=KPRLIM+1
            WRITE(6,100) IOPT
100         FORMAT(' **** ZSRFTV CALLED WITH INVALID OPTION :',I8)
         CIF
         RETURN
      CIF
C
C
      IF KOPT.EQ.4
      THEN
C PRODUCE ARRAY OF ALL TRACKS REQUESTED; CHECK CONSISTENCY OF REQUEST
         IF NTSTZS(1).EQ.1
         THEN
            KTC=KTSTZS(1,1,1)
            IF KTC.LT.1 .OR. KTC.GT.100
            THEN
               LINCON=1
            ELSE
               LINCON=0
               NTRREQ=1
               ITRREQ(1)=KTC
               KTSTZS(1,2,1)=0
            CIF
         ELSE
            IF NSETZS.GE.1.AND.NSETZS.LE.5
            THEN
               NTRREQ=0
               LINCON=0
               FOR ISET=1,NSETZS
                  SQCHZS(1,ISET)=0.
                  SQCHZS(2,ISET)=0.
                  SQCHZS(3,ISET)=0.
                  NTSETI=NTSTZS(ISET)
                  IF NTSETI.LT.2 .OR. NTSETI.GT.100
                  THEN
                     LINCON=1
                     XFOR
                  CIF
                  FOR JT=1,NTSETI
                     KTC=KTSTZS(JT,1,ISET)
                     IF KTC.LT.1 .OR. KTC.GT.100
                     THEN
                        LINCON=1
                        XFOR
                     CIF
                     KTSTZS(JT,2,ISET)=0
                     LPRES=0
                     IF NTRREQ.GT.0
                     THEN
                        FOR J=1,NTRREQ
                           IF ITRREQ(J).EQ.KTC
                           THEN
                              LPRES=1
                              XFOR
                           CIF
                        CFOR
                     CIF
                     IF LPRES.EQ.0
                     THEN
                        IF NTRREQ.EQ.100
                        THEN
                           LINCON=1
                           XFOR
                        CIF
                        NTRREQ=NTRREQ+1
                        ITRREQ(NTRREQ)=KTC
                     CIF
                  CFOR
                  IF(LINCON.NE.0) XFOR
               CFOR
            ELSE
               LINCON=1
            CIF
         CIF
         IF LINCON.NE.0
         THEN
            IF KPRLIM.LT.NPRLIM
            THEN
               KPRLIM=KPRLIM+1
               WRITE(6,200)
200            FORMAT(' **** ZSRFTV: INVALID REQUEST IN /CZSSTE/')
            CIF
            RETURN
         CIF
      ELSE
         SQCHZS(1,1)=0.
         SQCHZS(2,1)=0.
         SQCHZS(3,1)=0.
         FOR J=1,100
            KTSTZS(J,2,1)=0
         CFOR
      CIF
C-----------------------------------------------------------------------
N     INITIALIZATION
      IF LBINIT .LE.0
      THEN
         LBINIT = 1
         IQPATR = IBLN('PATR')
         IQJHTL = IBLN('JHTL')
         IQHEAD = IBLN('HEAD')
         IQJETC = IBLN('JETC')
      CIF
C
C-----------------------------------------------------------------------
C
N     CHECK IF PATR-BANK
      IF(IDATA(IQPATR).LE.0) RETURN
C-----------------------------------------------------------------------
C
C     CREATE NEW PATR BANK IF REQUESTED
      IF MODE.EQ.1
      THEN
         IPPAT0 = IDATA(IQPATR)
         NBNK1  = IDATA(IPPAT0-2) - 1
         NWRD   = IDATA(IPPAT0)
         NBYTE  = NWRD*4
         CALL CCRE(IPPATR,'PATR',NBNK1,NWRD,IERR)
         IF IERR.NE.0
         THEN
            PRINT 2900, IERR
 2900       FORMAT(' CREATION OF NEW PATR-BANK RESULTED IN ERROR',I3)
            RETURN
         CIF
N        COPY CONTENTS OF 'PATR'-BANK
         CALL MVCL(IDATA(IPPATR+1),0,IDATA(IPPAT0+1),0,NBYTE)
      CIF
C-----------------------------------------------------------------------
C
      IPPATR = IDATA(IQPATR)
      IPTR   = IDATA(IPPATR+1) + IPPATR
      LDTR   = IDATA(IPPATR+3)
      NTR    = IDATA(IPPATR+2)
C
N     CHECK IF 1 TRACK
      IF(NTR.LT.1) RETURN
C
      IF NTR.GT.100
      THEN
         IF KPRLIM.LT.NPRLIM
         THEN
            KPRLIM=KPRLIM+1
            WRITE(6,300) NTR
300         FORMAT(' **** ZSRFTV : NUMBER OF TRACKS IN PATR BANK :',
     +      I4,'. FIRST 100 WILL BE CONSIDERED.')
         CIF
         NTR=100
      CIF
C
C-----------------------------------------------------------------------
C
C GET LATEST AMPLITUDE CALIBRATION
      CALL JRECAL(IERR)
      IF IERR.NE.0
      THEN
         PRINT 6784,IERR
 6784    FORMAT(' *** EROR IN JRECAL',I3)
         RETURN
      CIF
C-----------------------------------------------------------------------
C
N     RECALIBRATE Z-COORDINATES
      IPJETC = IDATA(IQJETC)
      IPJHTL = IDATA(IQJHTL)
C
C MODEZ=1 MEANS    CALIBRATION ONLY
C
      MODEZ  = 1
      CALL ZSFIT(IPJETC,IDATA(IPJETC-1),IPJHTL,IPPATR,MODEZ)
C
C
C=======================================================================
C
C     COLLECTION OF HIT DATA IN /CWORK/ AND SINGLE TRACK FITS
C
C DIMENSION OF WRK(.)
      NDIWRK=20000
C NUMBER OF TRACKS STORED IN /CWORK/
      NTRKS=0
C POINTER TO TRACK DATA THAT STORES THE STRUCTURE OF WRK(.) ETC.
      IDTR2=1
C LENGTH OF ABOVE DATA PRO TRACK
      LDTR2=11
C LENGTH OF HIT AND SUBSEQUENT TRACK DATA IN /CWORK/
      LHIT=8
      LTRREC=0
C POINTER TO FIRST HIT OF TRACK IN /CWORK/
      IHIT1=NTR*LDTR2+1
C
C
      FOR ITR=1,NTR
C NO SPACE TO STORE MORE TRACKS
         IF(IHIT1 .GT. NDIWRK-200-(70*LHIT+LTRREC)) XFOR
C CHECK IF TRACK CONSIDERED FOR REFIT
         IF KOPT.NE.4
         THEN
            LFIT=1
         ELSE
            LFIT=0
            FOR J=1,NTRREQ
               IF ITRREQ(J).EQ.ITR
               THEN
                  LFIT=1
                  XFOR
               CIF
            CFOR
         CIF
         IF LFIT.EQ.1
         THEN
            INDFET = 4
            CALL JFETCH(IPTR,IPJHTL,WRK(IHIT1),LHIT,IPRES,INDFET,XD,YD)
            NHIT=(IPRES-1)/LHIT
            IF NHIT.GT.1
            THEN
C OTHERWISE TRACK IS NOT CONSIDERED FOR REFIT
               IPRES=IHIT1+IPRES-1
C ----------------------------------------------------------------------
C S-Z FIT FOR SINGLE TRACK; MARK USED HITS
C IOPT IS PASSED ONLY TO INDICATE WHETHER SPECIAL BANK IS TO BE
C CREATED (IF IOPT>8)
               CALL ZSRFT1(IPTR,LDTR,IHIT1,IPRES,LHIT,IQUAL,IOPT)
C-----------------------------------------------------------------------
C
C SET SINGLE TRACK FIT FLAG IN   KTSTZS(.,2,.)
               IF KOPT.EQ.4
               THEN
                  IF NTSTZS(1).EQ.1
                  THEN
C  S-Z FIT OF A SINGLE TRACK WAS REQUESTED
                     KTSTZS(1,2,1)=IQUAL
                     RETURN
                  ELSE
                     FOR ISET=1,NSETZS
                        NTSETI=NTSTZS(ISET)
                        FOR JT=1,NTSETI
                     IF(KTSTZS(JT,1,ISET).EQ.ITR)KTSTZS(JT,2,ISET)=IQUAL
                        CFOR
                     CFOR
                  CIF
               ELSE
                  KTSTZS(ITR,2,1)=IQUAL
               CIF
C-----------------------------------------------------------------------
               IF IQUAL.GT.0 .AND. KOPT.NE.1
               THEN
C STORE TRACK FOR SUBSEQUENT COMMON S-Z FIT
                  NTRKS=NTRKS+1
                  IWRK(IDTR2  )=ITR
                  IWRK(IDTR2+1)=IPTR
C
                  IF(KOPT.EQ.2) IQUAL=2
C
                  IWRK(IDTR2+2)=IQUAL
                  IWRK(IDTR2+3)=IHIT1
                  IWRK(IDTR2+4)=IPRES
C
                  IHIT1=IPRES+LTRREC
                  IDTR2=IDTR2+LDTR2
               CIF
            CIF
         CIF
         IPTR=IPTR+LDTR
      CFOR
C
C NO COMMON FIT IS REQUESTED OR POSSIBLE
C
      IF(KOPT.EQ.1.OR.NTRKS.LT.2) RETURN
C
C=======================================================================
C
C     COMMON S-Z FIT TO RUN VERTEX
C
C
      IF KOPT.EQ.2
      THEN
         IRUN=HDATA( 2*IDATA(IQHEAD) + 10)
         IF  IRUN.GE.100
         THEN
            IPV    = ICALIB(10)
            XCOMM  = ACALIB(IPV+ 1)
            YCOMM  = ACALIB(IPV+ 3)
         ELSE
            XCOMM  = 0.
            YCOMM  = 0.
         CIF
         IVNEED=IZVCST(1)
C
         PERFORM COMMZS
C
      CIF
C
C=======================================================================
C
C     COMMON S-Z FIT FOR USER SPECIFIED TRACK SETS
C
C
      IF KOPT.EQ.4
      THEN
         FOR ISET=1,NSETZS
            NTSETI=NTSTZS(ISET)
            NTRFIT=0
            IDTR2=1
            FOR JTR=1,NTRKS
               ITR=IWRK(IDTR2)
               IWRK(IDTR2+2)=1
               FOR JT=1,NTSETI
                  IF KTSTZS(JT,1,ISET).EQ.ITR
                  THEN
                     NTRFIT=NTRFIT+1
                     IWRK(IDTR2+2)=2
                     XFOR
                  CIF
               CFOR
               IDTR2=IDTR2+LDTR2
            CFOR
            IF NTRFIT.GE.2
            THEN
               XCOMM=XYSTZS(1,ISET)
               YCOMM=XYSTZS(2,ISET)
               IVNEED=IZVCST(ISET)
               PERFORM COMMZS
            CIF
         CFOR
      CIF
C
      RETURN
C
C
C=======================================================================
C
C
C  CODE FOR THE COMMON S-Z FIT
C  HITS ARE USED   IFF   MARKED AS USED IN ZSRFT1
C
      PROC COMMZS
C
C STARTING VALUE FOR COMMON Z AND
C CHECK IF TRACK CONSISTENT WITH THE COMMON POINT IN R-PHI
C AND IF Z AT COMMON POINT IS WITHIN LIMITS
C
         ZCUT=ZCUTV
         IF(KOPT.EQ.4) ZCUT=ZCUTVV
         IDTR2=1
         NTRFIT=0
         ZCOMM=0.
         FOR JTR=1,NTRKS
            IF IWRK(IDTR2+2).EQ.2
            THEN
C TRACK WAS REQUESTED
               IPTR=IWRK(IDTR2+1)
C CALCULATE DISTANCE OF COMMON POINT TO CIRCLE IN R-PHI
               CURVXY=ADATA(IPTR+25)
               IF(ABS(CURVXY).LT.1.E-9) CURVXY = SIGN(1.E-9,CURVXY)
               DDR0=DISTXY(ADATA(IPTR+5),ADATA(IPTR+6),ADATA(IPTR+8),
     +         ADATA(IPTR+9),1./CURVXY,XCOMM,YCOMM,CORTRC(1,JTR),
     +         CORTRC(2,JTR),FI)
               IF ABS(DDR0).LT.DIMPCT
               THEN
C CIRCLE CLOSE ENOUGH TO COMMON POINT
C TRACK DIRECTION AT COMMON POINT
                  WRK(IDTR2+5)=COS(FI)
                  WRK(IDTR2+6)=SIN(FI)
C CALCULATE Z OF TRACK AT THE COMMON POINT
                  DDR0=DISTXY(ADATA(IPTR+5),ADATA(IPTR+6),ADATA(IPTR+8),
     +            ADATA(IPTR+9),1./CURVXY,0.,0.,XP,YP,FI)
                  UU=SQRT((CORTRC(1,JTR)-XP)**2+(CORTRC(2,JTR)-YP)**2)
                  IF(ABS(CURVXY*UU).GT.1.E-5)
     +            UU=2.*SIN(.5*CURVXY*UU)/CURVXY
                  ZCOMM1=ADATA(IPTR+31)+ADATA(IPTR+30)*UU
                  IF ABS(ZCOMM1) .LT. ZCUT
                  THEN
                     WRK(IDTR2+7)=ZCOMM1
                     WRK(IDTR2+10)=CURVXY
                     ZCOMM=ZCOMM+ZCOMM1
                     NTRFIT=NTRFIT+1
                  ELSE
                     IWRK(IDTR2+2)=1
                  CIF
               ELSE
                  IWRK(IDTR2+2)=1
               CIF
            CIF
            IDTR2=IDTR2+LDTR2
         CFOR
C
C STARTING VALUE OF COMMON Z; NO CHECK IF Z OF TRACK CONSISTENT WITH IT
C COLLECT SUMS FOR COMMON Z FIT
         IF NTRFIT.GE.2
         THEN
            ZCOMM=ZCOMM/NTRFIT
COMIT       DZLIM=400.
COMIT       NTRFIT=0
            NHTOT=0
            IF IVNEED.EQ.1
            THEN
C VERTEX CONSTRAINT OF 10 MM ON THE COMMON Z
               S0=(20./10.)**2
               IF( LDATYP(DUMMY).EQ.2 ) S0=S0*4.
               S3=-ZCOMM*S0
               S7=ZCOMM**2*S0
            ELSE
               S0=0.
               S3=0.
               S7=0.
            CIF
            S5=0.
            S6=0.
            IDTR2=1
            FOR JTR=1,NTRKS
               IF IWRK(IDTR2+2).EQ.2
               THEN
C TRACK REQUESTED AND SURVIVED THE R-PHI DISTANCE AND Z CUTS
COMIT             IF ABS(WRK(IDTR2+7)-ZCOMM).LT.DZLIM
COMIT             THEN
C TRACK CLOSE ENOUGH IN Z
COMIT                NTRFIT=NTRFIT+1
                     IPTR=IWRK(IDTR2+1)
                     CURVXY=WRK(IDTR2+10)
                     CTGTH=ADATA(IPTR+30)
                     S1=0.
                     S2=0.
                     S4=0.
C LOOP OVER HITS
                     IPCO =IWRK(IDTR2+3)
                     IPCO9=IWRK(IDTR2+4)-LHIT
                     FOR IP=IPCO,IPCO9,LHIT
                        IF HWRK(2*IP+3).EQ.1
                        THEN
C HIT WAS USED IN THE SINGLE TRACK FIT
C CALCULATE TRACK LENGTH IN R-PHI COUNTED FROM COMMON POINT
                           UX=WRK(IP+3)-CORTRC(1,JTR)
                           UY=WRK(IP+4)-CORTRC(2,JTR)
                           UU=SQRT(UX**2+UY**2)
                           IF(ABS(CURVXY*UU).GT.1.E-5)
     +                     UU=2.*SIN(.5*CURVXY*UU)/CURVXY
                           IF(UX*WRK(IDTR2+5)+UY*WRK(IDTR2+6).LT.0.)
     +                     UU=-UU
C RESIDUAL TO LINE WITH START PARAMETERS
                           DZ=WRK(IP+5)-ZCOMM-CTGTH*UU
                           W=WRK(IP+7)
                           NHTOT=NHTOT+1
                           S0=S0+W
                           S3=S3+DZ*W
                           S1=S1+UU*W
                           S2=S2+UU**2*W
                           S4=S4+DZ*UU*W
                           S7=S7+DZ**2*W
                        CIF
                     CFOR
                     WRK(IDTR2+7)=S4
                     WRK(IDTR2+8)=S1
                     WRK(IDTR2+9)=S2
                     S5=S5+S1*S4/S2
                     S6=S6+S1*S1/S2
COMIT             ELSE
COMIT                IWRK(IDTR2+2)=1
COMIT             CIF
               CIF
               IDTR2=IDTR2+LDTR2
            CFOR
C
C RESULTS OF COMMON FIT; FILL 'PATR' BANK
COMIT       IF NTRFIT.GE.2
COMIT       THEN
               DZCOMM=(S3-S5)/(S0-S6)
               ZCOMMR=ZCOMM+DZCOMM
C
               ISETOP=1
               IF(KOPT.EQ.4) ISETOP=ISET
               SQCHZS(2,ISETOP)=NHTOT
               SQCHZS(3,ISETOP)=ZCOMMR
               SQCHZS(1,ISETOP)=S7+DZCOMM*(DZCOMM*S0-2.*S3)
C
               IDTR2=1
               FOR JTR=1,NTRKS
                  IF IWRK(IDTR2+2).EQ.2
                  THEN
                     IPTR=IWRK(IDTR2+1)
                     PERFORM FITBNK
C
C SET SINGLE TRACK FIT FLAG IN   KTSTZS(.,2,.)
                     ITR=IWRK(IDTR2)
                     IF KOPT.EQ.4
                     THEN
                        FOR JT=1,NTSETI
                         IF(KTSTZS(JT,1,ISET).EQ.ITR)KTSTZS(JT,2,ISET)=201691300
                        CFOR
                     ELSE
                        KTSTZS(ITR,2,1)=2
                     CIF
C
                  CIF
                  IDTR2=IDTR2+LDTR2
               CFOR
C
               IF(SQCHZS(1,ISETOP).LT.1.E-5) SQCHZS(1,ISETOP)=1.E-5
               SQCHZS(1,ISETOP)=SQRT(SQCHZS(1,ISETOP)/(NHTOT-NTRFIT-1))
C
COMIT       CIF
         CIF
      CPROC
C=======================================================================
C
C
N     *************************
N     *      F I T B N K      *
N     *************************
C
C
N     SET UP FIT-BANK
      PROC FITBNK
C
      DCTGTH=(WRK(IDTR2+7)-DZCOMM*WRK(IDTR2+8))/WRK(IDTR2+9)
C
      SQCHZS(1,ISETOP)=SQCHZS(1,ISETOP)+DCTGTH*(DCTGTH*WRK(IDTR2+9)+
     +2.*DZCOMM*WRK(IDTR2+8)-2.*WRK(IDTR2+7))
C
      CTGTH=ADATA(IPTR+30)+DCTGTH
      CSTH = 1./SQRT(CTGTH**2 + 1.)
      SNTH  = CSTH * CTGTH
      CURVXY=WRK(IDTR2+10)
C
C
C
N     COPY TRACK BANK
         IFREE=NDIWRK-100
         CALL MVCL(IWRK(IFREE),0,IDATA(IPTR+1),0,4*LDTR)
C
N     FILL FIT-BANK
         IP    = IFREE - 1
         IWRK(IP+ 2) = LOR(IWRK(IP+2),8192)
C FIRST POINT ON TRACK
         UX=WRK(IP+5)-CORTRC(1,JTR)
         UY=WRK(IP+6)-CORTRC(2,JTR)
         UU=SQRT(UX**2+UY**2)
         IF(ABS(CURVXY*UU).GT.1.E-5) UU=2.*SIN(.5*CURVXY*UU)/CURVXY
         IF(UX*WRK(IDTR2+5)+UY*WRK(IDTR2+6).LT.0.) UU=-UU
         WRK (IP+ 7) = ZCOMMR+CTGTH*UU
         A=SQRT(WRK(IP+8)**2+WRK(IP+9)**2)
         WRK (IP+ 8) = WRK (IP+ 8)/A*CSTH
         WRK (IP+ 9) = WRK (IP+ 9)/A*CSTH
         WRK (IP+10) = SNTH
C LAST POINT ON TRACK
         UX=WRK(IP+12)-CORTRC(1,JTR)
         UY=WRK(IP+13)-CORTRC(2,JTR)
         UU=SQRT(UX**2+UY**2)
         IF(ABS(CURVXY*UU).GT.1.E-5) UU=2.*SIN(.5*CURVXY*UU)/CURVXY
         IF(UX*WRK(IDTR2+5)+UY*WRK(IDTR2+6).LT.0.) UU=-UU
         WRK (IP+14) = ZCOMMR+CTGTH*UU
         A=SQRT(WRK(IP+15)**2+WRK(IP+16)**2)
         WRK (IP+15) = WRK (IP+15)/A*CSTH
         WRK (IP+16) = WRK (IP+16)/A*CSTH
         WRK (IP+17) = SNTH
C STORE COMMON FIT PARAMETERS
         IWRK(IP+29) = 2
         WRK (IP+30) = CTGTH
C GET CLOSEST POINT (XP,YP) TO ORIGIN
         DDR0=DISTXY(ADATA(IPTR+5),ADATA(IPTR+6),ADATA(IPTR+8),
     +   ADATA(IPTR+9),1./CURVXY,0.,0.,XP,YP,FI)
C CALCULATE TRACK LENGTH ALONG CIRCLE FROM FIRST POINT TO (XP,YP)
         UX=XP-CORTRC(1,JTR)
         UY=YP-CORTRC(2,JTR)
         UU=SQRT(UX**2+UY**2)
         IF(ABS(CURVXY*UU).GT.1.E-5) UU=2.*SIN(.5*CURVXY*UU)/CURVXY
         IF(UX*WRK(IDTR2+5)+UY*WRK(IDTR2+6).LT.0.) UU=-UU
         WRK (IP+31) = ZCOMMR+CTGTH*UU
N     PUT RESULT INTO PATR-BANK
         CALL MVCL(IDATA(IPTR+1),0,IWRK(IFREE),0,4*LDTR)
      CPROC
      END
C   09/06/83 805131835  MEMBER NAME  ZSRFTV3  (JADEGS)      SHELTRAN
C   09/06/83 803181238  MEMBER NAME  ZSRFTV   (S)           SHELTRAN
      SUBROUTINE ZSRFTV(MODE,IOPT)
C-----------------------------------------------------------------------
C                                   J. SPITZER 22/4/87
C                    UPDATED TO GIVE COMMON Z    1/6/87  J.S.
C    18.3.88   PROPER RUN NUMBER HANDLING USING LDATYP      E E
C    22.2.88   MVC CHANGED TO MVCL (256 BYTES NOT ENOUGH!)  J.H./J.O.
C    13.5.88 (FRIDAY!) SEVERE BUG CORRECTED IN SEVERAL PLACES J.H./J.O.
C            ARSIN INSTEAD OF SIN
C
C       A GENERAL S-Z FIT ROUTINE
C       S = TRACK LENGTH ALONG THE CIRCLE COUNTED FROM THE
C           FIRST POINT IN THE DIRECTION OF THE LAST ONE
C
C      MODE   = 0 : OVERWRITE OLD PATR-BANK WITH NEW RESULTS
C      MODE   = 1 : CREATE NEW PATR-BANK WITH NEW RESULTS
C
C      IOPT =   1 : S-Z FIT SEPARATELY FOR ALL TRACKS
C      IOPT =   2 : S-Z FIT SEPARATELY FOR ALL TRACKS AND SUBSEQUENTLY
C                   A COMMON S-Z FIT FOR THOSE ONES WHICH
C                   EXTRAPOLATE WITHIN 15 MM TO THE RUN VERTEX IN R-PHI
C                   AND HAVE  | Z(R=0) | < 800 MM
C      *****************************************************************
C      *  THE FOLLOWIG OPTION NEEDS FILLING OF A COMMON IN ADDITION !!!*
C      *****************************************************************
C      IOPT =   4 : COMMON S-Z FITS FOR USER SPECIFIED (UPTO 5) SETS
C                   OF TRACKS WITH USER SPECIFIED COMMON (X,Y) POINTS
C                   IN R-PHI (OR SINGLE TRACK S-Z FIT, SEE LATER).
C                   THE SINGLE TRACK S-Z FITS ARE ONLY DONE FOR THOSE
C                   TRACKS WHICH APPEAR IN THE SETS.
C                   IF A TRACK IS NOT WITHIN 15 MM TO THE SPECIFIED
C                   (X,Y) POINT OR | Z(X,Y) | > 1600 MM, IT WILL BE
C                   DISCARDED.
C
C    TRACK SELECTION PARAMETERS FOR COMMON Z FIT ARE IN /CCMZCT/
C
C    ( IOPT <= IOPT+8 CREATES A SPECIAL BANK IN ADDITION )
C
C-----------------------------------------------------------------------
C
      IMPLICIT INTEGER*2 (H)
C
C-----------------------------------------------------------------------
C *****************************************************************
C *  THE COMMON TO BE FILLED BY THE USER IF IOPT=4 IS REQUESTED   *
C *****************************************************************
C
      COMMON/CZSSTE/ NSETZS,NTSTZS(5),KTSTZS(100,2,5),XYSTZS(2,5),
     +SQCHZS(3,5)
C
C NSETZS : NUMBER OF TRACK SETS FOR WHICH COMMON FIT IS TO BE DONE
C NTSTZS(1..NSETZS) : NUMBER OF TRACKS IN EACH SET
C KTSTZS(1..,1,I) TRACK SEQUENCE NUMBERS FOR SET "I"
C XYSTZS(1,I), XYSTZS(2,I) : COMMON X,Y FOR SET "I"
C
C FOR SINGLE TRACK FIT PLEASE SET     NSETZS=1, NTSTZS(1)=1,
C                                     KTSTZS(1,1,1)=ITRK (TR. SEQ. NR.)
C                              NOTHING ELSE NEEDS TO BE SET.
C
C  ***********
C  * ON EXIT *
C  ***********
C
C  KTSTZS(.,2,.)  IS A FLAG THAT TELLS YOU:
C       2 : TRACK WAS USED IN A COMMON Z-FIT
C       1 : SINGLE TRACK FIT WAS SUCCESSFUL BUT
C           THE TRACK WAS NOT USED IN A COMMON FIT
C       0 : TRACK WAS NOT CONSIDERED FOR REFIT
C      <0 : SINGLE TRACK FIT FAILED (TOO FEW USABLE HITS IN GENERAL)
C
C  THIS INFORMATION IS PROVIDED FOR IOPT=1 AND 2 AS WELL BUT
C  WITHOUT FILLING  KTSTZS(J,1,.)=J
C
C  SQCHZS(1,.) : SQUARE ROOT OF {SUM OF WEIGHTED RESIDUAL SQUARES
C                DIVIDED BY THE NUMBER OF DEGREES OF FREEDOM (LATTER=
C                NHITS-NTRACKS-1)} FOR COMMON FIT (.)
C                OR 0.0 IF NO COMMON FIT WAS PERFORMED.
C                THE (DIFFERENT FOR DL8 AND FADC) NORMALIZATION
C                TO GET THE CHI**2/DOF. IS LEFT FOR THE USER
C  SQCHZS(2,.) : NUMBER OF HITS USED IN THE COMMON FIT OR 0.0; REAL !!!
C  SQCHZS(3,.) : COMMON Z AT THE COMMON (X,Y)    (OR 0.0)
C
      COMMON/CCMZCT/ DIMPCT, ZCUTV, ZCUTVV, IZVCST(5)
C
C THIS COMMON IS BLOCK DATA SET IN   JADEBD
C
C     BLOCK DATA
C     COMMON/CCMZCT/ DIMPCT, ZCUTV, ZCUTVV, IZVCST(5)
C     DATA  DIMPCT /15./, ZCUTV /800./, ZCUTVV /1600./, IZVCST/5*0/
C     END
C=======================================================================
C
#include "cdata.for"
#include "calibr.for"
C
      COMMON/CWORK/NDIWRK,WRK(20000)
      DIMENSION IWRK(20000),HWRK(40000)
      EQUIVALENCE (WRK(1),IWRK(1),HWRK(1))
C
      INTEGER ITRREQ(100)
      REAL CORTRC(2,100)
      DATA LBINIT /0/, IQPATR/0/, IQJHTL/0/, IQHEAD/0/, IQJETC/0/
      DATA NPRLIM/50/,KPRLIM/0/
C=======================================================================
C
      KOPT=IOPT
      IF(KOPT.GT.8) KOPT=KOPT-8
      IF KOPT.NE.1 .AND. KOPT.NE.2 .AND. KOPT.NE.4
      THEN
         IF KPRLIM.LT.NPRLIM
         THEN
            KPRLIM=KPRLIM+1
            WRITE(6,100) IOPT
100         FORMAT(' **** ZSRFTV CALLED WITH INVALID OPTION :',I8)
         CIF
         RETURN
      CIF
C
C
      IF KOPT.EQ.4
      THEN
C PRODUCE ARRAY OF ALL TRACKS REQUESTED; CHECK CONSISTENCY OF REQUEST
         IF NTSTZS(1).EQ.1
         THEN
            KTC=KTSTZS(1,1,1)
            IF KTC.LT.1 .OR. KTC.GT.100
            THEN
               LINCON=1
            ELSE
               LINCON=0
               NTRREQ=1
               ITRREQ(1)=KTC
               KTSTZS(1,2,1)=0
            CIF
         ELSE
            IF NSETZS.GE.1.AND.NSETZS.LE.5
            THEN
               NTRREQ=0
               LINCON=0
               FOR ISET=1,NSETZS
                  SQCHZS(1,ISET)=0.
                  SQCHZS(2,ISET)=0.
                  SQCHZS(3,ISET)=0.
                  NTSETI=NTSTZS(ISET)
                  IF NTSETI.LT.2 .OR. NTSETI.GT.100
                  THEN
                     LINCON=1
                     XFOR
                  CIF
                  FOR JT=1,NTSETI
                     KTC=KTSTZS(JT,1,ISET)
                     IF KTC.LT.1 .OR. KTC.GT.100
                     THEN
                        LINCON=1
                        XFOR
                     CIF
                     KTSTZS(JT,2,ISET)=0
                     LPRES=0
                     IF NTRREQ.GT.0
                     THEN
                        FOR J=1,NTRREQ
                           IF ITRREQ(J).EQ.KTC
                           THEN
                              LPRES=1
                              XFOR
                           CIF
                        CFOR
                     CIF
                     IF LPRES.EQ.0
                     THEN
                        IF NTRREQ.EQ.100
                        THEN
                           LINCON=1
                           XFOR
                        CIF
                        NTRREQ=NTRREQ+1
                        ITRREQ(NTRREQ)=KTC
                     CIF
                  CFOR
                  IF(LINCON.NE.0) XFOR
               CFOR
            ELSE
               LINCON=1
            CIF
         CIF
         IF LINCON.NE.0
         THEN
            IF KPRLIM.LT.NPRLIM
            THEN
               KPRLIM=KPRLIM+1
               WRITE(6,200)
200            FORMAT(' **** ZSRFTV: INVALID REQUEST IN /CZSSTE/')
            CIF
            RETURN
         CIF
      ELSE
         SQCHZS(1,1)=0.
         SQCHZS(2,1)=0.
         SQCHZS(3,1)=0.
         FOR J=1,100
            KTSTZS(J,2,1)=0
         CFOR
      CIF
C-----------------------------------------------------------------------
N     INITIALIZATION
      IF LBINIT .LE.0
      THEN
         LBINIT = 1
         IQPATR = IBLN('PATR')
         IQJHTL = IBLN('JHTL')
         IQHEAD = IBLN('HEAD')
         IQJETC = IBLN('JETC')
      CIF
C
C-----------------------------------------------------------------------
C
N     CHECK IF PATR-BANK
      IF(IDATA(IQPATR).LE.0) RETURN
C-----------------------------------------------------------------------
C
C     CREATE NEW PATR BANK IF REQUESTED
      IF MODE.EQ.1
      THEN
         IPPAT0 = IDATA(IQPATR)
         NBNK1  = IDATA(IPPAT0-2) - 1
         NWRD   = IDATA(IPPAT0)
         NBYTE  = NWRD*4
         CALL CCRE(IPPATR,'PATR',NBNK1,NWRD,IERR)
         IF IERR.NE.0
         THEN
            PRINT 2900, IERR
 2900       FORMAT(' CREATION OF NEW PATR-BANK RESULTED IN ERROR',I3)
            RETURN
         CIF
N        COPY CONTENTS OF 'PATR'-BANK
         CALL MVCL(IDATA(IPPATR+1),0,IDATA(IPPAT0+1),0,NBYTE)
      CIF
C-----------------------------------------------------------------------
C
      IPPATR = IDATA(IQPATR)
      IPTR   = IDATA(IPPATR+1) + IPPATR
      LDTR   = IDATA(IPPATR+3)
      NTR    = IDATA(IPPATR+2)
C
N     CHECK IF 1 TRACK
      IF(NTR.LT.1) RETURN
C
      IF NTR.GT.100
      THEN
         IF KPRLIM.LT.NPRLIM
         THEN
            KPRLIM=KPRLIM+1
            WRITE(6,300) NTR
300         FORMAT(' **** ZSRFTV : NUMBER OF TRACKS IN PATR BANK :',
     +      I4,'. FIRST 100 WILL BE CONSIDERED.')
         CIF
         NTR=100
      CIF
C
C-----------------------------------------------------------------------
C
C GET LATEST AMPLITUDE CALIBRATION
      CALL JRECAL(IERR)
      IF IERR.NE.0
      THEN
         PRINT 6784,IERR
 6784    FORMAT(' *** ERROR IN JRECAL',I3)
         RETURN
      CIF
C-----------------------------------------------------------------------
C
N     RECALIBRATE Z-COORDINATES
      IPJETC = IDATA(IQJETC)
      IPJHTL = IDATA(IQJHTL)
C
C MODEZ=1 MEANS    CALIBRATION ONLY
C
      MODEZ  = 1
      CALL ZSFIT(IPJETC,IDATA(IPJETC-1),IPJHTL,IPPATR,MODEZ)
C
C
C=======================================================================
C
C     COLLECTION OF HIT DATA IN /CWORK/ AND SINGLE TRACK FITS
C
C DIMENSION OF WRK(.)
      NDIWRK=20000
C NUMBER OF TRACKS STORED IN /CWORK/
      NTRKS=0
C POINTER TO TRACK DATA THAT STORES THE STRUCTURE OF WRK(.) ETC.
      IDTR2=1
C LENGTH OF ABOVE DATA PRO TRACK
      LDTR2=11
C LENGTH OF HIT AND SUBSEQUENT TRACK DATA IN /CWORK/
      LHIT=8
      LTRREC=0
C POINTER TO FIRST HIT OF TRACK IN /CWORK/
      IHIT1=NTR*LDTR2+1
C
C
      FOR ITR=1,NTR
C NO SPACE TO STORE MORE TRACKS
         IF(IHIT1 .GT. NDIWRK-200-(70*LHIT+LTRREC)) XFOR
C CHECK IF TRACK CONSIDERED FOR REFIT
         IF KOPT.NE.4
         THEN
            LFIT=1
         ELSE
            LFIT=0
            FOR J=1,NTRREQ
               IF ITRREQ(J).EQ.ITR
               THEN
                  LFIT=1
                  XFOR
               CIF
            CFOR
         CIF
         IF LFIT.EQ.1
         THEN
            INDFET = 4
            CALL JFETCH(IPTR,IPJHTL,WRK(IHIT1),LHIT,IPRES,INDFET,XD,YD)
            NHIT=(IPRES-1)/LHIT
            IF NHIT.GT.1
            THEN
C OTHERWISE TRACK IS NOT CONSIDERED FOR REFIT
               IPRES=IHIT1+IPRES-1
C ----------------------------------------------------------------------
C S-Z FIT FOR SINGLE TRACK; MARK USED HITS
C IOPT IS PASSED ONLY TO INDICATE WHETHER SPECIAL BANK IS TO BE
C CREATED (IF IOPT>8)
               CALL ZSRFT1(IPTR,LDTR,IHIT1,IPRES,LHIT,IQUAL,IOPT)
C-----------------------------------------------------------------------
C
C SET SINGLE TRACK FIT FLAG IN   KTSTZS(.,2,.)
               IF KOPT.EQ.4
               THEN
                  IF NTSTZS(1).EQ.1
                  THEN
C  S-Z FIT OF A SINGLE TRACK WAS REQUESTED
                     KTSTZS(1,2,1)=IQUAL
                     RETURN
                  ELSE
                     FOR ISET=1,NSETZS
                        NTSETI=NTSTZS(ISET)
                        FOR JT=1,NTSETI
                     IF(KTSTZS(JT,1,ISET).EQ.ITR)KTSTZS(JT,2,ISET)=IQUAL
                        CFOR
                     CFOR
                  CIF
               ELSE
                  KTSTZS(ITR,2,1)=IQUAL
               CIF
C-----------------------------------------------------------------------
               IF IQUAL.GT.0 .AND. KOPT.NE.1
               THEN
C STORE TRACK FOR SUBSEQUENT COMMON S-Z FIT
                  NTRKS=NTRKS+1
                  IWRK(IDTR2  )=ITR
                  IWRK(IDTR2+1)=IPTR
C
                  IF(KOPT.EQ.2) IQUAL=2
C
                  IWRK(IDTR2+2)=IQUAL
                  IWRK(IDTR2+3)=IHIT1
                  IWRK(IDTR2+4)=IPRES
C
                  IHIT1=IPRES+LTRREC
                  IDTR2=IDTR2+LDTR2
               CIF
            CIF
         CIF
         IPTR=IPTR+LDTR
      CFOR
C
C NO COMMON FIT IS REQUESTED OR POSSIBLE
C
      IF(KOPT.EQ.1.OR.NTRKS.LT.2) RETURN
C
C=======================================================================
C
C     COMMON S-Z FIT TO RUN VERTEX
C
C
      IF KOPT.EQ.2
      THEN
         IRUN=HDATA( 2*IDATA(IQHEAD) + 10)
         IF  IRUN.GE.100
         THEN
            IPV    = ICALIB(10)
            XCOMM  = ACALIB(IPV+ 1)
            YCOMM  = ACALIB(IPV+ 3)
         ELSE
            XCOMM  = 0.
            YCOMM  = 0.
         CIF
         IVNEED=IZVCST(1)
C
         PERFORM COMMZS
C
      CIF
C
C=======================================================================
C
C     COMMON S-Z FIT FOR USER SPECIFIED TRACK SETS
C
C
      IF KOPT.EQ.4
      THEN
         FOR ISET=1,NSETZS
            NTSETI=NTSTZS(ISET)
            NTRFIT=0
            IDTR2=1
            FOR JTR=1,NTRKS
               ITR=IWRK(IDTR2)
               IWRK(IDTR2+2)=1
               FOR JT=1,NTSETI
                  IF KTSTZS(JT,1,ISET).EQ.ITR
                  THEN
                     NTRFIT=NTRFIT+1
                     IWRK(IDTR2+2)=2
                     XFOR
                  CIF
               CFOR
               IDTR2=IDTR2+LDTR2
            CFOR
            IF NTRFIT.GE.2
            THEN
               XCOMM=XYSTZS(1,ISET)
               YCOMM=XYSTZS(2,ISET)
               IVNEED=IZVCST(ISET)
               PERFORM COMMZS
            CIF
         CFOR
      CIF
C
      RETURN
C
C
C=======================================================================
C
C
C  CODE FOR THE COMMON S-Z FIT
C  HITS ARE USED   IFF   MARKED AS USED IN ZSRFT1
C
      PROC COMMZS
C
C STARTING VALUE FOR COMMON Z AND
C CHECK IF TRACK CONSISTENT WITH THE COMMON POINT IN R-PHI
C AND IF Z AT COMMON POINT IS WITHIN LIMITS
C
         ZCUT=ZCUTV
         IF(KOPT.EQ.4) ZCUT=ZCUTVV
         IDTR2=1
         NTRFIT=0
         ZCOMM=0.
         FOR JTR=1,NTRKS
            IF IWRK(IDTR2+2).EQ.2
            THEN
C TRACK WAS REQUESTED
               IPTR=IWRK(IDTR2+1)
C CALCULATE DISTANCE OF COMMON POINT TO CIRCLE IN R-PHI
               CURVXY=ADATA(IPTR+25)
               IF(ABS(CURVXY).LT.1.E-9) CURVXY = SIGN(1.E-9,CURVXY)
               DDR0=DISTXY(ADATA(IPTR+5),ADATA(IPTR+6),ADATA(IPTR+8),
     +         ADATA(IPTR+9),1./CURVXY,XCOMM,YCOMM,CORTRC(1,JTR),
     +         CORTRC(2,JTR),FI)
               IF ABS(DDR0).LT.DIMPCT
               THEN
C CIRCLE CLOSE ENOUGH TO COMMON POINT
C TRACK DIRECTION AT COMMON POINT
                  WRK(IDTR2+5)=COS(FI)
                  WRK(IDTR2+6)=SIN(FI)
C CALCULATE Z OF TRACK AT THE COMMON POINT
                  DDR0=DISTXY(ADATA(IPTR+5),ADATA(IPTR+6),ADATA(IPTR+8),
     +            ADATA(IPTR+9),1./CURVXY,0.,0.,XP,YP,FI)
                  UU=SQRT((CORTRC(1,JTR)-XP)**2+(CORTRC(2,JTR)-YP)**2)
                  IF(ABS(CURVXY*UU).GT.1.E-5)
     +            UU=2.*ARSIN(.5*CURVXY*UU)/CURVXY
                  ZCOMM1=ADATA(IPTR+31)+ADATA(IPTR+30)*UU
                  IF ABS(ZCOMM1) .LT. ZCUT
                  THEN
                     WRK(IDTR2+7)=ZCOMM1
                     WRK(IDTR2+10)=CURVXY
                     ZCOMM=ZCOMM+ZCOMM1
                     NTRFIT=NTRFIT+1
                  ELSE
                     IWRK(IDTR2+2)=1
                  CIF
               ELSE
                  IWRK(IDTR2+2)=1
               CIF
            CIF
            IDTR2=IDTR2+LDTR2
         CFOR
C
C STARTING VALUE OF COMMON Z; NO CHECK IF Z OF TRACK CONSISTENT WITH IT
C COLLECT SUMS FOR COMMON Z FIT
         IF NTRFIT.GE.2
         THEN
            ZCOMM=ZCOMM/NTRFIT
COMIT       DZLIM=400.
COMIT       NTRFIT=0
            NHTOT=0
            IF IVNEED.EQ.1
            THEN
C VERTEX CONSTRAINT OF 10 MM ON THE COMMON Z
               S0=(20./10.)**2
               IF( LDATYP(DUMMY).EQ.2 ) S0=S0*4.
               S3=-ZCOMM*S0
               S7=ZCOMM**2*S0
            ELSE
               S0=0.
               S3=0.
               S7=0.
            CIF
            S5=0.
            S6=0.
            IDTR2=1
            FOR JTR=1,NTRKS
               IF IWRK(IDTR2+2).EQ.2
               THEN
C TRACK REQUESTED AND SURVIVED THE R-PHI DISTANCE AND Z CUTS
COMIT             IF ABS(WRK(IDTR2+7)-ZCOMM).LT.DZLIM
COMIT             THEN
C TRACK CLOSE ENOUGH IN Z
COMIT                NTRFIT=NTRFIT+1
                     IPTR=IWRK(IDTR2+1)
                     CURVXY=WRK(IDTR2+10)
                     CTGTH=ADATA(IPTR+30)
                     S1=0.
                     S2=0.
                     S4=0.
C LOOP OVER HITS
                     IPCO =IWRK(IDTR2+3)
                     IPCO9=IWRK(IDTR2+4)-LHIT
                     FOR IP=IPCO,IPCO9,LHIT
                        IF HWRK(2*IP+3).EQ.1
                        THEN
C HIT WAS USED IN THE SINGLE TRACK FIT
C CALCULATE TRACK LENGTH IN R-PHI COUNTED FROM COMMON POINT
                           UX=WRK(IP+3)-CORTRC(1,JTR)
                           UY=WRK(IP+4)-CORTRC(2,JTR)
                           UU=SQRT(UX**2+UY**2)
                           IF(ABS(CURVXY*UU).GT.1.E-5)
     +                     UU=2.*ARSIN(.5*CURVXY*UU)/CURVXY
                           IF(UX*WRK(IDTR2+5)+UY*WRK(IDTR2+6).LT.0.)
     +                     UU=-UU
C RESIDUAL TO LINE WITH START PARAMETERS
                           DZ=WRK(IP+5)-ZCOMM-CTGTH*UU
                           W=WRK(IP+7)
                           NHTOT=NHTOT+1
                           S0=S0+W
                           S3=S3+DZ*W
                           S1=S1+UU*W
                           S2=S2+UU**2*W
                           S4=S4+DZ*UU*W
                           S7=S7+DZ**2*W
                        CIF
                     CFOR
                     WRK(IDTR2+7)=S4
                     WRK(IDTR2+8)=S1
                     WRK(IDTR2+9)=S2
                     S5=S5+S1*S4/S2
                     S6=S6+S1*S1/S2
COMIT             ELSE
COMIT                IWRK(IDTR2+2)=1
COMIT             CIF
               CIF
               IDTR2=IDTR2+LDTR2
            CFOR
C
C RESULTS OF COMMON FIT; FILL 'PATR' BANK
COMIT       IF NTRFIT.GE.2
COMIT       THEN
               DZCOMM=(S3-S5)/(S0-S6)
               ZCOMMR=ZCOMM+DZCOMM
C
               ISETOP=1
               IF(KOPT.EQ.4) ISETOP=ISET
               SQCHZS(2,ISETOP)=NHTOT
               SQCHZS(3,ISETOP)=ZCOMMR
               SQCHZS(1,ISETOP)=S7+DZCOMM*(DZCOMM*S0-2.*S3)
C
               IDTR2=1
               FOR JTR=1,NTRKS
                  IF IWRK(IDTR2+2).EQ.2
                  THEN
                     IPTR=IWRK(IDTR2+1)
                     PERFORM FITBNK
C
C SET SINGLE TRACK FIT FLAG IN   KTSTZS(.,2,.)
                     ITR=IWRK(IDTR2)
                     IF KOPT.EQ.4
                     THEN
                        FOR JT=1,NTSETI
                         IF(KTSTZS(JT,1,ISET).EQ.ITR)KTSTZS(JT,2,ISET)=201759400
                        CFOR
                     ELSE
                        KTSTZS(ITR,2,1)=2
                     CIF
C
                  CIF
                  IDTR2=IDTR2+LDTR2
               CFOR
C
               IF(SQCHZS(1,ISETOP).LT.1.E-5) SQCHZS(1,ISETOP)=1.E-5
               SQCHZS(1,ISETOP)=SQRT(SQCHZS(1,ISETOP)/(NHTOT-NTRFIT-1))
C
COMIT       CIF
         CIF
      CPROC
C=======================================================================
C
C
N     *************************
N     *      F I T B N K      *
N     *************************
C
C
N     SET UP FIT-BANK
      PROC FITBNK
C
      DCTGTH=(WRK(IDTR2+7)-DZCOMM*WRK(IDTR2+8))/WRK(IDTR2+9)
C
      SQCHZS(1,ISETOP)=SQCHZS(1,ISETOP)+DCTGTH*(DCTGTH*WRK(IDTR2+9)+
     +2.*DZCOMM*WRK(IDTR2+8)-2.*WRK(IDTR2+7))
C
      CTGTH=ADATA(IPTR+30)+DCTGTH
      CSTH = 1./SQRT(CTGTH**2 + 1.)
      SNTH  = CSTH * CTGTH
      CURVXY=WRK(IDTR2+10)
C
C
C
N     COPY TRACK BANK
         IFREE=NDIWRK-100
         CALL MVCL(IWRK(IFREE),0,IDATA(IPTR+1),0,4*LDTR)
C
N     FILL FIT-BANK
         IP    = IFREE - 1
         IWRK(IP+ 2) = LOR(IWRK(IP+2),8192)
C FIRST POINT ON TRACK
         UX=WRK(IP+5)-CORTRC(1,JTR)
         UY=WRK(IP+6)-CORTRC(2,JTR)
         UU=SQRT(UX**2+UY**2)
         IF(ABS(CURVXY*UU).GT.1.E-5) UU=2.*ARSIN(.5*CURVXY*UU)/CURVXY
         IF(UX*WRK(IDTR2+5)+UY*WRK(IDTR2+6).LT.0.) UU=-UU
         WRK (IP+ 7) = ZCOMMR+CTGTH*UU
         A=SQRT(WRK(IP+8)**2+WRK(IP+9)**2)
         WRK (IP+ 8) = WRK (IP+ 8)/A*CSTH
         WRK (IP+ 9) = WRK (IP+ 9)/A*CSTH
         WRK (IP+10) = SNTH
C LAST POINT ON TRACK
         UX=WRK(IP+12)-CORTRC(1,JTR)
         UY=WRK(IP+13)-CORTRC(2,JTR)
         UU=SQRT(UX**2+UY**2)
         IF(ABS(CURVXY*UU).GT.1.E-5) UU=2.*ARSIN(.5*CURVXY*UU)/CURVXY
         IF(UX*WRK(IDTR2+5)+UY*WRK(IDTR2+6).LT.0.) UU=-UU
         WRK (IP+14) = ZCOMMR+CTGTH*UU
         A=SQRT(WRK(IP+15)**2+WRK(IP+16)**2)
         WRK (IP+15) = WRK (IP+15)/A*CSTH
         WRK (IP+16) = WRK (IP+16)/A*CSTH
         WRK (IP+17) = SNTH
C STORE COMMON FIT PARAMETERS
         IWRK(IP+29) = 2
         WRK (IP+30) = CTGTH
C GET CLOSEST POINT (XP,YP) TO ORIGIN
         DDR0=DISTXY(ADATA(IPTR+5),ADATA(IPTR+6),ADATA(IPTR+8),
     +   ADATA(IPTR+9),1./CURVXY,0.,0.,XP,YP,FI)
C CALCULATE TRACK LENGTH ALONG CIRCLE FROM FIRST POINT TO (XP,YP)
         UX=XP-CORTRC(1,JTR)
         UY=YP-CORTRC(2,JTR)
         UU=SQRT(UX**2+UY**2)
         IF(ABS(CURVXY*UU).GT.1.E-5) UU=2.*ARSIN(.5*CURVXY*UU)/CURVXY
         IF(UX*WRK(IDTR2+5)+UY*WRK(IDTR2+6).LT.0.) UU=-UU
         WRK (IP+31) = ZCOMMR+CTGTH*UU
N     PUT RESULT INTO PATR-BANK
         CALL MVCL(IDATA(IPTR+1),0,IWRK(IFREE),0,4*LDTR)
      CPROC
      END
C   09/06/83 801121951  MEMBER NAME  ZSRFT10  (JADEGS)      SHELTRAN
      SUBROUTINE ZSRFT1(IPTR,LDTR,IPCO0,IPRES,LHIT,IQUAL,IOPT)
C
C        S-Z ("HELIX") REFIT OF A SINGLE TRACK
C
C    TEST VERSION 1.
C
C                                J. SPITZER  12/4/87
C
C    COVARIANCE MATRIX FOR FIT PARAMETERS IF AREA (LDTR) LARGE ENOUGH
C
      IMPLICIT INTEGER*2 (H)
C
#include "cdata.for"
#include "calibr.for"
C
      COMMON/CWORK/NDIWRK,WRK(200)
      DIMENSION IWRK(200),HWRK(400)
      EQUIVALENCE (WRK(1),IWRK(1),HWRK(1))
C
C
      INTEGER DATE(5), IDAY /0/
C-----------------------------
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
C
C
      REAL RESCUT/600./
      DIMENSION ISORT1(71),ISORT2(3,71),IRESHT(71),ISORT3(91)
     +,ISORT4(2,91),KSORT3(91),KSZSRT(91,2)
      DATA IQHEAD/0/,MASK4/ZFFFFCFFF/
C
C
C
N     INITIALIZATION
      DATA LBINIT /0/
      IF LBINIT .EQ. 0
      THEN
         LBINIT = 1
C
         IQHEAD = IBLN('HEAD')
C
         CALL DAY2(DATE)
         IDAY = DATE(1)*1000 + DATE(2)
C
         WRITE(6,137)
 137     FORMAT(/,' *** ZSRFT1 ***  A NEW S-Z FITTING ROUTINE',/,
     +            '                 TEST VERSION 1. (J. SPITZER)',/
     +   ' COVARIANCE MATRIX IS PROVIDED',/,
     +   ' IF THE TRACK AREA LONG ENOUGH, THE NUMBER',/,
     +  ' OF HITS USED IN THE FIT IS AT LEAST 4.',
     +   /)
      CIF
C
C
N     GET RUN #
      IPHEAD = IDATA(IQHEAD)*2
      NRUN = HDATA(IPHEAD+10)
      NEV  = HDATA(IPHEAD+11)
C
N     TRACK #
      ITRK = IDATA(IPTR+1)
C
C=======================================================================
      IF NRUN.LT.24200
      THEN
         SRESO=24.
         STPFAC=.85
      ELSE
         SRESO=32.
         STPFAC=.92
      CIF
      SIGMIN=(SRESO/1.6)**2
      SIGFAC=(.14/30.)**2
C
C
C-----------------------------------------------------------------------
C
C     GYMNASTICS FOR PRIVATE HIT QUALIFICATION
C     AND FOR HANDLING MORE HITS ON SAME WIRE
C
C  STARTING VALUES OF FIT PARAMETERS Z=P1*S+P2
C
      PAR1=ADATA(IPTR+30)
      PAR2=ADATA(IPTR+7)
C
      NHALL=0
      NHWIR=0
      NHPOT=0
      NHPOTT=0
      IPCO=IPCO0
      IPCO9=IPRES-LHIT
      REPEAT
         NHWIR=NHWIR+1
         IF NHWIR.GT.70
         THEN
            IQUAL=-1
            RETURN
         CIF
         ISORT1(NHWIR)=NHWIR
         ISORT2(1,NHWIR)=IPCO
         ISORT2(3,NHWIR)=0
         IW0=IWRK(IPCO)
         LFL=0
         LFL1=0
         LFL2=0
         WHILE IPCO.LE.IPCO9
            IW9=IWRK(IPCO)
            IF IW9.EQ.IW0
            THEN
N        HIT ON THE SAME WIRE
               NHALL=NHALL+1
               IF NHALL.GT.90
               THEN
                  IQUAL=-2
                  RETURN
               CIF
               IF(ISORT2(3,NHWIR).EQ.0) ISORT2(2,NHWIR)=NHALL
               ISORT2(3,NHWIR)=ISORT2(3,NHWIR)+1
C
               KSORT3(NHALL)= NHALL
               LZGOOD=HWRK(2*IPCO+1)
               IF LZGOOD.NE.0
               THEN
                  ISORT3(NHALL)=-1
                  KSZSRT(NHALL,1)= 100000
               ELSE
                  ISORT3(NHALL)= 1
                  IF LFL2.EQ.0
                  THEN
                     KSZSRT(NHALL,1)= WRK(IPCO+6)
                     KSZSRT(NHALL,2)= WRK(IPCO+5)
                     LFL2=1
                  ELSE
                     KSZSRT(NHALL,1)= 100000
                  CIF
                  LFL=1
                  IF ABS(WRK(IPCO+5)-PAR1*WRK(IPCO+6)-PAR2).LT.RESCUT
                  THEN
                     IF LFL1.EQ.0
                     THEN
                        LFL1=1
                        NHPOTT=NHPOTT+1
                     CIF
                  CIF
               CIF
C
               IPCO=IPCO+LHIT
            ELSE
               XWHILE
            CIF
         CWHILE
         IF LFL.EQ.1
         THEN
            NHPOT=NHPOT+1
         ELSE
            ISORT3(ISORT2(2,NHWIR))=-2
         CIF
      UNTIL IPCO.GT.IPCO9
C-----------------------------------------------------------------------
C
C IF LESS THAN 2 WIRES WITH GOOD Z MEASUREMENT, NOTHING DONE
      IF NHPOT.LT.2
      THEN
         IQUAL=-3
         RETURN
      CIF
C-----------------------------------------------------------------------
      KFLIP=2
C
      NHFIT=NHPOTT
      LFOUND=-1
      IF NHPOTT.LT.6.OR.NHPOTT.LT.NHPOT*.75
      THEN
C TRY TO FIND BETTER START VALUES
         PERFORM STVSEA
      CIF
C-----------------------------------------------------------------------
C
      INDMAX=NHFIT/4+1
      IF(INDMAX.GT.13) INDMAX=13
      INDFIT=0
      WHILE INDFIT.LT.INDMAX
         INDFIT=INDFIT+1
N    LINEAR FIT
         PERFORM LINFIT
         IF LNOCON.EQ.1
         THEN
C NO CONVERGENCE AS INDICATED BY LOSS OF TOO MANY HITS
            IQUAL=-4
            RETURN
         CIF
         IF(SIG.LT.SIGMIN) XWHILE
         IF INDFIT.GE.2
         THEN
            PERFORM LLSTOP
            IF LSTOP.EQ.1
            THEN
C      PREVIOUS FIT ACCEPTED, RESTORE ITS RESULTS
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
               XWHILE
            CIF
         CIF
         IF(INDFIT.EQ.INDMAX.OR.NHFIT.EQ.2) XWHILE
N      SAVE FIT RESULTS
         NHFTLS=NHFIT
         PAR1LS=PAR1
         PAR2LS=PAR2
         SIGLST=SIG
         S0LS=S0
         S1LS=S1
         S2LS=S2
         S3LS=S3
         S4LS=S4
N      HIT CLEANING
         PERFORM HITCLN
      CWHILE
C
C
N     SET UP PATR-BANK
      PERFORM FITBNK
      IQUAL=1
      RETURN
C=======================================================================
C
N     *************************
N     *      L I N F I T      *
N     *************************
C
C
N      LINEAR FIT
      PROC LINFIT
C
      LNOCON=0
N     GET EQUATIONS
      KFLIP=3-KFLIP
      NHF1=0
      S0 = 0.
      S1 = 0.
      S2 = 0.
      S3 = 0.
      S4 = 0.
      FOR IHWIR=1,NHWIR
         IH=ISORT2(2,IHWIR)
         NNH=ISORT2(3,IHWIR)
         FOR JNH=1,NNH
            ISORT4(KFLIP,IH+JNH-1)=0
         CFOR
         IF ISORT3(IH).EQ.1 .OR. ISORT3(IH).EQ.-1.AND.NNH.GT.1
         THEN
            RESMIN=10000.
            FOR JNH=1,NNH
               JH=IH+JNH-1
               IF ISORT3(JH).EQ.1
               THEN
                  IPCO=ISORT2(1,IHWIR)+(JNH-1)*LHIT
                  SA = WRK(IPCO+6)
                  ZA = WRK(IPCO+5)
                  WA = WRK(IPCO+7)
                  DZRESA=ZA-PAR1*SA-PAR2
                  DF0=ABS(DZRESA)
                  IF DF0.LT.RESMIN
                  THEN
                     RESMIN=DF0
                     S=SA
                     W=WA
                     DZRES=DZRESA
                     JHUSE=JH
                  CIF
               CIF
            CFOR
            IF RESMIN.LT.RESCUT
            THEN
               NHF1=NHF1+1
               S0=S0+W
               S1=S1+S*W
               S2=S2+S**2*W
               S3=S3+DZRES*W
               S4=S4+DZRES*S*W
               ISORT4(KFLIP,JHUSE)=1
            ELSE
               ISORT3(IH)=-2
            CIF
         CIF
      CFOR
      IF NHF1.LT.2 .OR. S2.LT.1.
      THEN
         LNOCON=1
      ELSE
         NHFIT=NHF1
C
N        SOLVE EQUATIONS
         F1 = 1. / S2
         XX12 = S1*F1
         YY1  = S4*F1
         PARR2=(S3-S1*YY1)/(S0-S1*XX12)
         PAR1=YY1-PARR2*XX12+PAR1
         PAR2=PAR2+PARR2
C
N     CALC. CHISQ + SOLVE L/R AMBIGUITY
         CHISQ = 0.
         NHF1=0
         FOR IHWIR=1,NHWIR
            IRESHT(IHWIR)=-1
            IH=ISORT2(2,IHWIR)
            NNH=ISORT2(3,IHWIR)
            IF ISORT3(IH).GE.0 .OR. ISORT3(IH).EQ.-1.AND.NNH.GT.1
            THEN
               RESMIN=10000.
               FOR JNH=1,NNH
                  JH=IH+JNH-1
                  IF ISORT3(JH).GE.0
                  THEN
                     IFLG=ISORT3(JH)
                     IPCO=ISORT2(1,IHWIR)+(JNH-1)*LHIT
                     SA = WRK(IPCO+6)
                     ZA = WRK(IPCO+5)
                     WA = WRK(IPCO+7)
                     DF0=ABS(ZA-PAR1*SA-PAR2)
                     IF DF0.LT.RESMIN
                     THEN
                        RESMIN=DF0
                        W=WA
                     CIF
                  CIF
               CFOR
               IF RESMIN.LT.8000.
               THEN
                  RESMIN=RESMIN*SQRT(W)
                  IRESHT(IHWIR)=RESMIN*1.E4
                  IF IFLG.EQ.1
                  THEN
                     CHISQ=CHISQ+RESMIN**2
                     NHF1=NHF1+1
                  CIF
               CIF
            CIF
         CFOR
         IF NHF1.LT.2
         THEN
            LNOCON=1
         ELSE
            IF NHF1.EQ.2
            THEN
               SIG=1.E-5
            ELSE
               SIG=CHISQ/(NHF1-2)
            CIF
         CIF
         NHFIT=NHF1
      CIF
      CPROC
C=======================================================================
      PROC HITCLN
C      LABEL HITS NOT TO BE USED IN THE NEXT ITRATION
C-------------------------------------------------------------
C
N       SORT HITS ACCORDING TO RESIDUALS
C  EXCLUDE THE INDFIT LARGEST RESIDUAL HITS,
C  RESTORE THE OTHERS (EXLUDED FOR EVER HITS NOT COUNTED)
C
         CALL SHELL9(IRESHT,ISORT1,NHWIR)
         KOMIT=0
         FOR J1=1,NHWIR
            IHWIR=ISORT1(NHWIR+1-J1)
            IPCO=ISORT2(1,IHWIR)
            NNH=ISORT2(3,IHWIR)
            IH=ISORT2(2,IHWIR)
            LFLG=0
            FOR JNH=1,NNH
               IHA=IH+JNH-1
               IQA=ISORT3(IHA)
               IF IQA.GT.-1
               THEN
                  IF LFLG.EQ.0
                  THEN
                     LFLG=1
                     KOMIT=KOMIT+1
                  CIF
                  IF KOMIT.LE.INDFIT
                  THEN
                     ISORT3(IHA)=0
                  ELSE
                     ISORT3(IHA)=1
                  CIF
               CIF
            CFOR
         CFOR
      CPROC
C=======================================================================
      PROC LLSTOP
         IF INDFIT.LE.6
         THEN
            INDCK=INDFIT-1
         ELSE
            INDCK=5
         CIF
         ICHCK=NCHECK(INDCK)
         WHILE SIGLST*SIGFAC.LT.RCHECK(ICHCK,1,INDCK)
            ICHCK=ICHCK-1
         CWHILE
         IF(ICHCK.LT.1) ICHCK=1
         IF SIG/SIGLST*STPFAC.GT.RCHECK(ICHCK,2,INDCK)
         THEN
            LSTOP=1
         ELSE
            LSTOP=0
         CIF
      CPROC
C=======================================================================
C
C
N     *************************
N     *      F I T B N K      *
N     *************************
C
C
N     SET UP FIT-BANK
      PROC FITBNK
C
      CTGTH=PAR1
      CSTH = 1./SQRT(CTGTH**2 + 1.)
      SNTH  = CSTH * CTGTH
C
C
C
N     COPY TRACK BANK
         IFREE=NDIWRK-100
         CALL MVC(IWRK(IFREE),0,IDATA(IPTR+1),0,4*LDTR)
C
N     FILL FIT-BANK
         IP    = IFREE - 1
         IWRK(IP+ 2) = LAND(IWRK(IP+2),MASK4)
         IWRK(IP+ 2) = LOR(IWRK(IP+2),4096)
         WRK (IP+ 7) = PAR2
         A=SQRT(WRK(IP+8)**2+WRK(IP+9)**2)
         WRK (IP+ 8) = WRK (IP+ 8)/A*CSTH
         WRK (IP+ 9) = WRK (IP+ 9)/A*CSTH
         WRK (IP+10) = SNTH
C CALCULATE TRACK LENGTH IN R-PHI FROM FIRST TO LAST POINT ON TRACK
         CURVXY=WRK(IP+25)
         IF(ABS(CURVXY).LT.1.E-9) CURVXY = SIGN(1.E-9,CURVXY)
         UU=SQRT((WRK(IP+12)-WRK(IP+5))**2+(WRK(IP+13)-WRK(IP+6))**2)
         IF(ABS(CURVXY*UU).GT.1.E-5) UU=2.*SIN(.5*CURVXY*UU)/CURVXY
         WRK (IP+14) = PAR2+UU*PAR1
         A=SQRT(WRK(IP+15)**2+WRK(IP+16)**2)
         WRK (IP+15) = WRK (IP+15)/A*CSTH
         WRK (IP+16) = WRK (IP+16)/A*CSTH
         WRK (IP+17) = SNTH
         IWRK(IP+33) = NHFIT
         WRK (IP+32) = SQRT(SIG)
C FIT TYPE WILL BE 2: "HELIX FIT"
         IWRK(IP+29) = 2
         WRK (IP+30) = PAR1
C GET CLOSEST POINT (XP,YP) TO ORIGIN
         DDR0=DISTXY(ADATA(IPTR+5),ADATA(IPTR+6),ADATA(IPTR+8),
     +   ADATA(IPTR+9),1./CURVXY,0.,0.,XP,YP,FI)
C CALCULATE TRACK LENGTH ALONG CIRCLE FROM FIRST POINT TO (XP,YP)
         UU=SQRT((XP-WRK(IP+5))**2+(XP-WRK(IP+6))**2)
         IF(ABS(CURVXY*UU).GT.1.E-5) UU=2.*SIN(.5*CURVXY*UU)/CURVXY
         WRK (IP+31) = PAR2-PAR1*UU
C
         IF LDTR.GE.59 .AND. NHFIT.GE.4 .AND. LNOCON.EQ.0
         THEN
C CALCULATE COVARIANCE MATRIX
            DET=S0*S2-S1**2
            FACT=SIG/DET
            WRK(IP+56)=SIG*(NHFIT-2)/20.**2
            WRK(IP+57)=(S2+2.*UU*S1+UU**2*S0)*FACT
            WRK(IP+58)=-(UU*S0+S1)*FACT
            WRK(IP+59)=S0*FACT
         CIF
N     PUT RESULT INTO PATR-BANK
         CALL MVC(IDATA(IPTR+1),0,IWRK(IFREE),0,4*LDTR)
C
C MARK HITS USED IN THE FIT
         FOR IHWIR=1,NHWIR
            IH=ISORT2(2,IHWIR)
            NNH=ISORT2(3,IHWIR)
            FOR JNH=1,NNH
               IPCO=ISORT2(1,IHWIR)+(JNH-1)*LHIT
               HWRK(2*IPCO+3)=ISORT4(KFLIP,IH+JNH-1)
            CFOR
         CFOR
C
C CREATE Z-S BANK 'ZSPD'
C
         IF IOPT.GT.8
         THEN
            CALL CCRE(NPZSPD,'ZSPD',ITRK,5*NHALL+1,IERR)
            IF IERR.EQ.0
            THEN
               CALL BSAW(1,'ZSPD')
               NPZSP1=NPZSPD+1
               IDATA(NPZSP1)=5
               FOR IHWIR=1,NHWIR
                  IH=ISORT2(2,IHWIR)
                  NNH=ISORT2(3,IHWIR)
                  FOR JNH=1,NNH
                     IPCO=ISORT2(1,IHWIR)+(JNH-1)*LHIT
                     IFL=ISORT4(KFLIP,IH+JNH-1)
                     IF IFL.EQ.1
                     THEN
                        IFL=0
                     ELSE
                        IFL=16
                     CIF
                     IDATA(NPZSP1+1)=HWRK(2*IPCO+4)
                     ADATA(NPZSP1+2)=WRK(IPCO+6)
                     ADATA(NPZSP1+3)=WRK(IPCO+5)
                     IDATA(NPZSP1+4)=IFL
                     ADATA(NPZSP1+5)=WRK(IPCO+7)
                     NPZSP1=NPZSP1+5
                  CFOR
               CFOR
            CIF
         CIF
      CPROC
C=======================================================================
      PROC STVSEA
C  SEARCH FOR STARTING VALUES
C
C  ORDER ACCORDING TO S
         CALL SHELL9(KSZSRT(1,1),KSORT3,NHALL)
         NH9=NHALL
         WHILE KSZSRT(KSORT3(NH9),1).GT.99999
            NH9=NH9-1
         CWHILE
         NH99=NH9
         NH1=1
         LFOUND=0
         NHMAX=0
         CI2MAX=1.E10
         WHILE NH9.GT.NH1
            FOR KK=1,3
               IF KK.EQ.1
               THEN
                  JH1=NH1
                  JH9=NH9
               ELSE
                  IF KK.EQ.2
                  THEN
                     JH9=JH9-1
                  ELSE
                     JH9=JH9+1
                     JH1=JH1+1
                  CIF
               CIF
               IF JH9.GT.JH1
               THEN
                  AS1=KSZSRT(KSORT3(JH1),1)
                  AS9=KSZSRT(KSORT3(JH9),1)
                  IF(AS9.LT.AS1+5.) XWHILE
                  AZ1=KSZSRT(KSORT3(JH1),2)
                  AZ9=KSZSRT(KSORT3(JH9),2)
                  PAR1=(AZ9-AZ1)/(AS9-AS1)
                  PAR2=AZ1-PAR1*AS1
                  NHFIT=0
                  CI2=0.
                  FOR I=1,NH99
                     DZ=ABS(KSZSRT(KSORT3(I),2)-PAR1*KSZSRT(KSORT3(I),1)
     +               -PAR2)
                     IF DZ.LT.RESCUT
                     THEN
                        NHFIT=NHFIT+1
                        CI2=CI2+DZ**2
                     CIF
                  CFOR
                  IF NHFIT.GT.NHPOT*.75
                  THEN
                     LFOUND=1
                     XWHILE
                  CIF
                  IF NHFIT.GT.NHMAX .OR.NHFIT.EQ.NHMAX.AND.CI2.LT.CI2MAX
                  THEN
                     NHMAX=NHFIT
                     P1MAX=PAR1
                     P2MAX=PAR2
                     CI2MAX=CI2
                  CIF
               CIF
            CFOR
            NH1=NH1+1
            NH9=NH9-1
         CWHILE
         IF LFOUND.EQ.0 .AND. NHMAX.GT.0
         THEN
            NHFIT=NHMAX
            PAR1=P1MAX
            PAR2=P2MAX
         CIF
      CPROC
      END
C   09/06/83 802221542  MEMBER NAME  ZSRFT11  (JADEGS)      SHELTRAN
      SUBROUTINE ZSRFT1(IPTR,LDTR,IPCO0,IPRES,LHIT,IQUAL,IOPT)
C
C        S-Z ("HELIX") REFIT OF A SINGLE TRACK
C
C    22.2.88   MVC CHANGED TO MVCL (256 BYTES NOT ENOUGH!)  J.H./J.O.
C    TEST VERSION 1.
C
C                                J. SPITZER  12/4/87
C
C    COVARIANCE MATRIX FOR FIT PARAMETERS IF AREA (LDTR) LARGE ENOUGH
C
      IMPLICIT INTEGER*2 (H)
C
#include "cdata.for"
#include "calibr.for"
C
      COMMON/CWORK/NDIWRK,WRK(200)
      DIMENSION IWRK(200),HWRK(400)
      EQUIVALENCE (WRK(1),IWRK(1),HWRK(1))
C
C
      INTEGER DATE(5), IDAY /0/
C-----------------------------
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
C
C
      REAL RESCUT/600./
      DIMENSION ISORT1(71),ISORT2(3,71),IRESHT(71),ISORT3(91)
     +,ISORT4(2,91),KSORT3(91),KSZSRT(91,2)
      DATA IQHEAD/0/,MASK4/ZFFFFCFFF/
C
C
C
N     INITIALIZATION
      DATA LBINIT /0/
      IF LBINIT .EQ. 0
      THEN
         LBINIT = 1
C
         IQHEAD = IBLN('HEAD')
C
         CALL DAY2(DATE)
         IDAY = DATE(1)*1000 + DATE(2)
C
         WRITE(6,137)
 137     FORMAT(/,' *** ZSRFT1 ***  A NEW S-Z FITTING ROUTINE',/,
     +            '                 TEST VERSION 1. (J. SPITZER)',/
     +   ' COVARIANCE MATRIX IS PROVIDED',/,
     +   ' IF THE TRACK AREA LONG ENOUGH, THE NUMBER',/,
     +  ' OF HITS USED IN THE FIT IS AT LEAST 4.',
     +   /)
      CIF
C
C
N     GET RUN #
      IPHEAD = IDATA(IQHEAD)*2
      NRUN = HDATA(IPHEAD+10)
      NEV  = HDATA(IPHEAD+11)
C
N     TRACK #
      ITRK = IDATA(IPTR+1)
C
C=======================================================================
      IF NRUN.LT.24200
      THEN
         SRESO=24.
         STPFAC=.85
      ELSE
         SRESO=32.
         STPFAC=.92
      CIF
      SIGMIN=(SRESO/1.6)**2
      SIGFAC=(.14/30.)**2
C
C
C-----------------------------------------------------------------------
C
C     GYMNASTICS FOR PRIVATE HIT QUALIFICATION
C     AND FOR HANDLING MORE HITS ON SAME WIRE
C
C  STARTING VALUES OF FIT PARAMETERS Z=P1*S+P2
C
      PAR1=ADATA(IPTR+30)
      PAR2=ADATA(IPTR+7)
C
      NHALL=0
      NHWIR=0
      NHPOT=0
      NHPOTT=0
      IPCO=IPCO0
      IPCO9=IPRES-LHIT
      REPEAT
         NHWIR=NHWIR+1
         IF NHWIR.GT.70
         THEN
            IQUAL=-1
            RETURN
         CIF
         ISORT1(NHWIR)=NHWIR
         ISORT2(1,NHWIR)=IPCO
         ISORT2(3,NHWIR)=0
         IW0=IWRK(IPCO)
         LFL=0
         LFL1=0
         LFL2=0
         WHILE IPCO.LE.IPCO9
            IW9=IWRK(IPCO)
            IF IW9.EQ.IW0
            THEN
N        HIT ON THE SAME WIRE
               NHALL=NHALL+1
               IF NHALL.GT.90
               THEN
                  IQUAL=-2
                  RETURN
               CIF
               IF(ISORT2(3,NHWIR).EQ.0) ISORT2(2,NHWIR)=NHALL
               ISORT2(3,NHWIR)=ISORT2(3,NHWIR)+1
C
               KSORT3(NHALL)= NHALL
               LZGOOD=HWRK(2*IPCO+1)
               IF LZGOOD.NE.0
               THEN
                  ISORT3(NHALL)=-1
                  KSZSRT(NHALL,1)= 100000
               ELSE
                  ISORT3(NHALL)= 1
                  IF LFL2.EQ.0
                  THEN
                     KSZSRT(NHALL,1)= WRK(IPCO+6)
                     KSZSRT(NHALL,2)= WRK(IPCO+5)
                     LFL2=1
                  ELSE
                     KSZSRT(NHALL,1)= 100000
                  CIF
                  LFL=1
                  IF ABS(WRK(IPCO+5)-PAR1*WRK(IPCO+6)-PAR2).LT.RESCUT
                  THEN
                     IF LFL1.EQ.0
                     THEN
                        LFL1=1
                        NHPOTT=NHPOTT+1
                     CIF
                  CIF
               CIF
C
               IPCO=IPCO+LHIT
            ELSE
               XWHILE
            CIF
         CWHILE
         IF LFL.EQ.1
         THEN
            NHPOT=NHPOT+1
         ELSE
            ISORT3(ISORT2(2,NHWIR))=-2
         CIF
      UNTIL IPCO.GT.IPCO9
C-----------------------------------------------------------------------
C
C IF LESS THAN 2 WIRES WITH GOOD Z MEASUREMENT, NOTHING DONE
      IF NHPOT.LT.2
      THEN
         IQUAL=-3
         RETURN
      CIF
C-----------------------------------------------------------------------
      KFLIP=2
C
      NHFIT=NHPOTT
      LFOUND=-1
      IF NHPOTT.LT.6.OR.NHPOTT.LT.NHPOT*.75
      THEN
C TRY TO FIND BETTER START VALUES
         PERFORM STVSEA
      CIF
C-----------------------------------------------------------------------
C
      INDMAX=NHFIT/4+1
      IF(INDMAX.GT.13) INDMAX=13
      INDFIT=0
      WHILE INDFIT.LT.INDMAX
         INDFIT=INDFIT+1
N    LINEAR FIT
         PERFORM LINFIT
         IF LNOCON.EQ.1
         THEN
C NO CONVERGENCE AS INDICATED BY LOSS OF TOO MANY HITS
            IQUAL=-4
            RETURN
         CIF
         IF(SIG.LT.SIGMIN) XWHILE
         IF INDFIT.GE.2
         THEN
            PERFORM LLSTOP
            IF LSTOP.EQ.1
            THEN
C      PREVIOUS FIT ACCEPTED, RESTORE ITS RESULTS
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
               XWHILE
            CIF
         CIF
         IF(INDFIT.EQ.INDMAX.OR.NHFIT.EQ.2) XWHILE
N      SAVE FIT RESULTS
         NHFTLS=NHFIT
         PAR1LS=PAR1
         PAR2LS=PAR2
         SIGLST=SIG
         S0LS=S0
         S1LS=S1
         S2LS=S2
         S3LS=S3
         S4LS=S4
N      HIT CLEANING
         PERFORM HITCLN
      CWHILE
C
C
N     SET UP PATR-BANK
      PERFORM FITBNK
      IQUAL=1
      RETURN
C=======================================================================
C
N     *************************
N     *      L I N F I T      *
N     *************************
C
C
N      LINEAR FIT
      PROC LINFIT
C
      LNOCON=0
N     GET EQUATIONS
      KFLIP=3-KFLIP
      NHF1=0
      S0 = 0.
      S1 = 0.
      S2 = 0.
      S3 = 0.
      S4 = 0.
      FOR IHWIR=1,NHWIR
         IH=ISORT2(2,IHWIR)
         NNH=ISORT2(3,IHWIR)
         FOR JNH=1,NNH
            ISORT4(KFLIP,IH+JNH-1)=0
         CFOR
         IF ISORT3(IH).EQ.1 .OR. ISORT3(IH).EQ.-1.AND.NNH.GT.1
         THEN
            RESMIN=10000.
            FOR JNH=1,NNH
               JH=IH+JNH-1
               IF ISORT3(JH).EQ.1
               THEN
                  IPCO=ISORT2(1,IHWIR)+(JNH-1)*LHIT
                  SA = WRK(IPCO+6)
                  ZA = WRK(IPCO+5)
                  WA = WRK(IPCO+7)
                  DZRESA=ZA-PAR1*SA-PAR2
                  DF0=ABS(DZRESA)
                  IF DF0.LT.RESMIN
                  THEN
                     RESMIN=DF0
                     S=SA
                     W=WA
                     DZRES=DZRESA
                     JHUSE=JH
                  CIF
               CIF
            CFOR
            IF RESMIN.LT.RESCUT
            THEN
               NHF1=NHF1+1
               S0=S0+W
               S1=S1+S*W
               S2=S2+S**2*W
               S3=S3+DZRES*W
               S4=S4+DZRES*S*W
               ISORT4(KFLIP,JHUSE)=1
            ELSE
               ISORT3(IH)=-2
            CIF
         CIF
      CFOR
      IF NHF1.LT.2 .OR. S2.LT.1.
      THEN
         LNOCON=1
      ELSE
         NHFIT=NHF1
C
N        SOLVE EQUATIONS
         F1 = 1. / S2
         XX12 = S1*F1
         YY1  = S4*F1
         PARR2=(S3-S1*YY1)/(S0-S1*XX12)
         PAR1=YY1-PARR2*XX12+PAR1
         PAR2=PAR2+PARR2
C
N     CALC. CHISQ + SOLVE L/R AMBIGUITY
         CHISQ = 0.
         NHF1=0
         FOR IHWIR=1,NHWIR
            IRESHT(IHWIR)=-1
            IH=ISORT2(2,IHWIR)
            NNH=ISORT2(3,IHWIR)
            IF ISORT3(IH).GE.0 .OR. ISORT3(IH).EQ.-1.AND.NNH.GT.1
            THEN
               RESMIN=10000.
               FOR JNH=1,NNH
                  JH=IH+JNH-1
                  IF ISORT3(JH).GE.0
                  THEN
                     IFLG=ISORT3(JH)
                     IPCO=ISORT2(1,IHWIR)+(JNH-1)*LHIT
                     SA = WRK(IPCO+6)
                     ZA = WRK(IPCO+5)
                     WA = WRK(IPCO+7)
                     DF0=ABS(ZA-PAR1*SA-PAR2)
                     IF DF0.LT.RESMIN
                     THEN
                        RESMIN=DF0
                        W=WA
                     CIF
                  CIF
               CFOR
               IF RESMIN.LT.8000.
               THEN
                  RESMIN=RESMIN*SQRT(W)
                  IRESHT(IHWIR)=RESMIN*1.E4
                  IF IFLG.EQ.1
                  THEN
                     CHISQ=CHISQ+RESMIN**2
                     NHF1=NHF1+1
                  CIF
               CIF
            CIF
         CFOR
         IF NHF1.LT.2
         THEN
            LNOCON=1
         ELSE
            IF NHF1.EQ.2
            THEN
               SIG=1.E-5
            ELSE
               SIG=CHISQ/(NHF1-2)
            CIF
         CIF
         NHFIT=NHF1
      CIF
      CPROC
C=======================================================================
      PROC HITCLN
C      LABEL HITS NOT TO BE USED IN THE NEXT ITRATION
C-------------------------------------------------------------
C
N       SORT HITS ACCORDING TO RESIDUALS
C  EXCLUDE THE INDFIT LARGEST RESIDUAL HITS,
C  RESTORE THE OTHERS (EXLUDED FOR EVER HITS NOT COUNTED)
C
         CALL SHELL9(IRESHT,ISORT1,NHWIR)
         KOMIT=0
         FOR J1=1,NHWIR
            IHWIR=ISORT1(NHWIR+1-J1)
            IPCO=ISORT2(1,IHWIR)
            NNH=ISORT2(3,IHWIR)
            IH=ISORT2(2,IHWIR)
            LFLG=0
            FOR JNH=1,NNH
               IHA=IH+JNH-1
               IQA=ISORT3(IHA)
               IF IQA.GT.-1
               THEN
                  IF LFLG.EQ.0
                  THEN
                     LFLG=1
                     KOMIT=KOMIT+1
                  CIF
                  IF KOMIT.LE.INDFIT
                  THEN
                     ISORT3(IHA)=0
                  ELSE
                     ISORT3(IHA)=1
                  CIF
               CIF
            CFOR
         CFOR
      CPROC
C=======================================================================
      PROC LLSTOP
         IF INDFIT.LE.6
         THEN
            INDCK=INDFIT-1
         ELSE
            INDCK=5
         CIF
         ICHCK=NCHECK(INDCK)
         WHILE SIGLST*SIGFAC.LT.RCHECK(ICHCK,1,INDCK)
            ICHCK=ICHCK-1
         CWHILE
         IF(ICHCK.LT.1) ICHCK=1
         IF SIG/SIGLST*STPFAC.GT.RCHECK(ICHCK,2,INDCK)
         THEN
            LSTOP=1
         ELSE
            LSTOP=0
         CIF
      CPROC
C=======================================================================
C
C
N     *************************
N     *      F I T B N K      *
N     *************************
C
C
N     SET UP FIT-BANK
      PROC FITBNK
C
      CTGTH=PAR1
      CSTH = 1./SQRT(CTGTH**2 + 1.)
      SNTH  = CSTH * CTGTH
C
C
C
N     COPY TRACK BANK
         IFREE=NDIWRK-100
         CALL MVCL(IWRK(IFREE),0,IDATA(IPTR+1),0,4*LDTR)
C
N     FILL FIT-BANK
         IP    = IFREE - 1
         IWRK(IP+ 2) = LAND(IWRK(IP+2),MASK4)
         IWRK(IP+ 2) = LOR(IWRK(IP+2),4096)
         WRK (IP+ 7) = PAR2
         A=SQRT(WRK(IP+8)**2+WRK(IP+9)**2)
         WRK (IP+ 8) = WRK (IP+ 8)/A*CSTH
         WRK (IP+ 9) = WRK (IP+ 9)/A*CSTH
         WRK (IP+10) = SNTH
C CALCULATE TRACK LENGTH IN R-PHI FROM FIRST TO LAST POINT ON TRACK
         CURVXY=WRK(IP+25)
         IF(ABS(CURVXY).LT.1.E-9) CURVXY = SIGN(1.E-9,CURVXY)
         UU=SQRT((WRK(IP+12)-WRK(IP+5))**2+(WRK(IP+13)-WRK(IP+6))**2)
         IF(ABS(CURVXY*UU).GT.1.E-5) UU=2.*SIN(.5*CURVXY*UU)/CURVXY
         WRK (IP+14) = PAR2+UU*PAR1
         A=SQRT(WRK(IP+15)**2+WRK(IP+16)**2)
         WRK (IP+15) = WRK (IP+15)/A*CSTH
         WRK (IP+16) = WRK (IP+16)/A*CSTH
         WRK (IP+17) = SNTH
         IWRK(IP+33) = NHFIT
         WRK (IP+32) = SQRT(SIG)
C FIT TYPE WILL BE 2: "HELIX FIT"
         IWRK(IP+29) = 2
         WRK (IP+30) = PAR1
C GET CLOSEST POINT (XP,YP) TO ORIGIN
         DDR0=DISTXY(ADATA(IPTR+5),ADATA(IPTR+6),ADATA(IPTR+8),
     +   ADATA(IPTR+9),1./CURVXY,0.,0.,XP,YP,FI)
C CALCULATE TRACK LENGTH ALONG CIRCLE FROM FIRST POINT TO (XP,YP)
         UU=SQRT((XP-WRK(IP+5))**2+(XP-WRK(IP+6))**2)
         IF(ABS(CURVXY*UU).GT.1.E-5) UU=2.*SIN(.5*CURVXY*UU)/CURVXY
         WRK (IP+31) = PAR2-PAR1*UU
C
         IF LDTR.GE.59 .AND. NHFIT.GE.4 .AND. LNOCON.EQ.0
         THEN
C CALCULATE COVARIANCE MATRIX
            DET=S0*S2-S1**2
            FACT=SIG/DET
            WRK(IP+56)=SIG*(NHFIT-2)/20.**2
            WRK(IP+57)=(S2+2.*UU*S1+UU**2*S0)*FACT
            WRK(IP+58)=-(UU*S0+S1)*FACT
            WRK(IP+59)=S0*FACT
         CIF
C
C MARK HITS USED IN THE FIT
         LFL=0
         FOR IHWIR=1,NHWIR
            IH=ISORT2(2,IHWIR)
            NNH=ISORT2(3,IHWIR)
            FOR JNH=1,NNH
               IPCO=ISORT2(1,IHWIR)+(JNH-1)*LHIT
               HWRK(2*IPCO+3)=ISORT4(KFLIP,IH+JNH-1)
               IF(HWRK(2*IPCO-1).GT.100.AND.HWRK(2*IPCO+3).EQ.1) LFL=1
            CFOR
         CFOR
         IF(LFL.EQ.1) IWRK(IP+ 2) = LOR(IWRK(IP+2),16384)
C
N     PUT RESULT INTO PATR-BANK
         CALL MVCL(IDATA(IPTR+1),0,IWRK(IFREE),0,4*LDTR)
C
C CREATE Z-S BANK 'ZSPD'
C
         IF IOPT.GT.8
         THEN
            CALL CCRE(NPZSPD,'ZSPD',ITRK,5*NHALL+1,IERR)
            IF IERR.EQ.0
            THEN
               CALL BSAW(1,'ZSPD')
               NPZSP1=NPZSPD+1
               IDATA(NPZSP1)=5
               FOR IHWIR=1,NHWIR
                  IH=ISORT2(2,IHWIR)
                  NNH=ISORT2(3,IHWIR)
                  FOR JNH=1,NNH
                     IPCO=ISORT2(1,IHWIR)+(JNH-1)*LHIT
                     IFL=ISORT4(KFLIP,IH+JNH-1)
                     IF IFL.EQ.1
                     THEN
                        IFL=0
                     ELSE
                        IFL=16
                     CIF
                     IDATA(NPZSP1+1)=HWRK(2*IPCO+4)
                     ADATA(NPZSP1+2)=WRK(IPCO+6)
                     ADATA(NPZSP1+3)=WRK(IPCO+5)
                     IDATA(NPZSP1+4)=IFL
                     ADATA(NPZSP1+5)=WRK(IPCO+7)
                     NPZSP1=NPZSP1+5
                  CFOR
               CFOR
            CIF
         CIF
      CPROC
C=======================================================================
      PROC STVSEA
C  SEARCH FOR STARTING VALUES
C
C  ORDER ACCORDING TO S
         CALL SHELL9(KSZSRT(1,1),KSORT3,NHALL)
         NH9=NHALL
         WHILE KSZSRT(KSORT3(NH9),1).GT.99999
            NH9=NH9-1
         CWHILE
         NH99=NH9
         NH1=1
         LFOUND=0
         NHMAX=0
         CI2MAX=1.E10
         WHILE NH9.GT.NH1
            FOR KK=1,3
               IF KK.EQ.1
               THEN
                  JH1=NH1
                  JH9=NH9
               ELSE
                  IF KK.EQ.2
                  THEN
                     JH9=JH9-1
                  ELSE
                     JH9=JH9+1
                     JH1=JH1+1
                  CIF
               CIF
               IF JH9.GT.JH1
               THEN
                  AS1=KSZSRT(KSORT3(JH1),1)
                  AS9=KSZSRT(KSORT3(JH9),1)
                  IF(AS9.LT.AS1+5.) XWHILE
                  AZ1=KSZSRT(KSORT3(JH1),2)
                  AZ9=KSZSRT(KSORT3(JH9),2)
                  PAR1=(AZ9-AZ1)/(AS9-AS1)
                  PAR2=AZ1-PAR1*AS1
                  NHFIT=0
                  CI2=0.
                  FOR I=1,NH99
                     DZ=ABS(KSZSRT(KSORT3(I),2)-PAR1*KSZSRT(KSORT3(I),1)
     +               -PAR2)
                     IF DZ.LT.RESCUT
                     THEN
                        NHFIT=NHFIT+1
                        CI2=CI2+DZ**2
                     CIF
                  CFOR
                  IF NHFIT.GT.NHPOT*.75
                  THEN
                     LFOUND=1
                     XWHILE
                  CIF
                  IF NHFIT.GT.NHMAX .OR.NHFIT.EQ.NHMAX.AND.CI2.LT.CI2MAX
                  THEN
                     NHMAX=NHFIT
                     P1MAX=PAR1
                     P2MAX=PAR2
                     CI2MAX=CI2
                  CIF
               CIF
            CFOR
            NH1=NH1+1
            NH9=NH9-1
         CWHILE
         IF LFOUND.EQ.0 .AND. NHMAX.GT.0
         THEN
            NHFIT=NHMAX
            PAR1=P1MAX
            PAR2=P2MAX
         CIF
      CPROC
      END
C   09/06/83 803181311  MEMBER NAME  ZSRFT12  (JADEGS)      SHELTRAN
C   09/06/83 803181242  MEMBER NAME  ZSRFT1   (S)           SHELTRAN
      SUBROUTINE ZSRFT1(IPTR,LDTR,IPCO0,IPRES,LHIT,IQUAL,IOPT)
C
C        S-Z ("HELIX") REFIT OF A SINGLE TRACK
C
C    18.3.88   PROPER RUN NUMBER HANDLING USING LDATYP      E E
C    22.2.88   MVC CHANGED TO MVCL (256 BYTES NOT ENOUGH!)  J.H./J.O.
C    TEST VERSION 1.
C
C                                J. SPITZER  12/4/87
C
C    COVARIANCE MATRIX FOR FIT PARAMETERS IF AREA (LDTR) LARGE ENOUGH
C
      IMPLICIT INTEGER*2 (H)
C
#include "cdata.for"
#include "calibr.for"
C
      COMMON/CWORK/NDIWRK,WRK(200)
      DIMENSION IWRK(200),HWRK(400)
      EQUIVALENCE (WRK(1),IWRK(1),HWRK(1))
C
C
      INTEGER DATE(5), IDAY /0/
C-----------------------------
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
C
C
      REAL RESCUT/600./
      DIMENSION ISORT1(71),ISORT2(3,71),IRESHT(71),ISORT3(91)
     +,ISORT4(2,91),KSORT3(91),KSZSRT(91,2)
      DATA IQHEAD/0/,MASK4/ZFFFFCFFF/
C
C
C
N     INITIALIZATION
      DATA LBINIT /0/
      IF LBINIT .EQ. 0
      THEN
         LBINIT = 1
C
         IQHEAD = IBLN('HEAD')
C
         CALL DAY2(DATE)
         IDAY = DATE(1)*1000 + DATE(2)
C
         WRITE(6,137)
 137     FORMAT(/,' *** ZSRFT1 ***  (J.SPITZER) VERSION OF 18/3/88'/
     +   ' COVARIANCE MATRIX IS PROVIDED',/,
     +   ' IF THE TRACK AREA LONG ENOUGH, THE NUMBER',/,
     +  ' OF HITS USED IN THE FIT IS AT LEAST 4.',
     +   /)
      CIF
C
C
N     GET RUN #
      IPHEAD = IDATA(IQHEAD)*2
      NRUN = HDATA(IPHEAD+10)
      NEV  = HDATA(IPHEAD+11)
C
N     TRACK #
      ITRK = IDATA(IPTR+1)
C
C=======================================================================
      IF LDATYP(DUMMY) .EQ. 1
      THEN
         SRESO=24.
         STPFAC=.85
      ELSE
         SRESO=32.
         STPFAC=.92
      CIF
      SIGMIN=(SRESO/1.6)**2
      SIGFAC=(.14/30.)**2
C
C
C-----------------------------------------------------------------------
C
C     GYMNASTICS FOR PRIVATE HIT QUALIFICATION
C     AND FOR HANDLING MORE HITS ON SAME WIRE
C
C  STARTING VALUES OF FIT PARAMETERS Z=P1*S+P2
C
      PAR1=ADATA(IPTR+30)
      PAR2=ADATA(IPTR+7)
C
      NHALL=0
      NHWIR=0
      NHPOT=0
      NHPOTT=0
      IPCO=IPCO0
      IPCO9=IPRES-LHIT
      REPEAT
         NHWIR=NHWIR+1
         IF NHWIR.GT.70
         THEN
            IQUAL=-1
            RETURN
         CIF
         ISORT1(NHWIR)=NHWIR
         ISORT2(1,NHWIR)=IPCO
         ISORT2(3,NHWIR)=0
         IW0=IWRK(IPCO)
         LFL=0
         LFL1=0
         LFL2=0
         WHILE IPCO.LE.IPCO9
            IW9=IWRK(IPCO)
            IF IW9.EQ.IW0
            THEN
N        HIT ON THE SAME WIRE
               NHALL=NHALL+1
               IF NHALL.GT.90
               THEN
                  IQUAL=-2
                  RETURN
               CIF
               IF(ISORT2(3,NHWIR).EQ.0) ISORT2(2,NHWIR)=NHALL
               ISORT2(3,NHWIR)=ISORT2(3,NHWIR)+1
C
               KSORT3(NHALL)= NHALL
               LZGOOD=HWRK(2*IPCO+1)
               IF LZGOOD.NE.0
               THEN
                  ISORT3(NHALL)=-1
                  KSZSRT(NHALL,1)= 100000
               ELSE
                  ISORT3(NHALL)= 1
                  IF LFL2.EQ.0
                  THEN
                     KSZSRT(NHALL,1)= WRK(IPCO+6)
                     KSZSRT(NHALL,2)= WRK(IPCO+5)
                     LFL2=1
                  ELSE
                     KSZSRT(NHALL,1)= 100000
                  CIF
                  LFL=1
                  IF ABS(WRK(IPCO+5)-PAR1*WRK(IPCO+6)-PAR2).LT.RESCUT
                  THEN
                     IF LFL1.EQ.0
                     THEN
                        LFL1=1
                        NHPOTT=NHPOTT+1
                     CIF
                  CIF
               CIF
C
               IPCO=IPCO+LHIT
            ELSE
               XWHILE
            CIF
         CWHILE
         IF LFL.EQ.1
         THEN
            NHPOT=NHPOT+1
         ELSE
            ISORT3(ISORT2(2,NHWIR))=-2
         CIF
      UNTIL IPCO.GT.IPCO9
C-----------------------------------------------------------------------
C
C IF LESS THAN 2 WIRES WITH GOOD Z MEASUREMENT, NOTHING DONE
      IF NHPOT.LT.2
      THEN
         IQUAL=-3
         RETURN
      CIF
C-----------------------------------------------------------------------
      KFLIP=2
C
      NHFIT=NHPOTT
      LFOUND=-1
      IF NHPOTT.LT.6.OR.NHPOTT.LT.NHPOT*.75
      THEN
C TRY TO FIND BETTER START VALUES
         PERFORM STVSEA
      CIF
C-----------------------------------------------------------------------
C
      INDMAX=NHFIT/4+1
      IF(INDMAX.GT.13) INDMAX=13
      INDFIT=0
      WHILE INDFIT.LT.INDMAX
         INDFIT=INDFIT+1
N    LINEAR FIT
         PERFORM LINFIT
         IF LNOCON.EQ.1
         THEN
C NO CONVERGENCE AS INDICATED BY LOSS OF TOO MANY HITS
            IQUAL=-4
            RETURN
         CIF
         IF(SIG.LT.SIGMIN) XWHILE
         IF INDFIT.GE.2
         THEN
            PERFORM LLSTOP
            IF LSTOP.EQ.1
            THEN
C      PREVIOUS FIT ACCEPTED, RESTORE ITS RESULTS
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
               XWHILE
            CIF
         CIF
         IF(INDFIT.EQ.INDMAX.OR.NHFIT.EQ.2) XWHILE
N      SAVE FIT RESULTS
         NHFTLS=NHFIT
         PAR1LS=PAR1
         PAR2LS=PAR2
         SIGLST=SIG
         S0LS=S0
         S1LS=S1
         S2LS=S2
         S3LS=S3
         S4LS=S4
N      HIT CLEANING
         PERFORM HITCLN
      CWHILE
C
C
N     SET UP PATR-BANK
      PERFORM FITBNK
      IQUAL=1
      RETURN
C=======================================================================
C
N     *************************
N     *      L I N F I T      *
N     *************************
C
C
N      LINEAR FIT
      PROC LINFIT
C
      LNOCON=0
N     GET EQUATIONS
      KFLIP=3-KFLIP
      NHF1=0
      S0 = 0.
      S1 = 0.
      S2 = 0.
      S3 = 0.
      S4 = 0.
      FOR IHWIR=1,NHWIR
         IH=ISORT2(2,IHWIR)
         NNH=ISORT2(3,IHWIR)
         FOR JNH=1,NNH
            ISORT4(KFLIP,IH+JNH-1)=0
         CFOR
         IF ISORT3(IH).EQ.1 .OR. ISORT3(IH).EQ.-1.AND.NNH.GT.1
         THEN
            RESMIN=10000.
            FOR JNH=1,NNH
               JH=IH+JNH-1
               IF ISORT3(JH).EQ.1
               THEN
                  IPCO=ISORT2(1,IHWIR)+(JNH-1)*LHIT
                  SA = WRK(IPCO+6)
                  ZA = WRK(IPCO+5)
                  WA = WRK(IPCO+7)
                  DZRESA=ZA-PAR1*SA-PAR2
                  DF0=ABS(DZRESA)
                  IF DF0.LT.RESMIN
                  THEN
                     RESMIN=DF0
                     S=SA
                     W=WA
                     DZRES=DZRESA
                     JHUSE=JH
                  CIF
               CIF
            CFOR
            IF RESMIN.LT.RESCUT
            THEN
               NHF1=NHF1+1
               S0=S0+W
               S1=S1+S*W
               S2=S2+S**2*W
               S3=S3+DZRES*W
               S4=S4+DZRES*S*W
               ISORT4(KFLIP,JHUSE)=1
            ELSE
               ISORT3(IH)=-2
            CIF
         CIF
      CFOR
      IF NHF1.LT.2 .OR. S2.LT.1.
      THEN
         LNOCON=1
      ELSE
         NHFIT=NHF1
C
N        SOLVE EQUATIONS
         F1 = 1. / S2
         XX12 = S1*F1
         YY1  = S4*F1
         PARR2=(S3-S1*YY1)/(S0-S1*XX12)
         PAR1=YY1-PARR2*XX12+PAR1
         PAR2=PAR2+PARR2
C
N     CALC. CHISQ + SOLVE L/R AMBIGUITY
         CHISQ = 0.
         NHF1=0
         FOR IHWIR=1,NHWIR
            IRESHT(IHWIR)=-1
            IH=ISORT2(2,IHWIR)
            NNH=ISORT2(3,IHWIR)
            IF ISORT3(IH).GE.0 .OR. ISORT3(IH).EQ.-1.AND.NNH.GT.1
            THEN
               RESMIN=10000.
               FOR JNH=1,NNH
                  JH=IH+JNH-1
                  IF ISORT3(JH).GE.0
                  THEN
                     IFLG=ISORT3(JH)
                     IPCO=ISORT2(1,IHWIR)+(JNH-1)*LHIT
                     SA = WRK(IPCO+6)
                     ZA = WRK(IPCO+5)
                     WA = WRK(IPCO+7)
                     DF0=ABS(ZA-PAR1*SA-PAR2)
                     IF DF0.LT.RESMIN
                     THEN
                        RESMIN=DF0
                        W=WA
                     CIF
                  CIF
               CFOR
               IF RESMIN.LT.8000.
               THEN
                  RESMIN=RESMIN*SQRT(W)
                  IRESHT(IHWIR)=RESMIN*1.E4
                  IF IFLG.EQ.1
                  THEN
                     CHISQ=CHISQ+RESMIN**2
                     NHF1=NHF1+1
                  CIF
               CIF
            CIF
         CFOR
         IF NHF1.LT.2
         THEN
            LNOCON=1
         ELSE
            IF NHF1.EQ.2
            THEN
               SIG=1.E-5
            ELSE
               SIG=CHISQ/(NHF1-2)
            CIF
         CIF
         NHFIT=NHF1
      CIF
      CPROC
C=======================================================================
      PROC HITCLN
C      LABEL HITS NOT TO BE USED IN THE NEXT ITRATION
C-------------------------------------------------------------
C
N       SORT HITS ACCORDING TO RESIDUALS
C  EXCLUDE THE INDFIT LARGEST RESIDUAL HITS,
C  RESTORE THE OTHERS (EXLUDED FOR EVER HITS NOT COUNTED)
C
         CALL SHELL9(IRESHT,ISORT1,NHWIR)
         KOMIT=0
         FOR J1=1,NHWIR
            IHWIR=ISORT1(NHWIR+1-J1)
            IPCO=ISORT2(1,IHWIR)
            NNH=ISORT2(3,IHWIR)
            IH=ISORT2(2,IHWIR)
            LFLG=0
            FOR JNH=1,NNH
               IHA=IH+JNH-1
               IQA=ISORT3(IHA)
               IF IQA.GT.-1
               THEN
                  IF LFLG.EQ.0
                  THEN
                     LFLG=1
                     KOMIT=KOMIT+1
                  CIF
                  IF KOMIT.LE.INDFIT
                  THEN
                     ISORT3(IHA)=0
                  ELSE
                     ISORT3(IHA)=1
                  CIF
               CIF
            CFOR
         CFOR
      CPROC
C=======================================================================
      PROC LLSTOP
         IF INDFIT.LE.6
         THEN
            INDCK=INDFIT-1
         ELSE
            INDCK=5
         CIF
         ICHCK=NCHECK(INDCK)
         WHILE SIGLST*SIGFAC.LT.RCHECK(ICHCK,1,INDCK)
            ICHCK=ICHCK-1
         CWHILE
         IF(ICHCK.LT.1) ICHCK=1
         IF SIG/SIGLST*STPFAC.GT.RCHECK(ICHCK,2,INDCK)
         THEN
            LSTOP=1
         ELSE
            LSTOP=0
         CIF
      CPROC
C=======================================================================
C
C
N     *************************
N     *      F I T B N K      *
N     *************************
C
C
N     SET UP FIT-BANK
      PROC FITBNK
C
      CTGTH=PAR1
      CSTH = 1./SQRT(CTGTH**2 + 1.)
      SNTH  = CSTH * CTGTH
C
C
C
N     COPY TRACK BANK
         IFREE=NDIWRK-100
         CALL MVCL(IWRK(IFREE),0,IDATA(IPTR+1),0,4*LDTR)
C
N     FILL FIT-BANK
         IP    = IFREE - 1
         IWRK(IP+ 2) = LAND(IWRK(IP+2),MASK4)
         IWRK(IP+ 2) = LOR(IWRK(IP+2),4096)
         WRK (IP+ 7) = PAR2
         A=SQRT(WRK(IP+8)**2+WRK(IP+9)**2)
         WRK (IP+ 8) = WRK (IP+ 8)/A*CSTH
         WRK (IP+ 9) = WRK (IP+ 9)/A*CSTH
         WRK (IP+10) = SNTH
C CALCULATE TRACK LENGTH IN R-PHI FROM FIRST TO LAST POINT ON TRACK
         CURVXY=WRK(IP+25)
         IF(ABS(CURVXY).LT.1.E-9) CURVXY = SIGN(1.E-9,CURVXY)
         UU=SQRT((WRK(IP+12)-WRK(IP+5))**2+(WRK(IP+13)-WRK(IP+6))**2)
         IF(ABS(CURVXY*UU).GT.1.E-5) UU=2.*SIN(.5*CURVXY*UU)/CURVXY
         WRK (IP+14) = PAR2+UU*PAR1
         A=SQRT(WRK(IP+15)**2+WRK(IP+16)**2)
         WRK (IP+15) = WRK (IP+15)/A*CSTH
         WRK (IP+16) = WRK (IP+16)/A*CSTH
         WRK (IP+17) = SNTH
         IWRK(IP+33) = NHFIT
         WRK (IP+32) = SQRT(SIG)
C FIT TYPE WILL BE 2: "HELIX FIT"
         IWRK(IP+29) = 2
         WRK (IP+30) = PAR1
C GET CLOSEST POINT (XP,YP) TO ORIGIN
         DDR0=DISTXY(ADATA(IPTR+5),ADATA(IPTR+6),ADATA(IPTR+8),
     +   ADATA(IPTR+9),1./CURVXY,0.,0.,XP,YP,FI)
C CALCULATE TRACK LENGTH ALONG CIRCLE FROM FIRST POINT TO (XP,YP)
         UU=SQRT((XP-WRK(IP+5))**2+(XP-WRK(IP+6))**2)
         IF(ABS(CURVXY*UU).GT.1.E-5) UU=2.*SIN(.5*CURVXY*UU)/CURVXY
         WRK (IP+31) = PAR2-PAR1*UU
C
         IF LDTR.GE.59 .AND. NHFIT.GE.4 .AND. LNOCON.EQ.0
         THEN
C CALCULATE COVARIANCE MATRIX
            DET=S0*S2-S1**2
            FACT=SIG/DET
            WRK(IP+56)=SIG*(NHFIT-2)/20.**2
            WRK(IP+57)=(S2+2.*UU*S1+UU**2*S0)*FACT
            WRK(IP+58)=-(UU*S0+S1)*FACT
            WRK(IP+59)=S0*FACT
         CIF
C
C MARK HITS USED IN THE FIT
         LFL=0
         FOR IHWIR=1,NHWIR
            IH=ISORT2(2,IHWIR)
            NNH=ISORT2(3,IHWIR)
            FOR JNH=1,NNH
               IPCO=ISORT2(1,IHWIR)+(JNH-1)*LHIT
               HWRK(2*IPCO+3)=ISORT4(KFLIP,IH+JNH-1)
               IF(HWRK(2*IPCO-1).GT.100.AND.HWRK(2*IPCO+3).EQ.1) LFL=1
            CFOR
         CFOR
         IF(LFL.EQ.1) IWRK(IP+ 2) = LOR(IWRK(IP+2),16384)
C
N     PUT RESULT INTO PATR-BANK
         CALL MVCL(IDATA(IPTR+1),0,IWRK(IFREE),0,4*LDTR)
C
C CREATE Z-S BANK 'ZSPD'
C
         IF IOPT.GT.8
         THEN
            CALL CCRE(NPZSPD,'ZSPD',ITRK,5*NHALL+1,IERR)
            IF IERR.EQ.0
            THEN
               CALL BSAW(1,'ZSPD')
               NPZSP1=NPZSPD+1
               IDATA(NPZSP1)=5
               FOR IHWIR=1,NHWIR
                  IH=ISORT2(2,IHWIR)
                  NNH=ISORT2(3,IHWIR)
                  FOR JNH=1,NNH
                     IPCO=ISORT2(1,IHWIR)+(JNH-1)*LHIT
                     IFL=ISORT4(KFLIP,IH+JNH-1)
                     IF IFL.EQ.1
                     THEN
                        IFL=0
                     ELSE
                        IFL=16
                     CIF
                     IDATA(NPZSP1+1)=HWRK(2*IPCO+4)
                     ADATA(NPZSP1+2)=WRK(IPCO+6)
                     ADATA(NPZSP1+3)=WRK(IPCO+5)
                     IDATA(NPZSP1+4)=IFL
                     ADATA(NPZSP1+5)=WRK(IPCO+7)
                     NPZSP1=NPZSP1+5
                  CFOR
               CFOR
            CIF
         CIF
      CPROC
C=======================================================================
      PROC STVSEA
C  SEARCH FOR STARTING VALUES
C
C  ORDER ACCORDING TO S
         CALL SHELL9(KSZSRT(1,1),KSORT3,NHALL)
         NH9=NHALL
         WHILE KSZSRT(KSORT3(NH9),1).GT.99999
            NH9=NH9-1
         CWHILE
         NH99=NH9
         NH1=1
         LFOUND=0
         NHMAX=0
         CI2MAX=1.E10
         WHILE NH9.GT.NH1
            FOR KK=1,3
               IF KK.EQ.1
               THEN
                  JH1=NH1
                  JH9=NH9
               ELSE
                  IF KK.EQ.2
                  THEN
                     JH9=JH9-1
                  ELSE
                     JH9=JH9+1
                     JH1=JH1+1
                  CIF
               CIF
               IF JH9.GT.JH1
               THEN
                  AS1=KSZSRT(KSORT3(JH1),1)
                  AS9=KSZSRT(KSORT3(JH9),1)
                  IF(AS9.LT.AS1+5.) XWHILE
                  AZ1=KSZSRT(KSORT3(JH1),2)
                  AZ9=KSZSRT(KSORT3(JH9),2)
                  PAR1=(AZ9-AZ1)/(AS9-AS1)
                  PAR2=AZ1-PAR1*AS1
                  NHFIT=0
                  CI2=0.
                  FOR I=1,NH99
                     DZ=ABS(KSZSRT(KSORT3(I),2)-PAR1*KSZSRT(KSORT3(I),1)
     +               -PAR2)
                     IF DZ.LT.RESCUT
                     THEN
                        NHFIT=NHFIT+1
                        CI2=CI2+DZ**2
                     CIF
                  CFOR
                  IF NHFIT.GT.NHPOT*.75
                  THEN
                     LFOUND=1
                     XWHILE
                  CIF
                  IF NHFIT.GT.NHMAX .OR.NHFIT.EQ.NHMAX.AND.CI2.LT.CI2MAX
                  THEN
                     NHMAX=NHFIT
                     P1MAX=PAR1
                     P2MAX=PAR2
                     CI2MAX=CI2
                  CIF
               CIF
            CFOR
            NH1=NH1+1
            NH9=NH9-1
         CWHILE
         IF LFOUND.EQ.0 .AND. NHMAX.GT.0
         THEN
            NHFIT=NHMAX
            PAR1=P1MAX
            PAR2=P2MAX
         CIF
      CPROC
      END
C   09/06/83 805141629  MEMBER NAME  ZSRFT13  (JADEGS)      SHELTRAN
C   09/06/83 803181242  MEMBER NAME  ZSRFT1   (S)           SHELTRAN
      SUBROUTINE ZSRFT1(IPTR,LDTR,IPCO0,IPRES,LHIT,IQUAL,IOPT)
C
C        S-Z ("HELIX") REFIT OF A SINGLE TRACK
C
C    18.3.88   PROPER RUN NUMBER HANDLING USING LDATYP      E E
C    22.2.88   MVC CHANGED TO MVCL (256 BYTES NOT ENOUGH!)  J.H./J.O.
C    TEST VERSION 1.
C    13.5.88 (FRIDAY!) SEVERE BUG CORRECTED IN TWO PLACES J.H./J.O.
C            ARSIN INSTEAD OF SIN
C            IN ADDITION MISPRINT XP CHANGED INTO YP
C
C                                J. SPITZER  12/4/87
C
C    COVARIANCE MATRIX FOR FIT PARAMETERS IF AREA (LDTR) LARGE ENOUGH
C
      IMPLICIT INTEGER*2 (H)
C
#include "cdata.for"
#include "calibr.for"
C
      COMMON/CWORK/NDIWRK,WRK(200)
      DIMENSION IWRK(200),HWRK(400)
      EQUIVALENCE (WRK(1),IWRK(1),HWRK(1))
C
C
      INTEGER DATE(5), IDAY /0/
C-----------------------------
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
C
C
      REAL RESCUT/600./
      DIMENSION ISORT1(71),ISORT2(3,71),IRESHT(71),ISORT3(91)
     +,ISORT4(2,91),KSORT3(91),KSZSRT(91,2)
      DATA IQHEAD/0/,MASK4/ZFFFFCFFF/
C
C
C
N     INITIALIZATION
      DATA LBINIT /0/
      IF LBINIT .EQ. 0
      THEN
         LBINIT = 1
C
         IQHEAD = IBLN('HEAD')
C
         CALL DAY2(DATE)
         IDAY = DATE(1)*1000 + DATE(2)
C
         WRITE(6,137)
 137     FORMAT(/,' *** ZSRFT1 ***  (J.SPITZER) VERSION OF 13/5/88'/
     +   ' SEVERE BUGS CORRECTED, WHICH AFFECT LOW-MOMENTUM TRACKS',/,
     +   ' AS WELL AS CONVERSIONS, K0, LAMBDAS, ETC...',/,
     +   ' COVARIANCE MATRIX IS PROVIDED',/,
     +   ' IF THE TRACK AREA LONG ENOUGH, THE NUMBER',/,
     +  ' OF HITS USED IN THE FIT IS AT LEAST 4.',
     +   /)
      CIF
C
C
N     GET RUN #
      IPHEAD = IDATA(IQHEAD)*2
      NRUN = HDATA(IPHEAD+10)
      NEV  = HDATA(IPHEAD+11)
C
N     TRACK #
      ITRK = IDATA(IPTR+1)
C
C=======================================================================
      IF LDATYP(DUMMY) .EQ. 1
      THEN
         SRESO=24.
         STPFAC=.85
      ELSE
         SRESO=32.
         STPFAC=.92
      CIF
      SIGMIN=(SRESO/1.6)**2
      SIGFAC=(.14/30.)**2
C
C
C-----------------------------------------------------------------------
C
C     GYMNASTICS FOR PRIVATE HIT QUALIFICATION
C     AND FOR HANDLING MORE HITS ON SAME WIRE
C
C  STARTING VALUES OF FIT PARAMETERS Z=P1*S+P2
C
      PAR1=ADATA(IPTR+30)
      PAR2=ADATA(IPTR+7)
C
      NHALL=0
      NHWIR=0
      NHPOT=0
      NHPOTT=0
      IPCO=IPCO0
      IPCO9=IPRES-LHIT
      REPEAT
         NHWIR=NHWIR+1
         IF NHWIR.GT.70
         THEN
            IQUAL=-1
            RETURN
         CIF
         ISORT1(NHWIR)=NHWIR
         ISORT2(1,NHWIR)=IPCO
         ISORT2(3,NHWIR)=0
         IW0=IWRK(IPCO)
         LFL=0
         LFL1=0
         LFL2=0
         WHILE IPCO.LE.IPCO9
            IW9=IWRK(IPCO)
            IF IW9.EQ.IW0
            THEN
N        HIT ON THE SAME WIRE
               NHALL=NHALL+1
               IF NHALL.GT.90
               THEN
                  IQUAL=-2
                  RETURN
               CIF
               IF(ISORT2(3,NHWIR).EQ.0) ISORT2(2,NHWIR)=NHALL
               ISORT2(3,NHWIR)=ISORT2(3,NHWIR)+1
C
               KSORT3(NHALL)= NHALL
               LZGOOD=HWRK(2*IPCO+1)
               IF LZGOOD.NE.0
               THEN
                  ISORT3(NHALL)=-1
                  KSZSRT(NHALL,1)= 100000
               ELSE
                  ISORT3(NHALL)= 1
                  IF LFL2.EQ.0
                  THEN
                     KSZSRT(NHALL,1)= WRK(IPCO+6)
                     KSZSRT(NHALL,2)= WRK(IPCO+5)
                     LFL2=1
                  ELSE
                     KSZSRT(NHALL,1)= 100000
                  CIF
                  LFL=1
                  IF ABS(WRK(IPCO+5)-PAR1*WRK(IPCO+6)-PAR2).LT.RESCUT
                  THEN
                     IF LFL1.EQ.0
                     THEN
                        LFL1=1
                        NHPOTT=NHPOTT+1
                     CIF
                  CIF
               CIF
C
               IPCO=IPCO+LHIT
            ELSE
               XWHILE
            CIF
         CWHILE
         IF LFL.EQ.1
         THEN
            NHPOT=NHPOT+1
         ELSE
            ISORT3(ISORT2(2,NHWIR))=-2
         CIF
      UNTIL IPCO.GT.IPCO9
C-----------------------------------------------------------------------
C
C IF LESS THAN 2 WIRES WITH GOOD Z MEASUREMENT, NOTHING DONE
      IF NHPOT.LT.2
      THEN
         IQUAL=-3
         RETURN
      CIF
C-----------------------------------------------------------------------
      KFLIP=2
C
      NHFIT=NHPOTT
      LFOUND=-1
      IF NHPOTT.LT.6.OR.NHPOTT.LT.NHPOT*.75
      THEN
C TRY TO FIND BETTER START VALUES
         PERFORM STVSEA
      CIF
C-----------------------------------------------------------------------
C
      INDMAX=NHFIT/4+1
      IF(INDMAX.GT.13) INDMAX=13
      INDFIT=0
      WHILE INDFIT.LT.INDMAX
         INDFIT=INDFIT+1
N    LINEAR FIT
         PERFORM LINFIT
         IF LNOCON.EQ.1
         THEN
C NO CONVERGENCE AS INDICATED BY LOSS OF TOO MANY HITS
            IQUAL=-4
            RETURN
         CIF
         IF(SIG.LT.SIGMIN) XWHILE
         IF INDFIT.GE.2
         THEN
            PERFORM LLSTOP
            IF LSTOP.EQ.1
            THEN
C      PREVIOUS FIT ACCEPTED, RESTORE ITS RESULTS
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
               XWHILE
            CIF
         CIF
         IF(INDFIT.EQ.INDMAX.OR.NHFIT.EQ.2) XWHILE
N      SAVE FIT RESULTS
         NHFTLS=NHFIT
         PAR1LS=PAR1
         PAR2LS=PAR2
         SIGLST=SIG
         S0LS=S0
         S1LS=S1
         S2LS=S2
         S3LS=S3
         S4LS=S4
N      HIT CLEANING
         PERFORM HITCLN
      CWHILE
C
C
N     SET UP PATR-BANK
      PERFORM FITBNK
      IQUAL=1
      RETURN
C=======================================================================
C
N     *************************
N     *      L I N F I T      *
N     *************************
C
C
N      LINEAR FIT
      PROC LINFIT
C
      LNOCON=0
N     GET EQUATIONS
      KFLIP=3-KFLIP
      NHF1=0
      S0 = 0.
      S1 = 0.
      S2 = 0.
      S3 = 0.
      S4 = 0.
      FOR IHWIR=1,NHWIR
         IH=ISORT2(2,IHWIR)
         NNH=ISORT2(3,IHWIR)
         FOR JNH=1,NNH
            ISORT4(KFLIP,IH+JNH-1)=0
         CFOR
         IF ISORT3(IH).EQ.1 .OR. ISORT3(IH).EQ.-1.AND.NNH.GT.1
         THEN
            RESMIN=10000.
            FOR JNH=1,NNH
               JH=IH+JNH-1
               IF ISORT3(JH).EQ.1
               THEN
                  IPCO=ISORT2(1,IHWIR)+(JNH-1)*LHIT
                  SA = WRK(IPCO+6)
                  ZA = WRK(IPCO+5)
                  WA = WRK(IPCO+7)
                  DZRESA=ZA-PAR1*SA-PAR2
                  DF0=ABS(DZRESA)
                  IF DF0.LT.RESMIN
                  THEN
                     RESMIN=DF0
                     S=SA
                     W=WA
                     DZRES=DZRESA
                     JHUSE=JH
                  CIF
               CIF
            CFOR
            IF RESMIN.LT.RESCUT
            THEN
               NHF1=NHF1+1
               S0=S0+W
               S1=S1+S*W
               S2=S2+S**2*W
               S3=S3+DZRES*W
               S4=S4+DZRES*S*W
               ISORT4(KFLIP,JHUSE)=1
            ELSE
               ISORT3(IH)=-2
            CIF
         CIF
      CFOR
      IF NHF1.LT.2 .OR. S2.LT.1.
      THEN
         LNOCON=1
      ELSE
         NHFIT=NHF1
C
N        SOLVE EQUATIONS
         F1 = 1. / S2
         XX12 = S1*F1
         YY1  = S4*F1
         PARR2=(S3-S1*YY1)/(S0-S1*XX12)
         PAR1=YY1-PARR2*XX12+PAR1
         PAR2=PAR2+PARR2
C
N     CALC. CHISQ + SOLVE L/R AMBIGUITY
         CHISQ = 0.
         NHF1=0
         FOR IHWIR=1,NHWIR
            IRESHT(IHWIR)=-1
            IH=ISORT2(2,IHWIR)
            NNH=ISORT2(3,IHWIR)
            IF ISORT3(IH).GE.0 .OR. ISORT3(IH).EQ.-1.AND.NNH.GT.1
            THEN
               RESMIN=10000.
               FOR JNH=1,NNH
                  JH=IH+JNH-1
                  IF ISORT3(JH).GE.0
                  THEN
                     IFLG=ISORT3(JH)
                     IPCO=ISORT2(1,IHWIR)+(JNH-1)*LHIT
                     SA = WRK(IPCO+6)
                     ZA = WRK(IPCO+5)
                     WA = WRK(IPCO+7)
                     DF0=ABS(ZA-PAR1*SA-PAR2)
                     IF DF0.LT.RESMIN
                     THEN
                        RESMIN=DF0
                        W=WA
                     CIF
                  CIF
               CFOR
               IF RESMIN.LT.8000.
               THEN
                  RESMIN=RESMIN*SQRT(W)
                  IRESHT(IHWIR)=RESMIN*1.E4
                  IF IFLG.EQ.1
                  THEN
                     CHISQ=CHISQ+RESMIN**2
                     NHF1=NHF1+1
                  CIF
               CIF
            CIF
         CFOR
         IF NHF1.LT.2
         THEN
            LNOCON=1
         ELSE
            IF NHF1.EQ.2
            THEN
               SIG=1.E-5
            ELSE
               SIG=CHISQ/(NHF1-2)
            CIF
         CIF
         NHFIT=NHF1
      CIF
      CPROC
C=======================================================================
      PROC HITCLN
C      LABEL HITS NOT TO BE USED IN THE NEXT ITRATION
C-------------------------------------------------------------
C
N       SORT HITS ACCORDING TO RESIDUALS
C  EXCLUDE THE INDFIT LARGEST RESIDUAL HITS,
C  RESTORE THE OTHERS (EXLUDED FOR EVER HITS NOT COUNTED)
C
         CALL SHELL9(IRESHT,ISORT1,NHWIR)
         KOMIT=0
         FOR J1=1,NHWIR
            IHWIR=ISORT1(NHWIR+1-J1)
            IPCO=ISORT2(1,IHWIR)
            NNH=ISORT2(3,IHWIR)
            IH=ISORT2(2,IHWIR)
            LFLG=0
            FOR JNH=1,NNH
               IHA=IH+JNH-1
               IQA=ISORT3(IHA)
               IF IQA.GT.-1
               THEN
                  IF LFLG.EQ.0
                  THEN
                     LFLG=1
                     KOMIT=KOMIT+1
                  CIF
                  IF KOMIT.LE.INDFIT
                  THEN
                     ISORT3(IHA)=0
                  ELSE
                     ISORT3(IHA)=1
                  CIF
               CIF
            CFOR
         CFOR
      CPROC
C=======================================================================
      PROC LLSTOP
         IF INDFIT.LE.6
         THEN
            INDCK=INDFIT-1
         ELSE
            INDCK=5
         CIF
         ICHCK=NCHECK(INDCK)
         WHILE SIGLST*SIGFAC.LT.RCHECK(ICHCK,1,INDCK)
            ICHCK=ICHCK-1
         CWHILE
         IF(ICHCK.LT.1) ICHCK=1
         IF SIG/SIGLST*STPFAC.GT.RCHECK(ICHCK,2,INDCK)
         THEN
            LSTOP=1
         ELSE
            LSTOP=0
         CIF
      CPROC
C=======================================================================
C
C
N     *************************
N     *      F I T B N K      *
N     *************************
C
C
N     SET UP FIT-BANK
      PROC FITBNK
C
      CTGTH=PAR1
      CSTH = 1./SQRT(CTGTH**2 + 1.)
      SNTH  = CSTH * CTGTH
C
C
C
N     COPY TRACK BANK
         IFREE=NDIWRK-100
         CALL MVCL(IWRK(IFREE),0,IDATA(IPTR+1),0,4*LDTR)
C
N     FILL FIT-BANK
         IP    = IFREE - 1
         IWRK(IP+ 2) = LAND(IWRK(IP+2),MASK4)
         IWRK(IP+ 2) = LOR(IWRK(IP+2),4096)
         WRK (IP+ 7) = PAR2
         A=SQRT(WRK(IP+8)**2+WRK(IP+9)**2)
         WRK (IP+ 8) = WRK (IP+ 8)/A*CSTH
         WRK (IP+ 9) = WRK (IP+ 9)/A*CSTH
         WRK (IP+10) = SNTH
C CALCULATE TRACK LENGTH IN R-PHI FROM FIRST TO LAST POINT ON TRACK
         CURVXY=WRK(IP+25)
         IF(ABS(CURVXY).LT.1.E-9) CURVXY = SIGN(1.E-9,CURVXY)
         UU=SQRT((WRK(IP+12)-WRK(IP+5))**2+(WRK(IP+13)-WRK(IP+6))**2)
         ARGARG = .5*CURVXY*UU
         IF(ABS(ARGARG).GT.1.) ARGARG = SIGN(1.,ARGARG)
         IF(ABS(CURVXY*UU).GT.1.E-5) UU=2.*ARSIN(ARGARG)/CURVXY
C        IF(ABS(CURVXY*UU).GT.1.E-5) UU=2.*ARSIN(.5*CURVXY*UU)/CURVXY
         WRK (IP+14) = PAR2+UU*PAR1
         A=SQRT(WRK(IP+15)**2+WRK(IP+16)**2)
         WRK (IP+15) = WRK (IP+15)/A*CSTH
         WRK (IP+16) = WRK (IP+16)/A*CSTH
         WRK (IP+17) = SNTH
         IWRK(IP+33) = NHFIT
         WRK (IP+32) = SQRT(SIG)
C FIT TYPE WILL BE 2: "HELIX FIT"
         IWRK(IP+29) = 2
         WRK (IP+30) = PAR1
C GET CLOSEST POINT (XP,YP) TO ORIGIN
         DDR0=DISTXY(ADATA(IPTR+5),ADATA(IPTR+6),ADATA(IPTR+8),
     +   ADATA(IPTR+9),1./CURVXY,0.,0.,XP,YP,FI)
C CALCULATE TRACK LENGTH ALONG CIRCLE FROM FIRST POINT TO (XP,YP)
         UU=SQRT((XP-WRK(IP+5))**2+(YP-WRK(IP+6))**2)
         ARGARG = .5*CURVXY*UU
         IF(ABS(ARGARG).GT.1.) ARGARG = SIGN(1.,ARGARG)
         IF(ABS(CURVXY*UU).GT.1.E-5) UU=2.*ARSIN(ARGARG)/CURVXY
C        IF(ABS(CURVXY*UU).GT.1.E-5) UU=2.*ARSIN(.5*CURVXY*UU)/CURVXY
         WRK (IP+31) = PAR2-PAR1*UU
C
         IF LDTR.GE.59 .AND. NHFIT.GE.4 .AND. LNOCON.EQ.0
         THEN
C CALCULATE COVARIANCE MATRIX
            DET=S0*S2-S1**2
            FACT=SIG/DET
            WRK(IP+56)=SIG*(NHFIT-2)/20.**2
            WRK(IP+57)=(S2+2.*UU*S1+UU**2*S0)*FACT
            WRK(IP+58)=-(UU*S0+S1)*FACT
            WRK(IP+59)=S0*FACT
         CIF
C
C MARK HITS USED IN THE FIT
         LFL=0
         FOR IHWIR=1,NHWIR
            IH=ISORT2(2,IHWIR)
            NNH=ISORT2(3,IHWIR)
            FOR JNH=1,NNH
               IPCO=ISORT2(1,IHWIR)+(JNH-1)*LHIT
               HWRK(2*IPCO+3)=ISORT4(KFLIP,IH+JNH-1)
               IF(HWRK(2*IPCO-1).GT.100.AND.HWRK(2*IPCO+3).EQ.1) LFL=1
            CFOR
         CFOR
         IF(LFL.EQ.1) IWRK(IP+ 2) = LOR(IWRK(IP+2),16384)
C
N     PUT RESULT INTO PATR-BANK
         CALL MVCL(IDATA(IPTR+1),0,IWRK(IFREE),0,4*LDTR)
C
C CREATE Z-S BANK 'ZSPD'
C
         IF IOPT.GT.8
         THEN
            CALL CCRE(NPZSPD,'ZSPD',ITRK,5*NHALL+1,IERR)
            IF IERR.EQ.0
            THEN
               CALL BSAW(1,'ZSPD')
               NPZSP1=NPZSPD+1
               IDATA(NPZSP1)=5
               FOR IHWIR=1,NHWIR
                  IH=ISORT2(2,IHWIR)
                  NNH=ISORT2(3,IHWIR)
                  FOR JNH=1,NNH
                     IPCO=ISORT2(1,IHWIR)+(JNH-1)*LHIT
                     IFL=ISORT4(KFLIP,IH+JNH-1)
                     IF IFL.EQ.1
                     THEN
                        IFL=0
                     ELSE
                        IFL=16
                     CIF
                     IDATA(NPZSP1+1)=HWRK(2*IPCO+4)
                     ADATA(NPZSP1+2)=WRK(IPCO+6)
                     ADATA(NPZSP1+3)=WRK(IPCO+5)
                     IDATA(NPZSP1+4)=IFL
                     ADATA(NPZSP1+5)=WRK(IPCO+7)
                     NPZSP1=NPZSP1+5
                  CFOR
               CFOR
            CIF
         CIF
      CPROC
C=======================================================================
      PROC STVSEA
C  SEARCH FOR STARTING VALUES
C
C  ORDER ACCORDING TO S
         CALL SHELL9(KSZSRT(1,1),KSORT3,NHALL)
         NH9=NHALL
         WHILE KSZSRT(KSORT3(NH9),1).GT.99999
            NH9=NH9-1
         CWHILE
         NH99=NH9
         NH1=1
         LFOUND=0
         NHMAX=0
         CI2MAX=1.E10
         WHILE NH9.GT.NH1
            FOR KK=1,3
               IF KK.EQ.1
               THEN
                  JH1=NH1
                  JH9=NH9
               ELSE
                  IF KK.EQ.2
                  THEN
                     JH9=JH9-1
                  ELSE
                     JH9=JH9+1
                     JH1=JH1+1
                  CIF
               CIF
               IF JH9.GT.JH1
               THEN
                  AS1=KSZSRT(KSORT3(JH1),1)
                  AS9=KSZSRT(KSORT3(JH9),1)
                  IF(AS9.LT.AS1+5.) XWHILE
                  AZ1=KSZSRT(KSORT3(JH1),2)
                  AZ9=KSZSRT(KSORT3(JH9),2)
                  PAR1=(AZ9-AZ1)/(AS9-AS1)
                  PAR2=AZ1-PAR1*AS1
                  NHFIT=0
                  CI2=0.
                  FOR I=1,NH99
                     DZ=ABS(KSZSRT(KSORT3(I),2)-PAR1*KSZSRT(KSORT3(I),1)
     +               -PAR2)
                     IF DZ.LT.RESCUT
                     THEN
                        NHFIT=NHFIT+1
                        CI2=CI2+DZ**2
                     CIF
                  CFOR
                  IF NHFIT.GT.NHPOT*.75
                  THEN
                     LFOUND=1
                     XWHILE
                  CIF
                  IF NHFIT.GT.NHMAX .OR.NHFIT.EQ.NHMAX.AND.CI2.LT.CI2MAX
                  THEN
                     NHMAX=NHFIT
                     P1MAX=PAR1
                     P2MAX=PAR2
                     CI2MAX=CI2
                  CIF
               CIF
            CFOR
            NH1=NH1+1
            NH9=NH9-1
         CWHILE
         IF LFOUND.EQ.0 .AND. NHMAX.GT.0
         THEN
            NHFIT=NHMAX
            PAR1=P1MAX
            PAR2=P2MAX
         CIF
      CPROC
      END
