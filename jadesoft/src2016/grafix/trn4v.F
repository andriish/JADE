C   21/03/83 807251813  MEMBER NAME  TRN4V    (S)           FORTRAN
      SUBROUTINE TRN4V(MODE,IPPATR,ISTART,NLENG,NCONT,ISTAT,IER)
C----------------------------------------------------------------
C---     VERSION ???      P.STEFFEN
C---     STORE 4-VECTORS OF CHARGED TRACKS + NEUTRALS IN COMMON /CTRCK0
C---     MODIFICATION  24.2.86                R.RAMCKE
C---     MODE = 0 INCLUSIVE NEUTRALS
C---          = -1  WITHOUT NEUTRALS
C---     MODIFICATION  19.9.86                R.RAMCKE
C---     MODIFICATION  23.1.87                R.RAMCKE
C---     MODIFICATION  16.4.87                R.RAMCKE
C---     MODIFICATION   2.7.87                J.HAGEMANN     IER SET
C---     MODIFICATION  25.11.87               J.HAGEMANN     UPDATED
C        CUT FOR MOMENTUM-RESCALING SET BY DEFAULT OR ENTRY TRNSET
C        INPUT  MODE
C               IPPATR = POINTER TO PATR-BANK
C               ISTART = ISTARTINDEX OF STATISTICS ARRAY
C               NLENG  = LENGTH OF STATITICS ARRAY
C               NCONT  = ISTART + NUSED + 1 : FIRST FREE IN STAT ARRAY
C               ISTAT  =  STATISTICS ARRAY
C        OUTPUT   COMMON / CTRCK0 / NTCH,NTALL,P(20,200),INDTRK(200)
C               NTCH = USED CHARGED TRACKS
C               NTALL = IF MODE = 0 THEN NTALL = NTCH + NEUTRALS
C                                   ELSE NTALL = NTCH
C               FOR EACH TRACK I:
C               P(1,I)  = PX
C               P(2,I)  = PY
C               P(3,I)  = PZ
C               P(4,I)  = E = SQRT(P**2 + M(PI)**2)
C               P(5,I)  = M(PI)
C               P(6,I)  = P TOTAL
C               P(7,I)  = CHARGED +-1.0
C               P(8,I)  = RAPIDITY
C               P(9,I)  = P TRANSVERS
C              IP(10,I) = TRACK# IN PATR BANK
C         ADDITIONAL INFOMATION
C               PE(1,I)  = DIST TO RUN-VTX + OUTSIDE - INSIDE CIRCLE
C               PE(2,I)  = X } CLOSEST APPROACH TO VERTEX
C               PE(3,I)  = Y
C               PE(4,I)  = DX }  DIRECTION OF PARTICLE
C               PE(5,I)  = DY }  AT THE VERTEX
C               PE(6,I)  = SIGMA RPHI OF TRACK
C              IPE(7,I)  = # OF ALL HITS FOR FIT
C              IPE(8,I)  = # OF VTXC HITS FOR FIT
C               PE(9,I)  = COSPHI TO JETAXIS
C              IPE(10,I) = 1) JADE PARTICLE-TYPE*1000
C                          2) ORIGIN FLAG FROM PARORG * 100
C                          3) LEPTON QUALITY (*1 FOR E, *10 FOR MU)
C
C               INDTRK(IPATTRK#) = POS IN P-ARRAY
C------------------------------------------------------------------
      IMPLICIT INTEGER*2 (H)
C
#include "cdata.for"
C
#include "calibr.for"
C                         MACRO FOR TRACK-INFO
#include "mtrinf.for"
C
C
      DIMENSION RES(10),ISTAT(NLENG)
C
C     /////////////////// STANDARD-CUTS ON TRACKS////////////////////
      DATA SIG / 0.60 / , MHITS / 12 /, SIGZ /120.0 /, MZHITS / 12 /,
     *     ZDIS / 200.0 /,TRKL2/ 75635.0 /,DRPHI / 20.0 /
C     ////////////////////////////////////////////////////////////////
C
      DATA LBINIT /0/ ,CUT / 17.5 /
C
      IF(LBINIT.NE.0) GOTO 100
         LBINIT = 1
         IQHEAD = IBLN('HEAD')
CCC      IQPATR = IBLN('PATR')
         IQLGCL = IBLN('LGCL')
         WRITE(6,876) CUT
 876     FORMAT(/' ++++ TRN4V ++++  MOMENTUM RESCALING : ' ,F8.2/)
C
         AMPI = .140
C
         ELGMIN = .100
         ELGTRK = .450
         NUSED = 5
         NCONT = ISTART + NUSED
C
  100 CONTINUE
C
        IER    = 0
        NTALL = 0
        NTCH = 0
        DO 23 I = 1,400
          ITRFLG(I) = 0
  23      INDTRK(I) = 0
C
C       GET RUN # + EVENT #
        IPHEAD = IDATA(IQHEAD)*2
        NRUN = HDATA(IPHEAD+10)
C
C       CHECK IF PATR-BANK WITH >1 TRACK
CCC     IPPATR = IDATA(IQPATR)
        IF(IPPATR.LE.0) GOTO 900
C       CHECK IF >0 TRACKS
        NTR=IDATA(IPPATR+2)
        IF(NTR.LE.0) GOTO 900
C
C       GET X-Y-VERTEX
        CALL VTXCRV( NRUN, XO, YO ,XDX, YDY)
C
COLD    IPV    = ICALIB(10)
COLD    XO     = ACALIB(IPV+ 1)
COLD    YO     = ACALIB(IPV+ 3)
C
C
        L0=IDATA(IPPATR+1)
        LTR=IDATA(IPPATR+3)
        IPTR1=IPPATR+L0-LTR
C
        DO 39 ITR=1,NTR
           IPTR1 = IPTR1 + LTR
C                                          DETERMINE 4-MOMENTUM
           CALL TRPVTX(IPTR1,XO,YO,RES)
C
C            RES(1)   : DR = DISTANCE (XV,YV)-TRACK(IPTR)
C                                  +VE IF OUTSIDE CIRCLE
C                                  -VE IF INSIDE CIRCLE
C            RES(2)   : X-POSITION OF CLOSEST POINT
C            RES(3)   : Y-POSITION OF CLOSEST POINT
C            RES(4)   : X-DIRECTION AT CLOSEST POINT (ONLY IN R-FI)
C            RES(5)   : Y-DIRECTION AT CLOSEST POINT (ONLY IN R-FI)
C            RES(6)   : )
C            RES(7)   : )  DIRECTION COSINES IN SPACE
C            RES(8)   : )
C            RES(9)   : PT, TRANSVERSE MOMENTUM
C            RES(10)  : P , MOMENTUM
C
C                                          CHECK R-FI-FIT, # OF HITS
 33        ISTAT(ISTART) = ISTAT(ISTART)+ 1
           IF(ADATA(IPTR1+23) .GT. SIG .OR.
     *        IDATA(IPTR1+24).LE. MHITS) GOTO 38
C                                          CHECK ZFIT, # OF HITS
           ISTAT(ISTART+1) = ISTAT(ISTART+1)+ 1
           IF(ADATA(IPTR1+32) .GT. SIGZ .OR.
     *        IDATA(IPTR1+33) .LE. MZHITS)  GOTO 38
C                                          Z(TRACK) < 100. MM
           ISTAT(ISTART+2) = ISTAT(ISTART+2)+ 1
           IF(ABS(ADATA(IPTR1+31)) .GT. ZDIS) GOTO 38
C                                          MEASURED LENGTH > 275 MM
           ALMSQ = (ADATA(IPTR1+12) - ADATA(IPTR1+5))**2
     &           + (ADATA(IPTR1+13) - ADATA(IPTR1+6))**2
           ISTAT(ISTART+3) = ISTAT(ISTART+3)+ 1
           IF(ALMSQ .LT. TRKL2) GOTO 38
C                                        CHECK IF TRACK FROM MAIN VERTEX
           ISTAT(ISTART+4) = ISTAT(ISTART+4)+ 1
           IF(ABS(RES(1)) .GT. DRPHI) GOTO 38
C                                        REGISTRER NEW TRACK
           ISTAT(ISTART+5) = ISTAT(ISTART+5)+ 1
           NTALL = NTALL + 1
           ITRFLG(NTALL) = 1
           RES(10) = AMIN1(RES(10),CUT)
           P ( 1,NTALL) = RES(10) * RES(6)
           P ( 2,NTALL) = RES(10) * RES(7)
           P ( 3,NTALL) = RES(10) * RES(8)
           P ( 4,NTALL) = SQRT(RES(10)**2 + AMPI**2)
           P ( 5,NTALL) = AMPI
           P ( 6,NTALL) = RES(10)
           P ( 7,NTALL) =-SIGN(1.,ADATA(IPTR1+25))
           P ( 8,NTALL) = 0.
           P ( 9,NTALL) = RES( 9)
           IP(10,NTALL) = IDATA(IPTR1+1)
C
           DO 32 K = 1,5
 32           PE(K,NTALL) = RES(K)
           PE (6,NTALL) = ADATA(IPTR1+23)
           IPE(7,NTALL) = IDATA(IPTR1+24)
           IPE(8,NTALL) = 0
           IF(LTR .EQ. 64) IPE(8,NTALL) = IDATA(IPTR1+59)
C
           IDTRCK = IDATA(IPTR1+1)
           IF(IDTRCK.GT.0 .AND. IDTRCK.LE.400) INDTRK(IDTRCK) = NTALL
C
C
           GOTO 39
   38      CONTINUE
   39   CONTINUE
C
        NTCH = NTALL
        IF(MODE .EQ. -1) GOTO 900
C
C       SET # OF CHARGED TRACKS
C
C                                      --- ADD NEUTRALS ---
      IPLGCL = IDATA(IQLGCL)
      NCLST = IDATA(IPLGCL+7)
      IF (NCLST.LE.0) GOTO 900
C
  120 IP3 = IDATA(IPLGCL+3)
      LCLST = IDATA(IPLGCL+25)
      IP1 = IPLGCL + IP3 -1
      IP7 = IP1 + (NCLST-1) * LCLST
      JC = 0
C                                      --- LOOP OVER ALL CLUSTERS ---
      DO 150 L=IP1,IP7,LCLST
         JC = JC + 1
         ECL = ADATA(L+2)
         ICON = IDATA(L+8) / 100
         ITR  = IDATA(L+8) - ICON*100
         IF(ICON .GT. 1) ITR  =-ITR
         IF(ICON .LT. 0) GOTO 150
C          SUBTR. EQUIV. ENERGY FOR TRACKS FROM CLUSTER ---
           ETOT = ECL - ELGTRK*ICON
           IF (ETOT.LT.ELGMIN) GOTO 150
           NTALL = NTALL + 1
           ITRFLG(NTALL) = 2
           P ( 1,NTALL) = ETOT * ADATA(L+ 9)
           P ( 2,NTALL) = ETOT * ADATA(L+10)
           P ( 3,NTALL) = ETOT * ADATA(L+11)
           P ( 4,NTALL) = ETOT
           P ( 5,NTALL) = 0.
           P ( 6,NTALL) = ETOT
           P ( 7,NTALL) = 0.
           IP( 8,NTALL) = ITR
           P ( 9,NTALL) = ETOT * SQRT(1.-ADATA(L+11)**2)
           IP(10,NTALL) = JC
  150 CONTINUE
C
C
  900 CONTINUE
C                                  FIND JET TO WHICH BELONGS TRACK
      NH = NTALL
      NH1 = NH+1
      CALL SPTHAK(1,NH,0,THR,NH1,SPHR,0,AKO,IER )
C
      IF(IER .NE. 0) RETURN
C
      DO 1000 I = 1,NTALL
         RNORM = SQRT((P(1,NH1)**2 + P(2,NH1)**2)*
     *           (P(1,I)**2 + P(2,I)**2))
C
         PE(9,I) = -100.
         IF( RNORM .GT. 0.0)
     *      PE(9,I) = (P(1,I)*P(1,NH1) + P(2,I)*P(2,NH1))/RNORM
 1000 CONTINUE
C
C
      RETURN
      ENTRY TRNSET(X)
      CUT = X
      RETURN
C
      END
