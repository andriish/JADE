C   06/03/82 705311925  MEMBER NAME  DEDXB1   (S)           FORTRAN
      SUBROUTINE DEDXB1(ITR)
C-----------------------  LAST CHANGE: 24.03.86   L.BECKER  ---C
C-----------------------  LAST CHANGE: 04.06.86  J.A.J.SKARD --C
C-----------------------  LAST CHANGE: 06.06.86  J.A.J.SKARD --C
C-----------------------  LAST CHANGE: 27.08.86  J.OLSSON    --C
C  SEVERAL CHANGES IN 1987, NOT COMMENTED,   L.SMOLEK...     --C
C  PMCUT IN COMMON/CDEDXU/ FOR VARIABLE P-CUT   J.O.  28.5.87 -C
C  UPDATED PERIODS ACC. TO DEDXBN               J.O.  31.5.87 -C
C                                                              C
C     DEDX FOR ONE TRACK ITR IN 'PATR' BANK                    C
C     SLIGHTLY MODIFIED VERSION OF DEDXBN                      C
C     PUTS ENERGY DEPOSITS FOR EACH WIRE INTO ARRAY EWIR(60),  C
C     CORRESPONDING X,Y,R, AND FITTED Z-VALUES INTO ARRAYS     C
C     XWIR(60),YWIR(60),RWIR(60), AND ZWIR(60), RESPECTIVELY   C
C                                                              C
C     CALCULATION OF DE/DX, SIGMA(DE/DX)      -   P.DITTMANN   C
C     COMPARISON WITH THE THEORETICAL VALUE   -   J.SKARD      C
C     RESULTS ARE WRITTEN INTO COMMON /CWORK1/                 C
C     THE ARRAY TRES(10,ITR) HOLDS THE FOLLOWING RESULTS       C
C                                                              C
C     ITRES(1,ITR)  =  NHIT, QUALITY OF DEDX                   C
C      TRES(2,ITR)  =  DEDX                                    C
C      TRES(3,ITR)  =  SIGMA(DEDX)                             C
C      TRES(4,ITR)  =  CHISQ(ELECTRON)                         C
C      TRES(5,ITR)  =  CHISQ(PION)                             C
C      TRES(6,ITR)  =  CHISQ(KAON)                             C
C      TRES(7,ITR)  =  CHISQ(PROTON)                           C
C     ITRES(8,ITR)  =  JMIN, NUMBER FOR MINIMUM CHISQUARE      C
C                      1 = P, 2 = K, 3 = PI, 4 = E, 0=NO DEDX  C
C      TRES(9,ITR)  =  MOMENTUM (GEV)                          C
C     TRES(10,ITR)  =  MOMENTUM ERROR                          C
C--------------------------------------------------------------C
C----------------DEDX RESULTS-----------------------
C      COMMON /CWORK1/ IER,NTRR,TRES(10,60)
       COMMON /CWORK1/ IER,NTRR,TRES(10,60),EWIR(60),XWIR(60),YWIR(60),
     *                 RWIR(60),ZWIR(60)
       DIMENSION ITRES(10,60)
       EQUIVALENCE (TRES(1,1),ITRES(1,1))
C----------------END--------------------------------
C
C HDATA IS DECLARED INTEGER*2 IN MACRO CDEDEDX
C
#include "cdata.for"
#include "cgraph.for"
#include "cjdrch.for"
C
#include "cdededx.for"
       COMMON/CDEJAS/DEBEST,PBEST
      DIMENSION ICEL(60),XYRZ(6)
C
      DATA IWARN / 0 /, JWARN/0/
C
      IPVERS=JPOINT(IPN+1)
      ICALL =ICALL+1
      IF(ICALL.GT.1)GO TO 9
      IPJETC = IBLN('JETC')
      IPJETV = IBLN('JETV')
      IPJHTL = IBLN('JHTL')
      IPPATR = IBLN('PATR')
      IPHEAD = IBLN('HEAD')
    9 CONTINUE
C
      IJETC = IDATA( IPJETC )
      IJETV = IDATA( IPJETV )
      IJHTL = IDATA( IPJHTL )
      IPATR = IDATA( IPPATR )
      IX    = 2*IDATA( IPHEAD )
      IRUN=HDATA(IX+10)
C        MONTE CARLO - RETURN
      IF(IRUN.EQ.0) RETURN
      IPN = JPOINT(8)
      IF(ICALL.EQ.1 .AND. NDDINN .EQ. 0 ) PRINT 901, IPVERS
  901 FORMAT(/'  ***********  DEDX - RECALIBRATION  VERSION',I8/)
      IF(NHFCUT.EQ.15) GO TO 903
      JWARN=JWARN+1
      IF(NDDINN .EQ. 0 .AND. JWARN .LE. 1) PRINT 902, NHFCUT
  902 FORMAT(/'  ***********  WARNING:         NHFCUT=     ',I8/)
C
  903 CONTINUE
      IF(IJETC.EQ.0 .OR.IJHTL.EQ.0 .OR.IPATR.EQ.0) GOTO 56
      IJETC2 = IJETC*2
      IJETV2 = IJETV*2 + 2
      NT = IDATA(IPATR+2)
C     IF(NT.LE.0 .OR. NT.GT.NTRTOT) GOTO 57
      IF(NT.LE.0 .OR. NT.GT.NTRTOT .OR. ITR.GT.NT) GOTO 57
      IER=0
      NTRR=NT
C        DATE FOR RUN-DEPENDENT CORRECTIONS
      IFLAG = 0
      IPERD = 1
      IF(IRUN.GE.1400)  IPERD=2
      IF(IRUN.GE.2600)  IPERD=3
      IF(IRUN.GE.3730)  IPERD=4
      IF(IRUN.GE.6000)  IPERD=5
      IF(IRUN.GE.7592)  IPERD=6
      IF(IRUN.GE.10000) IPERD=7
      IF(IRUN.GE.11021) IPERD=8
      IF(IRUN.GE.13088) IPERD=9
      IF(IRUN.GE.14605) IPERD=10
      IF(IRUN.GE.15690) IPERD=11
      IF(IRUN.GE.16803) IPERD=12
      IF(IRUN.GE.17988) IPERD=13
      IF(IRUN.GE.19068) IPERD=14
      IF(IRUN.GE.21123) IPERD=15
      IF(IRUN.GE.21705) IPERD=16
      IF(IRUN.GE.22651) IPERD=17
      IF(IRUN.GE.24201) IPERD=18
      IF(IRUN.GE.25454) IPERD=19
      IF(IRUN.GE.27938) IPERD=20
C        Z RECALIBRATION (IF NOT YET DONE)
      IHH=HDATA(IJETC2+2)
      MODE=1
      IF(IPERD.LT.9  .AND. IHH.NE.8305) CALL ZSFIT(MODE)
      IF(IPERD.EQ.9  .AND. IHH.NE.8404) CALL ZSFIT(MODE)
      IF(IPERD.EQ.10 .AND. IHH.NE.8404) CALL ZSFIT(MODE)
      IF(IPERD.EQ.11 .AND. IHH.NE.8412) CALL ZSFIT(MODE)
      IF(IPERD.EQ.12 .AND. IHH.NE.8412) CALL ZSFIT(MODE)
      IF(IPERD.EQ.13 .AND. IHH.NE.8412) CALL ZSFIT(MODE)
      IF(IPERD.EQ.14 .AND. IHH.NE.8511) CALL ZSFIT(MODE)
      IF(IPERD.EQ.15 .AND. IHH.NE.8511) CALL ZSFIT(MODE)
      IF(IPERD.EQ.16 .AND. IHH.NE.8611) CALL ZSFIT(MODE)
      IF(IPERD.EQ.17 .AND. IHH.NE.8611) CALL ZSFIT(MODE)
      IF(IPERD.EQ.18 .AND. IHH.NE.8610) CALL ZSFIT(MODE)
      IF(IPERD.EQ.19 .AND. IHH.NE.8610) CALL ZSFIT(MODE)
      IF(IPERD.EQ.20 .AND. IHH.NE.8610) CALL ZSFIT(MODE)
C
      FIELD = HDATA(IX+30)
      NDAY=HDATA(IX+7)*30+HDATA(IX+6)-30
      MDAY=NDAY
      IF (IFLAG.EQ.1) GOTO 60
   70 CONTINUE
C              --- SPECIAL CORRECTIONS:
      IF (IRUN.GT.12644) CA(8)=0.248
C
      NDAY = NDAY - ID0(IPERD)
      DAY = NDAY
C --------------------------------------- GAS PRESSURE
      PREFAC = 1. / (1.+CP(IPERD)*BP(IPERD)*DAY/AP(IPERD))
C --------------------------------------- SUBPERIODS
      DO 5 L=1,3
      IF(NDAY.LT.LSP(L,IPERD)) GOTO 6
    5 CONTINUE
      L = 1
    6 SUBFAC = SPF(L,IPERD)
C --------------------------------------- ELECTRON ATTACHMENT
      ICLBIN = DAY/10. + 1.5
      IF(ICLBIN.LE.0) ICLBIN=1
      IF(ICLBIN.GT.19) ICLBIN=19
      FRACT = DAY/10. - ICLBIN + 1
      TIMCOR = CA(IPERD) * (CL(ICLBIN)+FRACT*(CL(ICLBIN+1)-CL(ICLBIN)))
      TIMCOR = TIMCOR/1000.
C --------------------------------------- LOOP OVER ALL TRACKS
C ---------- LOOP TAKEN OUT ----------
      L0 = IDATA(IPATR+1)
      LT = IDATA(IPATR+3)
CCC   KT = IPATR + L0
CCC   DO 59 I=1,NT
C--------
      I=ITR
      KT = IPATR + L0 + (ITR-1)*LT
      DEBEST=0.
      PBEST=0.
C--------
      CALL MOMENP(KT,PX,PY,PZ,PTRANS,PMOM,PHI,THE)
C--------
      IF (PMOM.LT.PMCUT) GOTO 58
      NHIT = 0
      CALL VZERO(IZB,60)
      NSAME = 0
      CALL VZERO(EWIR,300)
      IF(ADATA(KT+25).EQ.0.) GOTO 58
      R = -1. / ADATA(KT+25)
      SR = SIGN(1.,R)
      CTH = SQRT(ADATA(KT+8)**2+ADATA(KT+9)**2)
      STH = ADATA(KT+10)
      TTH = ABS(STH/CTH)
      IF (TTH.GT.6.) GOTO 58
C
      CPHI = ADATA(KT+8) / CTH
      SPHI = ADATA(KT+9) / CTH
      X0 = ADATA(KT+5)
      Y0 = ADATA(KT+6)
      XM = X0 - R*SPHI
      YM = Y0 + R*CPHI
      R0 = SQRT(X0**2+Y0**2)
      XE = ADATA(KT+12)
      YE = ADATA(KT+13)
      SE = R * ATAN2(SR*(CPHI*(XE-X0)+SPHI*(YE-Y0)),
     *               SR*(SPHI*(XE-X0)-CPHI*(YE-Y0)+R))
      IF(ABS(R).GT.1.E5) SE=SQRT((XE-X0)**2+(YE-Y0)**2)
C
C --------------------------------------- ORDER CELLS
      NC = 0
      DO 12 J=1,6
      KC = IDATA(KT+40-J)
      IF(KC.LE.0 .OR. KC.GT.96) GOTO 12
      IF(NC.EQ.0) GOTO 11
      IF(KC.GT.IC(NC)) GOTO 11
      DO 10 K=1,NC
      IF(KC.GT.IC(K)) GOTO 10
      IF(KC.EQ.IC(K)) GOTO 12
      JC = IC(K)
      IC(K) = KC
      KC = JC
   10 CONTINUE
   11 NC = NC + 1
      IC(NC) = KC
   12 CONTINUE
C --------------------------------------- LOOP OVER CELLS
      IM2 = I*2
      IM1 = IM2*65536
      DO 19 J=1,NC
      NCP = IJETC2 + IC(J) + 2
      NHCELL = (HDATA(NCP+1)-HDATA(NCP))/4
      KHCELL = (HDATA(NCP)-1)/4
      DO 14 L=1,16
   14 IHDT(L) = 0
C --------------------------------------- LOOP OVER HITS
      DO 18 K=1,NHCELL
      KA = IJHTL + KHCELL + K + 1
      IF(IDATA(KA).EQ.0) GOTO 18
      LRFLAG = 0
      ZFLAG = 0
      IMH=LAND(IDATA(KA),MTRNO1)
      IF(IMH.NE.IM1) GOTO 13
      IF(TBIT(IDATA(KA),15)) ZFLAG=1
      IF(TBIT(IDATA(KA), 7)) LRFLAG=1
      KABIT = 15
      GOTO 15
   13 IMH=LAND(IDATA(KA),MTRNO2)
      IF(IMH.NE.IM2) GOTO 18
      IF(TBIT(IDATA(KA),31)) ZFLAG=1
      IF(TBIT(IDATA(KA),23)) LRFLAG=1
      KABIT = 31
C ------------------------------------- HIT FOUND IN CELL IC(J), TRACK I
   15 IF (NHIT.EQ.60) GOTO 20
      NHIT = NHIT + 1
      NH1 = IJETC2 + (KHCELL+K-1)*4 + 101
      IJC(NHIT) = NH1
      ICEL(NHIT) = IC(J)
      IJH(NHIT) = KA
      IF(KABIT.EQ.15) IJH(NHIT)=-IJH(NHIT)
      LRFL(NHIT) = LRFLAG
      MHIT = HDATA(NH1)
      NW(NHIT) = MHIT / 8
      LH(NHIT) = LAND(MHIT,7) + 1
      ILAY = LAND(NW(NHIT),15) + 1
      IF (ILAY.LE.0 .OR. ILAY.GT.16) GOTO 18
      IHDT(ILAY) = NHIT
      IZB(NHIT) = 0
      IF(ZFLAG.EQ.0) GOTO 18
      IZB(NHIT) = LH(NHIT)
   18 CONTINUE
C
C --------------------------------------- X-TALK MATRIX
      DO 119 L=1,16
      K = IHDT(L)
      IF(K.LE.0 .OR. K.GT.60) GOTO 119
      DO 114 M=1,4
      IXL(K,M) = 0
      IXH(K,M) = 0
      IF(L.LE.M) GOTO 112
      IF(IHDT(L-M).EQ.0) GOTO 112
      IXL(K,M) = IHDT(L-M)
  112 IF(L.GE.17-M) GOTO 114
      IF(IHDT(L+M).EQ.0) GOTO 114
      IXH(K,M) = IHDT(L+M)
  114 CONTINUE
  119 CONTINUE
   19 CONTINUE
   20 CONTINUE
C --------------------------------------- LOOP OVER ALL HITS OF TRACK I
      NHF = 0
      NCL = -1
C
      DO 29 J=1,NHIT
      IF(IZB(J).EQ.0) GOTO 29
      NC = NW(J) / 16
      NR = NC / 24
      IF(NR.LT.3) NR=NR+1
      INR = (NR-1)*2
      NL = LAND(NW(J),15)
      NH1 = IJC(J)
C --------------------------------------- AMPLITUDES & DRIFT TIME
      AR = HDATA(NH1+1)
      AL = HDATA(NH1+2)
      T = HDATA(NH1+3) / 64.
C
      ILR = LRFL(J) + 1
      TM = VDREL(IPERD)*(TMAX(ILR,NR)+TSLP(ILR,NR)*NL)
      XT = T / TM
      AMP = AL + AR
      AMP = AMP / 8.
C --------------------------------------- REMOVE HITS IN CELL CORNERS
      IF(NL.GT.0 .AND. NL.LT.14) GOTO 21
      IF(T.LT.90.) GOTO 21
      IF(NL.EQ.15.AND.LRFL(J).EQ.1) GOTO 26
      IF(NL.EQ. 0.AND.LRFL(J).EQ.0) GOTO 26
      IF(NR.EQ.2.AND.NL.EQ.14.AND.LRFL(J).EQ.1.AND.T.GT.180.) GOTO 26
   21 IF(XT.GT.0.97) GOTO 26
      IF(T.LE.4.) GOTO 26
      IF(IPERD.EQ.1 .AND. NC.EQ.9) GOTO 26
C --------------------------------------- X - TALK
      DW(J) = 0
      DO 124 M=1,4
      JXT = 1
      K = IXL(J,M)
      IF(K.LE.0 .OR. K.GT.60) GOTO 122
  121 NHX = IJC(K)
      AMPX = (HDATA(NHX+1)+HDATA(NHX+2))/8.
      TX = HDATA(NHX+3)/64.
      GATE = T - TX - 1.0
      IF(GATE.LT.0.) GOTO 125
      IF(GATE.GT. 9.) GOTO 122
      BT = GATE
      GOTO 126
  125 BT = 10. + GATE
      IF(BT.LE.0.) GOTO 122
  126 FXT = 1.-BT/5.*(1.-BT/20.)
      IF(GATE.LT.0.) FXT=1.-FXT
      AMP = AMP + AMPX*CTALK(M)*FXT
      DW(J) = DW(J) + AMPX*CTALK(M)
  122 JXT = JXT + 1
      IF(JXT.GT.2) GOTO 124
      K = IXH(J,M)
      IF(K.NE.0) GOTO 121
  124 CONTINUE
      DL(J) = AMP
C --------------------------------------- GAIN
      GAIN=ACALIB(IPN+6+NW(J))
      AMP = AMP * GAIN
C --------------------------------------- WIRE STAGGERING
      IMH=LAND(NL,1)
      IF(IMH.NE.LRFL(J)) AMP=AMP*0.92
C -------------------- COMPUTE TRACK LENGTH IN DRIFTSPACE, ANODE CURRENT
C ----IF(ABS(R).GT.4000. .AND. NC.EQ. NCL) GOTO 24----
      IF(NC.EQ.NCL) GOTO 23
      NCL = NC
      CURR = 0.
      IF(IJETV.EQ.0) GOTO 22
      NS = NC - (NR-1)*24
      IF(NR.EQ.3) NS=NS/2
      KR = NR
      IMH=LAND(NC,1)
      IF(NR.EQ.3 .AND. IMH.EQ.1) KR=4
      ICURR = HDATA(IJETV2+NS*4+KR)
      CURR = LAND(ICURR,2047)
      IF (CURR.GT.80.) CURR=0.
   22 PHIW = 0.1309 + 0.2618*NC
      IF(NC.GE.48) PHIW=PHIW/2.
      CPHW = COS(PHIW)
      SPHW = SIN(PHIW)
C ----CPHD =  SPHW*CALF - CPHW*SALF----
C ----SPHD = -CPHW*CALF - SPHW*SALF----
      CPHD =  SPHW*DRICOS + CPHW*DRISIN
      SPHD = -CPHW*DRICOS + SPHW*DRISIN
   23 RW = 211.*NR + 10.*NL
      XW = RW * CPHW
      YW = RW * SPHW
      SGAM = ((XW-XM)*SPHD-(YW-YM)*CPHD)/R
      IF(ABS(SGAM).GT..87) GOTO 26
      CGAM = SQRT(1.-SGAM**2)
      TGAM = SGAM/CGAM
C --------------------------------------- TRACK LENGTH
   24 AMP = AMP * CGAM*CTH
      DT(J) = TGAM
C --------------------------------------- ANODE CURRENT
      AMP = AMP / (1.-0.00011*CURR**2)
C --------------------------------------- FINITE GATE LENGTH
      AMP = AMP / (1.-0.25*TGAM-0.60*TGAM**2)
C --------------------------------------- SATURATION
      AMP = AMP/(1.-SATTO(INR+1,IPERD)*EXP(-SATTO(INR+2,IPERD)*SQRT(T)))
      AMP = AMP/(1.-SATQUA(NR,IPERD)*T**2/1000.)
      AMP = AMP*9.67/SATFAC(NR,IPERD)
C
C --------------------------------------- GAS PRESSURE
      AMP = AMP * PREFAC
C --------------------------------------- SUBPERIODS
      AMP = AMP * SUBFAC
C --------------------------------------- DAY/4 CORRECTION
      IDAY4=MDAY/4 + (NR-1)*100 + 1
      ADAY4=ACALIB(IPN+1541+IDAY4)
      AMP = AMP * ADAY4
C --------------------------------------- ATTACHMENT
      AMP = AMP * EXP(TIMCOR*T)
C --------------------------------------- GAIN SATURATION
      AMP = AMP/(1.-SATSO(INR+1,IPERD)*EXP(-SATSO(INR+2,IPERD)*
     *                                   SQRT(TTH**2+SATSO(7,IPERD)*T)))
C --------------------------------------- EDGE FIELD DISTORTIONS
      IF(NL.GT.3 .AND. NL.LT.12) GOTO 25
      IL = NL
      IF(NL.GT.8) IL=15-NL
      ICORN = 0
      IF(NL.LT.8 .AND. LRFL(J).EQ.1) ICORN=1
      IF(NL.GT.8 .AND. LRFL(J).EQ.0) ICORN=1
      ICORN = ICORN*4 + IL + 1
      XTORT = XT - 0.5
      IF(XTORT.LT.0.) XTORT=0.
      AMP = AMP * FTORT(1,ICORN)*(1.+FTORT(2,ICORN)*XTORT)
   25 CONTINUE
      IF (NHF.EQ.60) GOTO 30
      NHF = NHF + 1
      G(NHF) = AMP
      S(J) = AMP
      CALL JFXYRZ(KT,NH1,LRFL(J),NSAME,ICEL(J),XYRZ)
      NSAME = 1
      EWIR(NHF) = AMP
      XWIR(NHF) = XYRZ(1)
      YWIR(NHF) = XYRZ(2)
      RWIR(NHF) = XYRZ(5)
      ZWIR(NHF) = XYRZ(6)
      GOTO 29
   26 IZB(J) = 0
   29 CONTINUE
   30 CONTINUE
C
      IF(NHF.LT.NHFCUT) GOTO 58
      CALL DSORT(NHF,G,TRCM,DTRCM)
      IF(TRCM.LT.0.1) GOTO 58
C
C-------------  RESULTS  ----------------------------------------------
C
      ITRES(1,I)=NHF
      TRES(2,I)=TRCM
      TRES(3,I)=2.6*DTRCM
      TRES(9,I)=PMOM
C
      PP2=TRCM
      SIGDE=TRES(3,I)
C
       CHMIN=1000000.
       SIGPP=1000000.
       JMIN=0
      CALL ERRMOM(KT,PMOM,ELMASS,SIGMP)
      CALL DECHIF(PMOM,SIGMP,PP2,SIGDE,ELMASS,PDEELC,DEDELC,CHIELC)
       IF(CHMIN .LT. CHIELC) GOTO 250
       CHMIN=CHIELC
       SIGPP=SIGMP
       DEBEST=DEDELC
       PBEST=PDEELC
       JMIN=4
250   CALL ERRMOM(KT,PMOM,PIOMAS,SIGMP)
      CALL DECHIF(PMOM,SIGMP,PP2,SIGDE,PIOMAS,PDEPIO,DEDPIO,CHIPIO)
       IF(CHMIN .LT. CHIPIO) GOTO 251
       CHMIN=CHIPIO
       SIGPP=SIGMP
       DEBEST=DEDPIO
       PBEST=PDEPIO
       JMIN=3
251   CALL ERRMOM(KT,PMOM,FKAMAS,SIGMP)
      CALL DECHIF(PMOM,SIGMP,PP2,SIGDE,FKAMAS,PDEKAO,DEDKAO,CHIKAO)
       IF(CHMIN .LT. CHIKAO) GOTO 252
       CHMIN=CHIKAO
       SIGPP=SIGMP
       DEBEST=DEDKAO
       PBEST=PDEKAO
       JMIN=2
252   CALL ERRMOM(KT,PMOM,PROTMA,SIGMP)
      CALL DECHIF(PMOM,SIGMP,PP2,SIGDE,PROTMA,PDEPRO,DEDPRO,CHIPRO)
       IF(CHMIN .LT. CHIPRO) GOTO 253
       CHMIN=CHIPRO
       SIGPP=SIGMP
       DEBEST=DEDPRO
       PBEST=PDEPRO
       JMIN=1
253    TRES(4,I)=CHIELC
       TRES(5,I)=CHIPIO
       TRES(6,I)=CHIKAO
       TRES(7,I)=CHIPRO
       ITRES(8,I)=JMIN
       TRES(10,I)=SIGPP
       GOTO 59
C
   58 CONTINUE
      ITRES(1,I)=-1
      TRES(2,I)=0.
      TRES(3,I)=0.
      TRES(4,I)=-1.
      TRES(5,I)=-1.
      TRES(6,I)=-1.
      TRES(7,I)=-1.
      ITRES(8,I)=0
      TRES(9,I)=PMOM
      TRES(10,I)=1000000.
C
C-------------  END OF RESULTS  ---------------------------------------
C
CCC59 KT = KT + LT
C----
   59 CONTINUE
C----
C
      RETURN
C
   56 PRINT 561
  561 FORMAT ('  ***** ERROR IN DEDXB1: BANK POINTER = 0')
      IER=1000
      NTRR=0
      RETURN
C
   57 PRINT 571
  571 FORMAT ('  ***** ERROR IN DEDXB1: # OF TRACKS < 0 , > 60 , OR GIVE
     *N TRACK NO. > MAX.')
      IER=4000
      NTRR=0
      RETURN
C
   60 NDAY=75 + ID0(IPERD)
      IWARN=IWARN+1
      IF(IWARN.EQ.1) PRINT 601, IRUN
  601 FORMAT(/'  ***********  WARNING: DEDX CALIBRATION CONSTANTS NOT VA
     +LID FOR THESE DATA !!!'/'  ***********  RUN #  :',I10,'  AND LATER
     +'/)
      GOTO 70
C
      END
