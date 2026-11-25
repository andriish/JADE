      SUBROUTINE MUMCEV
C-----------------------------------------------------------------------
C Toy simulation of the muon chambers: makes the raw muon bank MUEV of a
C Monte Carlo event from the true muons of the bank VECT.
C
C The original muon simulation (the job #PRODMU, see JMC/MCNEWS) is not in
C the preserved sources, and the Monte Carlo events have no bank MUEV. This
C routine is called by MUORDR (library JMC) at the end of the tracking of an
C event in MCJADE, before WRTMCB writes the bank, if the control card
C MUMC ON is given. It does the following:
C   - the muons (type 3 of the bank VECT) with a momentum above PMUMIN
C     are followed in the solenoid field (helix) up to the coil, then on a
C     straight line up to the yoke and in the field of the yoke (BYOKE of
C     the calibration) in steps, without multiple scattering and energy
C     loss (a toy),
C   - the straight line is intersected with the plane of each of the 634
C     chambers (geometry of the muon calibration, bank MUCA: frame,
C     orientation, normal coordinate, wire position, limits),
C   - a chamber is hit if the crossing is within the limits of the frame
C     in the long direction and within +-DRIMAX of the wire in the
C     drift direction (efficiency EFFCH),
C   - the drift time and the longitudinal time difference are made with the
C     inverse of the formulae of MUCOOR for Monte Carlo (no non-linear,
C     angular and shift effects): ITD = DTP + 1000*|drift|/VDRIFT and
C     ITL = TLP + 100*(long - centre)/LSFACT, smeared,
C   - the hits are packed in the half-words as MUCOOR reads them: for each
C     crate (3840 + number) the time reference (3584) if there are hits,
C     the chamber/hit words (4*chamber + hit - 1, the hits of a chamber in
C     the order of decreasing hit number) each followed by the drift time
C     and the longitudinal time, and the end-of-crate word 3840.
C The half-words have the order of this machine (as for any Monte Carlo
C bank). The simulation is steered by the control cards MUMC, MUDUMP, MUSMEAR
C and MUSIGN of MCJADE (subroutine MUMCST sets the parameters).
C-----------------------------------------------------------------------
      IMPLICIT INTEGER*2 (H)
#include "cmubcs.for"
#include "cmucalib.for"
      INTEGER MAXC
      PARAMETER (MAXC=634)
      INTEGER NHC(MAXC), ITDC(4,MAXC), ITLC(4,MAXC)
      EXTERNAL MUMCBD
      INTEGER MUGEO(4185), MUGEOB(4185), IW0
      COMMON /MUMCGE/ MUGEO, MUGEOB
      REAL BKGAUS, AVFOO
      COMMON /CGEO1/ BKGAUS, AVFOO(65)
      INTEGER IPV, L0, L1, NV, IPART, J, IPM, NWORDS, NHW
      INTEGER IC, IFR, IOR, ISECT, IX, K, KK, ICR, NHCR, ITYPE, ICH
      INTEGER IER, ISWP, IT, N, IPRT
      REAL B, BT, PX, PY, PZ, P, PT, X0, Y0, Z0, Q, R, DPHI, PXN
      REAL XS, YS, ZS, UX, UY, UZ, S, T, PLN, XP, YP, ZP
      REAL RAND1, GAUSS1, DRIFT, XLONG, WIRE, CEN, CLLO, CLHI, TD, TL
      REAL VDR, DTP, TLP, XLSF, BY, XN, YN, ZN, F0, F1, RCOILI
      REAL RYOKE, DSTEP, RMAXD, ZMAXD, BL, DELTA
      REAL R0, R1, DR
      REAL RY1, RY2, RIRON, DEDX, XRAD, PSTOP, PNEW, TH0, T1, T2,
     +  E1X, E1Y
      REAL E2X, E2Y, E2Z, UN
      INTEGER KSTEP
      REAL PMUMIN, DRIMAX, EFFCH
      LOGICAL LFIRST
      REAL SIGTD, SIGTL, YSIGN, CSIGN
      LOGICAL LDUMP, LACT
      COMMON /MUMCCM/ SIGTD, SIGTL, YSIGN, CSIGN, LDUMP, LACT
      INTEGER*8 ISEED
      DATA PMUMIN /1.5/, DRIMAX /155./, EFFCH /0.95/
      DATA RCOILI /968./, RYOKE /1500./, DSTEP /10./
      DATA RY1 /1760./, RY2 /1910./
      DATA RIRON /1760./, DEDX /1.16E-3/, XRAD /17.6/, PSTOP /0.3/
      DATA RMAXD /4300./, ZMAXD /5000./
      DATA LFIRST /.TRUE./, ISEED /123457/
C-----------------------------------------------------------------------
      IF(.NOT.LACT) RETURN
C the geometry of the block data MUMCBD (MCJADE reads no calibration)
      IF(LFIRST) THEN
C the words of a little-endian host or of a big-endian one (the first byte
C in memory of the integer 1 is 1 on a little-endian host)
        IF(TRANSFER(1,1_1).EQ.1_1) THEN
          DO 1 IW0=1,4185
    1     MUCAL(IW0)=MUGEO(IW0)
        ELSE
          DO 2 IW0=1,4185
    2     MUCAL(IW0)=MUGEOB(IW0)
        ENDIF
      ENDIF
      IF(IZOEP1.EQ.0 .AND. IZOEP3.EQ.0) RETURN
C the bank exists already
      IF(IDATA(IBLN('MUEV')).GT.0) RETURN
C
      IF(LFIRST) THEN
        LFIRST=.FALSE.
        IF(LDUMP) THEN
          WRITE(6,'(A)') ' MUMCEV geometry: ICHAM ICRATE IFRAME IOR'//
     +      ' NORMAL WIRE LONGLO LONGHI TRANSLO TRANSHI VDRIFT DTP'//
     +      ' TLP LSF1 IX'
          WRITE(6,'(A,F10.3,I8)') ' MUMCEV BYOKE IYKIND',BYOKE,IYKIND
          DO 5 IC=1,NCHAMS
            IFR=HFR(IC)
            IOR=HNORM(IFR)
            IX=HOVALL(HUNIT(IFR))
            WRITE(6,'(I5,3I4,2I7,4I7,I6,3I6,I6)') IC,HSECT(IFR),IFR,IOR,
     +        HDIST(IFR)+HD1(IC),HCTW(IC),HCLLO(IFR),HCLHI(IFR),
     +        HCTLO(IFR),HCTHI(IFR),HVDRFT(IC),HDTP(IC),HLTP(IC),
     +        HLSF(1,IC),IX
    5     CONTINUE
        ENDIF
      ENDIF
C-----------------------------------------------------------------------
      DO 10 IC=1,MAXC
        NHC(IC)=0
   10 CONTINUE
C magnetic field (kG) of the event
      B=BKGAUS
      IF(ABS(B).LT.1.) B=-4.85
      BT=0.1*B*CSIGN
C-----------------------------------------------------------------------
C the muons of the bank VECT: px,py,pz,E,m (words 1-5), charge (6), type
C (7), vertex x,y,z (8-10)
      IPV=IDATA(IBLN('VECT'))
      IF(IPV.LE.0) GO TO 200
      L0=IDATA(IPV+1)
      L1=IDATA(IPV+2)
      NV=IDATA(IPV+4)
      DO 100 IPART=1,NV
        J=IPV+L0+(IPART-1)*L1
        ITYPE=IDATA(J+7)
        IF(ITYPE.NE.3) GO TO 100
        PX=ADATA(J+1)
        PY=ADATA(J+2)
        PZ=ADATA(J+3)
        P=SQRT(PX*PX+PY*PY+PZ*PZ)
        IF(P.LT.PMUMIN) GO TO 100
        Q=IDATA(J+6)
        X0=ADATA(J+8)
        Y0=ADATA(J+9)
        Z0=ADATA(J+10)
        PT=SQRT(PX*PX+PY*PY)
C The path: helix in the solenoid up to the inner radius of the coil
C (RCOILI), a straight line up to the yoke (RYOKE), then a helix in the
C field of the iron yoke (BYOKE, along z, opposite to the solenoid field),
C in steps, and the planes of the chambers are looked for in each step.
C The change of the angle is  dphi = -q*0.3*Bz(T)*ds(m)/pT.
        XS=X0
        YS=Y0
        ZS=Z0
        UX=PX/P
        UY=PY/P
        UZ=PZ/P
        IF(PT.GT.1.E-3) THEN
          DPHI=-Q*0.3*BT/PT*0.02
          DO 20 K=1,200
            IF(SQRT(XS*XS+YS*YS).GE.RCOILI) GO TO 21
            XS=XS+UX*20.
            YS=YS+UY*20.
            ZS=ZS+UZ*20.
            PXN=PX*COS(DPHI)-PY*SIN(DPHI)
            PY=PX*SIN(DPHI)+PY*COS(DPHI)
            PX=PXN
            UX=PX/P
            UY=PY/P
   20     CONTINUE
   21     CONTINUE
        ENDIF
C straight line up to the yoke
        DO 22 K=1,200
          IF(SQRT(XS*XS+YS*YS).GE.RYOKE) GO TO 23
          XS=XS+UX*20.
          YS=YS+UY*20.
          ZS=ZS+UZ*20.
   22   CONTINUE
   23   CONTINUE
C in the yoke: steps of DSTEP mm. The bend in the yoke is that of
C MUFFLE: the integral of B*dl over the yoke is BL = B(coil)*pi*R**2/14680
C (flux of the coil divided by the perimeter, T*mm), the change of the
C direction CON/P with CON = 0.3E-3*BL*charge; here it is spread over the
C radii RY1 to RY2
        BL=ABS(B)*0.1*3.14159*1006.5**2/14680.
        DELTA=-YSIGN*SIGN(1.,B)*0.3E-3*BL*Q/P
        DO 90 KSTEP=1,1000
          R0=SQRT(XS*XS+YS*YS)
          XN=XS+UX*DSTEP
          YN=YS+UY*DSTEP
          ZN=ZS+UZ*DSTEP
C the chambers: crossing of the plane between the two points of the step
          DO 50 IC=1,NCHAMS
            IFR=HFR(IC)
            IOR=HNORM(IFR)
            IX=HOVALL(HUNIT(IFR))
            PLN=HDIST(IFR)+HD1(IC)
            WIRE=HCTW(IC)
            IF(IOR.EQ.1) THEN
              F0=XS-(PLN+IX)
              F1=XN-(PLN+IX)
            ELSEIF(IOR.EQ.2) THEN
              F0=YS-PLN
              F1=YN-PLN
            ELSE
              F0=ZS-PLN
              F1=ZN-PLN
            ENDIF
            IF(F0*F1.GT.0. .OR. F0.EQ.F1) GO TO 50
            T=F0/(F0-F1)
            XP=XS+(XN-XS)*T
            YP=YS+(YN-YS)*T
            ZP=ZS+(ZN-ZS)*T
            IF(IOR.EQ.1) THEN
              DRIFT=YP-WIRE
              XLONG=ZP
            ELSEIF(IOR.EQ.2) THEN
              DRIFT=XP-IX-WIRE
              XLONG=ZP
            ELSE
              DRIFT=XP-IX-WIRE
              XLONG=YP
            ENDIF
            CLLO=HCLLO(IFR)
            CLHI=HCLHI(IFR)
            IF(XLONG.LT.CLLO .OR. XLONG.GT.CLHI) GO TO 50
            IF(ABS(DRIFT).GT.DRIMAX) GO TO 50
            IF(NHC(IC).GE.4) GO TO 50
C efficiency of the chamber
            ISEED=MOD(ISEED*69069_8+1_8,2147483647_8)
            IF(FLOAT(ISEED)/2147483647..GT.EFFCH) GO TO 50
C times (inverse of MUCOOR for Monte Carlo)
            VDR=HVDRFT(IC)
            DTP=HDTP(IC)/10.
            TLP=HLTP(IC)
            XLSF=HLSF(1,IC)
            CEN=0.5*(CLLO+CLHI)
            TD=DTP+1000.*ABS(DRIFT)/VDR
            IF(XLSF.NE.0.) THEN
              TL=TLP+100.*(XLONG-CEN)/XLSF
            ELSE
              TL=TLP
            ENDIF
C smearing
            TD=TD+SIGTD*GAUSS1(ISEED)
            TL=TL+SIGTL*GAUSS1(ISEED)
            IT=NINT(TD)
            IF(IT.LT.1) IT=1
            IF(IT.GT.250) GO TO 50
            KK=NINT(TL)
            IF(KK.LT.4) KK=4
            IF(KK.GT.255) KK=255
            NHC(IC)=NHC(IC)+1
            ITDC(NHC(IC),IC)=IT
            ITLC(NHC(IC),IC)=KK
   50     CONTINUE
C next step: rotation of the direction in the yoke field
          XS=XN
          YS=YN
          ZS=ZN
          R1=SQRT(XS*XS+YS*YS)
C the iron (a cylinder of soft iron from the yoke outwards): energy loss
C and multiple scattering in each step; the muon stops below PSTOP
          IF(R0.GE.RIRON) THEN
            PNEW=P-DEDX*DSTEP
            IF(PNEW.LT.PSTOP) GO TO 100
            P=PNEW
            PX=UX*P
            PY=UY*P
            PZ=UZ*P
            TH0=13.6E-3/P*SQRT(DSTEP/XRAD)
            T1=TH0*GAUSS1(ISEED)
            T2=TH0*GAUSS1(ISEED)
            E1X=-UY
            E1Y=UX
            UN=SQRT(E1X*E1X+E1Y*E1Y)
            IF(UN.GT.1.E-6) THEN
              E1X=E1X/UN
              E1Y=E1Y/UN
              E2X=-UZ*E1Y
              E2Y=UZ*E1X
              E2Z=UX*E1Y-UY*E1X
              UX=UX+T1*E1X+T2*E2X
              UY=UY+T1*E1Y+T2*E2Y
              UZ=UZ+T2*E2Z
              UN=SQRT(UX*UX+UY*UY+UZ*UZ)
              UX=UX/UN
              UY=UY/UN
              UZ=UZ/UN
              PX=UX*P
              PY=UY*P
              PZ=UZ*P
            ENDIF
          ENDIF
          DR=MAX(0.,MIN(R1,RY2)-MAX(R0,RY1))
          IF(DR.GT.0.) THEN
            DELTA=-YSIGN*SIGN(1.,B)*0.3E-3*BL*Q/P
            DPHI=DELTA*DR/(RY2-RY1)
            PXN=PX*COS(DPHI)-PY*SIN(DPHI)
            PY=PX*SIN(DPHI)+PY*COS(DPHI)
            PX=PXN
            UX=PX/P
            UY=PY/P
          ENDIF
          IF(SQRT(XS*XS+YS*YS).GT.RMAXD .OR. ABS(ZS).GT.ZMAXD) GO TO 100
   90   CONTINUE
  100 CONTINUE
C-----------------------------------------------------------------------
C the hits of a chamber in the order of the time: hit 1 is the earliest
  200 CONTINUE
      DO 210 IC=1,NCHAMS
        N=NHC(IC)
        IF(N.LT.2) GO TO 210
        DO 205 K=1,N-1
          DO 204 KK=K+1,N
            IF(ITDC(KK,IC).LT.ITDC(K,IC)) THEN
              IT=ITDC(K,IC)
              ITDC(K,IC)=ITDC(KK,IC)
              ITDC(KK,IC)=IT
              IT=ITLC(K,IC)
              ITLC(K,IC)=ITLC(KK,IC)
              ITLC(KK,IC)=IT
            ENDIF
  204     CONTINUE
  205   CONTINUE
  210 CONTINUE
C number of half-words: 2 (descriptor) + for each crate the crate word,
C the end-of-crate word, the time reference if there are hits and 3 per
C hit (one more 0 if the number is odd)
      NHW=2
      DO 221 ICR=1,14
        NHCR=0
        DO 220 IC=1,NCHAMS
          IF(HSECT(HFR(IC)).EQ.ICR) NHCR=NHCR+NHC(IC)
  220   CONTINUE
        NHW=NHW+2+3*NHCR
        IF(NHCR.GT.0) NHW=NHW+1
  221 CONTINUE
      NWORDS=(NHW+1)/2
      CALL CCRE(IPM,'MUEV',0,NWORDS,IER)
      IF(IER.NE.0) RETURN
      CALL BSAW(1,'MUEV')
C descriptor: 1 (not 0, which MUCOOR takes for the old Monte Carlo) and 0
      K=2*IPM
      HDATA(K+1)=1
      HDATA(K+2)=0
      K=K+2
      DO 300 ICR=1,14
        K=K+1
        HDATA(K)=3840+ICR
        NHCR=0
        DO 230 IC=1,NCHAMS
          IF(HSECT(HFR(IC)).EQ.ICR) NHCR=NHCR+NHC(IC)
  230   CONTINUE
        IF(NHCR.GT.0) THEN
          K=K+1
          HDATA(K)=3584
          DO 250 IC=1,NCHAMS
            IF(HSECT(HFR(IC)).NE.ICR) GO TO 250
            DO 240 KK=NHC(IC),1,-1
              HDATA(K+1)=4*IC+KK-1
              HDATA(K+2)=ITDC(KK,IC)
              HDATA(K+3)=ITLC(KK,IC)
              K=K+3
  240       CONTINUE
  250     CONTINUE
        ENDIF
        K=K+1
        HDATA(K)=3840
  300 CONTINUE
C the last word is 0 (or 3840)
      DO 310 KK=K+1,2*IPM+2*NWORDS
        HDATA(KK)=0
  310 CONTINUE
      RETURN
      END
C-----------------------------------------------------------------------
      REAL FUNCTION GAUSS1(ISEED)
C Gaussian random number (sum of 12 uniform numbers) for MUMCEV
      INTEGER*8 ISEED
      INTEGER I
      REAL S
      S=-6.
      DO 10 I=1,12
        ISEED=MOD(ISEED*69069_8+1_8,2147483647_8)
        S=S+FLOAT(ISEED)/2147483647.
   10 CONTINUE
      GAUSS1=S
      RETURN
      END
C-----------------------------------------------------------------------
      SUBROUTINE MUMCST(LON,LDMP,SD,SL,YS,CS)
C Sets the parameters of the toy simulation of the muon chambers (the
C control cards of SUPERV): LON switches the simulation on, LDMP prints
C the geometry once, SD and SL are the smearing of the drift time and of
C the longitudinal time (clock pulses), YS and CS the signs of the yoke
C and of the solenoid field (1. or -1.).
C-----------------------------------------------------------------------
      LOGICAL LON, LDMP
      REAL SD, SL, YS, CS
      REAL SIGTD, SIGTL, YSIGN, CSIGN
      LOGICAL LDUMP, LACT
      COMMON /MUMCCM/ SIGTD, SIGTL, YSIGN, CSIGN, LDUMP, LACT
      LACT=LON
      LDUMP=LDMP
      SIGTD=SD
      SIGTL=SL
      YSIGN=YS
      CSIGN=CS
      RETURN
      END
