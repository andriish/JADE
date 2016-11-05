C   09/11/84            MEMBER NAME  TRKADC   (S)           FORTRAN
C
C-----------------------------------------------------------------------
      SUBROUTINE TRKADC(PM,R,R0,STOPL,*)
C-----------------------------------------------------------------------
C
C WRITTEN BY K.-H. HELLENBRAND ON 24.JAN.1984
C THIS ROUTINE DECAYS K+- AND K0L IN THE TRACKING PROGRAMM
C K+ --> MU+ NU        63.5%      K0L --> PI+ E- NU    19.35%
C    --> PI+ PI0       21.2%          --> PI- E+ NU    19.35%
C    --> PI+ PI+ PI-    5.6%          --> PI+ MU- NU   13.55%
C    --> PI+ PI0 PI0    1.7%          --> PI- MU+ NU   13.55%
C    --> PI0 MU+ NU     3.2%          --> PI+ PI- PI0  12.6%
C    --> PI0 E+ NU      4.8%          --> PI+ PI-      21.6%
C IT WORKS SIMILAR TO THE ROUTINE 'LUDECY' IN THE LUND PROGRAMM
C IT IS CALLED BY 'TRCDET' FOR K+-
C          AND BY 'KTRNTR' FOR K0L                      R(1...3) VERTEX
C PK(1...4) 4 VECTOR, PK(5) MASS, PK(6) |P|, PK(7) CHARGE, PK(8) TYP
C PARTICLE CODE (ONLY IN TRKADC) 1 NU, 2 E, 3 MU, 4 PI+-, 5 PI0, 6 GAM
C
C-----------------------------------------------------------------------
C
      COMMON / CJTRLE / TOTLEN, STPLEN, TRCOFS
      DIMENSION PM(10),PK(10),R(3),R0(3),PKD(10,10),IPKD(10,10)
      DIMENSION CKBR(12),KKDP(36),UKMAS(5)
      DIMENSION PV(5,5),RORD(5),U(3),BE(3)
      DIMENSION AMLEP2(4), AMK2(4), AMPI2(4)
      DIMENSION ALAMPL(4), ALAM(4), AFM(4)
      DIMENSION WTKA(4)
      EQUIVALENCE (PKD(1,1),IPKD(1,1))
C
      DATA CKBR/0.1935,0.387,0.5225,0.658,0.784,1.,0.635,0.847,0.903,
     +          0.92,0.952,1./
      DATA KKDP/1001,2,-4,-1001,-2,4,2001,3,-4,-2001,-3,4,4,-4,4*5,
     +          1,3,0,4,5,0,4,4,-4,4,5,5,3001,3,5,4001,2,5/
      DATA UKMAS/0.,0.0005,0.106,0.14,0.135/
      DATA AMLEP2/0.0,2*0.011164,0.0/
      DATA AMK2/2*0.24768,2*0.2437/
      DATA AMPI2/2*0.019479,2*0.018215/
      DATA ALAMPL/0.03,0.034,0.032,0.0285/
      DATA ALAM/0.0,-0.009,-0.028,0.0/
      DATA AFM/0.0,24.549,26.1022,0.0/
      DATA WTKA/5.8,6.2,6.6,6.5/
C
C...FUNCTIONS : MOMENTUM IN TWO-PARTICLE DECAYS AND FOUR-PRODUCT
      PAWT(A,B,C)=SQRT((A**2-(B+C)**2)*(A**2-(B-C)**2))/(2.*A)
      FOUR(I,J)=PKD(I,4)*PKD(J,4)-PKD(I,1)*PKD(J,1)-PKD(I,2)*PKD(J,2)
     +                                             -PKD(I,3)*PKD(J,3)
C
C------------------  C O D E  ------------------------------------------
C
C
      DO 80 I=1,10
   80 PKD(10,I)=PM(I)
C
C..........................CHOOSE DECAY CHANNEL
      KFA=ABS(PM(8))
      KFS=PM(7)
       IKDB=7
      IF(ABS(PM(7)).LT.1.E-3) IKDB=1
  100 RBR=RN(0)
      IDC=IKDB-1
  110 IDC=IDC+1
      IF(RBR.GT.CKBR(IDC)) GOTO 110
      IDC3=3*IDC-2
C
C........START READOUT OF DECAY CHANNEL: MATRIX ELEMENT, RESET COUNTERS
      MMAT=IABS(KKDP(IDC3))/1000
      NP=0
      PS=0.
      PSQ=0.
C........................READ OUT DECAY PRODUCTS
      I1A=3*IDC-2
      I1E=3*IDC
      DO 130 I1=I1A,I1E
      KP=MOD(KKDP(I1),1000)
      IF(KP.EQ.0) GOTO 130
      KFP=KFS*KP
      IF(KFS.EQ.0) KFP=KP
      KPA=IABS(KFP)
C........................PUT DECAY PRODUCTS INTO PKD
      NP=NP+1
      IPKD(NP,7)=ISIGN(1,KFP)
      IF(IABS(KFP).EQ.5) IPKD(NP,7)=0
      IF(IABS(KFP).EQ.1) IPKD(NP,7)=0
      IPKD(NP,8)=KPA
      PKD(NP,5)=UKMAS(KPA)
      PS=PS+PKD(NP,5)
  130 CONTINUE
C.................CALCULATE MAXIMUM WEIGHT NP-PARTICLE DECAY
      DO 210 J=1,5
  210 PV(1,J)=PM(J)
      PV(NP,5)=PKD(NP,5)
      IF(NP.EQ.2) GOTO 270
      WTMAX=1./2.
      PMAX=PV(1,5)-PS+PKD(NP,5)
      PMIN=0.
      NP22=NP-1
      DO 220 ILN=1,NP22
C     DO 220 INL=NP-1,1,-1
      INL=NP-ILN
      PMAX=PMAX+PKD(INL,5)
      PMIN=PMIN+PKD(INL+1,5)
  220 WTMAX=WTMAX*PAWT(PMAX,PMIN,PKD(INL,5))
C
C...M-GENERATOR GIVES WEIGHT, IF REJECTED TRY AGAIN
  230 RORD(1)=1.
      NP25=NP-1
      DO 250 IL1=2,NP25
      RSAV=RN(0)
      IL24=IL1-1
      DO 240 I2L=1,IL24
C     DO 240 IL2=IL1-1,1,-1
      IL2=IL1-I2L
      IF(RSAV.LE.RORD(IL2)) GOTO 250
  240 RORD(IL2+1)=RORD(IL2)
  250 RORD(IL2+1)=RSAV
      RORD(NP)=0.
      WT=1.
      IL26=NP-1
      DO 260 LI=1,IL26
C     DO 260 IL=NP-1,1,-1
      IL=NP-LI
      PV(IL,5)=PV(IL+1,5)+PKD(IL,5)+(RORD(IL)-RORD(IL+1))*(PM(5)-PS)
  260 WT=WT*PAWT(PV(IL,5),PV(IL+1,5),PKD(IL,5))
      IF(WT.LT.RN(0)*WTMAX) GOTO 230
C
C...PERFORM TWO-PARTICLE DECAYS IN RESPECTIVE CM FRAME
  270 IL29=NP-1
      DO 290 IL=1,IL29
      PA=PAWT(PV(IL,5),PV(IL+1,5),PKD(IL,5))
      U(3)=2.*RN(0)-1.
      PHI=6.283*RN(0)
      U(1)=SQRT(1.-U(3)**2)*COS(PHI)
      U(2)=SQRT(1.-U(3)**2)*SIN(PHI)
      DO 280 J=1,3
      PKD(IL,J)=PA*U(J)
  280 PV(IL+1,J)=-PA*U(J)
      PKD(IL,4)=SQRT(PA**2+PKD(IL,5)**2)
  290 PV(IL+1,4)=SQRT(PA**2+PV(IL+1,5)**2)
C
C...LORENTZ TRANSFORM DECAY PRODUCTS TO LAB FRAME
      DO 300 J=1,4
  300 PKD(NP,J)=PV(NP,J)
      IL33=NP-1
      DO 330 LI=1,IL33
C     DO 330 IL=NP-1,1,-1
      IL=NP-LI
      DO 310 J=1,3
  310 BE(J)=PV(IL,J)/PV(IL,4)
      GA=PV(IL,4)/PV(IL,5)
      DO 330 I=IL,NP
      BEP=BE(1)*PKD(I,1)+BE(2)*PKD(I,2)+BE(3)*PKD(I,3)
      DO 320 J=1,3
  320 PKD(I,J)=PKD(I,J)+GA*(GA*BEP/(1.+GA)+PKD(I,4))*BE(J)
  330 PKD(I,4)=GA*(PKD(I,4)+BEP)
C
         IF(MMAT.LT.1) GOTO 340
C.............MATRIX ELEMENTS FOR K+- AND K0L --> PI + E + NU
         IK=MMAT
         TFO=(PM(4)-PKD(3,4))**2
         TFO=TFO-(PM(1)-PKD(3,1))**2
         TFO=TFO-(PM(2)-PKD(3,2))**2
         TFO=TFO-(PM(3)-PKD(3,3))**2
         ALAK=ALAM(IK)
         AMPK=AFM(IK)
         FPL=1.+ALAMPL(IK)*TFO/AMPI2(IK)
         FPL2=FPL**2
C        FMI=ALAM(IK)*AFM(IK)
C        FMI2=FMI**2
         FMI=ALAK*AMPK
         FMI2=FMI**2
         WTA=2*FOUR(10,1)*FOUR(10,2)
         WTA=WTA+(AMLEP2(IK)/4.-AMK2(IK))*FOUR(1,2)
         WTA=WTA-AMLEP2(IK)*FOUR(10,1)
         WTB=AMLEP2(IK)*(FOUR(10,1)-FOUR(1,2)/2.)
         WTC=AMLEP2(IK)*FOUR(1,2)/4.
         WT=FPL2*WTA+FPL*FMI*WTB+FMI2*WTC
         WT=WT*1000.
         IF(WT.LT.RN(0)*WTKA(IK)) GOTO 230
C
C....................DECAY OF PI0'S FROM K-DECAY
  340 NPK=NP
      DO 400 I=1,NPK
      IF(IPKD(I,8).NE.5) GOTO 400
      U(3)=2.*RN(0)-1.
      PHI=6.283*RN(0)
      U(1)=SQRT(1.-U(3)**2)*COS(PHI)
      U(2)=SQRT(1.-U(3)**2)*SIN(PHI)
      GA=PKD(I,4)/PKD(I,5)
      DO 405 J=1,3
  405 BE(J)=PKD(I,J)/PKD(I,4)
C....................FILL PKD(NP,J) FOR GAMMAS
      DO 410 INP=1,2
      NP=NP+1
      FVOR=(-1.)**INP
      PKD(NP,4)=PKD(I,5)/2.
      PKD(NP,5)=0.
      PKD(NP,6)=0.
      DO 420 IKO=1,3
  420 PKD(NP,IKO)=FVOR*PKD(NP,4)*U(IKO)
      IPKD(NP,7)=0
      IPKD(NP,8)=6
C....................LORENTZ TRANSFORMATION OF GAMMAS INTO LAB SYSTEM
      BEP=BE(1)*PKD(NP,1)+BE(2)*PKD(NP,2)+BE(3)*PKD(NP,3)
      DO 430 J=1,3
  430 PKD(NP,J)=PKD(NP,J)+GA*(GA*BEP/(1.+GA)+PKD(NP,4))*BE(J)
      PKD(NP,4)=GA*(PKD(NP,4)+BEP)
  410 CONTINUE
  400 CONTINUE
C
C........................FILL DECAY PRODUCTS INTO 'VECT',1
            TRCOFS = STOPL + SQRT(R0(1)**2 + R0(2)**2 + R0(3)**2)
      DO 500 I=1,NP
      IF(IPKD(I,8).EQ.1) GOTO 500
      IF(IPKD(I,8).EQ.5) GOTO 500
      DO 510 IKO=1,5
  510 PK(IKO)=PKD(I,IKO)
      PK(6)=0.
      PK(7)=FLOAT(IPKD(I,7))
      PK(8)=FLOAT(IPKD(I,8))
      IF(IPKD(I,8).EQ.6) PK(8)=1.
      PK(9)=0.
      PK(10)=0.
      CALL SVECT1(PK,R)
  500 CONTINUE
      RETURN 1
      END
