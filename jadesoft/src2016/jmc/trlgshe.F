C   20/01/83 609041900  MEMBER NAME  TRLGSHE  (S)           FORTRAN
      SUBROUTINE TRLGSH(PV,R,*)
C
C      THIS ROUTINE TRACKS PARTICLES THROUGH THE LEAD GLASS
C      ARRAYS AND ACCUMULATES PULSEHIGHTS
C      PV = INCOMING 4 VECTOR IS CHANGED DUE TO ENERGY LOSS
C      R  = 3 VECTOR POINTING TO THE FACE OF THE LEAD GLASS
C      RETURN 1 IS USED IF THE INCIDENT MOMENTUM IS LESS THAN
C               10 MEV
C
C      CHANGE       04/02/83    K.MEIER
C      CHANGE OF EXPECM : SHOWER PARAMETER FROM EGS3-MC
C                       : NEW NORMALISATION OF SHOWER FUNCTIONS
C                       : RADIATION LENGTH RADL = 23.9 MM
C                   25/10/85    N.MAGNUSSEN
C      THIS IS ALMOST IDENTICAL TO TRLG1 BUT THE 3-DIMENSIONAL
C      SHOWER INTEGRATION IS DONE ONLY FOR ELECTRONS AND GAMMAS
C      WITH E > 100 MEV (STANDARD PROCEDURE IS USED FOR LOW ENERGIES)
C      CHANGE       20/09/83    K.MEIER
C      ERROR IN CHARGE CALCULATION CORRECTED
C
C   PRINT STATEMENT COMMENTED OUT    11.6.1986   J.OLSSON
C   NBLOCK=0 STATEMENT REMOVED    02.7.1986     20.12  J.OLSSON
C   RATIO ESEEN/ENERGY CORRECTED  03.7.1986     19.30  J.OLSSON
C    ERROR IN NOT UPDATING R-VECTOR FOR ENDCAP PHOTONS, LEADING TO
C    DOUBLE TRACKING OF ENDCAP PHOTONS AND ELECTRONS, CORRECTED.
C    LIKEWISE THE SCALING OF BLOCK ENERGIES DEPENDENT ON READOUT
C    THRESHOLD REMOVED, SINCE THIS IS HANDLED CORRECTLY IN RDALGN
C                                  20.08.1986     J.OLSSON
C    TEST VERSION WITH OLD ERRORS IN AGAIN....   4.9.86  J.OLSSON
C
      COMMON/CGEO1/BKGAUS, RPIP,DRPIP,XRLPIP, RBPC,DRBPC,XRLBPC,
     *             RITNK,DRITNK,XRLTKI, R0ROH,DR0ROH,XR0ROH,
     *             R1ROH,DR1ROH,XR1ROH, R2ROH,DR2ROH,XR2ROH,
     *             R3ROH,DR3ROH,XR3ROH, ROTNK,DROTNK,XRLTKO,
     *             RTOF,DRTOF,XRTOF, RCOIL, DRCOIL, XRCOIL,
     *             ZJM,DZJM,XRZJM, ZJP,DZJP,XRZJP,
     *             ZTKM,DZTKM,XRZTKM, ZTKP,DZTKP,XRZTKP,
     *             ZBPPL,ZBPMI,ZTOFPL,ZTOFMI,
     *             XRJETC,
     *             RLG,ZLGPL,ZLGMI,OUTR2,CTLIMP,CTLIMM,DELFI,
     *             BLXY,BLZ,BLDEP,ZENDPL,ZENDMI,DEPEND,
     *             XHOL1,XHOL2,YHOL1,YHOL2,BLFI
CAV     *             XHOL1,XHOL2,YHOL1,YHOL2
CAV  Same size required
      COMMON/CLGHIT/AMPLG(3000)
      DIMENSION RR(3),LISTBL(30),BLCFRC(30),VAR(2),DC(3)
      LOGICAL * 1 LFLAG
      COMMON/CFLAG/LFLAG(10)
C        LFLAG(1) = SMEAR GAMMA AND ELECTRON ENERGIES
C        LFLAG(2) = GAMMA CONVERSION IN OUTER TANK AND COIL (TRKGAM)
C        LFLAG(3) = ABSORPTION LOSSES
C
      DATA TWOPI/6.28319/
      REAL RADL/23.90/
      DIMENSION R(1),PV(1)
      DIMENSION R0(3)
      DATA ICALL /0/
C
      IF(ICALL.EQ.1) GOTO 1111
      ICALL = 1
      WRITE(6,1113)
1113  FORMAT(' TRLGSH, VERSION WITH ERRORS...  4.9.86 ')
C
C      FIND INDEX FOR ENERGY LOSS
C       LOSS = 0  PHOTON
C       LOSS = 1  ELECTRON
C       LOSS = 2  MINIMUM IONIZING PARTICLE
C
 1111 IF(PV(6).LT.0.01) RETURN 1
C
      OUTR=OUTR2*OUTR2
      ELOSS=0.
      ELS  =0.
      EDEPOT = 0.
      LOSS=2
      IF(PV(5).LT.1.E-03) LOSS=1
      IF(PV(5).EQ.0.)     LOSS=0
C
C                                           ENERGIES IN MEV
      ENERGY=PV(4)*1000.
      EINC = ENERGY / 1000.
C
C                                           THE DIRECTION OF THE TRACKIN
C                                           VECTOR IS NOT CHANGED
C                                           WHEN TRACKED THROUGH THE
C                                           BLOCKS
      PV(6)=SQRT(PV(1)**2+PV(2)**2+PV(3)**2)
      PVX=PV(1)/PV(6)
      PVY=PV(2)/PV(6)
      PVZ=PV(3)/PV(6)
      PVXY=PVX**2+PVY**2
      IF(PVXY.LT.1.E-5) RETURN
      DC(1) = PVX
      DC(2) = PVY
      DC(3) = PVZ
      X1=0.
      R1=R(1)**2+R(2)**2
      RADIUS = SQRT( R1 )
      IF( .NOT.
     *  ( ZLGMI+5..LE.R(3).AND.R(3).LE.ZLGPL-5..AND.RADIUS.GT.RLG-5. ))
     *                             GO TO 1000
C
C                                           CENTRAL PART
C
C
C                                           ANTI ENERGY CORRECTION
C                                           ABSORPTION IN COIL
      IBE = 0
      IF( LOSS .LT.2 .AND. LFLAG(3) ) ENERGY=ELGOBS(IBE,DC,EINC)*1000.
C                                           ENERGY SMEARING
      ENERGY = AMAX1( ENERGY, 1. )
      EINC = ENERGY/1000.
      IF( LOSS .LT.2 .AND. LFLAG(1) ) ENERGY=ELSMR(IBE,DC,EINC)*1000.
C
C                                           CALCULATE THICKNESS OF SLAB
C                                           OF LEAD GLASS CORRESPONING
C                                           TO 1CM TRACK LENGTH
C
      PVR=R(1)*PVX+R(2)*PVY
      DELR=PVR/RADIUS
      TRKL=BLDEP/DELR
C
C                                           DETERMINE STEP SIZE
      NSTEP=BLDEP/(10.*DELR)+1
      XL=10.
      DX=XL/RADL
C
C                                           ENERGY LOSS FOR HADRONS
      IF( LOSS.LT.2.AND.ENERGY.GT.100.) GOTO 500
      IF( LOSS.LT.2 ) GO TO 26
C
C     CALCULATE EXITIN POINT AT THE BACK SURFACE OF THE LEAD GLASS
C
      R0(1)=R(1)+PVX*TRKL
      R0(2)=R(2)+PVY*TRKL
      R0(3)=R(3)+PVZ*TRKL
C
C
C     CALCULATE PATH LENGTH THROUGH LIGHT GUIDE (IFLG = 2)
C
      IFLG = 2
      CALL PLLIGD(PLLI,R(1),R(2),R(3),R0(1),R0(2),R0(3),IFLG)
      ICHG=IFIX(PV(7))
      ITYP=IFIX(PV(8))
      CALL HDECLS(EDEPOT,ICHG,ITYP,PV,IBE,TRKL,PLLI)
      ED=EDEPOT
      EDEPOT=10.*EDEPOT/TRKL*1000.
C     WRITE(6,600) ICHG,ITYP,PV(4),IBE,TRKL,PLLI,ED,EDEPOT
  600 FORMAT(1H ,2I3,F8.3,I5,2F8.2,2X,2F8.3)
   26 CONTINUE
      N0 = 1
C
C                                           DEPTH OF CONVERSION
C                                           FOR PHOTONS IN THE LEAD GLAS
C                                           ASSUME UNIFORM CONVERSION
C                                           PROBABILITY WITHIN
C                                           THE FIRST 2 X0
      IF(LOSS.NE.0) GO TO 23
      X0=51.*RN(DUM)
      N0=X0/(10.*DELR)+1
   23 CONTINUE
C
      ISTEP=0
   21 ISTEP=ISTEP+1
C
C                                           CALCULATE INDEX FOR BLOCKS
C                                           WHICH ARE HIT
      PHI=ATAN2(R(2),R(1))
      IF(PHI.LT.0.) PHI=TWOPI+PHI
      NFI=PHI/DELFI
      IF(ABS(R(3)).GT.ZLGPL) GO TO 22
      DZ=R(3)-ZLGMI
      NZ=DZ/BLZ+1
      NBL=NZ+32*NFI
C
C                                           ACCUMULATE PULSEHEIGHTS
C                                           IN BLOCKS
      IF(LOSS.GE.2) GO TO 25
C                                           PHOTONS AND E
      EDEPOT=0.
      IF(ISTEP.LT.N0) GO TO 24
      X1=X1+DX
      EDEPOT=SHOWR(ENERGY,X1,DX)
      IF(X1.GT.9.) EDEPOT=ENERGY-ELOSS
      ELOSS=ELOSS+EDEPOT
      IF(ELOSS.GT.ENERGY) GO TO 24
      AMPLG(NBL)=AMPLG(NBL)+EDEPOT
      IF(X1.GT.9.) RETURN
      GO TO 24
C
C                                           HADRONS
   25 AMPLG(NBL)=AMPLG(NBL)+EDEPOT
      ELS=ELS+EDEPOT
      IF(ELS.GE.ENERGY) GO TO 22
C
C                                           PROPAGATE TRACK
   24 R(1)=R(1)+XL*PVX
      R(2)=R(2)+XL*PVY
      R(3)=R(3)+XL*PVZ
C
C                                           CHECK WHETHER PARTICLE IS
C                                           STILL IN THE LEAD GLASS
      R1=R(1)**2+R(2)**2
      IF(R1.GT.OUTR) GO TO 22
      IF(ZLGPL.LT.R(3).OR.R(3).LT.ZLGMI) GO TO 22
      GO TO 21
   22 CONTINUE
      IF(ELOSS.EQ.0.) RETURN
      RX=(PV(4)-ELOSS/2000.)/PV(4)
      IF(ELOSS.GE.ENERGY) RX=0.001
      IF(ELS  .GE.ENERGY) RX=0.001
      RX=ABS(RX)
      DO 1004 I4=1,4
 1004 PV(I4)=PV(I4)*RX
      PV(6)=PV(6)*RX
      RETURN
C
C                                           END CAP LEAD GLASS
C                                           FOLLOW PHOTON TO THE FACE
C                                           OF THE END CAP COUNTERS
 1000 CONTINUE
      RABS=0.
      DO 1001 I3=1,3
 1001 RABS=RABS+R(I3)**2
      RABS=SQRT(RABS)
      DO 1002 I3=1,3
 1002 R(I3)=R(I3)/RABS
      DISTZ=ZENDPL
      IF(R(3).LT.0) DISTZ=ABS(ZENDMI)
      ZLAM=ABS(DISTZ/R(3))
      DO 1003 I3=1,3
 1003 R(I3)=ZLAM*R(I3)
      CALL ENDCLG(R,NBL,*9000)
C
C                                           ANTI ENERGY CORRECTION
C                                           ABSORPTION IN COIL
      IBE = 1
      IF( R(3) .LT. 0. ) IBE = -1
      IF( LOSS .LT.2 .AND. LFLAG(3) ) ENERGY=ELGOBS(IBE,DC,EINC)*1000.
C                                           ENERGY SMEARING
      ENERGY = AMAX1( ENERGY, 1. )
      EINC = ENERGY/1000.
      IF( LOSS .LT.2 .AND. LFLAG(1) ) ENERGY=ELSMR(IBE,DC,EINC)*1000.
C
C                                           CALCULATE DELZ OF SLAB
C                                           OF LEAD GLASS CORRESPONDING
C                                           1CM TRACK LENGTH IN LEAD GLA
      DELZ=ABS(PVZ*10.)
      TRKL=DEPEND/ABS(PVZ)
C
C                                           DETERMINE STEP SIZE
      NSTEP=DEPEND/DELZ +1
      XL=10.
      DX=XL/RADL
C
C                                           ENERGY LOSS FOR HADRONS
      IF( LOSS.LT.2.AND.ENERGY.GT.100.) GOTO 700
      IF( LOSS.LT.2 ) GO TO 260
      PLLI=0.
      ICHG=IFIX(PV(7))
      ITYP=IFIX(PV(8))
      CALL HDECLS(EDEPOT,ICHG,ITYP,PV,IBE,TRKL,PLLI)
      ED=EDEPOT
      EDEPOT=10.*EDEPOT/TRKL*1000.
C     WRITE(6,600) ICHG,ITYP,PV(4),IBE,TRKL,PLLI,ED,EDEPOT
  260 CONTINUE
      N0 = 1
C
      IF(LOSS.NE.0) GO TO 230
C
C                                           DEPTH OF CONVERSION
C                                           FOR PHOTONS IN THE LEAD GLAS
C                                           ASSUME UNIFORM CONVERSION
C                                           PROBABILITY WITHIN
C                                           THE FIRST 2 X0
      IF(LOSS.NE.0) GO TO 230
      X0=51.*RN(DUM)
      N0=X0/DELZ +1
  230 CONTINUE
C
      ISTEP=0
  210 ISTEP=ISTEP+1
C
C                                           CALCULATE INDEX FOR
C                                           BLOCKS WHICH ARE HIT AND
C                                           CHECK WHETHER PARTICLE IS
C                                           STILL IN THE LEAD GLASS
      CALL ENDCLG(R,NBL,*9000)
C
C                                           ACCUMULATE PULSEHEIGHTS
      IF(LOSS.GE.2) GO TO 250
C                                           PHOTONS AND E
      EDEPOT=0.
      IF(ISTEP.LT.N0) GO TO 240
      X1=X1+DX
      EDEPOT=SHOWR(ENERGY,X1,DX)
      IF(X1.GT.8.) EDEPOT=ENERGY-ELOSS
      ELOSS=ELOSS+EDEPOT
      IF(ELOSS.GT.ENERGY) GO TO 240
      AMPLG(NBL)=AMPLG(NBL)+EDEPOT
      IF(X1.GT.8.) RETURN
      GO TO 240
C                                           HADRONS
  250 AMPLG(NBL)=AMPLG(NBL)+EDEPOT
      ELS=ELS+EDEPOT
      IF(ELS.GE.ENERGY) GO TO 9000
C                                           PROPAGATE TRACK
  240 R(1)=R(1)+XL*PVX
      R(2)=R(2)+XL*PVY
      R(3)=R(3)+XL*PVZ
      GO TO 210
C
C                                           DE/DX ENERGY LOSS
 9000 IF(ELOSS.EQ.0.) RETURN
      RX=(PV(4)-ELOSS/2000.)/PV(4)
      IF(ELOSS.GE.ENERGY) RX=0.001
      IF(ELS  .GE.ENERGY) RX=0.001
      RX=ABS(RX)
      DO 9001 I4=1,4
 9001 PV(I4)=PV(I4)*RX
      PV(6)=PV(6)*RX
C
      RETURN
C
C  500   CENTRAL PART ELECTRONS AND PHOTONS
C  700   ENDCAP       ELECTRONS AND PHOTONS
C
C      CENTRAL PART
C      CALCULATE BLOCKS WHICH MAY SEE PART OF THE EL. MG. SHOWER
C
  500 XL = 40.
      NO = 1
      RR(1) = R(1)
      RR(2) = R(2)
      RR(3) = R(3) + 100.
C
C      CHECK WHETHER PARTICLE IS STILL IN THE LEAD GLASS
C
      R1=RR(1)**2+RR(2)**2
      IF(R1.GT.OUTR) GO TO 510
      IF(ZLGPL.LT.RR(3).OR.RR(3).LT.ZLGMI) GO TO 510
  501 PHI=ATAN2(RR(2),RR(1))
      IF(PHI.LT.0.) PHI=TWOPI+PHI
      NFI=PHI/DELFI
      IF(ABS(RR(3)).GT.ZLGPL) GO TO 510
      DZ=RR(3)-ZLGMI
      NZ=DZ/BLZ+1
      NBL=NZ+32*NFI
C
C     CHECK WHETHER NBL IS ALREADY IN THE BLOCK LIST IF NOT
C     ADD THE BLOCK NUMBER TO THE LIST
C
      IF(NO.EQ.1) GO TO 505
      DO 504 N=1,NO
      IF(LISTBL(N).EQ.NBL) GO TO 503
  504 CONTINUE
      NO = NO + 1
  505 LISTBL(NO)=NBL
      NPL = NFI+1
      NMI = NFI-1
      IF(NFI.EQ. 0) NMI = 83
      IF(NFI.EQ.83) NPL =  0
      LISTBL(NO+1) =NZ+32*NPL
      LISTBL(NO+2) =NZ+32*NMI
      NO = NO + 2
  503 RR(1)=RR(1)+XL*PVX
      RR(2)=RR(2)+XL*PVY
      RR(3)=RR(3)+XL*PVZ
C
C      CHECK WHETHER PARTICLE IS STILL IN THE LEAD GLASS
C
      R1=RR(1)**2+RR(2)**2
      IF(R1.GT.OUTR) GO TO 510
      IF(ZLGPL.LT.RR(3).OR.RR(3).LT.ZLGMI) GO TO 510
      GO TO 501
C
  510 RR(1) = R(1)
      RR(2) = R(2)
      RR(3) = R(3) - 100.
C
C      CHECK WHETHER PARTICLE IS STILL IN THE LEAD GLASS
C
      R1=RR(1)**2+RR(2)**2
      IF(R1.GT.OUTR) GO TO 520
      IF(ZLGPL.LT.RR(3).OR.RR(3).LT.ZLGMI) GO TO 520
C
  511 PHI=ATAN2(RR(2),RR(1))
      IF(PHI.LT.0.) PHI=TWOPI+PHI
      NFI=PHI/DELFI
      IF(ABS(RR(3)).GT.ZLGPL) GO TO 520
      DZ=RR(3)-ZLGMI
      NZ=DZ/BLZ+1
      NBL=NZ+32*NFI
C
C     CHECK WHETHER NBL IS ALREADY IN THE BLOCK LIST IF NOT
C     ADD THE BLOCK NUMBER TO THE LIST
C
      IF(NO.EQ.1) GO TO 515
      DO 514 N=1,NO
      IF(LISTBL(N).EQ.NBL) GO TO 513
  514 CONTINUE
      NO = NO + 1
  515 LISTBL(NO)=NBL
      NPL = NFI+1
      NMI = NFI-1
      IF(NFI.EQ. 0) NMI = 83
      IF(NFI.EQ.83) NPL =  0
      LISTBL(NO+1) =NZ+32*NPL
      LISTBL(NO+2) =NZ+32*NMI
      NO = NO + 2
  513 RR(1)=RR(1)+XL*PVX
      RR(2)=RR(2)+XL*PVY
      RR(3)=RR(3)+XL*PVZ
C
C      CHECK WHETHER PARTICLE IS STILL IN THE LEAD GLASS
C
      R1=RR(1)**2+RR(2)**2
      IF(R1.GT.OUTR) GO TO 520
      IF(ZLGPL.LT.RR(3).OR.RR(3).LT.ZLGMI) GO TO 520
C
      GO TO 511
C
  520 RR(1) = R(1)
      RR(2) = R(2)
      RR(3) = R(3)
C
C      CHECK WHETHER PARTICLE IS STILL IN THE LEAD GLASS
C
      R1=RR(1)**2+RR(2)**2
      IF(R1.GT.OUTR) GO TO 530
      IF(ZLGPL.LT.RR(3).OR.RR(3).LT.ZLGMI) GO TO 530
C
  521 PHI=ATAN2(RR(2),RR(1))
      IF(PHI.LT.0.) PHI=TWOPI+PHI
      NFI=PHI/DELFI
      IF(ABS(RR(3)).GT.ZLGPL) GO TO 530
      DZ=RR(3)-ZLGMI
      NZ=DZ/BLZ+1
      NBL=NZ+32*NFI
C
C     CHECK WHETHER NBL IS ALREADY IN THE BLOCK LIST IF NOT
C     ADD THE BLOCK NUMBER TO THE LIST
C
      IF(NO.EQ.1) GO TO 525
      DO 524 N=1,NO
      IF(LISTBL(N).EQ.NBL) GO TO 523
  524 CONTINUE
      NO = NO + 1
  525 LISTBL(NO)=NBL
      NPL = NFI+1
      NMI = NFI-1
      IF(NFI.EQ. 0) NMI = 83
      IF(NFI.EQ.83) NPL =  0
      LISTBL(NO+1) =NZ+32*NPL
      LISTBL(NO+2) =NZ+32*NMI
      NO = NO + 2
  523 RR(1)=RR(1)+XL*PVX
      RR(2)=RR(2)+XL*PVY
      RR(3)=RR(3)+XL*PVZ
C
C      CHECK WHETHER PARTICLE IS STILL IN THE LEAD GLASS
C
      R1=RR(1)**2+RR(2)**2
      IF(R1.GT.OUTR) GO TO 530
      IF(ZLGPL.LT.RR(3).OR.RR(3).LT.ZLGMI) GO TO 530
C
      GO TO 521
C
C     CALCULATE BLOCK ENERGIES
C
  530 IPART = 0
      VAR(1)= ATAN2(R(2),R(1))
      VAR(2)= R(3)
      IF(LISTBL(1).LT.0) RETURN 1
C ****    EXPECT USES BLOCK COUNTING STARTIN AT 0
      DO 532 N=1,NO
  532 LISTBL(N) = LISTBL(N) - 1
      CALL EXPECM(IPART,ENERGY,VAR,DC,NO,LISTBL,BLCFRC,AVGSUM)
C *** CORRECT FOR READOUT THRESHOLD
      ESEEN = 0.
      DO 533 NB=1,NO
      EBLOCK = ENERGY*BLCFRC(NB)
      IF(EBLOCK.GT.28.) ESEEN = ESEEN + EBLOCK
  533 CONTINUE
      READCR = 1.
      IF(ESEEN.GT.0.) READCR = ENERGY/ESEEN
C  CHANGED 3.7.86  19.30    J.OLSSON
C     IF(ENERGY.GT.0.) READCR = ESEEN/ENERGY
C ***  UPDATE BLOCK ENERGIES
      DO 531 NB=1,NO
      NBL = LISTBL(NB) + 1
      AMPLG(NBL) = AMPLG(NBL) + READCR*ENERGY*BLCFRC(NB)
  531 CONTINUE
C
      R(1) = RR(1)
      R(2) = RR(2)
      R(3) = RR(3)
      RETURN
C
C     END CAP LEAD GLASS
C     FOLLOW PHOTON TO THE FACE OF THE END CAP COUNTERS
C
  700 CONTINUE
      RABS=0.
      DO 701 I3=1,3
  701 RABS=RABS+R(I3)**2
      RABS=SQRT(RABS)
      DO 702 I3=1,3
  702 R(I3)=R(I3)/RABS
      DISTZ=ZENDPL
      IF(R(3).LT.0) DISTZ=ABS(ZENDMI)
      ZLAM=ABS(DISTZ/R(3))
      DO 703 I3=1,3
  703 R(I3)=ZLAM*R(I3)
C
C      SET UP BLOCK ARRAY OF BLOCKS WHICH MIGHT RECIEVE ENERGY
C      FROM THE EL.MG. SHOWER
C
      NO = 0
      RR(3) = R(3)
      DO 710 IC=1,3
      RR(1) = R(1) + (IC-2)*100.
      DO 711 JC=1,3
      RR(2) = R(2) + (JC-2)*100.
      CALL ENDCLG(RR,NBL,*711)
      IF(NO.EQ.0) GO TO 714
      DO 712 N=1,NO
      IF(LISTBL(N).EQ.NBL) GO TO 711
  712 CONTINUE
  714 NO = NO+1
      LISTBL(NO)=NBL
  711 CONTINUE
  710 CONTINUE
C
C     CALCULATE BLOCK ENERGIES
C
  730 IPART = 1
      IF(R(3).LT.0.) IPART = -1
      VAR(1)= R(1)
      VAR(2)= R(2)
      IF(LISTBL(1).LT.0) RETURN 1
C *****     EXPECT USES USUAL BLOCK COUNTING STARTING FROM 0
      DO 732 N=1,NO
  732 LISTBL(N) = LISTBL(N) - 1
      CALL EXPECM(IPART,ENERGY,VAR,DC,NO,LISTBL,BLCFRC,AVGSUM)
C *** CORRECT FOR READOUT THRESHOLD
      ESEEN = 0.
      DO 733 NB=1,NO
      EBLOCK = ENERGY*BLCFRC(NB)
      IF(EBLOCK.GT.28.) ESEEN = ESEEN + EBLOCK
  733 CONTINUE
      READCR = 1.
      IF(ESEEN.GT.0.) READCR = ENERGY/ESEEN
C  CHANGED 3.7.86  19.30    J.OLSSON
C     IF(ENERGY.GT.0.) READCR = ESEEN/ENERGY
C ***  UPDATE BLOCK ENERGIES
      DO 731 NB=1,NO
      NBL = LISTBL(NB) + 1
      AMPLG(NBL) = AMPLG(NBL) + READCR*ENERGY*BLCFRC(NB)
  731 CONTINUE
C
C UPDATE POSITION VECTOR
C
      TRKL=DEPEND/ABS(PVZ)
C
      R(1)=R(1)+PVX*TRKL
      R(2)=R(2)+PVY*TRKL
      R(3)=R(3)+PVZ*TRKL
C
      RETURN
      END
      SUBROUTINE EXPECM(IPART,E,VAR,DIRC,NBLOCK,IBLIST,BLCFRC,AVGSUM)
C
C     CALCULATE EXPECTED FRACTIONS OF ELECTROMAGNETIC SHOWER
C     IN NBLOCK BLOCKS GIVEN IN A BLOCKLIST IBLIST
C
C         >>>>>>>>>  MONTE CARLO VERSION  <<<<<<<<<
C
C     I/P : IPART        DETECTOR PART -1,0,+1
C           E            SHOWER ENERGY (DEFAULT 500 MEV)
C           VAR          IMPACT POINTS ON LEAD-GLASS SURFACE
C                        VAR(1),VAR(2) = PHI,Z IN BARREL CASE
C                        VAR(1),VAR(2) =   X,Y IN ENDCAP CASE
C           DIRC         DIRECTION COSINE OF SHOWER AXIS (DX,DY,DZ)
C           NBLOCK       LENGTH OF BLOCKLIST
C           IBLIST       LIST OF BLOCKS FOR WHICH THE FRACTIONS
C                        SHOULD BE CALCULATED
C
C     O/P : BLCFRC       FRACTION IN EACH REQUESTED BLOCK &
C                        CORRECTION FOR OUTSIDE FRACTION
C           AVGSUM       ENERGY FRACTION IN BLOCKS CONTAINED IN IBLIST
C
C                                       22/06/82
      EXTERNAL SF
C
#include "cgeo1.for"
C
      COMMON /TRAFO/ STR,CTR,SPR,CPR,CTRCPR,CTRSPR,STRCPR,STRSPR,
     *               JPART,XMOV,YMOV,ZMOV
      COMMON /PARAME/ P(6)
      COMMON / CJTRIG / PI,TWOPI
      DIMENSION VAR(2),DIRC(3)
      DIMENSION EL(6),AL(6),ALL(6),BL(6),ED(5),AD(5),ALD(5),BD(5)
      DIMENSION XL(3),XU(3)
      DIMENSION IBLIST(1),BLCFRC(1)
C
C     SHOWER FUNCTION PARAMETERS ACCORDING TO EGS3-MONTE-CARLO
C
      DATA EL     /100.   ,300.   ,500.   ,700.   ,1000.  ,5000.  /
      DATA AL     /  3.25 ,  4.59 ,  5.80 ,  6.06 ,  6.72 ,  8.63 /
      DATA ALL    /   .73 ,  1.45 ,  1.65 ,  1.84 ,  2.03 ,  2.74 /
      DATA BL     /   .395,   .443,   .447,   .449,   .460,   .455/
C
      DATA ED     /100.   ,200.   ,200.   ,300.   ,4000.   /
      DATA AD     /  1.34 ,  1.21 ,  1.26 ,   .66 ,   1.91 /
      DATA ALD    /   .72 ,   .2  ,   .19 ,   .19 ,    .71 /
      DATA BD     /   .048,   .004,   .002,   .011,   -.005/
C
      DATA X0 /23.90/
      DATA FACMC /.9550/
C
      JPART = IPART
      DO 115 IBL = 1,NBLOCK
      BLCFRC(IBL) = 0.
  115 CONTINUE
      AVGSUM = 0.
C
C     IMPACT POINT ON LG SURFACE ??
C
      IF(ICHECK(VAR(1),VAR(2),IPART).NE.1) RETURN
C
C     TRANSFORMATION PARAMETERS
C
      DX = DIRC(1)
      DY = DIRC(2)
      DZ = DIRC(3)
C
      IF(IPART.EQ.0) GOTO 1
      ZENDCP = IPART*ZENDPL
      XMOV = VAR(1)
      YMOV = VAR(2)
      ZMOV = ZENDCP
      GOTO 2
    1 XMOV = RLG*COS(VAR(1))
      YMOV = RLG*SIN(VAR(1))
      ZMOV = VAR(2)
    2 CTR = DZ/SQRT(DX*DX+DY*DY+DZ*DZ)
      STR = SQRT(1.-CTR*CTR)
      PR=ATAN2(DY,DX)
      IF(PR.LT.0.) PR=PR+TWOPI
      SPR = SIN(PR)
      CPR = COS(PR)
      CTRCPR = CTR*CPR
      CTRSPR = CTR*SPR
      STRCPR = STR*CPR
      STRSPR = STR*SPR
C
C     CHOOSE SHOWER START POINT (THIS IS THE MC-VERSION !)
C
      SSPOIN = -X0*ALOG(RN(DUMMY))/.773278
      XMOV = XMOV + SSPOIN*DX
      YMOV = YMOV + SSPOIN*DY
      ZMOV = ZMOV + SSPOIN*DZ
C
C     SHOWER PARAMETERS (PLO'I' ARE INTERPOLATED FROM THE LONGO PARAM.)
C
      IF(E.LE.0.) E=500.
      IF(E.LT.5000.) GOTO 3
      PLO1 = AL(6)
      PLO2 = ALL(6)
      PLO3 = BL(6)
      GOTO 9
    3 IF(E.LT.1000.) GOTO 4
      EFAC = (E-EL(5))/ED(5)
      PLO1 = AL(5)+AD(5)*EFAC
      PLO2 = ALL(5)+ALD(5)*EFAC
      PLO3 = BL(5)+BD(5)*EFAC
      GOTO 9
    4 IF(E.LT.700.)  GOTO 5
      EFAC = (E-EL(4))/ED(4)
      PLO1 = AL(4)+AD(4)*EFAC
      PLO2 = ALL(4)+ALD(4)*EFAC
      PLO3 = BL(4)+BD(4)*EFAC
      GOTO 9
    5 IF(E.LT.500.)  GOTO 6
      EFAC = (E-EL(3))/ED(3)
      PLO1 = AL(3)+AD(3)*EFAC
      PLO2 = ALL(3)+ALD(3)*EFAC
      PLO3 = BL(3)+BD(3)*EFAC
      GOTO 9
    6 IF(E.LT.300.)  GOTO 7
      EFAC = (E-EL(2))/ED(2)
      PLO1 = AL(2)+AD(2)*EFAC
      PLO2 = ALL(2)+ALD(2)*EFAC
      PLO3 = BL(2)+BD(2)*EFAC
      GOTO 9
    7 IF(E.LT.100.)  GOTO 8
      EFAC = (E-EL(1))/ED(1)
      PLO1 = AL(1)+AD(1)*EFAC
      PLO2 = ALL(1)+ALD(1)*EFAC
      PLO3 = BL(1)+BD(1)*EFAC
      GOTO 9
    8 PLO1 = AL(1)
      PLO2 = ALL(1)
      PLO3 = BL(1)
C
    9 P(1) = PLO2
      P(2) = PLO1*2.023
      P(3) = PLO3
      P(4) = 2.04604
      P(5) = PLO1*.17
      P(6) = .597942
C
C     NORMALIZE INTEGRAL
C
      P2A1=PLO2+1.
      FNORM = FACMC*69306.0*PLO1*GAMMA(P2A1)/PLO3**P2A1
C
C     INTEGRATION
C
C
C     CHECK TOTAL VALUE ON REQUEST
C
C     NBLOCK=0
      IF(NBLOCK.GT.0) GOTO 113
      XL(1) = RLG
      IF(IPART.NE.0) XL(1) = VAR(1)-500.
      XL(2) = VAR(1)-2.
      IF(IPART.NE.0) XL(2) = VAR(2)-500.
      XL(3) = VAR(2)-500.
      IF(IPART.EQ. 1) XL(3) = ZENDPL
      IF(IPART.EQ.-1) XL(3) = ZENDMI-1000.
      XU(1) = RLG+1000.
      IF(IPART.NE.0) XU(1) = VAR(1)+500.
      XU(2) = VAR(1)+2.
      IF(IPART.NE.0) XU(2) = VAR(2)+500.
      XU(3) = VAR(2)+500.
      IF(IPART.EQ. 1) XU(3) = ZENDPL+1000.
      IF(IPART.EQ.-1) XU(3) = ZENDMI
      CALL WEGAS(SF,.001,1000,9,XL,XU,AVG,SD,C2)
      AVG = AVG/FNORM
      SD = SD/FNORM
C     WRITE(6,1133) AVG,SD,C2
C1133 FORMAT(10X,'---> VEGAS INTEGRAL RESULT : ',F12.5,/,
C    *       10X,'---> ERROR + CHI**2 VALUE  : ',2F12.5)
      RETURN
C
C     PERFORM INTEGRATION OVER REQUESTED BLOCKS
C
  113 BEHIND = E*X0/100.
      BFRONT = 10.
C
      DO 114 IBL = 1,NBLOCK
      IBLOCK = IBLIST(IBL)
      CALL LIMINT(IBLOCK,IPART,IER,XL(1),XL(2),XL(3),XU(1),XU(2),XU(3))
      IF(IER.GT.0) GOTO 114
      IF(IPART.EQ. 0) XU(1) = XU(1) + BEHIND
      IF(IPART.EQ. 1) XU(3) = XU(3) + BEHIND
      IF(IPART.EQ.-1) XL(3) = XL(3) - BEHIND
      IF(IPART.EQ. 0) XL(1) = XL(1) - BFRONT
      IF(IPART.EQ. 1) XL(3) = XL(3) - BFRONT
      IF(IPART.EQ.-1) XU(3) = XU(3) + BFRONT
      CALL WEGAS(SF,.01,200,3,XL,XU,AVG,SD,C2)
      AVG = AVG/FNORM
      SD = SD/FNORM
      AVGSUM = AVGSUM+AVG
      BLCFRC(IBL) = AVG
  114 CONTINUE
C
      AVGDIF = 1.-AVGSUM
C
      RETURN
      END
C
      REAL FUNCTION SF(X)
C
C     FUNCTION TO BE INTEGRATED BY VEGAS
C                                       16/06/82
C
      COMMON /TRAFO/ STR,CTR,SPR,CPR,CTRCPR,CTRSPR,STRCPR,STRSPR,
     *               JPART,XMOV,YMOV,ZMOV
      COMMON /PARAME/ P(6)
      REAL X(3)
      DATA X0 /23.90/
C
      SF = 0.
C
C     TRANSFORM INTO CARTESIAN DETECTOR SYSTEM
C
      IF(JPART.NE.0) GOTO 1
      XDET = X(1)*COS(X(2))
      YDET = X(1)*SIN(X(2))
      ZDET = X(3)
      FJACOB = X(1)
      GOTO 2
    1 XDET = X(1)
      YDET = X(2)
      ZDET = X(3)
      FJACOB = 1.
C
C     LINEAR TRANSFORMATION TO SHOWER START POINT
C
    2 XLTR = XDET-XMOV
      YLTR = YDET-YMOV
      ZLTR = ZDET-ZMOV
C
C     ROTATE INTO SHOWER SYSTEM (FIRST CHECK SIGN OF ZSHW)
C
      ZSHW  =  XLTR * STRCPR   +   YLTR * STRSPR   +   ZLTR * CTR
C
      IF(ZSHW.LE.0.) RETURN
C
      XSHW  =  XLTR * CTRCPR   +   YLTR * CTRSPR   -   ZLTR * STR
      YSHW  = -XLTR * SPR      +   YLTR * CPR
C
C     SCALE IN RADIATION LENGTHS
C
      RSHS = SQRT(XSHW*XSHW+YSHW*YSHW)/X0
      ZSHS = ZSHW/X0
C
C     COMPRESS SHOWER FUNCTION
C
      P3Z = P(3)*ZSHS
      REX = -1.*P(1)*ALOG(ZSHS)
      EX1 = -1.*(REX+P3Z+P(4)*RSHS)
      EX2 = -1.*(REX+P3Z+P(6)*RSHS)
      IF(EX1.LT.-10..OR.EX2.LT.-10.) RETURN
C
      SF =  P(2)*EXP(EX1) + P(5)*EXP(EX2)
C
C     JACOBIAN
C
      SF = SF*FJACOB
C
      RETURN
      END
