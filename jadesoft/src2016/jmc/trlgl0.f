C   20/03/81 312061537  MEMBER NAME  TRLGL0   (S)           FORTRAN
      SUBROUTINE TRLGL(PV,R,*)
C
C      THIS ROUTINE TRACKS PARTICLES THROUGH THE LEAD GLASS
C      ARRAYS AND ACCUMULATES PULSEHIGHTS
C      PV = INCOMING 4 VECTOR IS CHANGED DUE TO ENERGY LOSS
C      R  = 3 VECTOR POINTING TO THE FACE OF THE LEAD GLASS
C      RETURN 1 IS USED IF THE INCIDENT MOMENTUM IS LESS THAN
C               10 MEV
C     LAST CHANGE NOV.28. 1983   W BARTEL
C ==> CHANGED :  USE THE SIMULATION BY THE RESULTS OF KEK TEST EXP.
C ==> CHANGED :  USE LFLAG TO STEER SHOWER CALCALATION.
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
     *             XHOL1,XHOL2,YHOL1,YHOL2
      COMMON/CLGHIT/AMPLG(3000)
      DIMENSION DC(3)
      LOGICAL * 1 LFLAG
      COMMON/CFLAG/LFLAG(10)
C
C        LFLAG(1) = SMEAR GAMMA AND ELECTRON ENERGIES
C        LFLAG(2) = GAMMA CONVERSION IN OUTER TANK AND COIL (TRKGAM)
C        LFLAG(3) = ABSORPTION LOSSES
C        LFLAG(4) = 3 DIM SHOWER PROFILE FIT TO EGGS CODE
C
      DATA TWOPI/6.28319/
      REAL RADL/25.4/
      DIMENSION R(3),PV(10)
      DIMENSION R0(3)
C
C
C      FIND INDEX FOR ENERGY LOSS
C       LOSS = 0  PHOTON
C       LOSS = 1  ELECTRON
C       LOSS = 2  MINIMUM IONIZING PARTICLE
C
      IF(PV(6).LT.0.01) RETURN 1
      IF(LFLAG(4)) CALL TRLGSH(PV,R,&999)
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
      IF(RADIUS.GT.OUTR2 .OR. R(3).GT.ZENDPL+DEPEND
     *  .OR. R(3).LT.ZENDMI-DEPEND) RETURN
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
      PVR=ABS(R(1)*PVX+R(2)*PVY)
      DELR=PVR/RADIUS
      TRKL=BLDEP/DELR
C
C                                           DETERMINE STEP SIZE
      NSTEP=BLDEP/(10.*DELR)+1
      XL=10.
      DX=XL/RADL
C
C                                           ENERGY LOSS FOR HADRONS
      IF( LOSS.LT.2 ) GO TO 26
C
C     CALCULATE EXITIN POINT AT THE BACK SURFACE OF THE LEAD GLASS
C
      R0(1)=R(1)+PVX*TRKL
      R0(2)=R(2)+PVY*TRKL
      R0(3)=R(3)+PVZ*TRKL
C
C     CALCULATE PATH LENGTH THROUGH LIGHT GUIDE
C
      IFLG=2
      CALL PLLIGD(PLLI,R(1),R(2),R(3),R0(1),R0(2),R0(3),IFLG)
      ICHG=INT(PV(7))
      ITYP=INT(PV(8))
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
      PV(1)=PV(1)*RX
      PV(2)=PV(2)*RX
      PV(3)=PV(3)*RX
      PV(4)=PV(4)*RX
      PV(6)=PV(6)*RX
      RETURN
C
C                                           END CAP LEAD GLASS
C                                           FOLLOW PHOTON TO THE FACE
C                                           OF THE END CAP COUNTERS
 1000 CONTINUE
      RABS=R(1)**2 + R(2)**2 + R(3)**2
      RABS=SQRT(RABS)
      R(1)=R(1)/RABS
      R(2)=R(2)/RABS
      R(3)=R(3)/RABS
      DISTZ=ZENDPL
      IF(R(3).LT.0) DISTZ=ABS(ZENDMI)
      ZLAM=ABS(DISTZ/R(3))
      R(1)=ZLAM*R(1)
      R(2)=ZLAM*R(2)
      R(3)=ZLAM*R(3)
      CALL ENDCLG(R,NBL,&9000)
      IF(NBL.LT.2689 .OR. NBL.GT.2880) GO TO 9000
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
      IF( LOSS.LT.2 ) GO TO 260
      PLLI=0.
      ICHG=INT(PV(7))
      ITYP=INT(PV(8))
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
      CALL ENDCLG(R,NBL,&9000)
      IF(NBL.LT.2689 .OR. NBL.GT.2880) GO TO 9000
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
      PV(1)=PV(1)*RX
      PV(2)=PV(2)*RX
      PV(3)=PV(3)*RX
      PV(4)=PV(4)*RX
      PV(6)=PV(6)*RX
C
      RETURN
  999 RETURN 1
      END
C
      FUNCTION ELSMR( IBE, DC, EINC )
C-----------------------------------------------------------
C  VERSION OF 12/02/81     LAST MOD 31/03/81    E.ELSEN
C  ENERGY SMEARING ACCORDING TO WATANABES LGESMR
C  ENERGIES IN GEV
C-----------------------------------------------------------
C
      DIMENSION DC(3)
      REAL*8 FNORM
      DATA ICALL/0/
      ICALL=ICALL+1
      IF(ICALL.LE.1) WRITE(6,9101)
 9101 FORMAT(2X,5(1H%),' E SMEAR IS CALLED',5(1H%))
C
      IF( IBE ) 200, 100, 200
C                                           BARREL PART
  100 CONTINUE
      IF(EINC.GT.6.) GO TO 120
      SIG=AMAX1( 0.055, SQRT(0.0036/EINC+0.001225)  )
      GO TO 240
  120 SIG=AMAX1( 0.04, 0.055-(ABS(DC(3))-0.4)*(EINC-6.)/144.)
      GO TO 240
C
C                                           END CAP
  200 CONTINUE
      SIG= AMAX1( 0.12, 0.28/SQRT(EINC) )
C
  240 ELSMR=EINC*(1.+FNORM(0.)*SIG)
      ELSMR = AMAX1( ELSMR, 0.001 )
      RETURN
      END
