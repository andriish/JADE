C   02/11/81 903061123  MEMBER NAME  MCJADEPL (S)           FORTRAN
      SUBROUTINE MCJADE( NREVS, IPRINT )
C--------------------------------------------------------
C   SPECIAL VERSION OF JADE MONTE CARLO TRACKING PROGRAM
C                      USING
C   TOKYO SHOWER MONTE CARLO (A.SATO, S.YAMADA)
C                       IMPLEMENTED BY J.OLSSON
C
C   PHOTONS ARE TREATED IN SPECIAL VERSION OF TRKGAM, WHICH RETURNS2
C   AFTER COMPLETION OF SHOWER. THIS GIVES RETURN1 IN JTRKGM AND NEXT..
C   ELECTRONS ARE FED INTO LGMC56 DIRECTLY IN THIS MAIN ROUTINE..
C--------------------------------------------------------
C
C             11/05/79                          E.ELSEN
C      MOD  E. ELSEN    10/06/82 :
C      MOD  J. OLSSON   30/08/83 :  SUBSET TRACKED & EVENT COUNTING
C      MOD  W. BARTEL   25/10/83 :  PARTICLES BEYOND COIL NOT TRACKED
C      MOD  C. BOWDERY  17/11/83 :  CALL TO MCVSUM FOR ERROR SUMMARY
C      MOD  C. BOWDERY  24/11/83 :  DELETE CALL TO TRCOIL FOR PHOTONS
C      MOD  W. BARTEL   29/11/83 :  IEVMIN/MAX (/CIEVS/) DEFLT. IN BLDAT
C      MOD  W. BARTEL   29/11/83 :  INTRODUCE N AND K0L TRACKING
C      MOD  W. BARTEL   29/11/83 :  EXTERNAL JADEBD  INSTEAD OF BLDAT
C      MOD  J. HAGEMANN 28/08/84 :  VERTEX CHAMBER TRACKING FACILITY
C           R. RAMCKE               IMPLEMENTED
C      MOD  J. HAGEMANN 19/04/86 :  FWDDET REPLACED BY MCTAGM
C      MOD  J.OLSSON    09/06/86 :  LFLAG(2) IS TRUE IF LFLAG(4) IS TRUE
C      MOD  J.OLSSON    11/06/86 :  NSEC T.O. LIMIT DEPENDS ON LFLAG(4)
C      MOD  J.OLSSON    25/04/87 :  LFLAG(6) FOR TOKYO SHOWER PROGRAM
C      MOD  J.O, A.WEG. 20/01/88 :  JBWTOR INSTALLED FOR OPERATOR STOP
C LAST MOD  J.O, C.H.      02/89 :  GEANT FOR TAGGING SIMULATION
C
C
C    ALLOWS SECTIONS OF THE INPUT FILE TO BE TRACKED.
C    MINIMUM AND MAXIMUM NR OF EVENT SET IN SUBR. JGETEV
C    EVENT COUNT PASSED VIA /CIEVS/ KIEV,IEVMIN,IEVMAX
C
C    WRITE NREVS MTC EVENTS ON UNIT 2. TAKE INPUT FOUR VECTORS
C    FROM ROUTINE JGETVC.
C    IF NREVS .EQ. 0, EVENTS ARE WRITTEN UNTIL EOF OR TIME OUT.
C    INPUT FOUR VECTORS FOR THE FIRST IPRINT ARE PRINTED.
C
C-----------------------------------------------------------------------
C
C      UPDATED TO JMC.S STANDARD     11.5.1984   J.OLSSON
C      UPDATED TO ALEX FINCH NEW FW   4.11.1985   J.OLSSON
C      UPDATED TO CALL LGMC56, SF5-SF6 SHOWER MC  J.OLSSON   23.1.86
C
C--------------------------------------------------------
C
      IMPLICIT INTEGER*2 (H)
C                                              BLOCK DATA IS NOW JADEBD
      EXTERNAL JADEBD
C
      COMMON / CJTCDC / IBANK, IPART
      COMMON / CIEVS / KIEV,IEVMIN,IEVMAX
C
      COMMON /CCSF56/ NFINAL,NTR56
C
      COMMON/CMCTAG/MARKMC,HGG(192),IHIT,INMARK,NCHANS,SIGMAS,CALIBS
C
C
      COMMON /BCS/ IW(30000)
      DIMENSION HW(1),AW(1)
      EQUIVALENCE(IW(1),HW(1)),(IW(1),AW(1))
C
      COMMON / COLSON/ IPRO
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
      DIMENSION PV(10),R0(5),RR(5),RYAM(3)
      DIMENSION PVSAVE(10),R0SAVE(3)
C-----------------------
C  SHOWER MC COMMON, BLOCK DATA SET IN LGCR56
C
      COMMON /CLGD56/ RADIUS(6),ZEND(2),ZWID,ZGAP,PHWID,
     2                ECXLST(24), ECYLST(24),ZECAP(4),ZPV(4),TPV,
     3                TH(4),THECP
C
C     RADIUS(1,2) = DISTANCE TO THE COIL SURFACES(INNER AND OUTER)
C       THESE VALUES ARE AVERAGES, SINCE THE TOTAL RADIATION LENGTH IS
C       IS ASSUMED TO COME FROM PURE AL. IN REALITY PLASTIC AND H20 IS
C       ALSO CONSIDERED:
C
C  1979-1983: 97.85 % X0 = 0.9785*88.947 MM = 87.03 MM
C        DATA RADIUS/ 900.0, 987.0, 1100.0, 1400.0, 1150.0, 1400.0/
C
C  1984-1986: 104.10 % X0 = 1.0410*88.947 MM = 92.59 MM
C        DATA RADIUS/ 900.0, 992.6, 1100.0, 1400.0, 1150.0, 1400.0/
C
C     RADIUS(3,4) = DISTANCE TO THE LEAD GLASS SURFACES(TYPE A)
C     RADIUS(5,6) = DISTANCE TO THE LEAD GLASS SURFACES(TYPE A-PR)
C---------------------------
C
      DATA NSEC / 4 /, IERR2/0/, IERR3/0/, IERR4/0/
      DATA NUNIT / 2 /
C
      LOGICAL*1 LFLAG
      COMMON/CFLAG/LFLAG(10)
C
C        LFLAG(1) = SMEAR GAMMA AND ELECTRON ENERGIES
C        LFLAG(2) = GAMMA CONVERSION IN OUTER TANK AND COIL (TRKGAM)
C        LFLAG(3) = ABSORPTION LOSSES
C        LFLAG(4) = 3 DIM SHOWER PROFILE FIT TO EGS CODE
C                              (MEIER / MAGNUSSEN PROGRAM)
C        LFLAG(5) = .TRUE.   -->  WITH VERTEX CHAMBER TRACKING
C                 = .FALSE.  -->  WITHOUT VERTEX CHAMBER TRACKING
C                                 BUT OLD BEAM PIPE GEOMETRY AND
C                                 BEAM PIPE COUNTERS (BEFORE MAI 84)
C        LFLAG(6) = 3 DIM TOKYO SHOWER PROGRAM, JADE NOTE 20A
C
C        LFLAG(7) = GEANT TRACKING FOR HADRONS, NOT RECOMMENDED  (D.P.)
C        LFLAG(8) = GEANT SHOWER PROGRAM FOR TAGGING APPARATUS 1983-86
C
      INTEGER*2 HDATE
      COMMON /TODAY/ HDATE(6)
C
      DATA  ICALLP/0/
C
      IF(ICALLP.NE.0) GO TO 11
C
      CALL HBOOK1(2301,'MCJADE: ETOT BEFORE MCTAGM$',100,0.,50.)
      CALL HBOOK1(2302,'MCJADE: ETOT AFTER MCTAGM$',100,0.,50.)
      ICALLP = 1
11    CONTINUE
C
C
C------------------  C O D E  ------------------------------------------
C
C                                           PRINT NEWS
      CALL MCNEWS
C
C     ***** JBWTOR ENABLE OPERATOR TO CANCEL IN ORDERED FASHION *****
C             INITIALIZE
C
      CALL JBWTOR(&2727)
      GO TO 2728
C
2727  WRITE(6,2729)
2729  FORMAT(/,'========== JBWTOR INIT ERROR =======')
2728  CONTINUE
C
      IF(LFLAG(6)) WRITE(6,913)
913   FORMAT('  ****************************************************'/
     $       '  *                                                  *'/
     $       '  *   TOKYO SHOWER MONTE CARLO,   SF5 + SF6  VERSION *'/
     $       '  *                                                  *'/
     $       '  ****************************************************')
C
      IF(LFLAG(8)) WRITE(6,222)
  222 FORMAT(///,X,'***********************************************',/,
     *           X,'*                  G E A N T                  *',/,
     *           X,'*        SIMULATION OF TAGGING APPARATUS      *',/,
     *           X,'*      SHOULD ONLY BE USED FOR 1983-86 !!!    *',/,
     *           X,'***********************************************')
C
      IF(LFLAG(4)) WRITE(6,111)
  111 FORMAT(///,X,'***********************************************',/,
     *           X,'* LEAD GLASS SIMULATION IS DONE USING         *',/,
     *           X,'*        F11MEI.MCSHOWS(TRLG3)                *',/,
     *           X,'*                                             *',/,
     *           X,'* CONTAINING :                                *',/,
     *           X,'*                                             *',/,
     *           X,'*     - EM. SHOWERS FROM INTEGRATION OVER     *',/,
     *           X,'*       3 DIM SHOWER FUNCTION FOR PARTICLES   *',/,
     *           X,'*       ABOVE 100 MEV                         *',/,
     *           X,'*     - STANDARD EM-SHOWER PROCEDURE FOR      *',/,
     *           X,'*       FOR LOW ENERGY PARTICLES              *',/,
     *           X,'*     - HADRON INTERACTION USING THE KANZAKI  *',/,
     *           X,'*       SCHEME BASED ON THE KEK MEASURMENTS   *',/,
     *           X,'*       LIGHT GUIDES ARE INCLUDED FOR BARREL  *',/,
     *           X,'*                                             *',/,
     *           X,'*                           VERSION 20/09/83  *',/,
     *           X,'***********************************************',//)
C
C
C   LFLAG(1-3) MUST BE TRUE IF TOKYO SHOWER SIMULATION IS USED
C
      IF(LFLAG(4).AND..NOT.LFLAG(2)) WRITE(6,681)
681   FORMAT(' ### WARNING ###  LFLAG(2) SET TRUE, SINCE LFLAG(4) TRUE')
      IF(LFLAG(6).AND..NOT.LFLAG(2)) WRITE(6,693)
693   FORMAT(' ### WARNING ###  LFLAG(2) SET TRUE, SINCE LFLAG(6) TRUE')
      IF(LFLAG(4).AND..NOT.LFLAG(2)) LFLAG(2) = .TRUE.
      IF(LFLAG(6).AND..NOT.LFLAG(2)) LFLAG(2) = .TRUE.
C
      IF(LFLAG(6).AND..NOT.LFLAG(1)) WRITE(6,678)
678   FORMAT(' ### WARNING ###  LFLAG(1) SET TRUE, SINCE LFLAG(4) TRUE')
      IF(LFLAG(6).AND..NOT.LFLAG(1)) LFLAG(1) = .TRUE.
      IF(LFLAG(6).AND..NOT.LFLAG(3)) WRITE(6,679)
679   FORMAT(' ### WARNING ###  LFLAG(3) SET TRUE, SINCE LFLAG(4) TRUE')
      IF(LFLAG(6).AND..NOT.LFLAG(3)) LFLAG(3) = .TRUE.
C IF TOKYO OR MEIER CODE IS USED, LARGER TIME OUT LIMIT IS NEEDED
      IF(LFLAG(6).OR.LFLAG(4)) NSEC = 6
C IF GEANT CODE IS USED, VERY LARGE TIME OUT LIMIT IS NEEDED
      IF(LFLAG(8)) NSEC = 70
C
C  LFLAG(5) MUST BE TRUE IF VERTEX CHAMBER GEOMETRY IS USED
C
      IF(LFLAG(5).AND.(HDATE(6).GT.1984.OR.
     $   (HDATE(6).EQ.1984.AND.HDATE(5).GE.5))) GO TO 692
      IF(.NOT.LFLAG(5).AND.(HDATE(6).LT.1984.OR.
     $   (HDATE(6).EQ.1984.AND.HDATE(5).LT.5))) GO TO 692
      IF(HDATE(6).GT.1984.OR.
     $   (HDATE(6).EQ.1984.AND.HDATE(5).GE.5)) LFLAG(5) = .TRUE.
      IF(HDATE(6).LT.1984.OR.
     $   (HDATE(6).EQ.1984.AND.HDATE(5).LT.5)) LFLAG(5) = .FALSE.
      WRITE(6,691)
691   FORMAT('  #####  WARNING ####  LFLAG(5) HAS BEEN SWITCHED TO AGREE
     $ WITH COMMON /TODAY/ SETTING OF DETECTOR HARDWARE')
692   CONTINUE
C
C THE SHOWER MC COMMON, /CLGD56/, NEEDS ADJUSTMENT OF SOME PARAMETERS
C                                 FOR 1984-1986 (Z-CHAMBER)
C
      IF(HDATE(6).LT.1984) GO TO 1450
C
      RADIUS(2) = 992.6
      TH(1) = RADIUS(2) - RADIUS(1)
      TH(2) = RADIUS(3) - RADIUS(2)
C
1450  CONTINUE
C                                           INPUT OF MUCHAMBER CALIBRATI
      CALL MUCONM
C
C                                           INITIALIZE PROGRAM
 2200 NEVTS = NREVS
      IF( NEVTS .EQ. 0 ) NEVTS = 999999
C
      IWRT = 0
      CALL BWRO( NUNIT, 1558, 2 )
      CALL JINIT
C
      IF(LFLAG(8)) CALL GEANTI
C
C                                      ************************
C                                       **                  **
C                                         **  EVENT LOOP  **
C                                       **                  **
C                                      ************************
      DO 1001 IEV=1,NEVTS
C
C TEST FOR OPERATOR STOP
C
      CALL JBWTOR(&92)
C
C
      IF( JUHR(NSEC) .NE. 2 ) GO TO 1011
      IF( JUHR(NSEC) .NE. 2 ) GO TO 1011
      GO TO 90
C                                           INITIALISE EVENT
1011  CALL EVTINI
      WRITE(6,2331) IEV
2331  FORMAT(' EVTINI WAS CALLED, IEV ',I6)
C
      IJETCP = 0
      IPRI = 1
      IF( IWRT .GT. IPRINT -1) IPRI= 0
C                                           GET EVENT
      CALL JGETEV( &91 )
C
C                                      ************************
C                                       **                  **
C                                         **  TRACK LOOP  **
C                                       **                  **
C                                      ************************
      NTR56 = 0
 1002 CONTINUE
C                                           NEXT PARTICLE
      CALL JGETVC( PV, R0, ICODE, IPRI )
      NTR56 = NTR56 + 1
      WRITE(6,2332) IEV,NTR56
2332  FORMAT(' TRACK LOOP, IEV NTR56 ',2I6)
C
      IF(NTR56.GT.NFINAL) GO TO 2120
      IPSF56 = IW(IBLN('SF56'))
      IF(IPSF56.GT.0) HW(2*IPSF56+2+2*NTR56) = -1
C
2120  IF( ICODE .GT. 0 ) GO TO 1003
C
C                                           CHECK TIME FOR PARTICLES
C                                           NECCESSARY IF MUONS INCLUDED
C                                           NECCESSARY IF SHOWER IS USED
      IF( JUHR(NSEC) .NE. 2 ) GO TO 1012
      IF( JUHR(NSEC) .NE. 2 ) GO TO 1012
      KIEV = KIEV - 1
      GO TO 90
C
C     DON'T TRACK PARTICLES WHICH START ONLY BEYOND THE COIL OR
C     THE END CAP LEAD GLASS ARRAY
C                                   OCT. 25. 1983 W.BARTEL
C
1012  R4 =  R0(1)*R0(1) + R0(2)*R0(2)
      R4 = SQRT( R4 )
      WRITE(6,2333) IEV,NTR56,R4,RCOIL,R0(3),ZENDPL
2333  FORMAT(' TRACK LOOP, IEV NTR56 R4 RCOIL R3 ZEN',2I6,4E12.4)
C
      IF( R4 .GT. RCOIL) GO TO 1000
      IF( ABS(R0(3)) .GT. ZENDPL) GO TO 1000
C
C                                           FWD TRACKING
         SUMPLO = 0.
         DO 2002 J=1,192
         IF(HGG(J).GT.0) SUMPLO = SUMPLO + .001*HGG(J)
 2002    CONTINUE
         CALL HFILL(2301,SUMPLO,0,1.)
      WRITE(6,2334) IEV,NTR56,SUMPLO
2334  FORMAT(' BEF MCTAGM, IEV NTR56 SUMPLO ',2I6,E12.4)
      IF(SUMPLO.GT.0.) WRITE(6,2324) (HGG(IK),IK=1,192)
2324  FORMAT(' HGG ',10I10)
C
      CALL MCTAGM( PV, R0)
         SUMPLO = 0.
         DO 2003 J=1,192
         IF(HGG(J).GT.0) SUMPLO = SUMPLO + .001*HGG(J)
 2003    CONTINUE
         CALL HFILL(2302,SUMPLO,0,1.)
C                                           BRANCH ACCORDING
C                                           TO CHARGE OF PARTICLE, PV(7)
C                                           AND TYPE OF PARTICLE, PV(8)
C
C
      IF(IPRO.NE.0) WRITE(6,2431) (PV(IK),IK=1,10)
2431  FORMAT(' MCJADE: PV ',10(F10.4,1X))
C
C                                           NEUTRINOS
      IF( PV(7) .EQ. 0. .AND. PV(8).LT.0.5) GO TO 1002
C                                           PHOTONS
      IF( PV(7) .EQ. 0. .AND. PV(8).LT.2.) GO TO 3
C
C                                           BRANCH OFF K0L AND N
      IF( PV(7) .EQ. 0. .AND. ABS(PV(8)-5.) . LT. 1.E-5) GO TO 4
      IF( PV(7) .EQ. 0. .AND. ABS(PV(8)-6.) . LT. 1.E-5) GO TO 4
C
C                                     *** CHARGED PARTICLES HERE ***
C
C***********************************************************************
C                                           CHECK IF VERTEX CHAMBER
C                                           TRACKING IS WANTED
      IF( .NOT. LFLAG(5) )  CALL TRCDET(PV,R0,RR,&1004)
      IF(  LFLAG(5) )  CALL TRCDTV(PV,R0,RR,&1004)
C***********************************************************************
C                                          MOMENTUM CHECK, PV(6)
      IF( PV(6) .LT. .001 ) GO TO 1000
C#######################################################################
C    SPECIAL SECTION FOR PASSING ELECTRONS INTO THE TOKYO SHOWER PROGRAM
C
C  LOSS IS A MASS LABEL
C
      LOSS=2
      IF(PV(5).LT.1.E-03) LOSS=1
      IF(PV(5).EQ.0.)     LOSS=0
      IF(LOSS.NE.1) GO TO 5300
C
C                               ELECTRONS HERE
      PVX=PV(1)/PV(6)
      PVY=PV(2)/PV(6)
      PVZ=PV(3)/PV(6)
      PVXY=PVX**2+PVY**2
C
      R1=RR(1)**2+RR(2)**2
C TEST HERE IF THE ELECTRON ALREADY TRACKED FARTHER THAN RADIUS(1)
      REL1 = SQRT(R1)
      IF(REL1.LE.RADIUS(1)) GO TO 1400
C  SCALE ELECTRON RR VECTOR
      SCEL = RADIUS(1)/REL1
      RR(1) = SCEL*RR(1)
      RR(2) = SCEL*RR(2)
      RR(3) = SCEL*RR(3)
      R1=RR(1)**2+RR(2)**2
C
1400  DR2=(ROTNK**2-R1)/PVXY
      IF(IPRO.NE.0) WRITE(6,931) DR2,R1,ROTNK,PVXY
931   FORMAT(' MCJADE ELECTRON:  DR2 R1 ROTNK PVXY ',4E15.7)
      IF( DR2 .GT. 0. ) GO TO 1515
      RY=RR(1)**2+RR(2)**2
      DRY=(RADIUS(1)**2-RY)/PVXY
      PVR=RR(1)*PVX+RR(2)*PVY
      PVR=PVR/PVXY
      PVR2 = PVR**2 + DRY
      IF(PVR2.GE.0.) GO TO 1074
      IERR2 = IERR2 + 1
      IF(IERR2.LT.50) WRITE(6,1073) IEV,IWRT,R0(1),R0(2),R0(3),PVR,DRY
1073  FORMAT(' ---> ERROR2  MCJADE: IEV+W R01-3 PVR DRY ',2I6,5E14.6)
      IF(IERR2.LT.50) WRITE(6,1081) (PV(IO),IO=1,10)
1081  FORMAT(' ---> ERROR2  MCJADE: PV1-10 ',5E14.6)
      IF(IERR2.LT.50) WRITE(6,1082) RADIUS(1),(RR(IO),IO=1,3),PVXY
1082  FORMAT(' ---> ERROR2  MCJADE: RADIUS1 RR1-3 PVXY ',5E14.6)
      GO TO 1002
1074  XL=-PVR+SQRT(PVR2)
C
      RYAM(1)=RR(1)+XL*PVX
      RYAM(2)=RR(2)+XL*PVY
      RYAM(3)=RR(3)+XL*PVZ
      GO TO 15
1515  PVR=RR(1)*PVX+RR(2)*PVY
      PVR=PVR/PVXY
      PVR2 = PVR**2 + DR2
      IF(PVR2.GE.0.) GO TO 1075
      IERR3 = IERR3 + 1
      IF(IERR3.LT.50) WRITE(6,1076) IEV,R0(1),R0(2),R0(3),PVR,DR2
1076  FORMAT(' ---> ERROR3  MCJADE: IEV R01-3 PVR DR2 ',I6,5E14.6)
      GO TO 1001
1075  XL=-PVR+SQRT(PVR2)
C
      RYAM(1)=RR(1)+XL*PVX
      RYAM(2)=RR(2)+XL*PVY
      RYAM(3)=RR(3)+XL*PVZ
15    IREGFL = 0
      IF(IPRO.EQ.0) GO TO 1516
      RYRAD = SQRT(RYAM(1)**2+RYAM(2)**2)
      WRITE(6,9333) ZTKP,ZTKM,RYRAD,RYAM(3)
9333  FORMAT(' ZTKP,ZTKM RYRAD RYAM3 ',4E14.6)
C
1516  IF(RYAM(3).LT.ZTKP.AND.RYAM(3).GT.ZTKM) GO TO 1517
C
C END CAPS
C
      RABS=0.
      DO 801 I3=1,3
 801  RABS=RABS+RYAM(I3)**2
      IF(RABS.GE.0.) GO TO 1077
      IERR4 = IERR4 + 1
      IF(IERR4.LT.50) WRITE(6,1078) IEV,R0(1),R0(2),R0(3),PVR,DR2
1078  FORMAT(' ---> ERROR4  MCJADE: IEV R01-3 PVR DR2 ',I6,5E14.6)
      GO TO 1001
1077  RABS=SQRT(RABS)
      DO 802 I3=1,3
 802  RYAM(I3)=RYAM(I3)/RABS
      ZLAM=ZPV(1)/RYAM(3)
      IREGFL = -1
      IF( RYAM(3) .GT. 0. ) ZLAM=ZPV(3)/RYAM(3)
      IF( RYAM(3) .GT. 0. ) IREGFL = 1
      DO 803 I3=1,3
 803  RYAM(I3)=ZLAM*RYAM(I3)
      IF(IPRO.EQ.0) GO TO 1517
      RYRAD = SQRT(RYAM(1)**2+RYAM(2)**2)
      WRITE(6,9332) ZPV(1),ZPV(3),RYRAD,RYAM(3)
9332  FORMAT(' ZPV1,3 RYRAD RYAM3 ',4E14.6)
C
1517  IF(IPRO.GT.0) WRITE(6,6411) IREGFL
6411  FORMAT(' MCJADE ELECTRON*: IREGFL IN LGMC56 CALL= ',I4)
      CALL LGMC56(0,PV(1),RYAM(1),IREGFL,1)
      GO TO 1000
C                             END SPECIAL SECTION HERE
C#######################################################################
C
C
5300  CALL TRCOIL(PV,RR,&992 )
C                                           SAVE VECTORS
  992 R0(1) = RR(1)
      R0(2) = RR(2)
      R0(3) = RR(3)
      GO TO 996
C
C                                           TRACK PHOTONS
    3 IF(IPRO.NE.0) WRITE(6,3751)
3751  FORMAT(' MCJADE PHOTON AND CALL TO JTRKGM ')
      CALL JTRKGM(PV,R0,&1000)
      GO TO 996
C                                           TRACK K0L , N
    4 CALL JTRNTR(PV,R0,&1000)
C                                           SAVE VECTORS
  996 R0SAVE(1) = R0(1)
      R0SAVE(2) = R0(2)
      R0SAVE(3) = R0(3)
      DO 995 ISA = 1,10
  995 PVSAVE(ISA) = PV(ISA)
C                                           LEAD GLASS
C           WITH SHOWER PROGRAM, THIS IS ONLY FOR MUONS AND HADRONS
      CALL TRLGL(PV,R0,&997)
C
  997 CONTINUE
C
  998 IF( PV(6) .LT. .001 ) GO TO 1000
C                                  MURTNE IS A DUMMY
      CALL MURTNE(PVSAVE,R0SAVE)
      GO TO 1000
C
 1004 IF( IJETCP .GT. 0 ) GO TO 1000
      IJETCP = 1
      WRITE(6,151) IEV, IPART
  151 FORMAT(5X,' TOO MANY JET-CHAMBER HITS AT EVENT',I5,' TRACK',I4)
      GO TO 1000
C
C
 1000 CONTINUE
C                               >>>>>>>  END OF TRACK LOOP  <<<<<<<<
      GO TO 1002
C
 1003 CONTINUE
C
C***********************************************************************
C                                           SORT VERTEX CHAMBER HITS
      IF( LFLAG(5) ) CALL JSTORE
C***********************************************************************
C
      CALL JSPOIN
      CALL ORDLG
      CALL MUORDR
      CALL TOFMAK
      WRITE(6,2335) IEV,NTR56
2335  FORMAT(' BEF WRTMCB, IEV NTR56 ',2I6)
C
      CALL WRTMCB( NUNIT )
      IWRT = IWRT + 1
C
 1001 CONTINUE
C                               >>>>>>>  END OF EVENT LOOP  <<<<<<<<
C
C                                           NO MORE EVENTS REQUESTED
C
      WRITE(6,9105)
 9105 FORMAT(/' --  END OF REQUESTED DATA  --')
      GO TO 91
C
C                                           TIME OUT
   90 CONTINUE
      WRITE(6,9101)
 9101 FORMAT(/' ========= TIME OUT =========')
      GO TO 91
C
C
C                                          OPERATOR STOP
C
   92 CONTINUE
      WRITE(6,9102)
 9102 FORMAT(/'======== OPERATOR STOP=========')
      GO TO 91
C
C                                           END OF DATA
   91 CONTINUE
      CALL BMLT( 0, DUMMY )
      CALL BWRITE( NUNIT )
C
C                                           PRINT INPUT ERROR SUMMARY
C
      CALL MCVSUM
C
      ILAST = IEVMIN + KIEV - 1
      WRITE(6,109) NEVTS, IEVMIN,ILAST,KIEV, IWRT
  109 FORMAT(//1X,I6,' EVENT(S) REQUESTED TO BE TRACKED.'/
     +  1X,I6,' IS RECORD NO. OF FIRST REQUESTED EVT FROM INPUT FILE.'/
     +  1X,I6,' IS RECORD NO. OF LAST EVENT READ FROM INPUT FILE.'/
     +  1X,I6,' EVENT(S) READ FROM THE INPUT FILE.'/
     +  1X,I6,' EVENT(S) WRITTEN OUT.'//)
      RETURN
      END
