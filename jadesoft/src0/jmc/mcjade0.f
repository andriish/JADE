C   24/08/85 606091709  MEMBER NAME  MCJADE0  (S)           FORTRAN
C   23/08/85            MEMBER NAME  MCJADE   (S)           FORTRAN
C   25/11/83 402061220  MEMBER NAME  MCJADE   (S)           FORTRAN
C
C-----------------------------------------------------------------------
      SUBROUTINE MCJADE( NREVS, IPRINT )
C-----------------------------------------------------------------------
C
C   AUTHOR:   E. ELSEN    11/05/79 :  MC TRACKING PROGRAM
C
C      MOD  E. ELSEN    10/06/82 :
C      MOD  J. OLSSON   30/08/83 :  SUBSET TRACKED & EVENT COUNTING
C      MOD  W. BARTEL   25/10/83 :  PARTICLES BEYOND COIL NOT TRACKED
C      MOD  C. BOWDERY  17/11/83 :  CALL TO MCVSUM FOR ERROR SUMMARY
C      MOD  C. BOWDERY  24/11/83 :  DELETE CALL TO TRCOIL FOR PHOTONS
C      MOD  W. BARTEL   29/11/83 :  IEVMIN/MAX (/CIEVS/) DEFLT. IN BLDAT
C      MOD  W. BARTEL   29/11/83 :  INTRODUCE N AND K0L TRACKING
C      MOD  W. BARTEL   29/11/83 :  EXTERNAL JADEBD  INSTEAD OF BLDAT
C LAST MOD  A.FINCH     24/08/85 :  FWDDET REPLACED BY MCTAGM
C LAST MOD  J.OLSSON    09/06/86 :  LFLAG(2) IS TRUE IF LFLAG(4) IS TRUE
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
      IMPLICIT INTEGER*2 (H)
C                                      JADE BLOCK DATA IS NOW JADEBD
      EXTERNAL JADEBD
C
      COMMON / CJTCDC / IBANK, IPART
      COMMON / CIEVS  / KIEV,IEVMIN,IEVMAX
      COMMON / CGEO1  / BKGAUS, RPIP,DRPIP,XRLPIP, RBPC,DRBPC,XRLBPC,
     *                  RITNK,DRITNK,XRLTKI, R0ROH,DR0ROH,XR0ROH,
     *                  R1ROH,DR1ROH,XR1ROH, R2ROH,DR2ROH,XR2ROH,
     *                  R3ROH,DR3ROH,XR3ROH, ROTNK,DROTNK,XRLTKO,
     *                  RTOF,DRTOF,XRTOF, RCOIL, DRCOIL, XRCOIL,
     *                  ZJM,DZJM,XRZJM, ZJP,DZJP,XRZJP,
     *                  ZTKM,DZTKM,XRZTKM, ZTKP,DZTKP,XRZTKP,
     *                  ZBPPL,ZBPMI,ZTOFPL,ZTOFMI,
     *                  XRJETC,
     *                  RLG,ZLGPL,ZLGMI,OUTR2,CTLIMP,CTLIMM,DELFI,
     *                  BLXY,BLZ,BLDEP,ZENDPL,ZENDMI,DEPEND,
     *                  XHOL1,XHOL2,YHOL1,YHOL2
C
      DIMENSION PV(10),R0(5),RR(5)
      DIMENSION PVSAVE(10),R0SAVE(3)
C
      DATA NSEC / 4 /
      DATA NUNIT / 2 /
C
      LOGICAL*1 LFLAG
      COMMON/CFLAG/LFLAG(10)
C
C        LFLAG(1) = SMEAR GAMMA AND ELECTRON ENERGIES
C        LFLAG(2) = GAMMA CONVERSION IN OUTER TANK AND COIL (TRKGAM)
C        LFLAG(3) = ABSORPTION LOSSES
C        LFLAG(4) = 3 DIM SHOWER PROFILE FIT TO EGGS CODE
C
C
C------------------  C O D E  ------------------------------------------
C
C                                           PRINT NEWS
      CALL MCNEWS
C
C   LFLAG(2) MUST BE TRUE IF MEIER/MAGNUSSEN LG TRACKING IS USED
C
      IF(LFLAG(4).AND..NOT.LFLAG(2)) WRITE(6,681)
681   FORMAT(' ### WARNING ###  LFLAG(2) SET TRUE, SINCE LFLAG(4) TRUE')
      IF(LFLAG(4).AND..NOT.LFLAG(2)) LFLAG(2) = .TRUE.
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
      DO 1001 IEV=1,NEVTS
      IF( JUHR(NSEC) .NE. 2 ) GO TO 1011
      IF( JUHR(NSEC) .NE. 2 ) GO TO 1011
      GO TO 90
C                                           INITIALISE EVENT
1011  CALL EVTINI
      IJETCP = 0
      IFWHIT = 0
      IPRI = 1
      IF( IWRT .GT. IPRINT-1 ) IPRI= 0
C                                           GET EVENT
      CALL JGETEV( &91 )
C
 1002 CONTINUE
C                                           NEXT PARTICLE
      CALL JGETVC( PV, R0, ICODE, IPRI )
      IF( ICODE .GT. 0 ) GO TO 1003
C
C                                           CHECK TIME FOR PARTICLES
C                                           NECESSARY IF MUONS INCLUDED
      IF( JUHR(NSEC) .NE. 2 ) GO TO 1012
      IF( JUHR(NSEC) .NE. 2 ) GO TO 1012
      KIEV = KIEV - 1
      GO TO 90
C
C     DON'T TRACK PARTICLES WHICH START ONLY BEYOND THE COIL OR
C     THE END CAP LEAD GLASS ARRAY
C                                   OCT. 25. 1983 W.BARTEL
C
1012  R4 = SQRT( R0(1)*R0(1) + R0(2)*R0(2) )
      IF( R4 .GT. RCOIL) GO TO 1000
      IF( ABS(R0(3)) .GT. ZENDPL) GO TO 1000
C
C                                           FWD TRACKING
C     CALL FWDDET( PV, R0, IFWHIT )
      CALL MCTAGM( PV, R0 )
C                                           BRANCH ACCORDING
C                                           TO CHARGE OF PARTICLE
C                                           PHOTONS
      IF( PV(7) .EQ. 0. .AND. PV(8).LT.2.) GO TO 3
C                                           BRANCH OFF K0L AND N
      IF( PV(7) .EQ. 0. .AND. ABS(PV(8)-5.) . LT. 1.E-5) GO TO 4
      IF( PV(7) .EQ. 0. .AND. ABS(PV(8)-6.) . LT. 1.E-5) GO TO 4
C
      CALL TRCDET(PV,R0,RR,&1004)
      IF( PV(6) .LT. .001 ) GO TO 1000
C
      CALL TRCOIL(PV,RR,&992 )
  992 R0(1) = RR(1)
      R0(2) = RR(2)
      R0(3) = RR(3)
      GO TO 996
C
C                                           TRACK PHOTONS
    3 CALL JTRKGM(PV,R0,&1000)
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
      CALL TRLGL(PV,R0,&997)
C
  997 CONTINUE
C
  998 IF( PV(6) .LT. .001 ) GO TO 1000
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
      GO TO 1002
C
 1003 CONTINUE
C
      CALL JSPOIN
      CALL ORDLG
      CALL MUORDR
      CALL TOFMAK
      CALL WRTMCB( NUNIT )
      IWRT = IWRT + 1
C
 1001 CONTINUE
C                                           NO MORE EVENTS REQUESTED
C
      WRITE(6,9105)
 9105 FORMAT(/' --  END OF REQUESTED DATA  --')
      GO TO 91
C
C                                           TIME OUT
C
   90 CONTINUE
      WRITE(6,9101)
 9101 FORMAT(/' ========= TIME OUT =========')
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
