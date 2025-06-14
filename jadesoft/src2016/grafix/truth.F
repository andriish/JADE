C   01/11/84 807251629  MEMBER NAME  TRUTH    (S)           FORTRAN
C
C-----------------------------------------------------------------------
      SUBROUTINE TRUTH(INDEX)
C-----------------------------------------------------------------------
C
C    AUTHOR:   L. O'NEILL  29/11/78 :  DRAW TRUE MC TRACKS
C
C       MOD:   J. OLSSON   07/05/81 :
C       MOD:   J. OLSSON   13/11/81 :
C       MOD:   J. OLSSON   16/11/81 :
C       MOD:   J. OLSSON   26/01/83 :  DARCOS ERROR...
C       MOD:   J. OLSSON   16/05/83 :  QUARKS,NEUTRINOS NOT DRAWN
C       MOD:   J. OLSSON   19/02/84 :  VRX,VRZX,VRZY VIEWS
C       MOD:   C. BOWDERY  08/06/84 :  NEW COMMAND NUMBERS
C       MOD:   J. HAGEMANN 19/10/84 :  VC VIEW
C       MOD:   J. HAGEMANN 06/03/86 :  DRAWING OF JET AXIS IN R-FI VIEWS
C       MOD:   J. HAGEMANN 14/04/86 :  CORRECT DISPLAY OF TRUE TRACKS IN
C                                      MAGNIFIED VERTEX VIEWS
C       MOD:   J. HAGEMANN 14/05/86 :  DRAWS TRUE TRACKS DASHED IN R-FI
C       MOD:   J. HAGEMANN 25/06/86 :  NEGATIVE INDEX DISPLAYS PARTICLE
C                                      NUMBER=+INDEX OF VECT-BANK
C                                      BANK NUMBER (0/1) IS SWITCHED BY
C                                      FLVCDO(13)
C  LAST MOD:   J. HAGEMANN 23/06/87 :  DISPLAY PARTICLE TYPE TABLE
C                                      (INDEX = 5)
C        A SUBROUTINE FOR DISPLAYING THE EXACT PATHS OF TRACKS AND
C        GAMMA RAYS WHEN SCANNING MONTE CARLO DATA. MULTIPLE SCATTER-
C        ING AND DE/DX LOSSES ARE NEGLECTED. "INDEX" IN THE ARGUMENT
C        CONTROLS DISPLAY OF TRACKS AND GAMMA RAYS SEPARATELY. INDEX=0
C        CAUSES TRACKS ONLY TO BE DISPLAYED. INDEX=1 RESULTS IN ONLY
C        GAMMA RAYS APPEARING. INDEX=2 CAUSES BOTH TRACKS AND GAMMAS
C        TO APPEAR.
C
C-----------------------------------------------------------------------
C
      IMPLICIT INTEGER*2 (H)
C
      LOGICAL FLVCDO
      LOGICAL FL18,FL22,FL24,DSPDTM
      REAL*8 B,C,DORG2,DTOF2,DCENT2,DRAD2
C
#include "cdata.for"
#include "cgrscl.for"
#include "cgraph.for"
#include "cgeo1.for"
C
      COMMON / CPROJ  / XMINR,XMAXR,YMINR,YMAXR,IPRJC,FL18,FL22,FL24
      COMMON / CGRAP2 / BCMD,DSPDTM(30),ISTVW,JTVW
      COMMON / CJTRIG / PI,TWOPI,PIHALF,PI3HALF
      COMMON / CGVCDO / FLVCDO(20)
C
      DIMENSION EAXIS(3),EOUT(3),SPHITY(3)
      DIMENSION HDATSV(22)
      DIMENSION HWRITE(40)
      EQUIVALENCE(IDATSV(1),HDATSV(1))
*** PMF 17/11/99: add variables needed for emulation of DESYLIB routine 'CORE'  
      CHARACTER cHWRITE*80
      EQUIVALENCE (cHWRITE,HWRITE(1))
*** PMF(end)
C
C------------------   C O D E  -----------------------------------------
C
      ISEL = 0
      IBKSEL = 0
      IF( INDEX .GE. 0 ) GO TO 50
         IPAR   = -INDEX
         INDEX  = 0
         ISEL   = 1
         IF( FLVCDO(13) ) IBKSEL = 1
   50 CONTINUE
      Z1 = 0.
      ITRNUM = 0
      IPAGE = 0
      BCONST=2.99792458E-5*BKGAUS
      BCONST = ABS(BCONST)
      IF(LASTVW.LT.12) GO TO 351
      IF(LASTVW.EQ.20) GO TO 351
      IF(LASTVW.GE.17.AND.LASTVW.LE.19) GO TO 351
      CALL TRMOUT(80,'OPTION NOT IMPLEMENTED IN CURRENT VIEW.^')
      RETURN
351   I4VPNT=IDATA(IBLN('VECT'))
      IF(I4VPNT.LT.1) CALL TRMOUT(80,'NO FOUR VECTOR BANK.^')
      IF(I4VPNT.LT.1) RETURN
C--
2222  LHEAD=IDATA(I4VPNT+1)
      LNUMB = IDATA(I4VPNT-2)
      LNEXT = IDATA(I4VPNT-1)
      LTR = IDATA(I4VPNT+2)
      NPART=IDATA(I4VPNT+4)
C     NPART=IDATA(I4VPNT+12)
C     IF(NPART.EQ.0) NPART=IDATA(I4VPNT+4)
      IF(NPART.LT.1) CALL TRMOUT(80,'NO MONTE CARLO PARTICLES.^')
      IF(NPART.LT.1) RETURN
      IF(LNUMB.EQ.0 .AND. IBKSEL.EQ.1) GOTO 2766
C---
C---  CODE FOR OPTION 4         DRAW JET AXIS
C---
      IF( INDEX .NE. 4 ) GO TO 777
         IF( LASTVW .GT. 4 .AND. LASTVW .NE. 17 .AND. LASTVW .NE. 20 )
     &                                         GO TO 780
            CALL EVTAX(EAXIS,EOUT,SPHITY,XMASS,IERR)
            IF( IERR .NE. 0 ) RETURN
            PHIJET = ATAN2(EAXIS(2),EAXIS(1))
            RAB    = RTOF - 50.0
            IF( LASTVW .EQ. 17 ) RAB = 6.0
            IF( LASTVW .EQ. 20 ) RAB = RITNK + 110.0
            XX     = RAB*COS(PHIJET)
            YY     = RAB*SIN(PHIJET)
            CALL MOVEA( -XX,  YY )
            CALL DASHA(  XX, -YY, 4 )
            RETURN
C---
C---  CODE FOR OPTION 5         PARTICLE TYPE TABLE
C---
  777 CONTINUE
      IF( INDEX .NE. 5 ) GO TO 780
          CONTINUE ! CALL CLRCON PMF 03/12/99
          WRITE(6,9771)
 9771 FORMAT(' JADE-TYPE :',/,
     &       ' 1 = PHOTON, 2 = ELECTRON, 3 = MUON, 4 = PION, 5 = KAON,'/
     &       ' 6 = NUCLEON, 7 = PHI, 8 = ETA, 9 = ETAP, 10 = OMEGA,'/
     &       ' 11 = K*(890), 12 = RHO, 22 = TAU, 50 = K0')
C
          RETURN
C---
C---  SET BOUNDARY PARAMETERS FOR THE DISPLAY
C---
 780  ZZPL = ZENDPL
      ZZMI = ZENDMI
      RFRF = RTOF
      DELL = 10.
      IF( LASTVW .LT. 17 .OR. LASTVW .GT. 19 ) GO TO 4933
         XYZLIM = 0.33*(XMAXST(17)-XMINST(17))
         ZZPL   = XYZLIM
         ZZMI   = -XYZLIM
         RFRF   = XYZLIM
         DELL   = XYZLIM*.01
4933  CONTINUE
C---
C---     LOOP OVER ALL PARTICLES IN FINAL STATE.
C---
      EPTOT=0.
      ECTOT=0.
      ENTOT=0.
      DO 1 IPART=1,NPART
C---
C---     SELECT VECT-0-BANK PARTICLE IF INDEX WAS NEGATIVE
C---
      IF( ISEL .EQ. 1  .AND.  LNUMB .NE. IBKSEL ) RETURN
      IF( ISEL .EQ. 1  .AND.  IPART .NE. IPAR ) GO TO 1
C---
C---     SET UP INITIAL MOMENTUM VECTOR AND POINT OF ORIGIN.
C---
      IOFF=I4VPNT+LHEAD+LTR*(IPART-1)
      AMASS=ADATA(IOFF+5)
      ICHRG=IDATA(IOFF+6)
      ITYPE=IDATA(IOFF+7)
      ICSIGN=1
      IBCHRG=1
      IF(ICHRG.LT.0) ICSIGN=-1
      IF(ICHRG.LT.0) IBCHRG=-1
      IF(BKGAUS.LT.0.) ICSIGN = - ICSIGN
      IF(BKGAUS.LT.0.) IBCHRG = - IBCHRG
      PX=ADATA(IOFF+1)
      PY=ADATA(IOFF+2)
      PZ=ADATA(IOFF+3)
      EPRT=ADATA(IOFF+4)
      EPTOT=EPTOT+EPRT
      IF(ICHRG.NE.0) ECTOT=ECTOT+EPRT
      IF(ICHRG.EQ.0) ENTOT=ENTOT+EPRT
      XORG=ADATA(IOFF+ 8)
      YORG=ADATA(IOFF+ 9)
      ZORG=ADATA(IOFF+10)
      ZVERT=ZORG
      IF(INDEX.NE.3) GO TO 25
C   ENTER HERE FOR WRITING RESULTS NUMERICALLY ON SCREEN
C
C---
C---     WRITE OUT PARTICLE DATA TO SCREEN, WITH HARDCOPY OPTION.
C---
      IPAGE=IPAGE + 1
      IF(IPAGE.NE.1) GO TO 26

c      call hdcopy

      CALL ERASE
      CALL HOME

      do 888 i=1,3
 888     call newlin

      CALL CORE(HWRITE,80)
      WRITE(cHWRITE,446) ICREC  ! PMF 17/11/99: JUSCRN changed to cHWRITE
  446 FORMAT(' RECORD NO.',I5)
      DO 27 I=1,8
      J=9-I
      HWRITE(J+22)=HWRITE(J)
   27 CONTINUE
      DO 28 I=1,22
      HWRITE(I)=HDATSV(I)
   28 CONTINUE
      CALL NEWLIN
      CALL EOUTST(60,HWRITE)
      CALL LINEF
   26 CONTINUE
      CALL CORE(HWRITE,80)
      WRITE(cHWRITE,445)
     + IPART,ITYPE,ICHRG,AMASS,XORG,YORG,ZORG,PX,PY,PZ,EPRT  ! PMF 17/11/99: UNIT=6 changed to cHWRITE
  445 FORMAT(3I3,F9.4,F10.2,2F8.2,F10.4,2F8.4,F10.4)
      CALL NEWLIN
      CALL EOUTST(80,HWRITE)
      IF(IPART.NE.NPART.AND.IPAGE.NE.50) GO TO 1
      IF(IPART.NE.NPART) GO TO 29
      CALL CORE(HWRITE,80)
      WRITE(cHWRITE,447) EPTOT,ECTOT,ENTOT  ! PMF 17/11/99: UNIT=6 changed to cHWRITE
  447 FORMAT('   EPTOT, ECTOT, ENTOT = ',3F10.4)
      CALL NEWLIN
      CALL NEWLIN
      CALL EOUTST(55,HWRITE)
   29 CONTINUE
      CALL TRMOUT(80,'
     1                  HARDCOPY?^')
      CALL DECIDE(IANSW)
      IF(IANSW.EQ.1) CALL HDCOPY
      IPAGE = 0
      GO TO 1
   25 CONTINUE
C   ENTER HERE FOR DISPLAY OF TRACKS AND GAMMAS IN THE EVENT
C
C---
C---     IF WE ARE USING THE "WRAP AROUND" TRACK DISPLAY AND
C---     IF PARTICLE TRACK STARTS EXTREMELY CLOSE TO THE BEAM AXIS SET
C---     ITS ORIGINAL X,Y COORDINATES TO TRUE ZEROES. THIS SIMPLY AVOIDS
C---     NUMERICAL DIFFICULTIES LATER ON.
C---
      IF(LASTVW.LE.3.OR..NOT.DSPDTL(9).OR.LASTVW.EQ.20) GO TO 23
      IF(LASTVW.EQ.17) GO TO 23
      IF(ABS(XORG).GT.0.0001*(XMAX-XMIN)) GO TO 23
      IF(ABS(YORG).GT.0.0001*(XMAX-XMIN)) GO TO 23
      XORG=0.
      YORG=0.
   23 CONTINUE
      KINK1=0
      KINK2=0
C---
C---     REQUIRE THAT PARTICLES ORIGINATE WITHIN AND NOT CLOSER THAN
C---     ONE CENTIMETER TO THE BOUNDARY OF A CYLINDER DEFINED BY
C---     THE RADIUS OF THE TIME-OF-FLIGHT COUNTERS AND THE Z COORDINATES
C---     OF THE FACES OF THE LEAD GLASS END CAPS. PARTICLES WILL BE
C---     TRACKED AS FAR AS THE BOUNDARY OF THIS CYLINDER.
C---     THE RADIUS CUT IS MADE SEVERAL LINES LOWER.
C---
      IF(ZORG.LE.ZZMI+DELL) GO TO 1
      IF(ZORG.GE.ZZPL-DELL) GO TO 1
      PT2=PX**2+PY**2
      PT=SQRT(PT2)
      IF(PT.LT.0.01) GO TO 1
      PA=SQRT(PT2+PZ**2)
      DORG2=XORG**2+YORG**2
      RORG2=DORG2
      RORG=SQRT(RORG2)
      DTOF2=RFRF**2
      IF(RORG.GT.RFRF-DELL) GO TO 1
C---
C---     DEAL FIRST WITH THE GAMMA RAYS.
C---
      IF(ICHRG.NE.0) GO TO 2
      IF(ITYPE.NE.1.AND.ITYPE.NE.-1) GO TO 1
      IF(INDEX.EQ.0) GO TO 1
      RLEN=0.
      DRIFT=1000000.
      IF(PZ.LT.0.) DRIFT=(ZZMI-ZORG)*PA/PZ
      IF(PZ.GT.0.) DRIFT=(ZZPL-ZORG)*PA/PZ
      XIMPCT=XORG+PX/PA*DRIFT
      YIMPCT=YORG+PY/PA*DRIFT
      RIMPCT=SQRT(XIMPCT**2+YIMPCT**2)
      IF(RIMPCT.LE.RFRF) RLEN=DRIFT
      IF(RLEN.GT.0.) GO TO 3
C---
C---    THE GAMMA WILL HIT THE TOF COUNTERS RATHER THAN AN END CAP.
C---
      B=2.*(XORG*PX+YORG*PY)/PT
      C=DORG2-DTOF2
      RLEN=(-B+DSQRT(B**2-4.*C))/2.
      RLEN=RLEN*PA/PT
    3 CONTINUE
C---
C---     THE GAMMA RAYS HITTING THE END CAPS JOIN THE FLOW HERE.
C---
      XEND=XORG+RLEN*PX/PA
      YEND=YORG+RLEN*PY/PA
      ZEND=ZORG+RLEN*PZ/PA
    4 CONTINUE
C ENTER HERE ALSO WITH CHARGED TRACKS COMING FROM LOOP 12
      IF(LASTVW.EQ.20) GO TO 30
      IF(LASTVW.GT.3.AND.LASTVW.NE.17) GO TO 5
C RFI VIEWS HERE
   30 X0=-XORG
      Y0= YORG
      X1=-XEND
      Y1= YEND
      GO TO 7
    5 CONTINUE
C RZ VIEWS HERE
      X0=ZORG
      X1=ZEND
      IF(LASTVW.GT.7.AND.LASTVW.NE.18) GO TO 6
      Y0=XORG
      Y1=XEND
      Z0=YORG
      Z1=YEND
      GO TO 8
    6 CONTINUE
      Y0=YORG
      Y1=YEND
      Z0=XORG
      Z1=XEND
    8 CONTINUE
      IF(.NOT.DSPDTL(9)) GO TO 7
C---
C---     TAKE CARE OF THE ABSOLUTE RADIUS VERSUS Z PROJECTION CASE.
C---
      SIGN0=1.
      IF(Y0.LT.0.) SIGN0=-1.
      SIGN1=1.
      IF(Y1.LT.0.) SIGN1=-1.
      ICROSS=0
      IF(SIGN1.NE.SIGN0) ICROSS=1
      IF(ICROSS.EQ.0) GO TO 13
      Y2=0.
      FRAC=(Y2-Y0)/(Y1-Y0)
      X2=X0+FRAC*(X1-X0)
   13 CONTINUE
      Y0=SIGN0*SQRT(Y0**2+Z0**2)
      Y1=SIGN1*SQRT(Y1**2+Z1**2)
      IF(ICROSS.EQ.0) GO TO 7
      Y2=Y0+FRAC*(-Y1-Y0)
C THE DRAWING HERE IS ONLY IN RZ VIEWS
      IF(ICHRG.EQ.0) CALL SAEGE(X0,Y0,X2,Y2)
      IF(ICHRG.NE.0) CALL MOVEA(X0,Y0)
      IF(ICHRG.NE.0) CALL DRAWA(X2,Y2)
      call iuwk(1)
      Y3=-Y2
      IF(ICHRG.EQ.0) CALL SAEGE(X2,Y2,X2,Y3)
      IF(ICHRG.NE.0) CALL DRAWA(X2,Y3)
      X0=X2
      Y0=Y3
      call iuwk(1)
    7 CONTINUE
C---
C---     CHECK HERE ON THE CHARGE BECAUSE CHARGED PARTICLE TRACKING
C---            FROM LOOP 12
C---     WILL JUMP BACK TO STATEMENT 4 AND USE THE SAME CODING.
C---
      IF(ICHRG.NE.0) GO TO 9
C---
C---     FINISHED WITH THE GAMMA RAY. DRAW SAWTOOTH SYMBOL ON SCREEN.
C---
      CALL SAEGE(X0,Y0,X1,Y1)
C     CALL TRKNM(X0,Y0,X1,Y1,IPART)
      CALL TRNUMB(IPART,ITRNUM,-X1,Y1,Z1)
C GO TO 1  MEANS END OF LOOP, READY WITH THIS PARTICLE
      GO TO 1
    2 CONTINUE
C ENTER HERE WITH CHARGES,ICHARG = 0 SKIPPED IF ONLY CHARGES REQUESTED
      IF(ICHRG.NE.1.AND.ICHRG.NE.-1) GO TO 1
      IF(IABS(ITYPE).GT.99) GO TO 1
C---
C---     NOW DEAL WITH THE CHARGED CASE. WERE CHARGED TRACKS REQUESTED?
C---
      IF(INDEX.EQ.1) GO TO 1
C---
C---     LOCATE THE CENTER OF GYRATION IN THE X-Y PLANE. B FIELD
C---     ALONG THE +Z AXIS.
C---
C     XCENT=XORG+PY/BCONST/ICHRG
C     YCENT=YORG-PX/BCONST/ICHRG
      XCENT=XORG+PY/BCONST/IBCHRG
      YCENT=YORG-PX/BCONST/IBCHRG
      DCENT2=XCENT**2+YCENT**2
      RCENT2=DCENT2
      RCENT=SQRT(RCENT2)
      RAD=PT/BCONST/IABS(ICHRG)
      DRAD2=RAD**2
      RAD2=DRAD2
C---
C---     SET UP INITIAL AZIMUTH WITH RESPECT TO CENTER OF LARMOR ORBIT.
C---
      PHI0=ATAN2(PY,PX)+ICSIGN*PI/2.
      IF(PHI0.GE. PI) PHI0=PHI0-TWOPI
      IF(PHI0.LT.-PI) PHI0=PHI0+TWOPI
      ANG0=ATAN2(YCENT,XCENT)
      ANG1=ANG0-PI
      IF(ANG1.LT.(-PI)) ANG1=ANG1+TWOPI
C---
C---     CAN THE TRACK POSSIBLY REACH THE TOF COUNTERS? IF NOT, BRANCH
C---     TO THE CASE WHERE TRACKS STRIKE THE END CAPS. ADD ONE
C---     CENTIMETER TO THE RADIUS OF THE TOF COUNTERS TO AVOID NUMERICAL
C---     DIFFICULTIES. PARTICLES WHICH JUST BARELY PENETRATE THE TOF
C---     COUNTERS WILL BE TRANSPORTED TO THE END CAPS ANYWAY.
C---
      IF(2.*RAD+RORG.LE.RFRF+DELL) GO TO 10
C---
C---     THE TRACK MAY OR MAY NOT REACH THE TOF COUNTERS BEFORE
C---     STRIKING AN END CAP.
C---
      DELTAA=PI
      B=(DTOF2-DCENT2-DRAD2)/(2.*RAD*RCENT)
      IF(RCENT.LE.10.) GO TO 2821
      IF(DABS(B).LT.1.0) DELTAA = DARCOS(B)
2821  SOL1=ANG0-DELTAA
      SOL2=ANG0+DELTAA
      IF(SOL1.LT.-PI) SOL1=SOL1+TWOPI
      IF(SOL1.GE. PI) SOL1=SOL1-TWOPI
      IF(SOL2.LT.-PI) SOL2=SOL2+TWOPI
      IF(SOL2.GE. PI) SOL2=SOL2-TWOPI
C---
C---     "SOL1" AND "SOL2" ARE THE TWO AZIMUTHAL ANGLES ABOUT THE
C---     CENTER OF THE LARMOR ORBIT AT WHICH THE TRACK WOULD INTERSECT
C---     THE TOF COUNTERS IF IT MOVED COMPLETELY AROUND A CIRCLE.
C---     NOW WE WILL ATTEMPT TO SELECT THE CORRECT ONE FOR THE ACTUAL
C---     CASE.
C---
      DIST1=SOL1-PHI0
      IF(DIST1.LT.0.) DIST1=DIST1+TWOPI
      DIST2=SOL2-PHI0
      IF(DIST2.LT.0.) DIST2=DIST2+TWOPI
C---
C---     "DIST1" AND "DIST2" ARE THE ABSOLUTE ANGULAR DISTANCES, NOT
C---     RESTRICTED TO VALUES BETWEEN -PI AND +PI, WHICH A POINT MUST
C---     MOVE ALONG THE LARMOR ORBIT FROM THE POINT OF ORIGIN OF THE
C---     TRACK TO THE INTERSECTIONS WITH THE TOF COUNTERS, IF THIS POINT
C---     MOVES IN THE DIRECTION OF INCREASING AZIMUTHAL ANGLE ABOUT
C---     THE CENTER OF THE ORBIT. AN ACTUAL PARTICLE MOVING IN THE
C---     SAME SENSE (COUNTER CLOCKWISE IN THE CONVENTIONAL X-Y PLANE
C---     BUT CLOCKWISE ON OUR PICTURE WITH THE +X AXIS RUNNING TOWARD
C---     THE LEFT) WILL ENCOUNTER THE TOF COUNTERS AT AN AZIMUTH
C---     CORRESPONDING TO THE SMALLER OF DIST1 AND DIST2.
C---
      SCCW=SOL1
      SCW =SOL2
      IF(DIST2.LT.DIST1) SCCW=SOL2
      IF(DIST2.LT.DIST1) SCW =SOL1
C---
C---     POSITIVE PARTICLES MOVE CLOCKWISE IN THE CONVENTIONAL
C---     X-Y PLANE, COUNTERCLOCKWISE IN OUR PICTURE.
C---
C---        ONLY TRUE IF BKGAUS > 0  !!   (J.O.)
C---
      SOL=SCW
C     IF(ICHRG.LT.0) SOL=SCCW
      IF(IBCHRG.LT.0) SOL=SCCW
C THE ABOVE STATEMENT PUT IN BY J.O., TO ACCOUNT FOR NEGATIVE FIELD
      DIST=-ICSIGN*(SOL-PHI0)
      IF(DIST.LT.0.) DIST=DIST+TWOPI
C---
C---     "DIST" IS NOW SUPPOSED TO BE THE ABSOLUTE VALUE OF THE
C---     AZIMUTHAL ANGLE TRAVERSED BY THE PARTICLE IN GOING FROM
C---     THE POINT OF ORIGIN TO THE TOF RADIUS.
C---
C---     DOES THE PARTICLE ENCOUNTER AN END CAP BEFORE ACTUALLY REACHING
C---     THE RADIUS OF THE TOF COUNTERS?
C---
      ZIMPCT=ZORG+PZ/PT*RAD*DIST
      IF(ZIMPCT.LT.ZZMI) GO TO 10
      IF(ZIMPCT.GT.ZZPL) GO TO 10
      GO TO 11
   10 CONTINUE
C---
C---     ALL PARTICLES STRIKING THE END CAPS MERGE AT THIS POINT.
C---
      ZDRIFT=0.
      IF(PZ.LT.-0.001) ZDRIFT=ZZMI-ZORG
      IF(PZ.GT. 0.001) ZDRIFT=ZZPL-ZORG
      DIST=TWOPI
      IF(ZDRIFT.NE.0.) DIST=ZDRIFT*PT/PZ/RAD
   11 CONTINUE
C---
C---     NOW SET UP STEP SIZE FOR DRAWING THE ORBIT ONTO THE SCREEN
C---     AND TAKE CARE THAT ONE STEP ALWAYS OCCURS AT THE MINIMUM
C---     DISTANCE OF THE ORBIT FROM THE BEAM AXIS. THE LATTER IS
C---     DONE FOR THE BENEFIT OF THE Z-ABSOLUTE VALUE OF R VIEW.
C---
      PHI1=PHI0-ICSIGN*DIST
      IF(LASTVW.EQ.20) GO TO 34
      IF(LASTVW.GT.3.AND.LASTVW.NE.17) GO TO 14
   34 Q=RAD/8.
      GO TO 17
   14 CONTINUE
      Q=0.
      ZK=RAD*PZ/PT
      IF(ABS(ZK).LT.1E-3) GO TO 17
      MAX=0
      IF(DSPDTL(9)) GO TO 16
      IF(LASTVW.GT.7.AND.LASTVW.NE.18) GO TO 15
      PLIM1=-PI
      PLIM2=0.
      IF(PHI0.GE.0.) PLIM1=0.
      IF(PHI0.GE.0.) PLIM2=PI
      GO TO 18
   15 CONTINUE
      PLIM1=-PI
      PLIM2=-PI/2.
      IF(PHI0.GE.(-PI/2.)) PLIM1=-PI/2.
      IF(PHI0.GE.(-PI/2.)) PLIM2= PI/2.
      IF(PHI0.GE.( PI/2.)) PLIM1= PI/2.
      IF(PHI0.GE.( PI/2.)) PLIM2= PI
   18 CONTINUE
      IF(PHI1.LT.PLIM1) MAX=1
      IF(PHI1.GE.PLIM2) MAX=1
      IF(MAX.EQ.1) Q=RAD/8.
      IF(MAX.EQ.1) GO TO 17
      DOFF=0.
      IF(LASTVW.GT.7.AND.LASTVW.NE.18) DOFF=PI/2.
      DO 19 I=1,2
      S=PHI0-ICSIGN*DIST*(I-1)+DOFF
      Q1=ZK*RAD*COS(S)/(8.*SQRT(ZK**2+(RAD*SIN(S))**2))
      IF(ABS(Q1).GT.Q) Q=ABS(Q1)
   19 CONTINUE
      GO TO 17
   16 CONTINUE
      PLIM1=ANG1-TWOPI
      PLIM2=ANG1
      IF(PHI0.GT.ANG1) PLIM1=ANG1
      IF(PHI0.GT.ANG1) PLIM2=ANG1+TWOPI
      IF((PHI1.GT.PLIM1).AND.(PHI1.LT.PLIM2)) GO TO 20
      IF((XORG.EQ.0.).AND.(YORG.EQ.0.)) GO TO 20
      KINK1=1
      IF(DSQRT(DABS(DCENT2-DRAD2)).LT.100.) GO TO 20
      KINK1=0
      Q=ABS(RCENT*RAD/(8.*(RCENT-RAD)))
      GO TO 17
   20 CONTINUE
      PLIM1=ANG0-TWOPI
      PLIM2=ANG0
      IF(PHI0.GT.ANG0) PLIM1=ANG0
      IF(PHI0.GT.ANG0) PLIM1=ANG0+TWOPI
      IF((PHI1.GT.PLIM1).AND.(PHI1.LT.PLIM2)) GO TO 21
      Q=RCENT*RAD/(8.*(RCENT+RAD))
   21 CONTINUE
      DO 22 I=1,2
      S=PHI0-ICSIGN*DIST*(I-1)
      B=-XCENT*RAD*SIN(S)+YCENT*RAD*COS(S)
      C=-XCENT*RAD*COS(S)-YCENT*RAD*SIN(S)
      ARGM=DCENT2+DRAD2-2.*C
      IF(ARGM.LE.0.) GO TO 22
      Y=SQRT(ARGM)
      IF((XORG.EQ.0.).AND.(YORG.EQ.0.)) GO TO 22
      KINK2=1
      RADIPN=DSQRT(DABS(DCENT2-DRAD2))
      IF((RADIPN.LT.100.).AND.(Y.LT.RADIPN)) GO TO 22
      KINK2=0
      Q1=(-ZK*B*B+ZK*Y*Y*C)/(8.*DSQRT(ZK*ZK*(Y**6)+(Y**4)*B*B))
      IF(ABS(Q1).GT.Q) Q=ABS(Q1)
   22 CONTINUE
   17 CONTINUE
      STEP=PI
      IF(Q.NE.0.) STEP=0.01*SQRT((XMAX-XMIN)/Q)
C---
C---     IN CASE WE ARE IN THE "WRAP AROUND" VIEW AND IN CASE A REGION
C---     OF VERY HIGH CURVATURE NEAR THE BEAM AXIS HAS BEEN IGNORED IN
C---     SETTING "Q" MAKE SURE THAT THE STEP SIZE IS NO MORE THAN 100
C---     MILLIMETERS. THIS ASSURES THAT THERE IS AT LEAST ONE FULL STEP
C---     OUTSIDE THE REGION OF HIGH CURVATURE BUT STILL INSIDE THE BEAM
C---     PIPE AND THUS THAT THE TRACKS INSIDE THE JET CHAMBERS ARE STILL
C---     WELL FITTED.
C---
      IF((KINK1.EQ.0).AND.(KINK2.EQ.0)) GO TO 24
      STEPP=100./RAD
      IF(STEP.GT.STEPP) STEP=STEPP
   24 CONTINUE
      MODSTP=TWOPI/STEP+1
      IF(STEP.GT.PI) STEP=PI
      IF(STEP.LT.PI) STEP=TWOPI/MODSTP
      IF(LASTVW.GE.17.AND.LASTVW.LE.19) STEP = STEP*.005
      DIST1=-ICSIGN*(ANG1-PHI0)
      IF(DIST1.LT.0.) DIST1=DIST1+TWOPI
      NSTEP1=DIST1/STEP+1
      SHFTP=DIST1-NSTEP1*STEP
      NSTEP=(DIST-SHFTP)/STEP+2
      STEP=-ICSIGN*STEP
      SHFTP=-ICSIGN*SHFTP
C---
C---     NOW STEP ALONG THE TRACK AND DRAW THE CURVE ON THE SCREEN.
C---
      RRRF1 = 0.
      DO 12 ISTEP=2,NSTEP
      PHI=PHI0+SHFTP+STEP*(ISTEP-1)
      IF(ISTEP.EQ.NSTEP) PHI=PHI1
      XEND=XCENT+RAD*COS(PHI)
      YEND=YCENT+RAD*SIN(PHI)
      ZEND=ZVERT+PZ/PT*RAD*ABS(PHI-PHI0)
C---
C---     JUMP BACKWARDS INTO THE GAMMA CODING TO TAKE CARE OF THE
C---     SCREEN COORDINATES
C---
      GO TO 4
    9 CONTINUE
      IF(ISTEP.EQ.2) CALL MOVEA(X0,Y0)
CHECK TO SEE IF ALREADY OUTSIDE
      IF(LASTVW.LT.17.OR.LASTVW.GT.19) GO TO 5213
      RRRF = SQRT(X1*X1 + Y1*Y1)
      IF(RRRF.LE.RFRF) GO TO 5213
      RATF = (RFRF-RRRF1)/(RRRF-RRRF1)
      X1 = X0 + RATF*(X1-X0)
      Y1 = Y0 + RATF*(Y1-Y0)
5213  CALL DASHA(X1,Y1,1)
      call iuwk(0)
C      IF(DSPDTM(30)) WRITE(6,492) X1,Y1,ZEND,STEP,RRRF,RFRF
C492   FORMAT(' XY1 ZEND STEP RRRF RFRF',6E12.4)
      IF(RRRF.GT.RFRF) GO TO 1212
      XORG=XEND
      YORG=YEND
      ZORG=ZEND
      RRRF1=RRRF
   12 CONTINUE
 1212 CONTINUE
C     CALL TRKNM(X0,Y0,X1,Y1,IPART)
      CALL TRNUMB(IPART,ITRNUM,-X1,Y1,Z1)
    1 CONTINUE
 2766 IF(LNEXT.EQ.0) GO TO 1111
      I4VPNT = LNEXT
C CHANGE SIZE OF TRACK NUMBER FOR SECOND VECT BANK, VARIABLE ITRNUM
      ITRNUM = 1
      GO TO 2222
1111  RETURN
      END
