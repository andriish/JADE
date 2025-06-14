C   01/10/83 805261727  MEMBER NAME  MUFFLY   (JADEMUS)     FORTRAN
C
C LAST CHANGE  9.40 14/04/88 J. HAGEMANN - CORRECT CALC. OF EXTR. ERRORS
C      CHANGE 02.30 22/11/83 HUGH MCCANN  - PROTECT MHREG AND ARRAYS.
C      CHANGE 22.00 22/10/83 HUGH MCCANN  - FOR YOKE END PLATES.
C                                           AND INDENT SOME OF CODE.
C      CHANGE 03.00 14/07/83 HUGH MCCANN  - VARIOUS MODIFICATIONS.
C      CHANGE 18.31 21/03/83 CHRIS BOWDERY- NEW CHAMBER TWIST CORRECTION
C      CHANGE 12.00 12/01/83 HUGH MCCANN  - CHANGE NTHIS > 10 MESSAGES.
C      CHANGE 19.05 16/03/82 CHRIS BOWDERY- LET-OFF CHECK(LOCH)
C      CHANGE 14.30 04/02/82 HUGH MCCANN  - INCREASE KDEPTH LIMIT TO 4.
C      CHANGE 12.15 19/10/81 HUGH MCCANN  - MAKING ALLOWANCE FOR OVER-
C                                           WRITTEN HIT1'S + FIX HTC.
C      CHANGE 21.40 05/10/81 HUGH MCCANN  - OVERLAP HITS.
C      CHANGE 14.00 17/06/81 JOHN ALLISON - REFITTING, ETC.
C      CHANGE 07.30 10/04/81 HUGH MCCANN  - JADEMUS UPDATE / 2 GLOBALS.
C-----------------------------------------------------------------------
      SUBROUTINE MUFFLY(HC,NHITS,NWHIT,HLUN,HTC,HAMB,NTPH,*)
C-----------------------------------------------------------------------
C
C          THIS SUBROUTINE TRACKS THROUGH THE REGIONS FOUND BY MUREGY
C          TO ASSOCIATE HITS. DEVIATIONS ARE STORED FOR THE 'LET-OFF'
C          CHECK.
C
C IDTRK   IS INNER DETECTOR TRACK NUMBER.
C NCEPTS  IS THE NUMBER OF INTERCEPTS FOUND BY MUREGY.
C NTHIS   IS NUMBER OF HITS ASSOCIATED WITH THIS TRACK.
C HC      CONTAINS MUON HIT COORDINATES.
C NHITS   IS THE NUMBER OF MUON HITS.
C NWHIT   IS NUMBER OF WORDS PER HIT IN HIT COORDINATE ARRAY.
C HLUN    IS HIT STATUS ARRAY.
C HTC     IS HIT-TRACK CORELLATION ARRAY - FILLED BY MUFFLE.
C HAMB    IS AMBIGUITY FLAG FOR EACH ENTRY IN HTC.
C NTPH    IS NUMBER OF TRACKS PER HIT ALLOCATED IN HTC.
C INEFF   IS NO. OF TIMES A HIT WAS NOT FOUND AND THERE WAS NO
C           DEAD CHAMBER TO EXPLAIN ITS ABSENCE.
C NHLAYR  IS RELATED TO THE NO. OF LAYERS WITH HITS WHICH ARE
C           ASSOCIATED WITH THIS TRACK.
C INFLAG  IS SET = 1 IF AN INEFFICIENCY IS FOUND, AND SET = 0 IF A HIT
C           IS FOUND WITHIN CUTS.  SO IF IT IS STILL 1 WHEN THIS ROUTINE
C           HAS FINISHED IT MEANS THERE WAS AN INEFFICIENCY IN THE LAST
C           LAYER INTERCEPTED.
C
C DEVDR(20,1/2) HOLDS THE DRIFT DEVIATIONS OF LEFT/RIGHT AMBIGUITIES
C                OF HITS IN THE CURRENT REGION WHICH ARE ASSOCIATED
C                WITH THIS TRACK ( ONLY IF L/R ASSOCIATED ). THIS IS
C                CLEARED FOR EACH INTERCEPT; INDEX=1-->NO. OF ASSOCIATED
C                HITS IN THIS REGION ( NHREG ).
C
C NCHAMR(20)     HOLDS CHAMBER NOS. CORRESPONDING TO DEVDR.
C NHITR(20)       "     HIT NOS. (MUR1)  "         "   "  .
C
C EEVDR(20,1/2)  AS DEVDR , BUT HOLDS THE DEVIATIONS FOR ALL HITSIN
C                THE CURRENT REGION, REGARDLESS OF WHETHER THEY ARE
C                ASSOCIATED.
C
C MCHAMR(20)     HOLDS CHAMBER NOS. CORRESPONDING TO EEVDR.
C MHITR(20)       "     HIT NOS. (MUR1)  "         "   "  .
C MBADR(20)       "     HIT  STATUS      "         "   "  .
C NDISQ(20)      IS A FLAG FOR TESTS ASSOCIATED WITH 'M...' ARRAYS.
C                THE INDEX GOES 1--> NHREG   (N.B. NHREG ) .
C
C RETURN 1 IF TRACK STOPS (I.E. RUNS OUT OF ENERGY).
C
C-----------------------------------------------------------------------
C
      IMPLICIT INTEGER*2 (H)
C
      LOGICAL FOUND1,FOUND2,FOUNDL,NONE,BADT,BADL,BADH1,LCOVAR
*xxx 13.01.98
      LOGICAL TBIT
*xxx
C
C               COMMONS AND ARRAY DIMENSIONS
C
#include "cmuffl.for"
#include "cmufwork.for"
#include "cmureg.for"
#include "cmucalib.for"
      COMMON /CMUIEV/ IEV,NEV,IRD,KRUN,KREC,
     +                ISECS,IMINS,IHOURS,IDAY,IMONTH,IYEAR
      COMMON /CMULOC/ ALLDEV(12,2),ALLABS(12),REGDEV(30),REGDCE(60),
     +                REGZE(60),LOCHRS(60),ABLOCH(60)
      COMMON /CMUPTC/ XEXTRP,YEXTRP,ZEXTRP,VTZCV,LCOVAR
C
      DIMENSION DEVDR(20,2),NCHAMR(20),NHITR(20)
C
      DIMENSION EEVDR(20,2),MCHAMR(20),MHITR(20),MBADR(20),NDISQ(20)
      LOGICAL MBADR,NDISQ
      DIMENSION HC(1),HLUN(1),HTC(1),HAMB(1)
      DIMENSION KTYPE(4),KREGN(4)
C
C-----------------------------------------------------------------------
C
C        DATA INITIALISATION STATEMENTS, ETC.
C
C     FOR CHAMBER RESOLUTIONS, SEE BLOCK DATA FOR /CMUFFL/ AT THE END
C     SUBROUTINE MUFFLE.
C
C     ICONST IS THE CONSTANT SUBTRACTED FROM THE DRIFT TIME OF HIT2
C     TO ESTIMATE THE DRIFT TIME OF HIT1 IN THE EVENT THAT THE DRIFT
C     INFORMATION OF HIT1 HAS BEEN LOST---NECESSARY HERE TO CALCULATE
C     THE EXTRA ALLOWANCE FOR OVERWRITTEN HIT 1'S IN 'MEASURING ERROR'.
C
      DATA ICONST / 7 /
C
C-----------------------------------------------------------------------
C
C             START OF EXECUTABLE STATEMENTS.
C
      IF(NCEPTS.LE.0) RETURN
      VCT1 = REST1**2
      VCT2 = REST2**2
      VCL  = RESL **2
C
C             SET KDEPTH, WHICH RECORDS HOW MANY REGIONS ONE IS IN
C             AT ANY ONE TIME. NORMALLY THIS IS 0 OR 1 BUT CAN BE
C             UP TO 4 BECAUSE CHAMBER REGIONS ARE ARTIFICIALLY
C             EXTENDED IN MUREG AND CAN THEREFORE OVERLAP.
C
      KDEPTH=0
C
C             CLEAR THE 'OVER' VARIABLES WHICH, AFTER LEAVING
C             MUFFLY, WILL CONTAIN AMOUNTS FOR STEPS LEFT OVER
C             AFTER LEAVING THE LAST ABSORBER.
C
      DOVER  = 0.
      GMOVER = 0.
      ABOVER = 0.
      RDOVER = 0.
      DEOVER = 0.
C
C-----------------------------------------------------------------------
C START OF MAIN LOOP ---------------------------------------------------
C-----------------------------------------------------------------------
C
C             LOOK THROUGH INTERCEPTS.
C
      DO 1000   ICEPT = 1,NCEPTS
C
C             PROPAGATE TO INTERCEPT.
C
          JTYPE  = 0
          DSTEP  = RDOTD(ICEPT)
C
          IF(ICEPT.GT.1) DSTEP = DSTEP - RDOTD(ICEPT-1)
C
C            IF THIS STEP IS NOT THRO' DENSE MATTER, GOTO 1011
C            AFTER PUTTING THE MATTER VARIABLES TO ZERO.
          IF(KDEPTH.NE.0) GO TO 1010
             GMSTEP = 0.
             ABSTEP = 0.
             RDSTEP = 0.
             DESTEP = 0.
             GO TO 1011
 1010     CONTINUE
C           THIS STEP DOES GO THRO' DENSE MATTER.
C           FIND DENSEST TYPE - JTYPE.
          DO 1015  K = 1,KDEPTH
             IF(JTYPE.LT.KTYPE(K) ) JTYPE = KTYPE(K)
             IF(JTYPE.EQ.KTYPE(K) ) IRONRG= KREGN(K)
 1015     CONTINUE
C
C           OCTOBER 1983 :  FIX-UP FOR YOKE END PLATES .
C           THEY ARE COMPLETELY DEFINED BY HRFACE/IUNIT(IREGN)=5,6/3  ,
C           SINCE THEY ARE THE ONLY PARTS OF UNIT 3 (= MAGNET & PLINTH)
C           WHICH ARE IN FACES 5 AND 6.
C           IF MOST DENSE REGION IS NOT YOKE END PLATE, CARRY ON.
          IF( JTYPE.NE.3 )  GO TO 1016
C               THE REGION NO. OF THE IRON REGION WAS NOTED ABOVE.
C               NOTE : CAN'T BE IN 2 IRON REGIONS AT SAME TIME.
              IF(HRFACE(IRONRG).NE.5.AND.HRFACE(IRONRG).NE.6)GO TO 1016
                  IF(HUNIT(IRONRG).NE.3)GO TO 1016
                      JCEPT=ICEPT
                      CALL MUENDP(JCEPT,IRONRG,FACSTP)
C
C                        N.B.  DSTEP IS NOT CHANGED.
                      GMSTEP = (FACSTP*DSTEP)*GMG(JTYPE)
                      ABSTEP = (FACSTP*DSTEP)*ABG(JTYPE)
                      RDSTEP = (FACSTP*DSTEP)*RDG(JTYPE)
                      DESTEP = (FACSTP*DSTEP)*DEG(JTYPE)
                      GO TO 1011
 1016     CONTINUE
C
          GMSTEP = DSTEP*GMG(JTYPE)
          ABSTEP = DSTEP*ABG(JTYPE)
          RDSTEP = DSTEP*RDG(JTYPE)
          DESTEP = DSTEP*DEG(JTYPE)
 1011     CONTINUE
C+++
C     WRITE(6,8935)JTYPE
C8935 FORMAT(' JTYPE',I5)
C+++
C             ACCUMULATE THE 'OVER' VARIABLES WHICH, AFTER LEAVING
C             MUFFLY, WILL CONTAIN AMOUNTS FOR STEPS LEFT OVER AFTER
C             LEAVING THE LAST ABSORBER.
C
          IF(JTYPE.GT.1)GO TO 1017
              DOVER  = DOVER  + DSTEP
              GMOVER = GMOVER + GMSTEP
              ABOVER = ABOVER + ABSTEP
              RDOVER = RDOVER + RDSTEP
              DEOVER = DEOVER + DESTEP
              GO TO 1018
 1017     CONTINUE
              DOVER  = 0.
              GMOVER = 0.
              ABOVER = 0.
              RDOVER = 0.
              DEOVER = 0.
 1018     CONTINUE
C
          CALL MUFFLS(*3333)
C
C-----------------------------------------------------------------------
C
C             FIND OUT WHAT TYPE OF INTERCEPT.
C
          IREGN=IREG(ICEPT)
          ITYPE=HRTYPE(IREGN)
C             IS THIS A NEW REGION BOUNDARY?
          IF(KDEPTH.EQ.0) GO TO 1012
              DO 1013 K=1,KDEPTH
                 IF(IREGN.EQ.KREGN(K) ) GO TO 1014
 1013         CONTINUE
C
              GO TO 1012
C                IT MUST BE AN EXIT POINT - DELETE REFERENCE AND
C                DECREMENT KDEPTH. THIS MAY SEEM UNNECESSARILY
C                COMPLICATED BUT WITH OVERLAPPING REGIONS, AN EXIT
C                POINT DOES NOT NECESSARILY FOLLOW THE CORRESPONDING
C                ENTRY POINT.
C
 1014             JDEPTH=0
                  DO 1019  K = 1,KDEPTH
                     IF(IREGN.EQ.KREGN(K))GO TO 1019
                         JDEPTH = JDEPTH + 1
                         IF(JDEPTH.EQ.K)GO TO 1019
                             KREGN(JDEPTH) = KREGN(K)
                             KTYPE(JDEPTH) = KTYPE(K)
 1019             CONTINUE
                  KDEPTH=JDEPTH
C+++
C     WRITE(6,1657)            KDEPTH,KREGN,KTYPE
C1657 FORMAT('            KDEPTH,KREGN,KTYPE',I15,2(I15,3I5))
C+++
                  GO TO 1000
C
C            IT IS A NEW REGION - INCREMENT KDEPTH.
C
 1012     KDEPTH = KDEPTH + 1
          IF(KDEPTH.LE.4) GO TO 97
           CALL MUERRY('MUFFLY',KDEPTH,'REGIONS OVERLAPPING TOO MUCH.^')
           GO TO 1000
C
   97     KREGN(KDEPTH) = IREGN
          KTYPE(KDEPTH) = ITYPE
C+++
C     WRITE(6,1656)IREGN,ITYPE,KDEPTH,KREGN,KTYPE
C1656 FORMAT(' IREG,ITYPE,KDEPTH,KREGN,KTYPE',3I5,2(I15,3I5))
C+++
C             GO TO NEXT INTERCEPT IF NOT A CHAMBER REGION.
C
          IF(ITYPE.NE.1) GO TO 1000
C
C-----------------------------------------------------------------------
C
C             CHAMBER INTERCEPTED.
C
C CHECK AND RECORD HIT AND PRESERVE SPECIAL VARIABLES IF THERE IS A HIT.
C
      IFIRST = HRFIRS(IREGN)
      ILAST  = HRLAST(IREGN)
      IOR    = HRORI (IREGN)
      IOVALL = HOVALL(HUNIT(HFR(IFIRST)))
C
C-----------------------------------------------------------------------
C
C             WORK OUT PROJECTED VARIANCES FOR THIS REGION.
C
C ADD MULTIPLE SCATTERING AND TRACK FITTING ERRORS TO PRODUCE VARIANCES
C   ON DEVIATIONS AND COVARIANCES BETWEEN ANGLES AND DEVIATIONS
C   NORMAL TO TRACK IN THE 2 PLANES (THE A- & B-PLANES OF JADE NOTE 68.
C   NOTE THAT THE A-PLANE GETS THE LABEL 'Z' & THE B-PLANE 'XY' HERE.)
C
        IF( LCOVAR ) GOTO 995
           VZP = VTZD + VTZANG*(D-DTC)**2
           VZD = VMSD + VZP
           CZ  = CMS + VTZANG*(D-DTC)
           GOTO 996
 995    CONTINUE
           R0P = SQRT(X**2 + Y**2)
           VZP = VTZD + 2.0*R0P*VTZCV + R0P**2*VTZANG
           VZD = VMSD + VZP
           CZ  = CMS + (R0P*VTZANG+VTZCV)/COSEC
C
CCC     VXYD  = VMSD + VTXYD + VTXYAN*(D-DTC)**2
 996    DFXY2 = (X-XEXTRP)**2 + (Y-YEXTRP)**2
        VXYP  = VTXYD + VTXYAN*DFXY2
        VXYD  = VMSD + VXYP
CCC     CXY   = CMS + VTXYAN*(D-DTC)*COSEC
        CXY   = CMS + VTXYAN*SQRT(DFXY2)
C+++
C     WRITE(6,7491)VMSD,CMS,VZP,VZD,CZ,VXYP,CXY
C7491 FORMAT(' MUFFLY: VMSD,CMS,VZP,VZD,CZ,VXYP,CXY',/7G15.5)
C+++
C
C             PREPARE COEFFICIENTS FOR PROJECTING ONTO CHAMBER PLANES.
C
      IF(IOR.EQ.3) GO TO 1003
      AA = -COSEC
      CC = 0.
      IF(IOR.EQ.2)GO TO 1005
      BB = DCY * DCZ * COSEC/DCX
      DD = 1./(DCX * COSEC)
      GO TO 1002
 1005 CONTINUE
        BB = -DCX * DCZ * COSEC/DCY
        DD = -1./(DCY * COSEC)
        GO TO 1002
 1003 CONTINUE
        AA =  (DCY * COSEC)/DCZ
        BB =  (DCX * COSEC)
        CC =  (DCX * COSEC)/DCZ
        DD = -(DCY * COSEC)
 1002 CONTINUE
C+++
C     WRITE(6,2398)AA,BB,CC,DD
C2398 FORMAT(' AA,BB,CC,DD',4G15.5)
C+++
C
C               WORK OUT VARIANCES IN DRIFT (TRANSVERSE)
C               AND WIRE (LONGITUDINAL) DIRECTIONS.
C
      VL1  = AA**2 * VZD + BB**2 * VXYD
      VT1  = CC**2 * VZD + DD**2 * VXYD
      CTL1 = AA*CC * VZD + BB*DD * VXYD
C+++
C     WRITE(6,2399)VL1,VT1,CTL1
C2399 FORMAT(' VL1,VT1,CTL1',3G15.5)
C+++
C
C             STORE THESE VARIANCES AND COVARIANCES, WHICH GIVE
C             THE EFFECT OF MULTIPLE SCATTERING AND INNER DETECTOR
C             TRACK FITTING ERRORS,IN THE ARRAY ELIPSE FOR GRAPHICS USE.
C
      IF(NELIPS.GE.700) GO TO 1004
        ELIPSE(NELIPS+1) = X
        ELIPSE(NELIPS+2) = Y
        ELIPSE(NELIPS+3) = Z
        HLIPSE(2*NELIPS+7) = IDTRK
        HLIPSE(2*NELIPS+8) = IOR
        ELIPSE(NELIPS+5) = VT1
        ELIPSE(NELIPS+6) = VL1
        ELIPSE(NELIPS+7) = CTL1
C
        NELIPS = NELIPS + 7
 1004 CONTINUE
C
C-----------------------------------------------------------------------
C LOOP OVER HITS. ------------------------------------------------------
C-----------------------------------------------------------------------
C
      IF(NHITS.LE.0) GO TO 1000
      NONE=.TRUE.
C
C CLEAR DEVDR ETC. & SET COUNTER OF ASSOCIATED HITS WITHIN REGION.
C   "   EEVDR & MCHAMR    "     "    "  ALL HITS WITHIN REGION.
C
      CALL VZERO(DEVDR,40)
      CALL VZERO(NCHAMR,20)
      CALL VZERO(NHITR,20)
      NHREG = 0
      CALL VZERO(EEVDR,40)
      CALL VZERO(MCHAMR,20)
      CALL VZERO(MHITR,20)
      CALL VZERO(MBADR,20)
      CALL VZERO(NDISQ,20)
      MHREG = 0
C
      IP    = 0
      DO 2000  IHIT = 1,NHITS
      ICHAM=HC(IP+1)/4
C
C                  CHECK IF THIS CHAMBER IN THIS REGION.
C
      IF( ICHAM.LT.IFIRST .OR. ICHAM.GT.ILAST ) GO TO 2001
      ICHIT  = HC(IP+1) - 4*ICHAM + 1
      IFRAME = HFR(ICHAM)
      IFACE  = HFACE(IFRAME)
C
C                  FOR CALCULATION OF ANGLE CORRECTION WITH TRACK "Z" :
      IANG=HANG(IFRAME)
      ZMID=0.5*( HCLLO(IFRAME) + HCLHI(IFRAME) )
      ZHAF=0.5*( HCLHI(IFRAME) - HCLLO(IFRAME) )
C
      BADT   = .NOT.TBIT(HLUN(IHIT),14)
      BADL   = .NOT.TBIT(HLUN(IHIT),15)
      BADH1  = (ICHIT.EQ.1) .AND. (.NOT.TBIT(HLUN(IHIT),7))
      FOUND1 = .FALSE.
      FOUND2 = .FALSE.
      FOUNDL = .FALSE.
C                  NOW ADD CHAMBER MEASURING ERRORS TO COMPUTE CUTS.
      VL   = VL1 + VCL
      CUTL = FACTOR * SQRT(VL)
      IF(BADL) CUTL = FACTOR * SQRT(VL1) + RESLMX
      VCT  = VCT1
      IF(IREGN.GT.32) VCT=VCT2
      VT   = VT1 + VCT
      CUTT = FACTOR * SQRT(VT)
      IF(.NOT.BADH1) GO TO 1920
C WHAT IS THE DRIFT VELOCITY OF THIS CHAMBER AT THIS TIME ?
          IVDRCH = HVDRFT(ICHAM)
          ICRATE = HSECT (IFRAME)
          DELCDM = IVDRCH * ICONST/1000
C
C  CORRECT FOR CRATE 6 CLOCK PULSE .... BUT NOT FOR MONTE-CARLO EVENTS.
C  ( NOTE THAT THE CALIBRATION FILES ALWAYS HAVE CRATE 6 VDR & DTP)
C  ( EQUAL TO GLOBAL VALUES....THIS IS THE EFFECT OF THE VARIOUS)
C  ( UPDATES AS OF 16/03/83.                      )
          IF(ICRATE.EQ.6.AND.KRUN.LT.8256.AND.KRUN.GT.0)
     *    DELCDM=1.26*DELCDM
C
 1920 CONTINUE
C
C             EXCLUDE OVER-WRITTEN HIT1'S FROM THE CHI**2 CALCULATION.
C
      IF(BADH1) BADT = .TRUE.
C
C         CALCULATE THE WIRE COORDINATE (APART FROM OVALL).
C******  AS OF 16/03/83 , APPLY SOFTWARE SHIFT TO WIRE POSITION FOR ALL
C******  DATA AFTER BEGINNING OF 1980 :
      ISHIFT=0
      IF(IYEAR.GE.1980)ISHIFT=HLSF(4,ICHAM)
      IF(IABS(ISHIFT).GT.300)ISHIFT=0
C**** DON'T APPLY SOFTWARE SHIFTS TO MONTE-CARLO DATA.
      IF(KRUN.EQ.0)ISHIFT=0
      ICWIRE=HCTW(ICHAM)-ISHIFT
C
C
C++++
C     WRITE(6,9000)VXYD,VZD,VT1,VL1,CTL1,VT,VL,CUTT,CUTL
C9000 FORMAT(' VXYD,VZD,VT1,VL1,CTL1,VT,VL,CUTT,CUTL',9F10.4)
C++++
C
C-----------------------------------------------------------------------
C
C             PREPARE DEVIATIONS. PROJECT TO ACTUAL CHAMBER PLANE.
C
      GO TO (2101,2102,2103) , IOR
C
 2101 CONTINUE
        DDC = (HDIST(IFRAME) + HD1(ICHAM) + IOVALL -X) / DCX
        CT  = DCY * DDC + Y
        CL  = DCZ * DDC + Z
C
C                  CALCULATE ANGLE CORRECTION WITH TRACK "Z" :
C
        CHAMZ=CL-ZMID
        IF(ABS(CHAMZ).GT.ZHAF)CHAMZ=SIGN(ZHAF,CHAMZ)
        CANG =CHAMZ*FLOAT(IANG)/10000.
C
C                  APPLY ANGLE CORRECTION TO THE Y COORDINATE :
C
        DT1 = HC(IP+4)+CANG-CT
        DT2 = HC(IP+7)+CANG-CT
C
        DL  = HC(IP+5)  - CL
C
C              FOR BAD HITS AND OVER-WRITTEN HIT1'S , NOTE SOME
C              INFORMATION FOR HIT ASSOCIATION TESTS.
C                BADLY DIGITISED HIT ; NOTE CHAMBER EDGES.
C                   OVER-WRITTEN HIT1; NOTE POSITION OF HIT2.
        IF(.NOT.BADT)GO TO 2200
           IF(BADH1)GO TO 2111
              TEDGE1=ICWIRE+CANG-150.
              TEDGE2=ICWIRE+CANG+150.
              GO TO 2200
 2111            TEDGE1=HC(IP+4)+CANG-DELCDM
                 TEDGE2=HC(IP+7)+CANG+DELCDM
                 GO TO 2200
C
C
 2102 CONTINUE
        DDC = (HDIST(IFRAME) + HD1(ICHAM) - Y) / DCY
        CT  = DCX * DDC + X
        CL  = DCZ * DDC + Z
C
C                  CALCULATE ANGLE CORRECTION WITH TRACK "Z" :
C
        CHAMZ=CL-ZMID
        IF(ABS(CHAMZ).GT.ZHAF)CHAMZ=SIGN(ZHAF,CHAMZ)
        CANG =CHAMZ*FLOAT(IANG)/10000.
C
C                  APPLY ANGLE CORRECTION TO THE X COORDINATE :
C
        DT1 = HC(IP+3)+CANG-CT
        DT2 = HC(IP+6)+CANG-CT
C
        DL  = HC(IP+5)  - CL
C
        IF(.NOT.BADT)GO TO 2200
           IF(BADH1)GO TO 2112
              TEDGE1=ICWIRE+IOVALL+CANG-150.
              TEDGE2=ICWIRE+IOVALL+CANG+150.
              GO TO 2200
 2112            TEDGE1=HC(IP+3)+CANG-DELCDM
                 TEDGE2=HC(IP+6)+CANG+DELCDM
                 GO TO 2200
C
C
 2103 CONTINUE
        DDC = (HDIST(IFRAME) + HD1(ICHAM) - Z) / DCZ
        CT  = DCX * DDC + X
        CL  = DCY * DDC + Y
C
C                  CALCULATE ANGLE CORRECTION WITH TRACK "Z" :
C
        CHAMZ=CL-ZMID
        IF(ABS(CHAMZ).GT.ZHAF)CHAMZ=SIGN(ZHAF,CHAMZ)
        CANG =CHAMZ*FLOAT(IANG)/10000.
C
C                  APPLY ANGLE CORRECTION TO THE X COORDINATE :
C
        DT1 = HC(IP+3)+CANG-CT
        DT2 = HC(IP+6)+CANG-CT
C
        DL  = HC(IP+4)  - CL
C
        IF(.NOT.BADT)GO TO 2200
           IF(BADH1)GO TO 2113
              TEDGE1=ICWIRE+IOVALL+CANG-150.
              TEDGE2=ICWIRE+IOVALL+CANG+150.
              GO TO 2200
 2113            TEDGE1=HC(IP+3)+CANG-DELCDM
                 TEDGE2=HC(IP+6)+CANG+DELCDM
                 GO TO 2200
C
C
C-----------------------------------------------------------------------
C
C             CHECK IF COORDINATES ARE WITHIN CUTS.
C
 2200 IF( ABS(DT1).LE.CUTT ) FOUND1 = .TRUE.
      IF( ABS(DT2).LE.CUTT ) FOUND2 = .TRUE.
      IF( ABS(DL) .LE.CUTL ) FOUNDL = .TRUE.
C
      IF(FOUND1.OR.FOUND2)GO TO 2210
C
C         FURTHER CHECKS OF ASSOCIATION FOR BAD HITS :
C
C
          IF(.NOT.BADT)GO TO 2210
C
C CHECK IF THE TEST EDGE IS WITHIN FACTOR*SIGMA OF THE TRACK ; IF SO,
C THEN ASSOCIATE THE HIT WITH THE TRACK.
C   IF THE TRACK IS EXPECTED TO GO BEWEEN THE TEDGES, THEN THERE
C   IS NO NEED TO DO THE WEDGE TESTS.
              IF(TEDGE1.LT.CT  .AND.  TEDGE2.GT.CT)GO TO 2205
                WEDGE1=CT-CUTT
                WEDGE2=CT+CUTT
                IF(WEDGE1.LT.TEDGE1  .AND.  WEDGE2.GT.TEDGE1)GO TO 2205
                 IF(WEDGE1.LT.TEDGE2  .AND.  WEDGE2.GT.TEDGE2)GO TO 2205
CAV                 00056000
                   GO TO 2210
 2205           FOUNDL=.TRUE.
                FOUND1=.TRUE.
                FOUND2=.FALSE.
                IF(ABS(DT1).LT.ABS(DT2))GO TO 2210
                   FOUND1=.FALSE.
                   FOUND2=.TRUE.
C
 2210    CONTINUE
C
C
C-----------------------------------------------------------------------
C
 2020 CONTINUE
C
C  PROTECT AGAINST OVERFLOWS IN THE ARRAYS FOR WHICH MHREG IS INDEX.
      IF(MHREG.LT.20) GO TO 2018
      CALL MUERRY('MUFFLY',IREGN,'(=REGION NO.) HAS >20 MUCOOR HITS -
     * MUFFLY CHECKS ON NON-ASSOCIATED HITS LIMITED.^')
        GO TO 2019
C           NOTE EEVDR, MCHAMR,MHITR.
 2018 MHREG=MHREG+1
      EEVDR(MHREG,1)  = DT1
      EEVDR(MHREG,2)  = DT2
      MCHAMR(MHREG)   = ICHAM
      MHITR(MHREG)    = IHIT
      MBADR(MHREG)    = BADT
 2019 CONTINUE
C+++
C                WRITE(6,2016)IDTRK,IREGN,MHREG,BADT
C2016            FORMAT('           TRACK NO./IREGN/MHREG/BADT:',3I6,L3)
C+++
C             DECIDE WHETHER TO KEEP HIT OR NOT.
C++++
C     WRITE(6,9001)DL,DT1,DT2,FOUND1,FOUND2,FOUNDL
C9001 FORMAT(' DL,DT1,DT2,FOUND,IAMB',3F10.4,3L5)
C++++
C
C   AT THE MOMENT THE HIT IS ASSOCIATED WITH THE EXTRAPOLATED INNER
C   DETECTOR TRACK USING THE DRIFT INFORMATION ALONE. THE LONGITUDINAL
C   INFORMATION IS INSPECTED (BUT NOT USED FOR SELECTION) ONLY IF
C   AT LEAST ONE OF THE DRIFT COORDINATES (LEFT OR RIGHT) IS IN THE CUT.
C
      IF( .NOT.(FOUND1.OR.FOUND2) ) GO TO 2001
C
C             ***  HIT HAS BEEN FOUND  ***
C             FLAG A HIT FOR THIS TRACK, THIS REGION.
C
      NONE=.FALSE.
C
C             INCREMENT HIT COUNTER AND SAVE THE REGION NUMBER IN
C             WHICH THE HIT WAS FOUND. SAVE THE DEVIATIONS (IN UNITS
C             OF SIGMA(TOT) ) FOR EACH AMBIGUITY IF ASSOCIATED.
C             SAVE THE AMOUNT OF ABSORBER TRAVERSED TO THIS REGION.
C
      NTHIS = NTHIS + 1
C
C             PROTECT AGAINST OVERFLOWS IN IHTREG :
C
      IF(NTHIS.LE.20) GO TO 96
      CALL MUERRY('MUFFLY',IDTRK,'(=TRACK NO.) HAS >20 ASSOCIATED HITS -
     * MUFFLY PROCESSING TERMINATED FOR THIS TRACK.^')
        GO TO 1000
C
 96   IHTREG(NTHIS) = IREGN
      ALLABS(NTHIS) = AB
C                         WHEN A TRACK CHANGES FACES THE DEVIATIONS
C                         MUST HAVE THE SAME LEFT/RIGHT SIGN CONV.
      IFAS = 1
      IF(IFACE.EQ.2 .OR. IFACE.EQ.3) IFAS = -1
      IF(FOUND1) ALLDEV(NTHIS,1) = IFAS * DT1/SQRT(VT)
      IF(FOUND2) ALLDEV(NTHIS,2) = IFAS * DT2/SQRT(VT)
C
C             MARK BAD DRIFT HITS WITH A 100 SIGMA DEVIATION
C
      IF(.NOT.(BADT .OR. BADH1) ) GO TO 3050
        ALLDEV(NTHIS,1) = 100.
        ALLDEV(NTHIS,2) = 100.
C
C                COUNT HITS ASSOCIATED WITH THIS TRACK IN THIS REGION.
 3050            NHREG = NHREG + 1
C+++
C                WRITE(6,3006)NTHIS,IREGN,NHREG,BADT
C3006            FORMAT(' NTHIS/IREGN/NHREG/BADT:',3I6,L3)
C+++
C                NOTE DEVDR, NCHAMR,NHITR
                 NCHAMR(NHREG) = ICHAM
                 NHITR(NHREG)  = IHIT
                 IF(FOUND1)DEVDR(NHREG,1)=DT1
                 IF(FOUND2)DEVDR(NHREG,2)=DT2
C
C             MAKE ENTRIES IN MUR2 BANKS 2 AND 3.
C             BANK 3 IS MODIFIED IN MUFFLX.
C
      J     = NTPH * (IHIT-1)
      JLAST = J + NTPH
 2021 CONTINUE
      J = J + 1
      IF(HTC(J).EQ.0) GO TO 2022
      IF(J.LT.JLAST)  GO TO 2021
      IF(HTC(J).GT.0) HTC(J) = - HTC(J)
      J = -1
      GO TO 2023
C
 2022 HTC(J) = IDTRK
      IF(FOUND1) HAMB(J) = -1
      IF(FOUND2) HAMB(J) =  1
      IF(FOUND1.AND.FOUND2) HAMB(J) = 0
C
C              FILL ENTRIES IN ERROR MATRIX, ETC.
C
 2023 IF(NTHIS.LE.10) GO TO 98
      CALL MUERRY('MUFFLY',IDTRK,'(=TRACK NO.) HAS >10 ASSOCIATED HITS -
     * NO FURTHER HITS USED IN CHI**2.^')
        GO TO 2001
C
   98 I3 = 3 * NTHIS
      I2 = I3 - 1
      I1 = I2 - 1
      IL = 2 * NTHIS
      IT = IL - 1
      IJA(NTHIS)    = J
      DALONG(NTHIS) = D
      DEV(I1) = DT1
      DEV(I2) = DT2
      DEV(I3) = DL
      EM(IT,IT) = VT
      EM(IL,IL) = VL
      EM(IT,IL) = CTL1
C             (FILL IN TRANSPOSED ELEMENT LATER - EM IS SYMMETRIC.)
      VXYDA(NTHIS) = VXYD
      VZDA(NTHIS)  = VZD
      CXYA(NTHIS)  = CXY
      CZA(NTHIS)   = CZ
      AAA(NTHIS)   = AA
      BBB(NTHIS)   = BB
      CCC(NTHIS)   = CC
      DDD(NTHIS)   = DD
      INCUT(I1)    = FOUND1
      INCUT(I2)    = FOUND2
      INCUT(I3)    = FOUNDL
      BADA(IT)     = BADT
      BADA(IL)     = BADL
      ITHISA(NTHIS)= IHIT
      CTDASH(NTHIS)= CT
      CLDASH(NTHIS)= CL
      VTDASH(NTHIS)= VT1
      VLDASH(NTHIS)= VL1
C
C        NOW FILL IN CORRELATIONS WITH PREVIOUS HITS (IF ANY).
C
      IF(NTHIS.LE.1) GO TO 2001
      NTHOSE = NTHIS - 1
      DO 2030  NTHAT = 1,NTHOSE
        J3    = 3 * NTHAT
        J2    = J3 - 1
        J1    = J2 - 1
        JL    = 2 * NTHAT
        JT    = JL - 1
        D12   = D - DALONG(NTHAT)
        CZ12  = VZDA(NTHAT)  + CZA(NTHAT)  * D12
        CXY12 = VXYDA(NTHAT) + CXYA(NTHAT) * D12
        CL12  = AAA(NTHAT) * AA * CZ12 + BBB(NTHAT) * BB * CXY12
        CT12  = CCC(NTHAT) * CC * CZ12 + DDD(NTHAT) * DD * CXY12
        CL1T2 = AAA(NTHAT) * CC * CZ12 + BBB(NTHAT) * DD * CXY12
        CT1L2 = CCC(NTHAT) * AA * CZ12 + DDD(NTHAT) * BB * CXY12
C+++
C     WRITE(6,4691)NTHAT,AAA(NTHAT),BBB(NTHAT),CCC(NTHAT),DDD(NTHAT)
C4691 FORMAT(' NTHAT,AAA(NTHAT),BBB-,CCC-,DDD-',I5,4G15.5)
C     WRITE(6,3429)D12,CZ12,CXY12,CL12,CT12,CL1T2,CT1L2
C3429 FORMAT(' D12,CZ12,CXY12,CL12,CT12,CL1T2,CT1L2',7G12.5)
C+++
        EM(JT,IT) = CT12
        EM(JL,IL) = CL12
        EM(JT,IL) = CT1L2
        EM(JL,IT) = CL1T2
C             (FILL IN TRANSPOSED ELEMENTS LATER.)
 2030 CONTINUE
C
C-----------------------------------------------------------------------
C
C END OF LOOP.
C
 2001 CONTINUE
      IP=IP+NWHIT
 2000 CONTINUE
C
C-----------------------------------------------------------------------
C END OF LOOP OVER HITS. -----------------------------------------------
C-----------------------------------------------------------------------
C
C             HAVE ANY HITS BEEN FOUND IN THIS REGION?
C
      IF(NONE)GO TO 1000
C
C AT LEAST ONE HIT HAS BEEN FOUND FOR THIS TRACK IN THIS REGION.
C UPDATE SPECIAL VARIABLES.
C
      CALL UCOPY(X,X0,NSPECI)
C
C             INCREMENT LAYRAB AND NOTE THE REGION NO. IN IEFFRG.
C
      LAYRAB         = LAYRAB+1
      IEFFRG(LAYRAB) = IREGN
C
C             FLAG  PAIRS OF ASSOCIATED OVERLAP HITS.
C               ( IT WAS ORIGINALLY INTENDED THAT THIS SHOULD BE
C                 DONE IN A SEPARATE ROUTINE , MUOLAP , BUT COMPILER
C                 BUG PREVENTED COMMUNICATION OF IOVLAP.  )
C
      NLOREG = NTHIS-NHREG+1
      IF(NHREG.LT.2)GO TO 4100
          NHLES1=NHREG-1
          DO 4050 NHIT=1,NHLES1
              NHAND1=NHIT+1
              DO 4050 NOVHIT=NHAND1,NHREG
C+++
C         WRITE(6,4006)
C    *   (DEVDR(NHIT  ,K),K=1,2),NCHAMR(NHIT),NHITR(NHIT),
C    *   (DEVDR(NOVHIT,K),K=1,2),NCHAMR(NOVHIT),NHITR(NOVHIT)
C4006    FORMAT('0 DEV/NCHAMR/NHITR(NHIT) :',2F8.1,' / ',I5,' / ',I5,
C    *        '  DEV/NCHAMR/NHITR(NOVHIT) :',2F8.1,' / ',I5,' / ',I5)
C+++
C
C                     ARE NHIT & NOVHIT IN ADJACENT CHAMBERS ?
C
                  IF(IABS(NCHAMR(NHIT)-NCHAMR(NOVHIT)).NE.1)GO TO 4050
C
C                         PICK UP INDICES IN RANGE 1-->NTHIS
                      NTHIS1 = 0
                      NTHIS2 = 0
                      DO 4010 ITH=NLOREG,NTHIS
                              IHIT=ITHISA(ITH)
                              IF(NHITR(NHIT  ).EQ.IHIT)NTHIS1=ITH
                              IF(NHITR(NOVHIT).EQ.IHIT)NTHIS2=ITH
 4010                 CONTINUE
C+++
C         WRITE(6,4016)NTHIS1,NTHIS2
C4016    FORMAT('0 NTHIS OF NHIT / NOVHIT =',2I3)
C+++
C
C                        SET OVERLAP TEST TOLERANCE ; OVLCUT FOR 2 GOOD
C                        HITS , (OVLCUT+15)MM IF ONE OR MORE OF THE HITS
C                        IS BAD.
C
                      OVTEST=OVLCUT
                      IF(BADA(2*NTHIS1-1).OR.BADA(2*NTHIS2-1))
     +                OVTEST=OVLCUT+15.
C+++
C         WRITE(6,4026)OVTEST
C4026    FORMAT('+',45X,' TOLERANCE ( OVTEST) =',F5.1)
C+++
C
                      I3=3*NTHIS1
                      I2=I3-1
                      I1=I2-1
                      J3=3*NTHIS2
                      J2=J3-1
                      J1=J2-1
C+++
C         WRITE(6,4036)INCUT(I1),INCUT(I2),INCUT(J1),INCUT(J2)
C4036    FORMAT('+',75X,' INCUTS NHIT/NOVHIT =',2L2,' / ',2L2)
C+++
                      IF(.NOT.INCUT(I1))GO TO 4020
C
C                           LEFT AMB OF NTHIS1 IS ASSOCIATED ;
C                           IS IT AN OVERLAP WITH RIGHT AMB OF NTHIS2 ?
C
                          IF(.NOT.INCUT(J2))GO TO 4020
                             DEVDIF=DEVDR(NHIT,1)-DEVDR(NOVHIT,2)
                             IF(ABS(DEVDIF).GT.OVTEST)GO TO 4020
C
C                              NHIT (LEFT) & NOVHIT (RIGHT) OVERLAP.
                               IOVLAP(NTHIS1,NTHIS2)=1
C
 4020                  IF(.NOT.INCUT(I2))GO TO 4050
C
C                            RIGHT AMB OF NTHIS1 IS ASSOCIATED ;
C                            IS IT AN OVERLAP WITH LEFT AMB OF NTHIS2 ?
C
                          IF(.NOT.INCUT(J1))GO TO 4050
                             DEVDIF=DEVDR(NHIT,2)-DEVDR(NOVHIT,1)
                             IF(ABS(DEVDIF).GT.OVTEST)GO TO 4050
C
C                               NHIT (RIGHT) & NOVHIT (LEFT) OVERLAP.
                                IOVLAP(NTHIS2,NTHIS1)=1
C
 4050      CONTINUE
C
 4100 CONTINUE
C
C+++
C                     WRITE(6,4106)
C4106                 FORMAT('0 NON-ZERO ELEMENTS OF OVERLAP MATRIX :')
C                     DO 4150 IOL=1,20
C                     DO 4150 JOL=1,20
C                     IF(IOVLAP(IOL,JOL).NE.0)WRITE(6,4156)IOL,JOL,
C    *                                        IOVLAP(IOL,JOL)
C4156                 FORMAT(' I / J / ELEMENT ',3I4)
C4150                 CONTINUE
C+++
C
C        CHECK IF ANY ASSOCIATED SINGLE AMBIGUITY OF ANY HIT IS ACTUALLY
C        THE 'OTHER' AMBIGUITY OF AN OVERLAP PAIR (NEITHER OF THE AMBS
C        IN THE PAIR NEED BE ASSOCIATED.)
C
      IF(MHREG.LT.2)GO TO 5100
          DO 5050 NHIT=1,NHREG
C+++
C         WRITE(6,5006)
C    *   (DEVDR(NHIT  ,K),K=1,2),NCHAMR(NHIT),NHITR(NHIT)
C5006    FORMAT('0 DEV/NCHAMR/NHITR(NHIT) :',2F8.1,' / ',I5,' / ',I5)
C+++
C
                  DO 5010 ITH=NLOREG,NTHIS
                          IHIT=ITHISA(ITH)
                          IF(NHITR(NHIT).EQ.IHIT)NTHIS1=ITH
 5010             CONTINUE
                  I3=3*NTHIS1
                  I2=I3-1
                  I1=I2-1
C+++
C         WRITE(6,5016)NTHIS1,INCUT(I1),INCUT(I2)
C5016    FORMAT('0 NTHIS OF NHIT =',I3,' INCUTS =',2L3)
C+++
C         ----------- IF BOTH AMBS ARE ASSOCD, THEN NO REJECTION.
                  IF(INCUT(I1).AND.INCUT(I2))GO TO 5050
C
C         ----------- NOTE INDEX IN 1--> MHREG OF THE CURRENT ASSOCIATED
C                     HIT. THIS IS NECESSARY SINCE DEVDR HOLDS
C                     DEVIATIONS OF ONLY THE ASSOCIATED AMB(S).
                     DO 5012 MHIT=1,MHREG
                             IF(NHITR(NHIT).EQ.MHITR(MHIT))MHITN=MHIT
 5012                CONTINUE
C+++
C          WRITE(6,5026)MHITN
C5026      FORMAT(' MHIT CORRESPONDING TO NHIT , I.E. MHITN =',I3)
C+++
C
C         ----------- LOOP OVER ALL HITS IN THE REGION AGAIN TO CARRY
C                     OUT THE CHECK.
                     DO 5020 MHIT=1,MHREG
C               ----------IS THIS HIT IN THE CHAMBER NEXT TO THE
C                         CURRENT ASSOCIATED HIT , AND IS IT ON THE
C                         OPPOSITE SIDE FROM THE ASSOCIATED AMBIGUITY ?
                        NEIGHB=MCHAMR(MHIT)-NCHAMR(NHIT)
                        IF(IABS(NEIGHB).NE.1)GO TO 5020
                        IF(NEIGHB.GT.0.AND.INCUT(I2))GO TO 5020
                        IF(NEIGHB.LT.0.AND.INCUT(I1))GO TO 5020
C                   ------- IF THE NEIGHBOURING HIT IS ALSO ASSOCIATED
C                           WITH THIS TRACK, NO REJECTION.
                        DO 5015 NHIT2=1,NHREG
                             IF(NHITR(NHIT2).EQ.MHITR(MHIT))GO TO 5020
 5015                   CONTINUE
C                   ------- HAVE FOUND A NON-ASSOCIATED HIT ON THE
C                           'CORRECT' SIDE OF THE CURRENT ASSOCD HIT.
C                           DOES IT MAKE AN OVERLAP  WITH THE NON-
C                           ASSOCD AMB OF THE CURRENT ASSOCD HIT?
C                           IF SO , SET THE DISQUALIFICATION FLAG FOR
C                           THIS ASSOCIATED HIT.
C+++
C         WRITE(6,5036)
C    *   (EEVDR(MHIT ,K),K=1,2),MCHAMR(MHIT),MHITR(MHIT),
C    *   (EEVDR(MHITN,K),K=1,2),MCHAMR(MHITN),MHITR(MHITN)
C5036    FORMAT('0 DEV/MCHAMR/MHITR(MHIT) :',2F8.1,' / ',I5,' / ',I5,
C    *        '  DEV/MCHAMR/MHITR(MHITN) :',2F8.1,' / ',I5,' / ',I5)
C+++
                            INHAMB=1
                            IMHAMB=2
                            IF(INCUT(2))GO TO 5017
                               INHAMB=2
                               IMHAMB=1
C                            ------- NOTE: COMPARE EEVDR ENTRIES OF
C                                          MHITN AND MHIT.
 5017                      DEVDIF=EEVDR(MHITN,INHAMB)-EEVDR(MHIT,IMHAMB)
                            OVTEST=OVLCUT
                            IF(MBADR(MHIT).OR.BADA(2*NTHIS1-1))
     *                         OVTEST=OVLCUT+15.
                            IF(ABS(DEVDIF).GT.OVTEST)GO TO 5020
                               NDISQ(NHIT)=.TRUE.
                               IHTREG(NTHIS1)=-IREGN
 5020                 CONTINUE
 5050      CONTINUE
C
C  LOOP OVER ALL ASSOCIATED HITS AGAIN TO CHECK IF ANY OF THEM SURVIVE
C  THE ABOVE DISQUALIFICATION PROCEDURE.
        IOK=0
        DO 5070 NHIT=1,NHREG
             IF(.NOT.NDISQ(NHIT))IOK=IOK+1
 5070   CONTINUE
        IF(IOK.GE.1)GO TO 5100
C     -----THIS REGION HAS ALL ASSOCIATED HITS DISQUALIFIED.
           IEFFRG(LAYRAB)=-IREGN
C+++
C        WRITE(6,5076)(NDISQ(K),K=1,20),(IHTREG(K),K=1,10),
C    *                (IEFFRG(K),K=1,10)
C5076    FORMAT('0 NDISQ/IHTREG/IEFFRG :',20L1,20I4)
C+++
C
 5100 CONTINUE
C
C-----------------------------------------------------------------------
C
 1000 CONTINUE
C
C-----------------------------------------------------------------------
C END OF MAIN LOOP OVER REGIONS. ---------------------------------------
C-----------------------------------------------------------------------
C
C+++
C     WRITE(6,1826)DOVER,GMOVER,ABOVER,RDOVER,DEOVER
C1826 FORMAT(' DOVER,GMOVER,ABOVER,RDOVER,DEOVER ',5G15.5)
C+++
C
C-----------------------------------------------------------------------
      RETURN
C-----------------------------------------------------------------------
C
C             TRACK STOPPED. JUMP HERE FROM RETURN OF MUFFLS
C
 3333 RETURN 1
      END
