C   06/05/83 802082156  MEMBER NAME  OMUFFLE  (JADEMUS)     FORTRAN
C
C-----------------------------------------------------------------------
      SUBROUTINE MUFFLE(IPATR,APATR,NTRKS,NWTRK,BGAUSS,HC,NHITS,NWHIT,
     +                  HLUN,IMUR,AMUR,NWMUR,HTC,HAMB,NTPH,
     +                  EXTRA,NPL,EBGEV)
C-----------------------------------------------------------------------
C
C LAST CHANGE 19.15 05/02/88 CHRIS BOWDERY-REMOVE CALL TO MUFFLM
C      CHANGE 15.00 08/08/84 PWA   CKB    -CHANGES FOR NALLWM,PMXFLG
C      CHANGE 19.20 22/03/84 CHRIS BOWDERY-STORE NGLAYR IN MUR2/1 WORD 7
C      CHANGE 15.00 09/03/84 CHRIS BOWDERY-MUSAFE CALLED EVEN FOR NON-MU
C      CHANGE 15.00 02/12/83 HUGH MCCANN - TO COPE WITH NEW LEAD GLASS.
C      CHANGE 22.00 22/10/83 HUGH MCCANN - BUGS REMOVED .
C      CHANGE 13.30 06/05/83 CHRIS BOWDERY- STRUCTURAL IMPROVEMENTS
C      CHANGE 18.10 21/03/83 CHRIS BOWDERY- NEW RESOLUTION/OVLCUT
C      CHANGE 17.41 29/10/82 IAN GLENDINNING - SET SAFE ACCEPTANCE FLAG.
C      CHANGE 19.07 16/03/82 CHRIS BOWDERY- CLEAR VARIABLES FOR MULOCH.
C      CHANGE 14.30 04/02/82 HUGH MCCANN  - CLEAR CWORK FOR EACH TRACK.
C
C
C    FOLLOWS INNER DETECTOR TRACKS OUT AND FINDS ASSOCIATED MUON CHAMBER
C    HITS (PHILOSOPHY 2).
C
C-----------------------------------------------------------------------
C
C                     D E F I N I T I O N S
C
C-----------------------------------------------------------------------
C
C  IPATR,APATR    I.D. PATREC DATA (SEE JADE COMP. NOTE 12, 23/2/79.)
C  NTRKS          NUMBER OF TRACKS.
C  NWTRK          NUMBER OF WORDS PER TRACK.
C  BGAUSS         MAGNETIC FIELD IN GAUSS.
C  HC             CONTAINS MUON HIT COORDINATES.
C  NHITS          NUMBER OF MUON HITS.
C  NWHIT          NUMBER OF WORDS PER HIT IN HIT COORDINATE ARRAY.
C  HLUN           HIT STATUS ARRAY.
C  IMUR,AMUR      MU-RESULTS 'MUR2' BANK 1 - FILLED BY MUFFLE.
C  NWMUR          NUMBER OF WORDS PER I. D. TRACK IN 'MUR2' BANK 1.
C  HTC            HIT-TRACK CORELLATION ARRAY - FILLED BY MUFFLE.
C  HAMB           AMBIGUITY FLAG FOR EACH ENTRY IN HTC.
C  NTPH           NUMBER OF TRACKS PER HIT ALLOCATED IN HTC.
C  EXTRA          STORES THE POINTS ON THE EXTRAPOLATED TRACK.
C  NPL            NUMBER OF POINTS PER EXTRAPOLATED TRACK.
C  EBGEV          BEAM ENERGY FOR THIS EVENT (IN GEV).
C  NHLAYR         NO. OF LAYERS WHICH WENT OFF FOR EACH TRACK.
C
C-----------------------------------------------------------------------
C
      IMPLICIT INTEGER*2 (H)
C
C                 LOGICAL VARIABLE FOR USE IN NEW LG CALCULATIONS:
      LOGICAL IHRING
C
C             COMMONS.  (CMULOC IS USED IN THE 'LET-OFF' CHECK)
C
#include "cmucalib.for"
#include "cmuffl.for"
#include "cmufwork.for"
#include "cmureg.for"
C
      COMMON / CMUIEV / IEV,NEV,IRD,KRUN,KREC,
     +                  ISECS,IMINS,IHOURS,IDAY,IMONTH,IYEAR
      COMMON / CMULOC / ALLDEV(12,2),ALLABS(12),REGDEV(30),REGDCE(60),
     +                  REGZE(60),LOCHRS(60),ABLOCH(60)
C
      DIMENSION  IPATR(1),APATR(1),HC(1),HLUN(1),IMUR(1),AMUR(1),
     +           HTC(1),HAMB(1),EXTRA(1),CUTS(3)
C
C             CUTS IS VECTOR OF CUTS FOR MUREGY *  CHAMBER INEFFICIENCY.
C
      DATA  CUTS /3*0.0/ , CHINEF /0.05/
C
C------------------  C O D E  ------------------------------------------
C
CDEL1
C     WRITE(6,1863)BGAUSS
C1863 FORMAT(' BGAUSS =',F10.4)
CDEL2
C
C             RCOILM IS MEAN COIL RADIUS.  RCOISQ IS ITS SQUARE.
C***    NO    22/10/83 ...... USE RCOISQ=RCOILI**2 .
C
      RCOILM = 0.5*(RCOILI + RCOILO)
      RCOISQ = RCOILI**2
C
C             SET UP POINTERS FOR MAIN LOOP.
C             .   IPMU IS POINTER TO TRACK INFORMATION IN AMUR.
C             .   IPL IS POINTER TO TRACK INFORMATION IN EXTRA.
C             .   NELIPS IS NUMBER OF MULTIPLE SCATTERING ELIPSES
C                                               ENTERED IN ELIPSE.
C
      IPT   = 0
      IPMU  = 0
      IPL   = 0
      NELIPS= 0
C
C-----------------------------------------------------------------------
C                  S T A R T   O F   M A I N   L O O P
C-----------------------------------------------------------------------
C
C             LOOP OVER TRACKS.                      ***** START LOOP 1.
C
      DO  1000  ITRK = 1,NTRKS
C
C-----------------------------------------------------------------------
C
C             CLEAR  COUNTER  WHICH  COUNTS   NUMBER   OF   SUCCESSFULLY
C             ASSOCIATED HITS FOR THIS TRACK AND THE TWO WHICH COUNT THE
C             NUMBER OF LAYERS WITH HITS ASSOCIATED WITH THIS TRACK.
C
C
      NTHIS  = 0
      NHLAYR = 0
      LAYRAB = 0
      NGLAYR = 0
C
C             CLEAR HIT DEVIATIONS USED IN THE 'LET-OFF' CHECK (LOCH)
C
      CALL VZERO(ALLDEV,24)
      CALL VZERO(ALLABS,12)
C
C             CLEAR INEFFICIENCY FLAG (.NE. 0 IF INEFFICIENCY OCCURRED
C                                   IN LAST CHAMBER REGION INTERCEPTED).
C             CLEAR INEFFICIENCY COUNTER FOR THIS TRACK.
C             CLEAR ARRAY OF REGION NOS. OF ALL ASSOCIATED HITS.
C             CLEAR ARRAY OF REGION NOS. OF  THOSE  REGIONS  WHICH  HAVE
C                                                       ASSOCIATED HITS.
      INFLAG = 0
      INEFF  = 0
      CALL VZERO(IHTREG,20)
      CALL VZERO(IEFFRG,10)
C
C             CLEAR ERROR MATRIX AND OVERLAP HIT MATRIX FOR THIS TRACK
C
      CALL VZERO(EM,900)
      CALL VZERO(IOVLAP,100)
C
C-----------------------------------------------------------------------
C
C             CLEAR COMMON /CWORK/ FOR THIS  TRACK  (BUT  NOT  LOCATIONS
C             ELIPSE(700) WHICH STORES PARAMS OF  ALL  ELLIPSES  OF  THE
C             EVENT, OR NELIPS WHICH COUNTS THE NO. OF ELLIPSES  IN  THE
C             EVENT, OR HBADCH(1000) )
C
      NELOLD = NELIPS
      CALL VZERO(X,4110)
      CALL VZERO(INCHAM(1),50)
      CALL VZERO(CTDASH(1),720)
      NELIPS = NELOLD
C
C-----------------------------------------------------------------------
C
C             FILL FIRST 3 WORDS OF INFORMATION.
C             TRACK NUMBER, PROGRAM IDENTIFIER AND DATE OF THIS VERSION.
C
      IDTRK = IPATR(IPT+1)
      IMUR(IPMU+1) = IDTRK
      AMUR(IPMU+2) = PGMID
      IMUR(IPMU+3) = 880208
C
C++++
C     WRITE(6,1100)ITRK
C1100 FORMAT('0TRACK',I5)
C++++
C-----------------------------------------------------------------------
C                  S T E P   1
C-----------------------------------------------------------------------
C
C             FOLLOW INNER DETECTOR TRACK OUT.
C
 1111 CONTINUE
C
C             FROM 19/05/81 , EFFECTIVELY HAVE NO CUTS ON TRACK QUALITY.
C             ----- ONLY INSIST THAT NHZ * NHXY > 0  .
C             AND LOWER LIMITS ON RMSXY > 185 MICRONS , RMSZ > 20 MM.
C
      NHXY  = IPATR(IPT+24)
      NHZ   = IPATR(IPT+33)
      IF(NHXY.LT.1 .OR. NHZ.LT.1) GO TO 1113
      RMSXY = APATR(IPT+23)
      RMSZ  = APATR(IPT+32)
C
C++++
C     WRITE(6,1105)RMSXY,RMSZ
C1105 FORMAT(1H+,13X,'RMSX-Y =',F6.2,2X,'RMSR-Z =',F6.2)
C++++
C
C             IF RMSXY/RMSZ ARE CLAIMED TO BE BETTER THAN THE NOMINAL
C             VALUES ( NOZAKI NOTE * DRUMM ET AL. DESY 80/38 ),
C             SET TO NOMINAL.
C
      IF(RMSXY.LT.0.185) RMSXY =  0.185
      IF(RMSZ .LT.20.0 ) RMSZ  = 20.0
C
C             SET UP STEPPING LOOP PARAMETERS AND OTHER PARAMETERS.
C
      X1 = APATR(IPT+5)
      Y1 = APATR(IPT+6)
      Z1 = APATR(IPT+7)
      D1 = SQRT(X1**2+Y1**2+Z1**2)
      X2 = APATR(IPT+12)
      Y2 = APATR(IPT+13)
      Z2 = APATR(IPT+14)
      D2 = SQRT(X2**2+Y2**2+Z2**2)
C
      XMID = 0.5*(X1+X2)
      YMID = 0.5*(Y1+Y2)
      ZMID = 0.5*(Z1+Z2)
      DTC  = 0.5*(D1+D2)
C
      X = X2
      Y = Y2
      Z = Z2
      DCX = APATR(IPT+15)
      DCY = APATR(IPT+16)
      DCZ = APATR(IPT+17)
C
C             CHECK DIRECTION COSINES
C
      CALL MUDCHK(1)
C
      ADCZ   = ABS(DCZ)
      COSEC  = 1. / SQRT(1.00001-DCZ**2)
      VTXYAN = 12. * RMSXY**2 / (NHXY*(D2-D1)**2)
      VTZANG = 12. * RMSZ**2  / (NHZ*(D2-D1)**2*COSEC**2)
      VTXYD  = RMSXY**2 / NHXY
      VTZD   = RMSZ**2  / (NHZ*COSEC**2)
      STPINI = XYSTEP * COSEC
      CURV   = - APATR(IPT+28)
C
C             PARTICLE CHARGE IS SIGN OF APATR(IP+28)*BGAUSS.
C
      CHARGE = SIGN( 1.0 , -CURV*BGAUSS)
C**** DANG   = CURV  * STPINI  <-- REPLACED  22/10/83  BY :
      DANG   = CURV  * XYSTEP
      DZ     = DCZ   * STPINI
      PT     = 3.E-8 * BGAUSS / CURV
C
C++++
C             OUTPUT SOME TRACK INFO
C     WRITE(6,1101)DCX,DCY,DCZ,PT
C1101 FORMAT(1H+,55X,19HDIRECTION COSINES :,3F8.3,2X,4HPT =,F10.3)
C     WRITE(6,3498)VTXYAN,VTZANG,VTXYD,VTZD
C3498 FORMAT(' VTXYAN,VTZANG,VTXYD,VTZD',4E15.4)
C++++
C
C             REJECT TRACKS WITH VERY LOW TRANSVERSE MOMENTUM  .
C
      IF(ABS(PT).LT.0.1) GO TO 1112
C
C             FILL EXTRA
C
      EXTRA(IPL+1) = X2
      EXTRA(IPL+2) = Y2
      EXTRA(IPL+3) = Z2
      P = ABS(PT*COSEC)
      AMUR(IPMU+38)= P
C
C                  IF P > PMAX, PUT P=PMAX (FOR MU TRACKING PURPOSES).
C                  NORMALLY PMAX IS BEAM ENERGY UNLESS THE FLAG PMXFLG
C                  IS NON-ZERO. IN THIS CASE, PMAX = PMXFLG.
C
      PMXVAL = PMXFLG
      IF( PMXVAL .EQ. 0.0 ) PMXVAL = EBGEV
      IF( P .GT. PMXVAL ) P = PMXVAL
C
      PSQ = P**2
      ESQ = PSQ+WMUSQ
      E   = SQRT(ESQ)
      D   = 0.0
      GM  = 0.0
      AB  = 0.0
      RD  = 0.0
      DE  = 0.0
C
      PPIDK  = 0.0
      PKDK   = 0.0
      PPBPEN = 1.0
      PPPEN  = 1.0
      PPIPEN = 1.0
      PKPEN  = 1.0
      VMSANG = 0.0
      VMSD   = 0.0
      CMS    = 0.0
C
C             INITIALISE FLAG TO DISTINGUISH THOSE TRACKS FOR  WHICH  NO
C             INTERCEPTS ARE EXPECTED. IN THE BARREL, THIS  MEANS  THOSE
C             TRACKS WHICH DO NOT GO AS FAR AS THE RADIUS OF  THE  OUTER
C             SURFACE OF THE BARREL LEAD GLASS. IN THE END CAP REGIONS,
C             THIS MEANS THOSE TRACKS WHICH DO NOT  PENETRATE  PAST  THE
C             END CAP IRON.
C
      INSTOP = 1
C
C             STEP OUT TO COIL OR END CAP.         ***** START LOOP 2.
C
 2000 CONTINUE
      DR  = X*DCX + Y*DCY
      IF(DR.LT.0.) GO TO 1112
      X   = X + DCX*STPINI
      Y   = Y + DCY*STPINI
      Z   = Z + DZ
C        22/10/83 : AT LEAST THE NEW DCX SHOULD BE ASSIGNED TO A TEMP
C                   VARIABLE SO THAT THE NEW DCY IS CORRECTLY CALC'D .
C***  DCX = DCX - DCY*DANG       WRONG
C***  DCY = DCY + DCX*DANG       WRONG   REPLACED BY :
      DCXNEW = DCX - DCY*DANG
      DCYNEW = DCY + DCX*DANG
      DCX    = DCXNEW
      DCY    = DCYNEW
C
C             CHECK DIRECTION COSINES
C
      CALL MUDCHK(2)
C
      IF(ABS(Z).GT.ZEP) GO TO 2222
      RSQ = X**2 + Y**2
      IF(RSQ.GT.RCOISQ) GO TO 3333
C
C             END STEPPING LOOP.                   ***** END LOOP 2.
C
      GO TO 2000
C
C             TRACK CURLS BACK TOWARDS INTERACTION  POINT.  ENERGY  LOW.
C             IGNORE. OR TRACK HAS VERY LOW TRANSVERSE MOMENTUM.
C
 1112 IMUR(IPMU+6) = -1
      GO TO 1001
C
C             TRACK REJECTED
C
 1113 IMUR(IPMU+6) = -2
      GO TO 1001
C
C-----------------------------------------------------------------------
C                  S T E P   2
C-----------------------------------------------------------------------
C
C             TRACK THROUGH END-CAP LEAD-GLASS AND END-PLUG IRON.
C             (STEPS 2 AND 3 ARE ALTERNATIVES.)
C
C             FILL EXTRA.
C
 2222 EXTRA(IPL+4) = X
      EXTRA(IPL+5) = Y
      EXTRA(IPL+6) = Z
C
C             ASSUME VERTEX IS INTERACTION POINT.
C             ASSUME TRACK HAS PASSED THROUGH END-CAP LEAD-GLASS.
C
C             BACK-TRACKING AND 1ST STEP SAME AS IN STEP 3.
C
      CALL MUFFLB
C
      CALL MUFFLS(*5555)
C
C-----------------------------------------------------------------------
C
C             STEP  FROM  MID-POINT  TO  END-CAP  LEAD-GLASS   SURFACE.
C             (ASSUME PARTICLE PASSES THROUGH 3RD SUPPORT AND END-PLATE,
C             BUT NOT THE OUTER CYLINDER OF THE PRESSURE VESSEL.)
C
      DSTEP  = D3/2.0 - TEPLG/ADCZ
      GMSTEP = GMJET2*COSEC + GMJETE/ADCZ
      ABSTEP = ABJET2*COSEC + ABJETE/ADCZ
      RDSTEP = RDJET2*COSEC + RDJETE/ADCZ
      DESTEP = DEJET2*COSEC + DEJETE/ADCZ
C
      CALL MUFFLS(*5555)
C
C-----------------------------------------------------------------------
C
C             PRESERVE SPECIAL VARIABLES HERE TO  GIVE  VALUES  AT  LAST
C             SEEN POINT TO GOOD APPROXIMATION.
C
      CALL UCOPY(X,X0,NSPECI)
C
C-----------------------------------------------------------------------
C
C             STEP THROUGH END-CAP LEAD-GLASS.
C
      DSTEP  = TEPLG /ADCZ
      GMSTEP = GMEPLG/ADCZ
      ABSTEP = ABEPLG/ADCZ
      RDSTEP = RDEPLG/ADCZ
      DESTEP = DEEPLG/ADCZ
C
      CALL MUFFLS(*5555)
C
C-----------------------------------------------------------------------
C
C             STEP THROUGH END-CAP.
C
      DSTEP  = TEP /ADCZ
      GMSTEP = GMEP/ADCZ
      ABSTEP = ABEP/ADCZ
      RDSTEP = RDEP/ADCZ
      DESTEP = DEEP/ADCZ
C
      CALL MUFFLS(*5555)
C
C             THIS PARTICLE PENETRATED THE EC LEAD GLASS AND THE EC.
C             FILL EXTRA.
C
      INSTOP = 0
      EXTRA(IPL+7) = X
      EXTRA(IPL+8) = Y
      EXTRA(IPL+9) = Z
C
C-----------------------------------------------------------------------
C
C             BEND IN FIELD OF ENDCAP. BENDING  DONE  ALL  IN  ONE  STEP
C             USING  INTEGRATED  B*DL.  ALSO  NOTE  BENDING  DONE  AFTER
C             PASSING THROUGH END CAP, WHICH IS NOT STRICTLY CORRECT BUT
C             SHOULD LEAD TO VERY SMALL ERRORS.
C
C             FOR  R.LT.RCOILM  THE FLUX IS  BCENT *PI * R**2. THEREFORE
C             INTEGRATED B*DL IS FLUX / (2 * PI * R) = BCENT*R / 2.
C
C             FOR  R.T.RCOIL  FLUX  IS BCENT * PI * RCOIL**2. THEREFORE
C             INTEGRATED B*DL IS FLUX / (2 * PI*R)=BCENT*RCOIL**2/(2*R).
C
C             THE COMPONENTS ARE BLX = SIGN(Z)*BL*X/R,
C                                BLY = SIGN(Z)*BL*Y/R,
C                                BLZ = 0.0
C
C             THEN DP=SIGN(CHARGE)*0.3E-3*(CROSS PRODUCT(DCOS*BL)),
C
C             WHERE DP     IS VECTOR CHANGE IN MOMENTUM,
C                   DCOS   IS UNIT VECTOR PARALLEL TO P,
C                   BL     IS INEGRATED B*DL VECTOR.
C
C             CALCULATE INTEGRATED B*DL.
C
 2224 CONTINUE
      BTESLA = 0.0001 * BGAUSS
      RSQ    = X**2 + Y**2
      R      = SQRT(RSQ)
      IF(R.GT.RCOILM) GO TO 2225
      BL     = BTESLA * R / 2.0
      GO TO 2226
 2225 CONTINUE
      BL     = BTESLA * RCOILM**2 / (2.0 * R)
 2226 CONTINUE
C
C             CALCULATE COMPONENTS OF BL.
C
      BLX = BL * X / R
      BLY = BL * Y / R
      IF(Z.GT.0) GO TO 2227
      BLX = - BLX
      BLY = - BLY
 2227 CONTINUE
C
C             CALCULATE CHANGE IN MOMEMTUM.
C
      CON = 0.3E-3 * CHARGE
      DPX = - CON * DCZ * BLY
      DPY =   CON * DCZ * BLX
      DPZ =   CON * (DCX * BLY  -  DCY * BLX)
C
C             CALCULATE NEW DIRECTION COSINES.
C
      DCX = DCX + DPX / P
      DCY = DCY + DPY / P
      DCZ = DCZ + DPZ / P
C
C             CHECK DIRECTION COSINES
C
      CALL MUDCHK(3)
C
      ADCZ  = ABS(DCZ)
      COSEC = 1.0 / SQRT(1.00001 - DCZ**2)
C
C             STORE VALUES OF SPECIAL VARIABLES ETC. FOR RE-TRACKING.
C
      CALL UCOPY(X,X7,NSPECI)
      X7(NSPECI+1) = VMSD
      X7(NSPECI+2) = CMS
      X7(NSPECI+3) = VMSANG
C
C             GO TO STEP 4.
C
      GO TO 4444
C
C-----------------------------------------------------------------------
C             S T E P   3
C-----------------------------------------------------------------------
C
C             TRACK THROUGH COIL AND BARREL LEAD GLASS.
C             (STEPS 2 AND 3 ARE ALTERNATIVES.)
C             FILL EXTRA.
C
 3333 EXTRA(IPL+4) = X
      EXTRA(IPL+5) = Y
      EXTRA(IPL+6) = Z
C
C             ASSUME VERTEX IS INTERACTION  POINT.  IGNORE  INCREASE  IN
C             LENGTH DUE TO CURVATURE.
C
C             BACK-TRACKING AND 1ST STEP SAME AS IN STEP 2.
C
      CALL MUFFLB
C
      CALL MUFFLS(*5555)
C
C-----------------------------------------------------------------------
C
C             STEP FROM MID-POINT TO COIL.
C
      DSTEP  = D3 / 2.0
      GMSTEP = (GMJET2 + GMJET4 + GMJETO) * COSEC
      ABSTEP = (ABJET2 + ABJET4 + ABJETO) * COSEC
      RDSTEP = (RDJET2 + RDJET4 + RDJETO) * COSEC
      DESTEP = (DEJET2 + DEJET4 + DEJETO) * COSEC
C
      CALL MUFFLS(*5555)
C
C-----------------------------------------------------------------------
C
C             PRESERVE SPECIAL VARIABLES HERE TO  GIVE  VALUES  AT  LAST
C             SEEN POINT TO GOOD APPROXIMATION.
C
      CALL UCOPY(X,X0,NSPECI)
C
C-----------------------------------------------------------------------
C
C             STEP THROUGH COIL.
C
       DSTEP  = TCOIL  * COSEC
C***     22/10/83 :
C        CHECK THAT THIS WON'T PUT THE TRACK INTO THE YOKE END PLATE.
      FACOIL=1.
      IF(  ABS(Z) + ABS(DSTEP*DCZ) .GT. (IZEIO-5.) )
     *           FACOIL = ( (IZEIO-5.) - ABS(Z) )  /  ABS(DSTEP*DCZ)
       DSTEP = DSTEP  * FACOIL
      GMSTEP = GMCOIL * ( COSEC * FACOIL )
      ABSTEP = ABCOIL * ( COSEC * FACOIL )
      RDSTEP = RDCOIL * ( COSEC * FACOIL )
      DESTEP = DECOIL * ( COSEC * FACOIL )
C
      CALL MUFFLS(*5555)
C
C-----------------------------------------------------------------------
C
C             PROJECT TO LEAD-GLASS BARREL  END  PLANES,  Z=+-ZLG.  THE
C             IDEA IS TO FIND OUT IF IT PASSES THROUGH  LEAD  GLASS  AT
C             ALL, AND IF SO, FIND OUT S,  THE  FRACTION  OF  THICKNESS
C             TRAVERSED (WHICH IS LESS THAN 1. IF IT CLIPS THE END).
C
C             HUGH MCCANN 02/12/83 :
C             ----------------------
C             UPDATE FOR CHANGES TO LEAD GLASS IN WINTER 82-83 SHUTDOWN:
C             (A)  2 NEW HALF-RINGS WERE ADDED AT -X+Z AND +X-Z
C                  ... SO THE ZLIMIT OF THE BARREL IS INCREASED TO
C                  ZLGHAF IN THESE QUADRANTS.
C                  INSTEAD OF JUST ZLG , WE HAVE TO USE A TEMPORARY
C                  VARIABLE ZLGBAR WHICH CAN VARY DEPENDING ON THE
C                  YEAR OF DATA-TAKING AND THE DIRECTION OF THE TRACK.
C                  JUST TO BE AWKWARD , THE HALF-RINGS ARE ONLY 25 CM
C                  DEEP COMPARED TO THE 30 CM DEPTH OF THE REST OF THE
C                  BARREL .  THIS IS TGHAF IN THE BLOCK DATA.
C                  SO , WE HAVE TO TEST R AT BOTH ZLG AND ZLGHAF.
C
C             (B)  HIGHER-DENSITY LEAD GLASS WAS PLACED IN THE CENTRAL
C                  PART OF THE BARREL, COVERING  -ZLGC TO +ZLGC .
C
C             XT,YT,ZT,RSQT,RT ARE TEMPORARY WORKING VARIABLES.
C
      ZLGBAR=ZLG
      IHRING=.FALSE.
      IF(IYEAR.LT.1983)GO TO 3330
           IF(  (DCX.LT.0. .AND. DCZ.GT.0. ) .OR.
     *          (DCX.GT.0. .AND. DCZ.LT.0. )      )IHRING=.TRUE.
           IF(IHRING)ZLGBAR=ZLGHAF
 3330 S = 1.0
      IF(ADCZ.LT.0.01) GO TO 3335
          IF(Z.LT..0) ZLGBAR=-ZLGBAR
             DZDCZ = (ZLGBAR-Z) / DCZ
C            FOR IHRING TRUE , ALSO NEED TO KNOW R AT ZLG :
             IF(.NOT.IHRING)GO TO 3336
                 DISZLG = (ZLG-Z) / DCZ
                 IF(Z.LT..0)DISZLG=(-ZLG-Z) / DCZ
 3336        CONTINUE
C
             XT   = X + DCX * DZDCZ
             YT   = Y + DCY * DZDCZ
             RSQT = XT**2 + YT**2
             RT   = SQRT(RSQT)
C            IF TRACK MISSED BARREL LG ALTOGETHER , GO TO 3338 :
             IF(RT.LT.RLG) GO TO 3338
C               IF IHRING NOT TRUE , CAN NOW JUST FOLLOW OLD PROCEDURE .
                IF(IHRING)GO TO 3331
C                   IF R AT ZLG IS GT R OF OUTER LG SURFACE, S IS 1.0 .
                    IF(RT.GT.RLG+TLG) GO TO 3335
C                       OTHERWISE , CALCULATE THICKNESS TRAVERSED.
                        GO TO 3337
C
C                    IN THE CASE WHERE IHRING IS TRUE :
C                    -----------------------------------
 3331            XTZLG  = X + DCX * DISZLG
                 YTZLG  = Y + DCY * DISZLG
                 RSQZLG = XTZLG**2 + YTZLG**2
                 RTZLG  = SQRT(RSQZLG)
C                IF R AT ZLG IS GT R OF OUTER LG SURFACE, S IS 1.0 .
                 IF(RTZLG.GT.RLG+TLG) GO TO 3335
C                        TO BE HERE , THIS TRACK HAS IHRING TRUE AND ITS
C                        VALUE OF R AT ZLG IS BETWEEN RLG AND (RLG+TLG).
C                        STILL HAVE 3 CASES :
C                        (A)  R(ZLGHAF) < RLG + TLGHAF
                      IF(RT.LT.RLG+TLGHAF)GO TO 3337
C                            (B)  R(ZLG) > RLG + TLGHAF
                         RT=RTZLG
                         IF(RT.GT.RLG+TLGHAF)GO TO 3337
C                                (C) FOR THIS ORDER OF TESTS, NOW
C                                    KNOW THAT THIS TRACK
C                                    LEAVES LG THRO' OUTSIDE RADIAL
C                                    SURFACE OF THE NEW HALF-RING .
                            RT=RLG+TLGHAF
3337  S  = (RT - RLG) / TLG
C
C-----------------------------------------------------------------------
C
C             STEP THROUGH BARREL LEAD-GLASS.
C
 3335 CONTINUE
C
C     CHECK IF TRACK TRAVERSES ANY SF6.
C     I.E. CHECK YEAR, AND , IF S .LT. 0.999 , CAN'T BE NEAR CENTRE.
      IF(IYEAR.LT.1983 .OR. S.LT.0.999)GO TO 3342
          IF(ADCZ.LT.0.01) GO TO 3340
C           CALCULATE R AT ZLGC.
            DLGC = (ZLGC-Z) / DCZ
            IF(Z.LT..0)DLGC=(-ZLGC-Z) / DCZ
            XT   = X + DCX * DLGC
            YT   = Y + DCY * DLGC
            RSQT = XT**2 + YT**2
            RT   = SQRT(RSQT)
C           IF TRACK HASN'T REACHED BARREL LG , GO TO 3342 :
            IF(RT.LT.RLG) GO TO 3342
C              IF R AT ZLGC IS GT R OF OUTER LG SURFACE, THEN ONLY
C              SF6 AND S IS 1.0.
               IF(RT.GT.RLG+TLG) GO TO 3340
C                 OTHERWISE , CALCULATE SF6 THICKNESS TRAVERSED.
                  S = ( RT - RLG ) / TLG
C
C              STEP THROUGH SF6 :
C              ------------------
 3340          DSTEP  = (S * COSEC) * TLG
               GMSTEP = (S * COSEC) * GMLGC
               ABSTEP = (S * COSEC) * ABLGC
               RDSTEP = (S * COSEC) * RDLGC
               DESTEP = (S * COSEC) * DELGC
C
               CALL MUFFLS(*5555)
C
C              CHECK IF STILL HAVE TO GO THRO' SOME SF5 .
               IF(S.GT.0.999)GO TO 3338
                   S = ( RLG+TLG - RT ) / TLG
C
C     STEP THROUGH SF5 :
C     ------------------
 3342 DSTEP  = (S * COSEC) * TLG
      GMSTEP = (S * COSEC) * GMLG
      ABSTEP = (S * COSEC) * ABLG
      RDSTEP = (S * COSEC) * RDLG
      DESTEP = (S * COSEC) * DELG
C
      CALL MUFFLS(*5555)
C
C             FIND INTERSECTION WITH INTERNAL CHAMBERS AND YOKE.
C
C
C             THIS PARTICLE DID NOT STOP BEFORE R = RADIUS OF THE OUTER
C             SURFACE OF THE BARREL LEAD GLASS.
C
 3338 INSTOP=0
C
C             BUT FIRST, STORE VALUES OF  SPECIAL  VARIABLES  ETC.  FOR
C             RE-TRACKING.
C
      CALL UCOPY(X,X7,NSPECI)
      X7(NSPECI+1) = VMSD
      X7(NSPECI+2) = CMS
      X7(NSPECI+3) = VMSANG
C
C             CALL MUREGM TO GET MASK FOR MUREGY.
C             THEN MULTIPLY BY 256 TO SELECT YOKE REGIONS.
C
      CALL MUREGM(DCX,CUTS,HMASK)
      HMASK = 256 * HMASK
C
C             CALL MUREGY.
C               1ST ARGUMENT =0 FOR ALL TYPES OF REGIONS.
C               2ND ARGUMENT .NE.0 TO SELECT ONLY +VE DIRECTION REGIONS.
C               3ND ARGUMENT SET TO -1 IF ALL OK.
C               4TH ARGUMENT IS X,Y,Z,DCX,DCY,DCZ.
C               5TH ARGUMENT SELECTION MASK.
C               6TH ARGUMENT NUMBER OF INTERCEPTS FOUND.
C
      CALL MUREGY(0,1,IER,X,HMASK,NCEPTS)
C
C++++
C     ASSIGN 3339 TO LABEL2
C     GO TO 200
C3339 CONTINUE
C++++
C
C             ONLY IER=-1 ACCEPTABLE  BECAUSE  IT  MUST,  REPEAT,  MUST
C             INTERCEPT SOMETHING. SO JUMP ON IF ACCEPTABLE.
C
      IF(IER.EQ.-1) GO TO 3350
C
C             HAVE WRONG IER FROM MUREGY , PUT WORD 6 OF  MUR2/1  =  -3
C             AND  LOOK  AT  REMAINING  TRACKS.  (THIS   SHOULD   NEVER
C             HAPPEN!!!! SORT IT OUT!!!!)
C
C             22/10/83 :  I T    I S    N O W    S O R T E D    O U T  !
C
        CALL MUERRY('MUFFLE',IER,'WRONG SIDE OF YOKE???????????^')
        IMUR(IPMU+6) = -3
C+++    WRITE(6,3346)KRUN,KREC,IDTRK,P,X2,Y2,Z2,
C    * APATR(IPT+15),APATR(IPT+16),APATR(IPT+17),
C    * EXTRA(IPL+4),EXTRA(IPL+5),EXTRA(IPL+6),
C    * X0,Y0,Z0,
C    * X,Y,Z,DCX,DCY,DCZ
C3346  FORMAT(//'0 RUN, RECORD, TRACK ',
C    *  3I6,'  MOMENTUM =',F7.3,/
C    *'0 ORIGINAL X, Y, Z, DCX, DCY, DCZ  =',3F9.1,3F9.4,/
C    *'0 BRANCH   X, Y, Z =',3F9.1,/
C    *'0 AFTER D3 X, Y, Z =',3F9.1,/
C+++ *'0  CURRENT X, Y, Z, DCX, DCY, DCZ  =',3F9.1,3F9.4,//)
        GO TO 1001
C
C
C-----------------------------------------------------------------------
C
C             LOOK THROUGH INTERCEPTS.
C
 3350 IF(NCEPTS.GT.0) GO TO 3360
        CALL MUERRY('MUFFLE',NCEPTS,'NO INTERCEPTS FOUND WITH YOKE.^')
        GO TO 1001
C
C             GO TO 5555 IF TRACK STOPS (I.E. RUNS OUT OF ENERGY).
C             NORMAL RETURN IF HIT FOUND AND PARTICLE NOT STOPPED.
C
 3360 CALL MUFFLY(HC,NHITS,NWHIT,HLUN,HTC,HAMB,NTPH,*5555)
C
C             BACK-TRACK  TO  LAST  NON-CHAMBER   REGION   EXIT   POINT
C             (I.E. YOKE). (THIS  IS  NECESSARY  BECAUSE   UNDER   SOME
C             CIRCUMSTANCES, NOW THAT THE CHAMBER REGIONS ARE  EXTENDED
C             IN MUREG, THE LAST EXIT POINT CAN BE FOR A CHAMBER REGION
C             IN THE ARTIFICIALLY EXTENDED PORTION.)
C
      DSTEP  = -DOVER
      GMSTEP = -GMOVER
      ABSTEP = -ABOVER
      RDSTEP = -RDOVER
      DESTEP = -DEOVER
C
      CALL MUFFLS(*5555)
C
C             FILL EXTRA.
C
      EXTRA(IPL+7) = X
      EXTRA(IPL+8) = Y
      EXTRA(IPL+9) = Z
C
C-----------------------------------------------------------------------
C
C             CHECK  WHICH  YOKE  PIECE.  GO  TO  END-CAP  ROUTINE   IF
C             APPROPRIATE.
C
      IF(HRFACE(IREG(NCEPTS)).GE.5) GO TO 2224
C
C-----------------------------------------------------------------------
C
C             BEND IN YOKE. (NOTE THAT BEND IS MADE AFTER YOKE HAS BEEN
C             TRAVERSED. THIS WILL INTRODUCE A SLIGHT ERROR.  A  BETTER
C             PLACE TO DO IT WOULD BE THE YOKE CENTRE OR DEAL  WITH  IT
C             IN MUFFLS. BUT THE ERROR WILL BE SMALL SO IGNORE  IT  FOR
C             NOW.)
C
C             MAG FIELD IN SIDE, TOP OR BOTTOM OF YOKE.
C
C             TOTAL FLUX = B*PI*R**2 = BL*PERIMETER, WHERE BL IS
C             INTEGRAL B*DL FOR YOKE. MULTIPLY BY 0.0001  TO  GET  FROM
C             GAUSS TO TESSLA.
C
      BL = 0.0001 * BGAUSS * 3.14159 * RCOILM**2 / 14680.
C
C             CONSTANTS (DONT FORGET VECTOR B=-BYOKE*Z(UNIT VEC)
C             CON = 0.3E-3 * MAGFIELD * PARTCHARGE
C
C             WITH THIS NORMALISATION ALL DIS SHOULD BE IN MM, ALL MTA
C             IN GEV AND ALL FIELDS IN TESLA.
C
      CON = 0.3E-3 * BL * CHARGE
C             CALCULATE THE CHANGE IN MTM
      DPX = -DCY * CON
      DPY =  DCX * CON
C
C             CALCULATE NEW DIRECTION COSINES.
C
      DCX = DCX + DPX / P
      DCY = DCY + DPY / P
C
C             CHECK DIRECTION COSINES
C
      CALL MUDCHK(4)
C
C             CONTINUE TO STEP 4.
C
C-----------------------------------------------------------------------
C                   S T E P   4
C-----------------------------------------------------------------------
C
C             CONTINUE TRACKING.
C
C             CALL MUREGM TO GET MASK FOR MUREGY. LEAVE HMASK UNTOUCHED
C             SO THAT IT SELECTS ONLY REGIONS OUTSIDE YOKE.
C
 4444 CALL MUREGM(DCX,CUTS,HMASK)
C
C             CALL MUREGY.
C               1ST ARGUMENT =0 FOR ALL TYPES OF REGIONS.
C               2ND ARGUMENT .NE.0 TO SELECT ONLY +VE DIRECTION REGIONS.
C               3ND ARGUMENT SET TO -1 IF ALL OK.
C               4TH ARGUMENT IS X,Y,Z,DCX,DCY,DCZ.
C               5TH ARGUMENT SELECTION MASK.
C               6TH ARGUMENT NUMBER OF INTERCEPTS FOUND.
C
      CALL MUREGY(0,1,IER,X,HMASK,NCEPTS)
C
C++++
C     ASSIGN 4465 TO LABEL2
C     GO TO 200
C4465 CONTINUE
C++++
C
C             THIS TIME ACCEPT  IER=2  TOO.  (IER=2  MEANS  NO  REGIONS
C             INTERCEPTED WHICH IS POSSIBLE IF TRACK GOES  THROUGH  THE
C             GAPS.) IF ERROR, GO TO POINT NEAR END OF DO LOOP.
C
      IF(IER.EQ.-1 .OR. IER.EQ.2) GO TO 4470
        CALL MUERRY('MUFFLE',IER,'WRONG ERROR CODE FROM MUREGY.^')
        IMUR(IPMU+6) = -2
        GO TO 1001
C
C             WE HAVE AN ACCEPTABLE CODE FROM MUREGY.
C             IF NO INTECEPTS FOUND, THERE ARE NO FURTHER CHAMBERS.
C
 4470 IF(NCEPTS.LE.0) GO TO 7777
C
C             TRACK THROUGH REGIONS.
C             GO TO 5555 IF PARTICLE STOPS (I.E. RUNS OUT OF ENERGY).
C             NORMAL RETURN IF HIT FOUND AND PARTICLE NOT STOPPED.
C
      CALL MUFFLY(HC,NHITS,NWHIT,HLUN,HTC,HAMB,NTPH,*5555)
C
      GO TO 7777
C
C-----------------------------------------------------------------------
C                  S T E P   5    -  PARTICLE STOPS
C-----------------------------------------------------------------------
C
 5555 CONTINUE
C++++
C     WRITE(6,5577)
C5577 FORMAT(' PARTICLE STOPS.')
C++++
C
C             FILL EXTRA.
C
      EXTRA(IPL+10) = X
      EXTRA(IPL+11) = Y
      EXTRA(IPL+12) = Z
C
C             CHECK WHETHER THIS PARTICLE STOPPED BEFORE  REACHING  THE
C             OUTSIDE SURFACE OF THE LEAD GLASS OR BEFORE REACHING  THE
C             OUTSIDE SURFACE OF THE END CAP.
C
      IF(INSTOP.EQ.0) GO TO 5556
C
C             THIS PARTICLE FAILED TO  REACH  THE  ABOVE  BOUNDARIES  .
C             THEREFORE , IT HAS NO INTERCEPTS.
C
      EXTRA(IPL+13) = 0.0
      EXTRA(IPL+14) = 0.0
      EXTRA(IPL+15) = 0.0
      GO TO 5557
C
C             THIS PARTICLE PENETRATED THE LEAD GLASS BARREL OR THE END
C             CAP. PUT THE COORDINATES OF THE LAST INTERCEPT  FOUND  BY
C             MUREGY INTO EXTRA (IPL+ 13,14,15) .
C
C
 5556 EXTRA(IPL+13) = CINT(1,NCEPTS)
      EXTRA(IPL+14) = CINT(2,NCEPTS)
      EXTRA(IPL+15) = CINT(3,NCEPTS)
C
 5557 CONTINUE
      GO TO 8888
C
C-----------------------------------------------------------------------
C                  S T E P   6     - DEFUNCT STEP
C-----------------------------------------------------------------------
C                  S T E P   7     - NO FURTHER CHAMBERS.
C-----------------------------------------------------------------------
C
C             NO FURTHER  CHAMBERS,  I.E.  TRACK  NOT  STOPPED  BUT  NO
C             CHAMBERS IN PATH. -
C
 7777 CONTINUE
C++++
C     WRITE(6,7788)
C7788 FORMAT(' NO FURTHER CHAMBERS.')
C++++
C
      EXTRA(IPL+10) = X
      EXTRA(IPL+11) = Y
      EXTRA(IPL+12) = Z
      EXTRA(IPL+13) = X
      EXTRA(IPL+14) = Y
      EXTRA(IPL+15) = Z
C
C             CONTINUE TO STEP 8
C
C-----------------------------------------------------------------------
C                  S T E P   8       -  RE-FIT, RE-TRACK, CLASSIFY AND
C                                       FILL RESULTS BANK (MUR2/1).
C-----------------------------------------------------------------------
C
C
C             PROCESS ONLY THOSE TRACKS WHICH MANAGE TO GET OUT OF  THE
C             BARREL LEAD-GLASS OR END-CAP.
C
 8888 IF(INSTOP.NE.0) GO TO 8889
C
C             CALL MUFFLX TO CALCULATE CHI-SQUARED, ETC. DO  THIS  EVEN
C             IF THERE IS ONLY 1 MUON HIT ASSOCIATED WITH THIS TRACK SO
C             THAT (A) AMBIGUITIES ARE RESOLVED WHEN POSSIBLE  AND  (B)
C             SHARED HITS CAN BE ASSIGNED TO THE BEST TRACK.
C
      IF(NTHIS.GT.0) CALL MUFFLX(HAMB)
C
C             CALL MUFFLR TO RE-FIT MUON HITS. PRODUCES  NEW  VALUE  OF
C             X,Y,Z, DCX,DCY,DCZ. IF THERE ARE LESS THAN  2  ASSOCIATED
C             HITS  THE  RE-FITTED   TRACK   LEFT   AS   THE   EXISTING
C             EXTRAPOLATED INNER DETECTOR TRACK.
C
      CALL MUFFLR(HAMB)
C
C             FIND  X,Y,Z  ON  NEW  TRACK  NEAR  TO  OLD  TRACK   AFTER
C             LEAD-GLASS BARREL OR YOKE END-CAP.
C
      DBACK = SQRT( (X-X7(1) )**2 + (Y-X7(2) )**2 + (Z-X7(3) )**2 )
      X = X - DCX * DBACK
      Y = Y - DCY * DBACK
      Z = Z - DCZ * DBACK
C
C             PICK UP VALUES  OF  SPECIAL  VARIABLES  PERTAINING  AFTER
C             LEAD-GLASS BARREL OR YOKE END-CAP (APART FROM  X,Y,Z  AND
C             DC'S).
C
      CALL UCOPY(X7(7),P,NSPECI-6)
      VMSD   = X7(NSPECI+1)
      CMS    = X7(NSPECI+2)
      VMSANG = X7(NSPECI+3)
C+++
C     WRITE(6,1865)X,Y,Z,DCX,DCY,DCZ,P
C1865 FORMAT(' X,Y,Z,DCX,DCY,DCZ,P',7G15.5)
C+++
C
C             CALL MUREGM TO GET MASK FOR MUREGY.
C             MULTIPLY BY 256 AND ADD TO SELECT ALL REGIONS.
C
      CALL MUREGM(DCX,CUTS,HMASK)
      HMASK = HMASK + 256*HMASK
      CALL MUREGY(0,1,IER,X,HMASK,NCEPTS)
C
C++++
C     ASSIGN 8887 TO LABEL2
C     GO TO 200
C8887 CONTINUE
C++++
C
C             RETRACK TO FIND THE 'MISSING' REGIONS AND THE CAUSES
C
      CALL MUFFLT
C
C-----------------------------------------------------------------------
C
C             CLASSIFY TRACK.
C++++
C             WRITE OUT NTHIS,NHLAYR,INEFF
C     WRITE(6,8890)NTHIS,NHLAYR,INEFF,INFLAG
C8890 FORMAT(1H0,5X,7HNTHIS =,I3,3X,8HNHLAYR =,I3,3X,7HINEFF =,I3,
C    * ', INFLAG =',I2)
C++++
C
C             FILL WORD 4 : 10000*INEFF + 100*NHLAYR + NTHIS
C             FILL WORD 7 : NGLAYR
C
      IMUR(IPMU+4) = 10000*INEFF + 100*NHLAYR + NTHIS
      IMUR(IPMU+7) = NGLAYR
C
C             INTERPRET NHLAYR AND INEFF.
C             FOR PRESENT , NHLAYR > 3 AND INEFF </= 1 ===>  GOOD  MUON
C             I.E. AT LEAST 2 LAYERS FIRING OUTSIDE  MAGNET  YOKE,  AND
C             NOT MORE THAN 'NALLWM' 'MISSING' CHAMBERS ON THIS TRACK.
C             NORMALLY NUMBER OF ALLOWED MISSING CHAMBERS = 1.
C
      IF( NHLAYR .LT. 4  .OR.  INEFF .GT. NALLWM ) GO TO 8889
C
C             ** GOOD MUON **
C++++
C     WRITE(6,8891)
C8891 FORMAT(1H+,70X,15H** GOOD MUON **)
C++++
C
C             FILL RESULTS BANK FOR 'GOOD' MUON.
C
C             FOR NOW, USE CHI-SQUARED PROBABILTY SIMPLY TO SET QUALITY
C             FLAG.  ALSO,  FOR  NOW,  JUST  USE   VALUE   FROM   DRIFT
C             INFORMATION ALONE.
C
      IQUAL = 1
      IF( CHIPRT .LT. 0.1 ) IQUAL = 2
C
C             FOR EACH 'MISSING' CHAMBER ADD 10 TO QUALITY FLAG.
C             ADD 100 TO QUALITY FLAG IF 'MISSING' CHAMBER IN LAST LAYER
C             EXPECTED TO HAVE A HIT.
C
      IQUAL = IQUAL +  10*INEFF
      IF( INFLAG .NE. 0 ) IQUAL = IQUAL + 100
      IMUR(IPMU+6)  = IQUAL
C
C             STORE  CHI-SQUARED  PROBABILTY  FROM  DRIFT   INFORMATION
C             ALONE, FOR NOW.
C
      AMUR(IPMU+11) = CHIPRT
C
C             THE IF STATMENT ABOVE SELECTS  A  'GOOD'  MUON.  SO  MUON
C             PROBABILITY IS 1,  EVEN  IF  THERE  IS  AN  INEFFICIENCY,
C             UNLESS THE INEFFICIENCY IS IN THE LAST LAYER INTERCEPTED.
C
      PMUON = 1.0
      IF(IQUAL.GT.100) PMUON = CHINEF
      AMUR(IPMU+25) = PMUON
C
C             (* SET SAFE ACCEPTANCE FLAG *)
C
 8889 IMUR(IPMU+5) = MUSAFE(DCZ,180.0*ATAN2(DCY,DCX)/3.14159)
C
C             FILL REST OF RESULTS BANK.
C
      AMUR(IPMU+12) = D0
      AMUR(IPMU+13) = GM0
      AMUR(IPMU+14) = DE0
      AMUR(IPMU+15) = AB0
      AMUR(IPMU+16) = E0
      AMUR(IPMU+17) = D
      AMUR(IPMU+18) = GM
      AMUR(IPMU+19) = DE
      AMUR(IPMU+20) = AB
      AMUR(IPMU+21) = PPIDK0
      AMUR(IPMU+22) = PPIPE0
      PPUN = 0.0
      IF(P.GT.5.) PPUN = 0.01
C
      AMUR(IPMU+23) = PPUN
      AMUR(IPMU+24) = PKDK0
      AMUR(IPMU+26) = PPIPE0 + PPUN + PPIDK0
      IF(NGLAYR.LT.4) GO TO 8892
C
      AMUR(IPMU+30) = PPIPE9 + PPUN + PPIDK9
      AMUR(IPMU+31) = PKPEN9 + PPUN + PKDK9
      AMUR(IPMU+32) = PPPEN9 + PPUN
      AMUR(IPMU+33) = PPBPE9 + PPUN
      IF(NGLAYR.GE.6) GO TO 8893
C
      AMUR(IPMU+34) = AMUR(IPMU+30)
      AMUR(IPMU+35) = AMUR(IPMU+31)
      AMUR(IPMU+36) = AMUR(IPMU+32)
      AMUR(IPMU+37) = AMUR(IPMU+33)
      GO TO 8892
 8893 CONTINUE
      AMUR(IPMU+34) = PPIPE8 + PPUN + PPIDK8
      AMUR(IPMU+35) = PKPEN8 + PPUN + PKDK8
      AMUR(IPMU+36) = PPPEN8 + PPUN
      AMUR(IPMU+37) = PPBPE8 + PPUN
 8892 CONTINUE
C
C             CALL GENERAL PURPOSE ROUTINE FOR EACH 'GOOD' MUON  TRACK.
C
      IGOOD = IMUR(IPMU+6)
      IF(IGOOD.GT.0 .AND. IGOOD.LT.1000)
     +  CALL MUFFLZ (HC,NHITS,NWHIT,HLUN,IMUR,AMUR,NWMUR,HTC,HAMB,NTPH)
C
C-----------------------------------------------------------------------
C
C             END OF MAIN LOOP.                       ***** END LOOP 1.
 1001 CONTINUE
      IPMU = IPMU + NWMUR
      IPT  = IPT  + NWTRK
      IPL  = IPL  + 3 * NPL
 1000 CONTINUE
C
C-----------------------------------------------------------------------
C                  E N D   O F  MAIN LOOP OVER INNER DETECTOR TRACKS.
C-----------------------------------------------------------------------
C
      RETURN
C
C-----------------------------------------------------------------------
C
C             PROCEDURE FOR PRINTING RESULTS OF MUREGY.
C++++
C200  CONTINUE
C     WRITE(6,203)NCEPTS
C203  FORMAT('0',I5,' INTERCEPTS FOUND IN MUREGY.',
C    *T43,'   REGION    RDOTD       X         Y         Z')
C     IF(NCEPTS.LE.0)GO TO 204
C     DO 201 I=1,NCEPTS
C     WRITE(6,202)IREG(I),RDOTD(I),(CINT(J,I),J=1,3)
C202  FORMAT(I50,4F10.4)
C201  CONTINUE
C204  CONTINUE
C     GO TO LABEL2,(3339,4465,8887)
C++++
C
      END
C
C-----------------------------------------------------------------------
C             B L O C K   D A T A
C-----------------------------------------------------------------------
C
      BLOCK DATA
C
#include "cmuffl.for"
C
      DATA XYSTEP/10./
C
C             XYSTEP IS STEP LENGTH OF INITIAL EXTRAPOLATION UP TO COIL
C             OR END-CAP WITH MORE OR LESS FIXED LENGTH  IN  X-Y  PLANE
C             (MM). (STPINI IS THE ACTUAL INITIAL STEP LENGTH).
C
      DATA RCOILI/968./,RCOILO/1045./
C             RCOILI/O ARE INNER/OUTER RADII OF COIL (MM).
C
      DATA PGMID/'MUFF'/
C             PGMID IS PROGRAM IDENTIFIER.
C
      DATA WMU/.10566/,WMUSQ/.011164/
C             WMU,WMUSQ ARE MUON MASS (GEV) AND MASS-SQARED (GEV**2).
C
      DATA NSPECI/21/
C             NSPECI IS THE NUMBER OF SPECIAL  VARIBLES  ACTUALLY  USED
C             (SEE CMUFWORK).
C
      DATA XPBAR,XPROT,XPION,XKAON/100.,40.,25.,20./
C
C             THESE ARE NOMINAL CROSS-SECTIONS OF PARTICLES ON HYDROGEN
C             AT 1 GEV USED FOR SCALING INTERACTION LENGTH. (NOT  USED,
C             ACTUALLY - SEE MUFFLS).
C
C
C             THE FOLLOWING ARE
C                   R-- = RADIUS (MM),
C                   Z--  = Z COORDINATE,
C                   T--  = THICKNESS (MM),
C                   GM-- = THICKNESS (GM CM**-2),
C                   AB-- = THICKNESS (ABSORPTION LENGTHS, USING DATA
C                            BOOKLET, I.E. FOR PROTONS. SCALE FOR OTHER
C                            PARTICLES.)
C                   RD-- = THICKNESS (RADIATION LENGTHS),
C                   DE-- = ENERGY LOSS FOR MIN. IONISING PARTICLE (GEV).
C
C             BEAM PIPE AND BEAM PIPE COUNTERS (4 MM ALUMINIUM).
C             (ALUMINIUM HAS A DENSITY = 2.7 GM CM**-3,
C                    ABSORPTION LENGTH = 37.2 CM,
C                     RADIATION LENGTH = 8.9 CM,
C                         (-DE/DX) MIN = 4.37 MEV CM**-1).
      DATA TBP,GMBP,ABBP,RDBP,DEBP/4.,1.08,.011,.045,1.7E-3/
C
C             JET CHAMBER INNER PIPE (SEE JADE NOTE 17).
      DATA TJETI,GMJETI,ABJETI,RDJETI,DEJETI/0.,1.89,.019,.079,3.02E-3/
C
C             JET CHAMBER SUPPORT 1 (SEE JADE NOTE 17).
C             (SUPPORTS ASSUMED TO HAVE AN ABSORPTION LENGTH =  100  GM
C             CM**-2, (-DE/DX)MIN = 1.6 MEV GM**-1 CM**2).
C
      DATA TJET1,GMJET1,ABJET1,RDJET1,DEJET1/0.,.208,.002,.006,.33E-3/
C
C             JET CHAMBER SUPPORTS 2 AND 3 (SEE JADE NOTE 17).
      DATA TJET2,GMJET2,ABJET2,RDJET2,DEJET2/0.,.33,.003,.01,.53E-3/
C
C             JET CHAMBER SUPPORT 4 (SEE JADE NOTE 17).
      DATA TJET4,GMJET4,ABJET4,RDJET4,DEJET4/0.,.258,.003,.008,.41E-3/
C
C             JET CHAMBER OUTER WALL (SEE JADE NOTE 17).
      DATA TJETO,GMJETO,ABJETO,RDJETO,DEJETO/12.,3.42,.032,.135,5.24E-3/
C
C             COIL. ASSUMED TO BE...
C               3 MM EPOXY (MYLAR?),
C               20 MM WATER (THIS IS 2 20 MM DIAM. PIPES AVERAGED OUT),
C               57 MM ALUMINIUM.
      DATA TCOIL,GMCOIL,ABCOIL,RDCOIL,DECOIL/80.,17.8,.185,.7,30.E-3/
C
C             JET CHAMBER END WALLS (JADE  NOTE  17  SAYS  'SIMILAR  TO
C             COIL', I.E. AS ABOVE, BUT I QUERY THIS).
C
      DATA ZJETE/1250./
      DATA TJETE,GMJETE,ABJETE,RDJETE,DEJETE/80.,17.8,.185,.7,30.E-3/
C
C             TOKYO END-CAP LEAD-GLASS.  (SF5)
C                       DENSITY = 4.08 GM CM**-3  ) ( BASED ON TOKYO
C             ABSORPTION LENGTH = 318.4 MM        ) ( DATA INTERPRETED
C              RADIATION LENGTH = 22.39 MM        ) ( BY J. ALLISON.
C                   (-DE/DX)MIN = 5.708 MEV/CM    ) (
C
      DATA ZEPLG/1510./
      DATA TEPLG,GMEPLG,ABEPLG,RDEPLG,DEEPLG/226.,92.21,.71,10.09,.129/
C
C             (BARREL) LEAD-GLASS.
C             RLG IS INNER RADIUS OF BARREL LG.
C             ZLG IS Z COORDINATE OF END OF BARREL.
C
      DATA RLG,ZLG/1100.,1602.25/
      DATA TLG,GMLG,ABLG,RDLG,DELG/300.,122.4,.942,13.4,.171/
C
C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C
C                 HUGH MCCANN  02/12/83 :
C                 -----------------------
C
C             CHANGES TO BARREL LEAD GLASS AT WINTER '82-'83 SHUTDOWN
C             REQUIRE THE FOLLOWING PARAMETERS ( SEE CODE ) :
C                    FOR THE HALF RINGS AT -X+Z  AND +X-Z  :
      DATA ZLGHAF,TLGHAF/1708.,250./
C                    FOR THE NEW LEAD GLASS RINGS (SF6) IN THE CENTRE
C                    OF THE BARREL :
C                              DENSITY =   5.20   GM/CC ; ( S.YAMADA)
C                    ABSORPTION LENGTH = 271.0   MM     ;
C                    RADIATION  LENGTH =  15.34  MM     ; ( S.YAMADA)
C                    (- DE/DX ) (MIN.) =   8.24  MEV/CM .
C            NOTE :  THE ABOVE NOS. WERE FOUND BY USING THE MUON
C                    MONTE-CARLO ROUTINE 'MUMAT', WHEREAS THE 'OLD'
C                    NOS. WERE COMPUTED BY A DIFFERENT PROCEDURE.
C                    IN PARTICULAR , THE 'NEW' DEDX WAS CALCULATED
C                    FOR  BETA = 0.98 .
      DATA GMLGC,ABLGC,RDLGC,DELGC/156.0,1.108,19.56,0.247/
      DATA ZLGC/319./
C       ABOVE NO. WAS PROVIDED BY T.KOBAYASHI  .
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C
C             MAGNET END-CAP.
C
      DATA ZEP/1755./
      DATA TEP,GMEP,ABEP,RDEP,DEEP/340.,267.,1.99,19.32,.394/
C
C             TEP,ETC., ARE (AVERAGE) FOR END-CAP (MM). (ASSUME IT  HAS
C             UNIFORM THICKNESS, AVERAGED TO SOMETHING LESS  THAN  REAL
C             THICKNESS TO TAKE  INTO  ACCOUNT  LEAD-GLASS  LIGHT-GUIDE
C             HOLES. FURTHER ASSUME DENSITY, ETC. OF IRON.)
C
C             GMG(3),ABG(3),RDG(3),DEG(3) ARE 'GENERAL' QUANTITIES  PER
C             MILLIMETRE FOR 1)CHAMBERS, 2)CONCRETE, 3)IRON.
C
      DATA GMG/0.05,0.536,0.787/
      DATA ABG/0.0005,0.00438,0.0058/
      DATA RDG/0.001,0.0368,0.0568/
      DATA DEG/0.07E-3,0.91E-3,1.16E-3/
C
C             SPECIFY NOMINAL CHAMBER RESOLUTION (IN S.D.
C             REST IS RESOLUTION IN TRANSVERSE (DRIFT) DIRECTION.
C             REST1 IS FOR BARREL FACES , REST2 IS FOR ENDWALLS.
C
C             RESTMX IS RESOLUTION IN TRANSVERSE DIRECTION FOR THE CASE
C             WHEN THE TRANSVERSE (DRIFT) COORDINATE IS  BAD.  THIS  IS
C             SET TO DRIFT DISTANCE =  150  MM,  RATHER  THAN  (CHAMBER
C             WIDTH), SINCE BOTH AMBIGUITIES ARE TRIED. FOR ASSOCIATION
C             OF HITS , IT IS ADDED LINEARLY TO THE SQUARE ROOT OF  THE
C             SUM OF THE SQUARES OF THE VARIANCES DUE TO  MULT  SCAT  *
C             PATREC FIT ERRORS. HOWEVER , FOR CALCULATION OF  CHI**2 ,
C             IT SHOULD BE ADDED QUADRATICALLY WITH THE OTHERS -- BUT ,
C             FOR  PRESENT,  BAD  HITS  ARE   NOT   USED   FOR   CHI**2
C             CALCULATION.
C
C             RESL IS RESOLUTION IN LONGITUDINAL (WIRE) DIRECTION.
C             RESLMX  IS  SIMILAR  TO  RESTMX,  I.E.  FOR   CASE   WHEN
C             LONGITUDINAL COORD. IS BAD. SET TO CHAMBER LENGTH.
C
      DATA REST1,REST2,RESTMX,RESL,RESLMX/ 5.,10.,150.,150.,5000./
C
C             FACTOR IS THE NO. OF STANDARD DEVIATIONS USED FOR HIT CUT
C
      DATA  FACTOR / 2.0 /
C
C             OVLCUT IS THE DIFFERENCE IN  DRIFT  DEVIATIONS  WHICH  IS
C             TOLERATED(MM) FOR HITS TO BE CONSIDERED AS OVERLAP  HITS.
C
      DATA  OVLCUT / 15.0 /
C
C             PMXFLG IS THE MAX MOMENTUM VALUE USED IN TRACKING.
C             NORMALLY IT IS A FLAG SET TO ZERO ==> USE BEAM ENERGY
C
      DATA  PMXFLG / 0.0 /
C
C             NALLWM IS THE NUMBER OF ALLOWED MISSING HITS = 1
C
      DATA  NALLWM / 1 /
C
      END
