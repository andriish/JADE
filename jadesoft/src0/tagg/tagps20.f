C   16/03/88 803161221  MEMBER NAME  TAGPS2   (S)           FORTRAN
C
C
C        THIS IS A VERSION  WITH ALL THE DEBUG REMOVED  TO MAKE
C
C        IT EASIER TO READ !
C
C
C
C
C------  T A G P S 2  ------  T A G P S 2  ------  T A G P S 2  ------
C
C------  T A G P S 2  ------  T A G P S 2  ------  T A G P S 2  ------
C
C------  T A G P S 2  ------  T A G P S 2  ------  T A G P S 2  ------
C
C
         SUBROUTINE TAGPS2(SUM,IWRITE)
C
C TAGPS2 - DOES THE WORK OF TAGPOS FOR 1983 TAGGER
C
C INPUT  :   SUM     -  SUM OF ENERGY IN CLUSTER
C            IWRITE  -  = 1  TURNS ON DEBUG INFO
C
C  MODIFIED : 12/4/85 - A.J.FINCH
C                       MAJOR CHANGES TO R AND FI DETERMINATION CODE.
C                       NOW MUCH SIMPLER - ALSO RESULTS ARE BETTER:
C                       PRODUCES CORRECT DISTRIBUTION IN R AND PHI
C                        FOR BHABHAS
C
C MODIFIED : 16/10/85 - A.J.FINCH
C                       ENERGY CORRECTED FOR POSITION AT SMALL ANGLES
C                       - BASED ON OBSERVED VARIATION IN DATA, AND MC
C
C MODIFIED : 11/9/87  - A.J.FINCH
C                       IMPROVED ENERGY / POSITION CORRECTIONS
C
C MODIFIED : 11/3/88  - A.J.FINCH
C                       IMPROVED ENERGY / POSITION CORRECTIONS
C                        ( DUE TO CHANGES IN JOHN NYE'S CALIBRATION )
C
C
#include "cwktag.for"
C
#include "cdata.for"

C
C
C
       LOGICAL FIRST
       INTEGER ONE
       INTEGER OUTER
       INTEGER ADC
       INTEGER   RTYPE(9)
       INTEGER   FITYPE(9)
       INTEGER  INRIN
C
C
       REAL THMIN(9),THMAX(9),MSLOPE(9),CSLOPE(9)
C
       DIMENSION FUDGE(3)
C
C CONSTANTS USED IN ENERGY/THETA CORRECTIONS
C
       DATA THMIN /0.0,0.031,0.034,0.037,0.0435,0.0485,0.0555,0.066,
     1            0.071/
       DATA THMAX /0.031,0.034,0.037,0.0435,0.0485,0.0555,0.066,0.071,
     1            10.0/
       DATA MSLOPE /0.0,3.854,1.445,16.23,-23.121,9.083,2.753,9.827,0.0/
       DATA CSLOPE /0.82,0.805,0.887,0.340,2.052,0.490,0.841,0.291,0.7/
C-----------------------------------------------------------------------
C
       DATA PIBY2  / 1.570796327 /
       DATA PIBY4  / 0.785398163 /
       DATA PIBY8  / 0.392699081 /
       DATA FUDGE  / 0.68 , 0.80 , 0.95 /
C
C
       DATA R1  /  96.0 / , R2  / 110.0 / , R3  / 140.0 / , R4 / 225.0 /
       DATA R12 / 102.0 / , R23 / 125.0 / , R34 / 210.0 /
C
       DATA RSLOP1 / 0.05 / , RSLP31 / 0.10 / , RCUT1 / 0.10 /
       DATA RSLP32 / 0.06 /
C
       DATA FSLOP1 / 0.11  / , FSLOP2 / 0.04  / , FCUT1 / 0.13 /
       DATA ZTAG / 3160.0 /, RM / 0.024 /
C
       DATA INRIN/27529/
       DATA FIRST/ .TRUE. /
C
C
C ONE IS THE ADC SOFTWARE ADDRESS OF THE HIT BLOCK (TOP OF SORTED
C CLUSTER MAP)
C
C
       ONE    = CLUS(1,1)
       ILOOP  = 1
C
C
C
    1  ONESUM = 0.0
       TWOSUM = 0.0
       THRSUM = 0.0
       TEST2  = SUM * 0.00001
       ILOOP  = ILOOP + 1
       OUTER = 0
C
       IF ( .NOT. FIRST ) GOTO 782
          FIRST = .FALSE.
        RSHIFT = (LOG(RCUT1*2))/(-1.0*RSLP31)
        FSHIFT = (LOG(FCUT1*2))/(-1.0*FSLOP1)
 782   CONTINUE
C
C FIRST FIND THE NUMBER OF BLOCKS,WITH LARGE AMOUNTS
C OF ENERGY IN THEM,AND FOR THESE ,SET UP THE LIST R - TYPE
C AND FI - TYPE
C
C R-TYPE = 1 FOR INNER BLOCKS
C          2     MIDDLE
C          3     OUTER
C
C FI-TYPE = 1 FOR BLOCKS IN THE - FI DIRECTION FROM THE BLOCK
C            WITH THE LARGEST AMOUNT OF ENERGY( = 'ONE')
C         = 2 FOR BLOCKS WITH THE SAME FI AS  ONE
C         = 3 FOR BLOCKS IN THE + FI DIRECTION.
C
C --------------------------------------------------------
C
C <<<<<<<< HERE STARTS THE LOOP>>>>>>>>>>>
C
C
       DO 10 I = 1,9
C
C HAS THIS BLOCK GOT A DECENT AMOUNT OF ENERGY ?
C
          IF ( CLUS(I,2) .LT. TEST2 ) GOTO 11
C
             NBIG      = I
C
C ADC IS ADC'S SOFTWARE ADDRESS
C
             ADC       = CLUS(I,1)
C
C SET UP RTYPE AND FITYPE LISTS
C
             RTYPE(I)  = TAGRTY(ADC)
             FITYPE(I) = TAGFTY(ADC,ONE)
C
C ------------------------------------------------------------
C NOW IT IS NECCESSARY TO CORRECT THE ENERGIES IN THE CLUSTER
C SO THAT THEY REPRESENT THE ACTUAL ENERGY DEPOSITED,RATHER
C THAN THE ENERGY SUCH THAT ALL CLUSTERS (SUMMED OVER MANY
C BLOCKS ) HAVE THE CORRECT ENERGY.THESE ARE DIFFERENT
C FOR INNER AND MIDDLE BLOCKS DUE TO EDGE EFFECTS,I.E.
C ALL SHOWERS IN THESE BLOCKS LOSE SOME PORTION INTO
C INSIDE EDGE OF DETECTOR.
C
C--------------------------------------------------------------
C
             IF ( ILOOP .EQ. 1)CLUS(I,2) = CLUS(I,2) * FUDGE(RTYPE(I))
C
C -------------------------------------------------------------
C WHILE WE ARE AT IT CALCULATE SUM OF ENERGIES IN RESPECTIVELY
C ALL BLOCKS OF FITYPE 1,DITTO 2,AND 3,USED IN RATIO
C CALCULATIONS LATER
C
C  -------------------------------------------------------------
C
             IF ( FITYPE(I) .EQ. 1 ) ONESUM = ONESUM + CLUS(I,2 )
             IF ( FITYPE(I) .EQ. 2 ) TWOSUM = TWOSUM + CLUS(I,2 )
             IF ( FITYPE(I) .EQ. 3 ) THRSUM = THRSUM + CLUS(I,2 )
C
C ---------------------------------------------------------------
C
C ALSO FIND THE POINTER TO THE BLOCK IN CLUS THAT IS
C THE OUTERMOST ONE IN THE HIT CAKE SLICE (SEGMENT).
C USED TO CHOOSE WHICH RATIO TO CALCULATE
C
C ( THE OUTER BLOCK IS BIGGEST SO IS MOST IMPORTANT )
C
C ----------------------------------------------------------------
C
             IF ( (FITYPE(I).EQ.2) .AND. (RTYPE(I).EQ.3) ) OUTER  = I
   10  CONTINUE
C
C >>>>> HERE ENDETH THE LOOP OVER BLOCKS FOR INITIAL SETTING UP <<<<<
C
   11  CONTINUE

C --------------------------------------------------------------------
C
C
C NOW DO THE CALCULATION OF R ( DISTANCE OF SHOWER CENTRE FROM BEAM LINE
C ) - THE CALCULATION WORKS AS FOLLOWS:
C
C FIND THE RATIO BETWEEN THE ENERGY THAT THE SHOWER DEPOSITS IN
C ONE PART OF THE DETECTOR, AND THE TOTAL ENERGY DEP[OSITED.
C
C E.G. IF THE LARGEST AMOUNT OF ENERGY IS IN A MIDDLE BLOCK,
C   AND THE NEXT LARGEST AMOUNT OF ENERGY IS IN THE BLOCK OUTSIDE
C THIS BLOCK , THEN WORK OUT THE RATIO OF THE ENERGY IN THIS OUTER
C BLOCK TO THGE TOTAL ENERGY IN ALL THREE BLOCKS IN A CAKESLICE
C - THIS RATIO THEN GIVES YOU RCORR - THE DISTANCE FROM
C THE BOUNDARY BETWEEN THESE TWO, AND THE CENTRE OF THE SHOWER.
C RCORR IS OBTAINED BY ASSUMING
C
C   RATIO = 0.5 EXP (-RSLOPE.RCORR) ; RSLOPE HAS BEEN ADJUSTED
C                                TO OPTIMISE THE  PROCEDURE
C
C  FOR LARGE BLOCKS ONLY - IF RATIO < RCUT1
C                             SLOPE CHANGES TO RSLP32
C                           OTHERWISE SLOPE = RSLP31
C
C WHICH RATIO TO USE ?:
C
C ------------------------------------------------------------------
C BLOCK WITH MOST                      RATIO:
C       ENERGY:
C
C ------------------------------------------------------------------
C
C INNER RING BLOCKS (RTYPE = 1)      ENERGY IN  OUTER
C                                     RINGS  DIVIDED BY TOTAL
C
C MIDDLE RING BLOCKS (RTYPE = 2)      ENERGY IN OUTER
C                                     RING DIVIDED BY TOTAL
C
C OUTER RING BLOCKS (RTYPE = 3)      ENERGY IN MIDDLE AND INNER
C                                     RINGS COMBINED DIVIDED BY TOTAL
C
C --------------------------------------------------------------------

       IF ( RTYPE(1).EQ.3) RATIO = (TWOSUM - CLUS (1,2))/TWOSUM
       IF(OUTER.EQ.0)GOTO 23
       IF ( RTYPE(1).NE.3) RATIO = CLUS(OUTER,2)/TWOSUM
       IF ( RATIO .GE. 0.0005) GOTO 25
C
C IF RATIO IS VERY SMALL - MUST USE 'CENTRE' OF BLOCK AS R POSITION
C
  23      IF ( RTYPE(1).EQ. 1 ) R = R12
          IF ( RTYPE(1).EQ. 2 ) R = R23
          IF ( RTYPE(1).EQ. 3 ) R = R34
C
C   THEREFORE END OF R CALCULATION
C
          GOTO 41
C
   25  CONTINUE
C
C
C NOW CALCULATE RCORR FROM RATIO
C
C
C
C
      IF ( RATIO .LE. 0.0 ) GOTO 32
C
        IF(RTYPE(1).EQ.3) GOTO 320
C
          RCORR =  (LOG(2.0 * RATIO ))/( -1.0 * RSLOP1 )
          GOTO 32
C
320     IF(RATIO.LE.RCUT1) GOTO 321
C
          RCORR =  (LOG(2.0 * RATIO ))/( -1.0 * RSLP31 )
          GOTO 32
C
321     RCORR =  ((LOG(RATIO/RCUT1))/( -1.0 * RSLP32))  + RSHIFT
C
 32    CONTINUE
C
C  RCORR CALCULATION COMPLETE
C
C ADD RCORR TO THE RELEVANT VALUE OF R ,THAT IS THE BOUNDARY
C WHICH DIVIDED RATIO (  = ENERGY OUTSIDE BOUNDARY/TOTAL ENERGY)
C
C
       IF ( RTYPE(1) .NE. 3 ) R = R3 - RCORR
       IF ( RTYPE(1) .EQ. 3 ) R = R3 + RCORR
C
C
C LIMIT CORRECTION TO NOT TAKING CENTRE OF CLUSTER OUT OF COUNTER
C TO PROTECT AGAINST OVER ZEALOUS CORRECTION DUE TO POOR CALIBRATION
C DEAD CHANELS ETC.
C
C
       IF ( R .LT. R1 ) R = R1
        IF ( R .GT. R34 ) R = R34
C
C END OF R CALCULATION
C
41     CONTINUE
C
C----------------------------------------------------------------------
C
C - DEBUG SECTION
C
       IF ( IWRITE .EQ. 1 ) WRITE(6,664)RATLOS,ECLS1,RTYPE(1)
       IF ( IWRITE .EQ. 1 ) WRITE(6,665)CLUS(INNER,2),CLUS(MIDDLE,2),
     *     CLUS(OUTER,2),TWOSUM,RATIO,RCORR,R
 665   FORMAT(' ++TAGPS2: E1,E2,E3,ET,RAT,R,RC=',4(F8.2,2X),/,
     *        ' ++TAGPS2: RAT,RCORR,R         =',3(F10.4,2X))
 664   FORMAT(' ++TAGPS2: RATLOS,ECLS1,RTYP1  =',3(F8.2,2X))
C
C
C -------------------------------------------------------
C
C  HAVING DETERMINED R WORK OUT FI
C
C -------------------------------------------------------
C
C
C  THE SAME IDEA IS USED HERE AS FOR THE LARGE BLOCKS R CALCULATION
C  EXCEPT THE RATIO IS CALCULATED USING SUMS OF TWO CAKESLICES:
C THE ONE WHICH CONTAINS THE BLOCK WITH THE HICHEST ENERGY ,
C AND IT'S NEIGHBOURING CAKESLICE WITH THE MOST ENERGY
C
C
C
C
       FICORR = 0
C
C
C IF ONLY ONE  HIT BLOCK NO FI CORRECTION CALCULATED
C
C
       IF ( NBIG .EQ. 1 ) GOTO 43
C
C
C     FISIGN = 0 = > NO ENERGY IN NEIGHBOURING BLOCKS
C     FISIGN = -1/+1 = > MOST ENERGY IN NEIGHBOURS IN -FI/+FI DIRECTION
C
C
       FISIGN = 0
C
C
C
       RATIO  = ONESUM / SUM
       FISIGN = - 1
   34  IF ( ONESUM .GT. THRSUM) GOTO 36
          RATIO  = THRSUM / SUM
          FISIGN = + 1
   36  CONTINUE
       IF ( RATIO .EQ. 0 ) FISIGN = 0
C
C
C IF FISIGN IS STILL 0 ONESUM AND THRSUM ARE 0 SO NO RATIO
C CALCULATED (FICOR = 0)
C
C
      IF ( FISIGN .EQ. 0 ) GOTO 43
C
C
C RCORR IS DISTANCE IN  MM FROM SHOWER TO BOUNDARY BETWEEN
C HIT OCTANT AND NEIGHBOURING OCTANT,DETERMINED FROM SHARING
C IF RATIO IS LARGE,MOST OF SHOWER IS IN HIT OCTANT,SO RCORR IS
C LARGE,RCORR IS DISTANCE OF CENTRE OF SHOWER FROM BOUNDARY
C
C
      FICORR = 0.0
      IF ( RATIO .LT. 0.0001 ) GOTO 37
C
        IF(RATIO.LE.FCUT1) GOTO 421
C
          RCORR =  (LOG(2.0 * RATIO ))/( -1.0 * FSLOP1 )
          GOTO 42
C
421     RCORR =  ((LOG(RATIO/FCUT1))/( -1.0 * FSLOP2))  + FSHIFT
C
 42    CONTINUE
C
C  RCORR CALCULATION COMPLETE
         IF((RCORR/R).GT.1.0)RCORR = R
         FICORR = FISIGN*(PIBY8-ASIN(RCORR/R))
         IF(ABS(FICORR).GT.PIBY8)FICORR = FISIGN*PIBY8
C

C
  37  CONTINUE
C
C
C     CORRECT R FOR FI POSITION
C
C
      R = R / ( COS( FICORR ) )
      IF(R.GT.R4) R = R4
C
C
C ACTUAL VALUE OF FI (TFI) IS FI OF 'HIT' BLOCK ('ONE') + FI CORRECTION
C
C
  43   TFI = TAGPHI(ONE) + FICORR
C
C
C CAND(2) = X = R COS(FI)   CAND(3) = Y = R SIN(FI)
C
C
      CAND(2) = R * (COS(TFI) )
      CAND(3) = R * (SIN(TFI) )
C
C
C CAND(1) = ENERGY OF CLUSTER
C
C
       CAND(1) = SUM
C
C
C  ADJUST ENERGY FOR EMPIRICALLY OBSERVED DEPENDENCE ON POSTITION
C        AT CERTAIN ANGLES - VERSION OF 5/5/87
C                            + UPDATES 11/3/88 FOR NEW CALIBRATION
C
C *** NOT DONE FOR MC DATA - AS MC DOES NOT SIMULATE THIS !!***
        IF(IMC.EQ.1) GOTO 200
C
C - FIRST CONVERT R TO RAD
C
        THETA  = ( R / ZTAG  )
C
C CALCULATE RATIO
C
C RATIO .NE. 1.0 => OVERALL SCALING
C TAKES MODAL VALUE OF BHABHA PEAK TO CORRECT E BEAM
C FOR 17.5 GEV EBEAM
C
        RATIO = 1.0286
        ISTART = 1
        IRUN = 0
C
C CHANGE 1 -> 4 AS FIRST INDEX,IF RUN NO.>
C                     WHEN INNER RING INSTALLED
C                     OR < 24600
C                                       CHANGE MADE-9/3/88
C
      IND = IDATA(IBLN('HEAD'))
      IF (IND.GT.0) IRUN = HDATA(2*IND+10)
      IF(IRUN.LT.24600)ISTART = 4
      IF(IRUN.GT.INRIN)ISTART = 4
      DO 145 I = ISTART,9
      IF(THETA.LE.THMIN(I).OR.THETA.GT.THMAX(I))GOTO 145
            RATIO = RATIO*( MSLOPE(I)*THETA + CSLOPE(I))
            GOTO 146
145   CONTINUE
146   CONTINUE
C
C NOW PHI DEPENDANCE ( SEEMS FAIRLY INDEPENDENT OF THETA, AND
C  RUN PERIOD )
C
C
       FIPRIM = AMOD(TFI+PIBY2,PIBY4)
       IF(FIPRIM.GT.PIBY8)FIPRIM = PIBY4 - FIPRIM
C
       IF(FIPRIM.LT.0.01786) GOTO 1786
       IF(FIPRIM.LT.0.03573.AND.FIPRIM.GE.0.01786)GOTO 3573
       IF(FIPRIM.LT.0.05359.AND.FIPRIM.GE.0.03573)GOTO 5359
       IF(FIPRIM.GT.0.37514)GOTO 37514
C
C LARGE FI - GEOMETRIC OPTICS ? OR ABSORBTION IN SCINTILLATOR?
C
         RATIO = RATIO * (-0.135*FIPRIM+ 1.022)
         GOTO 6252
C
C SMALL FI - BBQ EFFECT ?
C
1786    RATIO = RATIO *   0.881
        GOTO 6252
3573    RATIO = RATIO *   0.850
        GOTO 6252
5359    RATIO = RATIO *   0.994
        GOTO 6252
C
C EXACT MIDDLE OF BLOCKS ??? NON-UNDERSTOOD EFFECT!
C
37514   RATIO = RATIO *   0.92
C
6252   CONTINUE
C
C AND ADJUST ENERGY
C
        CAND(1) = CAND(1) / RATIO
C
C AND FINALLY - ERROR ON ENERGY = SIGEN ASSUME 10 PER CENT AT
C                              17 GEV
C
C
 200      SIGEN = 0.40 * SQRT(CAND(1) )
C
       IF ( IWRITE .EQ. 1 ) WRITE(6,666)R,TFI,CAND(1),SIGEN
 666   FORMAT(' ++TAGPS2: R,FI,E,SIGEN = ',4(F10.2,2X) )
C
C
C
C
 1000  RETURN
       END
