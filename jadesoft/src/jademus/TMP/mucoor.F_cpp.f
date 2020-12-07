C   23/02/84 403091202  MEMBER NAME  MUCOOR   (S)           FORTRAN
C
C-----------------------------------------------------------------------
      SUBROUTINE MUCOOR(HMU,NHITS,HC,NHITMX,LMU,HLUN,NWHIT)
C-----------------------------------------------------------------------
C
C
C     MUCOOR CONVERTS THE RAW DATA INTO CARTESIAN COORDINATES
C
C=======================================================================
C DON'T FORGET TO CHANGE MUANAC TO STORE DATE OF THIS VERSION IN BANK 0.
C ALSO CHANGE THE DATE IN THE BANNER PRINTOUT IN THIS ROUTINE.
C=======================================================================
C
C LAST CHANGE 11.40   9/03/84   C. BOWDERY  - TIDYING UP
C      CHANGE 16.00  23/02/84   C. BOWDERY  - NEW DOUBLE PULSE CUT FUNC.
C      CHANGE 15.00   1/02/84   T. GREENSHAW- NMU DESCRIPTION-->@MUSTAT
C      CHANGE 12.00   5/01/84   C. BOWDERY  - MARK MULTI-PULSED HITS
C      CHANGE 12.00  25/10/83   H. MCCANN   - BUGS REMOVED.
C      CHANGE 15.45  30/03/83   H. MCCANN   - FOR MONTE-CARLO.
C      CHANGE 10.30  16/03/83   H. MCCANN   - NEW CALIBRATION/METHOD.
C                               'OLD' CODE IS MARKED 'COBSOLETE' AND NEW
C                               CODE IS EXTENSIVELY COMMENTED.
C      CHANGE 12.30  31/03/82   H. MCCANN  - MODIFY BAD CHAMBER BODGE.
C      CHANGE 13.30  22/02/82   H. MCCANN  - FOR IMPROVED ERROR OUTPUT
C      CHANGE 14.30  04/02/82   H. MCCANN  - BODGE FOR BAD CHAMBERS
C      CHANGE 12.30  19/10/81   H. MCCANN  - CORRECT CRATE6 BAD GAS BUG
C      CHANGE 14.58  04/09/81   C. BOWDERY - CORRECT HANG BUG
C      CHANGE 07.30  10/04/81   H. MCCANN  - JADEMUS UPDATE.
C      CHANGE 10.19  26/03/81   J. ALLISON - TO ADD ABOVE COMMENT
C                                           ABOUT MUANAC, DATE * BANK 0.
C      CHANGE 14.45  24/03/81   H. MCCANN  - ALLOW FOR CRATE 6 DURING
C                                              THE BAD GAS PERIOD.
C      CHANGE 15.00  19/02/81   H. MCCANN  - MORE TIDYING * DEBUGGING
C MAJOR TIDYING UP   10/09/80   I. DUERDOTH
C
C
C-----------------------------------------------------------------------
C          ----------------
C          WHAT MUCOOR DOES
C          ----------------
C
C     CONVERTS MU-CHAMBER SIGNALS TO COORDINATES  USING  FULL  SET  OF
C     CALIBRATION DATA. IF THE COORDINATE IS BAD THEN IT IS SET TO THE
C     CENTRE OF THE CHAMBER,AND HLUN IS UPDATED APPROPRIATELY
C
C     IEV IS THE EVENT NO.
C     NEV IS NO. OF EVENTS PROCESSED SO FAR
C     KRUN IS THE RUN NO.
C     HSECT GIVES THE CRATE FOR EACH CHAMBER
C
C
C     30/03/83 : MONTE-CARLO DATA IS NOW CONSIDERED
C     TO BE DIFFERENT FROM REAL DATA IN THAT THE M-C HITS
C     ARE CREATED WITHOUT NON-LINEAR , ANGULAR * SHIFT EFFECTS.
C
C     OTHERWISE THIS VERSION OF MUCOOR TREATS ALL
C     INPUT MUEV BANKS IDENTICALLY, REGARDLESS OF WHETHER
C     THE INPUT IS MONTE-CARLO , UNDER THE ASSUMPTION THAT
C     M.C. MUEV BANKS ARE IDENTICAL TO REAL DATA.
C
C-----------------------------------------------------------------------
C
C                       INPUT VARIABLES
C
C HMU         THE RAW MU DATA.
C NHITMX      THE MAXIMUM NO OF HITS THAT CAN BE STORED IN OUTPUT
C               ARRAY HC.  THE DIMENSION OF HC IN THE CALLING ROUTINE
C               MUST BE AT LEAST NWHIT*NHITMX.
C LMU         THE LENGTH OF THE RAW DATA BANK.
C NWHIT       THE NUMBER OF WORDS PER HIT IN THE OUTPUT ARRAY HC
C
C-----------------------------------------------------------------------
C                       OUTPUT VARIABLES
C
C NHITS       THE NUMBER OF HITS,  OUTPUT TO THE ARRAY HC.
C HC          THE OUTPUT ARRAY, EACH HIT IS NWHIT WORDS.
C               WORD  1     -  4*CHAMBER NUMBER + (HIT NUMBER -1).
C               WORD  2     -  10*LAYER NUMBER + ORIENTATION PARAMATER.
C               WORDS 3-8   -  X,Y,Z(LEFT),X,Y,Z(RIGHT) (MM).
C
C                              AS OF 16/03/83, THE FOLLOWING APPLIES TO
C                              THE DRIFT COORDINATE ( X OR Y, DEPENDING
C                              ON CHAMBER ORIENTATION) :
C                              (A) 'SOFTWARE SHIFTS' OF WIRE POSITION
C                                  ARE IMPLEMENTED ;
C                              (B) CHAMBER TWIST CORRECTIONS ARE NOT
C                                  IMPLEMENTED, BUT ARE NOW DONE USING
C                                  TRACK "Z" 'S IN MUFFLY.
C
C               WORD  9     -  POINTER TO RAW DATA
C
C HLUN          STATUS BITS FOR EACH HIT IN HC
C
C     .------------.-------------------------------------------------.
C     | BIT NUMBER | 15 14 13 12 11 10  9  8  7  6  5  4  3  2  1  0 |
C     |------------+------------------------+------------------------|
C     | MNEMONIC   |  0  0  0  0  0  0  M  A  B  C  D  X  Y  Z  T  L |
C     '------------'-------------------------------------------------'
C
C ---->   THIS BIT NUMBERING RUNS OPPOSITE TO IBM CONVENTION
C         I.E. MACHINE SEES 'T' FLAG IN BIT NO. 14.
C         THE USAGE OF THE BITS IS AS FOLLOWS:
C
C      BIT) (TRUTH VALUE)
C HLUN  0  = 0(1)   IF LONGITUDINAL COORDINATE IS INVALID(VALID)
C
C HLUN  1  = 0(1)   IF TRANSVERSE   COORDINATE IS INVALID(VALID)
C                   I.E. MOD(HLUN,4)= 0 ---> BOTH  COORDS. BAD
C                        = 1 ---> LONG. COORD OK ( TRANS. COORD. BAD)
C                        = 2 ---> TRANS COORD OK ( LONG.  COORD. BAD)
C                        = 3 ---> ALL OK
C
C HLUN  2  = 0(1)   IF Z  COORDINATE IS INVALID(VALID)
C
C HLUN  3  = 0(1)   IF Y  COORDINATE IS INVALID(VALID)
C
C HLUN  4  = 0(1)   IF X  COORDINATE IS INVALID(VALID)
C
C HLUN  5  = 0(1)   IF Z  COORDINATE IS NORMAL(DRIFT OR LONG)
C
C HLUN  6  = 0(1)   IF Y  COORDINATE IS NORMAL(DRIFT OR LONG)
C
C HLUN  7  = 0(1)   IF X  COORDINATE IS NORMAL(DRIFT OR LONG)
C
C HLUN  8  = 0(1)   IF HIT1 LOST BY HIT4 * RECOVERED FROM HIT2(ALL OK)
C
C HLUN  9  = 0(1)   IF HIT DID NOT MULTI-PULSE (IF HIT DID MULTI-PULSE)
C
C NOTE:  THE 1'ST SET BIT(OF 5,6,7) IS ALWAYS THAT FOR  LONG. COORD.
C
C-----------------------------------------------------------------------
C
      IMPLICIT INTEGER*2 (H)
C
      LOGICAL FIRST / .TRUE. /, ENOUGH
      LOGICAL PFG   / .TRUE. /
      LOGICAL TIMREF,  GOOD
      LOGICAL ADJCHM, SAMCHM, HIT12
C
C                            COMMONS
C
C
C
C
C
C
C=======================<< MACRO CMUCALIB >>============================
C
C LAST CHANGE  25/09/79  13.20 UHR   HARRISON PROSPER
C
C BANK NAMES, NUMBERS AND LENGTHS
C
C  NAME/NUMBER LENGTH  CONTENTS
C  MUCD   0      16    VERSION NUMBER AND DESCRIPTION.
C  MUOV   0       3    OVERALL JADE UNIT TRANSLATIONS.
C  MFFI   2     370    FIXED FRAME PARAMETERS.
C  MCFI   3     318    FIXED CHAMBER PARAMETERS.
C  MFSU   4     246    'SURVEY' FRAME PARAMETERS.
C  MCSU   5     634    'SURVEY' CHAMBER PARAMETERS.
C  MCEL   6    2220    'ELECTRONIC' CHAMBER PARAMETERS.
C  MCST   7     317    CHAMBER STATUS WORDS.
C  MUFI   8      36    FILTER (ABSORBER BLOCK) PARAMETERS.
C  MUYO   9      10    SIDE, TOP AND BOTTOM YOKE PARAMETERS.
C  MUEN  10      15    YOKE END-PLUG PARAMETERS.
C
C TOTAL LENGTH 4185 WORDS.
C
C-----------------------------------------------------------------------
C-----------------------------------------------------------------------
      COMMON /CALIBR/ LARRY(100),MUCAL(4185)
C-----------------------------------------------------------------------
C-----------------------------------------------------------------------
C
C
C               NVERSN
      DIMENSION DESCRP(15),HOVALL(6)
C                                                    19 WORDS
C
      EQUIVALENCE ( NVERSN,MUCAL(1) ),( DESCRP(1),MUCAL(2) ),
     *            ( HOVALL(1),MUCAL(17) )
C----------------------------------------------------19 WORDS SO FAR
C
C     HMFFIX(740)                                   370 WORDS
      DIMENSION HMFFIX(740)
      EQUIVALENCE ( HMFFIX(1),MUCAL(20) )
      DIMENSION HFACE(82),HSECT(82),HLAYER(82),HNORM(82),HLONG(82),
     *          HTRANS(82),HAC(82),HAL(82),HUNIT(82)
      EQUIVALENCE (HMFFIX(1),NFRAMS),(HMFFIX(3),HFACE(1)),
     *            (HMFFIX(85),HSECT(1)),(HMFFIX(167),HLAYER(1)),
     *            (HMFFIX(249),HNORM(1)),(HMFFIX(331),HLONG(1)),
     *            (HMFFIX(413),HTRANS(1)),(HMFFIX(495),HAC(1)),
     *            (HMFFIX(577),HAL(1)),(HMFFIX(659),HUNIT(1))
C---------------------------------------------------389 WORDS SO FAR
C
C
C     HMCFIX(636)                                   318 WORDS
      DIMENSION HMCFIX(636)
      EQUIVALENCE ( HMCFIX(1),MUCAL(390) )
      DIMENSION HFR(634)
      EQUIVALENCE (HMCFIX(1),NCHAMS),(HMCFIX(3),HFR(1))
C---------------------------------------------------707 WORDS SO FAR
C
C     HMFSUR(492)                                   246 WORDS
      DIMENSION HMFSUR(492)
      EQUIVALENCE ( HMFSUR(1),MUCAL(708) )
      DIMENSION HDIST(82),HANG(82),HCLLO(82),HCLHI(82),HCTLO(82),
     *          HCTHI(82)
      EQUIVALENCE (HMFSUR(1),HDIST(1)),(HMFSUR(83),HANG(1)),
     *            (HMFSUR(165),HCLLO(1)),(HMFSUR(247),HCLHI(1)),
     *            (HMFSUR(329),HCTLO(1)),(HMFSUR(411),HCTHI(1))
C---------------------------------------------------953 WORDS SO FAR
C
C
C     HMCSUR(1268)                                  634 WORDS
      DIMENSION HMCSUR(1268)
      EQUIVALENCE ( HMCSUR(1),MUCAL(954) )
      DIMENSION HD1(634),HCTW(634)
      EQUIVALENCE (HMCSUR(1),HCTW(1)),(HMCSUR(635),HD1(1))
C--------------------------------------------------1587 WORDS SO FAR
C
C
C     HMCELE(4440)                                 2220 WORDS
      DIMENSION HMCELE(4440)
      EQUIVALENCE ( HMCELE(1),MUCAL(1588) )
      DIMENSION HDTP(634),HLTP(634),HLSF(4,634),HVDRFT(634)
      EQUIVALENCE (HMCELE(1),HVDR),(HMCELE(2),HDTP(1)),
     *            (HMCELE(636),HLTP(1)),(HMCELE(1270),HLSF(1,1)),
     *            (HMCELE(3806),HMCEDM),(HMCELE(3807),HVDRFT(1))
C--------------------------------------------------3807 WORDS SO FAR
C
C
C     HMCSTA(634)                                   317 WORDS
      DIMENSION HMCSTA(634)
      EQUIVALENCE ( HMCSTA(1),MUCAL(3808) )
C--------------------------------------------------4124 WORDS SO FAR
C
C
C     HFILDA(72)                                     36 WORDS
      DIMENSION HFILDA(72)
      EQUIVALENCE ( HFILDA(1),MUCAL(4125) )
      INTEGER*2 HBLLO(6),HBLHI(6),HBTLO(6),HBTHI(6),HBNLIM(36)
      INTEGER*4 IFCIND(6)
      INTEGER*2 HFILDA
      EQUIVALENCE (HBLLO(1),HFILDA(1)),(HBLHI(1),HFILDA(7)),
     *            (HBTLO(1),HFILDA(13)),(HBTHI(1),HFILDA(19)),
     *            (HBNLIM(1),HFILDA(25)),(IFCIND(1),HFILDA(61))
C--------------------------------------------------4160 WORDS SO FAR
C
C
C     HYKNMI(4),HYKNMO(4),HYKLDM(4),HYKTDM(4),BYOKE, 10 WORDS
C     IYKIND
      DIMENSION HYKNMI(4),HYKNMO(4),HYKLDM(4),HYKTDM(4)
      INTEGER*2 HYKTDM,HYKLDM,HYKNMI,HYKNMO
      EQUIVALENCE ( HYKNMI(1),MUCAL(4161) ),
     *            ( HYKNMO(1),MUCAL(4163) ),
     *            ( HYKLDM(1),MUCAL(4165) ),
     *            ( HYKTDM(1),MUCAL(4167) ),
     *            ( BYOKE,MUCAL(4169) ),( IYKIND,MUCAL(4170) )
C--------------------------------------------------4170 WORDS SO FAR
C
C
C    IZEII,IZEIO,IREP1,IREP2,IREP3,IREP4,IXYEP5,     15 WORDS
C    IZOEP1,IZOEP2,IZOEP3,IZOEP4,IZOEP5,CAEP2,
C    IEPIND,IEPSCT
C
      EQUIVALENCE ( IZEII,MUCAL(4171) ),( IZEIO,MUCAL(4172) ),
     *            ( IREP1,MUCAL(4173) ),( IREP2,MUCAL(4174) ),
     *            ( IREP3,MUCAL(4175) ),( IREP4,MUCAL(4176) ),
     *            ( IXYEP5,MUCAL(4177) ),( IZOEP1,MUCAL(4178) ),
     *            ( IZOEP2,MUCAL(4179) ),( IZOEP3,MUCAL(4180) ),
     *            ( IZOEP4,MUCAL(4181) ),( IZOEP5,MUCAL(4182) ),
     *            ( CAEP2,MUCAL(4183) ),( IEPIND,MUCAL(4184) ),
     *            ( IEPSCT,MUCAL(4185) )
C--------------------------------------------------4185 WORDS SO FAR
C
C=======================<< MACRO CMUCALIB >>============================
C
C
C
C
C
C----------- START OF MACRO CMUSTAT ------------------------------------
C   LAST CHANGE 12.47 15/06/79 JOHN ALLISON.
C      /CMUSTT/
C
      COMMON / CMUSTT / MUTIT(100),NMU(100)
      REAL*8 MUTIT, MUT1(50), MUT2(50)
      EQUIVALENCE(MUTIT(1),MUT1(1)),(MUTIT(51),MUT2(1))
C
C  NMU ARE USED FOR STATISTICS COUNTING IN THE MUON ROUTINES
C
C------------ END OF MACRO CMUSTAT -------------------------------------
C----------START OF MACRO CMUBCS----------------------------------------
      COMMON /BCS/IDATA(1)
      DIMENSION HDATA(1),ADATA(1)
      EQUIVALENCE (HDATA(1),ADATA(1),IDATA(1))
C----------END OF MACRO CMUBCS------------------------------------------
C
      COMMON / CMUIEV / IEV,NEV,IRD,KRUN,KREC,
     +                  ISECS,IMINS,IHOURS,IDAY,IMONTH,IYEAR
      COMMON / CMUPRN / IPRIN,MUHPR
C     COMMON / CMUDAT / HMUX(634),HMUY(634),HMUZ(634),
C    +                  HLEN(634),HLST(634)
C
      DIMENSION HMU(1),HC(1),HLUN(1)
      DIMENSION IDRIFT(4)
C
C                            LOOK-UP TABLE FOR NON-LINEAR PARTS OF
C                            DRIFT-TIME SPECTRUM
C
      REAL XDT(150)/
     +
     +     0.6,0.6,0.6,0.6,1.5,3.2,5.0,6.35,7.6,8.85,54*0.,
     +     63.75,64.5,65.25,66.0,66.75,67.5,
     +     68.25,69.0,69.5,69.75,70.0,70.5,70.75,71.0,71.0,71.25,
     +     71.25,71.5,71.75,71.75,71.75,72.0,72.25,72.25,72.5,72.5,
     +     72.5,72.75,72.75,73.0,73.0,73.0,73.25,73.25,73.25,73.25,
     +     7*73.5,10*73.75,33*74.0     /
C
      DATA KMESS / 0 /
      DATA HB2 /4/, HB3 /8/, HB4 /16/, HB5 /32/, HB6 /64/, HB7 /128/
C
C                            IMTDLO AND IMTDHI  ARE  RESPECTIVELY  THE
C                            SMALLEST AND LARGEST ACCEPTABLE VALUES OF
C                            THE DRIFT COORDINATE IN MM.
C                  ------>   NEW UNDERSTANDING OF DRIFT  TIME-DISTANCE
C                            RELATIONSHIP MEANS THAT THESE ARE
C                            EFFECTIVELY OBSOLETE. 16/03/83.
C
      DATA IMTDLO / -5 /, IMTDHI / 160 /
C
C                            MAXMES IS THE  MAXIMUM  NUMBER  OF  ERROR
C                            MESSAGES TO BE  PRINTED/JOB  THE  NO.  OF
C                            ERROR MESSAGES IN THE JOB IS  COUNTED  BY
C                            NMESS.
C
      DATA MAXMES / 100 /
C
C                            MAXMUM IS THE MAXIMUM NUMBER  OF  SINGLES
C                            MESSAGES TO BE PRINTED/JOB
C
      DATA MAXMUM / 100 /
C
C                            ICONST IS THE  CONSTANT  SUBTRACTED  FROM
C                            THE DRIFT TIME OF HIT2  TO  ESTIMATE  THE
C                            DRIFT TIME OF HIT1 IN THE EVENT THAT  THE
C                            DRIFT INFORMATION OF HIT1 HAS BEEN LOST
C
      DATA ICONST / 7 /
C
C                            MAXREC IS THE MAXIMUM NUMBER OF  ATTEMPTS
C                            AT DATA RECOVERY
C
      DATA MAXREC / 10 /
C
C                            IZEXTN IS ALLOWED EXTENTION PAST THE ENDS
C                            OF THE CHAMBER IN WHICH WE PULL THE HIT
C                            BACK TO THE PHYSICAL END OF THE CHAMBER
C
      DATA IZEXTN / 500 /
C
C------------------  C O D E  ------------------------------------------
C
C                            IF THIS IS A MUEV BANK  OUTPUT  FROM  THE
C                            OLD M.C. , NO PROCESSING IS ATTEMPTED.
C
      IF( HMU(1) .EQ. 0 ) RETURN
C
C                            COUNT NUMBER OF CALLS TO MUCOOR
C
      NMU(1) = NMU(1) + 1
C
C                            OPTIONAL OUTPUT
C
      PFG = IPRIN .NE. 0
      IF(FIRST) GO TO 861
      GO TO 862
C
C                            ENTER HERE ON FIRST CALL.
C
  861 FIRST = .FALSE.
      NMESS = 0
      IF( .NOT. PFG ) GO TO 862
C
C                            PRINT MUCOOR BANNER
C
      WRITE(6,865)
  865 FORMAT(//' --->  MUON SIGNAL-TO-COORDINATE CONVERSION BY MUCOOR:',
     +         '   VERSION  9/03/84')
C
C                            PRINT MEANING OF MUCOOR ERROR MESSAGES.
C
      WRITE(6,888)
  888 FORMAT(//6X,'THE MEANING OF THE MUON MESSAGE CODES FROM MUCOOR'/
     *         6X,'================================================='//
     *'     1.....CRATE NUMBER OUTSIDE RANGE 1 TO 14'/
     *'     2.....CRATE NUMBER OUT OF SEQUENCE'/
     *'     3.....TIME REFERENCE WORD MISSING'/
     *'     4.....CHAMBER NUMBER OUTSIDE RANGE 1 TO 634'/
     *'     5.....NEW CHAMBER FOUND BUT PREVIOUS CHAMBER NOT FINISHED'/
     *'     6.....CHAMBER NUMBER OUT OF SEQUENCE'/
     *'     7.....HIT NUMBER NOT DECREASED BY ONE'/
     *'     8.....END-OF-CRATE FOUND INSTEAD OF A NEW CRATE MARKER'/
     *'     9.....SINGLES COUNT < 0  OR  COUNT TIME < 0'/
     *'    10.....OUTPUT ARRAY FULL. EXCESS HITS IN THE EVENT ARE LOST'/
     *'    11.....LENGTH ERROR IN HMU ARRAY'/
     *'    12.....LAST WORD IN HMU ARRAY IS NOT 0 OR 3840'/
     *'    13.....SINGLES DATA AFTER CHAMBER WORD WITH HIT NOT 1'/
     *'    14.....UNUSED'/
     *'    15.....UNUSED'/
     *'    16.....CHAMBER IN WRONG CRATE'/
     *'  70+N.....CRATE N MISSING'//)
      WRITE(6,863)
  863 FORMAT(' ****      SOME OF THESE MESSAGES ARE TO BE',
     +       ' EXPECTED FOR REAL DATA, ESPECIALLY "CRATE MISSING".'/
     +       ' ****      MOST OF THE OTHERS ARE NOT SO SERIOUS UNLESS',
     +       ' THEY OCCUR MORE THAN A FEW TIMES TOGETHER.'/
     +       ' ****      THE USUAL RECOVERY ACTION IS FOR MUCOOR TO',
     +       ' IGNORE THE REMAINDER OF THE MU CRATE BEING PROCESSED.'//)
  862 CONTINUE
C
C-----------------------------------------------------------------------
C
C                            INITIALISE COUNTERS ETC
C
C                            SET THE DRIFT TIME OUT-OF-SEQUENCE FLAG = 000027500
C
      ITDOS  = 0
C                            CLEAR HIT COUNTER
      NHITS  = 0
      NDIG   = 0
C
C                            CLEAR COUNTER FOR THE NUMBER OF  ATTEMPTS
C                            AT DATA RECOVERY
C
      NRECOV = 0
C
C                            HAS THE MAXIMUM  NO.  OF  ERROR  MESSAGES
C                            BEEN PRINTED IN THIS JOB?
C
      IF( NMESS .GE. MAXMES ) PFG = .FALSE.
C
C                            SET UP LOOP TO FIND MU HIT DATA
C                            SET IP=2 TO POINT TO FIRST DATA
C                            WORD-1.(IE SKIP BANK DESCRIPTOR)
C
      IP     =   2
      IPLAST =   2 * LMU
      J2     = - NWHIT
      JHIT   =   1
      JCHAM  =   0
      JCRATE =   0
      IER    =   0
      JER    =   0
      ITDH4  =   0
C                            GET TYPE OF DATA
      GO TO 1001
C
C=======================================================================
C=======================================================================
C
C                            START OF MAIN LOOP
C
C        ALL JUMPS IN THIS PROGRAM ARE FORWARDS, EXCEPT TWO:
C          GO TO 2000     *     GO TO 2200
C-----------------------------------------------------------------------
C
C                            RETURN HERE AFTER CONVERTING LAST HIT  TO
C                            COORDS. AND AFTER A RECOVERABLE ERROR
C
 2000  IF( IER .NE. 0 ) GO TO 2201
C
C                            DECIDE WHERE TO RE-ENTER THE MAIN  SEARCH
C                            PROGRAM. THE NEXT WORD COULD BE:
C                                         - END OF DATA
C                                         - NEXT CRATE
C                                         - NEXT CHAM/HIT
C                            IF LAST HIT NUMBER WAS NOT 1 --> MUST  BE
C                            ANOTHER HIT
C
          IF( IHIT .NE. 1 ) GO TO 1012
C
C                            GET NEXT WORD
C
          IP = IP + 1
          IF( IP .GE. IPLAST ) GO TO 200
          NW = HMU(IP)
C
C                            TEST FOR END OF DATA(0F00)
C
          IF( NW .EQ. 3840 ) GO TO 1001
C
C                            TEST FOR NEXT CHAMBER/HIT WORD
C                            ALLOW FOR CORRUPTED CHAMBER NUMBER UP TO
C                            VALUE FOR EOC MARKER.
C
          IF( NW .GE. 4 .AND. NW .LT. 3840 ) GO TO 1013
C
C                            PROBABLY THE NEXT CRATE THEN,  BUT THIS IS
C                            NOT THE CORRECT WAY TO MEET THE NEXT CRATE
C
          IP = IP - 1
          GO TO 1001
C
C-----------------------------------------------------------------------
C
 2201 CONTINUE
      JER   = IER
      JHIT  = 1
      JCHAM = 0
C
C                            ALLOW AN ARBITRARY NO. OF E.O.C. MARKERS
C
      IF( IER    .GT. 1 ) GO TO 1990
      IF( ICRATE .NE. 0 ) GO TO 1990
        IER = 0
        GO TO 1001
 1990 CONTINUE
      IER = 0
      IF( NRECOV .GT. MAXREC ) GO TO 201
      NRECOV = NRECOV + 1
 2200 CONTINUE
C
C                            ERROR RECOVERY: SEARCH FOR NEXT
C                            END-OF-CRATE OR CRATE-MARKER.
C
      NW = HMU(IP)
      IF( NW .EQ. 3840 ) GO TO 2203
      IF( NW .GE. 3841 .AND. NW. LE. 3854 .AND. JER .NE. 2 ) GO TO 2204
C
C                            IF  JER=2  AND  PRESENT  CRATE   HAS   NO
C                            E.O.C.,NEED THE  FOLLOWING  STATEMENT  TO
C                            START NEXT CRATE :
C
      IF( NW .GE. 3841 .AND. NW .LE. 3854 .AND. JER .EQ. 2
     +                 .AND .NW .GT. ICRATE + 3840  ) GO TO 2204
      IP = IP + 1
      IF( IP .GE. IPLAST ) GO TO 201
      GO TO 2200
C
 2203 NMU(63) = NMU(63) + 1
      GO TO 1001
 2204 NMU(64) = NMU(64) + 1
      IP = IP - 1
      GO TO 1001
C
C-----------------------------------------------------------------------
C
C                            --------------
C                            GET NEXT CRATE
C                            --------------
C
 1001 CONTINUE
C
C                            THIS IS THE  INITIAL  ENTRY  POINT.  NEXT
C                            WORD  SHOULD  BE  A   CRATE   WORD.   THE
C                            PROCESSING OF EVERY CRATE MUST START HERE
C                            AND , IN NORMAL CIRCUMSTANCES,  MUST  END
C                            HERE---EXCEPT CRATE 14, WHICH CAN  GO  TO
C                            201 AT THE END WITHOUT  COMING  HERE.  AT
C                            THIS POINT, NOTE THOSE CRATES WHICH HAD A
C                            MISSING E.O.C. MARKER.
C
      IF( JCRATE .GT. 0 .AND. HMU(IP) .NE. 3840 )
     +             NMU(84 + JCRATE) = NMU(84 + JCRATE) + 1
      IP = IP + 1
      IF( IP .GE. IPLAST ) GO TO 200
      NW = HMU(IP)
C
C                            TEST FOR VALID CRATE NUMBER (0 = EOC MARK)
C
      ICRATE = NW - 3840
      IF( ICRATE .EQ. 0 ) GO TO 908
      IF( ICRATE .LT. 1  .OR.  ICRATE .GT. 14 ) GO TO 901
C
C                            CHECK THAT CRATE NUMBER HAS INCREASED
C
      IF( ICRATE .LE. JCRATE     ) GO TO 902
      IF( ICRATE .EQ. JCRATE + 1 ) GO TO 1003
        JCR1 = JCRATE + 1
        ICR1 = ICRATE - 1
C
C                            INCREMENT 'CRATE MISSING' COUNTERS.
C
        DO  1502  JJ = JCR1,ICR1
          IER = 70 + JJ
          MER = 100 * IP + IER
          CALL MUERRY('MUCOOR',MER,' = ERROR CODE. SEE LIST ABOVE.^')
          NMU(70 + JJ) = NMU(70 + JJ) + 1
 1502   CONTINUE
C
C                            MUST PUT IER BACK TO ZERO.
C
        IER   = 0
        NMESS = NMESS + 1
C
 1003 JCRATE = ICRATE
      JCHAM  = 0
      KF910  = 0
C
C-----------------------------------------------------------------------
C
C                            THE NEXT WORD CAN BE EITHER:
C
C                            1) END-OF-CRATE WORD, 3840 (HEX 0F00)
C                            2) TIME REF FOR CRATE, IF THERE ARE HITS
C                            3) CHAMBER/HIT WORD,IF NO HITS,ONLY SINGLES
C
      IP = IP + 1
      NW = HMU(IP)
      IF( NW .EQ. 3840 ) GO TO 1001
C
C                            TEST FOR VALID TIME REF (USUALLY ZERO BUT
C                            AT LEAST ONE EVENT HAS BEEN FOUND WITH
C                            TIME REF WORD = 3584 + NON-ZERO DIGIT)
C                            SO ALLOW TIMREF = 3584 --> 3594
C
      TIMREF = NW .GE. 3584  .AND.  NW .LE. 3594
      IF( .NOT. TIMREF ) IP = IP - 1
C
C                            NEXT WORD MUST BE A CHAMBER/HIT WORD
C
 1012 IP    = IP + 1
      IF( IP .GE. IPLAST ) GO TO 200
      NW    = HMU(IP)
 1013 ICHAM = NW / 4
C
C                            CHECK FOR A VALID CHAMBER NUMBER
C
      IF( ICHAM .LT. 1 .OR. ICHAM .GT. 634 ) GO TO 904
C
C                            CHECK IF CHAMBER IS IN THE CRATE
C
      IF( ICRATE .NE. HSECT(HFR(ICHAM) ) ) GO TO 916
      IHIT = NW - 4*ICHAM + 1
C
C                            IHIT IS BY DEFINITION IN RANGE 1 TO 4
C
C-----------------------------------------------------------------------
C
C                            TEST IF CHAMBER NUMBER AND HIT NUMBER ARE
C                            CONSISTENT WITH THE LAST PROCESSED HIT
C
      IF( JHIT .EQ. 1 ) GO TO 1010
        IF( IHIT  .NE. JHIT - 1 ) GO TO 907
        IF( ICHAM .NE. JCHAM    ) GO TO 905
        GO TO 3906
C
 1010 IF( ICHAM .LE. JCHAM ) GO TO 1906
      GO TO 3906
C
C                            NOTE: CHAMBERS IN CRATES 9 * 10  ARE  NOT
C                            IN SEQUENCE. BEARING THIS IN MIND, DO  WE
C                            HAVE A SEQUENCE ERROR?
C
 1906 IF( .NOT. ( ICRATE .EQ. 9 .OR. ICRATE .EQ. 10 ) ) GO TO 906
      IF( KF910 .EQ. 1 ) GO TO 906
C
      IF(   .NOT. (
     +
     +    ( ICRATE .EQ.  9 .AND. ICHAM .LE. 286 .AND. JCHAM .GE. 323 )
     +                           .OR.
     +    ( ICRATE .EQ. 10 .AND .ICHAM .LE. 298 .AND .JCHAM .GE. 335 )
     +              )
     +                 ) GO TO 906
C
C                            NOT AN ERROR BUT IT CAN ONLY HAPPEN  ONCE
C                            PER CRATE
C
      KF910 = 1
C
 3906 ADJCHM = ICHAM .EQ. JCHAM + 1
      SAMCHM = ICHAM .EQ. JCHAM
      HIT12  = SAMCHM .AND. JHIT .EQ. 2
      IF( .NOT. SAMCHM ) ITDH4 = 0
C
C                            SET ITDH4 = 1 IF 4  HITS IN THIS CHAMBER
C
      IF( .NOT. SAMCHM .AND. IHIT .EQ. 4 ) ITDH4 = 1
C
C                            NEXT WORD IS A DRIFT TIME
C
      IP   = IP + 1
      IITD = HMU(IP)
      KKTD = IITD
C
C                            NEXT WORD IS A LONGITUDINAL TIME DIFFERENCE
C
      IP   = IP + 1
      IITL = HMU(IP)
      KKTL = IITL
C
C                            IF NO TIME REF IN THIS CRATE -->  SINGLES
C                                                              DATA?
C
      IF( IITD .LT. 0 .OR. IITD .GT. 255 .OR.
     +    IITL .LT. 0 .OR. IITL .GT. 255      ) GO TO 1200
C
C                            ERROR IF STILL HERE AND  NO TIME REF.
C
      IF( .NOT. TIMREF ) GO TO 903
      GO TO 1202
C
C-----------------------------------------------------------------------
C
C                            THIS SECTION HANDLES SINGLES RATES DATA
C
 1200 NCNT =  IITD - 2048
      NTIM =  IITL - 2048
      IF( IHIT .NE. 1 ) GO TO 913
      IF( NCNT .LT. 0 .OR. NTIM .LE. 0 ) GO TO 909
      RATE = NCNT
      RATE = 2.0 * RATE / NTIM
C
      IF( ENOUGH ) GO TO 2004
        KMESS = KMESS + 1
CP      WRITE(6,80) ICHAM, NCNT, NTIM, RATE
CP 80   FORMAT(' MUCOOR SINGLES RATES. CHAM=',I3,'   COUNTS=',I3,
CP   +         '   TIME=',I3,'   CNTS/SEC=',F6.1 )
        ENOUGH = KMESS .GE. MAXMUM
C
 2004 CONTINUE
      GO TO 2002
C
C                            TEST IF DRIFT TIMES ARE IN SEQUENCE
C
 1202 IF( .NOT. SAMCHM ) GO TO 1014
C
C                            IF DRIFT TIMES ARE OUT OF  SEQUENCE,  SET
C                            ITDOS AND FLAG ALL HITS IN  THIS  CHAMBER
C                            ASBAD. DO THIS AFTER  PROCESSING HIT1.
C
        IF( IITD .LT. JITD ) GO TO 1014
        ITDOS   = 1
        NMU(26) = NMU(26) + 1
C
C                            NORMALLY, NO PRINTING FOR THIS ERROR
C
 1014 CONTINUE
C
C                            INITIALISE BIT 8 OF HIT STATUS  OK  (I.E.
C                            HIT ONE NOT KILLED ). THIS FLAG  IS  ONLY
C                            MEANINGFUL FOR HIT1.
C
      HLUN3 = 2**8
C
C                            INITIALISE BIT 9 OF HIT STATUS  OK  (I.E.
C                            HIT DID NOT MULTI-PULSE)
C
      HLUN4 = 0
      IF( .NOT. SAMCHM ) GO TO 1015
C
C                            SINCE THE 4'TH  HIT  DESTROYS  THE  DRIFT
C                            INFORMATION OF THE 1'ST, TRY  TO  RECOVER
C                            THIS INFORMATION BY SUBTRACTING  A  FIXED
C                            QUANTITY FROM THE DRIFT TIME OF HIT 2. IF
C                            THERE WERE 4 HITS NOTE THE DRIFT TIME FOR
C                            THE 2'ND HIT. RECORD TEMPORARILY THE DRIFT
C                            TIME FOR EACH HIT.
C
      IDRIFT(IHIT) = IITD
C-----------------------------------------------------------------------
C
C                            DO SOMETHING ABOUT HIT 4 KILLING HIT 1
C
      IF(IHIT.GT.1) GOTO 1015
        IF(ITDH4.EQ.1.AND.IITD.EQ.0.AND.IHIT.EQ.1) GOTO 915
          GOTO 1015
  915     NMU(25)=NMU(25)+1
C
C                            N.B. NORMALLY, NO PRINTING FOR THIS ERROR
C                            WE HAVE LOST HIT ONE, RECOVER IT
C
          KKTD = IDRIFT(2) - ICONST
C
C                            SET STATUS BANK THAT THIS HIT IS
C                            BODGED..( BIT 8=0 )
C
          HLUN3 = 0
 1015 CONTINUE
C
C-----------------------------------------------------------------------
C                            NOTE: KKTD,MAY AT THIS STAGE,BE DIFFERENT
C                            TO IITD IF HIT1 HAS BEEN KILLED BY HIT4
C
          ITD = 0
          IF(KKTD.GE.0.AND.KKTD.LT.256) ITD = KKTD
C
          ITL = 0
          IF(KKTL.GE.4.AND.KKTL.LE.255 ) ITL = KKTL
C
C                            COUNT NUMBER OF DIGITISINGS FOUND
C
          NDIG = NDIG + 1
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
C                            FIX-UP CHAMBER STATUS FOR CERTAIN PERIODS
C
          CALL MUFSTA( IMONTH, IYEAR )
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
C                            GET THE STATUS OF THE CHAMBER
C
          GOOD = HMCSTA(ICHAM).EQ.0
C
C                            IGNORE HITS FROM BAD CHAMBERS
C
          IF( GOOD ) GO TO 1017
C
C                            SIGNAL FROM CHAMBER WITH STATUS 'NOT OK'.
C
            NMU(40) = NMU(40) + 1
            GO TO 2001
C
C                            SIGNAL FROM CHAMBER WITH STATUS 'OK'.
C
 1017 CONTINUE
C
C----------------------------------------------------------------------
C
C                            REMOVE HITS FROM SECOND BEAM CROSSING.
C
C                            AS OF 16/03/83, THE PROCEDURE  RE  ITMCUT
C                            IS EFFECTIVELY OBSOLETE , EXCEPT THAT  IT
C                            WILL CUT  OUT  ANY  WILD  DRIFT  TIMES.
C                            WE NOW  PUT  ITMCUT  =  130 CLOCK PULSES.
C
C                            NORMALLY , THIS MEANS A FIXED CUT  OF  80
C                            CLOCK PULSES (4.0  MICROSECS)  BUT  ,  IF
C                            DRIFT VELOCITY DECREASES  ,  NEED  LARGER
C                            CUTOFF. ---- THIS HAPPENED  BETWEEN  RUNS
C                            4220 * 4482. THE NORMAL DRIFT VELOCITY IS
C                            2150 MICRONS  PER  CLOCK  PULSE.  ---  TO
C                            ABOUT   5   %,   THAT   IS.    SO     PUT
C                            ITMCUT=80*2260/NEW VDRIFT .
C
      ITMCUT = 130
      IF( KRUN .GE. 4220 .AND. KRUN .LE. 4482 )
     +                                  ITMCUT = 180800 / HVDRFT(ICHAM)
      IF ( ITD .GE. ITMCUT ) GO TO 2001
C
C-----------------------------------------------------------------------
C
C                            DOUBLE PULSE CUT (YEAR DEPENDENT)
C
C                            IF THE  PREVIOUS  HIT  WAS  IN  THE  SAME
C                            CHAMBER AND IF IT  WAS  CLOSER  THAN  THE
C                            CUT, MU2PUL, THEN DELETE  THE  PREVIOUSLY
C                            ENTERED HIT BY REDUCING NHITS BY ONE  AND
C                            RESETTING THE POINTER J2, THEN PROCEED TO
C                            PROCESS THE PRESENT HIT.
C
C
         IF( .NOT. SAMCHM ) GO TO 1016
         IF( JITD - KKTD .GT. MU2PUL( IMONTH, IYEAR )    .OR.
     +       JITD - KKTD .LE. 0                            ) GO TO 1016
C
           HLUN4 = 2 ** 9
C
C                            CHECK IF PREVIOUS HIT WAS KEPT. IF NOT DO
C                            NOT DELETE A HIT, JUST PROCESS THIS ONE.
C
           IF( JITD .GE. ITMCUT ) GO TO 1016
C
C                            ITS A  CLOSE  PULSE  ==> DELETE ONE
C
           NHITS   = NHITS - 1
           J2      = J2 - NWHIT
           NMU(24) = NMU(24) + 1
 1016    CONTINUE
C
C-----------------------------------------------------------------------
C
C      WE NOW HAVE A SET OF VALID NUMBERS (ICRATE,ICHAM,IHIT,ITD,ITL)
C      WHICH HAVE SATISFIED ALL THE DATA CONSISTENCY REQUIREMENTS.
C      THESE WILL NOW BE CONVERTED INTO COORDINATES.
C      THEY CAN NO LONGER BE REJECTED, BUT ONLY FLAGGED.
C      ITD, ITL  MAY BE ZERO, INDICATING A HIT BUT NO POSITIONAL
C      INFORMATION.
C
C-----------------------------------------------------------------------
C
C                            GET THE GLOBAL PARAMETERS
C
C                            FRAME NUMBER.
C
          IFRAME = HFR(ICHAM)
C
C                            ORIENTATION PARAMETER.
C
          IOR    = HNORM(IFRAME)
C
C                            CALCULATE THE NORMAL COORDINATE.
C
          ICNORM = HDIST(IFRAME) + HD1(ICHAM)
C
C                            PROCESS THE LONGITUDINAL DATA
C
          IMMTL  = 0
C
C                            WE HAVE TO ENSURE THAT THE  HLSF(4,ICHAM)
C                            IS NOT USED FOR  LONGITUDINAL  COORDINATE
C                            CALCULATION.
C
          LSFACT = HLSF(IHIT,ICHAM)
          IF( IHIT .EQ. 4 ) LSFACT = HLSF(3,ICHAM)
          IF( ITL  .NE. 0 ) IMMTL  = (LSFACT*( ITL - HLTP(ICHAM) ))/100
C
C                            SET HLUN1 TO 1 (=0001)
C
          HLUN1 = 1
          LONG  = 1
C
C                            FIND THE LONGITUDINAL CENTRE OF THE CHAMBER
C                            FIND THE LONGITUDINAL COORD.. FLAG IF BAD
C
          ICLCEN = (HCLLO(IFRAME) + HCLHI(IFRAME)) / 2
          ICLONG = ICLCEN + IMMTL
C
C                            DATA IS GOOD IF POSITION IS WITHIN IZEXTN
C                            OF END OF CHAMBER
C
          IF( ITL    .NE. 0                         .AND.
     *        ICLONG .GE. (HCLLO(IFRAME) - IZEXTN)  .AND.
     *        ICLONG .LE. (HCLHI(IFRAME) + IZEXTN)       ) GO TO 7
C
C                            THE DATA IS BAD. SET HLUN1 TO 0 (=0000)
C
             HLUN1 = 0
             LONG  = 0
C
C                            SET ICLONG TO COORDINATE OF CHAMBER CENTRE
C
             ICLONG = ICLCEN
C
C                            INCREMENT BAD LONGITUDINAL COUNT.
C
             NMU(21) = NMU(21) + 1
    7     CONTINUE
C
          IF( ICLONG .LT. HCLLO(IFRAME) .OR. ICLONG .GT. HCLHI(IFRAME) )
     *               NMU(23) = NMU(23) + 1
C
C                            IF POSITION IS OUTSIDE CHAMBER, PULL IT IN
C
          IF( ICLONG .LT. HCLLO(IFRAME)) ICLONG = HCLLO(IFRAME)
          IF( ICLONG .GT. HCLHI(IFRAME)) ICLONG = HCLHI(IFRAME)
C
C                            FOR  THE  HANG  CORRECTION  WE  NEED  THE
C                            CORRECTED IMMTL
C
C                            ICLONG, HLUN1, LONG ARE NOW SET.
C
C                            PROCESS THE DRIFT TIME DATA (NEW METHOD)
C
          IMMTD = 0
          IF( ITD .LT. 0 ) ITD = 0
          DTX   = ITD
C
C                            CORRECT SHORT AND LONG DRIFT TIMES
C                            (BUT NOT FOR MONTE-CARLO DATA).
C
          IF( ITD  .GT. 9  .AND.  ITD .LT. 64 ) GO TO 864
          IF( KRUN .LT. 100 ) GO TO 864
            DTX = XDT(ITD + 1)
  864     CONTINUE
C
          VDRIFT = HVDRFT(ICHAM)
          DTP    = HDTP(ICHAM) / 10.0
          ALFA   = ABS( ATAN( TANTHM(ICHAM)  )  )
          ALFAP  = ALFA - 0.436
          IF( ALFAP .LT. 0.0001 ) ANGEFF =  0.0
          IF( ALFAP .GT. 0.0001 ) ANGEFF = 15.0 * TAN(ALFAP)
C
C                            FOR MONTE-CARLO DATA, NO ANG. CORRECTION
C
          IF( KRUN .LT. 100 ) ANGEFF = 0.0
          XMMTD = VDRIFT * (DTX - DTP) / 1000.0  +  ANGEFF
C
C                            CORRECT FOR CRATE 6 CLOCK PULSE EXCEPT M.C.
C
C                            ( NOTE THAT THE CALIBRATION FILES  ALWAYS
C                            HAVE CRATE 6 VDR * DTP  EQUAL  TO  GLOBAL
C                            VALUES....THIS  IS  THE  EFFECT  OF   THE
C                            VARIOUS UPDATES AS OF 16/03/83. )
C
          IF( ICRATE .EQ. 6 .AND. KRUN .LT. 8256 .AND. KRUN .GT. 99 )
     +                                            XMMTD = 1.26 * XMMTD
C
          IMMTD = XMMTD
C
C                            CALCULATE THE WIRE COORDINATE.
C                            APPLY SOFTWARE SHIFT TO WIRE POSITION FOR
C                            ALL DATA AFTER BEGINNING OF 1980
C
          ISHIFT = 0
          IF( IYEAR .GE. 1980 ) ISHIFT = HLSF(4,ICHAM)
          IF( IABS(ISHIFT) .GT. 300 ) ISHIFT = 0
C
C                            NO SOFTWARE SHIFTS FOR MONTE-CARLO  DATA.
C
          IF( KRUN .LT. 100 ) ISHIFT = 0
          ICWIRE = HCTW(ICHAM) - ISHIFT
C
C                            SET HLUN2 TO 2 (=0010)
C
          HLUN2 = 2
          NDRIF = 1
C
C                            CALCULATE THE TWO (L/R AMBIGUOUS)
C                            TRANSVERSE COORDINATES.
C
          IF( IMMTD .LT. IMTDLO  .OR.  IMMTD .GT. IMTDHI ) GO TO 85
C
C                            DATA IS GOOD, BUT PULL IN IF JUST OUTSIDE
C                            PHYSICAL RANGE
C
          IF( IMMTD .LT. 0  .OR.  IMMTD .GT. 155 ) NMU(22) = NMU(22) + 100086900
          IF( IMMTD .LT. 0   ) IMMTD = 0
          IF( IMMTD .GT. 155 ) IMMTD = 155
C
C                            CALCULATE THE COORDINATES WITHOUT THE
C                            ANGULAR CORRECTION
C
          ICT1 = ICWIRE - IMMTD
          ICT2 = ICWIRE + IMMTD
          GO TO 8
C
C                            THE DATA IS BAD, SO SET HLUN2 TO 0(=00000)
C
   85     HLUN2 = 0
          NDRIF = 0
C
C                            SET IC1 * IC2 TO COORD. OF CHAMBER CENTRE
C
          ICT1  = ICWIRE
          ICT2  = ICWIRE
C
C                            INCREMENT BAD DRIFT TIME COUNT.
C
          NMU(20) = NMU(20) + 1
    8     CONTINUE
C
C                            ICT1, ICT2, HLUN2, NDRIF ARE NOW SET.
C
C-----------------------------------------------------------------------
C
C                            STORE THE RESULTING  COORDINATES  IN  THE
C                            OUTPUT ARRAY HC.
C
      IF( NHITS .GE. NHITMX ) GO TO 910
        NHITS = NHITS + 1
C                            J2 WAS SET TO -NWHIT AT BEGINNING OF EVENT
        J2 = J2 + NWHIT
C
C                            FILL OUTPUT ARRAY HC.
C                            4*(CHAMBER NUMBER) + (HIT NUMBER -1)
C
        HC(J2 + 1) = HMU(IP - 2)
C
C                            10*LAYER NO. + ORIENTATION PARAMETER (HOR).
C
        HC(J2 + 2) = 10 * HLAYER(IFRAME) + IOR
C
C                            OVERALL X DISPLACEMENT.
C
        IX = HOVALL( HUNIT( IFRAME ) )
C
C                            X,Y,Z COORDINATES
C
        GO TO (11,12,13),IOR
C
   11     HC(J2 + 3) = ICNORM + IX
          HC(J2 + 4) = ICT1
          HC(J2 + 5) = ICLONG
          HC(J2 + 6) = ICNORM + IX
          HC(J2 + 7) = ICT2
          HC(J2 + 8) = ICLONG
          HLUNY      = HB6 + HB5 + HB4 + NDRIF*HB3 + LONG*HB2
          GO TO 20
C
   12     HC(J2 + 3) = ICT1 + IX
          HC(J2 + 4) = ICNORM
          HC(J2 + 5) = ICLONG
          HC(J2 + 6) = ICT2 + IX
          HC(J2 + 7) = ICNORM
          HC(J2 + 8) = ICLONG
          HLUNY      = HB7 + HB5 + NDRIF*HB4 + HB3 + LONG*HB2
          GO TO 20
C
   13     HC(J2 + 3) = ICT1 + IX
          HC(J2 + 4) = ICLONG
          HC(J2 + 5) = ICNORM
          HC(J2 + 6) = ICT2 + IX
          HC(J2 + 7) = ICLONG
          HC(J2 + 8) = ICNORM
          HLUNY      = HB7 + HB6 + NDRIF*HB4 + LONG*HB3 + HB2
C
C                            POINTER TO CHAM/HIT DATA WORD IN RAW DATA
C
   20     HC(J2 + 9) = IP - 2
C
C                            PACK THE STATUS INFORMATION FOR THIS HIT
C                            IN THE FIRST 10 BITS OF HLUN
C
          HLUN(NHITS) = HLUN1 + HLUN2 + HLUN3 + HLUN4 + HLUNY
C
C-----------------------------------------------------------------------
C
C                            RECORD CURRENT PARAMETERS
C                            (TO CHECK NEXT DIGITISING)
C
 2001 JITD  = IITD
      JITL  = IITL
      JTD   = ITD
      JTL   = ITL
      JHIT  = IHIT
      JCHAM = ICHAM
C
      IF( IHIT  .GT. 1 ) GO TO 2002
      IF( ITDOS .EQ. 0 ) GO TO 2002
C
C                            DRIFT TIMES ARE OUT OF SEQUENCE FOR  THIS
C                            CHAMBER. LOOK BACK THRO' THE ARRAY HC AND
C                            COUNT THE NO. OF  HITS  WHICH  WERE  KEPT
C                            FROM  THIS  CHAMBER.  THEN  CORRECT  HLUN
C                            ENTRY FOR THESE HITS BY MAKING BOTH DRIFT
C                            AND LONGITUDINAL COORDINATES 'BAD'.
C
      IKEPT = 0
      J1    = J2 - 3 * NWHIT
C
      DO  3010  JJ = J1,J2,NWHIT
        JHCHAM = HC(JJ + 1) / 4
        IF( JHCHAM .EQ. ICHAM ) IKEPT = IKEPT + 1
 3010 CONTINUE
C
      HLUN1 = 0
      HLUN2 = 0
      LONG  = 0
      NDRIF = 0
C
      DO  3025  I = 1,IKEPT
        GO TO (3011,3012,3013),IOR
 3011     HLUNY = HB6 + HB5 + HB4 + NDRIF*HB3 + LONG*HB2
          GO TO 3020
 3012     HLUNY = HB7 + HB5 + NDRIF*HB4 + HB3 + LONG*HB2
          GO TO 3020
 3013     HLUNY = HB7 + HB6 + NDRIF*HB4 + LONG*HB3 + HB2
 3020     HLUN(NHITS-IKEPT+I) = HLUN1 + HLUN2 + HLUN3 + HLUN4 + HLUNY
C
 3025 CONTINUE
C
C                            BEFORE GOING ON TO  NEXT  CHAMBER  ,  SET
C                            ITDOS TO ZERO AND INCREMENT NMU(27).
C
          ITDOS   = 0
          NMU(27) = NMU(27) + 1
          GO TO 2002
C
C-----------------------------------------------------------------------
C
C                            ERROR COUNTING (RECOVERABLE ERRORS)
C
C                            901 IS INVALID CRATE NUMBER
C
  901 NMU(60) = NMU(60) + 1
      IER = 1
      GO TO 930
C
C                            902 IS CRATE NUMBER OUT OF ORDER
C
  902 NMU(61) = NMU(61) + 1
      IER = 2
      GO TO 930
C
C                            903 IS MISSING TIME REF WORD
C
  903 NMU(62) = NMU(62) + 1
      IER = 3
      GO TO 930
C
C                            904 IS INVALID CHAMBER NUMBER
C
  904 NMU(41) = NMU(41) + 1
      IER = 4
      GO TO 930
C
C                            905 IS NEW CHAMBER WITH JHIT = 1
C
  905 NMU(29) = NMU(29) + 1
      IER = 5
      GO TO 930
C
C                            906 IS CHAMBER NO OUT OF SEQUENCEE
C
  906 NMU(42) = NMU(42) + 1
      IER = 6
      GO TO 930
C
C                            907 IS HIT NUMBER NOT DECREASED BY ONE
C
  907 NMU(28) = NMU(28) + 1
      IER = 7
      GO TO 930
C
C                            908 IS END-OF-CRATE FOUND WHEN CRATE MARKER
C                                                      WAS EXPECTED.
C
  908 NMU(65) = NMU(65) + 1
      IER = 8
      GO TO 930
C
C                            909 IS NEGATIVE COUNTS OR TIME IN SINGLES
C                                                              DATA
  909 NMU(44) = NMU(44) + 1
      IER = 9
      GO TO 930
C
C                            913 IS SINGLES DATA WITH HIT NOT 1
C
  913 NMU(30) = NMU(30) + 1
      IER = 13
      GO TO 930
C
C                            916 IS CHAMBER IN WRONG CRATE
C
  916 NMU(43) = NMU(43) + 1
      IER = 16
C
  930 NMESS = NMESS + 1
      MER   = 100 * IP + IER
      CALL MUERRY('MUCOOR',MER,' = ERROR CODE. SEE LIST ABOVE.^')
C
C                            AFTER ERROR, TRY TO START AGAIN
C                            JUMP BACK TO BEGINNING OF THE MAIN LOOP
C
 2002 CONTINUE
      GO TO  2000
C
C                  END OF MAIN LOOP
C=======================================================================
C=======================================================================
C
C                            END OF INPUT DATA REACHED
C                            CHECK THE POSITION OF THE POINTER
C
  200  IF( IP .EQ. IPLAST + 1 ) GO TO 201
       IF( IP .NE. IPLAST     ) GO TO 911
       IF( HMU(IP) .EQ. 0  .OR.  HMU(IP) .EQ. 3840 ) GO TO 201
       GO TO 912
C
C                            DID CRATE  14  HAVE AN  E.O.C. MARKER?
C
  201  IF( .NOT. (    ICRATE  .EQ. 14  .AND.
     +              ( HMU(IP) .EQ. 3840  .OR.  HMU(IP-1) .EQ.3840 )
     +           )
     +   )                NMU(98)=NMU(98)+1
C
C                            INCREMENT NO. OF HITS AND 'NO DATA' COUNT
C
       NMU(2) = NMU(2) + NHITS
       IF( NHITS .LE. 0 ) NMU(3) = NMU(3) + 1
C
CP     WRITE(6,444)IEV,NHITS
CP444  FORMAT(1X,/,'++++ EVENT ',I3,' ++++ NUMBER OF HITS ',I3/)
C
      RETURN
C
C=======================================================================
C=======================================================================
C
C                            PROCESS THE VARIOUS ERROR CONDITIONS
C
C                            910 IS OVERFLOW OF OUTPUT ARRAY
C
  910 NMU(7) = NMU(7) + 1
      IER = 10
      GO TO 998
C
C                            911 IS LENGTH ERROR IN ARRAY HMU
C
  911 NMU(8) = NMU(8) + 1
      IER = 11
      GO TO 998
C
C                            912 IS LAST WORD NOT 0 OR 3840 IN HMU
C
  912 NMU(9) = NMU(9) + 1
      IER = 12
C
  998 NMESS = NMESS + 1
      MER   = 100 * IP + IER
      CALL MUERRY('MUCOOR',MER,' = ERROR CODE. SEE LIST ABOVE.^')
C
  999 CONTINUE
C                            999 IS THE EXIT FOR ERRORS 10, 11, 12.
C
C                            INCREMENT NO. OF HITS AND 'NO DATA' COUNT
C
       NMU(2) = NMU(2) + NHITS
       IF( NHITS .LE. 0 ) NMU(3) = NMU(3) + 1
C
      RETURN
C                            NMU DESCRIPTION NOW ON MEMBER @MUSTAT
      END
