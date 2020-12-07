C   09/10/85            MEMBER NAME  MUANAC   (JADEMUS)     FORTRAN
C
C-----------------------------------------------------------------------
      SUBROUTINE MUANAC
C-----------------------------------------------------------------------
C
C LAST CHANGE 15.40  8/10/85 CHRIS BOWDERY - EXPAND HC AND HLUN ARRAYS
C      CHANGE 23.30 29/09/84 CHRIS BOWDERY - CALIBRATION CHECKING CODE
C      CHANGE 12.00  9/03/84 CHRIS BOWDERY - NEW MUCOOR DATE.
C      CHANGE 10.00 13/01/84 CHRIS BOWDERY - NEW MUCOOR DATE.
C      CHANGE 12.00 25/10/83 HUGH MCCANN   - NEW MUCOOR DATE.
C      CHANGE 11.00 16/03/83 HUGH MCCANN   - NEW MUCOOR DATE.
C
C-----------------------------------------------------------------------
C
C        THIS IS THE  FIRST  ROUTINE  OF  THE  MUON  ANALYSIS  CHAIN.IT
C        CONVERTS THE THE MUON RAW DATA (IBM FORMAT) TO COORDINATES AND
C        STORES THEM, AND OTHER INFORMATION, IN 'MUR1' BANKS 0,1,AND 2.
C        (FOR DESCRIPTION OF BANKS SEE @MUINFOM).
C
C        NOTE BANK 0 IS CREATED EVEN IF THERE ARE NO MUON HITS.
C
C        THERE ARE 2 SIGNAL TO COORDINATE CONVERSION ROUTINES...
C
C    1)  MUTINY, WHICH USES  A  CONDENSED  SET  OF  CALIBRATION  DATA
C        PREPARED BY MUCONT (FOR MONTE  CARLO  OUTPUT  -  SEE  MCJADE),
C        WHICH CALLS A VERSION OF MUDOWN.
C
C    2)  MUCOOR, WHICH USES THE FULL MUON CALIBRATION DATA PREPARED BY
C        MUCON (FOR MONTE CARLO OUTPUT - SEE MCJADE).
C
C HMU IS THE RAW MU DATA.
C LMU IS THE LENGTH OF THE RAW DATA BANK MEASURED IN I*4 WORDS.
C
C HC IS THE MU COORDINATE OUTPUT ARRAY IN 9'S.....
C               WORD  1     -  4*CHAMBER NUMBER + (HIT NUMBER -1).
C               WORD  2     -  10*LAYER NUMBER + ORIENTATION PARAMETER.
C               WORDS 3 - 8 -  X,Y,Z(LEFT),X,Y,Z(RIGHT) (MM).
C               WORD  9     -  POINTER TO RAW DATA
C
C-----------------------------------------------------------------------
C
      IMPLICIT INTEGER*2 (H)
C
C                            COMMONS.
C
C----------START OF MACRO CMUBCS----------------------------------------
      COMMON /BCS/IDATA(1)
      DIMENSION HDATA(1),ADATA(1)
      EQUIVALENCE (HDATA(1),ADATA(1),IDATA(1))
C----------END OF MACRO CMUBCS------------------------------------------
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
C
      COMMON /CWORK/ HC(3600),HLUN(400)
C
C                            DATA INITIALISATION STATEMENTS.
C
C                            NWHIT IS NO. OF WORDS PER HIT
      DATA NWHIT/9/
C
C                            NHITMX  IS  THE  MAXIMUM   NO.   OF   HITS
C                            CONVERTED.
      DATA NHITMX/400/
C
C------------------  C O D E  ------------------------------------------
C
C                            FIND MUON DATA BANK POINTER.
C
      IPMU = IDATA(IBLN('MUEV'))
      IF( IPMU .NE. 0 ) GO TO 1
C
C                            NO MUON BANK PRESENT.
      NHITS = 0
      GO TO 2
C
C                            CHECK THE CALIBRATION TO SEE IF MISSING
C                            IF 2 CONSTANTS ARE ZERO, THEN NO CALIB.
C
  1   IF( IZOEP1 .EQ. 0  .AND.  IZOEP3 .EQ. 0 ) GO TO 95
C
C
C                            CALL MUCOOR TO GET COORDINATES, ETC.
C
      CALL MUCOOR(IDATA(IPMU+1),NHITS,HC(1),NHITMX,IDATA(IPMU),HLUN(1),
     +            NWHIT)
C-----------------------------------------------------------------------
C
C                            CREATE 'MUR1' BANK 0 - MUON GENERAL INFO
C
 2    CONTINUE
C                            ADD  'MUR1'  TO  SPECIAL  LIST  READY  FOR
C                            WRITING.
      CALL BSAW(1,'MUR1')
      IBANK=0
      CALL CCRE(IP0,'MUR1',IBANK,9,IER)
      IF(IER.NE.0)GO TO 96
C                            NO. OF HITS.
      IDATA(IP0+1) = NHITS
C                            NO. OF I*2 WORDS  PER  HIT  IN  COORDINATE
C                            BANK.
      IDATA(IP0+3) = NWHIT
C                            DATE OF VERSION OF MUCOOR  WHICH  PRODUCES
C                            COORDINATES.
      IDATA(IP0+8) = 840929
C                            CALIBRATION DATA ISSUE.
      IDATA(IP0+9) = NVERSN
C
C-----------------------------------------------------------------------
C
C                            CREATE 'MUR1' BANK  1  -  MUON  COORDINATES
C
      IF(NHITS.LE.0)GO TO 98
      LB1=(NWHIT*NHITS+1)/2
      IBANK=1
      CALL CCRE(IP1,'MUR1',IBANK,LB1,IER)
      IF(IER.NE.0)GO TO 96
      CALL UCOPY(HC(1),IDATA(IP1+1),LB1)
C
C
C-----------------------------------------------------------------------
C
C                            CREATE 'MUR1' BANK 2 - HIT STATUS BANK.
C
      IF(NHITS.LE.0) RETURN
      LB2=(NHITS+1)/2
      IBANK=2
      CALL CCRE(IP2,'MUR1',IBANK,LB2,IER)
      IF(IER.NE.0)GO TO 96
      CALL UCOPY(HLUN(1),IDATA(IP2+1),LB2)
C
      RETURN
C
C-----------------------------------------------------------------------
C
C                            ERROR CONDITIONS.
C
 95   CALL MUERRY('MUANAC',0,'WARNING: NO CALIBRATION FOUND. IF MC DATA,
     + WAS EVREAD USED TO PROCESS CALIB. RECORDS? MUCOOR NOT CALLED.^')
      RETURN
C
 96   IF( IER .GT. 1 ) GO TO 97
      CALL MUERRY('MUANAC',IBANK,'''MUR1'' ALREADY PRESENT.^')
      RETURN
C
 97   CALL MUERRY('MUANAC',IBANK,'NOT ENOUGH SPACE FOR ''MUR1''.^')
      RETURN
C
 98   IF( IPMU .NE. 0 ) RETURN
      CALL MUERRY('MUANAC',0,' WARNING: MUEV BANK NOT LOCATED.^')
      RETURN
C
      END
