C   18/12/87 807262019  MEMBER NAME  MUCON    (JADEMUS)     FORTRAN
C
C-----------------------------------------------------------------------
      SUBROUTINE MUCON
C-----------------------------------------------------------------------
C
C LAST CHANGE 20.03 26/07/88 CHRIS BOWDERY- PRINT MESSAGES ONLY ONCE
C      CHANGE 23.45 17/12/87 CHRIS BOWDERY- CHANGES IF NOT MUON MC DATA
C      CHANGE 20.30 19/02/84 CHRIS BOWDERY- REMOVE OLDSCR.NOW IN UCOPY
C      CHANGE 07.30 10/04/81 HUGH MCCANN  - JADEMUS UPDATE.
C      CHANGE 09.50 26/09/79 JOHN ALLISON.
C      CHANGE 20.16 21/09/79 HARRISON PROSPER.
C
C-----------------------------------------------------------------------
C
C      MUCON TRANSFERS ALL BOS BANKS INTO COMMON /CALIBR/
C
C-----------------------------------------------------------------------
C
      IMPLICIT INTEGER*2 (H)
C
      LOGICAL  OLD, FIRST, FIRST1, FIRST2
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
C                            DECLARATIONS, INITIALISATIONS, ETC.
C
      DIMENSION HUNI1(82)
C
      DATA HUNI1/3,1,1,1,1,1,1,1,1,1,
     *           1,1,1,3,2,2,2,2,2,2,
     *           2,2,2,2,2,2,3,3,3,3,
     *           3,3,3,3,3,3,3,3,4,4,
     *           5,5,4,4,5,5,4,4,5,5,
     *           4,4,5,5,4,4,5,5,4,4,
     *           5,5,4,4,5,5,4,5,4,5,
     *           4,5,4,5,4,5,4,5,4,5,
     *           4,5                  /
C
      DATA  FIRST, FIRST1, FIRST2 / .TRUE., .TRUE., .TRUE. /
C
C------------------  C O D E  ------------------------------------------
C
      IF( .NOT. FIRST ) GO TO 1
        CALL MUMESS('MUCON ',0,'CALLED TO COPY MUON CALIBRATION.^')
        CALL MUMESS('MUCON ',0,'WARNING: MESSAGE IS NOT REPEATED!^')
        FIRST = .FALSE.
C
C                            CHECK FOR 'MFFI' - SHOULD  BE  THERE  FOR
C                            OLD AND NEW.
C
 1    CALL CLOC(IP,'MFFI',2)
      IF( IP .LE. 0 ) GO TO 98
      CALL UCOPY(IDATA(IP+1),HMFFIX,IDATA(IP))
C
C                            CHECK FOR NEW BANKS.
C
      CALL CLOC(IP,'MUCD',0)
      IF( IP .GT. 0 ) GO TO 2
C                            OLD (PRE MAY 1979).
      OLD    = .TRUE.
      NVERSN = 1
      CALL UCOPY( 'OLD (PRE MAY 1979) MONTE CARLO CALIBRATION DATA.^
     +      ',DESCRP,15)
      CALL VZERO(HOVALL,3)
      GO TO 3
C                            NEW.
 2    OLD = .FALSE.
      CALL UCOPY(IDATA(IP+1),NVERSN,IDATA(IP))
      CALL CLOC(IP,'MUOV',0)
      CALL UCOPY(IDATA(IP+1),HOVALL,IDATA(IP))
C
C                            IF BANKS ARE EMPTY, TAKE NO FURTHER ACTION
C
      IF( NVERSN .LE. 0 ) GO TO 98
C
C                            FIND OTHER BANKS, ADDING NEW ARRAYS WHERE
C                            NECESSARY.
C
 3    CALL CLOC(IP,'MCFI',3)
      CALL UCOPY(IDATA(IP+1),HMCFIX,IDATA(IP))
      IF(OLD)CALL UCOPY(HUNI1,HUNIT,41)
C
      CALL CLOC(IP,'MFSU',4)
      CALL UCOPY(IDATA(IP+1),HMFSUR,IDATA(IP))
C
      CALL CLOC(IP,'MCSU',5)
      CALL UCOPY(IDATA(IP+1),HMCSUR,IDATA(IP))
C
      CALL CLOC(IP,'MCEL',6)
      CALL UCOPY(IDATA(IP+1),HMCELE,IDATA(IP))
      IF( .NOT. OLD ) GO TO 4
      DO  8  I = 1,NCHAMS
      HVDRFT(I) = HVDR
 8    CONTINUE
 4    CONTINUE
C
      CALL CLOC(IP,'MCST',7)
      CALL UCOPY(IDATA(IP+1),HMCSTA,IDATA(IP))
      IF(OLD)CALL VZERO(HMCSTA,IDATA(IP))
C
      CALL CLOC(IP,'MUFI',8)
      CALL UCOPY(IDATA(IP+1),HFILDA,IDATA(IP))
C
      CALL CLOC(IP,'MUYO',9)
      CALL UCOPY(IDATA(IP+1),HYKNMI,IDATA(IP))
C
      CALL CLOC(IP,'MUEN',10)
      CALL UCOPY(IDATA(IP+1),IZEII,IDATA(IP))
C
C                            PRINT OUT CALIBRATION DESCRIPTION ONCE
C
      IF( .NOT. FIRST2 ) RETURN
        CALL MUMESS('MUCON ',NVERSN,DESCRP)
        CALL MUMESS('MUCON ',0,'WARNING: MESSAGE IS NOT REPEATED!^')
        FIRST2 = .FALSE.
C
      RETURN
C
C                            NO MUON INFORMATION IN THE BANKS.
C
 98   IF( .NOT. FIRST1 ) RETURN
        CALL MUMESS('MUCON ',0,'MUON CALIB. DATA BANKS NOT PRESENT.^')
        CALL MUMESS('MUCON ',0,'WARNING: MESSAGE IS NOT REPEATED!^')
        FIRST1 = .FALSE.
C
      RETURN
      END
