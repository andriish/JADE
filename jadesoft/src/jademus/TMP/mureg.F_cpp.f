C   20/10/81 404162044  MEMBER NAME  MUREG    (JADEMUS)     FORTRAN
C
C-----------------------------------------------------------------------
      SUBROUTINE MUREG(IPRINT)
C-----------------------------------------------------------------------
C
C LAST CHANGE 20.00 16/04/84 CHRIS BOWDERY-BETTER TEST  NOW MADE
C      CHANGE 13.00  9/03/84 CHRIS BOWDERY-TEST TO ENSURE CAL. READ
C      CHANGE 13.00 23/11/83 HUGH MCCANN - CORRECT YOKE PARAMETERS.
C                                          ...FURTHER ITERATION !!
C      CHANGE 22.00 22/10/83 HUGH MCCANN - CORRECT YOKE PARAMETERS.
C                                          AND INDENT ENTIRE CODE.
C      CHANGE 13.30 22/02/82 HUGH MCCANN - FOR IMPROVED ERROR OUTPUT.
C                                     MUMESS O/P FOR IPRINT >= 6 * < 10.
C      CHANGE 16.05 17/06/81 JOHN ALLISON - TO EXTEND REGIONS.
C
C ABSTRACTS FROM THE MU-FILTER CONSTANTS THE REGIONS FOR /CMUREG/.
C PRINTING SUPPRESSED IF IPRINT.LT.10.
C
C-----------------------------------------------------------------------
C
      IMPLICIT INTEGER*2 (H)
C
      LOGICAL ERROR
C
C                            COMMONS.
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
C------------START OF MACRO CMUREG--------------------------------------
C
      COMMON /CMUREG/NREGS,XRLO(100),XRHI(100),YRLO(100),YRHI(100),
     * ZRLO(100),ZRHI(100),HRMASK(100),HRFACE(100),HRTYPE(100),
     * HRORI(100),HRFIRS(100),HRLAST(100)
C
C NREGS IS NUMBER OF REGIONS.
C XRLO ETC. ARE REGION BOUNDARIES.
C HRMASK =1 (FACE 1), =2 (FACE 2), =4 (FACE 3), =8 (FACE 4), ETC.
C HRFACE = FACE NUMBER, =1-6 FOR -X,+X,-Y,+Y,-Z,+Z.
C HRTYPE =1, MU CHAMBER SENSITIVE REGION,
C        =2, CONCRETE REGION,
C        =3, IRON REGION,
C        =4, LEAD GLASS. (IRTYPE=4 USED ONLY IN MUFFLD.)
C HRORI  = ORIENTATION OF NORMAL, =1 (|| X), =2 (||Y), =3 (|| Z).
C HRFIRS = FIRST CHAMBER NUMBER.
C HRLAST = LAST CHAMBER NUMBER.
C
C------------END OF MACRO CMUREG----------------------------------------
C----------START OF MACRO CMUBCS----------------------------------------
      COMMON /BCS/IDATA(1)
      DIMENSION HDATA(1),ADATA(1)
      EQUIVALENCE (HDATA(1),ADATA(1),IDATA(1))
C----------END OF MACRO CMUBCS------------------------------------------
C
C                        --- MACRO CADMIN ---
C
      LOGICAL*1 LBREAD
C
      COMMON / CADMIN / IEVTP,NRREAD,NRWRIT,NRERR,LBREAD(4),IFLG,IERCAL,
     +                  ISMEAR,IJETCI,NFLAGS(10)
C
C                                 NFLAGS IS AN ARRAY OF GENERAL FLAGS
C                                   (1) : USED BY RDDATE
C                                   (2) : USED BY RDTRIG
C                                   (3) : USED BY RDTRIG
C                                   (4) : USED BY RDTRIG / PRSTAT
C                                   (5) : USED BY EVREAD -COUNTS RECORDS
C                                   (6) : USED BY SUPERV -COUNTS ERRORS
C                                   (7) : USED BY EVWRIT -'HEAD'LESS EVS
C                                   (8) : USED BY EVREAD/RDMTCO (EVWRIT)
C                                   (9) : USED BY RDMTCO/EVWRIT
C                                  (10) : FREE
C
C                                  BLOCK DATA SET IN MEMBER JADEBD
C
C
      COMMON /CMUPRN/ MUPRIN,MUHPR,IANAC,ICOOR,IANAF,LANAC,LCOOR,LANAF,
     +                ITOTAL,LTOTAL,ICUTS,LCUTS
      COMMON /CWORK/  CNLO(500),CNHI(500),CLLO(500),CLHI(500),
     +                CTLO(500),CTHI(500),IUNIT(500)
C
C                            TEMPORARY STORE FOR REGION LIMITS.
C
C                            DATA INITIALISATIION STATEMENTS.
C
      DATA NPLMAX/48/
C
      DATA ERROR/.FALSE./
C
      DATA EXTRA/500./
C
C------------------  C O D E  ------------------------------------------
C
      IF( IPRINT .GE.6  .AND  .IPRINT .LT. 10 )
     +      CALL MUMESS('MUREG ',0,'CALLED TO COMPUTE MUON REGIONS.^')
C
C                            CHANGE HSECT FOR HANGING ROOF LAYER.
C
      HSECT(39)=9
      HSECT(40)=9
      HSECT(41)=10
      HSECT(42)=10
C
C                            CLEAR CMUREG - CURRENTLY 901 WORDS.
C
      CALL VZERO(NREGS,901)
C
C                            HAS THE CALIBRATION BEEN READ PROPERLY?
C                            CHECK SOME OF THE KNOWN NUMBERS
C
      IF( HFR(1) .EQ. 0  .AND.  HFR(200) .EQ. 0 ) GO TO 95
C
C                            DO CHAMBER PLANES.
C
      HRMASK(1)=256
      HRFACE(1)=1
      HRTYPE(1)=1
      HRORI(1)=HNORM(1)
      IUNIT(1)=3
      HRFIRS(1)=HAC(1)
      C1=HDIST(1)+HD1(HAC(1))
      C2=HDIST(1)+HD1(HAC(1)+1)
      IF(C1.GT.C2)GO TO 1
         CNLO(1)=C1
         CNHI(1)=C2
         GO TO 2
 1    CONTINUE
         CNLO(1)=C2
         CNHI(1)=C1
 2    CONTINUE
      CLLO(1)=HCLLO(1)
      CLHI(1)=HCLHI(1)
      CTLO(1)=HCTLO(1)
C
      J=1
C
      DO 3 I=2,NFRAMS
          K=HCTLO(I)-HCTHI(I-1)
          IF ( HDIST(I).EQ.HDIST(I-1) .AND.
     *         IABS(K).LT.50 ) GO TO 3
C
              J=J+1
              IF(J.GT.NPLMAX)GO TO 98
C
                  IFACE=HFACE(I)
                  HRMASK(J)=2**(IFACE-1)
                  IF(HLAYER(I).EQ.1)HRMASK(J)=256*HRMASK(J)
                  HRFACE(J)=IFACE
                  HRTYPE(J)=1
                  HRORI(J)=HNORM(I)
                  IUNIT(J)=HUNIT(I)
                  HRFIRS(J)=HAC(I)
                  C1=HDIST(I)+HD1(HAC(I))
                  C2=HDIST(I)+HD1(HAC(I)+1)
                  IF(C1.GT.C2)GO TO 4
                      CNLO(J)=C1
                      CNHI(J)=C2
                      GO TO 5
 4                CONTINUE
                      CNLO(J)=C2
                      CNHI(J)=C1
 5                CONTINUE
                  CLLO(J)=HCLLO(I)
                  CLHI(J)=HCLHI(I)
                  CTLO(J)=HCTLO(I)
C
                  HRLAST(J-1)=HAL(I-1)
                  CTHI(J-1)=HCTHI(I-1)
C
 3    CONTINUE
C
      I=NFRAMS
      HRLAST(J)=HAL(I)
      CTHI(J)=HCTHI(I)
      NPLANS=J
C
C-----------------------------------------------------------------------
C
C                            YOKE.
      DO 20 I=1,4
          J=J+1
          HRORI(J)=1
          IF(I.GE.3)HRORI(J)=2
          HRMASK(J)=2**(I+7)
          HRTYPE(J)=3
          HRFACE(J)=I
          IUNIT(J)=3
          C1=HYKNMI(I)
          C2=HYKNMO(I)
          IF(C1.GT.C2)GO TO 21
              CNLO(J)=C1
              CNHI(J)=C2
              GO TO 22
 21       CONTINUE
              CNLO(J)=C2
              CNHI(J)=C1
 22       CONTINUE
          CTLO(J)=-HYKTDM(I)
          CTHI(J)=HYKTDM(I)
          CLLO(J)=-HYKLDM(I)
          CLHI(J)=HYKLDM(I)
          IF(I.EQ.3)GO TO 20
              CLLO(J)=CLLO(J)+10
              CLHI(J)=CLHI(J)-10
 20   CONTINUE
C
C-----------------------------------------------------------------------
C
C                            END PLUG.
      DO 30 I=1,2
          J=J+1
          HRORI(J)=3
          HRMASK(J)=2**(I+11)
          HRTYPE(J)=3
          HRFACE(J)=I+4
          IUNIT(J)=3
C***       CORRECTION IMPLEMENTED ON 22/10/83 : YOKE END PLATE THICKNESS
C***       SET TO 340 MM .  SEE MUFFLY.
          CNLO(J)=-(IZEIO + 340 )
          CNHI(J)=-IZEIO
C***      CNLO(J)=-IZOEP5
C***      CNHI(J)=-IZOEP1
C
          IF(I.EQ.1)GO TO 31
C
              CNLO(J)=IZEIO
              CNHI(J)=IZEIO + 340
C***          CNLO(J)=IZOEP1
C***          CNHI(J)=IZOEP5
 31       CONTINUE
C
C              CORRECTION 23/11/83 , HMCC :
C              THE YOKE END-PLATES ARE COMPLETELY CONTAINED WITHIN
C              THE SIDE- , FLOOR- AND ROOF-PLATES IN X AND Y .
C              ( I DOUBT IF ANYONE WILL EVER NOTICE ANY EFFECT OF THIS!)
C              TO BE EVEN MORE PEDANTIC , THE Y.E.P.S ACTUALLY HAVE A
C              FLANGE AT +Y WHICH GOES A COUPLE OF CMS HIGHER THAN 1759
C              MM , BUT IGNORE THAT.
          CTLO(J)=-1759
          CTHI(J)= 1759
          CLLO(J)=-1759
          CLHI(J)= 1759
C***      CTLO(J)=HYKNMO(1)
C***      CTHI(J)=HYKNMO(2)
C***      CLLO(J)=HYKNMI(3)
C***      CLHI(J)=HYKNMO(4)
 30   CONTINUE
C
C-----------------------------------------------------------------------
C
C                            ABSORBER - FACES 1-3.
      DO 40 IFACE=1,3
          DO 40 IBLK=1,3
              J=J+1
              HRMASK(J)=2**(IFACE-1)
              HRFACE(J)=IFACE
              HRTYPE(J)=2
              IF(IFACE.EQ.3)HRTYPE(J)=3
              HRORI(J)=(IFACE+1)/2
              IUNIT(J)=IFACE
              K=6*(IFACE-1)+2*(IBLK-1)+1
              C1=HBNLIM(K)
              C2=HBNLIM(K+1)
              IF(C1.GT.C2)GO TO 41
                  CNLO(J)=C1
                  CNHI(J)=C2
                  GO TO 42
 41           CONTINUE
                  CNLO(J)=C2
                  CNHI(J)=C1
 42           CONTINUE
              CTLO(J)=HBTLO(IFACE)
              CTHI(J)=HBTHI(IFACE)
              CLLO(J)=HBLLO(IFACE)
              CLHI(J)=HBLHI(IFACE)
 40   CONTINUE
C
C                            ABSORBER - FACES 4-6.
      DO 50 IFACE=4,6
          DO 50 IBLK=1,3
              DO 50 ISIDE=1,2
                  J=J+1
                  HRMASK(J)=2**(IFACE-1)
                  HRFACE(J)=IFACE
                  HRTYPE(J)=2
                  HRORI(J)=(IFACE+1)/2
                  IUNIT(J)=ISIDE+3
                  K=6*(IFACE-1)+2*(IBLK-1)+1
                  C1=HBNLIM(K)
                  C2=HBNLIM(K+1)
                  IF(C1.GT.C2)GO TO 51
                      CNLO(J)=C1
                      CNHI(J)=C2
                      GO TO 52
 51               CONTINUE
                      CNLO(J)=C2
                      CNHI(J)=C1
 52               CONTINUE
                  IGAP=50.
                  IF(IFACE.GE.5)IGAP=350.
                  IF(ISIDE.EQ.2)GO TO 53
                      CTLO(J)=HBTLO(IFACE)
                      CTHI(J)=-IGAP
                      GO TO 54
 53               CONTINUE
                      CTLO(J)=IGAP
                      CTHI(J)=HBTHI(IFACE)
 54               CONTINUE
                  CLLO(J)=HBLLO(IFACE)
                  CLHI(J)=HBLHI(IFACE)
 50   CONTINUE
C
C-----------------------------------------------------------------------
C
C                            REMAINING PIECES - (OVERHANGING ARCH).
      DO 60 IFACE=5,6
          DO 60 IBLK=1,3
              DO 60 ISIDE=1,2
                  J=J+1
                  HRMASK(J)=2**(IFACE-1)
                  HRFACE(J)=IFACE
                  HRTYPE(J)=2
                  HRORI(J)=(IFACE+1)/2
                  IUNIT(J)=ISIDE+3
                  K=6*(IFACE-1)+2*(IBLK-1)+1
                  C1=HBNLIM(K)
                  C2=HBNLIM(K+1)
                  IF(C1.GT.C2)GO TO 61
                      CNLO(J)=C1
                      CNHI(J)=C2
                      GO TO 62
 61               CONTINUE
                      CNLO(J)=C2
                      CNHI(J)=C1
 62               CONTINUE
                  IF(ISIDE.EQ.2)GO TO 63
                      CTLO(J)=-350.
                      CTHI(J)=-50.
                      GO TO 64
 63               CONTINUE
                      CTLO(J)=50.
                      CTHI(J)=350.
 64               CONTINUE
                  CLLO(J)=350.
                  CLHI(J)=HBLHI(IFACE)
 60   CONTINUE
C
C-----------------------------------------------------------------------
C
      NREGS=J
C
C                            CHANGE FROM N,L,T TO X,Y,Z.
C                            (EXTEND CHAMBER REGIONS,I.E. I .LT. NPLANS)
      E=EXTRA
      DO 6 I=1,NREGS
          IF(I.GT.NPLANS)E=0.
          IOR=HRORI(I)
          GO TO (7,8,9),IOR
 7        CONTINUE
              XRLO(I)=CNLO(I)
              XRHI(I)=CNHI(I)
              YRLO(I)=CTLO(I)-E
              YRHI(I)=CTHI(I)+E
              ZRLO(I)=CLLO(I)-E
              ZRHI(I)=CLHI(I)+E
              GO TO 6
 8        CONTINUE
              XRLO(I)=CTLO(I)-E
              XRHI(I)=CTHI(I)+E
              YRLO(I)=CNLO(I)
              YRHI(I)=CNHI(I)
              ZRLO(I)=CLLO(I)-E
              ZRHI(I)=CLHI(I)+E
              GO TO 6
 9        CONTINUE
              XRLO(I)=CTLO(I)-E
              XRHI(I)=CTHI(I)+E
              YRLO(I)=CLLO(I)-E
              YRHI(I)=CLHI(I)+E
              ZRLO(I)=CNLO(I)
              ZRHI(I)=CNHI(I)
 6    CONTINUE
C
C-----------------------------------------------------------------------
C
C                            MAKE OVERALL UNIT TRANSLATIONS.
      DO 70 I=1,NREGS
          XRLO(I)=XRLO(I)+HOVALL(IUNIT(I))
          XRHI(I)=XRHI(I)+HOVALL(IUNIT(I))
 70   CONTINUE
C
C-----------------------------------------------------------------------
C
C                            PRINT OUT CONSTANTS.
 10   CONTINUE
      IF(IPRINT.LT.10)GO TO 99
      WRITE(6,100)NREGS,
     * (I,XRLO(I),XRHI(I),YRLO(I),YRHI(I),ZRLO(I),ZRHI(I),
     * HRMASK(I),HRFACE(I),HRTYPE(I),HRORI(I),HRFIRS(I),HRLAST(I),
     * I=1,NREGS)
 100  FORMAT('1MUREG - ABSTRACTS REGIONS AS FOLLOWS...'/
     *'0',I5,' REGIONS'/
     *'0     REGION  XRLO      XRHI      YRLO      YRHI      ZRLO',
     *'      ZRHI    HRMASK    HRFACE    HRTYPE      HRORI    HRFIRS',
     *'    HRLAST'/
     *(I10,6F10.2,Z10,5I10))
      GO TO 99
C
C-----------------------------------------------------------------------
C
C                            ERROR CONDITIONS.
C
 98   IF( MUPRIN .EQ. 0 ) MUPRIN = 2
      CALL MUERRY('MUREG ',NPLMAX,'TOO MANY MU PLANES. ++ STOP ++^')
      ERROR = .TRUE.
      GO TO 10
C
C                            NO MUON CALIBRATION IS ONLY AN ERROR FOR
C                            REAL DATA. CHECK EVENT TYPE BUT BE SURE
C
 95   IF( IEVTP .GT. 0 ) GO TO 99
      IPHEAD = IDATA(IBLN('HEAD'))
      IF( IPHEAD .LE. 0 ) GO TO 99
      KRUN   = HDATA( 2*IPHEAD + 10 )
      IF( KRUN .LT. 100 ) GO TO 99
      IF( MUPRIN .EQ. 0 ) MUPRIN = 2
      CALL MUERRY('MUREG ',0,' NO MUON CALIBRATION FOUND. ++ STOP ++^')
      ERROR = .TRUE.
C
C-----------------------------------------------------------------------
C
 99   IF( .NOT. ERROR ) RETURN
      CALL BSTA
      STOP 333
      END
