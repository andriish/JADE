C   10/05/82 811282010  MEMBER NAME  RDTRG1   (JADEGS)      FORTRAN
      SUBROUTINE RDTRG1
C-----------------------------------------------------------
C  VERSION OF 31/07/79  LAST MOD 15/11/81    E.ELSEN
C      MODIFIED TO CONFORM WITH RDTRIG  10.8.83   J.OLSSON
C
C  EXTENSION AND MODIFICATION 7/86
C
C   LAST CHANGE 08.6.87
C
C  NOTE!!  CHANGES IN RDTRG1 OR RDTRG2 NORMALLY SHOULD BE ACCOMPANIED
C          BY CHANGES IN PRSTAT !!
C
C  FILL TRIG 1 BANK ACCORDING TO JADE COMPUTER NOTE 23 A, JN32:SUPPL.5
C  FILL LATC 0 BANK ACCORDING TO JADE NOTE 32
C-----------------------------------------------------------
C
      IMPLICIT INTEGER*2 (H)
C
      COMMON / BCS / IW(1)
      DIMENSION HW(1)
      EQUIVALENCE ( HW(1), IW(1) )
C
      COMMON / CWORK / HBP(24), HTOF(42), IRES(8), NLGBR(42), NLGQ(8),
     +                 NTAG(2)
C
C MASK IS BLOCK DATA SET IN RDTRIG
      COMMON /CMASK/ MASK(16),MASK4,MASK3,MASK2,MASK1,HKRE(3,8)
C
C
      COMMON /CTRIGG/ IHIST(3),NBPTFW,TAGADC,IDUM1(4),HDUM1,
     +  HLGBRT,HLGBST,HLGQT,HLGTOT(4,2),HECAPT(4),HLGTL,HLGTH,HTGTRG,
     +                HDUM2(9),IWIDBS,NRNBSL,NRNBSH,NTOFBS,IDUM2(10),
     +                NTOFC,NTOFC1,NTOFC2,NTBGC,NTBGC1,NTBGC2,
     +                IWCOLL,IWCOLN,IWMPRG,HFMPRL,HFMPRH,HWCOLN,HMPRON,
     +                IWCTBG,NTFCOL,NTFCLN,NTFBS2,IDUM3(9),
     +  HITCLL(3),HITWLL(3),HITSUM(3),HCHAMB(3),HMASK(16,3),HDEADC(10),
     +  HACC1,HACC2,HACC3,HACC4,HACCM,HDUM6,IWIDT2,HACCB1,HACCB2,
     +  HT1ACC,HT1PSP,IDUM4(8)
C                                           BIT PACKING FUNCTION
      IBPAT(NUM) = MAX0( MIN0(NUM,7)*2, 1 )
C
      IYEAR = IHIST(3)
      IYEAR = MOD(IYEAR,100)
      IF(IYEAR.LT.82) GO TO 3231
C FOR 1982 AND LATER, CALL SUBR. RD82T1
      CALL RD82T1
      RETURN
C
C     >>>>>>>>>>   TRIGGER SIMULATION FOR 1979-1981  <<<<<<<<<<
C
3231  CONTINUE
C                                           CREATE TRIG, 1
      CALL CCRE( NPTRIG, 'TRIG', 1, 5, IER )
      IF( IER .EQ. 2 ) GO TO 8000
      CALL BSAW( 1, 'TRIG' )
      NTRIG2 = NPTRIG * 2
      IW(NPTRIG+1) = LOR(IW(NPTRIG+1),MASK1)
C
C                                           CREATE LATC, 0
      CALL BDLS( 'LATC', 0 )
      CALL CCRE( NPLATC, 'LATC', 0, 14, IER )
      IF( IER .EQ. 2 ) GO TO 8100
      CALL BSAW( 1, 'LATC' )
      NLATC2 = NPLATC * 2
      IW(NPLATC+1) = LOR(IW(NPLATC+1),MASK1)
C                                           BP COUNTER MULTIPLICITY
      NBP = 0
      NPATBP = IW(IBLN('ATBP'))
      IF( NPATBP .EQ. 0 ) GO TO 200
      NATBP2 = NPATBP * 2
      DO 100 IBP = 1,24
      HBP(IBP) = 0
      J = IBP + (IBP-1)/8 + 54
      IF( HW(NATBP2+J) .GE. 2048 ) GO TO 100
      NBP = NBP + 1
      HBP(IBP) = 1
C                                           LATCHES
      NW = ( IBP - 1 ) / 8
      IBT = IBP - NW*8
      HW(NLATC2+NW+3) = hLOR( HW(NLATC2+NW+3), hint( MASK(IBT) ) ) ! PMF 10/06/99: hlor, hint
  100 CONTINUE
C                                NTOF,      TOF COUNTER MULTIPLICITY
  200 NTOF = 0
      NPATOF = IW(IBLN('ATOF'))
      IF( NPATOF .EQ. 0 ) GO TO 400
      NATOF2 = NPATOF * 2
      DO 300 ITOF = 1,42
      HTOF(ITOF) = 0
      J = ITOF*2 + (ITOF-1)/4 + 92
      IF( (HW(NATOF2+J)+HW(NATOF2+J+1)) .GE. 4096 ) GO TO 300
      NTOF = NTOF + 1
      HTOF(ITOF) = 1
C                                           LATCHES
      NW = ( ITOF - 1 ) / 7
      IBT = ITOF - NW*7
      HW(NLATC2+NW+6) = hLOR( HW(NLATC2+NW+6), hint( MASK(IBT) ) ) ! PMF 10/06/99: hlor, hint
  300 CONTINUE
C
C                                           TOF AND BP MATRIX
C                                           IRES BUFFERS RESULT
  400 NBPM = 0
      NTOFM = 0
      CALL VZERO( IRES, 8 )
      IF( NTOF .LE. 0 ) GO TO 600
      IF( NBP .LE. 0 ) GO TO 600
      DO 510 ITOF = 1,42
      IF( HTOF(ITOF) .EQ. 0 ) GO TO 510
      JC = 4*ITOF + 165
      JL = ( JC - NBPTFW -1 ) / 7 + 1
      JH = ( JC + NBPTFW ) / 7
      DO 500 J = JL, JH
      IBP = MOD( J - 1, 24 ) + 1
      IF( HBP(IBP) .EQ. 0 ) GO TO 500
C                                           BP IN IRES(7-8), 2 BYTES USE
      IWORD = ( IBP - 1 ) / 16 + 7
      IBIT = IBP - ( IWORD - 7 ) * 16
      IRES(IWORD) = LOR( IRES(IWORD), MASK(IBIT) )
C
      IF( HBP(IBP) .EQ. 1 ) NBPM = NBPM + 1
      HBP(IBP) = 10
C                                           TOF IN IRES(1-6), 1 BYTE USE
      IF( HTOF(ITOF) .NE. 1 ) GO TO 500
      IWORD = ( ITOF-1 ) / 7 + 1
      IBIT = ITOF - ( IWORD - 1 ) * 7
      IRES(IWORD) = LOR( IRES(IWORD), MASK(IBIT) )
      NTOFM = NTOFM + 1
      HTOF(ITOF) = 10
  500 CONTINUE
  510 CONTINUE
C
C                                           LOAD FIRST HALF OF TRIG, 1
  600 HW(NTRIG2+3) = IBPAT(NTOFM) * 16 + IBPAT(NBPM)
      HW(NTRIG2+3) = HW(NTRIG2+3)*256 + IBPAT(NTOF) * 16 + IBPAT(NBP)
      HW(NTRIG2+4) = IRES(2)*256 + IRES(1)
      HW(NTRIG2+5) = IRES(4)*256 + IRES(3)
      HW(NTRIG2+6) = IRES(6)*256 + IRES(5)
      HW(NTRIG2+7) = IRES(7)
      HW(NTRIG2+8) = IRES(8)
C
C                                           TRIGGER CONDITIONS
C                                           LEAD GLASS ENERGY
C                                           ASSUMING NUMBERING 0-31 ETC
C
C   LGNE IS TOTAL ENERGY
C
C
 1000 IETOT = 0
      CALL VZERO( NLGBR, 50 )
      NLGE = 0
      NPALGN = IW( IBLN('ALGN' ) )
      IF( NPALGN .EQ. 0 ) GO TO 1200
      NALGN2 = NPALGN*2
      IL = NALGN2 + 6 + HW(NALGN2+3)
      IH = NALGN2 + 4 + HW(NALGN2+6)
      IF( IH .LT. IL ) GO TO 1200
      DO 1100 J=IL,IH, 2
      NBLO = HW(J)
      NAMP = HW(J+1)
      NLGE = NLGE + NAMP
      IF( NBLO .GT. 2687 ) GO TO 1060
      NBLO = NBLO - 32
      IF( NBLO .LT. 0 ) NBLO = NBLO +2688
      KL1 = NBLO / 64 + 1
      KL2 = KL1 + 1
      KL3 = KL2 + 1
      IF( KL2 .GT. 42 ) KL2 = KL2 - 42
      IF( KL3 .GT. 42 ) KL3 = KL3 - 42
      NLGBR(KL1) = NLGBR(KL1) + NAMP
      NLGBR(KL2) = NLGBR(KL2) + NAMP
      NLGBR(KL3) = NLGBR(KL3) + NAMP
      GO TO 1100
C                                           END CAP
 1060 KL = ( NBLO-2688 ) / 24 + 1
      NLGQ(KL) = NLGQ(KL) + NAMP
C
 1100 IETOT = IETOT + NAMP
C
C                                           LG-ROW LATCHES
      DO 1110 J=1,42
      IF( NLGBR(J) .LE. HLGBRT ) GO TO 1110
      NW = (J-1)/7
      IBT = J-NW*7
      HW(NLATC2+NW+12) = hLOR( HW(NLATC2+NW+12), hint( MASK(IBT) ) ) ! PMF 10/06/99: hlor, hint
 1110 CONTINUE
C                                           LG-END CAP LATCHES
      DO 1120 J=1,8
      IF( NLGQ(J) .LE. HLGQT  ) GO TO 1120
      HW(NLATC2+18) = hLOR( HW(NLATC2+18), hint( MASK(J) ) ) ! PMF 10/06/99: hlor, hint
 1120 CONTINUE
C                                           TOTAL ENERGY LATCHES
C    4 LOWER BITS FOR THE VARIOUS THRESHOLDS, BIT 4 IS THE HIGHEST
C
      DO 1130 J=1,4
      IF( NLGE .LE. HLGTOT(J,1))  GO TO 1130
      HW(NLATC2+23) = hLOR( HW(NLATC2+23), hint( MASK(J) )) ! PMF 10/06/99: hlor, hint
 1130 CONTINUE
C
C                                           COLLINEARITY TRIGGER
 1200 ICOLL = 0
      IF( NTOF .EQ. 0 ) GO TO 1270
      DO 1260 J = 1,42
      IF( HTOF(J) .EQ. 0 ) GO TO 1260
      JLO = J + 21 - IWCOLL
      JHI = J + 21 + IWCOLL
      DO 1250 JOP = JLO,JHI
      I = MOD( JOP - 1, 42 ) + 1
      IF( HTOF(I) .EQ. 0 ) GO TO 1250
      ICOLL = 1
      GO TO 1270
 1250 CONTINUE
 1260 CONTINUE
 1270 CONTINUE
C
C                                           NARROW COLLINEARITY
 1300 ICOLLN = 0
      IF( NTOF .EQ. 0 ) GO TO 1370
C     IF( HWCOLN .NE. 1 ) GO TO 1370
      IF(IHIST(3).LT.1981) GO TO 1370
      DO 1360 J = 1,42
      IF( HTOF(J) .EQ. 0 ) GO TO 1360
      JLO = J + 21 - IWCOLN
      JHI = J + 21 + IWCOLN
      DO 1350 JOP = JLO,JHI
      I = MOD( JOP - 1, 42 ) + 1
      IF( HTOF(I) .EQ. 0 ) GO TO 1350
      ICOLLN = 1
      GO TO 1370
 1350 CONTINUE
 1360 CONTINUE
 1370 CONTINUE
C
C                                           MULTIPRONG TRIGGER
      IMPRG = 0
      IF( NTOF .EQ. 0 ) GO TO 1800
      IF( HMPRON .NE. 1 ) GO TO 1800
      DO 1700 J = 1,42
      IF( HTOF(J) .EQ. 0 ) GO TO 1700
      JLO = J + 21 - IWMPRG
      JHI = J + 21 + IWMPRG
      DO 1600 JOP = JLO,JHI
      I = MOD( JOP - 1, 42 ) + 1
      IF( HTOF(I) .EQ. 0 ) GO TO 1600
      IMPRG = 1
      GO TO 1800
 1600 CONTINUE
 1700 CONTINUE
 1800 CONTINUE
C
C                                            TAGGING LATCHES
C   THE TAGG BANK CONTAINS ADC COUNTS, MATCH AGAINST THRESHOLDS BY
C   USE OF THE AVERAGE CONVERSION CONSTANT  TAGADC
C   IN 1979-1981, EITHER MARK 1 OR MARK 2 TAGGER, NO BLOCKS EXCLUDED
C    WHEN SETTING THE TRIGGER
C
      ITAG = 0
      NTAG(1) = 0
      NTAG(2) = 0
      NPATAG = IW(IBLN('ATAG'))
      NATAG2 = NPATAG * 2
      IF( NATAG2 .LE. 0 ) GO TO 3000
      NBAS = NATAG2 + 7
      DO 2400 J=1,2
      NH = HW(NATAG2+J+3) - 2
      NL = HW(NATAG2+J+2)
      IF( NL .GT. NH ) GO TO 2300
      DO 2200 I=NL, NH, 2
      NAMP = HW(NBAS+I)
      NTAG(J) = NTAG(J) + NAMP
 2200 CONTINUE
 2300 CONTINUE
      NTAG(J) = NTAG(J) * TAGADC
 2400 CONTINUE
C                                           THRESHOLDS
      IF( NTAG(1) .LT. HLGTL ) GO TO 2410
      ITAG = ITAG + 1
      HW(NLATC2+20) = hLOR( HW(NLATC2+20), hint(MASK(13)) ) ! PMF 10/06/99: hlor,hint
      IF( NTAG(1) .GT. HLGTH )
     +     HW(NLATC2+20) = hLOR( HW(NLATC2+20), hint( MASK(14) ) ) ! PMF 10/06/99: hlor,hint
 2410 IF( NTAG(2) .LT. HLGTL ) GO TO 3000
      ITAG = ITAG + 1
      HW(NLATC2+20) = hLOR( HW(NLATC2+20), hint( MASK(15) ) ) ! PMF 10/06/99: hlor,hint
      IF( NTAG(2) .GT. HLGTH )
     +     HW(NLATC2+20) = hLOR( HW(NLATC2+20), hint( MASK(16) ) ) ! PMF 10/06/99: hlor,hint
C  LUMI BIT
      IF( NTAG(1) .GT. HLGTH.AND.NTAG(2).GT.HLGTH )
     +                        HW(NLATC2+20) = HW(NLATC2+20) + 64
 3000 CONTINUE
C
C ------------------------------------  T1- ACCEPT CONDITIONS
C
      IACC = 0
C
C                     LUMI-TRIGGER
C
      IF( NTAG(1) .GT. HLGTH.AND.NTAG(2).GT.HLGTH )
     $ IACC = LOR( IACC, MASK(1) )
C
C                     TOTAL ENERGY TRIGGER
C
      IF(IETOT .GT.HLGTOT(1,1)) IACC = LOR( IACC, MASK(2) )
C
C                     TAG+LG ENERGY TRIGGER
C
      IF(ITAG.GT.0.AND.IETOT.GT.HLGTOT(2,1)) IACC=LOR(IACC,MASK(3))
C
C ------------------------------------- T1- POSTPONE CONDITIONS
C
      IPOST = 0
C
C                 TOF + LG ENERGY
C
      IF(NTOF.GE.NTOFC.AND.IETOT.GE.HLGTOT(3,1))IPOST=LOR(IPOST,MASK(1))
C
C                 TAGG
C
      IF( ITAG .GT. 0 ) IPOST = LOR( IPOST, MASK(2) )
C
C                 COLLINEAR TOF, WIDE
C
      IF(ICOLL.EQ.1 .AND. NTOF.LT.NTFCOL) IPOST = LOR( IPOST, MASK(5) )
C
C                 COLLINEAR TOF, NARROW
C
      IF(ICOLLN.EQ.1 .AND. NTOF.LT.NTFCLN) IPOST = LOR(IPOST,MASK(6))
C
C                 COLLINEAR TOF, WIDE MULTIPRONG
C
      IF(IMPRG.EQ.1 .AND. NTOF.LE.HFMPRH .AND. NTOF.GE.HFMPRL )
     +                                    IPOST = LOR( IPOST, MASK(7) )
C -------------------------------------------------
C                                           FINAL TRIGGER WORDS
C
      HW(NTRIG2+10) = IACC*256 + IPOST
      HW(NTRIG2+ 1) = 0
      HW(NTRIG2+ 2) = 0
      RETURN
C
C                                           NO SPACE IN BCS LEFT
 8000 WRITE(6,9101)
 9101 FORMAT(/' NOT ENOUGH SPACE IN BCS LEFT FOR CREATION OF TRIG, 1'/)
      RETURN
 8100 WRITE(6,9102)
 9102 FORMAT(/' NOT ENOUGH SPACE IN BCS LEFT FOR CREATION OF LATC, 0'/)
      RETURN
      END
      SUBROUTINE RD82T1
C-----------------------------------------------------------
C  VERSION OF 31/07/79  LAST MOD 17/05/82    J.OLSSON
C        NTBG -- NTOFBG BUG CORRECTED 27.2.1984   J.OLSSON
C   SETTING OF KREHBIEL TRIGGER, BUG CORRECTED 8.6.1987  J.OLSSON
C  FILL TRIG 1 BANK ACCORDING TO JADE NOTE 82
C  FILL LATC 0 BANK ACCORDING TO JADE NOTE 82, JADE NOTE 32:4
C-----------------------------------------------------------
C
      IMPLICIT INTEGER*2 (H)
      COMMON / BCS / IW(1)
      DIMENSION HW(1),HELP(2)
      EQUIVALENCE ( HW(1), IW(1) ),(ICAMWD,HELP(1))
      COMMON /CMASK/ MASK(16),MASK4,MASK3,MASK2,MASK1,HKRE(3,8)
C
      COMMON / CWORK / HTOF(42),NLGBR(42),NLGQ(8),NTAG(2),HTBG(42),
     +                 NBG(42),NBS(7),HBG(42),NECA(2),NETOT(2),
     +                 NQARR(8),NBARR(7)
C
C  HTOF:  TOF COUNTER LATCHES
C  NLGBR: ENERGY OF BARREL SUB-GROUPS, IMMEDIATELY BEHIND TOF-COUNTER
C  NLGQ:  ENERGY OF END CAP QUADRANTS
C  NTAG:  ENERGY OF TAGGING END CAPS
C  HTBG:  LATCHES FOR TBG
C  NBG:   ENERGY OF BARREL GROUPS  (SUMS OF 3 NLGBR ELEMENTS)
C  NBS:   ENERGY OF LEAD GLASS SEPTANTS
C  HBG:   LATCHES FOR BARREL GROUPS
C  NECA:  ENERGY OF END CAPS
C  NETOT: TOTAL ENERGY AND BARREL ENERGY
C  NQARR: LATCHES FOR END CAP QUADRANTS
C  NBARR: LATCHES FOR SEPTANTS
C
      COMMON /CTRIGG/ IHIST(3),NBPTFW,TAGADC,IDUM1(4),HDUM1,
     +  HLGBRT,HLGBST,HLGQT,HLGTOT(4,2),HECAPT(4),HLGTL,HLGTH,HTGTRG,
     +                HDUM2(9),IWIDBS,NRNBSL,NRNBSH,NTOFBS,IDUM2(10),
     +                NTOFC,NTOFC1,NTOFC2,NTBGC,NTBGC1,NTBGC2,
     +                IWCOLL,IWCOLN,IWMPRG,HFMPRL,HFMPRH,HWCOLN,HMPRON,
     +                IWCTBG,NTFCOL,NTFCLN,NTFBS2,IDUM3(9),
     +  HITCLL(3),HITWLL(3),HITSUM(3),HCHAMB(3),HMASK(16,3),HDEADC(10),
     +  HACC1,HACC2,HACC3,HACC4,HACCM,HDUM6,IWIDT2,HACCB1,HACCB2,
     +  HT1ACC,HT1PSP,IDUM4(8)
C
      DATA HELP/0,0/
C
      IBPAT(NUM) = MAX0( MIN0(NUM,7)*2, 1 )
C                                           BIT PACKING FUNCTION
C                        IBPAT OUTPUT:     NUM = 0 -->  IBPAT = 1
C                                          NUM = 1-7 --> IBPAT = 2*NUM
C                                          NUM > 7 --> IBPAT = 2*7
C
C                                           CREATE TRIG, 1
      LTRIGL = 5
      IF(IHIST(3).GT.1984) LTRIGL = 6
C
      CALL CCRE( NPTRIG, 'TRIG', 1, LTRIGL, IER )
      IF( IER .EQ. 2 ) GO TO 8000
      CALL BSAW( 1, 'TRIG' )
      NTRIG2 = NPTRIG * 2
      IF(IHIST(3).LT.1985) IW(NPTRIG+1) = LOR(IW(NPTRIG+1),MASK2)
      IF(IHIST(3).GT.1984) IW(NPTRIG+1) = LOR(IW(NPTRIG+1),MASK4)
C
C                                           CREATE LATC, 0
      CALL BDLS( 'LATC', 0 )
      CALL CCRE( NPLATC, 'LATC', 0, 14, IER )
      IF( IER .EQ. 2 ) GO TO 8100
      CALL BSAW( 1, 'LATC' )
      NLATC2 = NPLATC * 2
C BANK DESCRIPTOR 2
      IW(NPLATC+1) = LOR(IW(NPLATC+1),MASK2)
C                                           ****************************
C                                           *   SCINTILLATOR LATCHES   *
C                                           ****************************
      IF(IHIST(3).GT.1983) GO TO 200
C                                           BP COUNTER LATCHES
      NPATBP = IW(IBLN('ATBP'))
      IF( NPATBP .EQ. 0 ) GO TO 200
      NATBP2 = NPATBP * 2
      DO 100 IBP = 1,24
      J = IBP + (IBP-1)/8 + 54
      IF( HW(NATBP2+J) .GE. 2048 ) GO TO 100
      NW = ( IBP - 1 ) / 8
      IBT = IBP - NW*8
      HW(NLATC2+NW+3) = hLOR( HW(NLATC2+NW+3), hint( MASK(IBT) ) ) ! PMF 10/06/99: hlor,hint 
  100 CONTINUE
C                                NTOF,      TOF COUNTER MULTIPLICITY
C                                           LATCHES
  200 NTOF = 0
      NPATOF = IW(IBLN('ATOF'))
      IF( NPATOF .EQ. 0 ) GO TO 400
      NATOF2 = NPATOF * 2
      DO 300 ITOF = 1,42
      HTOF(ITOF) = 0
      J = ITOF*2 + (ITOF-1)/4 + 92
      IF( (HW(NATOF2+J)+HW(NATOF2+J+1)) .GE. 4096 ) GO TO 300
      NTOF = NTOF + 1
      HTOF(ITOF) = 1
      NW = ( ITOF - 1 ) / 7
      IBT = ITOF - NW*7
      HW(NLATC2+NW+6) = hLOR( HW(NLATC2+NW+6), hint( MASK(IBT) ) ) ! PMF 10/06/99: hlor,hint 
  300 CONTINUE
C
 400  CONTINUE
C                                           ****************************
C                                           *     LEAD GLASS ARRAYS    *
C                                           ****************************
C
C    FILL LEAD GLASS ARRAYS:  NLGBR FOR BARREL ROWS
C                             NBG FOR BARREL GROUPS
C                             NBS FOR BARREL SEPTANCES
C                             NLGQ FOR ENDCAP QUADRANTS
C                NUMBERING 0-31 ETC  FOR LEAD GLASS BLOCKS
C
C       I           I        I-----------I
C       I           I        I           I
C       I           IBRG NR 2I-----------I
C       I           I        I           I
C       I           I        I-----------I  I
C       I           I        I           I  I
C       I BG NR 1   IBRG NR 1I-----------I  I FI = 0   TOF NR 1
C       I           I        I           I  I
C       I           I        I-----------I  I
C       I           I        I           I
C       I           IBRG NR42I-----------I
C       I           I        I           I
C       I           I        I-----------I
C
 1000 CONTINUE
      CALL VZERO( NLGBR,147 )
      NPALGN = IW( IBLN('ALGN' ) )
      IF( NPALGN .EQ. 0 ) GO TO 1200
      NALGN2 = NPALGN*2
      IL = NALGN2 + 6 + HW(NALGN2+3)
      IH = NALGN2 + 4 + HW(NALGN2+6)
      IF( IH .LT. IL ) GO TO 1200
      DO 1100 J=IL,IH, 2
      NBLO = HW(J)
      NAMP = HW(J+1)
      NETOT(1) = NETOT(1) + NAMP
      IF( NBLO .GT. 2687 ) GO TO 1060
C                            BARREL ROWS
      NETOT(2) = NETOT(2) + NAMP
      NBLO = NBLO + 32
      IF( NBLO.GT.2687) NBLO = NBLO -2688
      KL1 = NBLO / 64 + 1
      NLGBR(KL1) = NLGBR(KL1) + NAMP
      GO TO 1100
C                                           END CAP QUADRANTS
 1060 KL = ( NBLO-2688 ) / 24 + 1
      NLGQ(KL) = NLGQ(KL) + NAMP
 1100 CONTINUE
C
 1200 CONTINUE
      DO 1101  I = 1,42
      HTBG(I) = 0
      HBG(I) = 0
      NBG(I) = 0
      J = I-1
      K = I+1
      IF(J.LE.0) J = 42
      IF(K.GT.42) K = 1
      NBG(I) = NLGBR(I) + NLGBR(J) + NLGBR(K)
      IF(NBG(I).GE.HLGBRT) HBG(I) = 1
      IF(HBG(I).GT.0.AND.HTOF(I).GT.0) HTBG(I) = 1
1101  CONTINUE
C                           SEPTANTS
      DO 7  J = 1,7
      NBS(J) = 0
      DO 7  I = 1,6
      K = (J-1)*6 + I - 3
      IF(K.LE.0) K = K + 42
      NBS(J) = NBS(J) + NLGBR(K)
7     CONTINUE
C                                           ****************************
C                                           *   LEAD GLASS LATCHES     *
C                                           ****************************
C
C             SET LATCHES FOR BARREL GROUPS, NBG
C
      J1 = NLATC2 + 14
      J2 = NLATC2 + 19
      DO 32 JK = J1,J2
      HELP(2) = 0
      IE = (JK-J1)*7
      DO 33 IBT = 1,7
      IF = IE + IBT
      IF(HBG(IF).NE.1) GO TO 33
      ICAMWD = LOR(ICAMWD,MASK(IBT))
33    CONTINUE
      HW(JK) = HELP(2)
32    CONTINUE
C
C             SET LATCHES FOR BARREL GROUPS WITH TOF, TBG
C
      NTOFBG = 0
      IF(NTOF.EQ.0) GO TO 1700
      J1 = NTRIG2 + 5
      J2 = NTRIG2 + 7
      DO 42 JK = J1,J2
      HELP(2) = 0
      IE = (JK-J1)*14
      DO 43 IBT = 1,14
      IF = IE + IBT
      IG = IBT
      IF(IBT.GT.7) IG = IG + 1
      IF(HTBG(IF).NE.1) GO TO 43
      NTOFBG = NTOFBG + 1
      ICAMWD = LOR(ICAMWD,MASK(IG))
43    CONTINUE
      HW(JK) = HELP(2)
42    CONTINUE
C
C             SET LATCHES FOR BARREL SEPTANTS, NBS
C    NRNBS IS TOTAL NR OF SEPTANTS SET
C
1700  J3 = NLATC2 + 20
      HELP(2) = 0
      NRNBS = 0
      DO 34 IBT = 1,7
      NBARR(IBT) = 0
      IF(NBS(IBT).LT.HLGBST) GO TO 34
      NRNBS = NRNBS + 1
      ICAMWD = LOR(ICAMWD,MASK(IBT))
      NBARR(IBT) = 1
34    CONTINUE
      HW(J3) = HELP(2)
C
C             SET LATCHES FOR ENDCAP QUADRANTS, NLGQ
C             SUM THE TWO ENDCAP TOTAL ENERGIES
C    NQLAT IS NUMBER OF QUADRANTS WITH LATCH SET
C
      NECA(1) = 0
      NECA(2) = 0
      NQLAT = 0
      J4 = NLATC2 + 21
      HELP(2) = 0
      DO 35 IBT = 1,8
      NQARR(IBT) = 0
      IC = 1
      IF(IBT.GT.4) IC = 2
      NECA(IC) = NECA(IC) + NLGQ(IBT)
      IF(NLGQ(IBT).LT.HLGQT) GO TO 35
      ICAMWD = LOR(ICAMWD,MASK(IBT))
      NQLAT = NQLAT + 1
      NQARR(IBT) = 1
35    CONTINUE
      HW(J4) = HELP(2)
C
C             SET LATCHES FOR ENDCAP ENERGIES, BARREL AND TOTAL ENERGIES
C
      J5 = NLATC2 + 22
      HELP(2) = 0
      DO 36 IBT = 1,8
      IC = 1
      IF(IBT.GT.4) IC = 2
      KC = IBT
      IF(KC.GT.4) KC = KC - 4
      IF(NECA(IC).GE.HECAPT(KC)) ICAMWD = LOR(ICAMWD,MASK(IBT))
36    CONTINUE
      HW(J5) = HELP(2)
      J6 = NLATC2 + 23
      HELP(2) = 0
      DO 37 IBT = 1,8
      IC = 1
      IF(IBT.GT.4) IC = 2
      KC = IBT
      IF(KC.GT.4) KC = KC - 4
      IF(NETOT(IC).GE.HLGTOT(KC,IC)) ICAMWD = LOR(ICAMWD,MASK(IBT))
37    CONTINUE
      HW(J6) = HELP(2)
C
C                         TAGGING LATCHES
C   THE TAGG BANK CONTAINS ADC COUNTS, MATCH AGAINST THRESHOLDS BY
C   USE OF THE AVERAGE CONVERSION CONSTANT  TAGADC
C   IN 1982-1986, EITHER MARK 2 OR MARK 3 TAGGER.
C   FOR 1983 TO MIDDLE 1986, INNER RING NOT INCLUDED IN TRIGGER
C
      ITAG = 0
      NTAG(1) = 0
      NTAG(2) = 0
      NPATAG = IW(IBLN('ATAG'))
      NATAG2 = NPATAG * 2
      IF( NATAG2 .LE. 0 ) GO TO 3000
      NBAS = NATAG2 + 7
      DO 2400 J=1,2
      NH = HW(NATAG2+J+3) - 2
      NL = HW(NATAG2+J+2)
      IF( NL .GT. NH ) GO TO 2300
C
      DO 2200 I=NL, NH, 2
      NBLOT = HW(NBAS+I-1)
C  INNER RING EXCLUDED IF FLAG ON
        IF(HTGTRG.EQ.1.AND.J.EQ.1.AND.NBLOT.LT.12) GO TO 2200
        IF(HTGTRG.EQ.1.AND.J.EQ.2.AND.NBLOT.LT.60) GO TO 2200
      NAMP = HW(NBAS+I)
      NTAG(J) = NTAG(J) + NAMP
 2200 CONTINUE
C
 2300 CONTINUE
      NTAG(J) = NTAG(J) * TAGADC
 2400 CONTINUE
C                                           THRESHOLDS
      IF( NTAG(1) .LT. HLGTL ) GO TO 2410
      ITAG = ITAG + 1
      HW(NLATC2+24) = hLOR( HW(NLATC2+24), hint( MASK(2) ) ) ! PMF 10/06/99: hlor,hint 
      HW(NLATC2+24) = hLOR( HW(NLATC2+24), hint( MASK(6) ) ) ! PMF 10/06/99: hlor,hint 
      HW(NLATC2+28) = hLOR( HW(NLATC2+28), hint( MASK(13)) ) ! PMF 10/06/99: hlor,hint 
      IF( NTAG(1) .GT. HLGTH )
     +     HW(NLATC2+28) = hLOR( HW(NLATC2+28), hint( MASK(14) ) ) ! PMF 10/06/99: hlor,hint 
 2410 IF( NTAG(2) .LT. HLGTL ) GO TO 3000
      ITAG = ITAG + 1
      HW(NLATC2+24) = hLOR( HW(NLATC2+24), hint( MASK(3) ) ) ! PMF 10/06/99: hlor,hint 
      HW(NLATC2+24) = hLOR( HW(NLATC2+24), hint( MASK(7) ) ) ! PMF 10/06/99: hlor,hint 
      HW(NLATC2+28) = hLOR( HW(NLATC2+28), hint( MASK(15) ) ) ! PMF 10/06/99: hlor,hint 
      IF( NTAG(2) .GT. HLGTH )
     +     HW(NLATC2+28) = hLOR( HW(NLATC2+28), hint( MASK(16) ) ) ! PMF 10/06/99: hlor,hint 
C  LUMI BIT
      IF( NTAG(1) .GT. HLGTH.AND.NTAG(2).GT.HLGTH )
     +                        HW(NLATC2+24) = HW(NLATC2+24) + 16
 3000 CONTINUE
C
C  - - - - - - - - - - - - -   END OF LATCHES - - - - - -
C
C                                           ****************************
C                                           *   MULTIPLICITIES AND     *
C                                           *   COPLANAR TOPOLOGIES    *
C                                           ****************************
C
C   DETERMINE NEIGHBOR SUPRESSED MULTIPLICITIES OF TOF AND TBG
C
      NTFNS = 0
      INS = 0
      DO 500 ITF = 15,56
      IF(INS.EQ.0) GO TO 501
      IF(ITF.EQ.56) GO TO 501
      INS = INS - 1
      GO TO 500
501   ITOF = ITF
      IF(ITOF.GT.42) ITOF = ITOF - 42
      IF(HTOF(ITOF).EQ.0) GO TO 500
      NTFNS = NTFNS + 1
      INS = 3
500   CONTINUE
      NTBGNS = 0
      INS = 0
      DO 600 ITF = 15,56
      IF(INS.EQ.0) GO TO 601
      IF(ITF.EQ.56) GO TO 601
      INS = INS - 1
      GO TO 600
601   ITOF = ITF
      IF(ITOF.GT.42) ITOF = ITOF - 42
      IF(HTBG(ITOF).EQ.0) GO TO 600
      NTBGNS = NTBGNS + 1
      INS = 3
600   CONTINUE
C
C                                  SET MULTIPLICITIES
C
      IF(NTFNS.GT.15) NTFNS = 15
      IF(NTBGNS.GT.15) NTBGNS = 15
      K1 = NTRIG2+3
      HW(K1) = IBPAT(NTOF)
      HW(K1) = HW(K1) + 16*IBPAT(NTOFBG)
      HW(K1) = HW(K1) + 256 * NTFNS
      HW(K1) = HW(K1) + 4096* NTBGNS
C
C                                  TOF:     COLLINEARITY TRIGGER
C
      ICOLL = 0
      IF( NTOF .LT. 2 ) GO TO 1270
      DO 1260 J = 1,42
      IF( HTOF(J) .EQ. 0 ) GO TO 1260
      JLO = J + 21 - IWCOLL
      JHI = J + 21 + IWCOLL
      DO 1250 JOP = JLO,JHI
      I = MOD( JOP - 1, 42 ) + 1
      IF( HTOF(I) .EQ. 0 ) GO TO 1250
      ICOLL = 1
      GO TO 1270
 1250 CONTINUE
 1260 CONTINUE
 1270 CONTINUE
C
C                               TBG:        COLLINEARITY TRIGGER
C
      ICLTBG = 0
      IF( NTOFBG .LT. 2 ) GO TO 1370
      DO 1360 J = 1,42
      IF( HTBG(J) .EQ. 0 ) GO TO 1360
      JLO = J + 21 - IWCTBG
      JHI = J + 21 + IWCTBG
      DO 1350 JOP = JLO,JHI
      I = MOD( JOP - 1, 42 ) + 1
      IF( HTBG(I) .EQ. 0 ) GO TO 1350
      ICLTBG = 1
      GO TO 1370
 1350 CONTINUE
 1360 CONTINUE
 1370 CONTINUE
C
C                                 TOF:      NARROW COLLINEARITY
C
      ICOLLN = 0
      IF( NTOF .LT. 2 ) GO TO 1470
      DO 1460 J = 1,42
      IF( HTOF(J) .EQ. 0 ) GO TO 1460
      JLO = J + 21 - IWCOLN
      JHI = J + 21 + IWCOLN
      DO 1450 JOP = JLO,JHI
      I = MOD( JOP - 1, 42 ) + 1
      IF( HTOF(I) .EQ. 0 ) GO TO 1450
      ICOLLN = 1
      GO TO 1470
 1450 CONTINUE
 1460 CONTINUE
 1470 CONTINUE
C
C                       NEUTRAL TRIGGER:    SEPTANCE COLLINEARITY
C
      IOLS = 0
      IF( NRNBS.LT.NRNBSL) GO TO 1570
      DO 1560 J = 1,7
      IF(NBS(J).LT.HLGBST) GO TO 1560
      J3 = J + IWIDBS
      IF(J3.GT.7) J3 = J3 - 7
      IF(NBS(J3).LT.HLGBST) GO TO 1560
      IOLS = 1
      GO TO 1570
 1560 CONTINUE
 1570 CONTINUE
C
C                   SEPTANT-ENDCAP QUADRANT COPLANARITY LOGIC
C
      IKREH = 0
      IF(NQLAT.LT.1.OR.NQLAT.GT.2) GO TO 1380
      IF(NRNBS.LT.1) GO TO 1380
CHECK NEIGHBOR RELATION IF 2 QUADRANTS SET
      IF(NQLAT.NE.2) GO TO 1374
C
      DO 1373  IBTQ = 1,8
      IF(NQARR(IBTQ).EQ.0) GO TO 1373
      IF(NQARR(IBTQ+1).EQ.0.AND.(IBTQ.EQ.2.OR.IBTQ.EQ.3)) GO TO 1380
      IF(NQARR(IBTQ+1).EQ.0.AND.(IBTQ.EQ.6.OR.IBTQ.EQ.7)) GO TO 1380
      IF(NQARR(IBTQ+3).EQ.0.AND.NQARR(IBTQ+1).EQ.0.AND.
     $ (IBTQ.EQ.1.OR.IBTQ.EQ.5)) GO TO 1380
      GO TO 1374
1373  CONTINUE
C
1374  CONTINUE
C
      DO 1375  IBTQ = 1,8
      IF(NQARR(IBTQ).EQ.0) GO TO 1375
       DO 1376  IBTS = 1,7
       IF(NBARR(IBTS).EQ.0) GO TO 1376
       IF(IBTS.EQ.HKRE(1,IBTQ)) IKREH = 1
       IF(IBTS.EQ.HKRE(2,IBTQ)) IKREH = 1
       IF(IBTS.EQ.HKRE(3,IBTQ)) IKREH = 1
1376   CONTINUE
1375  CONTINUE
C
1380  CONTINUE
C                                           ****************************
C                                           *       TRIGGER WORDS      *
C                                           ****************************
C
C                   >>>>>>>>>>>>>>>>            T1:    ACCEPT CONDITIONS
C
      IACC = 0
C
C                         LUMI TRIGGER
C
      IF( NTAG(1) .GT. HLGTH.AND.NTAG(2).GT.HLGTH )
     $ IACC = LOR( IACC, MASK(1) )
C
C                         TOTAL ENERGY
C
      IF(NETOT(1).GE.HLGTOT(1,1)) IACC = LOR( IACC, MASK(2) )
C
C                         TAGG + TOTAL LG ENERGY
C
      IF(ITAG.GT.0.AND.NETOT(1).GE.HLGTOT(3,1)) IACC=LOR(IACC,MASK(3))
C
C           SEPTANT - ECAP QUADRANT COPLANARITY   (KREHBIEL TRIGGER)
C
      IF(IKREH.EQ.1.AND.NTOF.LT.1.AND.IHIST(3).GT.1985)
     $                                          IACC=LOR(IACC,MASK(5))
C
C                         ECAP1 * ECAP2
C
      IF(NECA(1).GE.HECAPT(2).AND.NECA(2).GE.HECAPT(2))
     $                                 IACC=LOR(IACC,MASK(9))
C
C                         ECAP1 + ECAP2, BARREL ENERGY
C
      IF(NECA(1)+NECA(2).GE.HECAPT(3).AND.NETOT(2).GE.HLGTOT(4,2))
     $                                 IACC=LOR(IACC,MASK(10))
C
C                         TAGG + BARREL LG ENERGY
C
      IF(ITAG.GT.0.AND.NETOT(2).GE.HLGTOT(4,2))
     $                                 IACC=LOR(IACC,MASK(11))
C
C                         BARREL LG ENERGY
C
      IF(NETOT(2).GE.HLGTOT(2,2))      IACC=LOR(IACC,MASK(12))
C
C                         ECAP1 * ECAP2 * TOTAL LG ENERGY
C
      IF(NECA(1).GE.HECAPT(4).AND.NECA(2).GE.HECAPT(4).AND.NETOT(1).
     $ GE.HLGTOT(2,1))                 IACC=LOR(IACC,MASK(13))
C
C                         SEPTANT COLLINEARITY  (OLSSON 1)
C
      IF(IOLS.EQ.1.AND.NRNBS.LE.NRNBSH.AND.NTOF.LT.NTOFBS)
     $                                      IACC=LOR(IACC,MASK(14))
C
C                         SEPTANT + TAGG  (ZORN 1)
C
      IF(IHIST(3).GT.1982.AND.
     $   NRNBS.GT.0.AND.ITAG.GT.0.AND.NTOF.LT.NTOFBS)
     $                                 IACC=LOR(IACC,MASK(15))
C
      ICAMWD = IACC
      HW(NTRIG2+8) = HELP(2)
C
C                     >>>>>>>>>>>>>>         T1:     POSTPONE CONDITIONS
C
      IPOST = 0
C
C                     TOF, TOTAL ENERGY
C
      IF(NTOF.GE.NTOFC.AND.NETOT(1).GE.HLGTOT(4,1))
     $                                     IPOST = LOR(IPOST,MASK(1))
C
C                     TOF, ECAP1 + ECAP2
C  OFF IN HARDWARE DUE TO MISTAKE FROM OKT 85 TILL JULY 86
C
      IF(IHIST(3).EQ.1986.AND.IHIST(2).LT.7) GO TO 8790
      IF(NECA(1)+NECA(2).GE.HECAPT(2).AND.NTOF.GE.NTOFC1)
     $                                     IPOST = LOR(IPOST,MASK(3))
C
C                     TBGNS
C
8790  CONTINUE
      IF(NTBGNS.GE.NTBGC2.AND.NTOF.LT.NTOFC2)
     $                                     IPOST = LOR(IPOST,MASK(4))
C
C                     TBG, BARREL LG ENERGY
C
      IF(NTOFBG.GT.NTBGC1.AND.NETOT(2).GE.HLGTOT(4,2))
     $                                     IPOST = LOR(IPOST,MASK(9))
C
C                     TAGG, TBG
C
      IF(NTOFBG.GT.NTBGC .AND.ITAG.GT.0)     IPOST = LOR(IPOST,MASK(10))
C
      IF(IHIST(3).LT.1985) GO TO 8010
C
C              COLL. SEPTANT + TOF
C
      IF(IOLS.EQ.1.AND.NTOF.GT.0.AND.NTOF.LT.NTFBS2)
     $                                      IPOST=LOR(IPOST,MASK(11))
C
      IF(IHIST(3).LT.1986) GO TO 8010
C
C              TAGG + SEPTANT + TOF
C
      IF(ITAG.GT.0.AND.NRNBS.GT.0.AND.NTOF.GT.0)
     $                                      IPOST=LOR(IPOST,MASK(12))
C
8010  CONTINUE
C
C                     COLLINEAR TOF, WIDE
C
      IF(NTOF.LT.NTFCOL.AND.ICOLL.NE.0)    IPOST = LOR(IPOST,MASK(13))
C
C                     COLLINEAR TOF, NARROW
C
      IF(NTOF.LT.NTFCLN.AND.ICOLLN.NE.0)   IPOST = LOR(IPOST,MASK(14))
C
C                     COLLINEAR TBG
C
      IF(ICLTBG.NE.0)                      IPOST = LOR(IPOST,MASK(15))
C
C
      ICAMWD = IPOST
      HW(NTRIG2+9) = HELP(2)
      RETURN
C
C                                           NO SPACE IN BCS LEFT
 8000 WRITE(6,9101)
 9101 FORMAT(/' NOT ENOUGH SPACE IN BCS LEFT FOR CREATION OF TRIG, 1'/)
      RETURN
 8100 WRITE(6,9102)
 9102 FORMAT(/' NOT ENOUGH SPACE IN BCS LEFT FOR CREATION OF LATC, 0'/)
      RETURN
      END
