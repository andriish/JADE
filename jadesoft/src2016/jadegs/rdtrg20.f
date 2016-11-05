C   10/05/82 608291510  MEMBER NAME  RDTRG20  (JADEGS)      FORTRAN
      SUBROUTINE RDTRG2
C***********************************************************************
C     VERSION OF 5/3/79     AUTHOR M.HELM
C     LAST MOD 13/05/82     E.ELSEN
C     MODIFIED 8.8.83 TO CONFORM WITH RDTRG    J.OLSSON
C     COPLANARITY LOGIC FOR TRACKS CORRECTED   J.OLSSON
C        LAST CHANGE 24.8.86
C     THIS ROUTINE PERFORMS A SOFTWARE TRIGGER T2.
C     INPUT ARE THE LATCHES FOR JET CHAMBER WIRES AND TOF COUNTERS.
C     FILLS BANK 'TRIG' NO.2 AND TRIGGER ACTION WORD(22) IN BANK 'HEAD'
C******************************************************************
C
      IMPLICIT INTEGER*2 (H)
C
      COMMON /CMASK/ MASK(16),MASK4,MASK3,MASK2,MASK1
      COMMON/BCS/IDATA(1)
      DIMENSION HDATA(2),IW22(8)
      EQUIVALENCE (IDATA(1),HDATA(1))
      COMMON/CWORK/HJHIT(96),HJCTRK(192),HJTRKA(96),HJTRKF(96),ITOF(42),
     +             HWORD(24),HFPLA(16),NJTRKA,NJTRKF,
     +             IDUMMY(96)
      DIMENSION IWORD(13)
      EQUIVALENCE(IWORD(2),HWORD(1))
      DIMENSION NZERO(400)
      EQUIVALENCE(NZERO(1),HJHIT(1))
      COMMON /CTRIGG/ IHIST(3),NBPTFW,TAGADC,IDUM1(4),HDUM1,
     +  HLGBRT,HLGBST,HLGQT,HLGTOT(4,2),HECAPT(4),HLGTL,HLGTH,HTGTRG,
     +                HDUM2(9),IWIDBS,NRNBSL,NRNBSH,NTOFBS,IDUM2(10),
     +                NTOFC,NTOFC1,NTOFC2,NTBGC,NTBGC1,NTBGC2,
     +                IWCOLL,IWCOLN,IWMPRG,HFMPRL,HFMPRH,HWCOLN,HMPRON,
     +                IWCTBG,NTFCOL,NTFCLN,NTFBS2,IDUM3(9),
     +  HITCLL(3),HITWLL(3),HITSUM(3),HCHAMB(3),HMASK(16,3),HDEADC(10),
     +  HACC1,HACC2,HACC3,HACC4,HACCM,HDUM6,IWIDT2,HACCB1,HACCB2,
     +  HT1ACC,HT1PSP,IDUM4(8)
      COMMON /CFPLA/ HFT(12,4),HR1(12,4),HR2(12,4)
C
C  RETURN, WHEN BANKS 'HEAD' OR 'TRIG' NO.1 NOT EXISTING
C                   OR BANK 'TRIG' NO.2 ALREADY EXISTING
      IYEAR = IHIST(3)
      IYEAR = MOD(IYEAR,100)
      IF(IYEAR.LT.82) GO TO 3231
C FOR 1982 AND LATER, CALL SUBR. RD82T2
      CALL RD82T2
      RETURN
3231  CONTINUE
      IND0=IDATA(IBLN('HEAD'))
      CALL CLOC(IND1,'TRIG',1)
      CALL CLOC(IND2,'TRIG',2)
      IF(IND2.NE.0.OR.IND0.EQ.0.OR.IND1.EQ.0) RETURN
C
      NJTRKA=0
      NJTRKF=0
C                         ZERO SET COMMON /CWORK/
      CALL VZERO(NZERO,400)
C
C  FILL JETC HIT CELL ARRAY     HJHIT:  HIT WIRES/CELL
C
             IPJ=IDATA(IBLN('JETC'))
             IF(IPJ.EQ.0) GOTO 1000
             IL4=IPJ*2+101
             IH4=IL4+HDATA(IPJ*2+99)-4
             NWSTOR=-1
             DO 300 KWIRE=IL4,IH4,4
             NWIRE=HDATA(KWIRE)/8
             IF(NWIRE.EQ.NWSTOR) GOTO 300
             NWSTOR=NWIRE
             NWCLL=NWIRE/16+1
             HJHIT(NWCLL)=HJHIT(NWCLL)+1
  300        CONTINUE
C
C  OVERWRITE HITS PER CELL WHEN IN DEAD CELLS WIRES ARE SWITCHED ON
C
             DO 310 K=1,10
             IF(HDEADC(K).NE.0) HJHIT(HDEADC(K))=16
  310        CONTINUE
C
C  FILL ARRAY HJCTRK: HIT CELLS (ODD INDEX) AND HIT WALLS (EVEN INDEX)
C
      DO 301  IRING=1,3
      IEND=HCHAMB(IRING)
      DO 301  ICELL=1,IEND
      I=ICELL+(IRING-1)*24
      IF(HJHIT(I).GT.HITCLL(IRING)) HJCTRK(2*I-1)=1
C  I IS CELL INDEX, I1 IS NEXT CELL
      I1=I+1
      IF(ICELL.EQ.HCHAMB(IRING)) I1=I1-HCHAMB(IRING)
      IF(HJHIT(I).GT.HITWLL(IRING).AND.HJHIT(I1).GT.HITWLL(IRING).AND.
     +  (HJHIT(I)+HJHIT(I1)).GT.HITSUM(IRING)) HJCTRK(2*I)=1
  301 CONTINUE
C
C  FILL ARRAY HWORD(1-24)   IT IS LATER STORED IN    TRIG:2
C
C      KWD = 2,6,10,14,18,22    KBIT = 9-16, 1-8   RING 1,2
C      KWD = 1,5,9,13,17,21     KBIT = 1-16        RING 3
C
C
             DO 302 I=1,192
             IF(HJCTRK(I).EQ.0) GOTO 302
             KWD=(I-1)/8*4+2
             KBIT=I-(I-1)/8*8+8
             IF(I.LE.48) GOTO 303
             KWD=(I-49)/8*4+2
             KBIT=I-(I-49)/8*8-48
             IF(I.LE.96) GOTO 303
             KWD=(I-97)/16*4+1
             KBIT=I-(I-97)/16*16-96
 303         CONTINUE
             HWORD(KWD)=hLOR(HWORD(KWD),hint(MASK(KBIT))) ! PMF 10/06/99: hlor,hint 
 302         CONTINUE
C
C  FILL TOF COUNTER LATCH ARRAY     ITOF(1-42)
C
                   IPL=IDATA(IBLN('LATC'))
                   IF(IPL.EQ.0) GOTO 201
                   DO 200 KWD=1,6
                   JBIT10=HDATA(IPL*2+5+KWD)
                   DO 200 KBIT=1,7
  200              ITOF((KWD-1)*7+KBIT)=JBIT(JBIT10,KBIT)
  201              CONTINUE
C
C                                           FOLLOWING ILLUSTRATION
C                                           BY ARTIST JAN OLSSON.
C                           N S E G
C                       --------------
C                       .     I     I
C                       .     I     I
C                       .  1  2  3  4
C                       .     I     I
C                       .     I     I
C           -------------------------------------
C           .           I           I           .
C           .           I           I           .
C           .     5     6     7     8     9     .
C           .           I           I           .
C           .           I           I           .
C           -------------------------------------
C           I           I           I           I
C           I           I           I           I
C          10    11     12    13    14    15    16
C           I           I           I           I
C           I           I           I           I
C           -------------------------------------
C     CELL AND WALL NUMBERING FOR TRACK DEFINITION
C
C  A SLOW TRACK IS DEFINED BY  1 - 5 - 10         (HFT = 0)
C  A FAST TRACK IS DEFINED BY  1 - 5 - 11, 1 - 5 - 12   ETC.   (HFT=1)
C
C   DEFINED BY ARRAY HFT(12,4)   1-4 GIVEN BY 1-4 IN RING 3
C   TRACKS DEFINED BY COINCIDENCE RING 1 - 3, GIVEN BY ARRAYS HR1,HR2
C
C
C  LOOP OVER INNER DETECTOR TRACK ELEMENTS FOR LINKING
C
      KA=1
      KF=1
      DO 400 NSEG=1,24
      NR3=96+(NSEG-1)*4
C  ARE THERE R3 TRACK ELEMENTS IN THIS SEGMENT
      IF((HJCTRK(NR3+1)+HJCTRK(NR3+2)+HJCTRK(NR3+3)+HJCTRK(NR3+4)).GT.0)
     +  GOTO 420
      KA=1
      KF=1
      GOTO 400
 420  CONTINUE
C
C  FILL 16 INPUTS OF FPLA FOR ONE STREET WITH TRACK ELEMENTS
C    HFPLA(1-4) :    RING3
C    HFPLA(5-9) :    RING2
C    HFPLA(10-16) :    RING1
C
C                           R3(1-4),R2(5-9),R1(10-16)
      NR2I=2*NSEG-4+48
      IF(NR2I.LE.48) NR2I=NR2I+48
        DO 403 N2=1,5
        NR2=NR2I+N2
        IF(NR2.GT.96) NR2=NR2-48
        IF(N2.NE.5) HFPLA(N2)=HJCTRK(NR3+N2)
  403   HFPLA(4+N2)=HJCTRK(NR2)
      NR1I=2*NSEG-5
      IF(NR1I.LE.0) NR1I=NR1I+48
        DO 404 N1=1,7
        NR1=NR1I+N1
        IF(NR1.GT.48) NR1=NR1-48
  404   HFPLA(9+N1)=HJCTRK(NR1)
C
C  PROOF FPLA CONDITIONS
        DO 401 ITYPE=1,4
        IF(HFPLA(ITYPE).EQ.0) GOTO 411
        H123A=0
        H123F=0
          DO 500 ICOND=1,12
          IF((ITYPE+ICOND).EQ.16) GOTO 500
          IF((HFPLA(HR2(ICOND,ITYPE))+HFPLA(HR1(ICOND,ITYPE))).EQ.2)
     +             H123A=1
          IF((HFPLA(HR2(ICOND,ITYPE))+HFPLA(HR1(ICOND,ITYPE)) +
     +             HFT(ICOND,ITYPE)).EQ.3) H123F=1
  500     CONTINUE
        IF((H123A+H123F).EQ.0) GOTO 411
C  TRACK FOUND THROUGH R1,R2,R3    KSEG DIVIDES DETECTOR IN 96 PARTS
        KSEG=(NSEG-1)*4+ITYPE
C
C  ADD TOF COUNTERS
C
             PHIJ=(360./96.)*KSEG
             DPHITO=360./42.
             ATOF=PHIJ/DPHITO+1.5001
             KTOF=ATOF
             KTOF=KTOF-KTOF/43*42
             KTP=KTOF+1-KTOF/42*42
             KTM=KTOF-1+(43-KTOF)/42*42
C            LOGIC OR OF 2 OR 3 TOF COUNTERS
             IF((ATOF-FLOAT(KTOF)).LT..001) KTP=KTM
             IF((ITOF(KTOF)+ITOF(KTP)+ITOF(KTM)).LT.1) GOTO 411
C  TRACKS GOES THROUGH FIRED TOF
  501 CONTINUE
C
C  COUNT TRACKS FOR ALL/HIGH MOMENTA AND CLASSIFY WITH SEGMENT IN R3
C    KA AND KF ARE 1 FIRST TIME, 0 FOLLING TIMES
C
      NJTRKA=NJTRKA+KA
      KA=1-KA
      HJTRKA(KSEG)=1
      IF(H123F.EQ.0) GOTO 412
      NJTRKF=NJTRKF+KF
      KF=1-KF
      HJTRKF(KSEG)=1
      GOTO 401
C  NO TRACKS FOUND, RESET KA,KF
  411 KA=1
  412 KF=1
  401 CONTINUE
  400 CONTINUE
C
C  FILL ARRAY HWORD WITH TRACK INFORMATION FOR LATER STORE IN TRIG:2
C    KWD = 3,7,11,15,19,23   KBIT = 1-16      ALL TRACKS
C    KWD = 4,8,12,16,20,24   KBIT = 1-16     FAST TRACKS
C
             DO 600 I=1,96
             KWD=(I-1)/16*4+3
             KBIT=I-(I-1)/16*16
        IF(HJTRKA(I).EQ.1) HWORD(KWD)=hLOR(HWORD(KWD),hint(MASK(KBIT))) ! PMF 10/06/99: hlor,hint 
      IF(HJTRKF(I).EQ.1)HWORD(KWD+1)=hLOR(HWORD(KWD+1),hint(MASK(KBIT))) ! PMF 10/06/99: hlor,hint 
  600        CONTINUE
C
C                                           CODE FOR NARROW COPLANARITY
C                                           CHECK LINKING TO FAST TRACKS
      ICOPLA = 0
      IF( NJTRKF .LE. 1 ) GO TO 790
        DO 700 I=1,72
        IF( HJTRKF(I) .EQ. 0 ) GO TO 700
        JLO = I + 48 - IWIDT2*2
        JHI = I + 48 + IWIDT2*2
          DO 700 J=JLO,JHI
          JOP = J
          IF(JOP.GT.96) JOP = JOP - 96
          IF( HJTRKF(JOP) .EQ. 0 ) GO TO 700
          ICOPLA = 1
          GO TO 790
  700   CONTINUE
  790 CONTINUE
C
 1000 CONTINUE
C
C  FILL BANK 'TRIG' NO.2  WITH CONTENT OF ARRAY HWORD
C
      CALL CCRE(IND2,'TRIG',2,13,IER)
      IF( IER .EQ. 2 ) GO TO 8000
      CALL MVCL( IDATA, IND2*4, IWORD, 0, 52 )
      IDATA(IND2+1) = 0
C
      CALL VZERO(IW22,8)
C
C  T2 LOGIC CONDITIONS ACCORDING TO T1 POSTPONE
C
      IT1=HDATA(IND1*2+10)
      ICO1 = 0
C
C--->>> 2-TRACK TRIGGERS
C
C         TOF AND TOTAL LG ENERGY
      IF(JBIT(IT1,1).EQ.1.AND.NJTRKA.GE.HACC1 ) ICO1= LOR(ICO1,MASK(10))
C
C         TAGG
C
      IF(JBIT(IT1,2).EQ.1.AND.NJTRKA.GE.HACC3 ) ICO1= LOR(ICO1,MASK(10))
C
C--->>> 2-COLL.TRACK TRIGGERS
C
C         COPLANARITY IN TOF, WIDE, CASE OF ANY TWO FAST TRACKS
C
C     IF(HWCOLN.NE.1 .AND.
      IF(IHIST(3).LT.1981.AND.
     +   JBIT(IT1,5).EQ.1.AND.NJTRKF.GE.HACC2 )
     +                                          ICO1= LOR(ICO1,MASK(11))
C
C         COPLANARITY IN TOF, WIDE, CASE OF COPLANAR FAST TRACKS
C
C     IF(HWCOLN.EQ.1 .AND.
      IF(IHIST(3).EQ.1981.AND.
     +   JBIT(IT1,5).EQ.1.AND.NJTRKF.GE.HACC2 .AND. ICOPLA .EQ. 1 )
     +                                          ICO1= LOR(ICO1,MASK(11))
C
C         COPLANARITY IN TOF, NARROW
C
C     IF(HWCOLN.EQ.1 .AND.
      IF(IHIST(3).EQ.1981.AND.
     +   JBIT(IT1,6).EQ.1.AND.NJTRKF.GE.HACC4 .AND. ICOPLA .EQ. 1 )
     +                                          ICO1= LOR(ICO1,MASK(11))
C
C--->>> 3-TRACK TRIGGERS
C
C         COPLANARITY IN TOF, WIDE MULTIPRONG CONDITION
C
      IF(JBIT(IT1,7).EQ.1.AND.NJTRKA.GE.HACCM ) ICO1= LOR(ICO1,MASK(12))
C -----------------------------------------------
      IF( ICO1 .GT. 0 ) IW22(6) = 1
C
      DO 1100 IBIT=1,8
      IW22(4)=IW22(4)+JBIT(IT1,IBIT+8)
 1100 IW22(5)=IW22(5)+JBIT(IT1,IBIT)
      IF(IW22(4).GE.1) IW22(1)=1
      IF(IW22(4).EQ.0.AND.IW22(6).EQ.1) IW22(2)=1
C
C  FILL WORD 22 IN 'HEAD': ACTUAL ACC T1,T2,T3; ACTION T1ACC,T1POST,
C                          T2ACC,T2POST,  T3ACC
      K=IND0*2+22
      HDATA(K) = 0
      DO 1101 IBIT=1,8
      IF(IW22(IBIT).GE.1) HDATA(K)=hLOR(HDATA(K),hint(MASK(IBIT)))! PMF 10/06/99: hlor,hint 
 1101 CONTINUE
C                                           FILL BEATES SPECIAL WORD
C                                           IN TRIG,1 SHOWING ACTUAL
C                                           T2 ACCEPT SOURCE
      HDATA(IND1*2+9) = hLOR(HDATA(IND1*2+9),hint(ICO1)) ! PMF 10/06/99: hlor,hint 
C
      RETURN
C
C                                           NO SPACE IN BCS LEFT
 8000 WRITE(6,9101)
 9101 FORMAT(/' NOT ENOUGH SPACE IN BCS LEFT FOR CREATION OF TRIG, 2'/)
      RETURN
      END
      SUBROUTINE RD82T2
C***********************************************************************
C     VERSION OF 5/3/79     AUTHOR M.HELM
C     LAST MOD 27/07/86     J.OLSSON
C     COPLANARITY LOGIC FOR CHARGED TRACKS CORRECTED
C              --------------
C     THIS ROUTINE PERFORMS A SOFTWARE TRIGGER T2.
C     INPUT ARE THE LATCHES FOR JET CHAMBER WIRES AND TOF COUNTERS.
C******************************************************************
C
      IMPLICIT INTEGER*2 (H)
      COMMON /CMASK/ MASK(16),MASK4,MASK3,MASK2,MASK1
      COMMON/BCS/IDATA(1)
      DIMENSION HDATA(2),IW22(8)
      EQUIVALENCE (IDATA(1),HDATA(1))
      COMMON/CWORK/HJHIT(96),HJCTRK(192),HJTRKA(96),HJTRKF(96),ITOF(42),
     +             HWORD(24),HFPLA(16),NJTRKA,NJTRKF,IDUMMY(96)
      DIMENSION IWORD(13)
      EQUIVALENCE(IWORD(2),HWORD(1))
      DIMENSION NZERO(400)
      EQUIVALENCE(NZERO(1),HJHIT(1))
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
      COMMON /CFPLA/ HFT(12,4),HR1(12,4),HR2(12,4)
C
C  RETURN, WHEN BANKS 'HEAD' OR 'TRIG' NO.1 NOT EXISTING
C                   OR BANK 'TRIG' NO.2 ALREADY EXISTING
      IND0=IDATA(IBLN('HEAD'))
      CALL CLOC(IND1,'TRIG',1)
      CALL CLOC(IND2,'TRIG',2)
      IF(IND2.NE.0.OR.IND0.EQ.0.OR.IND1.EQ.0) RETURN
C
      NJTRKA=0
      NJTRKF=0
C                         ZERO SET COMMON /CWORK/
      CALL VZERO(NZERO,400)
C
C  FILL JETC HIT CELL ARRAY     HJHIT:  HIT WIRES/CELL
C
             IPJ=IDATA(IBLN('JETC'))
             IF(IPJ.EQ.0) GOTO 1000
             IL4=IPJ*2+101
             IH4=IL4+HDATA(IPJ*2+99)-4
             NWSTOR=-1
             DO 300 KWIRE=IL4,IH4,4
             NWIRE=HDATA(KWIRE)/8
             IF(NWIRE.EQ.NWSTOR) GOTO 300
             NWSTOR=NWIRE
             NWCLL=NWIRE/16+1
             HJHIT(NWCLL)=HJHIT(NWCLL)+1
  300        CONTINUE
C
C  OVERWRITE HITS PER CELL WHEN IN DEAD CELLS WIRES ARE SWITCHED ON
C
             DO 310 K=1,10
             IF(HDEADC(K).NE.0) HJHIT(HDEADC(K))=16
  310        CONTINUE
C
C  FILL ARRAY HJCTRK: HIT CELLS (ODD INDEX) AND HIT WALLS (EVEN INDEX)
C
      DO 301  IRING=1,3
      IEND=HCHAMB(IRING)
      DO 301  ICELL=1,IEND
      I=ICELL+(IRING-1)*24
      IF(HJHIT(I).GT.HITCLL(IRING)) HJCTRK(2*I-1)=1
C  I IS CELL INDEX, I1 IS NEXT CELL
      I1=I+1
      IF(ICELL.EQ.HCHAMB(IRING)) I1=I1-HCHAMB(IRING)
      IF(HJHIT(I).GT.HITWLL(IRING).AND.HJHIT(I1).GT.HITWLL(IRING).AND.
     +  (HJHIT(I)+HJHIT(I1)).GT.HITSUM(IRING)) HJCTRK(2*I)=1
  301 CONTINUE
C
C  FILL ARRAY HWORD(1-24)   IT IS LATER STORED IN    TRIG:2
C
C      KWD = 2,6,10,14,18,22    KBIT = 9-16, 1-8   RING 1,2
C      KWD = 1,5,9,13,17,21     KBIT = 1-16        RING 3
C
             DO 302 I=1,192
             IF(HJCTRK(I).EQ.0) GOTO 302
             KWD=(I-1)/8*4+2
             KBIT=I-(I-1)/8*8+8
             IF(I.LE.48) GOTO 303
             KWD=(I-49)/8*4+2
             KBIT=I-(I-49)/8*8-48
             IF(I.LE.96) GOTO 303
             KWD=(I-97)/16*4+1
             KBIT=I-(I-97)/16*16-96
 303         CONTINUE
             HWORD(KWD)=hLOR(HWORD(KWD),hint(MASK(KBIT))) ! PMF 10/06/99: hlor,hint 
 302         CONTINUE
C
C  FILL TOF COUNTER LATCH ARRAY     ITOF(1-42)
C
                   IPL=IDATA(IBLN('LATC'))
                   IF(IPL.EQ.0) GOTO 201
                   DO 200 KWD=1,6
                   JBIT10=HDATA(IPL*2+5+KWD)
                   DO 200 KBIT=1,7
  200              ITOF((KWD-1)*7+KBIT)=JBIT(JBIT10,KBIT)
  201              CONTINUE
C
C                                           FOLLOWING ILLUSTRATION
C                                           BY ARTIST JAN OLSSON.
C                           N S E G
C                       --------------
C                       .     I     I
C                       .     I     I
C                       .  1  2  3  4
C                       .     I     I
C                       .     I     I
C           -------------------------------------
C           .           I           I           .
C           .           I           I           .
C           .     5     6     7     8     9     .
C           .           I           I           .
C           .           I           I           .
C           -------------------------------------
C           I           I           I           I
C           I           I           I           I
C          10    11     12    13    14    15    16
C           I           I           I           I
C           I           I           I           I
C           -------------------------------------
C     CELL AND WALL NUMBERING FOR TRACK DEFINITION
C
C  A SLOW TRACK IS DEFINED BY  1 - 5 - 10         (HFT = 0)
C  A FAST TRACK IS DEFINED BY  1 - 5 - 11, 1 - 5 - 12   ETC.   (HFT=1)
C
C   DEFINED BY ARRAY HFT(12,4)   1-4 GIVEN BY 1-4 IN RING 3
C   TRACKS DEFINED BY COINCIDENCE RING 1 - 3, GIVEN BY ARRAYS HR1,HR2
C
C
C  LOOP OVER INNER DETECTOR TRACK ELEMENTS FOR LINKING
C
      KA=1
      KF=1
      DO 400 NSEG=1,24
      NR3=96+(NSEG-1)*4
C  ARE THERE R3 TRACK ELEMENTS IN THIS SEGMENT
      IF((HJCTRK(NR3+1)+HJCTRK(NR3+2)+HJCTRK(NR3+3)+HJCTRK(NR3+4)).GT.0)
     +  GOTO 420
      KA=1
      KF=1
      GOTO 400
 420  CONTINUE
C
C  FILL 16 INPUTS OF FPLA FOR ONE STREET WITH TRACK ELEMENTS
C    HFPLA(1-4) :    RING3
C    HFPLA(5-9) :    RING2
C    HFPLA(10-16) :    RING1
C
C                           R3(1-4),R2(5-9),R1(10-16)
      NR2I=2*NSEG-4+48
      IF(NR2I.LE.48) NR2I=NR2I+48
        DO 403 N2=1,5
        NR2=NR2I+N2
        IF(NR2.GT.96) NR2=NR2-48
        IF(N2.NE.5) HFPLA(N2)=HJCTRK(NR3+N2)
  403   HFPLA(4+N2)=HJCTRK(NR2)
      NR1I=2*NSEG-5
      IF(NR1I.LE.0) NR1I=NR1I+48
        DO 404 N1=1,7
        NR1=NR1I+N1
        IF(NR1.GT.48) NR1=NR1-48
  404   HFPLA(9+N1)=HJCTRK(NR1)
C
C  PROOF FPLA CONDITIONS
        DO 401 ITYPE=1,4
        IF(HFPLA(ITYPE).EQ.0) GOTO 411
        H123A=0
        H123F=0
          DO 500 ICOND=1,12
          IF((ITYPE+ICOND).EQ.16) GOTO 500
          IF((HFPLA(HR2(ICOND,ITYPE))+HFPLA(HR1(ICOND,ITYPE))).EQ.2)
     +             H123A=1
          IF((HFPLA(HR2(ICOND,ITYPE))+HFPLA(HR1(ICOND,ITYPE)) +
     +             HFT(ICOND,ITYPE)).EQ.3) H123F=1
  500     CONTINUE
        IF((H123A+H123F).EQ.0) GOTO 411
C  TRACK FOUND THROUGH R1,R2,R3    KSEG DIVIDES DETECTOR IN 96 PARTS
        KSEG=(NSEG-1)*4+ITYPE
C
C  ADD TOF COUNTERS
C
             PHIJ=(360./96.)*KSEG
             DPHITO=360./42.
             ATOF=PHIJ/DPHITO+1.5001
             KTOF=ATOF
             KTOF=KTOF-KTOF/43*42
             KTP=KTOF+1-KTOF/42*42
             KTM=KTOF-1+(43-KTOF)/42*42
C            LOGIC OR OF 2 OR 3 TOF COUNTERS
             IF((ATOF-FLOAT(KTOF)).LT..001) KTP=KTM
             IF((ITOF(KTOF)+ITOF(KTP)+ITOF(KTM)).LT.1) GOTO 411
C  TRACKS GOES THROUGH FIRED TOF
  501 CONTINUE
C
C  COUNT TRACKS FOR ALL/HIGH MOMENTA AND CLASSIFY WITH SEGMENT IN R3
C    KA AND KF ARE 1 FIRST TIME, 0 FOLLING TIMES
C
      NJTRKA=NJTRKA+KA
      KA=1-KA
      HJTRKA(KSEG)=1
      IF(H123F.EQ.0) GOTO 412
      NJTRKF=NJTRKF+KF
      KF=1-KF
      HJTRKF(KSEG)=1
      GOTO 401
C  NO TRACKS FOUND, RESET KA,KF
  411 KA=1
  412 KF=1
  401 CONTINUE
  400 CONTINUE
C
C  FILL ARRAY HWORD WITH TRACK INFORMATION FOR LATER STORE IN TRIG:2
C    KWD = 3,7,11,15,19,23   KBIT = 1-16      ALL TRACKS
C    KWD = 4,8,12,16,20,24   KBIT = 1-16     FAST TRACKS
C
             DO 600 I=1,96
             KWD=(I-1)/16*4+3
             KBIT=I-(I-1)/16*16
        IF(HJTRKA(I).EQ.1) HWORD(KWD)=hLOR(HWORD(KWD),hint(MASK(KBIT))) ! PMF 10/06/99: hlor,hint 
      IF(HJTRKF(I).EQ.1)HWORD(KWD+1)=hLOR(HWORD(KWD+1),hint(MASK(KBIT))) ! PMF 10/06/99: hlor,hint 
  600        CONTINUE
C
C                                           CODE FOR NARROW COPLANARITY
C                                           CHECK LINKING TO FAST TRACKS
      ICOPLA = 0
      IF( NJTRKF .LE. 1 ) GO TO 790
        DO 700 I=1,72
        IF( HJTRKF(I) .EQ. 0 ) GO TO 700
        JLO = I + 48 - IWIDT2*2
        JHI = I + 48 + IWIDT2*2
          DO 700 J=JLO,JHI
          JOP = J
          IF(JOP.GT.96) JOP = JOP - 96
          IF( HJTRKF(JOP) .EQ. 0 ) GO TO 700
          ICOPLA = 1
          GO TO 790
  700   CONTINUE
  790 CONTINUE
C
 1000 CONTINUE
C
C  FILL BANK 'TRIG' NO.2      WITH CONTENT OF ARRAY HWORD
C
      CALL CCRE(IND2,'TRIG',2,13,IER)
      IF( IER .EQ. 2 ) GO TO 8000
      CALL MVCL( IDATA, IND2*4, IWORD, 0, 52 )
      IDATA(IND2+1) = LOR(IDATA(IND2+1),MASK2)
C
      CALL VZERO(IW22,8)
C
C  T2 LOGIC CONDITIONS ACCORDING TO T1 POSTPONE
C
      IT0=HDATA(IND1*2+8)
      IT1=HDATA(IND1*2+9)
      ICO1 = 0
      IW12 = 0
C
C--->>> 2-TRACK TRIGGERS
C
C              TOF AND TOTAL LG ENERGY
C
      IF(JBIT(IT1,1).EQ.1.AND.NJTRKA.GE.HACC1 ) ICO1= LOR(ICO1,MASK(1))
      IF(IHIST(3).GT.1984.AND.
     $   JBIT(IT1,1).EQ.1.AND.NJTRKA.GE.HACC1 ) IW12= LOR(IW12,MASK(1))
C
C                     TOF, ECAP1 + ECAP2
C
      IF(JBIT(IT1,3).EQ.1.AND.NJTRKA.GE.HACC1 ) ICO1= LOR(ICO1,MASK(1))
      IF(IHIST(3).GT.1984.AND.
     $   JBIT(IT1,3).EQ.1.AND.NJTRKA.GE.HACC1 ) IW12= LOR(IW12,MASK(2))
C
C              TBGNS
C
      IF(JBIT(IT1,4).EQ.1.AND.NJTRKA.GE.HACC2 ) ICO1= LOR(ICO1,MASK(1))
      IF(IHIST(3).GT.1984.AND.
     $   JBIT(IT1,4).EQ.1.AND.NJTRKA.GE.HACC2 ) IW12= LOR(IW12,MASK(3))
C
C--->>> 1-TRACK TRIGGERS
C
C              TBG AND BARREL LG ENERGY
C
      IF(JBIT(IT1,9).EQ.1.AND.NJTRKF.GE.HACC3)  ICO1=LOR(ICO1,MASK(3))
      IF(IHIST(3).GT.1984.AND.
     $   JBIT(IT1,9).EQ.1.AND.NJTRKF.GE.HACC3)  IW12=LOR(IW12,MASK(4))
C
C              TAGG AND TBG
C
      IF(JBIT(IT1,10).EQ.1.AND.NJTRKF.GE.HACC4) ICO1=LOR(ICO1,MASK(3))
      IF(IHIST(3).GT.1984.AND.
     $   JBIT(IT1,10).EQ.1.AND.NJTRKF.GE.HACC4) IW12=LOR(IW12,MASK(5))
C
C              COLL. SEPTANT + TOF
C
      IF(JBIT(IT1,11).EQ.1.AND.NJTRKA.GE.HACCB1) ICO1=LOR(ICO1,MASK(3))
      IF(IHIST(3).GT.1984.AND.
     $   JBIT(IT1,11).EQ.1.AND.NJTRKA.GE.HACCB1) IW12=LOR(IW12,MASK(9))
C
C              TAGG + SEPTANT + TOF
C
      IF(JBIT(IT1,12).EQ.1.AND.NJTRKA.GE.HACCB2) ICO1=LOR(ICO1,MASK(3))
      IF(IHIST(3).GT.1984.AND.
     $   JBIT(IT1,12).EQ.1.AND.NJTRKA.GE.HACCB2) IW12=LOR(IW12,MASK(10))
C
C--->>> 2-COLL.TRACK TRIGGERS
C
C              COPLANAR TOF, WIDE
C
      IF(JBIT(IT1,13).EQ.1.AND.ICOPLA.EQ.1)     ICO1=LOR(ICO1,MASK(4))
      IF(IHIST(3).GT.1984.AND.
     $   JBIT(IT1,13).EQ.1.AND.ICOPLA.EQ.1)     IW12=LOR(IW12,MASK(6))
C
C              COPLANAR TOF, NARROW
C
      IF(JBIT(IT1,14).EQ.1.AND.ICOPLA.EQ.1)     ICO1=LOR(ICO1,MASK(4))
      IF(IHIST(3).GT.1984.AND.
     $   JBIT(IT1,14).EQ.1.AND.ICOPLA.EQ.1)     IW12=LOR(IW12,MASK(7))
C
C              COPLANAR TBG
C
      IF(JBIT(IT1,15).EQ.1.AND.ICOPLA.EQ.1)     ICO1=LOR(ICO1,MASK(4))
      IF(IHIST(3).GT.1984.AND.
     $   JBIT(IT1,15).EQ.1.AND.ICOPLA.EQ.1)     IW12=LOR(IW12,MASK(8))
C -------------------------------------------
      IF( ICO1 .GT. 0 ) IW22(6) = 1
C
      DO 1100 IBIT=1,16
      IW22(4)=IW22(4)+JBIT(IT0,IBIT)
 1100 IW22(5)=IW22(5)+JBIT(IT1,IBIT)
      IF(IW22(4).GE.1) IW22(1)=1
      IF(IW22(4).EQ.0.AND.IW22(6).EQ.1) IW22(2)=1
C
C  FILL WORD 22 IN 'HEAD': ACTUAL ACC T1,T2,T3; ACTION T1ACC,T1POST,
C                          T2ACC,T2POST,  T3ACC
      K=IND0*2+22
      HDATA(K) = 0
      DO 1101 IBIT=1,8
      IF(IW22(IBIT).GE.1) HDATA(K)=hLOR(HDATA(K),hint(MASK(IBIT))) ! PMF 10/06/99: hlor,hint 
 1101 CONTINUE
C  FOR 1985-86 DATA, SET ALSO ENABLE BITS IN WORDS 37-38 OF HEAD
      IF(IHIST(3).LT.1985) GO TO 6670
      HDATA(IND0*2+37) = HT1ACC
      HDATA(IND0*2+38) = HT1PSP
      HDATA(IND0*2+1) = 3
6670  CONTINUE
C                                           FILL BEATES SPECIAL WORD
C                                           IN TRIG,1 SHOWING ACTUAL
C                                           T2 ACCEPT SOURCE
      HDATA(IND1*2+10) = hLOR(HDATA(IND1*2+10),hint(ICO1))        ! PMF 10/06/99: hlor,hint 
C                                           FILL T2 COINCIDENCES WORD
C                                           IN TRIG,1 SHOWING ACTUAL
C                                           T2 TRIGGER (ONLY 1985-86)
      IF(IHIST(3).GT.1984) HDATA(IND1*2+12) = IW12
C
      RETURN
C
C                                           NO SPACE IN BCS LEFT
 8000 WRITE(6,9101)
 9101 FORMAT(/' NOT ENOUGH SPACE IN BCS LEFT FOR CREATION OF TRIG, 2'/)
      RETURN
      END
