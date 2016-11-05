C   29/04/80 403251225  MEMBER NAME  MCREDU   (JMC.S)       FORTRAN
C
C-----------------------------------------------------------------------
      SUBROUTINE MCREDU( EBEFIX, * )
C-----------------------------------------------------------------------
C
C   AUTHOR:   E. ELSEN     29/04/80 :  CUTS FOR MULTI-HADRON REDUCTION
C
C        MOD: E. ELSEN      2/11/80 :
C   LAST MOD: C. BOWDERY   25/03/84 :  RECOMMENTING
C
C
C     EBEFIX IS VALUE OF BEAM ENERGY TO LIMIT MOMENTA . IF ZERO
C     EBEAM IS TAKEN FROM HEAD FOR RUN NUMBER.EQ.0, OTHERWISE
C     FUNCTION EBEAM IS USED.
C     IF( EBEAM < 10 GEV ) BARREL ENERGY CUT IS CHANGED
C     RETURN 1 IF NOT ACCEPTED.
C
C-----------------------------------------------------------------------
C
      IMPLICIT INTEGER*2 (H)
C
      REAL*4 R(3) / 3*0. /
      INTEGER NREJ(20) / 20*0 /
C
      COMMON / BCS / IW(1)
C
      DIMENSION RW(1),HW(1)
      DIMENSION HBEAM(2)
      DIMENSION P1(3), P2(3), JLONG(2)
      DIMENSION PCENTI(20)
C
      EQUIVALENCE (HW(1),RW(1),IW(1))
      EQUIVALENCE (HBEAM(1),IBEAM)
C
      DATA EBEAMI / 10000. /, EBEAMA / 0. /
      DATA IEFIX / 0 /
      DATA NCALL / 0 /, NACC / 0 /
      DATA IHIST / 0 /
C
C------------------  C O D E  ------------------------------------------
C
      IF( NCALL .NE. 0 ) GO TO 2
      IBHEAD = IBLN('HEAD')
      IBTRIG = IBLN('TRIG')
      IBPATR = IBLN('PATR')
      IBALGN = IBLN('ALGN')
      IBLGCL = IBLN('LGCL')
      IF( EBEFIX .NE. 0. ) IEFIX = 1
      IF( IHIST .NE. 1 ) GO TO 2
      CALL THIST( 10001,'#OF LONG TRACKS$' )
      CALL DHIST( 10001, 0., 100. )
      CALL THIST( 10002,'#OF TRACKS$' )
      CALL DHIST( 10002, 0., 100. )
      CALL THIST( 10003,'RMIN2 BEFORE PATR CUT$' )
      CALL DHIST( 10003, 0., 200. )
      CALL THIST( 10004,'ACOPLANARITY--COS PHI$' )
      CALL DHIST( 10004, -1., .02  )
      CALL THIST( 10005,'SUMPL$' )
      CALL DHIST( 10005, 0., 60. )
      CALL THIST( 10006,'SHOWER TOTAL$' )
      CALL DHIST( 10006, 0., 60. )
      CALL THIST( 10007,'E-SHOWER BARREL$' )
      CALL DHIST( 10007, 0., 60. )
      CALL THIST( 10008,'SHOWER + P$' )
      CALL DHIST( 10008, 0., 60. )
      CALL THIST( 10009,'SHOWER + PL$' )
      CALL DHIST( 10009, 0., 60. )
      CALL THIST( 10015,'ETOTSL$' )
      CALL DHIST( 10015, 0., 60. )
      CALL THIST( 10020,'#OF LONG TRACKS AFTER CUT$' )
      CALL DHIST( 10020, 0., 100. )
      CALL THIST( 10021,'#OF TRACKS$ AFTER CUT' )
      CALL DHIST( 10021, 0., 100. )
      CALL THIST( 10022,'SHOWER TOTAL AFTER CUT$' )
      CALL DHIST( 10022, 0., 60. )
      CALL THIST( 10023,'E SHOWER BARREL AFTER CUT$' )
      CALL DHIST( 10023, 0., 60. )
      CALL THIST( 10024,'SUMPL AFTER CUT$' )
      CALL DHIST( 10024, 0., 60. )
    2 CONTINUE
C
C                                           CHECK TRIGGER
      NCALL = NCALL + 1
      NPHEAD = IW(IBHEAD)
      IF( NPHEAD .GT. 0 ) GO TO 100
      NREJ( 1) = NREJ( 1) + 1
      GO TO 8000
C
  100 ITR = HW(NPHEAD*2+22)
      IF( MOD(ITR,8) .NE. 0 ) GO TO 110
      NREJ(13) = NREJ(13) + 1
      GO TO 8000
C                                           MAG FIELD, ENERGY
  110 BKGAUS = ABS( HW(NPHEAD*2+30)/1000. )
      IBEAM= HW(NPHEAD*2+29)
      HBEAM(1) = 0
      EBEA = IBEAM / 1000.
      HRUN = HW(NPHEAD*2+10)
      IF( HRUN .NE. 0 ) EBEA = EBEAM( HRUN ) / 1000.
      IF( IEFIX .EQ. 1 ) EBEA = EBEFIX
      IF( EBEA .LT. EBEAMI ) EBEAMI = EBEA
      IF( EBEA .GT. EBEAMA ) EBEAMA = EBEA
C                                           BARREL ENERGY CUT
      EBARC = 3.
      ENDCC = .4
      IF( EBEA .GT. 12. ) GO TO 120
      EBARC = 2.
      ENDCC = .4
  120 CONTINUE
      IF( EBEA .GT. 8. ) GO TO 130
      EBARC = 1.2
      ENDCC = .2
  130 CONTINUE
C
C                                           CHARGED TRACKS
      NPPATR = IW(IBPATR)
      IF( NPPATR .GT. 0 ) GO TO 210
      NREJ( 3) = NREJ( 3) + 1
      GO TO 8000
C
  210 SUMPL = 0.
      SUMLZ = 0.
      SUMP = 0.
      SUMZ = 0.
      NTRKL = 0
      RMIN1 = 10000.
      RMIN2 = 10000.
      ZMIN = 1000000.
      AZMIN = 1000000.
      NTRK = IW(NPPATR+2)
      IF( NTRK .GT. 0 ) GO TO 220
      NREJ(11) = NREJ(11) + 1
      GO TO 8000
C
  220 LPATR = IW(NPPATR+3)
      IP0 = NPPATR + IW(NPPATR+1)
      IP9 = IP0 + (NTRK-1)*LPATR
      DO 300 J=IP0,IP9,LPATR
C                                           Z VALUES
      ZV = RW(J+31)
      AZV = ABS(ZV)
      IF( AZV .GT. AZMIN ) GO TO 240
      ZMIN = ZV
      AZMIN = AZV
  240 CONTINUE
C                                           RMIN
      CALL PRTOCI( J, CAP, RMIN, PHIM, SIG )
      ARMIN = ABS(RMIN)
      IF( ARMIN .GT. RMIN1 ) GO TO 245
      RMIN2 = RMIN1
      RMIN1 = ARMIN
      GO TO 247
  245 IF( ARMIN .GT. RMIN2 ) GOTO 247
      RMIN2 = ARMIN
  247 CONTINUE
C
      PRAD = .3E-4*BKGAUS/CAP
      DZDR = RW(J+30)
      P = PRAD*SQRT(1.+DZDR**2)
      A = 1.
      IF( P .GT. EBEA ) A = EBEA / P * .5
      P = P * A
      PZ = PRAD*DZDR*A
      SUMP = SUMP + P
      SUMZ = SUMZ + PZ
C                                           NO OF HITS
      IF( IW(J+24) .LT. 24 ) GO TO 300
      IF( AZV .GT. 350.    ) GO TO 300
      IF( ARMIN .GT. 100.  ) GO TO 300
      NTRKL = NTRKL + 1
      SUMPL = SUMPL + P
      SUMLZ = SUMLZ + PZ
C                                           INDICES FOR FIRST LONG
      IF( NTRKL .GT. 2 ) GO TO 300
      JLONG(NTRKL) = J
  300 CONTINUE
C
      IF( IHIST .EQ. 1 ) CALL UHIST( 10001, FLOAT(NTRKL) )
      IF( IHIST .EQ. 1 ) CALL UHIST( 10002, FLOAT(NTRK) )
      IF( IHIST .EQ. 1 ) CALL UHIST( 10005, SUMPL )
C
C                                           PATR CUTS
      IF( NTRKL .GE. 2 .AND. NTRK .GE. 5 ) GO TO 310
      NREJ( 5) = NREJ( 5) + 1
      GO TO 8000
C
  310 IF( IHIST .EQ. 1 ) CALL UHIST( 10003, RMIN2 )
      IF( RMIN2 .LE. 30. ) GO TO 320
      NREJ( 6) = NREJ( 6) + 1
      GO TO 8000
C
  320 IF( ABS(ZMIN) .LE. 400. ) GO TO 330
      NREJ( 7) = NREJ( 7) + 1
      GO TO 8000
C
  330 CONTINUE
C                                           ACOPLANARITY
      ACOP = 1.5
      IF( NTRKL .NE. 2 ) GO TO 400
      CALL MOFRCI( JLONG(1), R, P1, CH )
      CALL MOFRCI( JLONG(2), R, P2, CH )
      ACOP = ( P1(1)*P2(1) + P1(2)*P2(2) ) /
     +       SQRT( ( P1(1)*P1(1) + P1(2)*P1(2) ) *
     +             ( P2(1)*P2(1) + P2(2)*P2(2) ) )
      IF( IHIST .EQ. 1 ) CALL UHIST( 10004, ACOP )
      IF( ACOP .GT. -.9848 ) GO TO 400
      NREJ( 8) = NREJ( 8) + 1
      GO TO 8000
C
C                                           TOTAL ENERGY
  400 NPLGCL = IW(IBLGCL)
      IF( NPLGCL .GT. 0 ) GO TO 410
      NREJ( 4 ) = NREJ( 4) + 1
      GO TO 8000
C
  410 J = NPLGCL + IW(NPLGCL+1)-1
      ETOT = RW(J+7)
      EBAR = RW(J+8)
      ENDM = RW(J+9)
      ENDP = RW(J+10)
      ESUM = SUMP + ETOT
      ESUML = SUMPL + ETOT
      IF( IHIST .EQ. 1 ) CALL UHIST( 10006, ETOT )
      IF( IHIST .EQ. 1 ) CALL UHIST( 10007, EBAR )
      IF( IHIST .EQ. 1 ) CALL UHIST( 10008, ESUM )
      IF( IHIST .EQ. 1 ) CALL UHIST( 10009, ESUML )
      IF( EBAR .GT. EBARC ) GO TO 420
      IF( ENDM.GT.ENDCC .AND. ENDP.GT.ENDCC ) GO TO 420
      NREJ( 9) = NREJ( 9) + 1
      GO TO 8000
C                                           CLUSTERS
  420 TOTSHW = 0.
      SHWZ = 0.
      NCLST = IW(NPLGCL+7)
      IF( NCLST .LE. 0 ) GO TO 450
      NWPCL = IW(NPLGCL+25)
      IB0 = NPLGCL + IW(NPLGCL+3) - 1
      IB9 = IB0 + (NCLST-1)*NWPCL
      DO 430 J=IB0,IB9,NWPCL
      ECL = RW(J+2)
      TOTSHW = TOTSHW + ECL
      SHWZ  = SHWZ + ECL*RW(J+11)
  430 CONTINUE
  450 CONTINUE
C
      ETOTSL = SUMPL + TOTSHW
      RSL = ( SUMLZ + SHWZ ) / ETOTSL
      IF( IHIST .EQ. 1 ) CALL UCORR( 10004, RSL, ETOTSL )
C                                           MOMENTUM BALANCE
      IF( ABS(RSL) .LT. .4 ) GO TO 510
      NREJ(10) = NREJ(10) + 1
      GO TO 8000
C                                           TOTAL ENERGY
  510 IF( IHIST .EQ. 1 ) CALL UHIST( 10015, ETOTSL )
      IF( ETOTSL .GT. EBEA ) GO TO 520
      NREJ(14) = NREJ(14) + 1
      GO TO 8000
C
  520 CONTINUE
      IF( IHIST .NE. 1 ) GO TO 530
      CALL UHIST( 10020, FLOAT(NTRKL) )
      CALL UHIST( 10021, FLOAT(NTRK) )
      CALL UHIST( 10022, ETOT )
      CALL UHIST( 10023, EBAR )
      CALL UHIST( 10024, SUMPL )
C
C
  530 NACC = NACC + 1
      RETURN
C
C                                           REJECTED
 8000 RETURN 1
C
C-----------------------------------------------------------------------
      ENTRY MCRDFL
C-----------------------------------------------------------------------
C ENTRY TO PRINT STATISTICS
C TO BE CALLED AT END OF JOB .
C-----------------------------------------------------------------------
      IF( NCALL .LE. 0 ) RETURN
      PCBAS = MAX0( NCALL, 1 ) * .01
      PCENT = NACC / PCBAS
      PCENTE = SQRT( FLOAT(NCALL - NACC) ) / PCBAS
      DO 8101 I=1,14
      PCENTI(I) = NREJ(I) / PCBAS
 8101 CONTINUE
      IF( IEFIX .NE. 1 )
     +WRITE(6,9101) NCALL, NACC, PCENT, PCENTE, EBEAMI, EBEAMA
 9101 FORMAT(//10X,' SUMMARY FOR THIS FILE',
     +           '         PROGRAM VERSION OF 04/11/80'/
     +       10X,'   EVENTS ANALYSED ',I8/
     +       10X,'   EVENTS ACCEPTED ',I8,'=',F7.2,' % +-',F5.2,' %'
     +                                   ,20X,F7.3,'=< EBEAM =<',F7.3,
     +           '  TAKEN FROM ''HEAD'''/)
      IF( IEFIX .EQ. 1 )
     +WRITE(6,9103) NCALL, NACC, PCENT, PCENTE, EBEAMI, EBEAMA
 9103 FORMAT(//10X,' SUMMARY FOR THIS FILE',
     +           '         PROGRAM VERSION OF 04/11/80'/
     +       10X,'   EVENTS ANALYSED ',I8/
     +       10X,'   EVENTS ACCEPTED ',I8,'=',F7.2,' % +-',F5.2,' %'
     +                                   ,20X,F7.3,'=< EBEAM =<',F7.3,
     +           '  VALUE WAS FIXED'/)
      WRITE(6,9102) (NREJ(I),PCENTI(I),I=1,14)
 9102 FORMAT(10X,' REASONS FOR REJECTION'/
     +/10X,'HEAD MISSING             ',I8,' =',F7.1,' %',
     +/10X,'TRIG MISSING             ',I8,' =',F7.1,' %',
     +/10X,'PATR MISSING             ',I8,' =',F7.1,' %',
     +/10X,'LGCL MISSING             ',I8,' =',F7.1,' %',
     +/10X,'NTRKL < 2                ',I8,' =',F7.1,' %',
     +/10X,'RMIN2 > 30.MM            ',I8,' =',F7.1,' %',
     +/10X,'ZMIN > 400.              ',I8,' =',F7.1,' %',
     +/10X,'ACOPL > 10 DEG           ',I8,' =',F7.1,' %',
     +/10X,'E - BARREL < 3(2,1.2)GEV ',
     +/10X,'E+ OR E- < .4 (.4,.2)GEV ',I8,' =',F7.1,' %',
     +/10X,'MOM BALANCE < .4         ',I8,' =',F7.1,' %',
     +/10X,'NO TRACKS                ',I8,' =',F7.1,' %',
     +/10X,'TOO MANY TRACKS          ',I8,' =',F7.1,' %',
     +/10X,'REJECTED BY TRIGGER      ',I8,' =',F7.1,' %',
     +/10X,'ETOTSL < EBEAM           ',I8,' =',F7.1,' %')
      IF( IHIST .NE. 1 ) GO TO 8900
      CALL PHIST( 10001 )
      CALL PHIST( 10002 )
      CALL PHIST( 10003 )
      CALL PHIST( 10004 )
      CALL PHIST( 10005 )
      CALL PHIST( 10006 )
      CALL PHIST( 10007 )
      CALL PHIST( 10008 )
      CALL PHIST( 10009 )
      CALL PHIST( 10015 )
      CALL PHIST( 10020 )
      CALL PHIST( 10021 )
      CALL PHIST( 10022 )
      CALL PHIST( 10023 )
      CALL PHIST( 10024 )
      CALL PCORR( 10004 )
 8900 RETURN
      END
