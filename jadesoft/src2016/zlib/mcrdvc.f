C   17/07/80 705252116  MEMBER NAME  MCRDVC   (S)           FORTRAN
      SUBROUTINE MCRDVC( NST, N, * )
C-----------------------------------------------------------
C  VERSION OF 17/07/80      LAST MOD 03/09/80     E.ELSEN
C  CUTS FOR MC REDUCTION WORKIN ON 4-VECTORS
C  ERASES INVISIBLE PARTICLES
C  NST = START POSITION IN P(10,**)
C  N = NUMBER OF PARTICLES ( CHANGED IN PLACE )
C  RETURN 1 IF NOT ACCEPTED.
C-----------------------------------------------------------
C
      IMPLICIT INTEGER*2 (H)
      COMMON / BCS / IW(1)
      DIMENSION RW(1),HW(1)
      EQUIVALENCE (HW(1),RW(1),IW(1))
C
      COMMON P(10,100)
C
      DIMENSION JLONG(2)
C
      DIMENSION PCENTI(20)
      INTEGER NREJ(20) / 20*0 /
      DATA PMINC / .05/, COSS /.9703/, COSL /.92 /
      DATA ELGCUT / .05/, COSBAR /.793/, COSENL /.97/, COSENU /.89/
      DATA EMINIO / .25 /
C                                           EXTRA TRACK CUTS
      DATA ELGPHM /.15/
      DATA PMIN  / .1 /
C

      DATA NCALL / 0 /, NACC / 0 /
      DATA EBEAMI / 1.E10 /, EBEAMA / 0. /
      DATA IHIST / 0 /
C
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
C
      NCALL = NCALL + 1
C
  100 CONTINUE
C                                           ENERGY
  110 CONTINUE
      EBEA = EBGENL( 0 )
      IF( EBEA .LT. EBEAMI ) EBEAMI = EBEA
      IF( EBEA .GT. EBEAMA ) EBEAMA = EBEA
C
C                                           CHARGED TRACKS
      IF( N .GT. 0 ) GO TO 210
      NREJ( 3) = NREJ( 3) + 1
      GO TO 8000
C
  210 SUMPL = 0.
      SUMLZ = 0.
      SUMP = 0.
      SUMZ = 0.
      NTRKL = 0
      NTRK = 0
      RMIN1 = 10000.
      RMIN2 = 10000.
      ZMIN = 1000000.
      AZMIN = 1000000.
      EBAR = 0.
      ENDP = 0.
      ENDM = 0.
      SHWZ = 0.
C
C                                           LOOP OVER TRACKS
      J1 = NST + 1
      JN = NST + N
      JCOPY = NST
      DO 1000 J=J1, JN
      ACOSZ = ABS(P(3,J))/ P(6,J)
C     P(10,J) = J - NST
      IF( P(7,J) .EQ. 0. ) GO TO 1500
C                                           CHARGED
      IF( P(6,J) .LT. PMINC ) GO TO 1000
      IF( ACOSZ .GT. COSS ) GO TO 1000
      NTRK = NTRK + 1
      ZMIN = 0.
      SUMP = SUMP + P(6,J)
      SUMZ = SUMZ + P(3,J )
C                                           LG FOR CHARGED=MIN IONI.
      EZ = EMINIO * P(3,J) / P(6,J)
      IF( ACOSZ .GT. COSBAR ) GO TO 1420
      EBAR = EBAR + EMINIO
      SHWZ = SHWZ+ EZ
      GO TO 1440
C
 1420 IF( ACOSZ.GT.COSENL .OR. ACOSZ .LT.COSENU ) GO TO 1440
      SHWZ = SHWZ + EZ
      IF( EZ .LT. 0. ) GO TO 1430
      ENDP = ENDP + EMINIO
      GO TO 1440
 1430 ENDM = ENDM + EMINIO
 1440 CONTINUE
C                                           I.D.CUTS
      IF( ACOSZ .GT. COSL .OR. P(6,J).LT.PMIN ) GO TO 1900
      NTRKL = NTRKL + 1
      SUMPL = SUMPL + P(6,J)
      SUMLZ = SUMLZ + P(3,J)
      IF( NTRKL .GT. 2 ) GO TO 1900
      JLONG(NTRKL) = JCOPY + 1
      GO TO 1900
C
C                                           NEUTRALS
 1500 CONTINUE
      IF( P(5,J) .GT. .0001  ) GO TO 1000
C                                           ONLY GAMMA HERE
      IF( P(6,J) .LT. ELGCUT ) GO TO 1000
      IF( ACOSZ .GT. COSBAR ) GO TO 1520
      EBAR = EBAR + P(6,J)
      SHWZ = SHWZ+ P(3,J)
      GO TO 1900
C
 1520 IF( ACOSZ.GT.COSENL .OR. ACOSZ .LT.COSENU ) GO TO 1000
      SHWZ = SHWZ + P(3,J)
      IF( P(3,J) .LT. 0. ) GO TO 1530
      ENDP = ENDP + P(6,J)
      GO TO 1540
 1530 ENDM = ENDM + P(6,J)
 1540 CONTINUE
C
 1900 CONTINUE
C                                           EXTRA TRACK CUTS (NOT COUNTE
      IF( P(7,J) .EQ. 0. ) GO TO 1910
      IF( P(6,J) .LT. PMIN   ) GO TO 1000
      GO TO 1920
 1910 IF( P(6,J) .LT. ELGPHM ) GO TO 1000
 1920 CONTINUE
C
      JCOPY = JCOPY + 1
      IF( JCOPY .EQ. J ) GO TO 1000
      CALL PCOP( J, J, JCOPY )
C
 1000 CONTINUE
      N = JCOPY - NST
C
      IF( NTRK .GT. 0 ) GO TO 220
      NREJ(11) = NREJ(11) + 1
      GO TO 8000
C
  220 CONTINUE
C
      IF( IHIST .EQ. 1 ) CALL UHIST( 10001, FLOAT(NTRKL) )
      IF( IHIST .EQ. 1 ) CALL UHIST( 10002, FLOAT(NTRK) )
      IF( IHIST .EQ. 1 ) CALL UHIST( 10005, SUMPL )
C
C                                           PATR CUTS
      IF( NTRKL .GE. 2 .AND. NTRK .GE. 4 ) GO TO 310
      NREJ( 5) = NREJ( 5) + 1
      GO TO 8000
C
  310 CONTINUE
C
  320 IF( ABS(ZMIN) .LE. 400. ) GO TO 330
      NREJ( 7) = NREJ( 7) + 1
      GO TO 8000
C
  330 CONTINUE
C                                           ACOPLANARITY
      ACOP = 1.5
      IF( NTRKL .NE. 2 ) GO TO 400
      ACOP = ( P(1,JLONG(1))*P(1,JLONG(2)) +
     *         P(2,JLONG(1))*P(2,JLONG(2)) )/
     *       SQRT( ( P(1,JLONG(1))*P(1,JLONG(1)) +
     *               P(2,JLONG(1))*P(2,JLONG(1)) ) *
     *             ( P(1,JLONG(2))*P(1,JLONG(2)) +
     *               P(2,JLONG(2))*P(2,JLONG(2)) ))
      IF( IHIST .EQ. 1 ) CALL UHIST( 10004, ACOP )
      IF( ACOP .GT. -.9848 ) GO TO 400
      NREJ( 8) = NREJ( 8) + 1
      GO TO 8000
C
C
  400 CONTINUE
C
  410 CONTINUE
      ETOT = EBAR + ENDP + ENDM
      ESUM = SUMP + ETOT
      ESUML = SUMPL + ETOT
      IF( IHIST .EQ. 1 ) CALL UHIST( 10006, ETOT )
      IF( IHIST .EQ. 1 ) CALL UHIST( 10007, EBAR )
      IF( IHIST .EQ. 1 ) CALL UHIST( 10008, ESUM )
      IF( IHIST .EQ. 1 ) CALL UHIST( 10009, ESUML )
      IF( EBAR .GT. 3. ) GO TO 420
      IF( ENDM.GT..4 .AND. ENDP.GT..4 ) GO TO 420
      NREJ( 9) = NREJ( 9) + 1
      GO TO 8000
C
  420 CONTINUE
C
      ETOTSL = SUMPL + ETOT
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
C
      ENTRY MCRDVF
C----------------------------------------------------------
C ENTRY TO PRINT STATISTICS
C TO BE CALLED AT END OF JOB .
C----------------------------------------------------------
      IF( NCALL .LE. 0 ) RETURN
      PCBAS = MAX0( NCALL, 1 ) * .01
      PCENT = NACC / PCBAS
      PCENTE = SQRT( FLOAT(NCALL - NACC) ) / PCBAS
      DO 8101 I=1,14
      PCENTI(I) = NREJ(I) / PCBAS
 8101 CONTINUE
      WRITE(6,9101) NCALL, NACC, PCENT, PCENTE, EBEAMI, EBEAMA
 9101 FORMAT(//10X,' SUMMARY FOR THIS FILE'/
     *       10X,'   EVENTS ANALYSED ',I8/
     *       10X,'   EVENTS ACCEPTED ',I8,'=',F7.2,' % +-',F5.2,' %',
     *                                   ,20X,F7.3,'=< EBEAM =<',F7.3,
     *           '  TAKEN FROM EBGENL'/)
      WRITE(6,9102) (NREJ(I),PCENTI(I),I=1,14)
 9102 FORMAT(10X,' REASONS FOR REJECTION'/
     */10X,'OBSOLETE                 ',I8,' =',F7.1,' %',
     */10X,'OBSOLETE                 ',I8,' =',F7.1,' %',
     */10X,'OBSOLETE                 ',I8,' =',F7.1,' %',
     */10X,'OBSOLETE                 ',I8,' =',F7.1,' %',
     */10X,'NTRKL < 2 AND NTRK < 4   ',I8,' =',F7.1,' %',
     */10X,'OBSOLETE                 ',I8,' =',F7.1,' %',
     */10X,'ZMIN > 400.              ',I8,' =',F7.1,' %',
     */10X,'ACOPL > 10 DEG           ',I8,' =',F7.1,' %',
     */10X,'E - BARREL < 3 GEV       ',
     */10X,'E+ OR E- < 400 MEV       ',I8,' =',F7.1,' %',
     */10X,'MOM BALANCE < .4         ',I8,' =',F7.1,' %',
     */10X,'NO TRACKS                ',I8,' =',F7.1,' %',
     */10X,'TOO MANY TRACKS          ',I8,' =',F7.1,' %',
     */10X,'OBSOLETE                 ',I8,' =',F7.1,' %',
     */10X,'ETOTSL < EBEAM           ',I8,' =',F7.1,' %')
C
C                                           CLEAR COUNTERS
      NACC = 0
      NCALL = 0
      CALL VZERO( NREJ, 20 )
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
      CALL BDLS( 'HST*',  10001 )
      CALL BDLS( 'HST*',  10002 )
      CALL BDLS( 'HST*',  10003 )
      CALL BDLS( 'HST*',  10004 )
      CALL BDLS( 'HST*',  10005 )
      CALL BDLS( 'HST*',  10006 )
      CALL BDLS( 'HST*',  10007 )
      CALL BDLS( 'HST*',  10008 )
      CALL BDLS( 'HST*',  10009 )
      CALL BDLS( 'HST*',  10015 )
      CALL BDLS( 'HST*',  10020 )
      CALL BDLS( 'HST*',  10021 )
      CALL BDLS( 'HST*',  10022 )
      CALL BDLS( 'HST*',  10023 )
      CALL BDLS( 'HST*',  10024 )
      CALL BDLS( 'COR*',  10004 )
 8900 RETURN
      END
