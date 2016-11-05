C   21/08/87 804271518  MEMBER NAME  EAMCDE   (S)           FORTRAN77
      SUBROUTINE EAMCDE( NP, IVECT, DEDX, SDEDX, NHDEDX )
C----------------------------------------------------------
C  VERSION OF 21/08/87       LAST MOD 27/04/88   MZ
C  Calculate dE/dx and sigma(dE/dx) for track specified in
C  Bank ZE4V at position NP (=absolute BOS pointer)
C
C  Input:
C           NP = Pointer to track in ZE4V
C           IVECT = Vect # and particle # in bank
C  Output:
C           DEDX  = dE/dx and
C           SDEDX =  error
C           NHDEDX =  Hits used in dE/dx calculation
C----------------------------------------------------------
      IMPLICIT INTEGER*2 (H)
      COMMON / BCS / IW(1)
      DIMENSION HW(1),RW(1)
      DIMENSION HR(2)
      EQUIVALENCE (HW(1),IW(1),RW(1)), (HR(1),IR)
C                                           FIRST RUNS CORRESPONDING TO
C                                           K.AMBRUS PERIODS
      INTEGER IRUN(39) /
     +   400, 2600, 3730, 6000, 6850, 7170, 7592, 7840, 8120, 8375,
     +  8850, 9188, 9464,10000,10391,10578,10761,11021,11331,11634,
     + 12004,12341,13088,14605,15690,16803,17989,19068,21123,21705,
     + 22651,23443,23798,24201,25454,26217,27000,27938,28444/
C                                           CORRESPONDING TIMES
C                                           THE FIRST PERIODS HAVE NOT
C                                           BEEN CORRELATED YET
      INTEGER ITIM(39) /
     +  97816480,  97816480,  97816480,  97816480,  97816480,  97816480,
     +  97816480,  97816480,  97816480,  97816480,  97816480,  97816480,
     +  97816480,  97816480,  99482653, 100603789, 101329523, 105019795,
     + 106819543, 109577623, 111073856, 112319826, 135611935, 149456837,
     + 159725022, 171437703, 177893401, 194822761, 202803459, 206147941,
     + 210908637, 213476746, 214678727, 225055662, 230412916, 233447245,
     + 236008290, 239739240, 241204057/
      INTEGER NOW / 0 /, INITL / 0 /, ITIME(6)
      INTEGER NRUN / 25000 /
      LOGICAL FIRST / .TRUE. /
C
      IF( FIRST ) THEN
        FIRST = .FALSE.
        IPZE4V = IBLN('ZE4V')
        NPHEAD = IW(IBLN('HEAD'))
        IF( NPHEAD .GT.0 ) THEN
          DO 1 I=1,6
            ITIME(I) = HW(NPHEAD*2+2+I)
    1     CONTINUE
          IEVTIM = KTMCON( ITIME, NOW, INITL )
          IPER = 1
          DO 2 WHILE( IPER.LT.39 .AND. ITIM(IPER).LT.IEVTIM )
            IPER = IPER + 1
    2     CONTINUE
          NRUN = IRUN(IPER)
          WRITE(6,9601) IPER, NRUN, IEVTIM
 9601     FORMAT(' +++ EAMCDE: Selecting K.Ambrus Period',I3,
     +           ' with first run',I6/
     +           '             recorded',I11,' secs after',
     +           ' beginning of 1/1/1979')
        ENDIF
      ENDIF
C
      DEDX = 0.
      SDEDX = 0.
C
      NPZE4V = IW(IPZE4V)
      IF( NPZE4V .GT. 0 ) THEN
        LT = HW(NPZE4V*2+5)
        NRPHZ = HW((NP+LT)*2+10)
C                                           HITS IN R-PHI FIT
        NRZ = NRPHZ/100
C                                           SET NHDEDX=NRZ
        NHDEDX = MAX( NRZ, 1 )
C                                           PARTICLE TYPE
        IPART = HW(NP*2+8)/100
        IR = IVECT
        NRVECT = HR(1)
        NR     = HR(2)
        CALL CLOC( NPVECT, 'VECT', NRVECT )
        IF( IPART .GT. 0 .AND. NPVECT.GT.0 .AND. NR.GT.0 ) THEN
C                                           TAKE ORIGINAL MOMENTUM FROM
C                                           VECT BANK
          J = NPVECT + IW(NPVECT+1) + (NR-1)*IW(NPVECT+2)
          PTOT = SQRT( RW(J+1)**2+RW(J+2)**2+RW(J+3)**2 )
C
        ELSE
          PTOT = RW(NP+6)
        ENDIF
        CALL DXMCGN( PTOT, NHDEDX, IPART, NRUN, DEDX, SDEDX, DEDXT )
      ENDIF
      END
