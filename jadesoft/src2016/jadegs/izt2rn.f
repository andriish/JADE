C   21/08/87 901111223  MEMBER NAME  IZT2RN   (JADEGS)   M  FORTRAN77
      FUNCTION IZT2RN( DUMMY )
C----------------------------------------------------------
C  Version of 06/11/87       Last Mod 06/11/87   E Elsen
C  Convert event time (HEAD Bank) into run number for
C  first events. Subsequent calls will return the same
C  run number.
C  Not all runs have been correlated.
C  IZT2RN = Equivalent run number.
C----------------------------------------------------------
      IMPLICIT INTEGER*2 (H)
      COMMON / BCS / IW(1)
      DIMENSION HW(1),RW(1)
      EQUIVALENCE (HW(1),IW(1),RW(1))
C                                           FIRST RUNS CORRESPONDING TO
C                                           K.AMBRUS PERIODS
      INTEGER IRUN(39) /
     *   400, 2600, 3730, 6000, 6850, 7170, 7592, 7840, 8120, 8375,
     *  8850, 9188, 9464,10000,10391,10578,10761,11021,11331,11634,
     * 12004,12341,13088,14605,15690,16803,17989,19068,21123,21705,
     * 22651,23443,23798,24201,25454,26217,27000,27938,28444/
C                                           CORRESPONDING TIMES
C                                           THE FIRST PERIODS HAVE NOT
C                                           BEEN CORRELATED YET
      INTEGER ITIM(39) /
     *  97816480,  97816480,  97816480,  97816480,  97816480,  97816480,
     *  97816480,  97816480,  97816480,  97816480,  97816480,  97816480,
     *  97816480,  97816480,  99482653, 100603789, 101329523, 105019795,
     * 106819543, 109577623, 111073856, 112319826, 135611935, 149456837,
     * 159725022, 171437703, 177893401, 194822761, 202803459, 206147941,
     * 210908637, 213476746, 214678727, 225055662, 230412916, 233447245,
     * 236008290, 239739240, 241204057/
      INTEGER NOW / 0 /, INITL / 0 /, ITIME(6)
      INTEGER NRUN / 25000 /
      LOGICAL FIRST / .TRUE. /
C
      IF( FIRST ) THEN
        FIRST = .FALSE.
        NPHEAD = IW(IBLN('HEAD'))
        IF( NPHEAD .GT.0 ) THEN
          DO 1 I=1,6
            ITIME(I) = HW(NPHEAD*2+2+I)
    1     CONTINUE
          IEVTIM = KTMCON( ITIME, NOW, INITL )
          IPER = 1
    2     CONTINUE
          IF( IPER.LT.39 .AND. ITIM(IPER).LT.IEVTIM ) THEN
            IPER = IPER + 1
            GO TO 2
          ENDIF
          NRUN = IRUN(IPER)
          WRITE(6,9601) IPER, NRUN, IEVTIM
 9601     FORMAT(' +++ IZT2RN: Selecting K.AMBRUS Period',I3,
     *           ' with first run',I6,
     *                       ' recorded',I11,' secs after',
     *           ' beginning of 1/1/1979')
        ENDIF
      ENDIF
      IZT2RN = NRUN
      END
