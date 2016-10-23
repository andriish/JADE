C   27/11/86 704161229  MEMBER NAME  ZE4VJBN  (S)           FORTRAN77
C
C   VERSION OF 04/02/85    E. ELSEN
C   MODIFIED   12/01/87    M. ZIMMER
C   MODIFIED   31/03/87    G. ECKERLIN
C   LAST MOD   16/04/87    G. ECKERLIN   EVENT SCIP AND SELECTION
C   COMPRESS EVENTBANKS INTO ZE4V FORMAT
C   WITH ZE4VTP FROM TPTR AS FAR AS POSSIBLE
C   WITH ZE4VPK FROM PATR AND LGCL BANK
C-----------------------------------------------------------
C
      IMPLICIT INTEGER*2 (H)
      COMMON / BCS / IW(80000)
      DIMENSION HW(1),RW(1)
      EQUIVALENCE (HW(1),IW(1),RW(1))
C
      DATA IEVLIM / 10000/
      DATA IFILMX / 1000 /
      DATA IWRMAX /  0  /
      DATA IWZMAX / 10000 /
      DATA ISCIP / 0 /
C                  ZE4V DUMP FOR 'IDULIM' EVENTS
      DATA IDUMP / 1 /,IDULIM / 10 /
C                  FLAG TO SELECT ZE4VTP=1/ZE4VPK=2
      DATA ITPPK / 2 /
C
      DIMENSION HDSN(22), HVOL(3)
      REAL*8 DDIN / 'FT02F001' /
      REAL*8 DDOU / 'FT03F001' /
      REAL*8 DDZE / 'FT51F001' /
      INTEGER IDDIN(2)
      EQUIVALENCE(IDDIN(1),DDIN)
C                                      INPUT/OUTPUT FTXX..
C          INPUT   (2)   EVT-OUT (3)   ZE4V-OUT (51)
      DATA IRUNIT / 2 /, IWUNIT / 0 /, IUNZE4 /  51 /
C                                   READ IN DD* CARDS
      READ(5,5001) ITPPK,IEVLIM,IFILMX,IWRMAX,IWZMAX,IDULIM,ISCIP,
     &             ISELFL,ISELMU,ISELEL
 5001 FORMAT(20X,I10)
      WRITE(6,6001) IEVLIM,IFILMX,IWRMAX,IWZMAX,IDULIM,ISCIP,ITPPK,
     &             ISELFL,ISELMU,ISELEL
 6001 FORMAT(/' MAX # OF EVENTS TO READ   :',I8/
     &         ' MAX # OF INPUTFILES       :',I8/
     &         ' MAX # OF EVENTS ON UNIT 3 :',I8/
     &         ' MAX # OF EVENTS ON UNIT51 :',I8/
     &         ' MAX # OF ZE4V-BANK PRINTS :',I8/
     &         ' INITIAL EVENT SCIP        :',I8/
     &         ' TP/PK-FLAG (1=TP/2=PK)    :',I8/
     &         ' SELECTION (0:ENAB/1:DISAB):',I8/
     &         ' MUON QUALITY SELECTION    :',I8/
     &         ' ELEKTRON SELECTION        :',I8/)
C                                          INIT BOS AND GEP
      CALL BINT( 80000, 40000, 10000, 0 )
      CALL GEPIC
C                                           I/O UNIT SPECIFICATION
      CALL DDCHA( DDIN, HDSN, HERR, HVOL )
      WRITE(6,9111) HDSN, HVOL
 9111 FORMAT(/'      INPUT IS FROM DSN=',22A2,' VOL=',3A2)
      IF( IWUNIT.LE.0 ) GO TO 10
         CALL DDCHA( DDOU, HDSN, HERR, HVOL )
         WRITE(6,9112) HDSN, HVOL
 9112    FORMAT(/'      OUTPUT IS TO  DSN=',22A2,' VOL=',3A2)
   10 CONTINUE
C
      IF( IUNZE4.LE.0 ) GO TO 20
         CALL DDCHA( DDZE, HDSN, HERR, HVOL )
         WRITE(6,9122) HDSN, HVOL
 9122    FORMAT(/' ZE4V OUTPUT IS TO  DSN=',22A2,' VOL=',3A2/)
   20 CONTINUE
C
      IFIL = 1
      IEOF = 0
      IWRITE = 0
      IEV = 0
      IZE4V = 0
      IWZE4V = 0
C                                      INITIALISE FEHLER
      CALL FEHLIN
C
  100 IF( JUHR(2) .EQ. 2 ) GO TO 4000
      CALL BREAD( IRUNIT, &1000, &3000 )
CC    WRITE(6,*) '******* READ ENDED NORMALLY FOR EVENT ',IEV
      IEOF = 0
      IEV = IEV + 1
C                                      LOOK FOR HEAD BANK
      NPHEAD = IW(IBLN('HEAD'))
      IF( NPHEAD.LE.0 ) GO TO 200
C                                      INITIAL EVENT SCIP
      IF (IEV.LE.ISCIP) GOTO 2000
C                                      FILL CALIBRATION CONSTANTS
      CALL KLREAD
C                                      FILL ZE4V OVER TP-BANK
C
      IF (ITPPK.EQ.1) THEN
        NPTPEV = IW(IBLN('TPEV'))
        IF( NPTPEV.LE.0 ) GO TO 200
        IF (IZE4V.LE.0) THEN
          CALL ZE4VIN( 1 )
          IZE4V = 1
        ELSE
          CALL ZE4VTP( 1 )
        ENDIF
C                                      FILL ZE4V FROM PATR-BANK
      ELSEIF (ITPPK.EQ.2) THEN
        IF (IZE4V.LE.0) THEN
          CALL ZE4VIN( 1 )
          IZE4V = 1
        ELSE
          CALL ZE4VPK( 1 )
        ENDIF
      ELSE
        WRITE(6,9001) ITPPK
 9001   FORMAT(1X'FLAG NOT SUPPORTET : ',I4)
        GOTO 8100
      ENDIF
C
      IF (IDUMP.LE.IDULIM) THEN
        CALL ZE4VDP
        IDUMP = IDUMP + 1
      ENDIF
  200 CONTINUE
C
C                                          WRITE WHOLE EVENT
      IF( IWUNIT .GT. 0 .AND. IWRITE .LT. IWRMAX ) THEN
        IWRITE = IWRITE + 1
        CALL BSLW
        CALL BWRITE( IWUNIT )
      ENDIF
C
C                                          SCIP ZE4V WRITE IF MC RECORD
      IF( NPHEAD.LE.0 ) GO TO 2000
C                                          WRITE ZE4V BANK
      IF( IUNZE4 .GT. 0 .AND. IWZE4V .LT. IWZMAX ) THEN
        IWZE4V = IWZE4V + 1
C                                          EVENT SELECTION
        IACCFL=ISELFL
        NPZE4V=IW(IBLN('ZE4V'))
        IF (NPZE4V.GT.0) THEN
          MAXTR = HW(NPZE4V*2 + 6)
          LT = HW(NPZE4V*2 + 5)
          LTCH = HW(NPZE4V*2 + 7)
          LTNE = HW(NPZE4V*2 + 9)
          LTRE = HW(NPZE4V*2 + 11)
          NPOINT = NPZE4V + HW(NPZE4V*2 + 1)
CC      WRITE (6,*) 'EVENT :',IEV,'TRACKS :',MAXTR
          DO 111 ITR= 1, MAXTR
            IF (HW(NPOINT*2+18).EQ.1) THEN
              IF (IW(NPOINT+LT+12).GE.ISELMU.AND.ISELMU.GT.0) IACCFL=1
CC      WRITE (6,*) 'ITR,NPOINT,IACCFL,IW(NPOINT+LT+12) :',
CC   &               ITR,NPOINT,IACCFL,IW(NPOINT+LT+12)
              NPOINT = NPOINT + LTCH
            ELSEIF (HW(NPOINT*2+18).EQ.0) THEN
              NPOINT = NPOINT + LTNE
            ELSEIF (HW(NPOINT*2+18).EQ.2) THEN
              NPOINT = NPOINT + LTRE
            ENDIF
            NPOINT = NPOINT + LT
 111      CONTINUE
        ENDIF
C                                          FORCED ACCEPT OF FIRST EVENT
        IF (IW(IBLN('ZEHD')).GT.0) IACCFL=1
C                                          SCIP REJECTED EVENTS
        IF (IACCFL.LE.0) GOTO 2000
CC      WRITE (6,*) ' *******  EVENT ACCEPTED ***********',IEV
        CALL BMLT( 3, 'ZEHDZE4VPALL' )
CC      CALL BPRL(IDULIM)
        CALL BWRITE( IUNZE4 )
      ENDIF
      GO TO 2000
C
C
C                                           ERRORS
 1000 CONTINUE
      CALL JOBFIN( 3, IEV, &1100 )
 1100 IF( IEOF .NE. 0 ) GO TO 8000
C
C
 2000 CONTINUE
      CALL BSLT
      CALL BDLG
      IF( IEV .LT. IEVLIM ) GO TO 100
      CALL JOBFIN( 4, IEV, &8100 )
C
C                                           EOF ON IRUNIT
 3000 IF( IFIL .GE. IFILMX .OR. IEOF.NE.0 )  GO TO 8000
      IFIL = IFIL + 1
      IEOF = 1
      IDDIN(2) = IDDIN(2) + 1
      CALL DDCHA( DDIN, HDSN, HERR, HVOL )
      IF( HERR .NE. 0 ) GO TO 8000
      CALL BDLS( '+BUF', IRUNIT )
      WRITE(6,9104) IEV, IFIL
 9104 FORMAT(' +++++ AFTER',I6,' EVENTS FILE NUMBER CHANGED TO',I4,' +++
     *++')
      WRITE(6,9113) HDSN, HVOL
 9113 FORMAT(/' +++++ INPUT CONTINUES FROM DSN=',22A2,' VOL=',3A2)
      GO TO 100
C
C                                        TIME OUT
 4000 CALL JOBFIN( 2, IEV, &8100 )
C
C                                           END OF JOB
 8000 CONTINUE
      CALL JOBFIN( 1, IEV, &8100 )
 8100 CONTINUE
C                                      MCREDU STATISTICS
      CALL FEHLPR
      CALL MCRDFL
      CALL BSTA
      CALL GEPW
C
      STOP
      END
