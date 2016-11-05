      PROGRAM ZE4VJB
      IMPLICIT NONE
C   27/11/86 704161229  MEMBER NAME  ZE4VJBN  (S)           FORTRAN77
C
C   VERSION OF 04/02/85    E. ELSEN
C   MODIFIED   12/01/87    M. ZIMMER
C   MODIFIED   31/03/87    G. ECKERLIN
C   LAST MOD   16/04/87    G. ECKERLIN   EVENT SKIP AND SELECTION
C
C   Last mod   09/12/00    P. Movilla Fernandez
C                          Adapt for use on new platforms
C   Last mod   05/06/00    P. Movilla Fernandez
C                          Introduce IMPLICIT NONE, declare
C                          variables explicitly
C                          Reorganize I/O, use FFREAD cards
C                          Introduce readable print out of event BOS banks 
C                          Introduce handling of random number seeds
C
C-----------------------------------------------------------
C   COMPRESS EVENTBANKS INTO ZE4V FORMAT
C   WITH ZE4VTP FROM TPTR AS FAR AS POSSIBLE
C   WITH ZE4VPK FROM PATR AND LGCL BANK
C-----------------------------------------------------------
C
C BOS
      INTEGER IW
      INTEGER*2 HW
      REAL RW
      COMMON / BCS / IW(80000)
      DIMENSION HW(80000*2),RW(80000)
      EQUIVALENCE (HW(1),IW(1),RW(1))
C Some counter limits
      INTEGER IEVLIM,IFILMX,IWRMAX,IWZMAX,ISCIP,ITPPK
      DATA IEVLIM / 10000/, IFILMX / 1 /, IWRMAX /  0  /
     >     IWZMAX / 10000 /, ISCIP / 0 /
C ZE4V DUMP FOR 'IDULIM' EVENTS
      INTEGER IDUMP,IDULIM
      DATA IDUMP/ 1 /, IDULIM / 10 /
C FLAG TO SELECT ZE4VTP=1/ZE4VPK=2
      DATA ITPPK/ 2 /
C Some old fashioned IBM stuff
      INTEGER*2 HDSN,HVOL
      DIMENSION HDSN(22), HVOL(3)
      DATA HVOL /2H  ,2H  ,2H  /
      REAL*8 DDIN, DDOU, DDZE
      DATA DDIN/8HFT02F001/, DDOU/8HFT03F001/, DDZE/8HFT51F001/
      INTEGER IDDIN(2)
      EQUIVALENCE(IDDIN(1),DDIN)
C INPUT/OUTPUT FTXX..
C          INPUT   (2)   EVT-OUT (3)   ZE4V-OUT (51)
      INTEGER IRUNIT,IWUNIT,IUNZE4
      DATA IRUNIT/ 2 /, IWUNIT/ 3 /, IUNZE4/ 51 /
C
C --- PMF: new stuff:
C
C I/O parameters set in BLOCK DATA JADEBD
      INTEGER IUNIT,JUNIT,NCALI,KUNITA,LUNITA
      COMMON/CIOUNI/IUNIT,JUNIT,NCALI,KUNITA(10),LUNITA(10)
C Flags for reading the calibration constants in klread
      INTEGER LBMC
      COMMON /CMCCAL/ LBMC(17)
C FFREAD
      REAL SPACE
      INTEGER FFSPAC
      PARAMETER (FFSPAC=20000)
      COMMON / CFREAD / SPACE(FFSPAC)
C Additional I/O options
      INTEGER IOBANK
      LOGICAL LBANK
      DATA IOBANK/41/
C Misc
      INTEGER NAMLEN,NBYTEW,NWNAM
      PARAMETER( NAMLEN=120, NBYTEW=4, NWNAM=NAMLEN/NBYTEW )
      INTEGER  SVBOSF(NWNAM),ZBOSF(NWNAM),ZE4VF(NWNAM),BANKF(NWNAM)
     >     ,CALF(NWNAM,3)
      CHARACTER*(NAMLEN) CSVBOSF,CZBOSF,CZE4VF,CBANKF,CCALF(3),CCALFU
      LOGICAL LEXIST,LEX(3),LSTART
      CHARACTER CSEEDF*9
      DATA CSEEDF / 'ze4v.stat' /
      INTEGER INDEX,LENOCC,IERR
      DATA IERR/0/
      INTEGER HBANK(NWNAM)
      CHARACTER*(NAMLEN) CBANK
      INTEGER IHEAD,IEVT,MOD
C Some more explicit declarations 
      INTEGER*2 HERR
      INTEGER I,LT,LTCH,LTNE,LTRE,IACCLF,NPZE4V,MAXTR,IACCFL,NPTPEV
     >     ,IBLN,ITR,NPOINT,ISELFL,ISELEL,ISELMU,IEOF,IWZE4V,JUHR,IZE4V
     >     ,IWRITE,IEV,NPHEAD,IFIL
C
C------------- C o d e ------------------------------------
C

C READ IN DD* CARDS
***PMF      READ(5,5001) ITPPK,IEVLIM,IFILMX,IWRMAX,IWZMAX,IDULIM,ISCIP,
***PMF     &             ISELFL,ISELMU,ISELEL
***PMF 5001 FORMAT(20X,I10)

C ------- PMF 05/06/00 --------------

C Read FFREAD cards (PMF 05/06/00)
      WRITE (*,600)
 600  FORMAT( /,' *',3(7('*'),16('*'),7('*')),'*'/
     +       ' *',3(7X,16X,7X),'*'/
     +       ' *',3(7X,'Z E 4 V    J O B',7X),'*'/
     +       ' *',3(7X,16X,7X),'*'/
     +       ' *',3(7X,'VERSION 05/06/00',7X),'*'/
     +       ' *',3(7X,16X,7X),'*'/
     +       ' *',3(7('*'),16('*'),7('*')),'*'/  //       )
C... Initialise FFREAD cards
      CALL FFINIT(0)
      CALL FFSET('SIZE',8)
C... Read FFREAD cards  
      WRITE(*,'(A)') 'ZE4VJB: read FFREAD cards'
      ITPPK=2
      CALL FFKEY('TPPK',ITPPK,1,'INTE')
      LSTART=.FALSE.
      CALL FFKEY('ZSTART',LSTART,1,'LOGI')
      IEVLIM=999999
      CALL FFKEY('EVTREAD',IEVLIM,1,'INTE')
      IWRMAX=10
      CALL FFKEY('EVTOUT',IWRMAX,1,'INTE')
      IWZMAX=IEVLIM
      CALL FFKEY('ZE4VOUT',IWZMAX,1,'INTE')
      IDULIM=30
      CALL FFKEY('ZE4VPRT',IDULIM,1,'INTE')
      ISCIP=0
      CALL FFKEY('EVTSKP',ISCIP,1,'INTE')
      ISELFL=1
      CALL FFKEY('SELECT',ISELFL,1,'INTE')
      ISELMU=0
      CALL FFKEY('MSELCT',ISELMU,1,'INTE')
      ISELEL=0
      CALL FFKEY('ESELCT',ISELEL,1,'INTE')
      LBANK=.FALSE.
      CALL FFKEY('BANKS',LBANK,1,'LOGI')
      CALL VBLANK(HBANK,NWNAM)
      CALL FFKEY('CONT',HBANK,NWNAM,'MIXED')
C... File names
      CALL VBLANK(SVBOSF,NWNAM)
      CALL FFKEY('SVBOS',SVBOSF,NWNAM,'MIXED')
      CALL VBLANK(ZE4VF,NWNAM)
      CALL FFKEY('ZE4V',ZE4VF,NWNAM,'MIXED')
      CALL VBLANK(ZBOSF,NWNAM)
      CALL FFKEY('ZBOS',ZBOSF,NWNAM,'MIXED')
      CALL VBLANK(BANKF,NWNAM)
      CALL FFKEY('ZBANK',BANKF,NWNAM,'MIXED')
      CALL VBLANK(CALF(1,1),NWNAM)
      CALL FFKEY('AUPDAT0',CALF(1,1),NWNAM,'MIXED')
      CALL VBLANK(CALF(1,2),NWNAM)
      CALL FFKEY('BUPDAT0',CALF(1,2),NWNAM,'MIXED')
      CALL VBLANK(CALF(1,3),NWNAM)
      CALL FFKEY('BUPDAT1',CALF(1,3),NWNAM,'MIXED')
      CALL FFGO
C... Convert hollerith to character
      CALL UHTOC(CALF,NBYTEW,CCALF,NAMLEN*3)
      CALL UHTOC(HBANK,NBYTEW,CBANK,NAMLEN)
      CALL UHTOC(ZBOSF,NBYTEW,CZBOSF,NAMLEN)
      CALL UHTOC(ZE4VF,NBYTEW,CZE4VF,NAMLEN)
      CALL UHTOC(SVBOSF,NBYTEW,CSVBOSF,NAMLEN)
      CALL UHTOC(BANKF,NBYTEW,CBANKF,NAMLEN)
C... Check some parameters
      IEVLIM=MAX(0,IEVLIM)
      IWZMAX=MAX(0,IWZMAX)
      IWRMAX=MAX(0,IWRMAX)
      ISCIP=MAX(0,ISCIP)
      IF( IEVLIM.EQ.0 ) IEVLIM=999999
      IF( IWZMAX.EQ.0 ) IWZMAX=999999
      IF( IWRMAX.EQ.0 ) IWRMAX=999999
C                                           I/O UNIT SPECIFICATION (reorganized)
*PMF      CALL DDCHA( DDIN, HDSN, HERR, HVOL )
*PMF       WRITE(6,9111) HDSN, HVOL
*PMF  9111 FORMAT(/'      INPUT IS FROM DSN=',22A2,' VOL=',3A2)
*PMF       IF( IWUNIT.LE.0 ) GO TO 10
*PMF          CALL DDCHA( DDOU, HDSN, HERR, HVOL )
*PMF          WRITE(6,9112) HDSN, HVOL
*PMF  9112    FORMAT(/'      OUTPUT IS TO  DSN=',22A2,' VOL=',3A2)
*PMF    10 CONTINUE
C
*PMF       IF( IUNZE4.LE.0 ) GO TO 20
*PMF          CALL DDCHA( DDZE, HDSN, HERR, HVOL )
*PMF          WRITE(6,9122) HDSN, HVOL
*PMF  9122    FORMAT(/' ZE4V OUTPUT IS TO  DSN=',22A2,' VOL=',3A2/)
*PMF    20 CONTINUE

C... Input file with JADE events in BOS format
      INQUIRE(FILE=CSVBOSF,EXIST=LEXIST)
      IF( LEXIST ) THEN
         OPEN(IUNIT,FILE=CSVBOSF,STATUS='UNKNOWN',FORM='UNFORMATTED')
      ELSE
         WRITE(*,'(A)') 'ZE4VJB: Input BOS file does not exist!'
         WRITE(*,'(A)') '... will stop now!'
         GOTO 9999
      ENDIF
      OPEN(IUNIT,FILE=CSVBOSF,STATUS='OLD',FORM='UNFORMATTED')
C... Calibration files
      DO I=1,3
         CCALFU=CCALF(I)
         CALL CLTOU(CCALFU)
         IF( I.EQ.1 ) LEX(I)=(INDEX(CCALFU,'AUPDAT1').NE.0)
         IF( I.EQ.2 ) LEX(I)=(INDEX(CCALFU,'BUPDAT0').NE.0)
         IF( I.EQ.3 ) LEX(I)=(INDEX(CCALFU,'BUPDAT1').NE.0)
         IF( LEX(I) ) INQUIRE(FILE=CCALF(I),EXIST=LEX(I))
      ENDDO
C  - Get constants from AUPDAT1
      IF( LEX(1) ) THEN
      OPEN(LUNITA(2),FILE=CCALF(1),STATUS='UNKNOWN',FORM='UNFORMATTED')
      GOTO 5
      ENDIF
C  - Get constants from BUPDAT0 and/or BUPDAT1
      IF( LEX(2) )
     >     OPEN(LUNITA(1),FILE=CCALF(2),STATUS='OLD',FORM='UNFORMATTED')
      IF( LEX(3) )
     >     OPEN(LUNITA(2),FILE=CCALF(3),STATUS='OLD',FORM='UNFORMATTED')
      IF( .NOT.( LEX(2) .OR. LEX(3) ) ) THEN
         WRITE(*,'(A)') 'ZE4VJB: No calibration files specified!'
         WRITE(*,'(A)') '... will stop now!'
         GOTO 9999
      ENDIF
C... Output file with compressed events in ZE4V format
 5    OPEN(IUNZE4,FILE=CZE4VF,STATUS='UNKNOWN',FORM='UNFORMATTED')
C... Output file with complete BOS events
      OPEN(JUNIT,FILE=CZBOSF,STATUS='UNKNOWN',FORM='UNFORMATTED')
C... Output file with BOS bank contents in readable ASCII format
      IF(LBANK)
     >     OPEN(IOBANK,FILE=CBANKF,STATUS='UNKNOWN',FORM='FORMATTED')
C... Initialize random number generator
      WRITE(*,FMT='(/,120(''=''))')
      IF( .NOT. LSTART ) CALL MCRAND('R',CSEEDF,0,0,IERR)
      IF( IERR.LT.0 ) GOTO 9999

      WRITE(*,'(A40)') 'Calibration files used in this run: '
      DO I=1,3
         IF( LEX(I) ) WRITE(*,'(T42,A)') CCALF(I)(1:LENOCC(CCALF(I)))
      ENDDO
      WRITE(*,'(A40,A)') 'Input BOS file is: '
     >     ,CSVBOSF(1:LENOCC(CSVBOSF))
C
      LBMC(1)=1

C ------- PMF 05/06/00 END --------------

      WRITE(6,6001) IEVLIM,IFILMX,IWRMAX,IWZMAX,IDULIM,ISCIP,ITPPK,
     &             ISELFL,ISELMU,ISELEL
 6001 FORMAT(/' MAX # OF EVENTS TO READ   :',I8/
     &         ' MAX # OF INPUTFILES       :',I8/
     &         ' MAX # OF EVENTS ON UNIT 3 :',I8/
     &         ' MAX # OF EVENTS ON UNIT51 :',I8/
     &         ' MAX # OF ZE4V-BANK PRINTS :',I8/
     &         ' INITIAL EVENT SKIP        :',I8/
     &         ' TP/PK-FLAG (1=TP/2=PK)    :',I8/
     &         ' SELECTION (0:ENAB/1:DISAB):',I8/
     &         ' MUON QUALITY SELECTION    :',I8/
     &         ' ELEKTRON SELECTION        :',I8/)
      WRITE(*,FMT='(/,120(''=''))')
C
C                                           INIT BOS AND GEP
      CALL BINT( 80000, 40000, 10000, 0 )
      CALL GEPIC
C
      IFIL = 1
      IEOF = 0
      IWRITE = 0
      IEV = 0
      IZE4V = 0
      IWZE4V = 0
C 
C                                          INITIALISE FEHLER
      CALL FEHLIN
C
  100 IF( JUHR(2) .EQ. 2 ) GO TO 4000
      CALL BREAD( IRUNIT, *1000, *3000 )
CC    WRITE(6,*) '******* READ ENDED NORMALLY FOR EVENT ',IEV
      IEOF = 0
***PMF 06/06/00      IEV = IEV + 1
C                                          LOOK FOR HEAD BANK
      NPHEAD = IW(IBLN('HEAD'))
      IF( NPHEAD.LE.0 ) GO TO 200

      IEV = IEV + 1             ! PMF 06/06/00: increment event counter HERE
 
C                                          INITIAL EVENT SCIP
      IF (IEV.LE.ISCIP) GOTO 2000
C                                          FILL CALIBRATION CONSTANTS
      CALL KLREAD
C                                          FILL ZE4V OVER TP-BANK
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
C                                          FILL ZE4V FROM PATR-BANK
      ELSEIF (ITPPK.EQ.2) THEN
        IF (IZE4V.LE.0) THEN
          CALL ZE4VIN( 1 )
          IZE4V = 1
        ELSE
          CALL ZE4VPK( 1 )
        ENDIF
      ELSE
        WRITE(6,9001) ITPPK
 9001   FORMAT(1X,'FLAG NOT SUPPORTET : ',I4)
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
C PMF 05/06/00: Write whole event out in readable format
        IF( LBANK ) THEN
           IHEAD=IW(IBLN('HEAD'))
           IEVT=HW(IHEAD*2+11)
           WRITE(IOBANK,'(2X,A,6I8)') 'Date',(HW(IHEAD*2+I),I=3,8)
           WRITE(IOBANK,'(3(2x,A,I8))') 'Run',HW(IHEAD*2+10)
     +          ,' Event',HW(IHEAD*2+11),' Type:',HW(IHEAD*2+12) 
           WRITE(IOBANK,'(2X,A,I8)') 'Ebeam',HW(IHEAD*2+29)
           CALL SHOWB(IOBANK,CBANK)
        ENDIF
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
        CALL BMLT( 1, 'ZE4V' ) ! PMF 28/01/00:  CALL BMLT( 3, 'ZEHDZE4VPALL' )
CC      CALL BPRL(IDULIM)
        CALL BWRITE( IUNZE4 )
      ENDIF
      GO TO 2000
C
C
C                                           ERRORS
 1000 CONTINUE
      CALL JOBFIN( 3, IEV, *1100 )
 1100 IF( IEOF .NE. 0 ) GO TO 8000
C
C
 2000 CONTINUE
      CALL BSLT
      CALL BDLG

      IF( MOD(IEV,50).EQ.0 )
     >     WRITE(*,'(''ZE4VJB: event numbers: '
     >     //'read event / written ZE4V record / written BOS event: '''
     > //',I8,''  /'',I8,''  /'',I8)') IEV,IWZE4V,IWRITE

      IF( IEV .LT. IEVLIM ) GO TO 100
      CALL JOBFIN( 4, IEV, *8100 )
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
C                                           TIME OUT
 4000 CALL JOBFIN( 2, IEV, *8100 )
C
C                                           END OF JOB
 8000 CONTINUE
      CALL JOBFIN( 1, IEV, *8100 )
 8100 CONTINUE
C                                           MCREDU STATISTICS
      CALL FEHLPR
      CALL EAFLDP               ! PMF 10/12/99: adopted from 'ze4vpkmn.for'
      CALL MCRDFL
      CALL BSTA
      CALL GEPW
C PMF: Save current random number sequence
      WRITE(*,FMT='(/,120(''=''))')
      CALL MCRAND('W',CSEEDF,ISCIP+1,IEV,IERR)
C PMF: Some output info
      WRITE(*,'(/,A,/,25(''-''))') 'ZE4VJB: I/O information:'
C      WRITE(*,'(A35,A)') 'Output BOS banks to: '
C     >     ,CZBOSF(1:LENOCC(CZBOSF))
      WRITE(*,'(A35,A)') 'Input BOS file was: '
     >     ,CSVBOSF(1:LENOCC(CSVBOSF))
      WRITE(*,'(A35,I8)') 'First processed event was: ',ISCIP+1
      WRITE(*,'(A35,I8)') 'Last processed event was: ',IEV
      WRITE(*,'(A35,I8)') 'Number of written ZE4V records: ',IWZE4V
      WRITE(*,'(A35,I8)') 'Number of written events: ',IWRITE
      WRITE(*,'(A35)') 'Calibration file used: '
      DO I=1,3
         IF( LEX(I) ) WRITE(*,'(T37,A)') CCALF(I)(1:LENOCC(CCALF(I)))
      ENDDO
      WRITE(*,'(A35,A)') 'Output BOS banks to: '
     >     ,CZBOSF(1:LENOCC(CZBOSF))
      WRITE(*,'(A35,A)') 'Output compressed ZE4V events to: '
     >     ,CZE4VF(1:LENOCC(CZE4VF))
      IF( LBANK ) THEN 
         WRITE(*,'(A35,A)') 
     >     'Output readable bank contents to: '
     >     ,CBANKF(1:LENOCC(CBANKF))
         WRITE(*,'(A35,A)') 'Printed BOS banks: ',CBANK(1:LENOCC(CBANK))
      ENDIF
C
      IF( ISCIP .GE. IEV ) WRITE(*,'(/,A)')
     >   'ZE4VJB: WARNING!!! Number of first event to be reconstructed'
     >   //' exceeds number of events contained in the input file!'
C
      WRITE(*,FMT='(/,120(''=''))')
C
 9999 STOP
      END
