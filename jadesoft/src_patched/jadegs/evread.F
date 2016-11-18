C   19/02/84 712202038  MEMBER NAME  EVREAD   (JADEGS)      FORTRAN
C
C-----------------------------------------------------------------------
      SUBROUTINE EVREAD( NUNIT, IRET )
C-----------------------------------------------------------------------
C
C   AUTHOR:   J. OLSSON PREHISTORY :  READ A JADE EVENT FROM NUNIT
C             E. ELSEN
C
C        MOD: C. BOWDERY  12/12/83 :  JUSCRN = 6 IF NOT SET | CALL MUREG
C        MOD: C. BOWDERY  08/02/84 :  MACRO CADMIN NOW ON PATRECSR
C                                  :  BLOCKDATA SET IN JADEBD
C                                  :  EXTRA ERROR MESSAGES AND TIDY-UP
C        MOD: C. BOWDERY  13/02/84 :  NRECS COUNTS LOGICAL RECORDS NOW
C NEW VERSION C. BOWDERY  16/02/84 :  CODE RE-WRITTEN
C        MOD: C. BOWDERY  20/02/84 :  SET NFLAGS(8) IF CONSTANTS FOUND
C        MOD: C. BOWDERY  14/09/84 :  REAL DATA AFTER MC - BUG KILLED
C
C  LAST  MOD: J. OLSSON  06/12/87  :  INFO PRINT IN MC EVENTS
C
C  Last mod:
C   P. Movilla Fernandez 12/06/00  : Introduce a smearing veto for MC events
C                   
C
C-----------------------------------------------------------------------
C
C     GENERAL READ ROUTINE FOR REAL AND MONTE CARLO DATA. DATA  HAVE  TO
C     BE IN JADE BOS FORMAT AND ARE READ INTO COMMON /CBS/
C
C     IF MONTE CARLO, SMEARING IS AUTOMATICALLY  INVOKED  IF  PREVIOUSLY
C     UNSMEARED; ALREADY SMEARED MC DATA ARE TREATED AS  REAL  DATA.  IF
C     DATA ARE  PRECEDED  BY  CONSTANTS  RECORDS,  THESE  CONSTANTS  ARE
C     AUTOMATICALLY FED INTO CORRESPONDING COMMONS.
C
C     IRET = 2 IF END OF FILE ENCOUNTERED, OTHERWISE 0.
C
C     READ ERRORS ARE NOT RETURNED, PROGRAM STOPS AFTER 10 READ ERRORS.
C
C     THREE EVENT TYPES ARE DISTINGUISHED, CHARACTERIZED BY VARIABLE
C     IEVTP IN COMMON /CADMIN/ :
C
C               IEVTP = 0           REAL DATA
C               IEVTP = 1           UNSMEARED MONTE CARLO DATA
C               IEVTP = 2           SMEARED MONTE CARLO DATA
C
C     FOR ALL MONTE CARLO DATA (IEVTP = 1 OR 2), THE PROGRAM EXPECTS TWO
C     CONSTANTS RECORDS TO PRECEDE THE DATA.
C
C     LBREAD(1) =  .TRUE. :  ID CALIB. RECORD ('MTCO') HAS BEEN READ,
C     LBREAD(2) =  .TRUE. :  MU CALIB. RECORD ('MUCO') HAS BEEN READ.
C     LBREAD(3) =  .TRUE. :  BOTH CALIB RECORDS HAVE BEEN READ, OR IF
C                                           REAL DATA ARE BEING READ
C
C----------------------------------------------------------------------
C
      IMPLICIT INTEGER*2 (H)
C                                      JADE BLOCK DATA
      EXTERNAL JADEBD
C
      LOGICAL FLG100
C
#include "cadmin.for"
#include "cgraph.for"
#include "cdata.for"
C
C                            NFLAGS(5) IN /CADMIN/ COUNTS RECORDS READ
C                            NFLAGS(8) SET WHEN CONSTANTS HAVE BEEN READ
C
      EQUIVALENCE (NRECS , NFLAGS(5) )
C
      DATA IPRCL /0/
*** PMF 14/06/00:
* Introduce a MC smearing veto when reading MC events.
      LOGICAL NOSMEAR
      COMMON /CMCSMEAR/ NOSMEAR
*** PMF(end)
C
C------------------  C O D E  ------------------------------------------
C
      IF( JUSCRN .LE. 0  .OR. JUSCRN .GT. 99 ) JUSCRN = 6
C
      IRET  = 0
      IHEAD = IBLN('HEAD')
      IMTCO = IBLN('MTCO')
      IMUCO = IBLN('MUCO')
            
C                                            CADMIN FLAG
      FLG100 = .FALSE.
      IF( IFLG .EQ. 100 ) FLG100 = .TRUE.
      IF( FLG100 ) GO TO 1
C
C                            MAIN READ STATEMENT.
C
 1000 CALL BSLT      
      CALL BDLG
      NRECS = NRECS + 1
      CALL BREAD(NUNIT,&60,&70)
      CALL USER(20)              ! PMF 07/07/99 added for additional checks
                                !     14/06/00 set MC smearing veto in user routine
C
C                            DO WE HAVE A 'HEAD' BANK I.E. AN EVENT ?
C                            IF NOT THEN SKIP NEXT SECTION
C

   1  IF( IDATA(IHEAD) .LE. 0 ) GO TO 10
        IHD  = IDATA( IHEAD )
        IRUN = HDATA( IHD*2 + 10 )
C
C                            IS IT LEGAL HERE ?
C
        IF( FLG100  .OR. LBREAD(3) )    GO TO 2
        IF( .NOT. ( LBREAD(1) .AND. .NOT. LBREAD(2) ) ) GO TO 3
C
C                            BANK 'MTCO' WAS LAST SO THIS OUGHT TO BE
C                            A RECORD WITH 'MUCO' NOT 'HEAD'
C
          WRITE(JUSCRN,69) NRECS
  69      FORMAT(/' *****  ERROR  *****  EVREAD DETECTED BAD DATA',
     +            ' STRUCTURE AT INPUT RECORD: ',I6//
     +            ' EVENT RECORD FOUND AFTER ''MTCO'' BANK RECORD.',
     +            ' EVREAD EXPECTED A RECORD WITH A ''MUCO'' BANK.'/
     +            ' I.E. MU CONSTANTS RECORDS DID NOT FOLLOW ID',
     +            ' CONSTANTS. MUON COMMONS NOT FILLED !!!')
          WRITE(JUSCRN,42)
          LBREAD(2) = .TRUE.
          LBREAD(3) = .TRUE.
C
C                            RECOVER FROM THIS ERROR
C
          GO TO 2
C
C                            MONTE CARLO DATA WITHOUT CALIBRATION
C                            FAKING REAL DATA? PRINT WARNING AND STOP
C
   3      IF( IRUN .GE. 100 ) GO TO 5
C
            WRITE(JUSCRN,44) NRECS
  44        FORMAT(/' *****  ERROR  *****  EVREAD DETECTED BAD DATA',
     +              ' STRUCTURE AT INPUT RECORD: ',I6//
     +              ' RUN NUMBER INDICATED MONTE CARLO DATA BUT NO',
     +              ' CALIBRATION RECORDS WERE FOUND.')
C
            WRITE(JUSCRN,42)
            WRITE(JUSCRN,43)
C
            CALL BSTA
            STOP 888
C
C                            SET FLAGS TO INDICATE REAL DATA DETECTED
C
  5     LBREAD(3) = .TRUE.
        IEVTP     = 0
C
C                            WE HAVE AN EVENT. COUNT IT. IF UNSMEARED
C                            MONTE CARLO, CALL SMEARING ROUTINES.
C                            CHECK RUN NUMBER BEFORE EVENT TYPE IN CASE
C                            REAL DATA EVENTS FOLLOW MC EVENTS.
C
  2     NRREAD = NRREAD + 1
        IF( IRUN .GE. 100 ) IEVTP = 0
        IF( IEVTP .EQ. 1 .and. .not. nosmear) CALL RDJETC ! PMF 14/06/00: MC smearing veto introduced
C
C PRINT INFO FOR FIRST MONTE CARLO EVENT
C
        IF(IPRCL.NE.0.OR.IEVTP.EQ.0) RETURN
        IPRCL = IPRCL + 1
        ITRKGM = HDATA(IHD*2+16)
        ILEADG = HDATA(IHD*2+17)
        ISF5F6 = HDATA(IHD*2+14)
        IELOSS = HDATA(IHD*2+18)
C
        IF(ITRKGM.EQ.0) WRITE(6,1451)
1451  FORMAT(' ## EVREAD --->>    TRKGAM WITH ERRORS BEFORE JCN 87 ')
        IF(ITRKGM.EQ.1) WRITE(6,1452)
1452  FORMAT(' ## EVREAD --->>    TRKGAM CORRECTED ACC. TO JCN 87 ')
        IF(ILEADG.EQ.0.AND.ITRKGM.NE.0) WRITE(6,1453)
1453  FORMAT(' ## EVREAD --->>    STANDARD 1-DIM LEAD GLASS TRACKING')
        IF(ILEADG.EQ.1) WRITE(6,1454)
1454  FORMAT(' ## EVREAD --->>    MEIER-MAGNUSSEN LEAD GLASS TRACKING')
        IF(ILEADG.EQ.2) WRITE(6,1455)
1455  FORMAT(' ## EVREAD --->>    TOKYO SHOWER LEAD GLASS TRACKING')
        IF(ILEADG.EQ.2) WRITE(6,1456) ISF5F6
1456  FORMAT('           --->>    WITH VERSION NUMBER ',I4)
        IF(IELOSS.NE.0) WRITE(6,1457)
1457  FORMAT(' ## EVREAD --->>    MULTIPLE SCATTERING AND ENERGY LOSS WA
     $S DISABLED IN TRACKING')
C
        RETURN
C
C                            DO WE HAVE AN 'MTCO' BANK RECORD? IF SO
C                            CHECK THAT THE LAST RECORD WAS NOT 'MTCO'
C                            OR 'MUCO', THEN SET FLAGS AND PROCESS BANK.
C
  10    IF( IDATA(IMTCO) .LE. 0 ) GO TO 20
        IF( FLG100 .OR. LBREAD(3) .OR. NRECS .EQ. 1 ) GO TO 11
C
          IF( LBREAD(1) .AND. .NOT. LBREAD(2) ) WRITE(JUSCRN,12) NRECS
C
          IF( LBREAD(2) .AND. .NOT. LBREAD(1) ) WRITE(JUSCRN,13) NRECS
C
  12      FORMAT(/' *****  ERROR  *****  EVREAD DETECTED BAD DATA',
     +            ' STRUCTURE AT INPUT RECORD: ',I6//
     +            ' THIS RECORD CONTAINED AN ''MTCO'' BANK',
     +            ' BUT SO DID THE PREVIOUS RECORD.',
     +            ' THE NEW CONSTANTS WILL REPLACE THE OLD ONES.')
C
  13      FORMAT(/' *****  ERROR  *****  EVREAD DETECTED BAD DATA',
     +            ' STRUCTURE AT INPUT RECORD: ',I6//
     +            ' THIS RECORD CONTAINED AN ''MTCO'' BANK BUT THE',
     +            ' PREVIOUS RECORD HAD A ''MUCO'' BANK.'/
     +            ' THUS THERE WERE CALIBRATION CONSTANTS BUT NO',
     +            ' EVENTS. HOWEVER THE NEW ''MTCO'' WILL BE ACCEPTED.')
C
          WRITE(JUSCRN,42)
C
  11    LBREAD(1) = .TRUE.
        LBREAD(2) = .FALSE.
        LBREAD(3) = .FALSE.

C
C                            PROCESS 'MTCO' BANK.
C
C                            SET NFLAGS(8) TO TELL EVWRIT THAT NEW
C                            CONSTANTS HAVE BEEN FOUND.
C
        IEVTP     = 1
        NFLAGS(8) = 1
        IPMTCO    = IDATA(IMTCO)
C
C                            IS THIS A SMEARED MC EVENT?
C
        IF( IDATA(IPMTCO+1) .NE. 0 ) IEVTP = 2
C
        WRITE(JUSCRN,14) IEVTP,IDATA(IPMTCO+2)
  14    FORMAT(/' ---> EVREAD READ A TYPE  ',I1,
     +          '  MONTE CARLO EVENT WITH IFLAG2 = ',I4/)
C
C                            COPY THE CALIBRATION CONSTANTS INTO COMMON
C
        CALL RDMTCO( 'DE' )
C
        GO TO 1000
C
C                            DO WE HAVE AN 'MUCO' BANK RECORD? THIS IS
C                            ONLY LEGAL AFTER AN 'MTCO' BANK RECORD.
C
 20   IF( IDATA(IMUCO) .LE. 0 ) GO TO 30
C
        IF( FLG100 ) GO TO 21
        IF( LBREAD(1) .AND. .NOT. LBREAD(2) ) GO TO 21
          WRITE(JUSCRN,22) NRECS
C
  22      FORMAT(/' *****  ERROR  *****  EVREAD DETECTED BAD DATA',
     +            ' STRUCTURE AT INPUT RECORD: ',I6//
     +            ' THIS RECORD CONTAINED A ''MUCO'' BANK WITH MUON',
     +            ' CONSTANTS BUT THE PREVIOUS RECORD DID NOT HAVE AN',
     +            ' ''MTCO'' BANK WITH ID CONSTANTS.')
C
          WRITE(JUSCRN,42)
          WRITE(JUSCRN,43)
          CALL BSTA
          STOP 666
C
C                            PROCESS 'MUCO' AND OTHER MUON BANKS
C                            TRANSFER MUON CALIBRATION TO /CALIBR/
C                            EXTRACT MUON REGIONS FROM THE CALIBRATION
C
  21    CALL MUCON
        CALL MUREG(0)
        LBREAD(2) = .TRUE.
        LBREAD(3) = .TRUE.
        GO TO 1000
C
  30  WRITE(JUSCRN,31) NRECS
  31  FORMAT(/' *****  ERROR  *****  EVREAD DETECTED BAD DATA',
     +        ' STRUCTURE AT INPUT RECORD: ',I6//
     +        ' THIS RECORD WAS NOT EXPECTED. IT DID NOT HAVE',
     +        ' A ''HEAD'' BANK OR AN ''MTCO'' (M.C. CONSTANTS)',
     +        ' BANK (BOTH OF WHICH WOULD BE LEGAL HERE).'/
     +        ' ALSO IT DID NOT HAVE A ''MUCO'' BANK WHICH WOULD HAVE',
     +        ' BEEN ILLEGAL ANYWAY. BOS BANK NAMES WILL BE PRINTED.')
C
      WRITE(JUSCRN,42)
C
C                            PRINT LIST OF BANKS IN THIS EVENTS
C
      CALL EVBKPR(IDUM)
      WRITE(JUSCRN,32)
  32  FORMAT(/' RECOVERY ACTION:  RECORD WILL BE SKIPPED AND COUNTED',
     +        ' AS A READ ERROR.'/)
C
      NRERR  = NRERR  + 1
      NRREAD = NRREAD + 1
      GO TO 63
C
C                            READ ERROR
C
  60  NRERR  = NRERR  + 1
      NRREAD = NRREAD + 1
      WRITE (JUSCRN,61) NRERR, NRECS
  61  FORMAT(/' *****  ERROR  *****  EVREAD ENCOUNTERED BOS READ ERROR'
     +      //' OCCURRENCE NO. ',I2,'.  SEQUENTIAL RECORD NO. ',I8,
     +        '.   EVENT/RECORD SKIPPED.'//)
C
  63  IF( NRERR .LT. 10 ) GO TO 1000
C
C                            TOO MANY READ ERRORS
C
      WRITE(JUSCRN,64) NRREAD, NRWRIT, NRERR
  64  FORMAT(//' *****  TOO MANY READ ERRORS  *****  FOUND BY EVREAD'//
     +       '   NR OF EVENTS READ: ',I6,'   NR OF EVENTS WRITTEN: ',I6,
     +       '   NR OF READ ERRORS: ',I3//)
      WRITE(JUSCRN,43)
      CALL BSTA
      STOP 777
C
C                            END-OF-FILE
C
  70  IF( LBREAD(3) ) GO TO 99
C
C                            NOT EXPECTED
C
      IF( NRECS .GT. 1 ) GO TO 95
        WRITE(JUSCRN,96)
  96    FORMAT(/' *****  WARNING  *****  EVREAD ANOMALOUS CONDITION'//
     +          ' THE INPUT EVENT DATASET IS EMPTY.')
        GO TO 94
C
  95    WRITE(JUSCRN,97) NRECS
  97    FORMAT(/' *****  WARNING  *****  EVREAD ANOMALOUS CONDITION',
     +          ' AT INPUT RECORD: ',I6//
     +          ' EOF ENCOUNTERED BEFORE ALL M.C. CONSTANTS READ')
C
  94  WRITE(JUSCRN,42)
      WRITE(JUSCRN,43)
      CALL BSTA
      STOP 999
C
  99  IRET = 2
      RETURN
C
C                            FORMAT STATEMENTS USED IN SEVERAL PLACES
C
  42  FORMAT( ' WERE THESE EVENTS WRITTEN CORRECTLY?  DID YOU USE S/R',
     +        ' EVWRIT?  IT ENSURES CORRECT STRUCTURE AUTOMATICALLY.'/)
  43  FORMAT( ' PROGRAM WILL STOP AFTER BOS STATISTICS PRINTOUT.'//)
C
      END
