C   09/10/85            MEMBER NAME  MUANAC   (JADEMUS)     FORTRAN
C
C-----------------------------------------------------------------------
      SUBROUTINE MUANAC
C-----------------------------------------------------------------------
C
C LAST CHANGE 15.40  8/10/85 CHRIS BOWDERY - EXPAND HC AND HLUN ARRAYS
C      CHANGE 23.30 29/09/84 CHRIS BOWDERY - CALIBRATION CHECKING CODE
C      CHANGE 12.00  9/03/84 CHRIS BOWDERY - NEW MUCOOR DATE.
C      CHANGE 10.00 13/01/84 CHRIS BOWDERY - NEW MUCOOR DATE.
C      CHANGE 12.00 25/10/83 HUGH MCCANN   - NEW MUCOOR DATE.
C      CHANGE 11.00 16/03/83 HUGH MCCANN   - NEW MUCOOR DATE.
C
C-----------------------------------------------------------------------
C
C        THIS IS THE  FIRST  ROUTINE  OF  THE  MUON  ANALYSIS  CHAIN.IT
C        CONVERTS THE THE MUON RAW DATA (IBM FORMAT) TO COORDINATES AND
C        STORES THEM, AND OTHER INFORMATION, IN 'MUR1' BANKS 0,1,AND 2.
C        (FOR DESCRIPTION OF BANKS SEE @MUINFOM).
C
C        NOTE BANK 0 IS CREATED EVEN IF THERE ARE NO MUON HITS.
C
C        THERE ARE 2 SIGNAL TO COORDINATE CONVERSION ROUTINES...
C
C    1)  MUTINY, WHICH USES  A  CONDENSED  SET  OF  CALIBRATION  DATA
C        PREPARED BY MUCONT (FOR MONTE  CARLO  OUTPUT  -  SEE  MCJADE),
C        WHICH CALLS A VERSION OF MUDOWN.
C
C    2)  MUCOOR, WHICH USES THE FULL MUON CALIBRATION DATA PREPARED BY
C        MUCON (FOR MONTE CARLO OUTPUT - SEE MCJADE).
C
C HMU IS THE RAW MU DATA.
C LMU IS THE LENGTH OF THE RAW DATA BANK MEASURED IN I*4 WORDS.
C
C HC IS THE MU COORDINATE OUTPUT ARRAY IN 9'S.....
C               WORD  1     -  4*CHAMBER NUMBER + (HIT NUMBER -1).
C               WORD  2     -  10*LAYER NUMBER + ORIENTATION PARAMETER.
C               WORDS 3 - 8 -  X,Y,Z(LEFT),X,Y,Z(RIGHT) (MM).
C               WORD  9     -  POINTER TO RAW DATA
C
C-----------------------------------------------------------------------
C
      IMPLICIT INTEGER*2 (H)
C
C                            COMMONS.
C
#include "cmubcs.for"
#include "cmucalib.for"
C
      COMMON /CWORK/ HC(3600),HLUN(400)
C
C                            DATA INITIALISATION STATEMENTS.
C
C                            NWHIT IS NO. OF WORDS PER HIT
      DATA NWHIT/9/
C
C                            NHITMX  IS  THE  MAXIMUM   NO.   OF   HITS
C                            CONVERTED.
      DATA NHITMX/400/
C
C------------------  C O D E  ------------------------------------------
C
C                            FIND MUON DATA BANK POINTER.
C
      IPMU = IDATA(IBLN('MUEV'))
      IF( IPMU .NE. 0 ) GO TO 1
C
C                            NO MUON BANK PRESENT.
      NHITS = 0
      GO TO 2
C
C                            CHECK THE CALIBRATION TO SEE IF MISSING
C                            IF 2 CONSTANTS ARE ZERO, THEN NO CALIB.
C
  1   IF( IZOEP1 .EQ. 0  .AND.  IZOEP3 .EQ. 0 ) GO TO 95
C
C
C                            CALL MUCOOR TO GET COORDINATES, ETC.
C
      CALL MUCOOR(IDATA(IPMU+1),NHITS,HC(1),NHITMX,IDATA(IPMU),HLUN(1),
     +            NWHIT)
C-----------------------------------------------------------------------
C
C                            CREATE 'MUR1' BANK 0 - MUON GENERAL INFO
C
 2    CONTINUE
C                            ADD  'MUR1'  TO  SPECIAL  LIST  READY  FOR
C                            WRITING.
      CALL BSAW(1,'MUR1')
      IBANK=0
      CALL CCRE(IP0,'MUR1',IBANK,9,IER)
      IF(IER.NE.0)GO TO 96
C                            NO. OF HITS.
      IDATA(IP0+1) = NHITS
C                            NO. OF I*2 WORDS  PER  HIT  IN  COORDINATE
C                            BANK.
      IDATA(IP0+3) = NWHIT
C                            DATE OF VERSION OF MUCOOR  WHICH  PRODUCES
C                            COORDINATES.
      IDATA(IP0+8) = 840929
C                            CALIBRATION DATA ISSUE.
      IDATA(IP0+9) = NVERSN
C
C-----------------------------------------------------------------------
C
C                            CREATE 'MUR1' BANK  1  -  MUON  COORDINATES
C
      IF(NHITS.LE.0)GO TO 98
      LB1=(NWHIT*NHITS+1)/2
      IBANK=1
      CALL CCRE(IP1,'MUR1',IBANK,LB1,IER)
      IF(IER.NE.0)GO TO 96
      CALL UCOPY(HC(1),IDATA(IP1+1),LB1)
C
C
C-----------------------------------------------------------------------
C
C                            CREATE 'MUR1' BANK 2 - HIT STATUS BANK.
C
      IF(NHITS.LE.0) RETURN
      LB2=(NHITS+1)/2
      IBANK=2
      CALL CCRE(IP2,'MUR1',IBANK,LB2,IER)
      IF(IER.NE.0)GO TO 96
      CALL UCOPY(HLUN(1),IDATA(IP2+1),LB2)
C
      RETURN
C
C-----------------------------------------------------------------------
C
C                            ERROR CONDITIONS.
C
 95   CALL MUERRY('MUANAC',0,'WARNING: NO CALIBRATION FOUND. IF MC DATA,
     + WAS EVREAD USED TO PROCESS CALIB. RECORDS? MUCOOR NOT CALLED.^')
      RETURN
C
 96   IF( IER .GT. 1 ) GO TO 97
      CALL MUERRY('MUANAC',IBANK,'''MUR1'' ALREADY PRESENT.^')
      RETURN
C
 97   CALL MUERRY('MUANAC',IBANK,'NOT ENOUGH SPACE FOR ''MUR1''.^')
      RETURN
C
 98   IF( IPMU .NE. 0 ) RETURN
      CALL MUERRY('MUANAC',0,' WARNING: MUEV BANK NOT LOCATED.^')
      RETURN
C
      END
