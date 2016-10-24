C   21/02/84 402211403  MEMBER NAME  MUCONW   (JADEMUS)     FORTRAN
C
C-----------------------------------------------------------------------
      SUBROUTINE MUCONW( IFLAG, LUNBOS, LUNJAD, HERR )
C-----------------------------------------------------------------------
C
C LAST CHANGE 15.50 20/02/84 C. BOWDERY   - NO ERROR IF BANKS EXIST
C      CHANGE 11.40 29/06/82 HUGH MCCANN  - ADD /CMUED/ .
C      CHANGE 10.00 28/05/82 HUGH MCCANN  - REDUCE CORE REQUIREMENTS FOR
C                                          PRODUCTION OF UPDATE DATASETS
C NEW VERSION 08.34 12/06/81 HUGH MCCANN  - TO ACCOM NEW JADE SYSTEM.
C NEW VERSION 19.09 26/03/81 CHRIS BOWDERY- CHANGES ARGUMENTS
C
C
C-----------------------------------------------------------------------
C
C OUTPUTS MUON CALIBRATION DATA.
C
C               FULL SET                    IF IFLAG=1
C           CHANGES ONLY (HALF WORDS)       IF IFLAG=0
C      BOS FORMAT OUTPUT                    ON LUNBOS UNLESS SET TO 0 .
C     JADE FORMAT (SINGLE LOGICAL RECORD)   ON LUNJAD UNLESS SET TO 0 .
C
C HERR =  N    IF BOS COULD NOT CREATE BANK N ===> NO OUTPUT ATTEMPTED
C HERR = 20    IF LUNBOS = LUNJAD BUT .NE.0   ===> NO OUTPUT ATTEMPTED
C HERR = -1    WARNING:NO POSITIVE LOGICAL UNIT NO. GIVEN ===> NO OUTPUT
C HERR =  0    NORMAL RETURN
C
C-----------------------------------------------------------------------
C
      IMPLICIT INTEGER*2 (H)
C
C                            COMMONS.
C
#include "cmubcs.for"
#include "cmucalib.for"
#include "cmuedwrk.for"
#include "cmued.for"
C
      DIMENSION IHED(9)
      character*4 NAME , MUNAME 
      DIMENSION NAME(12) , NUMBER(12) , LENGTH(12)
C
C                            DATA INITIALISATION STATEMENTS.
C
      DATA NBANKS /   12   /
      DATA MUNAME / 'MUCA' /
      DATA NAME/'MUCO','MUCD','MUOV','MFFI','MCFI','MFSU',
     +          'MCSU','MCEL','MCST','MUFI','MUYO','MUEN'/
      DATA NUMBER/  1, 0,0,  2,  3,  4,  5,   6,  7, 8, 9,10 /
      DATA LENGTH/100,16,3,370,318,246,634,2220,317,36,10,15 /
      DATA IHED(2)/0/,IHED(3)/0/,IHED(4)/0/,IHED(5)/0/,IHED(8)/0/,
     *     IHED(9)/0/
C
C------------------  C O D E  ------------------------------------------
C
CAV FIXME
C      IHED(1)=MUNAME
      IHED(6)=KTIME
      IHED(7)=IFLAG
C
C                            OUTPUT ENTRY MESSAGE THEN CHECK IF
C                            LUNBOS = LUNJAD. IF SO OUTPUT ERROR
C                            MESSAGE AND RETURN UNLESS BOTH 0 .
C
      CALL MUMESS('MUCONW',0,'CALLED TO OUTPUT MUON CALIBRATION.^')
      IF(LUNBOS.NE.LUNJAD) GO TO 3
      IF(LUNBOS.EQ.0) GO TO 99
C
        CALL MUERRY('MUCONW',LUNBOS,'=LOGICAL UNIT NO.  OF BOTH JADE AND
     + BOS DATASETS. NEITHER WRITTEN.^')
      HERR=20
      RETURN
C
C                            CREATE BANKS IF NOT ALREADY EXISTING,
C                            THAT IS, ALLOW IER = 1.
C
 3    HERR=0
      DO  1  I = 1,NBANKS
        CALL CCRE(IP,NAME(I),NUMBER(I),LENGTH(I),IER)
        IF( IER .GE. 2 ) GO TO 98
        IF(I.EQ.2)  CALL UCOPY(NVERSN,IDATA(IP+1),LENGTH(I))
        IF(I.EQ.3)  CALL UCOPY(HOVALL,IDATA(IP+1),LENGTH(I))
        IF(I.EQ.4)  CALL UCOPY(HMFFIX,IDATA(IP+1),LENGTH(I))
        IF(I.EQ.5)  CALL UCOPY(HMCFIX,IDATA(IP+1),LENGTH(I))
        IF(I.EQ.6)  CALL UCOPY(HMFSUR,IDATA(IP+1),LENGTH(I))
        IF(I.EQ.7)  CALL UCOPY(HMCSUR,IDATA(IP+1),LENGTH(I))
        IF(I.EQ.8)  CALL UCOPY(HMCELE,IDATA(IP+1),LENGTH(I))
        IF(I.EQ.9)  CALL UCOPY(HMCSTA,IDATA(IP+1),LENGTH(I))
        IF(I.EQ.10) CALL UCOPY(HFILDA,IDATA(IP+1),LENGTH(I))
        IF(I.EQ.11) CALL UCOPY(HYKNMI,IDATA(IP+1),LENGTH(I))
        IF(I.EQ.12) CALL UCOPY(IZEII ,IDATA(IP+1),LENGTH(I))
  1   CONTINUE
C
C-----------------------------------------------------------------------
C
C                  OUTPUT BANKS.
C
C             IF LOGICAL UNIT NUMBER LUNBOS > 0 , OUTPUT BOS DATASET
C
      IF(LUNBOS.LE.0)GO TO 2
        CALL BMLT(NBANKS,NAME)
        CALL BWRITE(LUNBOS)
        CALL BDLG
        CALL MUMESS('MUCONW',LUNBOS,'=LOGICAL UNIT NO. OF BOS OUTPUT^')
C
C                            IF LOGICAL  UNIT  NUMBER  LUNJAD  >  0  ,
C                            OUTPUT A SINGLE  RECORD  OF  LENGTH  4194
C                            WORDS FOR THE JADE SYSTEM (STEFFEN)
C
 2    IF( LUNJAD .LE. 0 ) GO TO 99
      IF( IFLAG  .EQ. 0 ) GO TO 22
      NLONG = 4194
      WRITE(LUNJAD) NLONG, (IHED(I),I=1,9), (MUCAL(I),I=1,4185)
      CALL MUMESS('MUCONW',LUNJAD,'=LOGICAL UNIT NO. OF JADE OUTPUT^')
      GO TO 99
C
 22   NLONG = NCHAN + 9
      WRITE(LUNJAD) NLONG, (IHED(I),I=1,9),
     +              (HLOC(I),HUPDAT(I),I=1,NCHAN)
C
C++++++++ DEBUGGING :
C
      DO 26 I=1,NCHAN
        WRITE(6,66)HLOC(I),HUPDAT(I)
   66   FORMAT('0  LOCATION / NEW VALUE : ',2I5)
   26 CONTINUE
C
C++++++++
C
      CALL MUMESS('MUCONW',LUNJAD,'=LOGICAL UNIT NO. OF JADE OUTPUT^')
      GO TO 99
C
C                            ERROR CONDITIONS. HERR = NUMBER  OF  BANK
C                            CAUSING THE ERROR.
C
  98  CALL MUERRY('MUCONW',I,'INSUFFICIENT SPACE FOR BANK CREATION.^')
      HERR=I
C
 99   IF( LUNBOS .GT. 0 .OR. LUNJAD .GT. 0 ) RETURN
C
      CALL MUMESS('MUCONW',0,'NO OUTPUT PRODUCED AS NO POSITIVE LOGICAL
     +UNIT NUMBERS GIVEN^')
      HERR=-1
      RETURN
      END
