C   18/12/87 807262019  MEMBER NAME  MUCON    (JADEMUS)     FORTRAN
C
C-----------------------------------------------------------------------
      SUBROUTINE MUCON
C-----------------------------------------------------------------------
C
C LAST CHANGE 20.03 26/07/88 CHRIS BOWDERY- PRINT MESSAGES ONLY ONCE
C      CHANGE 23.45 17/12/87 CHRIS BOWDERY- CHANGES IF NOT MUON MC DATA
C      CHANGE 20.30 19/02/84 CHRIS BOWDERY- REMOVE OLDSCR.NOW IN UCOPY
C      CHANGE 07.30 10/04/81 HUGH MCCANN  - JADEMUS UPDATE.
C      CHANGE 09.50 26/09/79 JOHN ALLISON.
C      CHANGE 20.16 21/09/79 HARRISON PROSPER.
C
C-----------------------------------------------------------------------
C
C      MUCON TRANSFERS ALL BOS BANKS INTO COMMON /CALIBR/
C
C-----------------------------------------------------------------------
C
      IMPLICIT INTEGER*2 (H)
C
      LOGICAL  OLD, FIRST, FIRST1, FIRST2
C
C                            COMMONS.
C
#include "cmubcs.for"
#include "cmucalib.for"
C
C                            DECLARATIONS, INITIALISATIONS, ETC.
C
      DIMENSION HUNI1(82)
C
      DATA HUNI1/3,1,1,1,1,1,1,1,1,1,
     +           1,1,1,3,2,2,2,2,2,2,
     +           2,2,2,2,2,2,3,3,3,3,
     +           3,3,3,3,3,3,3,3,4,4,
     +           5,5,4,4,5,5,4,4,5,5,
     +           4,4,5,5,4,4,5,5,4,4,
     +           5,5,4,4,5,5,4,5,4,5,
     +           4,5,4,5,4,5,4,5,4,5,
     +           4,5                  /
C
      DATA  FIRST, FIRST1, FIRST2 / .TRUE., .TRUE., .TRUE. /
C
C------------------  C O D E  ------------------------------------------
C
      IF( .NOT. FIRST ) GO TO 1
        CALL MUMESS('MUCON ',0,'CALLED TO COPY MUON CALIBRATION.^')
        CALL MUMESS('MUCON ',0,'WARNING: MESSAGE IS NOT REPEATED!^')
        FIRST = .FALSE.
C
C                            CHECK FOR 'MFFI' - SHOULD  BE  THERE  FOR
C                            OLD AND NEW.
C
 1    CALL CLOC(IP,'MFFI',2)
      IF( IP .LE. 0 ) GO TO 98
      CALL UCOPY(IDATA(IP+1),HMFFIX,IDATA(IP))
C
C                            CHECK FOR NEW BANKS.
C
      CALL CLOC(IP,'MUCD',0)
      IF( IP .GT. 0 ) GO TO 2
C                            OLD (PRE MAY 1979).
      OLD    = .TRUE.
      NVERSN = 1
      CALL UCOPY( 'OLD (PRE MAY 1979) MONTE CARLO CALIBRATION DATA.^
     +      ',DESCRP,15)
      CALL VZERO(HOVALL,3)
      GO TO 3
C                            NEW.
 2    OLD = .FALSE.
      CALL UCOPY(IDATA(IP+1),NVERSN,IDATA(IP))
      CALL CLOC(IP,'MUOV',0)
      CALL UCOPY(IDATA(IP+1),HOVALL,IDATA(IP))
C
C                            IF BANKS ARE EMPTY, TAKE NO FURTHER ACTION
C
      IF( NVERSN .LE. 0 ) GO TO 98
C
C                            FIND OTHER BANKS, ADDING NEW ARRAYS WHERE
C                            NECESSARY.
C
 3    CALL CLOC(IP,'MCFI',3)
      CALL UCOPY(IDATA(IP+1),HMCFIX,IDATA(IP))
      IF(OLD)CALL UCOPY(HUNI1,HUNIT,41)
C
      CALL CLOC(IP,'MFSU',4)
      CALL UCOPY(IDATA(IP+1),HMFSUR,IDATA(IP))
C
      CALL CLOC(IP,'MCSU',5)
      CALL UCOPY(IDATA(IP+1),HMCSUR,IDATA(IP))
C
      CALL CLOC(IP,'MCEL',6)
      CALL UCOPY(IDATA(IP+1),HMCELE,IDATA(IP))
      IF( .NOT. OLD ) GO TO 4
      DO  8  I = 1,NCHAMS
      HVDRFT(I) = HVDR
 8    CONTINUE
 4    CONTINUE
C
      CALL CLOC(IP,'MCST',7)
      CALL UCOPY(IDATA(IP+1),HMCSTA,IDATA(IP))
      IF(OLD)CALL VZERO(HMCSTA,IDATA(IP))
C
      CALL CLOC(IP,'MUFI',8)
      CALL UCOPY(IDATA(IP+1),HFILDA,IDATA(IP))
C
      CALL CLOC(IP,'MUYO',9)
      CALL UCOPY(IDATA(IP+1),HYKNMI,IDATA(IP))
C
      CALL CLOC(IP,'MUEN',10)
      CALL UCOPY(IDATA(IP+1),IZEII,IDATA(IP))
C
C                            PRINT OUT CALIBRATION DESCRIPTION ONCE
C
      IF( .NOT. FIRST2 ) RETURN
        CALL MUMESS('MUCON ',NVERSN,DESCRP)
        CALL MUMESS('MUCON ',0,'WARNING: MESSAGE IS NOT REPEATED!^')
        FIRST2 = .FALSE.
C
      RETURN
C
C                            NO MUON INFORMATION IN THE BANKS.
C
 98   IF( .NOT. FIRST1 ) RETURN
        CALL MUMESS('MUCON ',0,'MUON CALIB. DATA BANKS NOT PRESENT.^')
        CALL MUMESS('MUCON ',0,'WARNING: MESSAGE IS NOT REPEATED!^')
        FIRST1 = .FALSE.
C
      RETURN
      END
