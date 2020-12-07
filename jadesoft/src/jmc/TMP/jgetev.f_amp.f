C   25/11/83 402061213  MEMBER NAME  JGETEV   (S)           FORTRAN
C
C----------------------------------------------------------------------
      SUBROUTINE JGETEV( * , * ) ! PMF 22/06/00: add branch on RETURN2
C----------------------------------------------------------------------
C
C   AUTHOR    E. ELSEN    02/11/81 :  GET NEXT EVENT
C
C        MOD  J. OLSSON   30/08/83 :  TRACK A SUBSET OF THE DATASET
C        MOD  C. BOWDERY  17/11/83 :  CHANGE ERROR MESSAGE FROM BRVECT
C        MOD  W. BARTEL   29/11/83 :  CHANGE /CIEV/ DEFAULT IN BLDAT
C        MOD  C. BOWDERY   2/12/83 :  ALTER COMMON /CVERR/
C   LAST MOD  C. BOWDERY   6/02/84 :  IEV --> KIEV IN FORTRAN LINE 7000
C
C   Last mod.
C    P. Movilla Fernandez 22/06/00 :  Introduce `RETURN2' replacing `GO TO 95'
C   Last mod.
C    P. Movilla Fernandez 21/12/00 :  Use subroutine CPREAD to read
C                                     CPROD file with different endian format
C
C   GET NEXT EVENT BY CALLING BRVECT.
C
C   TO ALLOW FOR TRACKING ONLY A SELECTED PART OF THE
C   INPUT 4-VECTOR FILE. EVENT COUNT IS PASSED TO MCJADE VIA /CIEV/IEV
C
C   RETURN 1 IF END OF INPUT SEQUENCE (EOF)   OR
C            IF READING PASSES MAXIMUM EVENT NR ALLOWED
C
C   RETURN 2 If BRVECT detects an error when reading an event.
C            This is necessary in order to synchronize the event counter
C            IEV here and in subroutine MCJADE. Otherwise the counter
C            KIEV is not handled correctly.
C
C----------------------------------------------------------------------
C
      IMPLICIT INTEGER*2 (H)
C
      COMMON / CIEVS  / KIEV,IEVMIN,IEVMAX
C
      COMMON / CVERR / MESSAG(20)
C
      DATA IRUNIT / 3 / , ICALL/ 0 /
C PMF, 21/12/2000:
C MAXCP is the size of the CPROD common, see 'cprod.for'
      INTEGER MAXCP
      PARAMETER( MAXCP=7512 )
      INTEGER NBUFF(MAXCP)
C PMF, 21/12/00: IFORM tells us the endian format of the CPROD file
      INTEGER IFORM
      COMMON /CPFORM/ IFORM
C
C------------------  C O D E  -----------------------------------------
C
      ICALL = ICALL + 1
      IF(ICALL.NE.1) GO TO 95
      IEV = 0
      KIEV = 0
95    IEV = IEV + 1
1001  IF(IEV.GE.IEVMIN) GO TO 2001
*** PMF 23/06/00: Print out number of read events
*   (usefull in case of large CPROD files)
      if( mod(iev,1000).eq.0 ) write(*,'(A,I7,A)')
     >     'JGETEV:',IEV,' events read.'
*** PMF (end)
C
C             READ EVENTS, UNTIL EVENT IEVMIN REACHED, THEN CALL BRVECT
C
      IEV = IEV + 1
      IF( IFORM.GE.0 ) THEN
         READ(IRUNIT,ERR=1100,END=8100) NR
      ELSE
C PMF, 21/12/00: Read binary with different endian format using CFGET      
         CALL CPREAD(NBUFF,' ',*1100,*8100)
      ENDIF
      GO TO 1001
C
2001  IF(IEV.GT.IEVMAX) GO TO 90
C
      KIEV = KIEV + 1
      CALL BRVECT( IRUNIT, *1000, *8100 )
      RETURN
C
C             MESSAGES
C
 1000 MESSAG(20) = MESSAG(20) + 1
      IF( MESSAG(20) .GT. 100 ) RETURN 2 ! PMF 22/06/00: RETURN2 instead of GO TO 95
      WRITE(6,8002) IEV
 8002 FORMAT(' *** WARNING ***   ERROR DETECTED IN JGETEV FOR EVENT ',
     +        'ON FILE AT POSITION ',I6,'.    EVENT SKIPPED     ***'/)
      IF( MESSAG(20) .EQ. 100 ) WRITE(6,8003)
 8003 FORMAT(/' *** WARNING ***   NO FURTHER ERROR MESSAGES FROM JGETEV'
     +       ,' WILL BE PRINTED.  SEE ERROR SUMMARY AT END    ***'/)
      RETURN 2 ! PMF 22/06/00: RETURN2 instead of GO TO 95
C
 1100 WRITE(6,1102) IEV
 1102 FORMAT(/' *** READ ERROR  *** DETECTED IN JGETEV FOR EVENT ON ',
     +       'FILE AT POSITION ',I8,'. EVENT SKIPPED     ***'/)
      RETURN 2 ! PMF 22/06/00: RETURN2 instead of GO TO 95
C
 90   WRITE(6,8012)
 8012 FORMAT(/' -----   MAX NR OF EVENTS READ   -----')
      GO TO 7000
C
 8100 WRITE(6,8101)
 8101 FORMAT(/' -----   E O F   E O F   E O F   E O F   ----- ')
C
 7000 KIEV = KIEV - 1
      RETURN 1
      END
