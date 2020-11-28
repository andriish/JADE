C   08/06/86 807251549  MEMBER NAME  GPHMAIN  (S)           FORTRAN
C
C----------------------------------------------------------------------
C     MAIN PROGRAM FOR JADE GRAPHICS           JUNE       1986
      PROGRAM GPHMAIN
C----------------------------------------------------------------------
C
C   AUTHOR:   L. O'NEILL  17/05/78 :  MAIN PROGRAM FOR GRAPHICS
C
C        MOD  C. BOWDERY  13/03/84 :  MAJOR OVERHAUL
C        MOD  C. BOWDERY  25/04/84 :  CDTL DEFAULTS CHANGED
C        MOD  C. BOWDERY  14/06/84 :  NEW COMMONS BLOCK DATA SET
C        MOD  C. BOWDERY  12/10/84 :  BLOCK DATA CHANGE
C        MOD  J. HAGEMANN 24/10/84 :  UPDATE PRINTOUT
C        MOD  C. BOWDERY  24/04/85 :  BLOCK DATA COMES FROM MACRO NOW
C        MOD  C. BOWDERY   9/07/85 :  LINK HELP DATASET (DIRECT ACCESS)
C        MOD  C. BOWDERY   3/08/85 :  TIDY UP SCREEN CLEARING
C        MOD  C. BOWDERY   8/06/86 :  CODE PUT INTO SUBROUTINES
C   LAST MOD  J. HAGEMANN 05/03/88 :  FOR OWN BLOCKDATA
C
C         THIS PROGRAM STARTS A GRAPHICS SESSION AND CALLS SUPERV
C   ==>   It contains lower case letters. Check ASIS mode!
C
C----------------------------------------------------------------------
C
      IMPLICIT INTEGER*2 (H)
C
      character*4 IHLP
      integer*2 HERR
      CHARACTER*8   DDN,VOLSR
      LOGICAL  OK
C
C                            MACRO CBCSMX.... BOS COMMON + SIZE DEFINED
C
#include "cbcsmx.for"
#include "cgraph.for"
C
      COMMON / CGAMOD / MODEGA, GAHEX
C
      DIMENSION IHLP(11)
C
      DATA  VOLSR/'        '/
C      
      DATA  IHLP /'F22B','OW.H','ELP.','DATA','SET ','    ',5*'    '/
C
C------------------  C O D E  -----------------------------------------
C
C                            INITIALISE PLOT10
C
      CALL INITT(DUMMY)
      CALL TWINDO(0,4095,0,4095)
C
C                            ARE WE RUNNING IN GA MODE? MODEGA = 1 ?
C                            IF YES, CLEAR THE TSO SCREEN
C
      CALL TESTGA( MODEGA, GAHEX )
C
      IF( MODEGA .EQ. 1 ) CALL CLEAR
C
C                            WELCOME THE USER
C
      CALL WELCME
C
C                            ALLOCATE FORTRAN LUN TO INPUT DATA SET.
C
      CALL RALLOC( OK )
      IF( .NOT. OK ) GO TO 10
C
C                            ALLOCATE THE DIRECT ACCESS HELP DATASET
C
      CALL GETPDD(IHLP,VOLSR,DDN,50,HERR)
      IF( HERR .NE. 0  .AND.  HERR .NE. 1040 )
     +                  CALL TRMOUT(80,'Warning: The HELP dataset could
     +not be allocated.^')
C
C                            ALLOCATE THE CALIBRATION DATASETS
C
      CALL CALLOC ( OK )
      IF( .NOT. OK ) GO TO 10
C
C                            CLEAR SCREEN AND INITIALISE BOS
C
      CALL CLRCON
      CALL BINT(LDATA,LDATA,200,1)
C
C                            SET UP DEFAULT GRAPHICS VALUES
C
      CALL SETUPG
C
C                            CALL MAIN ANALYSIS SUPERVISOR ROUTINE.
C
      CALL SUPERV
C
C                            END SCAN SESSION AND TERMINATE PLOT10.
C                            OK IS INPUT ARGUMENT.
C
  10  CALL ENDMES( OK )
C
C                            CLEAR GRAPHICS SCREEN IF IN GA MODE
C                            THEN TERMINATE PLOT10.
C
      IF( MODEGA .EQ. 1 ) CALL ERASE
      CALL FINITT(0,0)
C
      STOP
      END
C
C                            BLOCK DATA FOR GRAPHICS PROGRAM
C
C #include "grblock.for" PMF 5.11.98
#include "grblock.F"
