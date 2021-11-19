C   03/08/85 508062008 MEMBER NAME  DEFMAC   (S)           FORTRAN
C
C-----------------------------------------------------------------------
      SUBROUTINE DEFMAC
C-----------------------------------------------------------------------
C
C   AUTHOR:   C. BOWDERY  17/07/85 :  DEFINE A NEW MACRO OF COMMANDS
C
C LAST MOD:   C. BOWDERY   3/08/85 :  CHECK THAT NAME IS UNIQUE
C
C     THIS ROUTINE DEFINES A NEW MACRO COMMAND. FIRST THE NAME IS
C     PROMPTED FOR, THEN THE INPUT OF THE COMMANDS IS REQUESTED.
C
C-----------------------------------------------------------------------
C
      IMPLICIT INTEGER*2 (H)
C
      CHARACTER*1  CHARS
      CHARACTER*1  CDEF
C
      COMMON / CWORK  / NFIELD, CHARS(80), ISTAFL(80), IENDFL(80)
      COMMON / CGRMAC / MACNR, MACSTA, MACDEP, MACPNT(2,10), CDEF(80,31)
*** PMF 20/11/99
      CHARACTER*80 CHARS2,CCDEF(31)
      EQUIVALENCE(CHARS2,CHARS(1))
      EQUIVALENCE(CCDEF,CDEF)
*** PMF(end)
C
C------------------  C O D E  ------------------------------------------
C
      IF( MACNR .LT. 30 ) GO TO 5
        WRITE(6,2)
   2    FORMAT(' 30 macros already defined. No room for more.')
        RETURN
C
   5  WRITE(6,10)
  10  FORMAT(' Please enter the NAME of the new macro (8 letters max.)')
      READ(5,40) CHARS
  40  FORMAT(80A1)
      CALL CLTOU(CHARS2)         ! PMF 20/11/99
C
C                            FIND THE NAME
C
      CALL LOCFLD( CHARS, NFIELD, ISTAFL, IENDFL )
      IF( NFIELD .EQ. 0 ) RETURN
C
C                            CORRECT LENGTH AND ONLY ONE?
C
      LEN = IENDFL(1) - ISTAFL(1) + 1
      IF( NFIELD .EQ. 1  .AND.  LEN .LE. 8 ) GO TO 60
        WRITE(6,50)
  50    FORMAT(' Sorry. Only one name of 8 letters or less and no ";"',
     +         '. Please try again')
        GO TO 5
C
C                            IS THE NAME UNIQUE?
C
  60  CALL FINABR( CHARS(ISTAFL(1)), LEN, IREC, MAXARG, IS )
      IF( IREC .LE. 0 ) GO TO 70
        WRITE(6,65)
  65    FORMAT(' A command or macro with that name already exists.'/
     +         ' Use EDITMAC if you wish to redefine a macro   or')
        GO TO 5
C
C                            READ DEFINITION
C
  70  MACNR = MACNR + 1
C
  75  WRITE(6,80)
  80  FORMAT(' Please enter the definition of the macro (1 line)')
C
      READ(5,90) (CDEF(K,MACNR),K=1,80)
  90  FORMAT(80A1)
      CALL CLTOU(CCDEF(MACNR)) ! PMF 20/11/99
C
C                            NULL LINE?
C
      CALL LOCFLD( CDEF(1,MACNR), NFIELD, ISTAFL, IENDFL )
      IF( NFIELD .GT. 0 ) GO TO 100
        MACNR = MACNR - 1
        WRITE(6,95)
  95    FORMAT(' Macro definition aborted. No macro defined.')
        RETURN
C
C
C                            ADD NAME TO COMMAND LIST
C                            COMMAND NUMBER IS MACRO NUMBER + BASE NO.
C
 100  NUMCMD = MACNR + MACSTA
      CALL SETCMD( NUMCMD, CHARS(ISTAFL(1)), 0, 0,0,0 )
C
      RETURN
      END
