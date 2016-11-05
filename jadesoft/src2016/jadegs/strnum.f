C   21/12/87 807261954  MEMBER NAME  STRNUM   (JADEGS)      FORTRAN
C
C-----------------------------------------------------------------------
      SUBROUTINE STRNUM( STRING, NCHAR, VALUE, IER )
C-----------------------------------------------------------------------
C
C   AUTHOR:   C. BOWDERY  21/12/87 :  CONVERT STRING INTO NUMBER 'VALUE'
C
C LAST MOD:   C. BOWDERY  26/07/88 :  CHECK FOR / CODE FROM NUMFUN
C
C     STRING CONTAINS NUMBERS AND OPTIONALLY '+-.' AND HAS A LENGTH
C     NCHAR. VALUE IS THE DERIVED VALUE. IER IS .TRUE. IF ERROR OCCURRED
C
C-----------------------------------------------------------------------
C
      IMPLICIT INTEGER*2 (H)
C
      LOGICAL  HADONE, IER
C
      LOGICAL*1  STRING(NCHAR)
C
C------------------  C O D E  ------------------------------------------
C
C
   2  NDIG   =  0
      NBP    = -1
      N      =  0
      SIGN   =  1.0
      HADONE = .FALSE.
C                                                CONVERT CHAR TO NUMBER
      DO  6  I = 1,NCHAR
C
C                                                ONLY ALLOW DIGITS,+-.
C
        MVAL = NUMFUN( STRING(I) )
        IF( MVAL .LT. 0  .OR.  MVAL .GT. 12 ) GO TO 99
C                                                PLUS SIGN?
        IF( MVAL .NE. 10 ) GO TO 3
          IF( I .NE. 1 ) GO TO 99
          GO TO 6
C                                                MINUS SIGN?
   3    IF( MVAL .NE. 11 ) GO TO 4
          IF( I .NE. 1 ) GO TO 99
          SIGN = -1.0
          GO TO 6
C                                                POINT? ALREADY HAD ONE?
   4    IF( MVAL .NE. 12 ) GO TO 5
          IF( HADONE ) GO TO 99
          HADONE = .TRUE.
          NBP    = 0
          GO TO 6
C
   5    N    = 10 * N + MVAL
        NDIG = NDIG + 1
        IF( NBP .GE. 0 ) NBP = NBP + 1
C
C                            DANGER OF OVERFLOW ?
C
        IF( N .LT. 2**26 ) GO TO 6
C
C                            YES. IF WE ARE PROCESSING FRACTION THEN
C                            JUST DROP LAST DIGITS. OTHERWISE ERROR.
C
          IF( HADONE ) GO TO 7
          GO TO 99
C
   6  CONTINUE
C
   7  IF( NDIG .EQ. 0 ) GO TO 99
      IF( NBP .EQ. -1 ) NBP = 0
      VALUE = FLOAT(N) * SIGN / (10.0 ** NBP)
C
      IER = .FALSE.
      RETURN
C
C                            HANDLE ANY ERRORS.
C
 99   IER = .TRUE.
      RETURN
C
      END
