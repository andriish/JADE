C   05/06/84 504242304  MEMBER NAME  JADARG   (S)           FORTRAN
C
C-----------------------------------------------------------------------
      SUBROUTINE JADARG( CARG, NCHAR, NUM, ARG )
C-----------------------------------------------------------------------
C
C   AUTHOR:   C. BOWDERY   5/06/84 :  EXTRACT ARGUMENT FROM CARG STRING
C
C  LAST MOD:  C. BOWDERY  24/04/85 :  CORRECTION IF BAD ARGUMENT GIVEN
C
C     CARG IS A STRING CONTAINING NUMBERS AND OPTIONALLY '+-.' OF
C     LENGTH NCHAR. NUM IS THE ARGUMENT NUMBER FOR ERROR PRINTING
C     PURPOSES AND ARG IS DERIVED VALUE.
C
C-----------------------------------------------------------------------
C
      IMPLICIT INTEGER*2 (H)
C
      LOGICAL  HADONE
C
      LOGICAL*1  CARG(NCHAR), CHARS(15), CBLANK
C
      DATA  CBLANK / ' ' /
C
C------------------  C O D E  ------------------------------------------
C
C
C                            COPY INPUT STRING TO LOCAL STRING CHARS
C
      DO  1  I = 1,NCHAR
        CHARS(I) = CARG(I)
   1  CONTINUE
      NCHARS = NCHAR
C
   2  NDIG   =  0
      NBP    = -1
      N      =  0
      SIGN   =  1.0
      HADONE = .FALSE.
C                                                CONVERT CHAR TO NUMBER
      DO  6  I = 1,NCHARS
        MVAL = NUMFUN( CHARS(I) )
        IF( MVAL .LT. 0 ) GO TO 99
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
      ARG = N * SIGN / (10.0 ** NBP)
C
      RETURN
C
C                            HANDLE ANY ERRORS.
C
 99   WRITE(6,98) NUM, (CHARS(L),L=1,NCHARS)
 98   FORMAT(' Error: Argument ',I1,' is bad ---> ',80A1)
      WRITE(6,94)
 94   FORMAT(' Please re-enter this value:')
      READ(5,97) CHARS
 97   FORMAT(15A1)
C
      NSTA = 0
      DO  66  I = 1,15
        IF( CHARS(I) .NE. CBLANK ) GO TO 64
          IF( NSTA .GT. 0 ) GO TO 65
          GO TO 66
 64     IF( NSTA .EQ. 0 ) NSTA = I
 66   CONTINUE
      I = 16
C
 65   NCHARS = I - NSTA
      IF( NSTA .EQ. 0 ) GO TO 69
      IF( NSTA .EQ. 1 ) GO TO 2
C
      DO  68  K = 1,NCHARS
        CHARS(K) = CHARS(K+NSTA-1)
 68   CONTINUE
      GO TO 2
C
 69   ARG = 0.0
      RETURN
C
      END
