C   22/05/84 807251604  MEMBER NAME  JADCMD   (S)           FORTRAN
C
C-----------------------------------------------------------------------
      SUBROUTINE JADCMD( CICS, NFLDS, IST, IEND, ICOMD , A, B )
C-----------------------------------------------------------------------
C
C   AUTHOR:   C. BOWDERY   5/06/84 :  DETERMINE WHICH JADE COMMAND
C
C      MOD:   C. BOWDERY  17/07/85 :  INCREASE DIMENSIONS OF IST,IEND
C LAST MOD:   J. HAGEMANN 03/09/86 :  ALLOW ARGUMENT TO BE A STRING
C                                  :  CHARACTER BEFORE STRING HAS TO
C                                  :  BE A '/'
C
C
C IN    CICS      = INPUT COMMAND STRING
C       NFLDS     = NUMBER OF FIELDS LOCATED IN CICS
C       IST       = ARRAY OF STARTING POSITIONS OF THE FIELDS IN CICS
C       IEND      = ARRAY OF ENDING POSITIONS OF THE FIELDS IN CICS
C
C OUT   ICOMD     = COMMAND NUMBER; 0 = NOT FOUND; -1 = NO COMMAND WORDS
C       A         = FIRST ARGUMENT (MAY BE ZERO)
C       B         = SECOND ARGUMENT (MAY BE ZERO)
C
C     INITIALLY THE FIELDS ARE EXAMINED TO DETERMINE HOW MANY COMMAND
C     WORDS ARE PRESENT (OR A SINGLE ABBREVIATION) AND HOW MANY
C     ARGUMENTS (IF ANY) FOLLOW. IF ONLY 1 COMMAND FIELD IS FOUND, THE
C     ABBREVIATIONS ARE SEARCHED FOR A MATCH. IF SUCCESSFUL THE
C     ARGUMENTS ARE EXTRACTED. IF NOT OR IF SEVERAL COMMAND FIELDS ARE
C     PRESENT, THE FIELDS ARE COMPARED WITH THE COMMAND WORD DICTIONARY
C     AND THE BEST MATCH WITH THE COMMAND LIST IS MADE.
C
C-----------------------------------------------------------------------
C
      IMPLICIT INTEGER*2 (H)
C
      character  CICS(80), CSLASH, CHLP(4)
C
      EQUIVALENCE( A1,CHLP(1))
C
      DIMENSION  IST(80), IEND(80), HWRD(10)
C
      DATA CSLASH / '/' /
C
C-------------------  C O D E  -----------------------------------------
C
C
      A  = 0.0
      B  = 0.0
      A1 = 0.0
C
C                            DETERMINE HOW MANY COMMAND WORDS PRESENT.
C                            NUMFUN RETURNS VALUE OF DIGIT (0-9) OR
C                            10-13 FOR '+','-','.','/'. OTHERWISE -1
C
      NCOMW = 0
      DO  1  I = 1,NFLDS
        IF( NUMFUN( CICS(IST(I)) ) .GE. 0 ) GO TO 2
        NCOMW = NCOMW + 1
   1  CONTINUE
C
C                            THERE IS NO COMMAND HERE
C
   2  IF( NCOMW .NE. 0 ) GO TO 3
        ICOMD = -1
        RETURN
C
C                            THERE IS A COMMAND HERE
C                            IF ONLY 1 WORD, TRY THE ABBREVIATIONS
C
   3  IF( NCOMW .GT. 1 ) GO TO 10
        LEN = IEND(1) - IST(1) + 1
        CALL FINABR( CICS( IST(1) ), LEN, ICOMD, MAXARG, IS )
        IF( ICOMD .EQ. 0 ) GO TO 10
        IF( IS .EQ. 0 ) GO TO 20
          IS = IS + IST(1) - 1
          GO TO 15
C
C                            DECODE THE COMMAND WORDS
C                            USING THE DICTIONARY DEFINED BY DEFWRD
C
  10  DO  11  N = 1,NCOMW
        LEN = IEND(N) - IST(N) + 1
        CALL FINWRD( CICS(IST(N)), LEN, N, NCOMW, HWRD(N), IS )
  11  CONTINUE
C
C                            PROCESS THESE WORDS TO DETERMINE COMMAND
C
      CALL FINCMD( HWRD, NCOMW, ICOMD, MAXARG )
      IF( ICOMD .LT. 1 ) RETURN
C
C                            IF  IS = 0  THEN THE LAST COMMAND
C                            FIELD DID NOT CONTAIN THE FIRST ARGUMENT
C
      IF( IS .EQ. 0 ) GO TO 20
C
C                            COMMAND RECOGNISED BUT FIRST ARGUMENT IS
C                            PART OF THE COMMAND FIELD SO SET START
C                            POSITION AND LENGTH OF ARGUMENT.
C
  15    LEN   = IEND(NCOMW) - IS + 1
        IARGP = NCOMW
        GO TO 23
C
C                            ARE THERE ANY ARGS?
C
  20    IF( NCOMW .EQ. NFLDS ) RETURN
C
C                            SET START POSITION AND LENGTH OF ARGUMENT
C
        IARGP = NCOMW + 1
        IS    = IST( IARGP )
        LEN   = IEND( IARGP ) - IS + 1
C
C                            ARE ANY ARGUMENTS ALLOWED?
C
  23  IF( MAXARG .LT. 1 ) GO TO 30
C
C                            TEST IF ARGUMENT IS A BANK NAME
C
      IF( CICS(IS) .NE. CSLASH ) GO TO 25
         DO 24 IC = 1, 4
            CHLP(IC) = CICS(IS+IC)
  24     CONTINUE
         A = A1
         GO TO 27
C
C                            EXTRACT FIRST ARGUMENT
C
  25  CALL JADARG( CICS(IS), LEN, 1, A )
C
C                            IS THERE A 2ND ARGUMENT? IS IT ALLOWED?
C
  27  IF( IARGP .EQ. NFLDS ) RETURN
      IF( MAXARG .LT. 2 ) GO TO 30
      IARGP = IARGP + 1
C
C                            EXTRACT SECOND ARGUMENT
C
      CALL JADARG( CICS(IST(IARGP)), IEND(IARGP)-IST(IARGP)+1, 2, B )
      IF( IARGP .EQ. NFLDS ) RETURN
C
C                            TOO MANY ARGUMENTS
C
  30  WRITE(6,31) MAXARG
  31  FORMAT(' Warning: Too many arguments.'/' Limit is ',I1,
     +       ' for this command. Excess values ignored.')
      RETURN
C
      END
