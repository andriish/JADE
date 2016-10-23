C   22/05/84 507222026  MEMBER NAME  SETCMD   (S)           FORTRAN
C
C-----------------------------------------------------------------------
      SUBROUTINE SETCMD( NUMCMD, CABBRV, MAXARG, HPR1, HPR2, HPR3 )
C-----------------------------------------------------------------------
C
C   AUTHOR:   C. BOWDERY  11/05/84 :  SET UP A COMMAND
C
C
C     THIS ROUTINE SETS UP A SINGLE COMMAND IN THE COMMAND LIST.
C     EACH COMMAND HAS A SHORT FORM (UP TO 8 LETTERS), AN IDENTITY
C     NUMBER WHICH DOES NOT HAVE TO BE UNIQUE TO A GIVEN STRING, MAX NO.
C     OF ARGUMENTS AND ASSOCIATED COMMAND WORDS.
C
C-----------------------------------------------------------------------
C
      IMPLICIT INTEGER*2 (H)
C
      LOGICAL*1  CABBRV(8), CCMD
C
      COMMON / CGRAP4 / IPOS4,CCMD(8,200),HCMD(5,200)
C
C------------------  C O D E  ------------------------------------------
C
C
      IF( IPOS4 .LT. 200 ) GO TO 10
         WRITE(6,5)
   5     FORMAT(' Error: Command definition array exceeded (200).',
     +          ' Report this to graphics expert!')
         RETURN
C
  10  IPOS4 = IPOS4 + 1
C
C                            ADD THE COMMAND ABBREVIATION (8 CHARS.)
C                            STORED IN THE CABBRV ARRAY.
C
      DO  20  I = 1,8
        CCMD(I,IPOS4) = CABBRV(I)
  20  CONTINUE
C
C                            ADD THE COMMAND NUMBER AND MAX. NO. OF ARGS
C
      HCMD(1,IPOS4) = NUMCMD
      HCMD(2,IPOS4) = MAXARG
C
C                            ADD THE COMMAND WORD POINTERS
C
      HCMD(3,IPOS4) = HPR1
      HCMD(4,IPOS4) = HPR2
      HCMD(5,IPOS4) = HPR3
C
      RETURN
      END
