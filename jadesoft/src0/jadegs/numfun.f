C   05/06/84 807251748  MEMBER NAME  NUMFUN   (JADEGS)      FORTRAN
C
C-----------------------------------------------------------------------
      INTEGER FUNCTION NUMFUN( CHAR )
C-----------------------------------------------------------------------
C
C   AUTHOR:   C. BOWDERY   5/06/84 :  DETERMINE VALUE OF CHARACTER
C
C LAST MOD:   J. HAGEMANN 03/09/86 :  ' /' ADDED
C
C     RETURNS THE VALUE 0 - 9 IF CHAR CONTAINS THE CORRESPONDING
C     CHARACTER CODE.
C     ADDITIONALLY  NUMFUN = 10 FOR ' +'
C                          = 11 FOR ' -'
C                          = 12 FOR ' .'
C                          = 13 FOR ' /'
C     NUMFUN = -1 FOR ALL OTHER CHARACTERS
C
C-----------------------------------------------------------------------
C
      IMPLICIT INTEGER*2 (H)
C
      LOGICAL*1  CNUMB(14), CHAR
C
      DATA  CNUMB / '0','1','2','3','4','5','6','7','8','9',
     +              '+','-','.','/'/
C
C------------------  C O D E  ------------------------------------------
C
C                            CHECK FOR MATCH BETWEEN INPUT AND CNUMB
C
      DO  1  I = 1,14
        NUMFUN = I - 1
        IF( CHAR .EQ. CNUMB(I) ) RETURN
   1  CONTINUE
C
C                            CHAR WAS NOT RECOGNISED
C
      NUMFUN = -1
      RETURN
C
      END
