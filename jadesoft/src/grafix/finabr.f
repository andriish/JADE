C   06/06/84 507222016  MEMBER NAME  FINABR   (S)           FORTRAN
C
C-----------------------------------------------------------------------
      SUBROUTINE FINABR( CCS, LEN, ICOMD, MAXARG, IS )
C-----------------------------------------------------------------------
C
C   AUTHOR:   C. BOWDERY   6/06/84 :  FIND COMMAND ABBREVIATION MATCH
C
C
C   INPUT   CCS    STRING OF LENGTH LEN BYTES WITH COMMAND IN IT
C           LEN    LENGTH OF STRING
C
C   OUTPUT  ICOMD  FOUND COMMAND.  0 = COMMAND NOT FOUND.
C           MAXARG MAX. NO. OF ARGUMENTS ALLOWED FOR THIS COMMAND
C           IS     POINTER TO START OF NUMERICAL ARGUMENT IN CCS
C
C-----------------------------------------------------------------------
C
      IMPLICIT INTEGER*2 (H)
C
      LOGICAL*1  CCMD,CBLANK,CCS(LEN)
C
      COMMON / CGRAP4 / IPOS4,CCMD(8,200),HCMD(5,200)
C
      DATA  CBLANK / ' ' /
C
C-------------------  C O D E  -----------------------------------------
C
C                            IS  POINTS TO START OF ARGUMENT IF IN FIELD
C
        IS = 0
C
C                            LOOP OVER THE ABBREVIATIONS LIST
C
        DO  8  J = 1,IPOS4
          ICOMD  = HCMD( 1, J )
          MAXARG = HCMD( 2, J )
          K      = 1
C
C                            LOOP OVER THE CHARS IN THE WORD FIELD
C                            AND CHECK FOR A MATCH
C
          DO  7  I = 1,LEN
            IF( K .GT. 8  .AND.  NUMFUN( CCS(I) ) .GE. 0 ) GO TO 15
            IF( CCS(I) .EQ. CCMD(K,J) ) GO TO 5
C
C                            THIS CHAR DOES NOT MATCH.
C                            IF THE INPUT CHAR IS A NUMBER AND THE
C                            COMMAND HAS NO MORE LETTERS  ==> SUCCESS
C
              IF( NUMFUN(CCS(I))  .GE. 0  .AND.
     +            CCMD(K,J) .EQ. CBLANK         ) GO TO 15
              GO TO 8
C
C                            THIS LETTER MATCHES
C
   5        K     = K + 1
   7      CONTINUE
C
          IF( K .GT. 8 ) RETURN
          IF( CCMD(K,J) .EQ. CBLANK ) RETURN
C
   8    CONTINUE
C
C                            NO MATCH
C
        ICOMD = 0
        RETURN
C
C                            THE END OF THIS FIELD FROM  I  ONWARDS
C                            CONTAINS A NUMERICAL ARGUMENT
C
  15    IS    = I
        RETURN
C
        END
