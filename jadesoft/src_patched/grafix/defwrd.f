C   17/05/84 406142013  MEMBER NAME  DEFWRD   (S)           FORTRAN
C
C-----------------------------------------------------------------------
      SUBROUTINE DEFWRD( HSTART, CSTRNG )
C-----------------------------------------------------------------------
C
C   AUTHOR:   C. BOWDERY  17/05/84 :  ADD CSTRNG TO DICTIONARY
C
C
C     THIS ROUTINE ADDS THE COMMAND WORD IN CSTRNG (ENDS IN ^) TO THE
C     DICTIONARY ARRAY CDICTY IN / CGRAP3 /.  IDEFWP IS BLOCKDATA SET 0.
C     BE CAREFUL NOT TO OVERFLOW THE CDICTY ARRAY.
C
C-----------------------------------------------------------------------
C
      IMPLICIT INTEGER*2 (H)
C
      CHARACTER*1  CDICTY, CSTRNG(20), CSTOP
C
      COMMON / CGRAP3 / IDEFWP, CDICTY(700)
C
      DATA  CSTOP / '^' /
C
C------------------  C O D E  ------------------------------------------
C
C
C                            COPY BYTE STRING IN CSTRNG TO DICTIONARY
C                            ARRAY CDICTY. STOP WHEN ^ IS FOUND.
C
      J = 0
      DO  1  I = 1,20
        CDICTY( I + IDEFWP ) = CSTRNG(I)
        J = J + 1
        IF( CSTRNG(I) .EQ. CSTOP ) GO TO 3
   1  CONTINUE
C
      WRITE(6,2) (CSTRNG(K),K=1,20)
   2  FORMAT(' ERROR DURING COMMAND DEFINITION PHASE'/
     +       ' CSTRNG WAS --> ',20A1/
     +       ' PROBABLY A ''^'' CHARACTER IS MISSING IN S/R COMWRD')
      STOP 999
C
C                            STORE THE STARTING POINT OF THIS WORD
C
   3  HSTART = IDEFWP + 1
      IDEFWP = IDEFWP + J
      RETURN
      END
