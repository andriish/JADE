C   05/12/84 508151400  MEMBER NAME  TAGFTY   (S)           FORTRAN
C
C
C-----------------------------------------------------------------------
      FUNCTION TAGFTY(IN,ONIN)
C-----------------------------------------------------------------------
C
C
C   TAGFTY FINDS THE FI-TYPE  OF  ADC  I
C   RELATIVE TO  ADC  ONE(SOFTWARE ADDRESSES) :
C
C   FI-TYPE = 1   FOR BLOCKS IN THE  - FI  DIRECTION FROM BLOCK 'ONE'
C           = 2   FOR BLOCKS IN THE SAME FI  DIRECTION AS BLOCK 'ONE'
C           = 3   FOR BLOCKS IN THE  + FI  DIRECTION FROM BLOCK 'ONE'
C
C   VALID FOR 1983 --> DETECTOR ONLY
C
C
C---------------------------------------------------------------------
C
      INTEGER  ONE, ONIN
C
C-------------------------  C O D E  ---------------------------------
C
      ONE = ONIN
      I   = IN
C
C---                          SUBTRACT 8 UNTIL  1 <=  I  <= 8
C
       DO 10 J = 1 , 5
          IF ( I .LE. 8 ) GOTO 11
          I = I - 8
   10  CONTINUE
   11  CONTINUE
C
C---                          SUBTRACT 8 UNTIL  1 <= ONE <= 8
C
       DO 12 J = 1,5
          IF ( ONE .LE. 8 ) GOTO 13
          ONE = ONE - 8
   12  CONTINUE
   13  CONTINUE
C
C---                          SPECIAL CASE FOR END BLOCKS
C
      IF ( ( ONE .EQ. 1 ) .AND. ( I .EQ. 8 )  ) I = 0
      IF ( ( ONE .EQ. 8 ) .AND. ( I .EQ. 1 )  ) I = 9
C
      TAGFTY = (I - ONE ) + 2
C
C
      RETURN
      END
