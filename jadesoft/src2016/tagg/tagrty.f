C   05/12/84 412051723  MEMBER NAME  TAGRTY   (S)           FORTRAN
C
C
C
C
      FUNCTION TAGRTY(I)
C
C
C   TAGRTY FINDS THE 'R TYPE' OF ADC SOFWARE ADDRESS I
C
C   R-TYPE = 1   FOR INNER   BLOCKS
C          = 2       MIDDLE BLOCKS
C          = 3       OUTERBLOCKS
C
C   VALID FOR 1983 --> DETECTOR ONLY
C
C------------------  C O D E  ------------------------------------------
C
      IF ( ( I .GE.  1 ) .AND. ( I .LE.  8 ) ) TAGRTY = 1
      IF ( ( I .GE.  9 ) .AND. ( I .LE. 16 ) ) TAGRTY = 2
      IF ( ( I .GE. 17 ) .AND. ( I .LE. 24 ) ) TAGRTY = 3
      IF ( ( I .GE. 25 ) .AND. ( I .LE. 32 ) ) TAGRTY = 1
      IF ( ( I .GE. 33 ) .AND. ( I .LE. 40 ) ) TAGRTY = 2
      IF ( ( I .GE. 41 ) .AND. ( I .LE. 48 ) ) TAGRTY = 3
C
      RETURN
      END
