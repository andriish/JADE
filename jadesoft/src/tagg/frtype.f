C   12/03/84 406221950  MEMBER NAME  FRTYPE   (S)           FORTRAN
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
C FRTYPE FINDS THE 'R TYPE' OF ADC SOFWARE ADDRESS I
C SEE CSLPS ROUTINE FOR EXPLANATION OF RTYPE (THIS IS FOR 1983 ONWARDS
C  ONLY)
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
      FUNCTION FRTYPE(I)
C
      IF ( (I .GE.  1) .AND. (I .LE.  8) ) FRTYPE = 1
      IF ( (I .GE.  9) .AND. (I .LE. 16) ) FRTYPE = 2
      IF ( (I .GE. 17) .AND. (I .LE. 24) ) FRTYPE = 3
      IF ( (I .GE. 25) .AND. (I .LE. 32) ) FRTYPE = 1
      IF ( (I .GE. 33) .AND. (I .LE. 40) ) FRTYPE = 2
      IF ( (I .GE. 41) .AND. (I .LE. 48) ) FRTYPE = 3
C
      RETURN
      END
