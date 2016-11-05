C   05/12/84 412051658  MEMBER NAME  TAGPHI   (S)           FORTRAN
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
C
      FUNCTION TAGPHI(I)
C
C
C   CALCULATE THE AZIMUTHAL ANGLE PHI OF THE CENTRE
C   OF A BLOCK WITH ADC SOFTWARE ADDRESS I
C
C   VALID FOR 1983 -->  DETECTOR ONLY
C
C
C
C-----------------------  C O D E  -----------------------------------
C
      DATA PIBY4 / 0.785398163 /
      DATA PIBY8 / 0.392699081 /
C
      ISAVE = I
C
C
      IF ( (I .GE.  1) .AND. (I .LE.  6) ) J = I + 2
      IF ( (I .GE.  7) .AND. (I .LE.  8) ) J = I - 6
      IF ( (I .GE.  9) .AND. (I .LE. 14) ) J = I + 2 -  8
      IF ( (I .GE. 15) .AND. (I .LE. 16) ) J = I - 6 -  8
      IF ( (I .GE. 17) .AND. (I .LE. 22) ) J = I + 2 - 16
      IF ( (I .GE. 23) .AND. (I .LE. 24) ) J = I - 6 - 16
      IF ( (I .GE. 25) .AND. (I .LE. 26) ) J = I + 6 - 24
      IF ( (I .GE. 27) .AND. (I .LE. 32) ) J = I - 2 - 24
      IF ( (I .GE. 33) .AND. (I .LE. 34) ) J = I + 6 -  8 - 24
      IF ( (I .GE. 35) .AND. (I .LE. 40) ) J = I - 2 -  8 - 24
      IF ( (I .GE. 41) .AND. (I .LE. 42) ) J = I + 6 - 16 - 24
      IF ( (I .GE. 43) .AND. (I .LE. 48) ) J = I - 2 - 16 - 24
C

      TAGPHI = (J * PIBY4) - PIBY8
C
      I = ISAVE
      RETURN
      END
