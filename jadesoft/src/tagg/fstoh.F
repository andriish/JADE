C   12/03/84 406201108  MEMBER NAME  FSTOH    (S)           FORTRAN
C
C
C
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
      FUNCTION FSTOH(I)
C
C CONVERT SOFTWARE ADDRESS TO HARDWARE ADDRESS
C SEE ALSO FHTOS
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
#include "cwktag.for"
C
      IF ( MARK .LT. 2 ) GOTO 100
C
C---                               HERE FOR 1983... TAGGER
C
      IF ( (I .LT.  1) .OR.  (I .GT. 48) ) FSTOH =  -1
      IF ( (I .GE.  1) .AND. (I .LE.  8) ) FSTOH = I +  3
      IF ( (I .GE.  9) .AND. (I .LE. 16) ) FSTOH = I + 11
      IF ( (I .GE. 17) .AND. (I .LE. 24) ) FSTOH = I + 19
      IF ( (I .GE. 25) .AND. (I .LE. 32) ) FSTOH = I + 27
      IF ( (I .GE. 33) .AND. (I .LE. 40) ) FSTOH = I + 35
      IF ( (I .GE. 41) .AND. (I .LE. 48) ) FSTOH = I + 43
      RETURN
C
C---                                HERE FOR 1981/2 TAGGER
C
 100  FSTOH = I - 1
      RETURN
      END
