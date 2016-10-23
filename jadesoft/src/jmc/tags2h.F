C   05/12/84 511071141  MEMBER NAME  TAGS2H   (S)           FORTRAN
C
C
C
C------  T A G S 2 H  ------  T A G S 2 H  ------  T A G S 2 H  ------
C
C------  T A G S 2 H  ------  T A G S 2 H  ------  T A G S 2 H  ------
C
C------  T A G S 2 H  ------  T A G S 2 H  ------  T A G S 2 H  ------
C
C
C
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
C
C
      FUNCTION TAGS2H(I)
C
C CONVERT SOFTWARE ADDRESS TO HARDWARE ADDRESS
C SEE ALSO TAGH2S
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
#include "cwktag.for"
C
      IF ( MARK .LT. 2 ) GOTO 100
C
C---                               HERE FOR 1983... TAGGER
C
      IF ( (I .LT.  1) .OR.  (I .GT. 48) ) TAGS2H =  -1
      IF ( (I .GE.  1) .AND. (I .LE.  8) ) TAGS2H = I +  3
      IF ( (I .GE.  9) .AND. (I .LE. 16) ) TAGS2H = I + 11
      IF ( (I .GE. 17) .AND. (I .LE. 24) ) TAGS2H = I + 19
      IF ( (I .GE. 25) .AND. (I .LE. 32) ) TAGS2H = I + 27
      IF ( (I .GE. 33) .AND. (I .LE. 40) ) TAGS2H = I + 35
      IF ( (I .GE. 41) .AND. (I .LE. 48) ) TAGS2H = I + 43
      RETURN
C
C---                                HERE FOR 1981/2 TAGGER
C
 100  TAGS2H = I - 1
      RETURN
      END
