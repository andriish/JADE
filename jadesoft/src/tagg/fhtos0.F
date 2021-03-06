C   12/03/84 403121301  MEMBER NAME  FHTOS    (S)           FORTRAN
C   24/07/83 403121234  MEMBER NAME  AFTP83   (S)           FORTRAN
C   03/05/83 307051220  MEMBER NAME  RALFUNC  (S)           FORTRAN
C   03/05/83            MEMBER NAME  RALFUNC  (S)           FORTRAN
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
      SUBROUTINE FHTOS(ISAD,IHAD,*)
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
C THIS ROUTINE CONVERTS HARDWARE ADDRESSES TO SOFTWARE ADDRESSES
C
C ISAD - OUTPUT - SOFTWARE ADDRESS
C IHAD - INPUT  - HARDWARE ADDRESS
C
C RETURN 1 IF IT CANT DO IT ( ILLEGAL ADDRESS SUPPLIED )
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
#include "cwktag.for"
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
C  CONVERT HARWARE ADDRESSES TO SOFTWARE ADDRESSES
C
C   THE PATTERN FOR 1983 ONWARDS IS -
C
C   HARDWARE ADDRESS                              SOFTWARE ADDRESS
C
C        0 - 3             NOT USED
C        4 - 7           INNER RING - Z               1 - 4
C        8 - 11            "      "                   5 - 8
C       12 - 15            NOT USED
C       16 - 19             "   "
C       20 - 23           MIDDLE RING - Z             9- 12
C       24 - 27              "  "                     13-16
C       28 - 31            NOT USED
C       32 - 35             "   '
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
C == CODE ===
C
      IF(MARK.LT.2)GOTO 100
C
C HERE FOR 1983 ONWARDS TAGGER
C
      K=0
      DO 10 I = 1,6
       ISTART=(I*16)-12
       IEND=ISTART+7
        DO 20 J=ISTART,IEND
         K=K+1
        IF(IHAD.NE.J) GOTO20
        ISAD=K
        RETURN
 20    CONTINUE
 10   CONTINUE
      RETURN    1
C
C HERE FOR 1981/2 TAGGER
C
C THE PATTERN IS MUCH SIMPLER !
C
 100  ISAD=IHAD+1
      RETURN
      END
