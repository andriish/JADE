C   18/06/84 705211142  MEMBER NAME  FHTOS    (S)           FORTRAN
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
       SUBROUTINE FHTOS(ISAD,IHAD,*)
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
C  THIS ROUTINE CONVERTS HARDWARE ADDRESSES TO SOFTWARE ADDRESSES
C
C  ISAD - OUTPUT - SOFTWARE ADDRESS
C  IHAD - INPUT  - HARDWARE ADDRESS
C
C  RETURN 1 IF IT CANT DO IT ( ILLEGAL ADDRESS SUPPLIED )
C
C
C  NEW VERSION 29/07/84     A.J.FINCH
C  LAST MOD :  18/067/84    J NYE  TIDIED UP
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
#include "cwktag.for"
C
      INTEGER*2 HDATA
#include "cdata.for"
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
C   CONVERT HARWARE ADDRESSES TO SOFTWARE ADDRESSES
C
C    THE PATTERN FOR 1983 ONWARDS IS -
C
C    HARDWARE ADDRESS                              SOFTWARE ADDRESS
C
C         0 - 3             NOT USED
C         4 - 7           INNER RING - Z               1 - 4
C         8 - 11            "      "                   5 - 8
C        12 - 15            NOT USED
C        16 - 19             "   "
C        20 - 23           MIDDLE RING - Z             9- 12
C        24 - 27              "  "                     13-16
C        28 - 31            NOT USED
C        32 - 35             "   "
C        36 - 39            OUTER RING - Z         17-20
C        40 - 43              "   "                21-24
C        44 - 47            NOT USED
C
C        48 - 51            NOT USED
C        52 - 55          INNER RING + Z              25 - 28
C        56 - 59            "      "                  29 - 32
C        60 - 63            NOT USED
C        64 - 67             "   "
C        68 - 71           MIDDLE RING + Z            33- 36
C        72 - 75              "  "                    37 -40
C        76 - 79            NOT USED
C        80 - 83             "   "
C        84 - 87            OUTER RING + Z         41-44
C        88 - 91              "   "                45-48
C        92 - 95            NOT USED
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
C
C
C---------------------------- C O D E ----------------------------------
C
C
C
       IF ( MARK .LT. 2 ) GOTO 100
C
C---                                     HERE FOR 1983 ONWARDS TAGGER
C
       K    =   0
       J    = -12
       ISAD =   0
       DO 10 K = 3,43,8
          J = J + 16
          IF ( (IHAD .GE. J) .AND. (IHAD .LE. (J+7)) ) ISAD = IHAD - K
          IF ( ISAD .NE. 0 ) GOTO 30
  10   CONTINUE
       RETURN 1
C
C---                                     CORRECT FOR BAD CABLING
C
C   FOR REAL DATA BETWEEN RUNS 14589
C   AND 15689 CABLES FROM COUNTERS
C   38 AND 40 TO ADCS WERE INTERCHANGED
C
  30    CONTINUE
        IF ( (ISAD .NE. 38) .AND. (ISAD .NE. 40) ) RETURN
        IF ( IMC .EQ. 1 ) RETURN
        IND   = IDATA(IBLN('HEAD'))
        IF ( IND .LE. 0 ) RETURN
        NRUN  = HDATA(IND+10)
        IF ( (NRUN .LT. 14589) .OR. (NRUN .GT. 15689) ) RETURN
        ISAVE = ISAD
        IF ( ISAVE .EQ. 38 ) ISAD = 40
        IF ( ISAVE .EQ. 40 ) ISAD = 38
        RETURN
C
C---                                     HERE FOR 1981/2 TAGGER
C
C  THE PATTERN IS MUCH SIMPLER !
C
  100  ISAD = IHAD + 1
       RETURN
       END
