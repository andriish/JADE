C   12/03/84 503141427  MEMBER NAME  TAGKAL   (S)           FORTRAN
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
C
C   THIS SUBROUTINE RECALIBRATES ADC'S USING FACTORS IN
C  COMMON CALIBR
C
C INPUT - IWRITE - IF THIS IS EQUAL TO ONE THE ROUTINE
C                  WRITES OUT SOME DEBUGGING INFORMATION
C
C  A.J.FINCH 6/10/83
C  LAST MOD :   J. NYE  24/05/83  TIDIED UP
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
       SUBROUTINE TAGKAL(IWRITE)
C
C
       IMPLICIT INTEGER * 2 (H)
#include "cdata.for"
C
#include "cwktag.for"
C
#include "calibr.for"
C
       DIMENSION FACTOR(64)
C
       DATA ICOUNT / 0 /
       DATA IDONT  / 0 /
C
C
C------------------------- C O D E -------------------------------------
C
C
C---                                     TEST FOR MONTE CARLO DATA
C---                                     - DONT CALIBRATE IF MC
C
       IF ( IMC .EQ. 1 ) GOTO 15
C
C
C===DEBUG  START=========================DEBUG  START===================
C
       IF ( IWRITE .NE. 1 ) GOTO 9
          WRITE(6,610)
          WRITE(6,605 ) ( CATAG(I),I=1,IENDPZ )
  610     FORMAT(1X,'CATAG ARRAY BEFORE RECALIBRATION ')
C
C===DEBUG  END===========================DEBUG  END=====================
C
C
C
C
C---                                     GET POINTER TO CALIBRATION
C---                                     CONSTANTS IN COMMON/CALIBR/
C
    9  IPOINT = 2 * ICALIB(12)
C
C---                                    ON FIRST CALL ONLY - CHECK
C---                                     CALIB CONSTANTS ARE SENSIBLE
C
       IF ( ICOUNT .EQ. 0 ) GOTO 8
       ICOUNT = 1
C
C                                        THIS LOOP ON FIRST EVENT ONLY
C
       DO 108 I = ISTMZ,IENDPZ
          J = IPOINT + I
          FACT = 0.01 * HCALIB(J)
          IF (  (FACT .LT. 0) .OR. (FACT .GT. 10.0) ) IDONT = 1
  108  CONTINUE
C
       IF ( IDONT .EQ. 1 ) WRITE(6,608)
  608  FORMAT(/,/,' WARNING FROM TAGGING ANALYSIS ROUTINE -TAGKAL-',
     *          /,' ****   ILLEGAL CALIBRATION CONSTANT',
     *               ' FOUND ON FIRST CALL   ****')
C
C
C--------------------------------------- READ FACTORS FROM CALIBR
C
    8  CONTINUE
       IF ( IDONT .EQ. 1 ) GOTO 11
C
C
       DO 10 I = ISTMZ,IENDPZ
          J = IPOINT + I
          CATAG (I) = 0.01 * HCALIB(J) * CATAG(I)
          FACTOR(I) = 0.01 * HCALIB(J)
   10  CONTINUE
C
   11  CONTINUE
       IF ( IWRITE .NE. 1 ) RETURN
C
C
C
C
C--------------------------------------- D E B U G   O N L Y -----------
C
C
C
       IF ( IDONT .EQ. 0 ) WRITE(6,600)
       IF ( IDONT .EQ. 0 ) WRITE(6,605 ) ( FACTOR(I),I=1,IENDPZ )
       IF ( IDONT .EQ. 1 ) WRITE(6,607)
       IF ( IDONT .EQ. 1 ) WRITE(6,611)
       WRITE(6,606)
       WRITE(6,605 ) ( CATAG(I),I=1,IENDPZ )
  600  FORMAT(' CALIBRATION CONSTANTS IN TAGKAL ')
  606  FORMAT(' CATAG ARRAY AFTER RECALIBRATION ')
  605  FORMAT(6(/,8(2X,F8.2) ) )
  607  FORMAT(/,' MESSAGE FROM TAGKAL --- NO RECALIBRATION OF TAGGING',
     *             ' DATA IS BEING PERFORMED ')
  611  FORMAT('  BECAUSE ILLEGAL CALIBRATION CONSTANTS WERE FOUND',
     *           ' IN FIRST EVENT')
C
       RETURN
C
C
C
C--------------------------------------- MONTE CARLO DEBUG -------------
C
C
15    CONTINUE
      IF ( IWRITE .EQ. 1 ) WRITE(6,607)
      IF ( IWRITE .EQ. 1 ) WRITE(6,612)
 612  FORMAT('  BECAUSE THIS IS MONTE CARLO DATA ')
      RETURN
      END
