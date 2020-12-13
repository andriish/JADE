C   23/08/85 511061950  MEMBER NAME  MCTAGI   (S)           FORTRAN
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
C
      SUBROUTINE MCTAGI
C
C INITIALISATION ROUTINE FOR TAGGING MONTE CARLO ROUTINES
C  - MUST BE CALLED ONCE PER EVENT
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
       IMPLICIT INTEGER*2 (H)
C
      COMMON/TODAY/HDATE(6)
#include "cmctag.for"
C   CALIB - CALIBRATION THAT CONVERTS ADC CHANNEL NUMBER INTO ENERGY
      REAL*4 CALIB(10)
      DATA CALIB/ 5.5,10.0,10.0,7*6.667/
      REAL*4 SIGMA(10)
      DATA SIGMA/0.26,0.35,0.35,7*0.10/
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
C
C
C     DIMENSION IWF(3)
C     DATA IWF/3*0/
      DATA ICALL/0/
C
C
C
C
C ===== CODE ========
C
      ICALL = ICALL + 1
      IHIT = 0
C
      CALL VZERO(HGG,96)
C
C
C CHOOSE VALUE OF MARKMC BASED ON DATE
C
      MARKMC = 0
       IF(HDATE(6).GE.1979.AND.HDATE(6).LE.2000)GOTO 5
999    IF(ICALL.EQ.1)WRITE(6,6000)
6000   FORMAT(/,/,/,'  WARNING FROM MCTAGI HDATE(6) NOT SET ',
     1 /,' DEFAULT TAGGER MC IS MARKMC = 3 (1981/2) ')
      MARKMC = 3
                         GOTO 6
C
5      IF(HDATE(6).LE.1980)MARKMC = 1
       IF(HDATE(6).GE.1983)MARKMC = 4
       IF(HDATE(6).EQ.1982)MARKMC = 3
C THIS ONLY LEAVES 1982 TO DEAL WITH
C - NEED TO USE MONTH TO DECIDE WHETHER OR NOT THERE ARE SNOUTS
       IF(MARKMC.EQ.0.AND.HDATE(5).LT.8)MARKMC = 2
       IF(MARKMC.EQ.0)MARKMC = 3
C
      IF(ICALL.EQ.1)WRITE(6,6001)HDATE(4),HDATE(5),HDATE(6),MARKMC
6001  FORMAT(/,/,/,'  DATE TAKEN FROM COMMON 1TODAY IS ',
     1I2,'/',I2,'/',I4,' SO MARKMC FOR TAGGER SET TO ',I2)
C
 6    CONTINUE
C
C MARKMC IS NOW SET
C
C JUST CHECK
      IF(MARKMC.LT.1.OR.MARKMC.GT.4)GOTO 999
C
C
      IF(MARKMC.EQ.1)INMARK = 0
      IF(MARKMC.EQ.2)INMARK = 1
      IF(MARKMC.EQ.3)INMARK = 1
      IF(MARKMC.EQ.4)INMARK = 2
C
C WRITE OUT A FURTHER  INITIAL MESSAGE
C
      ITMRK = INMARK + 1
       IF(ICALL.EQ.1)WRITE(6,678)ITMRK
678    FORMAT(/,
     1' ++++++++++++++++++++++++++++++++++++++++++++++++++++',/,
     1' +                                                  +',/,
     1' +               FIRST CALL TO MCTAGI               +',/,
     1' +                                                  +',/,
     1' +                 MARK ',I1,8X,'                   +',/,
     1' +             TAGGER IS BEING SIMULATED            +',/,
     1' +                                                  +',/,
     1' +                                                  +',/,
     1' +              CHECK THAT TAGAN AGREES             +',/,
     1' +                   WITH THIS IN                   +',/,
     1' +                ANALYSIS (TP) STEP                +',/,
     1' +                                                  +',/,
     1' +    SEE JADE COMPUTER NOTES 74 AND 86 FOR MORE    +',/,
     1' +               DETAILS                            +',/,
     1' +                                                  +',/,
     1' ++++++++++++++++++++++++++++++++++++++++++++++++++++')
C
C
C SET SOME VARIABLES FOR LATER
C
        IF(MARKMC.EQ.1)NCHANS = 192
        IF(MARKMC.EQ.2)NCHANS = 64
        IF(MARKMC.EQ.3)NCHANS = 64
        IF(MARKMC.GE.4)NCHANS = 48
C
C    SET UP VALUES FOR MCTAGS,MCTAGR WHICH
C    ARE CALLED FROM LHATAG (ENTRY IN STATAG)
C
      SIGMAS = SIGMA(MARKMC)
      CALIBS = CALIB(MARKMC)
C
C
          RETURN
C
         END
