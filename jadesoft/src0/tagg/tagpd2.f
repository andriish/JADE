C   12/03/84 412041859  MEMBER NAME  PEDFX3   (S)           FORTRAN
C
C
C
C
C
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
C    SECOND ROUTINE TO ATTEMPT OFFLINE SUBTRACTION OF PEDESTALS
C     FOR EACH 'SUPERBLOCK' OF 3 LEAD SCINTILLATOR ELEMENTS IT
C   CHECKS THAT
C     NOONE CHANNEL IS GREATER THAT 500 MEV,IF THIS IS THE CASE ALL
C  THREE BLOCKS ARE IGNORED , OTHERWISE THE VALUES IN THE BLOCKS ARE
C   USED TO ESTIMATE A MEAN ADC PEDESTAL , WHICH IS THEN SUBTACTED
C   FROMALL BLOCKS.
C
C A.J.FINCH 11/12/82
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
       SUBROUTINE TAGPD2
C
C
#include "cwktag.for"
C
C
       VALUE  = 500.0
       APICKM = 0.0
       APICKP = 0.0
       NUMM   = 0
       NUMP   = 0
C
C
C---                                MINUS Z FIRST
C
C---                                LOOP OVER ALL 8 'CAKESLICES'
C
       DO 10 I = 1,8
C
C---                                LOOP OVER 3 PARTS OF 'CAKESLICE'
C---                                AND TEST FOR ENERGY > 'VALUE'
C
          DO 20 J = 1,17,8
             ITEST = J+I-1
             IF ( CATAG(ITEST) .GT. VALUE ) GOTO 10
   20     CONTINUE
C
C   NONE OF THE BLOCKS IN THE 'CAKESLICE' UNDER TEST HAVE FAILED
C   TO BE LESS THAN 'VALUE'
C
C
C---                                SUM ALL NON ZERO BLOCKS IN NON-HIT
C---                                'CAKESLICE' AND COUNT HOW MANY
C---                                THERE ARE
C
          DO 30 L = 1,17,8
             ITEST = I+L-1
             IF ( CATAG(ITEST) .EQ. 0 ) GOTO 30
             APICKM = APICKM + CATAG(ITEST)
             NUMM = NUMM + 1
   30     CONTINUE
   10  CONTINUE
C
C
C
C
C
C---                                NOW PLUS Z
C
C
C
C
C
C---                                LOOP OVER ALL 8 'CAKESLICES'
C
       DO 11 I = 25,32
C
C---                                LOOP OVER 3 PARTS OF 'CAKESLICE'
C---                                AND TEST FOR ENERGY > 'VALUE'
C
C
          DO 21 J = 1,17,8
             ITEST = J + I - 1
             IF ( CATAG(ITEST) .GT. VALUE ) GOTO 11
   21     CONTINUE
C
C   NONE OF THE BLOCKS IN THE SUPERBLOCK UNDER TEST HAVE FAILED
C   TO BE LESS THAN VALUE
C
C---                                SUM ALL NON ZERO BLOCKS IN NON-HIT
C---                                'CAKESLICE' AND COUNT HOW MANY
C---                                THERE ARE
          DO 31 L = 1,17,8
             ITEST = I + L - 1
             IF ( CATAG(ITEST) .EQ. 0 ) GOTO 31
             APICKP = APICKP + CATAG(ITEST)
             NUMP = NUMP + 1
   31     CONTINUE
   11  CONTINUE
C
C
C
C---------------------------------- SUBTRACT PEDESTAL FROM CATAG ARRAY
C
C
C NOW WE HAVE THE VALUE APICKP AND APICKM TO BE SUBTRACTED
C  DO THE SUBTRACTION FROM CATAG ARRAY
C
C
C
C---                                CALC THE MEAN NON ZERO PEDESTAL - Z
C
       IF ( NUMM .LE. 6 ) GOTO 50
       APICKM = APICKM / NUMM
C
C
       DO 40 I = 1,24
          IF ( CATAG(I) .EQ. 0 ) GOTO 40
          CATAG(I) = CATAG(I) - APICKM
          IF ( CATAG(I) .LT. 0) CATAG(I) = 0
   40  CONTINUE
C
C
C---                                SAME FOR +Z
C
C

   50  IF ( NUMP .LE. 6 ) GOTO 41
C      IF ( NUMP .LE. 4 ) GOTO 41
C
       APICKP = APICKP / NUMP
C
       DO 41 I = 25,48
          IF ( CATAG(I) .EQ. 0 ) GOTO 41
          CATAG(I) = CATAG(I) - APICKP
          IF ( CATAG(I) .LT. 0) CATAG(I) = 0
   41  CONTINUE
C
C
   51  RETURN
       END
