C   23/08/85 508241351  MEMBER NAME  MCTAGS   (S)           FORTRAN
C-----------------------------------------------------------------------
C   15/03/84 403152035  MEMBER NAME  MCTAGS   (S)           FORTRAN
C
C ROUTINE TO SMEAR OUT CONTENTS OF ARRAY HGG
C IN ACCORDANCE WITH OBSERVED RESOLUTION
C
C
C
C AND CONVERT TO ADC CHANNEL NUMBER FROM ENERGY
C
C
C A.J.FINCH 3/84
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
       SUBROUTINE MCTAGS
       IMPLICIT INTEGER*2 (H)
#include "cmctag.for"
C
       DATA CUTOFF/20.0/
C
C
       TEST = CUTOFF * CALIBS
C
       DO 1 I = 1,NCHANS
         IF(HGG(I).LE.TEST)HGG(I)=0
         IF(HGG(I).LE.0)    GOTO 1
C
          ENERGY = HGG(I)/1000.0
          SMEAR=SQRT(ENERGY)*SIGMAS
          CALL NVERT(SMEAR,ENERGY,ENEW)
          HGG(I) = (ENEW*1000.0)/CALIBS
         IF(HGG(I).LE.CUTOFF)HGG(I)=0
   1   CONTINUE
       RETURN
       END
