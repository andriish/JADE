C   24/11/78 C9082901   MEMBER NAME  ELGTOT   (JADENS)      FORTRAN
      SUBROUTINE ELGTOT(ESUM1)
C
C     TOTAL ENERGY IN LEAD GLASS COUNTERS: P.STEFFEN(78/11/15)
C
      IMPLICIT INTEGER*2 (H)
C
#include "cdata.for"
C
      ESUM = 0
C                                       CHECK IF ANY ENERGIES
      IALGL=IBLN('ALGL')
      IPLGL4=IDATA(IALGL)
      IF(IPLGL4.LE.0) GO TO 100
C                                       POINTER TO FIRST AND LAST ENERGY
      IP0 = IPLGL4*2+5
      IP9 = IP0 + HDATA(IP0-1) - 1
C                                       SUM ALL LG-ENERGIES
      DO 10 IP=IP0,IP9,2
         ESUM = ESUM + HDATA(IP+1)
  10  CONTINUE
C
 100  CONTINUE
      ESUM1 = ESUM
      RETURN
      END
