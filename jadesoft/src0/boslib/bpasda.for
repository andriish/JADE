C   07/06/96 606071827  MEMBER NAME  BPASDA   (S4)          FORTG1
      SUBROUTINE BPASDA(IUNDA,IPASS)
C     BOS SUBPROGRAM   =4.7=
#include "ccs.for"
      IUND=IUNDA
      CALL BDLS('+DIR',IUND)
      LISTE(1)=0
      LISTE(2)=0
      IPAS=IPASS
      CALL PODA(1,LISTE,1)
      RETURN
      END
