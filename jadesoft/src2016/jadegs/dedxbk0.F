C   05/02/80 002161934  MEMBER NAME  DEDXBK0  (JADEGS)      FORTRAN
      SUBROUTINE DEDXBK(IPPATR)
C*800213*OLSSON*****************************************************
C*                                                                 *
C* C R E A T E  B A N K  DEDX  F R O M  D E D X A N  R E S U L T S *
C*                                                                 *
C*******************************************************************
C             IPPATR IS POINTER TO 'PATR' ;  SAME BOSBANK NR IS USED
      IMPLICIT INTEGER*2 (H)
#include "cdata.for"
      COMMON /CWORK1/ IER,NTR,TRES(10,60)
      DIMENSION ITRES(10,60)
      EQUIVALENCE (TRES(1,1),ITRES(1,1))
C
      IF(NTR.EQ.0) GO TO 100
      NTRR = MIN0(NTR,60)
C****
C
      NBNK = IDATA(IPPATR-2)
      NWRES = 2 + NTRR*10
      CALL CCRE(IPHT,'DEDX',NBNK,NWRES,IERR)
      IF(IERR.NE.0) GO TO 100
      CALL BSAW(1,'DEDX')
      IDATA(IPHT+1) = IER
      IDATA(IPHT+2) = NTRR
      DO 1  ITR = 1,NTRR
      ITRX = IPHT+2+(ITR-1)*10
      DO 1  I = 1,10
1     IDATA(ITRX+I) = ITRES(I,ITR)
  100 RETURN
      END
