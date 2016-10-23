C   05/02/80 801211400  MEMBER NAME  DEDXBK   (JADEGS)      FORTRAN
      SUBROUTINE DEDXBK(NPPATR)
C*800213*OLSSON*****************************************************
C* LAST MOD 21/01/88   E ELSEN                                     *
C* C R E A T E  B A N K  DEDX  F R O M  D E D X A N  R E S U L T S *
C* STRAIGHT COPY OF COMMON CWORK1                                  *
C*******************************************************************
C             NPPATR IS POINTER TO 'PATR' ;  SAME BOSBANK NR IS USED
      IMPLICIT INTEGER*2 (H)
#include "cdata.for"
      COMMON /CWORK1/ IER,NTR,TRES(10,60)
      DIMENSION ITRES(10,60)
      EQUIVALENCE (TRES(1,1),ITRES(1,1))
C
C
      IF(IER.NE.0 .OR.NTR.EQ.0) GO TO 100
        NTRR = MIN0(NTR,60)
C
        NRPATR = IDATA(NPPATR-2)
        NWRES = 2 + NTRR*10
        CALL CCRE(NPDEDX,'DEDX',NRPATR,NWRES,IERR)
        CALL BSAW(1,'DEDX')
        IF(IERR.NE.2) CALL UCOPY( IER, IDATA(NPDEDX+1), IDATA(NPDEDX) )
  100 RETURN
      END
