C   12/10/78 C9022601   MEMBER NAME  MASS2G   (LGSOURCE)    FORTRAN
      SUBROUTINE MASS2G
C
C     S.YAMADA   12-10-78   9:45
C     LAST MODIFICATION  16-10-78  08:55
C
C---- PLOT INVARIANT MASSES OF GAMMA PAIRS
C
      IMPLICIT INTEGER *2 (H)
C
C**** COMMON /CLGCLS/ NCLST,NCLBEC(3),HCLMAP(52),CLSPRP(10,51)
#include "cwork.for"
      DATA EMIN/0.04/, EMAX/2.0/
C
      IF(NCLST.LE.1) RETURN
      NC1 = NCLST-1
        DO 10 N=1,NC1
        IF(CLSPRP(8,N)) 10,11,10
   11   IF(CLSPRP(2,N).LT.EMIN.OR.CLSPRP(2,N).GT.EMAX) GO TO 10
        MS = N+1
          DO 20 M=MS,NCLST
          IF(CLSPRP(8,M)) 20,21,20
   21     IF(CLSPRP(2,M).LT.EMIN .OR. CLSPRP(2,M).GT.EMAX) GO TO 20
CCC       IF(CLSPRP(2,M).LT.CLSPRP(2,N)*0.1) GO TO 20
          COST = 0.
            DO 22 K=9,11
   22       COST = COST+CLSPRP(K,N)*CLSPRP(K,M)
          IF(COST.GT.1.0) COST = 1.0
          AMASS = SQRT(2.0*CLSPRP(2,N)*CLSPRP(2,M)*(1.0-COST))
          E2G = CLSPRP(2,N)+CLSPRP(2,M)
C//////////////////
C         WRITE(6,6000) N,M,CLSPRP(2,N),CLSPRP(2,M),COST,AMASS,E2G
C6000     FORMAT(' N,M=',2I4,'  E(N),E(M)=',2F6.3,'  COST,AMASS,E2G=',
C    $    3E11.3)
C//////////////////
C----     PLOT
          CALL HFILL(1,AMASS,E2G,1.0)
   20     CONTINUE
   10   CONTINUE
      RETURN
C
C*****************************************************************
C
      ENTRY INMS2G
C
      CALL HBOOK2(1,'TWO GAMMA MASS$',100,0.0,1.0, 50,0.0,5.0)
      CALL HBPRO(1,0)
      RETURN
C
C******************************************************************
C
      ENTRY ENMS2G
      CALL HISTDO
      RETURN
C
      END
