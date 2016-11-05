C   19/11/79 306101856  MEMBER NAME  PEDC     (S)           FORTRAN
      SUBROUTINE PEDC
#include "tfprm.for"
#include "tfadc.for"
      DIMENSION A(84,5),B(84,5),DIF(84,5)
      CALL SETSL(DIF,0,420*4,0)
      DO   5   I=1,42
      A(2*I-1,1) = PEDLM(I)
      A(2*I  ,1) = PEDLP(I)
    5 CONTINUE
      KEND = 4
      IUN = 10
   10 CONTINUE
      K = IUN-8
      IF(K.LT.1) K = 1
      READ(IUN) (A(L,K),L=1,84)
      IUN = IUN + 1
      IF(IUN.LE.8+KEND)  GOTO  10
      IF(KEND.LT.2)  GOTO  25
      DO   20   L=1,84
      DO   15   K=2,KEND
      DIF(L,K) = A(L,K) - A(L,1)
   15 CONTINUE
   20 CONTINUE
C
   25 DO   30   L=1,84
      PRINT 101,L,(A (L,K),K=1,KEND),(DIF(L,K),K=1,KEND)
 101  FORMAT(I5,10F10.3)
   30 CONTINUE
      PRINT 103
  103 FORMAT(///)
C
      RETURN
      END
