C   17/11/84 711091458  MEMBER NAME  DATEMC0  (S)           FORTRAN
               SUBROUTINE DATEMC(H)
               IMPLICIT INTEGER*2 (H)
               DIMENSION H(6)
C   COMMON TODAY MAY BE FILLED BY USER
               COMMON/TODAY/HSS,HMM,HHH,HDY,HMO,HYY
               H(1)=HSS
               H(2)=HMM
               H(3)=HHH
               H(4)=HDY
               H(5)=HMO
               H(6)=HYY
               RETURN
               END
