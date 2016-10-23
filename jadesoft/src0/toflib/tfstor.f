C   17/01/79 003201013  MEMBER NAME  TFSTOR   (S)           FORTRAN
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
       SUBROUTINE TFSTOR(NTK,IFL,NCNT,TOF,PL,BETA,DBETA,EMAS2,DMAS,
     -                   PRO,PKA,PPI,PEL,DEDX)
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
       COMMON/CWORK/NR,RAW(5,42),NC,ICRT1(5,42),NTRK,ICRT2(50),TRK(5,50)
     - ,ITRC(50),NTC,ITRK(5,42),INFM(4),IR(14,50)
       DIMENSION RE(14,50)
       EQUIVALENCE (IR(1,1),RE(1,1))
            IR( 1,NTK)= NTK
            IR( 2,NTK)= IFL
            IR( 3,NTK)= NCNT
            RE( 4,NTK)= TOF
            RE( 5,NTK)= PL
            RE( 6,NTK)= BETA
            RE( 7,NTK)= DBETA
            EM= ABS(EMAS2)
            EM= SQRT(EM)
            IFG= 0
            IF(EM.LT.1.E-50) IFG= 1
            IF(EMAS2.LT.0.) EM= -EM
            IF(IFG.EQ.0) GO TO 100
            RE( 8,NTK)= EM
            GO TO 200
100         RE( 8,NTK)= EM
200         IF(DMAS.GT.1.E-50) GO TO 300
            RE( 9,NTK)= DMAS
            GO TO 400
300         RE( 9,NTK)= DMAS
400         RE(10,NTK)= PRO
            RE(11,NTK)= PKA
            RE(12,NTK)= PPI
            RE(13,NTK)= PEL
            RE(14,NTK)= DEDX
            RETURN
            END
