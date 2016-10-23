C   05/03/79 810042026  MEMBER NAME  SPHRCY   (JADEGS)      FORTRAN
C
C-----------------------------------------------------------------------
      SUBROUTINE SPHRCY( P, NMOM, SPHITY , AXIS )
C-----------------------------------------------------------------------
C
C    ORIGINAL VERSION OF 23/02/79
C
C         MOD 14.05.87    R.RAMCKE    AXIS(1..3,4..6,7..9)  NORMALISED
C    LAST MOD  4.10.88    C.BOWDERY   PROTECT AGAINST DIVISION BY ZERO
C
C    COMPUTE SPHERICITY FOR A SET OF MOMENTA
C    P IS ARRAY OF MOMENTA P(1..4,1..NMOM), WHERE P(1..4)=PX,PY,PZ,PTOT
C
C    SPHITY IS ARRAY OF NORMALISED EIGENVALUES
C
C    AXIS(1..3)  EIGENVECTOR WITH SMALLEST EIGENVALUE
C    AXIS(4..6)  EIGENVECTOR
C    AXIS(7..9)  EIGENVECTOR WITH BIGGEST EIGENVALUE
C
C-----------------------------------------------------------------------
C
      DIMENSION P(4,NMOM),T(6),R(9),AXIS(9), SPHITY(3)
C
C--------------  C O D E  ----------------------------------------------
C
C
C                            LOAD MOMENTUM TENSOR
C
       DO 10 I=1,6
   10  T(I)=0.
       DO 20 J=1,NMOM
       T(1) = T(1) + P(2,J)*P(2,J) + P(3,J)*P(3,J)
       T(2) = T(2) - P(1,J)*P(2,J)
       T(3) = T(3) + P(1,J)*P(1,J) + P(3,J)*P(3,J)
       T(4) = T(4) - P(1,J)*P(3,J)
       T(5) = T(5) - P(2,J)*P(3,J)
   20  T(6) = T(6) + P(1,J)*P(1,J) + P(2,J)*P(2,J)
C
C                            DIAGONALIZE MOMENTUM TENSOR T
C
       CALL EIGEN( T, R, 3, 0 )
C
C                            LOAD SPHERICTY AND JET AXIS
C
       SUM = T(1) + T(3) + T(6)
C
       IF( SUM .NE. 0.0 ) GO TO 25
         WRITE(6, 23)
   23    FORMAT(' * * * * *   DIVISION BY ZERO PREVENTED IN SPHRCY. ',
     +          ' RESULTS PROBABLY MEANINGLESS!')
         SUM = 1.0
C
   25  SPHITY(1) = 3.* T(6) / SUM
       SPHITY(2) = 3.* T(3) / SUM
       SPHITY(3) = 3.* T(1) / SUM
       DO 30 I=1,3
   30  AXIS(I) = R(I+6)
       DO 40 I=4,6
   40  AXIS(I)= R(I)
       DO 50 I=7,9
   50  AXIS(I)= R(I-6)
C
       K = -3
       DO 60 I = 1,3
         K = K + 3
         ASC   = SQRT(AXIS(K+1)**2 + AXIS(K+2)**2 + AXIS(K+3)**2)
         IF( ASC .NE. 0.0 ) GO TO 53
           WRITE(6,23)
           ASC = 1.0
C
 53      ASCAL = 1.0/ASC
         DO 55 J = 1,3
 55         AXIS(K+J) = AXIS(K+J)*ASCAL
 60    CONTINUE
C
      RETURN
      END
