C   17/05/88 805171203  MEMBER NAME  SPHRCY   (S)           FORTRAN77
      SUBROUTINE zSPHRCY( P, NMOM, SPHITY , AXIS ) ! PMF 11/04/00 SPHRCY->zSPHRCY
C *---------------------------------------------------------
C *
C *  VERSION OF 23/02/79
C *  COMPUTE SPHERICITY FOR A SET OF MOMENTA
C *  P IS ARRAY OF MOMENTA P(1..4,1..NMOM), WHERE P(1..3)=PX,PY,PZ
C *  AND P(4)=PTOT
C *  SPHITY IS ARRAY OF NORMALIZED SPHERICITIES
C *  AXIS CONTAINS THE THREE EIGENVECTORS
C *  AXIS(1..3)  EIGENVECTOR WITH SMALLEST EIGENVALUE
C *  AXIS(4..6)  EIGENVECTOR
C *  AXIS(7..9)  EIGENVECTOR WITH BIGGEST EIGENVALUE
C *---------------------------------------------------------
C
      DIMENSION P(4,NMOM),T(6),R(9),AXIS(9), SPHITY(3)
C     DIMENSION A(6)
C
C  LOAD MOMENTUM TENSOR
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
CCCCCCCCC
C   DIAGONALIZE MOMENTUM TENSOR T
C
CAV Try to protect from NaNs
       if ( isnan(T(1))) GOTO 31
       if ( isnan(T(2))) GOTO 31
       if ( isnan(T(3))) GOTO 31
       if ( isnan(T(4))) GOTO 31
       if ( isnan(T(5))) GOTO 31
       if ( isnan(T(6))) GOTO 31
CAV
       CALL EIGEN( T, R, 3, 0 )
C
C      LOAD SPHERICTY AND JET AXIS
C
       SUM = T(1) + T(3) + T(6)
       SPHITY(1) = 3.* T(6) / SUM
       SPHITY(2) = 3.* T(3) / SUM
       SPHITY(3) = 3.* T(1) / SUM
       GOTO 32
   31  CONTINUE
        WRITE(*,*)'NAN in zsphrcy.f',p
        WRITE(*,*)'NMOM=',NMOM
        WRITE(*,*)'P=',p
        WRITE(*,*)'T=',T
   32  CONTINUE
       DO 30 I=1,3
   30  AXIS(I) = R(I+6)
       DO 40 I=4,6
   40  AXIS(I)= R(I)
       DO 50 I=7,9
   50  AXIS(I)= R(I-6)
      RETURN
      END
