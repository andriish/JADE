C   07/06/96 606071911  MEMBER NAME  SORTIR   (S4)          FORTG1
      SUBROUTINE SORTIR(A,N,M,IA)
      INTEGER A(2),AIM,AL
C
C     PURPOSE
C        SORT VECTORS A(*,J) OF ARRAY A(M,N) TO INCREASING ORDER
C        IN ELEMENT A(IA,J) (ASSUMED TO BE INTEGER)
C
C     USAGE
C                    - - - --
C        CALL SORTIR(A,N,M,IA)
C                    -
C        WHERE A( , ) = ARRAY
C              N      = NROF VECTORS
C              M      = LENGTH OF ONE VECTOR
C              IA     = INDEX OF SORTELEMENT IN VECTOR
C
C     EXAMPLE
C        INPUT     A  = 23.2  3  712.4  17.1  1  912.5
C        CALL      CALL SORTIR(A,2,3,2)
C        OUTPUT    A  = 17.1  1  912.5  23.2  3  712.4
C
C
      NM=(N-1)*M
      I=IA-M
   10 I=I+M
      IF(I.GT.NM) GOTO 100
      IF(A(I+M).GE.A(I)) GOTO 10
      AIM=A(I+M)
      K=I
   20 K=K-M
      IF(K.LE.0) GOTO 30
      IF(AIM.LT.A(K)) GOTO 20
   30 LM=K+M-IA+M
      DO 50 J=1,M
      L=I+M-IA+J
      AL=A(L)
   40 A(L)=A(L-M)
      L=L-M
      IF(L.GT.LM) GOTO 40
      A(L)=AL
   50 CONTINUE
      GOTO 10
  100 RETURN
      END
