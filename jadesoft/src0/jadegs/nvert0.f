C   19/02/84 402192259  MEMBER NAME  NVERT0   (JADEGS)      FORTRAN
C
C-----------------------------------------------------------------------
      SUBROUTINE NVERT(S,AM,V)
C-----------------------------------------------------------------------
C
C      PRODUCE NORMALLY DISTRIBUTED NUMBERS WITH SD S AROUND
C      MEAN VALUE AM
C      THE RESULT IS RETURNED IN V
C
C-----------------------------------------------------------------------
C
      A=0.
      DO 1 I=1,12
    1 A=A+RN(DUM)
      V=(A-6.)*S+AM
      RETURN
      END
