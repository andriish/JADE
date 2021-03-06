C   07/06/96 606071840  MEMBER NAME  CUBDER   (S4)          FORTG1
      SUBROUTINE CUBDER(P,Q,NU)
      REAL P(5,1),Q(5,1),RN
      REAL*8 SUM
      EXTERNAL CUBSPL
C
C     DIFFERENTIATION/INTEGRATION OF SPLINE FUNCTIONS
C
C
C     INPUT
C     P-ARRAY(RESULTING FROM CALL COBSPL)
C     NU POSITIVE MEANS NU-TH DERIVATIVE
C     NU ZERO     MEANS COPY
C     NU  =-1     MEANS INTEGRATION
C
C     RESULT
C     Q-ARRAY SPLINE FUNCTION OF DERIVATIVE/INTEGRAL
C
      NUN=NU
      N=P(5,1)
      DO 10 I=1,N
      DO 10 J=1,5
   10 Q(J,I)=P(J,I)
   20 IF(NUN) 30,100,50
C     INTEGRATION
   30 K=N-1
      DO 40 J=1,K
      I=N-J
      DX=Q(1,I+1)-Q(1,I)
   40 Q(2,I+1)=
     1     (((3.0*Q(5,I)*DX+4.0*Q(4,I))*DX+6.0*Q(3,I))*DX
     2     +12.0*Q(2,I))*DX/12.0
      Q(2,1)=0.0
      SUM=0.0
      DO 45 I=2,N
      SUM=SUM+Q(2,I)
   45 Q(2,I)=SUM
      NUN=NUN+1
      GOTO 70
C     DIFFERENTIATION
   50 K=N-1
      CALL CUBFUN(Q(1,N),D,Q(2,N),Q)
      DO 60 I=1,K
   60 Q(2,I)=Q(3,I)
      NUN=NUN-1
   70 CALL CUBCOP(N,Q)
      GOTO 20
  100 RETURN
      END
