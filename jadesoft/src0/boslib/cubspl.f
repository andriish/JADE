C   07/06/96 606071840  MEMBER NAME  CUBSPL   (S4)          FORTG1
      SUBROUTINE CUBSPL(X,Y,M,P)
      REAL X(1),Y(1),P(5,1)
      DATA MC/1/,CA/0.0/,CB/0.0/
C
C     MC     CONDITION
C      1     Y''(1) = CA
C            Y''(N) = CB
C
C      2     Y''(1) = Y''(2) * CA
C            Y''(N) = Y''(N-1) * CB
C
C      3     Y'(1) = CA
C            Y'(N) = CB
C
C
      N=M
      DO 10 I=1,N
      P(1,I)=X(I)
      P(2,I)=Y(I)
   10 CONTINUE
      GOTO 15
C
      ENTRY CUBCOP(M,P)
      N=M
      IF(N.NE.0) GOTO 15
      N=P(5,1)
   15 CONTINUE
C
      GOTO (111,222,333),MC
C
  111 N1=N-1
      P(3,1)=0.0
      P(4,1)=0.0
      DO 30 K=1,N1
      J2=K+1
      H2=P(1,J2)-P(1,K)
      R2=(P(2,J2)-P(2,K))/H2
      IF(K.EQ.1) GOTO 25
      Z=1.0/(2.0*(H1+H2)-H1*P(4,J1))
      P(4,K)=Z*H2
      H=6.0*(R2-R1)
      IF(K.EQ.2) H=H-H1*CA
      IF(K.EQ.N1) H=H-H1*CB
      P(3,K)=Z*(H-H1*P(3,J1))
   25 J1=K
      H1=H2
      R1=R2
   30 CONTINUE
      P(5,1)=CA
      P(5,N1)=P(3,N1)
      P(5,N)=CB
      IF(N1.LE.2) GOTO 90
      N2=N1-1
      DO 35 J1=2,N2
      K=N-J1
      P(5,K)=P(3,K)-P(4,K)*P(5,K+1)
   35 CONTINUE
      GOTO 90
C
C
  222 N1=N-1
      P(3,1)=0.0
      P(4,1)=0.0
      F2=2.0
      DO 55 K=1,N1
      J2=K+1
      H2=P(1,J2)-P(1,K)
      R2=(P(2,J2)-P(1,K))/H2
      IF(K.EQ.1) GOTO 50
      F1=2.0
      IF(K.EQ.2) F1=2.0+CA
      IF(K.EQ.N1) F2=2.0+CB
      Z=1.0/((F1-P(4,J1))*H1+F2*H2)
      P(4,K)=Z*H2
      P(3,K)=Z*(6.0*(R2-R1)-H1*P(3,J1))
   50 J1=K
      H1=H2
      R1=R2
   55 CONTINUE
      P(5,N1)=P(3,N1)
      IF(N1.LE.2) GOTO 65
      N2=N1-1
      DO 60 J1=2,N2
      K=N-J1
      P(5,K)=P(3,K)-P(4,K)*P(5,K+1)
   60 CONTINUE
   65 P(5,1)=CA*P(5,2)
      P(5,N)=CB*P(5,N1)
      GOTO 90
C
C
  333 N1=N-1
      J1=1
      H1=0.0
      P(3,1)=0.0
      P(4,1)=0.0
      R1=CA
      DO 80 K=1,N
      IF(K.LE.N1) GOTO 70
      H2=0.0
      R2=CB
      GOTO 75
   70 J2=K+1
      H2=P(1,J2)-P(1,K)
      R2=(P(2,J2)-P(2,K))/H2
   75 Z=1.0/(2.0*(H1+H2)-H1*P(4,J1))
      P(4,K)=Z*H2
      P(3,K)=Z*(6.0*(R2-R1)-H1*P(3,J1))
      J1=K
      H1=H2
      R1=R2
   80 CONTINUE
      P(5,N)=P(3,N)
      DO 85 J1=1,N1
      K=N-J1
      P(5,K)=P(3,K)-P(4,K)*P(5,K+1)
   85 CONTINUE
C
   90 DO 95 K=1,N1
      H2=P(1,K+1)-P(1,K)
      Y2=P(5,K)
      P(5,K)=(P(5,K+1)-Y2)/(6.0*H2)
      P(4,K)=0.5*Y2
      R2=P(2,K+1)-P(2,K)
      P(3,K)=R2/H2-(P(5,K+1)+2.0*Y2)*H2/6.0
   95 CONTINUE
      P(3,N)=0.0
      P(4,N)=0.0
      P(5,N)=P(5,1)
      P(5,1)=FLOAT(N)+0.5
      MC=1
      CA=0.0
      CB=0.0
      GOTO 100
C
      ENTRY CUBFUN(X,Y,DER,P)
      N=P(5,1)
      P(5,1)=P(5,N)
      I=1
      J=N
      IF(X(1).LT.P(1,I)) GOTO 1
      IF(X(1).LT.P(1,J)) GOTO 2
      K=J-1
      GOTO 4
    1 K=1
      GOTO 4
    2 K=(I+J)/2
      IF(I.EQ.K) GOTO 4
      IF(X(1).LT.P(1,K)) GOTO 3
      I=K
      GOTO 2
    3 J=K
      GOTO 2
C
    4 DX=X(1)-P(1,K)
      Y(1)=((P(5,K)*DX+P(4,K))*DX+P(3,K))*DX+P(2,K)
      DER=(3.0*P(5,K)*DX+2.0*P(4,K))*DX+P(3,K)
      P(5,N)=P(5,1)
      P(5,1)=FLOAT(N)+0.5
  100 RETURN
C
      ENTRY CUBDEF(MCX,CAX,CBX)
      IF(MCX.LT.1.OR.MCX.GT.3) GOTO 100
      MC=MCX
      CA=CAX
      CB=CBX
      GOTO 100
      END
