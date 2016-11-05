C   07/06/96 606071918  MEMBER NAME  VECSUB   (S4)          FORTG1
C  DESYLIB
      SUBROUTINE VECSUB
C     --- P-ARRAY ---
      COMMON P(10,100)
      INTEGER IP(10,100)
      EQUIVALENCE (P(1,1),IP(1,1))
C     ---
      REAL*8 PP(5,100)
      EQUIVALENCE (P(1,1),PP(1,1))
      INTEGER NT/0/,IPR(5)
      REAL*8 PI/3.1415926536/
      REAL*8 A,B,C,D,E,G(3)
      REAL*4 F,H
      RETURN
C
      ENTRY ADD(K,L,M)
      A=P(4,K)+P(4,L)
      P(4,M)=A
      B=A*A
      DO 2 I=1,3
      A=P(I,K)+P(I,L)
      B=B-A*A
    2 P(I,M)=A
      P(5,M)=DSIGN(DSQRT(DABS(B)),B)
      DO 3 I=9,10
    3 IP(I,M)=IP(I,K)+IP(I,L)
      RETURN
C
      ENTRY SUB(K,L,M)
      A=P(4,K)-P(4,L)
      P(4,M)=A
      B=A*A
      DO 4 I=1,3
      A=P(I,K)-P(I,L)
      B=B-A*A
    4 P(I,M)=A
      P(5,M)=DSIGN(DSQRT(DABS(B)),B)
      RETURN
C
      ENTRY vLOR(K,L,M) !PMF 24/10/99 name changed from LOR to vLOR 
      A=0.0
      DO 6 I=1,3
    6 A=A+P(I,K)*P(I,L)
      A=(A/(P(4,L)+P(5,L))-P(4,K))/P(5,L)
      B=P(5,K)*P(5,K)
      DO 8 I=1,3
      C=P(I,K)+A*P(I,L)
      B=B+C*C
    8 P(I,M)=C
      P(4,M)=DSQRT(B)
      P(5,M)=P(5,K)
      RETURN
C
      ENTRY DOT4(K,L,U)
      A=P(4,K)*P(4,L)
      DO 10 I=1,3
   10 A=A-P(I,K)*P(I,L)
      U=A
      RETURN
C
      ENTRY IMPU(K,L,U)
      A=P(4,K)-P(4,L)
      B=-A*A
      DO 12 I=1,3
      A=P(I,K)-P(I,L)
   12 B=B+A*A
      U=B
      RETURN
C
      ENTRY IMPULI(K,L,U,V)
      A=0.0
      B=0.0
      DO 14 I=1,3
      A=A+P(I,K)*P(I,K)
   14 B=B+P(I,L)*P(I,L)
      C=P(4,K)-P(4,L)
      C=C*C
      D=2.0*DSQRT(A*B)
      E=A+B-C
      U=E-D
      V=E+D
      RETURN
C
      ENTRY ADD3(K,L,M)
      DO 22 I=1,3
      A=P(I,K)+P(I,L)
   22 P(I,M)=A
      RETURN
C
      ENTRY SUB3(K,L,M)
      DO 24 I=1,3
      A=P(I,K)-P(I,L)
   24 P(I,M)=A
      RETURN
C
      ENTRY CROSS(K,L,M)
      A=P(2,K)*P(3,L)
      B=P(3,K)*P(2,L)
      G(1)=A-B
      A=P(3,K)*P(1,L)
      B=P(1,K)*P(3,L)
      G(2)=A-B
      A=P(1,K)*P(2,L)
      B=P(2,K)*P(1,L)
      G(3)=A-B
      DO 26 I=1,3
   26 P(I,M)=G(I)
      RETURN
C
      ENTRY DOT(K,L,U)
      A=0.0
      DO 28 I=1,3
   28 A=A+P(I,K)*P(I,L)
      U=A
      RETURN
C
      ENTRY SMUL(K,L,U)
      H=U
      DO 30 I=1,3
   30 P(I,L)=H*P(I,K)
      RETURN
C
      ENTRY NORZ(K,L)
      A=0.0
      DO 32 I=1,3
   32 A=A+P(I,K)*P(I,K)
      B=DSQRT(A)
      IF(B.NE.0.0) B=1.0/B
      DO 34 I=1,3
   34 P(I,L)=B*P(I,K)
      RETURN
C
      ENTRY LENGTH(K,U)
      A=0.0
      DO 36 I=1,3
   36 A=A+P(I,K)*P(I,K)
      B=DSQRT(A)
      U=B
      RETURN
C
      ENTRY ANG(K,L,U,V)
      A=0.0
      B=0.0
      C=0.0
      DO 38 I=1,3
      A=A+P(I,K)*P(I,K)
      B=B+P(I,L)*P(I,L)
   38 C=C+P(I,K)*P(I,L)
      D=DSQRT(A*B)
      IF(D.NE.0.0) D=C/D
      IF (DABS(D).GT.1.D0) D=DSIGN(1.D0,D)
      U=D
      V=DARCOS(D)
      RETURN
C
      ENTRY PARPER(K,L,U,V)
C
C     NEW ALGORITHM USED, TO AVOID NUMERICAL PROBLEMS (AUGUST 81)
C
      A=0.0
      B=0.0
      C=0.0
      DO 40 I=1,3
      A=A+P(I,K)*P(I,K)
      B=B+P(I,L)*P(I,L)
   40 C=C+P(I,K)*P(I,L)
C     D=DSQRT(B)
C     IF(B.NE.0.0) A=A-C*C/B
C     IF(D.NE.0.0) C=C/D
C     U=C
C     V=DSQRT(A)
      D=DSQRT(B)
      G(1)=P(2,K)*P(3,L)-P(3,K)*P(2,L)
      G(2)=P(3,K)*P(1,L)-P(1,K)*P(3,L)
      G(3)=P(1,K)*P(2,L)-P(2,K)*P(1,L)
      E=DSQRT(G(1)*G(1)+G(2)*G(2)+G(3)*G(3))
      IF(D.NE.0.0) GOTO 41
      U=DSQRT(A)
      V=0.0
      RETURN
   41 U=C/D
      V=E/D
      RETURN
C
      ENTRY PUNIT(K,L)
      DO 42 I=1,3
   42 P(I,K)=0.0
      P(L,K)=1.0
      RETURN
C
      ENTRY DEFS(K,L,M)
      MX=M
      MY=M+1
      MZ=M+2
      DO 52 I=1,3
      F=P(I,K)
      H=P(I,L)
      P(I,MY)=F
   52 P(I,MZ)=H
      A=P(2,MY)*P(3,MZ)
      B=P(3,MY)*P(2,MZ)
      P(1,MX)=A-B
      A=P(3,MY)*P(1,MZ)
      B=P(1,MY)*P(3,MZ)
      P(2,MX)=A-B
      A=P(1,MY)*P(2,MZ)
      B=P(2,MY)*P(1,MZ)
      P(3,MX)=A-B
      A=P(2,MZ)*P(3,MX)
      B=P(3,MZ)*P(2,MX)
      P(1,MY)=A-B
      A=P(3,MZ)*P(1,MX)
      B=P(1,MZ)*P(3,MX)
      P(2,MY)=A-B
      A=P(1,MZ)*P(2,MX)
      B=P(2,MZ)*P(1,MX)
      P(3,MY)=A-B
      DO 58 J=MX,MZ
      A=0.0
      DO 54 I=1,3
   54 A=A+P(I,J)*P(I,J)
      B=DSQRT(A)
      IF(B.NE.0.0) B=1.0/B
      DO 56 I=1,3
   56 P(I,J)=B*P(I,J)
   58 CONTINUE
      RETURN
C
      ENTRY TRAC(K,L,M)
      N=L
      DO 62 J=1,3
      B=0.0
      DO 60 I=1,3
   60 B=B+P(I,N)*P(I,K)
      G(J)=B
   62 N=N+1
      DO 64 I=1,3
   64 P(I,M)=G(I)
      RETURN
C
      ENTRY TRAP(K,L,U,V,W)
      N=L
      A=0.0
      DO 68 J=1,3
      A=A+P(J,K)*P(J,K)
      B=0.0
      DO 66 I=1,3
   66 B=B+P(I,N)*P(I,K)
      G(J)=B
   68 N=N+1
      A=DSQRT(A)
      B=A
      IF(B.NE.0.0) B=1.0/B
      B=B*G(3)
      C=DSQRT(G(1)*G(1)+G(2)*G(2))
      IF(C.NE.0.0) C=1.0/C
      D=DARCOS(C*G(1))
      IF(G(2).LT.0.0) D=PI+PI-D
      U=A
      V=B
      W=D
      RETURN
C
      ENTRY PCOP(K,L,M)
      IF(M.GT.K.AND.M.LE.L) GOTO 83
      N=M
      DO 82 J=K,L
      DO 81 I=1,5
   81 PP(I,N)=PP(I,J)
   82 N=N+1
      RETURN
   83 N=M+L-K
      DO 85 JJ=K,L
      J=K+L-JJ
      DO 84 I=1,5
   84 PP(I,N)=PP(I,J)
   85 N=N-1
      RETURN
C
      ENTRY PEXC(K,L)
      DO 86 I=1,5
      A=PP(I,K)
      PP(I,K)=PP(I,L)
   86 PP(I,L)=A
      RETURN
C
      ENTRY PZER(K,L)
      DO 88 J=K,L
      DO 88 I=1,5
   88 PP(I,J)=0.0
      RETURN
C
      ENTRY PWRT(K,L,M)
      IF(NT.GE.M) RETURN
      NT=NT+1
      WRITE(6,103) K,L
      DO 96 J=K,L
      DO 90 I=1,5
      IF(PP(I,J).NE.0.0) GOTO 92
   90 CONTINUE
      GOTO 96
   92 IPP=IP(6,J)
      DO 94 I=1,5
      IPR(I)=MOD(IPP,64)
   94 IPP=IPP/64
      WRITE(6,102) J,(P(I,J),I=1,5),IPR,
     1 (P(I,J),I=7,9),P(9,J),P(10,J),P(10,J),J
      IF(MOD(J,10).NE.0) GOTO 96
      WRITE(6,101)
   96 CONTINUE
      WRITE(6,101)
  100 RETURN
  101 FORMAT(1X)
  102 FORMAT(1X,I5,7X,5F8.3,1X,5I3,1X,
     1 2F8.3,2(F8.3,I12),I5)
  103 FORMAT('0VECTORS',I5,' TO',I5/
     1 17X,'1',7X,'2',7X,'3',7X,'4',7X,'5',
     2 12X,'6',11X,'7',7X,'8',7X,'9',14X,'9',
     3 4X,'10',12X,'10')
      END
