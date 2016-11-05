*** PMF 19/10/99
*     Since the original subroutine SYMIN4 was not found
*     in the JADE library, try SYMINV which has probably
*     the same functionality as SYMIN4 (Jan Olsson's suggestion!)
      SUBROUTINE SYMIN4(A,L,M,N,IFAIL)
      IMPLICIT REAL*8(A-H,O-Z)
      DIMENSION A(L,M)
C
      CALL SYMINV(A,L,M,N,IFAIL)
C
      RETURN
      END
*** PMF (end)

C            C7011402   MEMBER NAME  SYMINV   (MINSOR)      FORTRAN
      SUBROUTINE SYMINV(A,L,M,N,IFAIL)
C
C
      IMPLICIT REAL*8(A-H,O-Z)
      DIMENSION P(100),Q(100),KR(100),A(L,M)
C
      IFAIL=0
C
      EPSILN=1.0 D-12
      IF(L.LT.N) GO TO 95
      IF(M.LT.N) GO TO 96
C
C                   CONSTRUCT TRUTH TABLE
C
      DO 10 I=1,N
   10 KR(I)=1
C
C                   BEGIN PROGRAMME
C
      DO 65 I=1,N
      K=0
C
C                   SEARCH FOR PIVOT
C
      BIG=0.D0
      DO 37 J=1,N
      TEST=DABS(A(J,J))
      IF(TEST-BIG)37,37,31
   31 IF(KR(J))100,37,32
   32 BIG=TEST
      K=J
   37 CONTINUE
C
C                   TEST FOR ZERO MATRIX
C
      IF(K)100,100,38
C
C                   TEST FOR LINEARITY
C
   38 IF(I.EQ.1) PIVOT1=A(K,K)
      IF(DABS(A(K,K)/PIVOT1)-EPSILN) 100,39,39
C
C                   PREPARATION FOR ELIMINATION STEP1
C
   39 KR(K)=0
      Q(K)=1.D0/A(K,K)
      P(K)=1.D0
      A(K,K)=0.0D0
      KP1=K+1
      KM1=K-1
      IF(KM1)100,50,40
   40 DO 49 J=1,KM1
      P(J)=A(J,K)
      Q(J)= A(J,K)*Q(K)
      IF(KR(J))100,49,42
   42 Q(J)=-Q(J)
   49 A(J,K)=0.D0
   50 IF(K-N)51,60,100
   51 DO 59 J=KP1,N
      P(J)=A(K,J)
      Q(J)=-A(K,J)*Q(K)
      IF(KR(J))100,52,59
   52 P(J)=-P(J)
   59 A(K,J)=0.0D0
C
C                   ELIMINATION PROPER
C
   60 DO 65 J=1,N
      DO 65 K=J,N
   65 A(J,K)= A(J,K)+P(J)*Q(K)
C
C                   ELEMENTS OF LEFT DIAGONAL
C
      DO 70 J=2,N
      JM1=J-1
      DO 70 K=1,JM1
   70 A(J,K)=A(K,J)
      RETURN
C
C                   FAILURE RETURN
C
   95 WRITE(6,150) L,N
      GO TO 100
   96 WRITE(6,151) M,N
  100 IFAIL=1
      RETURN
  150 FORMAT(4H1L =,I5,4H N =,I5,33H L SHOULD BE LARGER OR EQUAL TO N)
  151 FORMAT(4H1M =,I5,4H N =,I5,33H M SHOULD BE LARGER OR EQUAL TO N)
       END
C            C7011402   MEMBER NAME  SYMIN2   (MINSOR)      FORTRAN
      SUBROUTINE SYMIN2(A,L,M,N,IFAIL)
C
C          DIES IST DIE 2. VERSION VON SYMINV
C     (scheint aber identisch zu sein mit der 1. Version, PMF 19/10/99)
C
      IMPLICIT REAL*8(A-H,O-Z)
      DIMENSION P(100),Q(100),KR(100),A(L,M)
C
      IFAIL=0
C
      EPSILN=1.0 D-12
      IF(L.LT.N) GO TO 95
      IF(M.LT.N) GO TO 96
C
C                   CONSTRUCT TRUTH TABLE
C
      DO 10 I=1,N
   10 KR(I)=1
C
C                   BEGIN PROGRAMME
C
      DO 65 I=1,N
      K=0
C
C                   SEARCH FOR PIVOT
C
      BIG=0.D0
      DO 37 J=1,N
      TEST=DABS(A(J,J))
      IF(TEST-BIG)37,37,31
   31 IF(KR(J))100,37,32
   32 BIG=TEST
      K=J
   37 CONTINUE
C
C                   TEST FOR ZERO MATRIX
C
      IF(K)100,100,38
C
C                   TEST FOR LINEARITY
C
   38 IF(I.EQ.1) PIVOT1=A(K,K)
      IF(DABS(A(K,K)/PIVOT1)-EPSILN) 100,39,39
C
C                   PREPARATION FOR ELIMINATION STEP1
C
   39 KR(K)=0
      Q(K)=1.D0/A(K,K)
      P(K)=1.D0
      A(K,K)=0.0D0
      KP1=K+1
      KM1=K-1
      IF(KM1)100,50,40
   40 DO 49 J=1,KM1
      P(J)=A(J,K)
      Q(J)= A(J,K)*Q(K)
      IF(KR(J))100,49,42
   42 Q(J)=-Q(J)
   49 A(J,K)=0.D0
   50 IF(K-N)51,60,100
   51 DO 59 J=KP1,N
      P(J)=A(K,J)
      Q(J)=-A(K,J)*Q(K)
      IF(KR(J))100,52,59
   52 P(J)=-P(J)
   59 A(K,J)=0.0D0
C
C                   ELIMINATION PROPER
C
   60 DO 65 J=1,N
      DO 65 K=J,N
   65 A(J,K)= A(J,K)+P(J)*Q(K)
C
C                   ELEMENTS OF LEFT DIAGONAL
C
      DO 70 J=2,N
      JM1=J-1
      DO 70 K=1,JM1
   70 A(J,K)=A(K,J)
      RETURN
C
C                   FAILURE RETURN
C
   95 WRITE(6,150) L,N
      GO TO 100
   96 WRITE(6,151) M,N
  100 IFAIL=1
      RETURN
  150 FORMAT(4H1L =,I5,4H N =,I5,33H L SHOULD BE LARGER OR EQUAL TO N)
  151 FORMAT(4H1M =,I5,4H N =,I5,33H M SHOULD BE LARGER OR EQUAL TO N)
       END

