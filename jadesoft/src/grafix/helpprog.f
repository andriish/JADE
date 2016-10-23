C   20/06/84 507190129  MEMBER NAME  HELPPROG (S)           FORTRAN
C
C-----------------------------------------------------------------------
C
C     INSTRUCTIONS:
C
C     1)  COPY MEMBER  HELPMOD  TO A SMALL SEQUENTIAL DATASET
C     2)  USE  ALLOC OLD TO GIVE THE SEQUENTIAL DS A DDNAME FT01F001
C     3)  USE  ALLOC OLD TO GIVE F22BOW.HELP.DATASET A DDNAME OF
C         FT10F001
C     4)  RUN  LOADGO  USING THIS PROGRAM (HELPPROG)
C     5)  THE DIRECT ACCESS DATASET IS NOW READY
C
C-----------------------------------------------------------------------
C
      IMPLICIT INTEGER*2 (H)
C
      LOGICAL*1  CHARS(80)
C
      EQUIVALENCE  (CHARS(1),IN(1))
C
      DIMENSION  IAR(240), IN(20)
      DIMENSION  ISTAFL(20), IENDFL(20)
C
      DATA  IEND / '----' /
C
      DEFINE FILE 10 ( 200, 1024, E, I1 )
C
      CALL DEFCMD
C
C                            SKIP THE FIRST LINE
C
      READ(1,1,END=99) IN
C
      DO  2  I = 1,200
        N = 0
        READ(1,1,END=99) IN
  1     FORMAT(20A4)
C
        CALL LOCFLD( CHARS, NFIELD, ISTAFL, IENDFL )
        LEN = IENDFL(2) - ISTAFL(2) + 1
        CALL FINABR( CHARS(ISTAFL(2)), LEN, IREC, MAXARG, IS )
        N = 1
        DO  5  J = 1,20
          IAR(J) = IN(J)
  5     CONTINUE
C
        DO  20  LL = 1,15
          READ(1,1) IN
          IF( IN(1) .EQ. IEND ) GO TO 6
          DO  7  J = 1,20
            K = N*20 + J
            IAR(K) = IN(J)
  7       CONTINUE
          N = N + 1
  20   CONTINUE
C
  6     L = N * 20
***PMF        WRITE(10'IREC,3) L,(IAR(K),K=1,L)
***PMF 07/05/99 ( This version has still to be tested!!!)
        WRITE(10,REC=IREC,FMT=3) L,(IAR(K),K=1,L)
***PMF(End)
  3     FORMAT(I4,240A4)
        WRITE(6,9) L,(IAR(K),K=1,L)
  9     FORMAT(X,I4/15(X,20A4/))
C
  2   CONTINUE
C
 99   STOP
      END
