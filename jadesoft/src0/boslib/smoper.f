C   07/06/96 606071909  MEMBER NAME  SMOPER   (S4)          FORTG1
      SUBROUTINE SMOPER(Y,N,IOP)
      REAL Y(N),EP(3,2),ZI(3)
      DIAN3(A,B,C)=AMAX1(AMIN1(A,B),AMIN1(B,C),AMIN1(C,A))
C
C     PERFORM SMOOTHING OPERATIONS ON ARRAY Y( ) OF DIMENSION N.
C     THE FOLLOWING OPERATIONS ARE POSSIBLE.
C
C     OP-CODE          OPERATION
C       1      MED3    REPLACE EACH VALUE BY MEDIAN OF THREE VALUES
C       2      MED5    REPLACE EACH VALUE BY MEDIAN OF FIVE  VALUES
C       3      MED3C   REPLACE EACH VALUE BY MEDIAN OF THREE VALUES,
C                      EXCEPT FIRST AND LAST VALUE
C       4         Q    CORRECT FLAT AREAS OF LENGTH THREE BY
C                      QUADRATIC INTERPOLATION
C       5         F    REPL.EACH VALUE BY AVERAGE OF THREE VALUES (FOLD)
C       6         U    INVERSE OPERATION TO F (UNFOLD)
C       7         MQ   REPL. EACH VALUE BY AVERAGE OF VALUE AND
C                      QUADRATIC FIT VALUE
C       8 = 321        353C COMBINED OPERATION
C       9 = 54321      353CQF COMBINED OPERATION
C
C
C     THE ORDER OF OPERATIONS IS CHOSEN BY ONE INTEGER IOP, CONTAINING
C     THE OP-CODES FROM RIGHT TO LEFT, E.G. 54321 MEANS
C     MED3 FOLLOWED BY MED5, MED3C, Q AND F.
C
C     USAGE          - - ---
C        CALL SMOPER(Y,N,IOP)
C                    -
C     THE VALUE IOP = 54321= 9 IS RECOMMENDED.
C
      JSTE=IOP
   10 LSTE=JSTE/10
      KSTE=JSTE-10*LSTE
      JSTE=LSTE
      IF(KSTE.LE.0.OR.KSTE.GT.9) GOTO 100
      GOTO (20,30,40,50,60,70,80,20,20),KSTE
C
C
C     (1) = MED3
C     REPLACE EACH VALUE BY MEDIAN OF 3 VALUES
C
   20 NM=N-2
      IF(NM.LT.1) GOTO 10
      EP(1,1)=Y(1)
      EP(1,2)=Y(N)
      ZL=Y(1)
      DO 22 I=1,NM
      ZN=DIAN3(Y(I),Y(I+1),Y(I+2))
      Y(I)=ZL
   22 ZL=ZN
      Y(N-1)=ZL
      EP(2,1)=Y(2)
      EP(2,2)=Y(N-1)
      EP(3,1)=3.0*Y(2)-2.0*Y(3)
      EP(3,2)=3.0*Y(N-1)-2.0*Y(N-2)
      Y(1)=DIAN3(EP(1,1),EP(2,1),EP(3,1))
      Y(N)=DIAN3(EP(1,2),EP(2,2),EP(3,2))
      IF(KSTE.EQ.1) GOTO 10
C
C     (2) = MED5
C     REPLACE EACH VALUE BY MEDIAN OF 5 VALUES
C
   30 NM=N-4
      IF(NM.LT.1) GOTO 10
      ZL=Y(1)
      ZN=DIAN3(Y(1),Y(2),Y(3))
      ZE=DIAN3(Y(N-2),Y(N-1),Y(N))
      DO 36 I=1,NM
C
C     MEDIAN OF 5 VALUES
C
      IF(Y(I).GT.Y(I+4)) GOTO 31
      MIN=0
      MAX=4
      GOTO 32
   31 MIN=4
      MAX=0
   32 DO 35 M=1,3
      ZZ=Y(I+M)
      IF(ZZ.GT.Y(I+MIN)) GOTO 33
      ZI(M)=Y(I+MIN)
      MIN=M
      GOTO 35
   33 IF(ZZ.LT.Y(I+MAX)) GOTO 34
      ZI(M)=Y(I+MAX)
      MAX=M
      GOTO 35
   34 ZI(M)=ZZ
   35 CONTINUE
      ZM=DIAN3(ZI(1),ZI(2),ZI(3))
      Y(I)=ZL
      ZL=ZN
   36 ZN=ZM
      Y(N-3)=ZL
      Y(N-2)=ZN
      Y(N-1)=ZE
      IF(KSTE.EQ.2) GOTO 10
C
C     (3) = MED3C
C     REPLACE EACH VALUE BY MEDIAN OF 3 VALUES, COPYING FIRST AND LAST
C
   40 NM=N-2
      IF(NM.LT.1) GOTO 10
      ZL=Y(1)
      DO 42 I=1,NM
      ZN=DIAN3(Y(I),Y(I+1),Y(I+2))
      Y(I)=ZL
   42 ZL=ZN
      Y(N-1)=ZL
      IF(KSTE.NE.9) GOTO 10
C
C     (4) = Q
C     CORRECT FLAT AREAS OF THREE EQUAL VALUES BY QUADRATIC INTERPOL.
C
   50 IA=3
      IB=N-2
      IF(IB.LT.IA) GOTO 10
      DO 52 I=IA,IB
      IF(Y(I).NE.Y(I-1).OR.Y(I).NE.Y(I+1)) GOTO 52
      A=Y(I)-Y(I-2)
      B=Y(I)-Y(I+2)
      IF(A*B.LE.0) GOTO 52
      J=1
      IF(ABS(A).GT.ABS(B)) J=-1
      Y(I-J)=Y(I+J)-0.5*(Y(I+J+J)-Y(I-J-J))
      Y(I)=(8.0*Y(I+J)-3.0*Y(I+J+J)+Y(I-J-J))/6.0
   52 CONTINUE
      IF(KSTE.NE.9) GOTO 10
C
C     (5) = FOLD
C     REPLACE EACH VALUE BY THE AVERAGE OVER 3 VALUES
C
   60 NM=N-2
      IF(NM.LT.1) GOTO 10
      ZL=0.25*(3*Y(1)+Y(2))
      DO 62 I=1,NM
      ZN=0.25*(Y(I)+Y(I+1)+Y(I+1)+Y(I+2))
      Y(I)=ZL
   62 ZL=ZN
      Y(N)=0.25*(3.0*Y(N)+Y(N-1))
      Y(N-1)=ZL
      GOTO 10
C
C     (6) = UNFOLD
C     UNFOLD IS INVERSE OPERATION TO FOLD
C
   70 NM=N-1
      A=1.0
      DO 72 I=1,NM
      B=A+2.0
      Y(I+1)=Y(I+1)-Y(I)*A/B
   72 A=B
      Y(N)=0.5*Y(N)/(1.0+1.0/A)
      DO 74 II=1,NM
      I=N-II
      B=A-2.0
      Y(I)=(Y(I)-Y(I+1))*B/A
      Y(I+1)=4.0*Y(I+1)
   74 A=B
      Y(1)=4.0*Y(1)
      GOTO 10
C
C     (7) = MQ
C     REPLACE EACH VALUE BY AVERAGE OF VALUE AND QUADRATIC FIT VALUE
C
   80 NM=N-4
      IF(NM.LT.1) GOTO 10
      ZL=Y(1)
      ZN=(12.0*Y(1)+17.0*Y(2)+12.0*Y(3)-3.0*Y(4))/38.0
      ZE=(12.0*Y(N)+17.0*Y(N-1)+12.0*Y(N-2)-3.0*Y(N-3))/38.0
      DO 82 I=1,NM
      ZM=(-3.0*Y(I)+12.0*Y(I+1)+17.0*Y(I+2)+12.0*Y(I+3)-3.0*Y(I+4))/35.000017300
      Y(I)=ZL
      ZL=ZN
   82 ZN=ZM
      Y(N-3)=ZL
      Y(N-2)=ZN
      Y(N-1)=ZE
      GOTO 10
C
  100 RETURN
      END
