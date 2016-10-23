C   07/06/96 606071908  MEMBER NAME  SMMED    (S4)          FORTG1
      SUBROUTINE SMMED(Y,N,ISTE)
      REAL Y(N),EP(3,2),ZI(3)
      DIAN3(A,B,C)=AMAX1(AMIN1(A,B),AMIN1(B,C),AMIN1(C,A))
C
C
C
C
C
C
C
C
C
C
      JSTE=ISTE
   10 LSTE=JSTE/10
      KSTE=JSTE-10*LSTE
      JSTE=LSTE
      IF(KSTE.LE.0.OR.KSTE.GT.6) GOTO 100
      GOTO (20,30,40,50,60,70),KSTE
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
      GOTO 10
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
   32 DO 35 N=1,3
      ZZ=Y(I+N)
      IF(ZZ.GT.Y(I+MIN)) GOTO 33
      ZI(N)=Y(I+MIN)
      MIN=N
      GOTO 35
   33 IF(ZZ.LT.Y(I+MAX)) GOTO 34
      ZI(N)=Y(I+MAX)
      MAX=N
      GOTO 35
   34 ZI(N)=ZZ
   35 CONTINUE
      ZM=DIAN3(ZI(1),ZI(2),ZI(3))
      Y(I)=ZL
      ZL=ZN
   36 ZN=ZM
      Y(N-3)=ZN
      Y(N-2)=ZM
      Y(N-1)=ZE
      GOTO 10
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
      GOTO 10
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
      GOTO 10
C
C     (5) = H
C     REPLACE EACH VALUE BY THE AVERAGE OVER 3 VALUES
C
   60 NM=N-2
      IF(NM.LT.1) GOTO 10
      ZL=Y(1)
      DO 62 I=1,NM
      ZN=0.25*Y(I)+0.5*Y(I+1)+0.25*Y(I+2)
      Y(I)=ZL
   62 ZL=ZN
      Y(N-1)=ZL
      GOTO 10
C
C     (6) = CH
C     REPLACE EACH VALUE BY THE AVERAGE OVER 5 VALUES,
C     IF QUADRATIC TERM NOT SIGNIFICANT
C
   70 NM=N-4
      IF(NM.LT.1) GOTO 10
      CUT=1.0
      ZL=Y(1)
      ZN=0.25*(Y(1)+2.0*Y(3)+Y(3))
      ZE=0.25*(Y(N)+2.0*Y(N-1)+Y(N-2))
      DO 74 I=1,NM
      ZM=Y(I+2)
      SUM=-2.0*Y(I)+Y(I+1)+2.0*Y(I+2)+Y(I+3)-2.0*Y(I+4)
      SUM=SUM*SUM/14.0
      IF(SUM.GT.CUT) GOTO 72
      SUM=Y(I)+2.0*Y(I+1)+3.0*Y(I+2)+2.0*Y(I+3)+Y(I+4)
      ZM=SUM/9.0
   72 Y(I)=ZL
      ZL=ZN
   74 ZN=ZM
      Y(N-3)=ZN
      Y(N-2)=ZM
      Y(N-1)=ZE
      GOTO 10
C
  100 RETURN
      END
