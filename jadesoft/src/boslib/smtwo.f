C   07/06/96 606071910  MEMBER NAME  SMTWO    (S4)          FORTG1
      SUBROUTINE SMTWO(Y,Z,N,IP)
      REAL Y(N),Z(N),H(320)
C                -   - --
C     CALL SMTWO(Y,Z,N,IP)
C                - -
C
C     SMOOTH N DATA Y( ) BY TWOFOLD APPLICATION
C     OF SIMPLE SM353 METHOD, QUADRATIC INTERPOLATION
C     OF FLAT AREAS AND AVERAGING. RESULT IN Z( ).
C     Y( ) AND Z( ) MAY NOT BE THE SAME ARRAYS, CONTENT
C     OF Y( ) IS DESTROYED.
C     IP = 0  FOR POISSON DISTRIBUTED (COUNTER) DATA
C        = NOT EQUAL 0 OTHERWISE
C
      IF(IP.EQ.0) CALL PTOEQ(Y,N)
      DO 10 I=1,N
   10 Z(I)=Y(I)
      DO 60 K=1,2
      CALL SMONE(Z,Z,N)
      CALL QANN(Z,N)
      CALL HANN(Z,N)
      IF(IP.EQ.0) CALL CANN(Z,N)
C     CALL CUBN(Z,N)
      GOTO (20,40),K
C  20 CALL HANN(Z,N)
C     CALL CUBN(Z,N)
   20 NSP=3+0.2*FLOAT(N)
      NSP=MIN0(64,NSP)
      CALL SPLSME(Z,N,H,NSP,CHDF)
      DO 30 I=1,N
      ZZ=Z(I)
      Z(I)=Y(I)-ZZ
   30 Y(I)=ZZ
      GOTO 60
   40 DO 50 I=1,N
      Z(I)=Y(I)+Z(I)
   50 Y(I)=Z(I)
   60 CONTINUE
      NSP=3+0.2*FLOAT(N)
      NSP=MIN0(64,NSP)
      CALL SPLSME(Y,N,H,NSP,CHDF)
      NSP=4+(N-3)/2
      IF(NSP.GT.64) GOTO 70
      CALL SPLSME(Z,N,H,NSP,CHDF)
      GOTO 90
   70 KL=(N+101)/112
      LK=(N+12*KL-11)/KL
      JA=1
      DO 80 LL=1,KL
      JB=MIN0(N,JA+LK-1)
      NSP=4+(JB-JA+1)/2
      NSP=MIN0(64,NSP)
      CALL SPLSME(Y(JA),JB-JA+1,H,NSP,CHDF)
   80 JA=JB-10
C     CALL CUBN(Y,N)
   90 IF(IP.EQ.0) CALL EQTOP(Y,N)
      IF(IP.EQ.0) CALL EQTOP(Z,N)
  100 RETURN
      END
      SUBROUTINE SMONE(Y,Z,N)
      REAL Y(N),Z(N)
C                -   -
C     CALL SMONE(Y,Z,N)
C                  -
C
C     SMOOTH N DATA Y( ) BY SIMPLE SM353 METHOD
C     WITHOUT QUADRATIC INTERPOLATION AND AVERAGING.
C     RESULT IN Z( ). Y( ) AND Z( ) MAY BE THE SAME ARRAY.
C
      DO 10 I=1,N
   10 Z(I)=Y(I)
      IF(N.LE.3) RETURN
      CALL SM3 (Z,N)
      IF(N.LE.5) RETURN
      CALL SM5 (Z,N)
      CALL SM3C(Z,N)
  100 RETURN
      END
      FUNCTION DIAN5(Z)
      REAL Z(5),ZI(3)
      DIAN3(A,B,C)=AMAX1(AMIN1(A,B),AMIN1(B,C),AMIN1(C,A))
C
C     MEDIAN OF 5 VALUES
C
      IF(Z(1).GT.Z(5)) GOTO 10
      MIN=1
      MAX=5
      GOTO 20
   10 MIN=5
      MAX=1
   20 DO 50 N=1,3
      ZZ=Z(N+1)
      IF(ZZ.GT.Z(MIN)) GOTO 30
      ZI(N)=Z(MIN)
      MIN=N+1
      GOTO 50
   30 IF(ZZ.LT.Z(MAX)) GOTO 40
      ZI(N)=Z(MAX)
      MAX=N+1
      GOTO 50
   40 ZI(N)=ZZ
   50 CONTINUE
      DIAN5=DIAN3(ZI(1),ZI(2),ZI(3))
      RETURN
      END
      SUBROUTINE SM3(Y,N)
      REAL Y(N),EP(3,2)
      DIAN3(A,B,C)=AMAX1(AMIN1(A,B),AMIN1(B,C),AMIN1(C,A))
C
C     REPLACE EACH VALUE BY MEDIAN OF 3 VALUES
C
      NM=N-2
      IF(NM.LT.1) GOTO 100
      EP(1,1)=Y(1)
      EP(1,2)=Y(N)
      ZL=Y(1)
      DO 10 I=1,NM
      ZN=DIAN3(Y(I),Y(I+1),Y(I+2))
      Y(I)=ZL
   10 ZL=ZN
      Y(N-1)=ZL
      EP(2,1)=Y(2)
      EP(2,2)=Y(N-1)
      EP(3,1)=3.0*Y(2)-2.0*Y(3)
      EP(3,2)=3.0*Y(N-1)-2.0*Y(N-2)
      Y(1)=DIAN3(EP(1,1),EP(2,1),EP(3,1))
      Y(N)=DIAN3(EP(1,2),EP(2,2),EP(3,2))
  100 RETURN
      END
      SUBROUTINE SM3C(Y,N)
      REAL Y(N)
      DIAN3(A,B,C)=AMAX1(AMIN1(A,B),AMIN1(B,C),AMIN1(C,A))
C
C     REPLACE EACH VALUE BY MEDIAN OF 3 VALUES, COPYING FIRST AND LAST
C
      NM=N-2
      IF(NM.LT.1) GOTO 100
      ZL=Y(1)
      DO 10 I=1,NM
      ZN=DIAN3(Y(I),Y(I+1),Y(I+2))
      Y(I)=ZL
   10 ZL=ZN
      Y(N-1)=ZL
  100 RETURN
      END
      SUBROUTINE SM5(Y,N)
      REAL Y(N)
      DIAN3(A,B,C)=AMAX1(AMIN1(A,B),AMIN1(B,C),AMIN1(C,A))
C
C     REPLACE EACH VALUE BY MEDIAN OF 5 VALUES
C
      NM=N-4
      IF(NM.LT.1) GOTO 100
      ZL=Y(1)
      ZN=DIAN3(Y(1),Y(2),Y(3))
      ZE=DIAN3(Y(N-2),Y(N-1),Y(N))
      DO 10 I=1,NM
      ZM=DIAN5(Y(I))
      Y(I)=ZL
      ZL=ZN
   10 ZN=ZM
      Y(N-3)=ZN
      Y(N-2)=ZM
      Y(N-1)=ZE
  100 RETURN
      END
      SUBROUTINE HANN(Y,N)
      REAL Y(N)
C
C     REPLACE EACH VALUE BY THE AVERAGE OVER 3 VALUES
C
      IF(N.LT.3) GOTO 100
      ZL=Y(1)
      NM=N-2
      DO 10 I=1,NM
      ZN=0.25*Y(I)+0.5*Y(I+1)+0.25*Y(I+2)
      Y(I)=ZL
   10 ZL=ZN
      Y(N-1)=ZL
  100 RETURN
      END
      SUBROUTINE QANN(Y,N)
      REAL Y(N)
C
C     CORRECT FLAT AREAS OF THREE EQUAL VALUES BY QUADRATIC INTERPOL.
C
      IA=3
      IB=N-2
      IF(IB.LT.IA) GOTO 100
      DO 10 I=IA,IB
      IF(Y(I).NE.Y(I-1).OR.Y(I).NE.Y(I+1)) GOTO 10
      A=Y(I)-Y(I-2)
      B=Y(I)-Y(I+2)
      IF(A*B.LE.0) GOTO 10
      J=1
      IF(ABS(A).GT.ABS(B)) J=-1
      Y(I-J)=Y(I+J)-0.5*(Y(I+J+J)-Y(I-J-J))
      Y(I)=(8.0*Y(I+J)-3.0*Y(I+J+J)+Y(I-J-J))/6.0
   10 CONTINUE
  100 RETURN
      END
      SUBROUTINE CANN(Y,N)
      REAL Y(N)
C
C     REPLACE EACH VALUE BY THE AVERAGE OVER 5 VALUES,
C     IF QUADRATIC TERM NOT SIGNIFICANT
C
      NM=N-4
      IF(NM.LT.1) GOTO 100
      CUT=1.0
      ZL=Y(1)
      ZN=0.25*(Y(1)+2.0*Y(3)+Y(3))
      ZE=0.25*(Y(N)+2.0*Y(N-1)+Y(N-2))
      DO 20 I=1,NM
      ZM=Y(I+2)
      SUM=-2.0*Y(I)+Y(I+1)+2.0*Y(I+2)+Y(I+3)-2.0*Y(I+4)
      SUM=SUM*SUM/14.0
      IF(SUM.GT.CUT) GOTO 10
      SUM=Y(I)+2.0*Y(I+1)+3.0*Y(I+2)+2.0*Y(I+3)+Y(I+4)
      ZM=SUM/9.0
   10 Y(I)=ZL
      ZL=ZN
   20 ZN=ZM
      Y(N-3)=ZN
      Y(N-2)=ZM
      Y(N-1)=ZE
  100 RETURN
      END
      SUBROUTINE CUBN(Y,N)
      REAL Y(N)
C
C     CORRECT EACH MIDDLE VALUE OF 5 VALUES,
C     TO OBTAIN SMOOTH CURVE
C
      NM=N-4
      IF(NM.LT.1) GOTO 100
      ZL=0.0
      ZU=0.0
      DO 10 I=1,NM
      ZM=Y(I+2)
      SUM=-Y(I)+4.0*Y(I+1)-6.0*Y(I+2)+4.0*Y(I+3)-Y(I+4)
      ZM=0.06*SUM
      IF(I.EQ.1) ZN=-0.08*SUM
      IF(I.EQ.NM) ZE=-0.08*SUM
      Y(I)=Y(I)+ZU
      ZU=(ZN-ZL-ZM)/3.0
      ZL=ZN
   10 ZN=ZM
C     Y(N-3)=Y(N-3)+ZL
C     Y(N-2)=Y(N-2)+ZN
C     Y(N-1)=Y(N-1)+ZE
      Y(N-3)=Y(N-3)+ZU
      Y(N-2)=Y(N-2)+(ZN-ZL-ZE)/3.0
      Y(N-1)=Y(N-1)+(ZE-ZN)/3.0
  100 RETURN
      END
      SUBROUTINE PTOEQ(Y,N)
      REAL Y(N)
C
C     TRANSFORM POISSON DISTRIBUTED DATA TO DATA OF VARIANCE 1.0
C
      IA=0
      IB=0
      DO 10 I=1,N
      IF(Y(I).EQ.0.0) GOTO 10
      IF(IA.EQ.0) IA=I
      IB=I
   10 CONTINUE
      IF(IB.EQ.0) GOTO 100
      DO 30 I=IA,IB
      IF(Y(I).GT.0.0) GOTO 20
      Y(I)=1.0
      GOTO 30
   20 Y(I)=SQRT(Y(I))+SQRT(Y(I)+1.0)
   30 CONTINUE
  100 RETURN
      END
      SUBROUTINE EQTOP(Y,N)
      REAL Y(N)
C
C     TRANSFORM BACK TO POISSON DISTRIBUTED DATA
C
      DO 10 I=1,N
      IF(Y(I).GT.1.0) GOTO 5
      Y(I)=0.0
      GOTO 10
    5 SY=(0.5*(Y(I)-1.0/Y(I)))**2
      NY=10.0*SY+0.5
      Y(I)=0.1*FLOAT(NY)
   10 CONTINUE
  100 RETURN
      END
