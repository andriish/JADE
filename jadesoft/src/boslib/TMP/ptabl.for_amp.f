C   07/06/96 606071903  MEMBER NAME  PTABL    (S4)          FORTG1
      SUBROUTINE PTABL(KNA)
      COMMON/BCS/IW(1)
      INTEGER NF(8),NY(16),N/0/
      INTEGER*2 NH(16),NCH(2)
      EQUIVALENCE (NF(1),NH(1)),(N,NCH(1))
      REAL*8 TEXT(16),TEXTS
      KAL=-1
      CALL BPOS('TAB*')
   10 CALL BNXT(IND,*90)
      NC=IW(IND-2)
      KA=NC/16
      IF(KNA.NE.0.AND.KNA.NE.KA) GOTO 10
      II=NC-KA*16
      II=II*16
      IF(KAL.EQ.KA) GOTO 20
      IF(KAL+1.EQ.0) GOTO 14
      NT=0
      DO 12 I=1,16
      CALL ITODA(NY(I),TEXT(I),6)
   12 NT=NT+NY(I)
      CALL ITODA(NT,TEXTS,8)
      WRITE(6,103) TEXT,TEXTS
   14 DO 18 I=1,16
      NY(I)=0
   18 CALL ITODA(I-1,TEXT(I),-6)
      WRITE(6,101) KA,TEXT
      KAL=KA
   20 DO 50 I=1,16
      DO 30 J=1,8
   30 NF(J)=IW(IND+8*(I-1)+J)
      NS=0
      DO 40 J=1,16
      NCH(2)=NH(J)
      NS=NS+N
      NY(J)=NY(J)+N
   40 CALL ITODA(N,TEXT(J),6)
      CALL ITODA(NS,TEXTS,8)
      IF(NS.EQ.0) GOTO 50
      IJ=II+I-1
      WRITE(6,102) IJ,TEXT,IJ,TEXTS
   50 CONTINUE
      GOTO 10
   90 IF(KAL+1.EQ.0) GOTO 100
      IF(KNA.NE.0.AND.KNA.NE.KA) GOTO 100
      NT=0
      DO 92 I=1,16
      CALL ITODA(NY(I),TEXT(I),6)
   92 NT=NT+NY(I)
      CALL ITODA(NT,TEXTS,8)
      WRITE(6,103) TEXT,TEXTS
  100 RETURN
  101 FORMAT('0',4('----')/' UTABL',I11//
     1                    8X,'J=',4(4A6,1X),13X,'SUM'/
     2   7X,'I',/7X,'= +',25('----'),'-+')
  102 FORMAT(1X,I7,' I',4(4A6,1X),' I',I4,2X,A8)
  103 FORMAT(9X,'+-',25('----'),'+'/5X,'SUM',2X,4(4A6,1X),8X,A8/)
      END
