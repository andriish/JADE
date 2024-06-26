C   07/06/96 606071916  MEMBER NAME  UHIST    (S4)          FORTG1
      SUBROUTINE UHIST(NA,X)
      COMMON/BCS/IW(1)
      REAL RW(1)
      EQUIVALENCE (RW(1),IW(1))
      REAL FAC(5)/0.023,0.1587,0.50,0.8413,0.977/
*xxx01.02.98
      REAL AR(115)
*xxx
      CALL BLOC(IND,'HST*',NA,*10)
    2 N=IW(IND+2)
      IF(IW(IND+20).GE.2) GOTO 100
      IF(N.NE.0) GOTO 4
      IW(IND+1)=NA
      RW(IND+7)=X
      RW(IND+8)=X
      RW(IND+19)=X
    4 N=N+1
      IW(IND+2)=N
      RW(IND+7)=AMIN1(RW(IND+7),X)
      RW(IND+8)=AMAX1(RW(IND+8),X)
      XD=X-RW(IND+19)
      RW(IND+17)=RW(IND+17)+XD
      RW(IND+18)=RW(IND+18)+XD*XD
      IF(IW(IND+20).EQ.0) GOTO 6
      K=(X-RW(IND+3))/RW(IND+4)+21
      IF(K.LE.20) K=5
      IF(K.GT.120) K=6
      RW(IND+K)=RW(IND+K)+1.0
      GOTO 100
    6 RW(IND+20+N)=X
      IF(N.LT.100) GOTO 100
      CALL DEFST(IND)
      GOTO 100
   10 CALL BCRE(IND,'HST*',NA,120,*100,IER)
      GOTO 2
C
      ENTRY DHIST(NA,XL,XH)
      CALL BLOC(IND,'HST*',NA,*12)
      GOTO 100
   12 CALL BCRE(IND,'HST*',NA,120,*100,IER)
      IW(IND+1)=NA
      RW(IND+3)=AMIN1(XL,XH)
      ST=ABS(XL-XH)*0.01
      IF(ST.EQ.0.0) ST=.01
      RW(IND+4)=ST
      IW(IND+20)=1
      GOTO 100
C
      ENTRY QHIST(NA,AR)
*xxx01.02.97      REAL AR(115)
      DO 19 I=1,115
   19 AR(I)=0.0
      KND=0
      CALL BLOC(IND,'HST*',NA,*100)
      KND=IND
      CALL DEFST(IND)
      IF(IW(IND+20).GE.2) GOTO 40
C     MEAN AND SIGMA
   20 SN=IW(IND+2)
      IF(SN.EQ.0.0) GOTO 38
      IF(SN.LE.1.0) GOTO 21
      RW(IND+11)=(RW(IND+18)-RW(IND+17)**2/SN)/(SN-1.0)
      RW(IND+11)=SQRT(RW(IND+11))
   21 RW(IND+9)=RW(IND+19)+RW(IND+17)/SN
C     QUANTILES
      I=0
      J=0
      SUM=RW(IND+5)
   22 IF(J.EQ.5) GOTO 36
      J=J+1
      SJ=FAC(J)*SN
      IJ=0
   24 IF(SJ-SUM) 26,32,34
   26 IF(I.EQ.0) GOTO 22
      IF(IJ.EQ.0) GOTO 28
      R=0.5*FLOAT(IJ+I-1)
      GOTO 30
   28 R=FLOAT(I)+(SJ-SUM)/RW(IND+20+I)
   30 RW(IND+11+J)=RW(IND+3)+R*RW(IND+4)
      GOTO 22
   32 IF(IJ.EQ.0) IJ=I
   34 IF(I.EQ.100) GOTO 36
      I=I+1
      SUM=SUM+RW(IND+20+I)
      GOTO 24
   36 RW(IND+10)=RW(IND+14)
      RW(IND+14)=RW(IND+15)
      RW(IND+15)=RW(IND+12)
      RW(IND+12)=0.0
      IF(RW(IND+13)*RW(IND+14).NE.0.0) RW(IND+12)=0.5*(RW(IND+14)-
     1   RW(IND+13))
   38 CONTINUE
      IW(IND+20)=2
   40 DO 50 I=1,100
   50 AR(I)=RW(IND+20+I)
      DO 60 I=1,15
   60 AR(100+I)=RW(IND+1+I)
      AR(101)=IW(IND+2)
      AR(104)=IW(IND+5)
      AR(105)=IW(IND+6)
  100 RETURN
      END
