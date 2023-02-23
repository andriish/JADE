C   07/06/96 606071812  MEMBER NAME  BFMT     (S4)          FORTRAN
      SUBROUTINE BFMT(X,N,XF,EF)
      CHARACTER*1CH(12)/1H0,1H1,1H2,1H3,1H4,1H5,1H6,1H7,1H8,1H9,1H.,1H-/
      REAL*8 XF(N),FXF,SXF,BLANK/8H        /,NULL/8H00000000/
      LOGICAL*1 FXL(8),SXL(8)
      EQUIVALENCE (FXL(1),FXF),(SXL(1),SXF)
      REAL*4 X(N)
      INTEGER BLK/4H    /
      INTEGER EF

      IP=0
      XM=0.0
      DO 10 I=1,N
   10 XM=AMAX1(XM,ABS(X(I)))
      JP=5
      FC=1.0
      IF(XM.EQ.0.0) GOTO 15
      JP=104-IFIX(ALOG10(ABS(XM))+100.04)
      FC=10.0**JP
   15 IM=6
      DO 30 I=1,N
      FXF=NULL
      IJ=FC*ABS(X(I))+0.5
      JM=6
      IF(IJ.EQ.0) GOTO 30
      DO 20 J=1,5
      JH=IJ/10
      JN=IJ-10*JH
      IF(JN.NE.0.AND.JM.EQ.6) JM=J
      FXL(J)=CH(JN+1)
   20 IJ=JH
      IM=MIN0(IM,JM)
   30 XF(I)=FXF
      JM=IM
   32 IF(6-JP.LE.5) GOTO 34
      JP=JP+3
      IP=IP+3
      GOTO 32
   34 IF(JP+1-JM.LE.5) GOTO 38
   36 JP=JP-3
      IP=IP-3
      GOTO 34
   38 IF(JP.GE.8) GOTO 36
      JA=MIN0(JM,JP)
      JB=MAX0(6,JP+1)
      DO 90 I=1,N
      FXF=XF(I)
      SXF=BLANK
      IB=7+(JB-JA)/2
      DO 80 J=JA,JB
      IF(FXL(J).NE.CH(1)) GOTO 70
      IF(J.GT.JP+1) GOTO 50
      IF(FXF.NE.NULL) GOTO 70
      IF(J.GE.JP) GOTO 70
      IB=IB-1
      GOTO 80
   50 DO 60 K=J,JB
      IF(FXL(K).NE.CH(1)) GOTO 70
   60 CONTINUE
      GOTO 80
   70 IB=IB-1
      SXL(IB)=FXL(J)
      IF(J.NE.JP) GOTO 80
      IB=IB-1
      SXL(IB)=CH(11)
   80 CONTINUE
      IF(X(I).LT.0.0) SXL(IB-1)=CH(12)
   90 XF(I)=SXF
      EF=BLK
      IF(IP.NE.0) EF=IAF(IP,'E   ')
  100 RETURN
      END
