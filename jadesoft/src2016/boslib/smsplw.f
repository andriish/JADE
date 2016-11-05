C   07/06/96 606071910  MEMBER NAME  SMSPLW   (S4)          FORTG1
      SUBROUTINE SPLSMW(Y,X,W,N,A,NSP,CHDF)
      REAL Y(N),X(N),W(N),A(5,NSP),S(4)
      CHDF=0.0
      IF(N.LT.3.OR.NSP.LT.4) GOTO 100
      DX=(X(N)-X(1))/FLOAT(NSP-3)
      DO 10 I=1,NSP
      DO 10 J=1,5
   10 A(J,I)=0.0
C
      DO 40 L=1,N
      XL=(X(L)-X(1))/DX
      IF(XL.LT.0.0) XL=0.0
      LX=XL
      LX=MIN0(LX,NSP-4)
      XL=FLOAT(LX+1)-XL
      DO 20 J=1,4
      IF(J/2.EQ.1) GOTO 15
      S(J)=XL*XL*XL
      GOTO 20
   15 S(J)=1.0+3.0*(1.0+XL*(1.0-XL))*XL
      IF(J.EQ.2) XL=1.0-XL
   20 CONTINUE
      DO 30 I=1,4
      A(5,LX+I)=A(5,LX+I)+Y(L)*S(I)*W(L)
      DO 30 J=1,I
      IJ=I+1-J
   30 A(IJ,LX+J)=A(IJ,LX+J)+S(I)*S(J)*W(L)
   40 CONTINUE
C
      CALL BANDSL(A,NSP,5,IER)
      IF(IER.NE.0) GOTO 100
C
      DO 60 L=1,N
      XL=(X(L)-X(1))/DX
      IF(XL.LT.0.0) XL=0.0
      LX=XL
      LX=MIN0(LX,NSP-4)
      XL=FLOAT(LX+1)-XL
      SUM=0.0
      DO 50 J=1,4
      IF(J/2.EQ.1) GOTO 45
      SX=XL*XL*XL
      GOTO 50
   45 SX=1.0+3.0*(1.0+XL*(1.0-XL))*XL
      IF(J.EQ.2) XL=1.0-XL
   50 SUM=SUM+SX*A(5,LX+J)
      CHDF=CHDF+(SUM-Y(L))*W(L)
      Y(L)=SUM
   60 CONTINUE
      CHDF=CHDF/FLOAT(N+2-NSP)
  100 RETURN
      END
