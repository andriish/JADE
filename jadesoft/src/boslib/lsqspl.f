C   07/06/96 606071855  MEMBER NAME  LSQSPL   (S4)          FORTG1
      SUBROUTINE LSQSPL(Y,N,A,NSP,CHDF)
      REAL Y(N),A(5,NSP),S(4)
C
C     PERFORM A LEAST SQUARE SPLINE FIT TO THE ARRAY Y( ) OF
C     DIMENSION N WITH NSP SPLINE REFERENCE POINTS. THE ARRAY Y( ) IS
C     REPLACED BY TH FITTED VALUES OF THE INTERPOLATING SPLINE
C     FUNCTION. THE NSP REFERENCE POINTS ARE CHOSEN EQUIDISTANT,
C     WTIH THE FIRST AND THE LAST ONE OUTSIDE THE X-RANGE.
C
C     LOWEST VALUE OF NSP  = 4
C                            ONE CUBIC POLYNOMIAL THROUGH ALL POINTS
C
C     LARGEST VALUE OF NSP = 2+(N+1)/2
C                            CUBIC POLYNOMIAL FOR EACH TWO PONTS
C
C     H( ) = AUXILIARY ARRAY OF LENGTH 5*NSP (AT LEAST).
C
C     IEQ = +1 OR -1   Y-VALUES AT EQUIDISTANT ABSCISSA VALUES X
C                      (ARGUMENT X CAN BE DUMMY)
C     IEQ = +2 OR -2   Y-VALUES AT GIVEN X-VALUES
C
C     IEQ = +1 OR +2   WEIGTHS W FOR ALL Y-VALUES GIVEN
C
C     IEQ = -1 OR -2   ALL WEIGTHS EQUAL (ASSUMED TO BE 1.0)
C                      (ARGUMENT W CAN BE DUMMY)
      CHDF=0.0
      IF(N.LT.3.OR.NSP.LT.4) GOTO 100
      DX=FLOAT(N-1)/FLOAT(NSP-3)
      DO 10 I=1,NSP
      DO 10 J=1,5
   10 A(J,I)=0.0
C
      DO 40 L=1,N
      XL=FLOAT(L-1)/DX
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
      A(5,LX+I)=A(5,LX+I)+Y(L)*S(I)
      DO 30 J=1,I
      IJ=I+1-J
   30 A(IJ,LX+J)=A(IJ,LX+J)+S(I)*S(J)
   40 CONTINUE
C
      CALL BANDSL(A,NSP,5,IER)
      IF(IER.NE.0) GOTO 100
C
      DO 60 L=1,N
      XL=FLOAT(L-1)/DX
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
      CHDF=CHDF+(SUM-Y(L))**2
      Y(L)=SUM
   60 CONTINUE
      CHDF=CHDF/FLOAT(N+2-NSP)
  100 RETURN
      END
