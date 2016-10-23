C   07/06/96 606071902  MEMBER NAME  PFUN     (S4)          FORTG1
      SUBROUTINE PFUN(M)
      COMMON/BCS/IW(1000)
      REAL RW(1000)
      EQUIVALENCE (RW(1),IW(1))
      REAL X(31),Y(31)
      YY=BFUN(M,0.0)
      CALL BLOC(IND,'+FUN',M,&100)
      XL=RW(IND+1)
      NW=IW(IND)
      XH=RW(IND+NW-4)
      CALL GPL(1)
      CALL GRX(XL,XH)
      DO 10 I=1,31
      X(I)=XL+FLOAT(I-1)*(XH-XL)/30.0
   10 Y(I)=BFUN(M,X(I))
      CALL GMY(Y,31)
      CALL GPT(Y,X,31,38)
      ND=NW/5
      INDH=IND
      DO 20 I=1,ND
      XX=RW(INDH+1)
      YY=RW(INDH+2)
      INDH=INDH+5
   20 CALL GPT(YY,XX,1,33)
      CALL GNE(M)
      CALL GTE('PLOT OF FUNCTION BY PFUN$')
      CALL GPR(1)
  100 RETURN
      END
