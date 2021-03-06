C   07/06/96 606071817  MEMBER NAME  BFUN     (S4)          FORTRAN
      FUNCTION BFUN(M,X)
      COMMON/BCS/IW(1000)
      REAL RW(1000)
      EQUIVALENCE (RW(1),IW(1))
      REAL H(2)
      EXTERNAL CUBSPL
C
C     PURPOSE
C        FOR A TABULATED FUNCTION Y = Y(X), CALCULATE THE INTERPOLATED
C        VALUES OF THE FUNCTION AND THE DERIVATIVE AT A GIVEN X
C
C     METHOD
C        THE TABLE OF X AND Y VALUES ARE READ FROM DATACARDS BY
C        SUBR. BREADC (CALLED BY THE USER). THE SPLINE INTERPOLATION
C        METHOD IS USED (SUBR. SPLCOF).
C
C     USAGE
C        EACH FUNCTION IS CHARACTERIZED BY A NUMBER M.
C
C     $FUN M X1 Y1 X2 Y2 . . . XN YN
C
C        F= BFUN(M,X)
C         = INTERPOLATED VALUE OF FUNCTION M FOR GIVEN X
C
C        D= BDER(M,X)
C         = INTERPOLATED VALUE OF DERIVATIVE OF FUNCTION M FOR X
C
C        F OR D = 0.0, IF NO DATA CARDS OR LESS THAN 3 POINTS
C
      IJ=1
   10 CALL BLOC(INDP,'+FUN',M,&20)
      CALL CUBFUN(X,H(1),H(2),RW(INDP+1))
      BFUN=H(IJ)
      GOTO 100
C
      ENTRY BDER(M,X)
      IJ=2
      GOTO 10
C
   20 CALL BLOC(INDF,'$FUN',M,&99)
   30 ND=IW(INDF)/2
      IF(ND.LT.3) GOTO 99
      CALL BCRE(INDP,'+FUN',M,5*ND,&99,IER)
      INDH=INDP
      DO 40 I=1,ND
      RW(INDH+1)=RW(INDF+I+I)
      RW(INDH+2)=RW(INDF+I+I-1)
   40 INDH=INDH+5
      CALL CUBCOP(ND,RW(INDP+1))
      GOTO 10
C
   99 BFUN=0.0
  100 RETURN
      END
