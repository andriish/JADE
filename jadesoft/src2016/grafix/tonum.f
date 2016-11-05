C   24/11/78 C9080401   MEMBER NAME  TONUM    (JADEGS)      FORTRAN
      SUBROUTINE TONUM(INT1,FLP1)
C
C     **********  FETCH PAIR OF NUMBERS FROM OPERATOR  **********
C
      IMPLICIT INTEGER*2 (H)
      INTEGER*1 BLANK,POINT,MINUS
      INTEGER*1 TNN(20),ZW, INTD
      DATA BLANK/1H /
      DATA POINT/1H./
      DATA MINUS/1H-/
*** PMF 20/11/99: Variables added to avoid problems 
*   when calling TRMIN/TRMOUT with logical arguments
      CHARACTER CTNN*20
      EQUIVALENCE(CTNN,TNN(1))
*** PMF(end)
C
   10 CONTINUE
      CALL TRMIN(20,cTNN) ! PMF 20/11/99: TNN-> cTNN
C                                       GET INTEGER NUMBER
      INT = 0
      NUM1 = 0
      ISIGN = 1
      DO 20 I=1,20
        MKI = I
        ZW = TNN(I)
        IF(ZW.EQ.BLANK) GOTO 30
        IF(ZW.NE.MINUS) GOTO 22
        ISIGN = -1
        GOTO 20
   22   IF(ZW.LT.(240-192) .OR. ZW.GT.(249-192)) GOTO 60!PMF 20/11/99: substract 192 ( EBCDIC-> ASCII Code for numbers)
        NUM1 = NUM1 + 1
        INTD = ZW-240+192!PMF 20/11/99: substract 192
        INT = 10*INT + INTD
   20 CONTINUE
C                                       CHECK IF MORE THAN 1 NUMBER
   30 INT1 = INT
      INT = INT * ISIGN
      IF(MKI .GT.8) GOTO 60
      IF(NUM1.EQ.0) GOTO 70
C                                       GET REAL NUMBER
      NBP=0
      NREL=0
      SIGN=1.
      FLP = 0.
      NUM2 = 0
      DO 40 I=MKI,20
        ZW = TNN(I)
        IF(ZW.EQ.BLANK) GOTO 40
        IF(ZW.NE.MINUS) GOTO 32
        SIGN = -1.
        GOTO 40
   32   IF(ZW.NE.POINT) GOTO 34
        NREL=1
        GOTO 40
   34   IF(ZW.LT.240-192 .OR. ZW.GT.249-192) GOTO 60!PMF 20/11/99: substract 192
        NUM2 = NUM2 + 1
        NBP=NBP+NREL
        INTD=ZW-240+192!PMF 20/11/99: substract 192
        FLP = 10.*FLP + INTD
   40 CONTINUE
C
      FLP1 = SIGN * FLP / (10.**NBP)
      GOTO 70
C
   60 CALL TRMOUT(80,'ILLEGAL CHARACTER. TRY AGAIN. STRING WAS:^')
      CALL TRMOUT(20,TNN)
      GOTO 10
C
   70 CONTINUE
      INT1 = INT
      RETURN
      END
