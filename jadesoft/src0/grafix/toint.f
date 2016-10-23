C   06/03/79 507081844  MEMBER NAME  TOINT    (S)           FORTRAN
      SUBROUTINE TOINT(I1,I2)
C
C     REVISED VERSION OF TONUM, TO GET TWO INTEGERS FROM SCREEN, SEPA-
C     RATED BY BLANK OR KOMMA.
C
      LOGICAL*1 BLANK,KOMMA,MINUS,TNN,ZW
      COMMON /CWORK1/ TNN(20)
      DATA BLANK/' '/
      DATA KOMMA/','/
      DATA MINUS/'-'/
C
      I1 = 0
      I2 = 0
   10 CONTINUE
      CALL TRMIN(20,TNN)
C                                       GET INTEGER NUMBER 1
      INT = 0
      NUM1 = 0
      ISGN = 1
      DO 20 I=1,20
        MKI = I
        ZW = TNN(I)
        IF(ZW.EQ.BLANK.OR.ZW.EQ.KOMMA) GOTO 30
        IF(ZW.NE.MINUS) GOTO 32
        ISGN = -1
        GOTO 20
 32     IF(ZW.LT.240 .OR. ZW.GT.249) GOTO 60
        NUM1 = NUM1 + 1
        INTD = ZW-240
        INT = 10*INT + INTD
   20 CONTINUE
C                                       CHECK IF MORE THAN 1 NUMBER
   30 I1 = ISGN*INT
      IF(MKI .GT.10) GOTO 60
      IF(NUM1.EQ.0) GOTO 70
C                                       GET INTEGER NUMBER 2
      ISGN=1
      INT = 0.
      DO 40 I=MKI,20
        ZW = TNN(I)
        IF(ZW.EQ.BLANK.OR.ZW.EQ.KOMMA) GOTO 40
        IF(ZW.NE.MINUS) GOTO 34
        ISGN = -1
        GOTO 40
   34   IF(ZW.LT.240 .OR. ZW.GT.249) GOTO 60
        INTD=ZW-240
        INT = 10*INT + INTD
   40 CONTINUE
C
      I2 = ISGN * INT
      GOTO 70
C
   60 CALL TRMOUT(80,'ILLEGAL CHARACTER. TRY AGAIN. STRING WAS:^')
      CALL TRMOUT(20,TNN)
      GOTO 10
   70 CONTINUE
      RETURN
      END
