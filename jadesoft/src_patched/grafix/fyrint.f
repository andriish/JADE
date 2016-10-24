C   07/11/79 507081832  MEMBER NAME  FYRINT   (S)           FORTRAN
      SUBROUTINE FYRINT(I1,I2,I3,I4)
C
C     REVISED VERSION OF TONUM, TO GET FOUR INTEGERS FROM SCREEN, SEPA-
C     RATED BY BLANK OR KOMMA.
C
      IMPLICIT INTEGER*2 (H)
      character*1 BLANK,KOMMA,MINUS,TNN,ZW,SLASH
      COMMON /CWORK1/ TNN(40)
      DATA BLANK/' '/,SLASH/'/'/
      DATA KOMMA/','/
      DATA MINUS/'-'/
*** PMF 20/11/99: Variables added to avoid problems 
*   when calling TRMIN/TRMOUT with logical arguments
      CHARACTER CTNN*40
      EQUIVALENCE(CTNN,TNN(1))
*** PMF(end)
C
10    I1 = 0
      I2 = 0
      I3 = 0
      I4 = 0
      CALL TRMIN(40,cTNN)    ! PMF 20/11/99: TNN-> cTNN
C                                       GET INTEGER NUMBER 1
      INT = 0
      NUM1 = 0
      ISGN = 1
      DO 20 I=1,40
        MKI = I
        ZW = TNN(I)
        IF(ZW.EQ.BLANK.OR.ZW.EQ.KOMMA.OR.ZW.EQ.SLASH) GOTO 30
        IF(ZW.NE.MINUS) GOTO 32
        ISGN = -1
        GOTO 20
 32     IF(ZW.LT.240-192 .OR. ZW.GT.249-192) GOTO 60 !PMF 20/11/99: substract 192 ( EBCDIC-> ASCII Code for numbers)
        NUM1 = NUM1 + 1
        INTD = ZW-240+192       !PMF 20/11/99: substract 192
        INT = 10*INT + INTD
   20 CONTINUE
C                                       CHECK IF MORE THAN 1 NUMBER
   30 I1 = ISGN*INT
      IF(MKI .GT.10) GOTO 60
      IF(NUM1.EQ.0) GOTO 70
C                                       GET INTEGER NUMBER 2
      NUM1 = 0.
      ISGN=1
      INT = 0.
      MKI = MKI + 1
      DO 41 I=MKI,40
        MKII = I
        ZW = TNN(I)
        IF(ZW.EQ.BLANK.OR.ZW.EQ.KOMMA.OR.ZW.EQ.SLASH) GOTO 50
        IF(ZW.NE.MINUS) GOTO 33
        ISGN = -1
        GOTO 41
   33   IF(ZW.LT.240-192 .OR. ZW.GT.249-192) GOTO 60!PMF 20/11/99: substract 192
        NUM1 = NUM1 + 1
        INTD=ZW-240+192!PMF 20/11/99: substract 192
        INT = 10*INT + INTD
   41 CONTINUE
C
 50   I2 = ISGN * INT
      IF(MKII.GT.20) GOTO 60
      IF(NUM1.EQ.0) GOTO 70
C                                       GET INTEGER NUMBER 3
      NUM1 = 0.
      ISGN=1
      INT = 0.
      MKI = MKII + 1
      DO 741 I=MKI,40
        MKII = I
        ZW = TNN(I)
        IF(ZW.EQ.BLANK.OR.ZW.EQ.KOMMA.OR.ZW.EQ.SLASH) GOTO 750
        IF(ZW.NE.MINUS) GOTO 733
        ISGN = -1
        GOTO 741
  733   IF(ZW.LT.240-192 .OR. ZW.GT.249-192) GOTO 60  ! PMF 20/11/99: substract 192
        NUM1 = NUM1 + 1
        INTD=ZW-240+192                            ! PMF 20/11/99: substract 192
        INT = 10*INT + INTD
  741 CONTINUE
C
750   I3 = ISGN * INT
      IF(MKII.GT.30) GOTO 60
      IF(NUM1.EQ.0) GOTO 70
C                                       GET INTEGER NUMBER 4
      ISGN=1
      INT = 0.
      MKII = MKII + 1
      DO 40 I=MKII,40
        ZW = TNN(I)
        IF(ZW.EQ.BLANK.OR.ZW.EQ.KOMMA.OR.ZW.EQ.SLASH) GOTO 55
        IF(ZW.NE.MINUS) GOTO 34
        ISGN = -1
        GOTO 40
   34   IF(ZW.LT.240-192 .OR. ZW.GT.249-192) GOTO 60 ! PMF 20/11/99: substract 192
        INTD=ZW-240+192                            ! PMF 20/11/99: substract 192
        INT = 10*INT + INTD
   40 CONTINUE
C
55    I4 = ISGN * INT
      GOTO 70
C
   60 CALL TRMOUT(80,'ILLEGAL CHARACTER. TRY AGAIN. STRING WAS:^')
      CALL TRMOUT(40,cTNN)                          ! PMF 20/11/99: TNN-> cTNN
      GOTO 10
   70 CONTINUE
      RETURN
      END
