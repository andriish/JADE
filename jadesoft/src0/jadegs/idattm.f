C   03/11/78 807251259  MEMBER NAME  IDATTM   (JADEGS)      FORTRAN
C
C-----------------------------------------------------------------------
      FUNCTION IDATTM( NEW )
C-----------------------------------------------------------------------
C
C
C   AUTHOR:    S. YAMADA      3/11/78 : GIVES DATE AND HOUR
C
C LAST MOD:    C. BOWDERY    25/07/88 : CORRECT FOR LEAP YEARS AFTER 84
C
C
C     GIVES DATE AND TIME
C
C-----------------------------------------------------------------------
C
      INTEGER  IDTN(5), LENMON(12)
      DATA     LENMON  / 31,28,31,30,31,30,31,31,30,31,30,31 /
      DATA     IDTO / 0 /
C
C------------------  C O D E  ------------------------------------------
C
C
      IF( NEW .NE. 0  .AND.  IDTO .GT. 0 ) GO TO 100
C
C                            GET DATE AND TIME
C                            IDTN(1) =  YEAR (2 DIGITS)
C                            IDTN(2) =  DAY OF THE YEAR
C                            IDTN(3) =  HOURS
C                            IDTN(4) =  MINUTES
C                            IDTN(5) =  SECONDS
C
      CALL DAY2(IDTN)
C
      IYEAR = IDTN(1)
      IDAYY = IDTN(2)
      IHOUR = IDTN(3)
C
C                            FEBRUARY: SPECIAL TREATMENT FOR LEAP YEARS
C
      IF( MOD( IYEAR, 4 ) .EQ. 0 ) GO TO 1
        LENMON(2) = 28
        GO TO 2
C
    1   LENMON(2) = 29
C
C                            EVALUATE MONTH.
C
    2 IDAY = IDAYY
      DO  11  MONTH = 1,12
        IF( IDAY .LE. LENMON( MONTH ) ) GO TO 12
        IDAY = IDAY - LENMON( MONTH )
   11 CONTINUE
C
   12 IDTO = IYEAR*1000000 + MONTH*10000 + IDAY*100 + IHOUR
C
  100 IDATTM = IDTO
      RETURN
      END
