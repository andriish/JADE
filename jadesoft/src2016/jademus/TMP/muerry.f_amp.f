C   12/01/83 403122039  MEMBER NAME  MUERRY   (JADEMUS)     FORTRAN
C
C-----------------------------------------------------------------------
      SUBROUTINE MUERRY(NAME,INF,MESS)
C-----------------------------------------------------------------------
C
C LAST CHANGE 20.15 12/03/84 CHRIS BOWDERY-TIDYING UP
C      CHANGE 12.00 12/01/83 HUGH MCCANN - TRIVIAL BUG.
C      CHANGE 10.30 23/02/82 HUGH MCCANN - TO ADD MUCUTS TREATMENT.
C      CHANGE 13.30 22/02/82 HUGH MCCANN - FOR IMPROVED ERROR OUTPUT.
C      CHANGE 10.3 11/09/79 JOHN ALLISON.
C
C      PRINTS MU ERROR MESSAGES.
C      .  PROVIDE SUBROUTINE NAME IN NAME.
C      .  PROVIDE INFORMATION IN INTEGER*4 WORD INF.
C      .  PROVIDE MESSAGE IN MESS TERMINATED BY '^'.
C
C
C-----------------------------------------------------------------------
C
      LOGICAL*1 MESS(1)
      LOGICAL*1 BAR/'^'/
C
      INTEGER*2 NAME(3)
      INTEGER*2 NANAC(3)/'MU','AN','AC'/,
     *          NCOOR(3)/'MU','CO','OR'/,
     *          NANAF(3)/'MU','AN','AF'/,
     *          NCUTS(3)/'MU','CU','TS'/
C
      COMMON /CMUPRN/ MUPRIN,MUHPR,IANAC,ICOOR,IANAF,LANAC,LCOOR,LANAF,
     +                ITOTAL,LTOTAL,ICUTS,LCUTS
      COMMON /CMUIEV/ IEV,NEV,IRD,KRUN,KREC,
     +                ISECS,IMINS,IHOURS,IDAY,IMONTH,IYEAR
C
C------------------ C O D E  -------------------------------------------
C
      IF( MUPRIN .EQ. 0 ) GO TO 99
C
C                           INITIALISE MUDUMP FLAG.
C
      IDUMP = 0
C
C                            LOOK OUT FOR CALLS  FROM  MUANAC,  MUCOOR,
C                            MUANAF AND MUCUTS.
C
C                            GO TO 20 IF NAME OF CALLING  ROUTINE  DOES
C                            NOT START WITH 'MU'.
C
      IF( NAME(1) .NE. NANAC(1) ) GO TO 20
      IF( NAME(2) .EQ. NANAC(2)  .AND.  NAME(3) .EQ. NANAC(3) ) GO TO 5
      IF( NAME(2) .EQ. NCOOR(2)  .AND.  NAME(3) .EQ. NCOOR(3) ) GO TO 1000020000
      IF( NAME(2) .EQ. NANAF(2)  .AND.  NAME(3) .EQ. NANAF(3) ) GO TO 1500020400
      IF( NAME(2) .EQ. NCUTS(2)  .AND.  NAME(3) .EQ. NCUTS(3) ) GO TO 1800020800
      GO TO 20
C
C                            CALL FROM MUANAC :
C
    5 IANAC = IANAC + 1
      IF( IANAC .EQ. LANAC ) WRITE(6,56) NANAC,LANAC
   56 FORMAT(/' **** WARNING : LIMIT ON NO. OF ERROR MESSAGES FROM ',
     +    3A2,' (',I3,' ) IS NOW REACHED WITH THE FOLLOWING MESSAGE :')
      IF( IANAC .GT. LANAC ) GO TO 99
      GO TO 20
C
C                            CALL FROM MUCOOR :
C
C                            DECODE IER. IP IS NEEDED BY MUDUMP.
C
   10 IP  = INF/100
      MER = INF-100*IP
      INF = MER
C
C                            SET FLAG FOR MUDUMP.
C
      IF( MUPRIN .GE. 3 ) IDUMP = 1
      ICOOR = ICOOR + 1
      IF( ICOOR .EQ. LCOOR ) WRITE(6,56) NCOOR,LCOOR
      IF( ICOOR .GT. LCOOR ) GO TO 99
      GO TO 20
C
C                            CALL FROM MUANAF :
C
   15 IANAF = IANAF + 1
      IF( IANAF .EQ. LANAF ) WRITE(6,56) NANAF,LANAF
      IF( IANAF .GT. LANAF ) GO TO 99
      GO TO 20
C
C                            CALL FROM MUCUTS :
C
   18 ICUTS = ICUTS + 1
      IF( ICUTS .EQ. LCUTS ) WRITE(6,56) NCUTS,LCUTS
      IF( ICUTS .GT. LCUTS ) GO TO 99
C
   20 CONTINUE
C
C                            COUNT  TOTAL  NO.  OF  MESSAGES  OUTPUT  *
C                            COMPARE WITH JOB LIMIT .
C
      ITOTAL = ITOTAL + 1
      IF( ITOTAL .EQ. LTOTAL ) WRITE(6,26) LTOTAL
   26 FORMAT(/'XXXX WARNING : LIMIT ON THE TOTAL NO. OF MESSAGES (',
     +I4,' ) FOR THIS JOB IS NOW REACHED WITH THE FOLLOWING MESSAGE :')
      IF( ITOTAL .GT. LTOTAL ) GO TO 99
C
C                            COUNT NUMBER OF CHARACTERS.
C
      N = 1
 1    IF( MESS(N) .EQ. BAR ) GO TO 2
      N = N + 1
      IF( N .LE. 200 ) GO TO 1
C
   2  N=N-1
C
C                            WRITE EVENT HEADER IF THIS  IS  THE  FIRST
C                            MESSAGE FOR THIS EVENT.
C
      IF( MUHPR .EQ. 0 ) WRITE(6,1006) KRUN,KREC,IEV,IHOURS,IMINS,
     +                                 ISECS,IDAY,IMONTH,IYEAR
 1006 FORMAT(/'   RUN',I6,2X,'EVENT',I6,
     +     2X,'RECORD (FILE)',I6,2X,'TIME =',I3,':',I2,':',I2,
     +     2X,'DATE =',I3,':',I2,':',I4,'   MESSAGES :')
C
      MUHPR = 1
C
      WRITE(6,100) NAME,INF,(MESS(I),I=1,N)
 100  FORMAT('   *** MU ERROR IN ',3A2,4X,', INFORMATION --->',I10,',',
     +       4X,70A1/(1X,132A1))
C
C                            FOR MUCOOR ERRORS AND MUPRIN .GE. 3 , DUMP
C                            THE MUEV BANK.
C
      IF( IDUMP .NE. 0 ) CALL MUDUMP(INF,IP)
C
 99   CONTINUE
      RETURN
      END
