C   29/03/82         4  MEMBER NAME  R1MODE   (JADESR)      FORTRAN
      SUBROUTINE R1MODE
C---------------------------------
C ROUTINE TO DETERMINE REDUC1 MODE
C GFP -- SEPTEMBER 1981
C---------------------------------
      LOGICAL*1 DSN(52),TAPEON
#include "cutsr1.for"
      REAL*8 WORD8,ORDERS(2)/'STARTRUN','NOTRUNS '/
C-------------------
C PRINT PROGRAM NAME
C-------------------
      PRINT5,NVRSN
   5  FORMAT(1H1//
     +1X,10('-')/
     +1X,10('-')/
     +1X,10('-'),5X,'==============================================='/
     +1X,10('-'),5X,'JADE DATA REDUCTION STEP 1 PROGRAM VERSION ',I4/
     +1X,10('-'),5X,'==============================================='/
     +1X,10('-')/
     +1X,10('-'))
C------------------------------
C DETERMINE CALIBRATION SOURCE
C------------------------------
      CALL DSNFDD(8HFT02F002,DSN,IER)
      IF(IER.NE.0)GOTO15
      TAPEON=.TRUE.
      PRINT10
 10   FORMAT(1X,10('-'),1X,
     +        'CALIBRATION FILE ON FT22F001 OVERWRITTEN FROM FT02F001')
      GOTO18
C
C
 15   CALL DSNFDD(8HFT02F001,DSN,IER)
      TAPEON=.FALSE.
      PRINT16
 16   FORMAT(1X,10('-'),1X,
     + 'CALIBRATION FILE READ DIRECTLY FROM FT22F001')
C----------------------
C PRINT INPUT FILE NAME
C----------------------
 18   PRINT99
      PRINT19,(DSN(I),I=1,44)
      PRINT99
 19   FORMAT(1X,10('-'),' INPUT FILE OF EVENTS  = ',44A1)
C-----------------------
C PRINT OUTPUT FILE NAME
C-----------------------
      CALL DSNFDD(8HFT03F001,DSN,IER)
      PRINT20,(DSN(I),I=1,44)
      PRINT99
 20   FORMAT(1X,10('-'),' OUTPUT FILE OF EVENTS = ',44A1)
C------------------------------------
C DETERMINE FIRST EVENT TO BE TREATED
C------------------------------------
      READ(5,30,END=35)WORD8,I1,I2
  30  FORMAT(A8,1X,I8,1X,I8)
      IF(WORD8.NE.ORDERS(1))GOTO35
      NRUNST = I1
      NEVTST = I2
  35  PRINT36,NRUNST,NEVTST
      PRINT99
  36  FORMAT(1X,10('-'),' PROGRAM TO START AT RUN',I6,' EVENT',I6)
C-----------------------------
C DETERMINE RUNS TO BE SKIPPED
C-----------------------------
      NOTTOT=0
      READ(5,40,END=42)WORD8,(NOTRUN(I),I=1,10)
  40  FORMAT(A8,10I6)
      IF(WORD8.NE.ORDERS(2))GOTO42
      DO 41 I=1,10
      IF(NOTRUN(I).EQ.0)GOTO42
  41  NOTTOT=NOTTOT+1
  42  IF(NOTTOT.GT.0)PRINT48,NOTTOT,(NOTRUN(I),I=1,10)
      IF(NOTTOT.EQ.0)PRINT49,NOTTOT
  48  FORMAT(
     +1X,10('-'),I3,' RUNS TO BE AXED',10I6/
     +1X,10('-')/
     +1X,10('-'))
  49  FORMAT(
     +1X,10('-'),I3,' RUNS TO BE AXED'/
     +1X,10('-')/
     +1X,10('-'))
C-----------------------------
C PRINT LEAD GLASS ENERGY CUTS
C-----------------------------
      IF(NVRSN.EQ.1981)PRINT80,ELGLM(1),ELGLM(2),ELGLM(3)
      IF(NVRSN.EQ.1982)PRINT81,(ELGLM(I),I=1,8)
      PRINT99
 80   FORMAT(1X,10('-'),' LEAD GLASS ENERGY THRESHOLDS ',
     +' ETOTLM = ',F6.0,', EMIN =',F6.0,', EMNCYL =',F6.0)
 81   FORMAT(1X,10('-'),    ' LEAD GLASS ENERGY THRESHOLDS ',
     +                      ' ETOT / EBAR :',4F6.0/
     +       1X,10('-'),38X,        'ECAP :',4F6.0)
C---------------------------------
C READ CALIBRATION FILES FROM TAPE
C---------------------------------
      IF(TAPEON)CALL KALPRE
      IF(TAPEON)PRINT99
 99   FORMAT(1X,10('-')/1X,10('-'))
      RETURN
      END
      SUBROUTINE VTXINI
C
C
C DUMMY ROUTINE TO REPLACE VTXINI FOR REDUC1 JOB
C
C
      RETURN
      END
      SUBROUTINE DUMP0C
C
C
C DUMMY ROUTINE TO REPLACE DUMP0C FOR REDUC1 JOB
C
C
      RETURN
      END
      SUBROUTINE RDMTCO(I)
      ENTRY      RDJETC
C
C
C DUMMY ROUTINE TO REPLACE RDMTCO/RDJETC FOR REDUC1 JOB
C
C
      PRINT100
 100  FORMAT(' *** RDMTCO IS A DUMMY ROUTINE --> THIS REDUC1 PACKAGE',
     +       ' IS NOT FOR MCARLO ***')
      RETURN
      END
