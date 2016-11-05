C   27/03/85 504191407  MEMBER NAME  MUANAB   (JADEMUS)     FORTRAN
C
C-----------------------------------------------------------------------
      SUBROUTINE  MUANAB
C-----------------------------------------------------------------------
C
C        THIS ROUTINES BEGINS MUON ANALYSIS BY EXTRACTING HEADER DATA
C
C      CHANGE 15.00 27/03/85 CHRIS BOWDERY- NEW ROUTINE
C LAST CHANGE 14.00 19/04/85 CHRIS BOWDERY- 29 FEB BUG CORRECTED
C
C
C-----------------------------------------------------------------------
C
      IMPLICIT INTEGER*2 (H)
C
C                            COMMONS
C
      COMMON / CMUPRN / MUPRIN,MUHPR
      COMMON / CMUIEV / IEV,NEV,IRD,KRUN,KREC,
     *                  ISECS,IMINS,IHOURS,IDAY,IMONTH,IYEAR
#include "cmubcs.for"
C
C------------------  C O D E  ------------------------------------------
C
C                            INITIALISE HEADER PRINT CONTROL
C                            FLAG FOR THIS EVENT( USED IN MUERRY).
C
      MUHPR = 0
C
C                            COUNT THE EVENTS
C
      IEV = IEV + 1
C
C                            EXTRACT EVENT INFORMATION FROM HEADER BANK
C
      IPHEAD = IDATA(IBLN('HEAD'))
C
      ISECS  = HDATA(2*IPHEAD+3)
      IMINS  = HDATA(2*IPHEAD+4)
      IHOURS = HDATA(2*IPHEAD+5)
      IDAY   = HDATA(2*IPHEAD+6)
      IMONTH = HDATA(2*IPHEAD+7)
      IYEAR  = HDATA(2*IPHEAD+8)
      KRUN   = HDATA(2*IPHEAD+10)
      KREC   = HDATA(2*IPHEAD+11)
C
C                            CORRECT YEAR/RUN ANOMALIES
C
      IF( KRUN .GE.  2570  .AND.  KRUN  .LE. 2802 ) IYEAR = 1980
      IF( KRUN .GT.  6000  .AND.  IYEAR .LT. 1981 ) IYEAR = 1981
      IF( KRUN .GT. 10000  .AND.  IYEAR .LT. 1982 ) IYEAR = 1982
C
C                            FIX 29 FEBRUARY MISTAKE FOR NON-LEAP YEARS
C                            IT HAPPENED IN 1981.
C
      IF( IYEAR  .EQ. 1980  .OR.
     +    IYEAR  .EQ. 1984  .OR.
     +    IMONTH .NE. 2     .OR.
     +    IDAY   .NE. 29           ) GO TO 1
        IMONTH = 3
        IDAY   = 1
   1  CONTINUE
C
      RETURN
C
      END
