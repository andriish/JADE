C   20/04/85 603042008  MEMBER NAME  RUNFIX   (JADESR)      FORTRAN
C
C-----------------------------------------------------------------------
         SUBROUTINE RUNFIX
C-----------------------------------------------------------------------
C
C      AUTHOR:  C. BOWDERY  20/05/85 :  FIX-UP SOME RUN DEPENDENT VALUES
C
C      CHANGE:  J. OLSSON   16/10/85 :  R22357 BEAM ENERGY FIXED
C      CHANGE:  J. OLSSON    9/02/86 :  NOTIFY FORMAT MODIFIED
C LAST CHANGE:  C. BOWDERY  10/02/86 :  CORRECT EARLIER BUG, ETC
C LAST CHANGE:  J. OLSSON   4/04/86 :  CORR.DATE 29.2-2.3
C
C
C-----------------------------------------------------------------------
C
      IMPLICIT INTEGER*2 (H)
C
#include "cdata.for"
C
      COMMON / CKALPR / NOHEAD, IBADF, IBADEN, LASTR1, LASTR2
C
C------------------  C O D E  ------------------------------------------
C
      IQHEAD = IBLN('HEAD')
      IHHEAD = IDATA(IQHEAD)*2
C
      IF( IHHEAD .GT. 0 ) GO TO 10
        NOHEAD = NOHEAD + 1
        IF( NOHEAD .LE. 10 ) WRITE(6,5)
   5    FORMAT(/' ***  R U N F I X  ***      ERROR:  NO ''HEAD'' BANK')
        RETURN
C
  10  NRUN   = HDATA(IHHEAD+10)
C
C                            FIX YEAR SCREW UP.
C
      IYEAR  = HDATA(IHHEAD+8)
C
      IF( NRUN .GE.  2570  .AND.  NRUN  .LE. 2802 ) HDATA(IHHEAD+8)=1980
CAV00003500
C
      IF( NRUN .GT.  6000  .AND.  IYEAR .LT. 1981 ) HDATA(IHHEAD+8)=1981
CAV00003700
      IF( NRUN .GT. 10000  .AND.  IYEAR .LT. 1982 ) HDATA(IHHEAD+8)=1982
CAV00003800
      IF( NRUN .GE. 20000  .AND.  IYEAR .LT. 1985 ) HDATA(IHHEAD+8)=1985
CAV00003900
C
C                            FIX 29 FEBRUARY -2 MARCH 1986 SCREW UP
C
      IF( HDATA(IHHEAD+8) .NE. 1986 ) GO TO 101
      IF( HDATA(IHHEAD+7) .GT. 3 ) GO TO 101
      IF( HDATA(IHHEAD+7) .LT. 2 ) GO TO 101
      IF( HDATA(IHHEAD+7).EQ.3.AND.HDATA(IHHEAD+6).EQ.2.AND.
     $ HDATA(IHHEAD+5).LT.4) HDATA(IHHEAD+6) = 3
      IF( HDATA(IHHEAD+7).EQ.3.AND.HDATA(IHHEAD+6).EQ.1)
     $ HDATA(IHHEAD+6) = 2
      IF( HDATA(IHHEAD+7).NE.2.OR.HDATA(IHHEAD+6).NE.29) GO TO 101
      HDATA(IHHEAD+6) = 1
      HDATA(IHHEAD+7) = 3
C
C                            FIX 29 FEBRUARY 1981 SCREW UP
C
101   CONTINUE
      IF( HDATA(IHHEAD+8) .EQ. 1980 ) GO TO 15
      IF( HDATA(IHHEAD+8) .EQ. 1984 ) GO TO 15
      IF( HDATA(IHHEAD+6) .NE.   29 ) GO TO 15
      IF( HDATA(IHHEAD+7) .NE.    2 ) GO TO 15
        HDATA(IHHEAD+6) = 1
        HDATA(IHHEAD+7) = 3
C
C                            RESET MAGNETIC FIELD FOR PERIOD WHEN  READ
C                            OUT SET IT WRONGLY
C
  15  IF( NRUN .EQ. 11908        )     HDATA(IHHEAD+30) = -4848
C
      IF( NRUN .GE. 13651  .AND.
     +    NRUN .LE. 13653        )     HDATA(IHHEAD+30) = -4844
C
      IF( NRUN .GE. 12255  .AND.
     +    NRUN .LE. 12270        )     HDATA(IHHEAD+30) = -4847
C
      IF( NRUN .GE. 20329  .AND.
     +    NRUN .LE. 20333        )     HDATA(IHHEAD+30) = -4850
C
C                            CORRECT BEAM ENERGIES FOR SELECTED RUNS
C
      IBEAM = HDATA(IHHEAD+29)
C
      IF( NRUN .LT. 10000 ) GO TO 20
      IF( NRUN .GT. 11037  .AND.  NRUN .LT. 11053 ) IBEAM = 17243
      IF( NRUN .EQ. 11104  .OR.   NRUN .EQ. 10666 ) IBEAM = 17300
      IF( NRUN .EQ. 11790  .OR.   NRUN .EQ. 11791 ) IBEAM = 17300
      IF( NRUN .EQ. 12804  .OR.   NRUN .EQ. 12805 ) IBEAM = 19120
      IF( NRUN .GE. 13707  .AND.  NRUN .LE. 13713 ) IBEAM = 20725
      IF( NRUN .EQ. 13714 )                         IBEAM = 20740
      IF( NRUN .EQ. 13826 )                         IBEAM = 20795
      IF( NRUN .EQ. 16342 )                         IBEAM = 23240
      IF( NRUN .EQ. 22357 )                         IBEAM = 21800
C
      HDATA(IHHEAD+29) = IBEAM
C
C                            PRINTOUT(WARNING) LOW MAG.FIELD
C                            MAX 500 TIMES AND ONLY ONCE PER RUN
C
  20  IFIELD = HDATA(IHHEAD+30)
      IF( IABS(IFIELD) .GE. 1000 ) GO TO 30
      IF( NRUN .EQ. LASTR1       ) GO TO 30
        IBADF  = IBADF + 1
        LASTR1 = NRUN
        IF( IBADF .LE. 500 ) WRITE(6,25) NRUN,IFIELD
  25    FORMAT(/' ***  R U N F I X  ***   WARNING:  RUN ',I6,' HAS ',
     +        'A MAGNETIC FIELD VALUE SET TO ',I5,' GAUSS')
        IF( IBADF .LE. 10 ) WRITE(6,28)
  28    FORMAT(/'       PLEASE NOTIFY:   CHRIS BOWDERY    J.OLSSON
     + W.BARTEL   P.STEFFEN   ')
C
C                            PRINTOUT(WARNING) LOW BEAM ENERGY
C                            MAX 500 TIMES AND ONLY ONCE PER RUN
C
 30   IF( NRUN  .LT.   6000 ) GO TO 50
      IF( IBEAM .GE.   5000 ) GO TO 50
      IF( NRUN  .EQ. LASTR2 ) GO TO 50
        IBADEN  = IBADEN + 1
        LASTR2  = NRUN
        IF( IBADEN .LE. 500 ) WRITE(6,40) NRUN,IBEAM
 40     FORMAT(/' ***  R U N F I X  ***   WARNING:  RUN ',I6,' HAS',
     +          ' BEAM ENERGY = ',I6,' MEV')
        IF( IBADEN .LE. 10 ) WRITE(6,28)
C
 50   CONTINUE
C
C                            ROOM FOR FUTURE BODGES HERE
C
      RETURN
      END
