C   20/04/85 702111937  MEMBER NAME  RUNFIX   (JADEGS)      FORTRAN
C
C-----------------------------------------------------------------------
         SUBROUTINE RUNFIX
C-----------------------------------------------------------------------
C
C      AUTHOR:  C. BOWDERY  20/05/85 :  FIX-UP SOME RUN DEPENDENT VALUES
C
C      CHANGE:  J. OLSSON   16/10/85 :  R22357 BEAM ENERGY FIXED
C      CHANGE:  J. OLSSON    9/02/86 :  NOTIFY FORMAT MODIFIED
C      CHANGE:  C. BOWDERY  10/02/86 :  CORRECT EARLIER BUG, ETC
C      CHANGE:  J. OLSSON   21/06/86 :  R10419 MAG FIELD FIXED
C      CHANGE:  J. OLSSON   10/07/86 :  R26435 BEAM ENERGY
C      CHANGE:  J. OLSSON   29/07/86 :  R4841 MAG FIELD
C      CHANGE:  J. OLSSON   13/08/86 :  R4544,5662 MAG FIELD
C      CHANGE:  J. OLSSON   25/08/86 :  R13106-13145 NO MAGNETIC FIELD
C      CHANGE:  J. OLSSON   27/08/86 :  R11396 MAG FIELD
C      CHANGE:  J. OLSSON   28/08/86 :  R24354 JUNK WARNING
C      CHANGE:  J. OLSSON   02/09/86 :  R25688 COSMIC WARNING
C LAST CHANGE:  J. OLSSON   03/09/86 :  R28382 EBEAM CORRECTION
C      CHANGE:  J. OLSSON   10/11/86 :  R6185 JUNK WARNING
C      CHANGE:  J. OLSSON   30/01/87 :  R29828 MAG.TRIP AT END
C      CHANGE:  J. OLSSON   11/02/87 :  COSMIC RUNS END 86 WARNING
C
C
C-----------------------------------------------------------------------
C
      IMPLICIT INTEGER*2 (H)
C
C----------------------------------------------------------------------
C             MACRO CDATA .... BOS COMMON.
C
C             THIS MACRO ONLY DEFINES THE IDATA/HDATA/ADATA NAMES.
C             THE ACTUAL SIZE OF /BCS/ IS FIXED ON MACRO CBCSMX
C             OR BY OTHER MEANS. A DEFAULT SIZE OF 40000 IS GIVEN HERE.
C
C----------------------------------------------------------------------
C
      COMMON /BCS/ IDATA(40000)
      DIMENSION HDATA(80000),ADATA(40000),IPNT(50)
      EQUIVALENCE (HDATA(1),IDATA(1),ADATA(1)),(IPNT(1),IDATA(55))
      EQUIVALENCE (NWORD,IPNT(50))
C
C------------------------ END OF MACRO CDATA --------------------------
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
      IF( NRUN .GE.  2570  .AND.  NRUN  .LE. 2802 ) HDATA(IHHEAD+8)=198000005000
C
      IF( NRUN .GT.  6000  .AND.  IYEAR .LT. 1981 ) HDATA(IHHEAD+8)=198100005200
      IF( NRUN .GT. 10000  .AND.  IYEAR .LT. 1982 ) HDATA(IHHEAD+8)=198200005300
      IF( NRUN .GE. 20000  .AND.  IYEAR .LT. 1985 ) HDATA(IHHEAD+8)=198500005400
C
C                            FIX 29 FEBRUARY 1981 SCREW UP
C
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
  15  IF( NRUN .EQ. 4544         )     HDATA(IHHEAD+30) = -4858
C
      IF( NRUN .EQ. 4841         )     HDATA(IHHEAD+30) = -4848
C
      IF( NRUN .EQ. 5662         )     HDATA(IHHEAD+30) = -4844
C
      IF( NRUN .EQ. 10419        )     HDATA(IHHEAD+30) = -4844
C
      IF( NRUN .EQ. 11396        )     HDATA(IHHEAD+30) = -4844
C
      IF( NRUN .EQ. 11908        )     HDATA(IHHEAD+30) = -4848
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
      IF( NRUN .EQ. 26435 )                         IBEAM = 17500
      IF( NRUN .EQ. 28382 )                         IBEAM = 17500
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
C--
        IF(NRUN.GE.13106.AND.NRUN.LE.13145.AND.IBADF.LE.500)
     $  WRITE(6,225) NRUN
 225    FORMAT(/' ***  R U N F I X  ***   WARNING:  RUN ',I6,' HAS B=0,
     $ DONT USE FOR ANALYSIS !!!')
C--
        IF(NRUN.EQ.29828.AND.IBADF.LE.500)
     $  WRITE(6,231) NRUN
 231    FORMAT(/' ***  R U N F I X  ***   WARNING:  RUN ',I6,' WITH MAGN
     $ET TRIP AT END, B=0 !!!')
C--
        IF(NRUN.EQ.24354.AND.IBADF.LE.500)
     $  WRITE(6,226) NRUN
        IF(NRUN.EQ. 6185.AND.IBADF.LE.500)
     $  WRITE(6,226) NRUN
 226    FORMAT(/' ***  R U N F I X  ***   WARNING:  RUN ',I6,' IS JUNK,
     $ DONT USE FOR ANALYSIS !!!')
        IF(NRUN.EQ.24354) GO TO 50
        IF(NRUN.EQ. 6185) GO TO 50
        IF(NRUN.EQ.29828) GO TO 50
        IF(NRUN.GE.13106.AND.NRUN.LE.13145) GO TO 50
C--
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
      IF( IBEAM .GE.   5000.AND.IBEAM.LT.25000 ) GO TO 50
      IF( NRUN  .EQ. LASTR2 ) GO TO 50
        IBADEN  = IBADEN + 1
        LASTR2  = NRUN
C--
        IF((NRUN.EQ.29902.OR.NRUN.EQ.29930).AND.IBADEN.LE.500)
     $  WRITE(6,236) NRUN
        IF((NRUN.EQ.29931.OR.NRUN.EQ.29932).AND.IBADEN.LE.500)
     $  WRITE(6,236) NRUN
        IF(NRUN.GE.29982.AND.NRUN.LE.29994.AND.IBADEN.LE.500)
     $  WRITE(6,236) NRUN
        IF(NRUN.EQ.25688.AND.IBADEN.LE.500)
     $  WRITE(6,236) NRUN
 236    FORMAT(/' ***  R U N F I X  ***   WARNING:  RUN ',I6,' IS COSMIC
     $ DATA !!!')
        IF(NRUN.EQ.25688) GO TO 50
        IF(NRUN.EQ.29902) GO TO 50
        IF(NRUN.GE.29930.AND.NRUN.LE.29932) GO TO 50
        IF(NRUN.GE.29982.AND.NRUN.LE.29994) GO TO 50
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
