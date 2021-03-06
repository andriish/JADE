C   12/05/82 206081415  MEMBER NAME  TFIN82   (S)           FORTRAN
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
       SUBROUTINE TOFINT(NRUN,INTF,INPA,*)
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
#include "ddata.for"
      COMMON/CTPCNS/BENGEV,BKGAUS
#include "chead.for"
      COMMON/CWORK/NR,RAW(5,42),NC,ICRT1(5,42),NTRK,ICRT2(50),TRK(5,50)
     - ,ITRC(50),NTC,ITRK(5,42),INFM(4),IR(14,50)
       DATA IFLG/0/,IENTRY/0/
       DATA IRUN/0/
      DATA IERL,IERP,IERH/3*0/
C
C========= LOAD CALIBRATION DATA
C
C     IF(IENTRY.EQ.0) PRINT 701
      HEADR(18) = NRUN
      HEADR(19) = IEV
  701 FORMAT(' NOW YOU CALLED THE ONLY LEGAL VERSION OF TOFINT')
      IF(INTF.LE.0) RETURN1
      IF(INPA.LE.0) RETURN1
      IF(NRUN.LE.1659)  CALL TOFIN2(NRUN,INTF,INPA,&3011)
      IF(NRUN.LE.1659)  RETURN
      IF(NRUN.EQ.0.AND.IENTRY.EQ.0) GOTO  101
      IF(NRUN.EQ.IRUN) GOTO  100
  101 IENTRY = IENTRY+1
      IRUN = NRUN
      IF(IENTRY.EQ.1)CALL RD0880(NRUN)
      IF(IENTRY.EQ.1)CALL SUBCAL
C
C========= CONVERSION OF PATT. REC. DATA
C
  100 CALL BLOC(INLA,'LATC',0,&3000)
      IPHEAD = IDATA(IBLN('HEAD'))
      IF(IPHEAD.LE.0)  GOTO  3001
      CALL UCOPY(IDATA(IPHEAD-3),IHEADR,54)
      IGAUS = HDATA(IPHEAD*2+30)
      BKGAUS = FLOAT(IGAUS)/1000.
      CALL TFCTD1(INPA)
      IF(NTRK.LT.1) GO TO 4500
C
C========= CONVERSION OF TOF DATA
C
       IF(NRUN.NE.0.AND.NRUN.LT.8712)CALL TOFARD(INLA,INTF,IFLG,&4000)
       IF(NRUN.GE.8712) CALL TFARNW(INLA,INTF,IFLG,&4000)
C
C========= MAKE CORRESPONDENCE BETW. TRACKS AND COUNTERS
C
       CALL TOFCOR
       CALL TOFMAS
C
       RETURN
C
 3011 PRINT 3010
 3010 FORMAT(' ERROR RETURN FROM TOFIN2')
      RETURN 1
 3000 CONTINUE
      IERL = IERL + 1
      IF(IERL.LE.10) PRINT 9300,IERL
 9300 FORMAT(1X,'RAW DATA ERROR FROM TOF PROG: NO LATCHED DATA',I5)
      RETURN1
 4000 CONTINUE
      PRINT 9400
 9400 FORMAT(' ERROR RETURN FROM TOFARD')
      RETURN1
 4500 IERP = IERP + 1
      IF(IERP.LE.10) PRINT 9450,IERP
 9450 FORMAT(1X,'**** MESSAGE FROM TOF PROG NO TRACK IN PATR BANK',I5)
      RETURN1
 3001 IERH = IERH + 1
      IF(IERH.LE.10) PRINT 3002,IERH
 3002 FORMAT(' MESSAGE FROM TOF PROG NO HEADER ',I5)
      RETURN1
  500 PRINT 501,NRUN
  501 FORMAT(' FOR RUN NUMBERS LESS THAN 1660 PLEASE USE INCLUDE OF
     1 TOF1660 ***********' )
      RETURN 1
      END
