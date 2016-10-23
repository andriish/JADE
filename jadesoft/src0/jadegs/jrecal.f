C   08/12/80 602051412  MEMBER NAME  JRECAL9  (JADEGS)      FORTRAN
      SUBROUTINE JRECAL(IERR)
C---
C---     RECALIBRATES JETC BANK
C---
C---
C---     NEW VERSION FOR FADC-READOUT    P. STEFFEN 12.30 86/02/05
C---
C---     SET AMPLITUDES = 0 IF OVERFLOW  P. STEFFEN 12.30 83/04/21
C---
C---     NEW VERSION WITH A TZERO FOR EACH OF THE THREE RINGS.
C---     AND NEW SLEWING CORRECTIONS
C---                                   P. STEFFEN   16.00 17.09.79
C---      PROTECTION AGAINST MONTE CARLO   J.OLSSON,C.BOWDERY  07.03.84
C---
      IMPLICIT INTEGER*2 (H)
C
#include "cdata.for"
C
#include "cgraph.for"
C
#include "cjdrch.for"
C
      COMMON/CALIBR/JPOINT(16)
      DIMENSION HCALIB(2)
      EQUIVALENCE (HCALIB(1),JPOINT(1))
C
      DIMENSION TOFSET(3)
C
      DATA ZPHS/2440./
      DATA RAMPS/100./
      DATA ICALL/0/
C---
      IERR = 1
C     NHIT = 0
C---                                     INITIALIZATION
      IF(ICALL.NE.0) GO TO 3
        ICALL=1
C---    FACTORS FOR AMPLITUDE CALCULATION
        AC0 =(ZPHS-ZAL)*.5 / ZAL
        AC1 =RAMPS*ZPHS*.5 / ZAL
        IQHEAD=IBLN('HEAD')
        IJETC=IBLN('JETC')
    3 CONTINUE
        IPHEA2=IDATA(IQHEAD)*2
        NRUN=HDATA(IPHEA2+10)
C
C FOR MONTE CARLO, JUST RETURN    CHANGE 7.3.1984   (J.O., C.B.)
C
        IF(NRUN.LT.100) GO TO 200
C
C---    DRIFT TIME OFFSET
        TOFSET(1) = (TZERO(1) + T0FIX(1))*64. + 10000.5
        TOFSET(2) = (TZERO(2) + T0FIX(2))*64. + 10000.5
        TOFSET(3) = (TZERO(3) + T0FIX(3))*64. + 10000.5
C
C---    AMPLITUDE CORRECTION FACTORS FOR TIME SLEWING
        IF(NRUN.GE.6185) GO TO 20
          AMCF1 = -64.*1.494 - .5
          AMCF2 =  7.872E-03*8.
          AMCF3 = -1.157E-05
          AMCF4 = -64.*0.8207 - .5
          AMCF5 =  2.926E-03*8.
          AMCF6 = -2.561E-06
          AMLM1 = 2000.
          AMLM2 = 4000.
          GOTO 30
   20   CONTINUE
          AMCF1 = -64.*1.811 - .5
          AMCF2 =  1.112E-02*8.
          AMCF3 = -2.155E-05
          AMCF4 = -64.*0.8430 - .5
          AMCF5 =  2.340E-03*8.
          AMCF6 = -1.623E-06
          AMLM1 = 1800.
          AMLM2 = 5600.
   30   CONTINUE
C                                        POINTER
      IPHALF = JPOINT(4)*2
      IPJETC = IDATA(IJETC)
      JPJETC = IDATA(IPJETC-1)
      IF(IPJETC.LE.0 .OR. JPJETC.LE.0) GOTO 900
      NWI = IDATA(IPJETC)
      NWJ = IDATA(JPJETC)
      IERR = 2
      IF(NWI.NE.NWJ .OR. NWI.LE.50) GOTO 900
C---
C---                                   COPY UNCALIBRATED BANK
C---
      IPJ2  = IPJETC*2
      IP0   = IPJ2 + 100
      IP9   = HDATA(IP0-1) + IP0 - 2
      NBYTE = HDATA(IP0-1)*2 - 2
      CALL MVCL(IDATA(IPJETC+51),0,IDATA(JPJETC+51),0,NBYTE)
      IDATA(IPJETC+1) = IDATA(JPJETC+1)
      HDATA(IPJ2  +1) = HDATA(IPJ2  +1) + 200
C---                                   LOOP OVER HITS
      DO 200 IP = IP0,IP9,4
        IWIRE  = HDATA(IP+1)
        IA1    = HDATA(IP+2)
        IA2    = HDATA(IP+3)
        IDTIME = HDATA(IP+4)
C
        IF(IA1.LT.0) IA1=0
        IF(IA1.GE.4095) IA1=0
        IA1=SHFTL(IA1,3)
        IF(IA2.LT.0) IA2=0
        IF(IA2.GE.4095) IA2=0
        IA2=SHFTL(IA2,3)
C
        IWIRE  = SHFTR(IWIRE,3)
        IPLOC  = IWIRE* 6 + IPHALF
        ITZERO = HCALIB(IPLOC+1)
        SF     = AC1/HCALIB(IPLOC+2) + AC0
        IPEDL1 = HCALIB(IPLOC+3)
        IGAIN1 = HCALIB(IPLOC+4)
        IPEDL2 = HCALIB(IPLOC+5)
        IGAIN2 = HCALIB(IPLOC+6)
C---
C---       THE 512 (2*9) ADDED BELOW CAUSES THE SUBSEQUENT SHIFT RIGHT
C---       BY 10 BITS TO DO A ROUND OFF RATHER THAN A TRUNCATION.
C---
        IF(IA1.GT.0) IA1 = (IA1-IPEDL1)*IGAIN1+512
        IF(IA1.LT.0) IA1 = 0
        IA1              = SHFTR(IA1,10)
        IF(IA2.GT.0) IA2 = (IA2-IPEDL2)*IGAIN2+512
        IF(IA2.LT.0) IA2 = 0
        IA2              = SHFTR(IA2,10)
C---
C---       COMPUTE AMPLITUDE SLEWING CORRECTION AT THIS POINT.
C---
        IAMCOR=0
        IF(NRUN .GE. 24200) GOTO 290
C
          AMX = MAX0(IA1,IA2)
          IF(AMX.GT.AMLM2) GO TO 202
          IF(AMX.GT.AMLM1) GO TO 201
            IAMCOR = (AMCF3*AMX + AMCF2)*AMX + AMCF1
            GO TO 202
201         IAMCOR = (AMCF6*AMX + AMCF5)*AMX + AMCF4
202       CONTINUE
C
C
  290   CONTINUE
C
        AX = (IA2-IA1)*SF
        IS = SIGN(.5,AX) + AX
        IF(IA1.GT.    0) IA1 = IA1-IS
        IF(IA1.LT.    0) IA1 = 0
        IF(IA1.GT.32767) IA1 = 32767
        IF(IA2.GT.    0) IA2 = IA2+IS
        IF(IA2.LT.    0) IA2 = 0
        IF(IA2.GT.32767) IA2 = 32767
C
C                                         DRIFTTIME
        IF(NRUN .GE. 24200) GOTO 310
          IF(IDTIME.LT.0) IDTIME=0
          IF(IDTIME.GT.255) IDTIME=255
          IDTIME = SHFTL(IDTIME,6)
  310   CONTINUE
        IDTIME = IDTIME - ITZERO + IAMCOR
C
        TZ0 = TOFSET(1)
        IF(IWIRE.GE.384) TZ0 = TOFSET(2)
        IF(IWIRE.GE.768) TZ0 = TOFSET(3)
        IPED = INT(TZ0) - 10000
C
C
        HDATA(IP+2) = IA1
        HDATA(IP+3) = IA2
        HDATA(IP+4) = IDTIME - IPED
C
C
C     NHIT = NHIT + 1
C     IF(NHIT.GT. 8) GOTO 200
C     PRINT 2001, IWIRE,KLOK,IDTIME,IPED,ITZERO,IAMCOR,IA1,IA2
C2001 FORMAT(' JRECAL',8I6)
  200 CONTINUE
      IERR = 0
      RETURN
C
  900 CONTINUE
      WRITE(JUSCRN,2901) IERR
 2901 FORMAT(' JRECAL(PST): ERROR',I4)
      RETURN
      END
