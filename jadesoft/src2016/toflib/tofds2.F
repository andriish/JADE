C   17/01/79 409061156  MEMBER NAME  TOFDS2   (S)           FORTRAN
      SUBROUTINE TOFDS
C--
C--     TOF DISPLAY FOR JADE, USING TOF PROGRAM FROM B.NAROSKA,KAWABATA
C--     TOF DISPLAY FOR JADE LEAD GLASS, USES YAMADA PROGRAM LGBTOF
C--     14.05.80       LAST CHANGE 28.05.82          J.OLSSON
C---
      IMPLICIT INTEGER*2 (H)
      LOGICAL FL18,FL22,FL24
      COMMON /CHEADR/ HEAD(108)
      COMMON /CWORK1/ HMW(112)
      COMMON/TFPED/ HADC(2,42),HTDC(2,42),HSTAT(42),HON(42)
      COMMON/CPROJ/XMINR,XMAXR,YMINR,YMAXR,IPRJC,FL18,FL22,FL24
      COMMON/CWORK/NR,RAW(5,42),NC,ICRT1(5,42),NTRK,ICRT2(50),TRK(5,50)
     - ,ITRC(50),NTC,ITRK(5,42),INFM(4),IR(14,50)
      DIMENSION RE(14,50),HTYP(4,4)
      EQUIVALENCE (RE(1,1),IR(1,1))
#include "cgraph.for"
#include "cdata.for"
      DATA HTYP /'  ','PR','OT','ON','  ','  ','KA','ON',
     $           '  ','  ','PI','ON','EL','EC','TR','ON'/
      DATA HBLANK /'  '/
C----------------------------------------
      NN = ACMD
      IF(NN.EQ.-3) GO TO 2110
      IPPATR = IDATA(IBLN('PATR'))
      IF(IPPATR.GT.0) GO TO 98
      WRITE(6,97)
97    FORMAT(' ERROR ----->>   PATR BANK DOES NOT EXIST')
      RETURN
98    IPATOF = IDATA(IBLN('ATOF'))
      IF(IPATOF.GT.0) GO TO 96
      WRITE(6,95)
95    FORMAT(' ERROR ----->>   ATOF BANK DOES NOT EXIST')
96    CONTINUE
      DO 107  I = 1,50
      DO 107  J = 1,14
107   IR(J,I) = 0
C---   CALL TOF PROGRAM HERE
      NRUN = HEAD(18)
      CALL TOFINT(NRUN,IPATOF,IPPATR,*1000)
      IF(NN.GE.0) GO TO 99
      IF(NN.EQ.-1) GO TO 91
C SHOW DIAGRAM BETA VS MOMENTUM
      CALL TOFVW
      RETURN
91    CALL TOFBK
      RETURN
2110  IF(HEAD(18).GT.10000) GO TO 2111
      CALL TRMOUT(80,'LEAD GLASS TOF NOT AVAILABLE FOR RUNS < 10000^')
      RETURN
2111  CALL LGBTDC(BLGTOF,LGFLA)
      IF(LGFLA.LT.2) GO TO 2112
      WRITE(6,4415) BLGTOF
4415  FORMAT(' WARNING **** ERROR FLAG >= 2, LG TOF VALUE ',E12.4)
      RETURN
2112  IF(LGFLA.LT.1) GO TO 2113
      WRITE(6,4416) BLGTOF
4416  FORMAT(' WARNING * LAST CALIBRATION RUN EXCEEDED, LGTOF = ',F8.2)
      RETURN
2113  WRITE(6,4417) BLGTOF
4417  FORMAT(' ERROR CODE = 0,  LGTOF = ',F8.2)
      RETURN
C---
99    CONTINUE
C--   LOOP OVER TRACKS

      WRITE(6,1381)
 1381 FORMAT(' TR.IFL.CN..MOM...TIME..BETA..DBET..BETM.BETP..ZTOF
     *.TDCM..TDCP..ADCM..ADCP'/)
      DO 109  ITR = 1,NTRK
      IF(NN.GT.0.AND.NN.NE.LTR) GO TO 111
      WRITE(6,1379) (IR(I,ITR),I=1,3),TRK(4,ITR),(RE(I,ITR),I=4,9),
     & TRK(1,ITR),HTDC(1,IR(3,ITR)),HTDC(2,IR(3,ITR)),
     & HADC(1,IR(3,ITR)),HADC(2,IR(3,ITR))
1379  FORMAT(' ',3I3,F6.3,F6.1,F6.0,4F6.2,F6.0,4I5)
  109 CONTINUE
C
      WRITE(6,1382)
 1382 FORMAT(' TR..PARTYP...PROT...PKAON...PPION..PELCT'/)
      DO 111  ITR = 1,NTRK
      IF(NN.GT.0.AND.NN.NE.ITR) GO TO 111
C DECIDE WHICH CHISQUARE IS SMALLEST, TO DECIDE PARTICLE TYPE
      CHIQ = 1.E06
      IPARTY = 0
      IF(IR(2,ITR).NE.1.AND.IR(2,ITR).NE.2) GO TO 712
      DO 711  I = 1,4
      IF(RE(9+I,ITR).GT.CHIQ) GO TO 711
      IPARTY = I
      CHIQ = RE(9+I,ITR)
711   CONTINUE
 712  CONTINUE
      DO 112  I = 1,4
      IF(IPARTY.LT.1.OR.IPARTY.GT.4) HMW(I) = HBLANK
      IF(IPARTY.LT.1.OR.IPARTY.GT.4) GO TO 112
      HMW(I) = HTYP(I,IPARTY)
 112   CONTINUE
      WRITE(6,1377) ITR,(HMW(I),I=1,4),
     $ (RE(I,ITR),I=10,13)
1377  FORMAT(' ',I2,1X,4A2,4F10.3)
111   CONTINUE
      RETURN
1000  WRITE(6,1001)
1001  FORMAT(' ERROR RETURN 1 FORM TOFINT ****')
      RETURN
      END
