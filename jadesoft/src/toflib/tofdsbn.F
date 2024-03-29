C   19/08/82 411121150  MEMBER NAME  TOFDSBN  (S)           FORTRAN
      SUBROUTINE TOFDS
C--
C--     TOF DISPLAY FOR JADE, USING TOF PROGRAM FROM B.NAROSKA,KAWABATA
C--     TOF DISPLAY FOR JADE LEAD GLASS, USES YAMADA PROGRAM LGBTOF
C--     14.05.80       LAST CHANGE 28.05.82          J.OLSSON
C--     14.05.80       LAST CHANGE 1.7.82          B.NAROSKA
C---
      IMPLICIT INTEGER*2 (H)
      LOGICAL FL18,FL22,FL24
      COMMON /CHEADR/ HEAD(108)
      COMMON /CWORK1/ HMW(112)
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
      CALL TOFINT(NRUN,IPATOF,IPPATR,&1000)
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
 1381 FORMAT(' TR.IFL.CN..MOM...TIME..PATH.TAUM1.TAUP1.IE..BETA..ZTOF..Z
     *TOFE.TDCL/R..ADCL/R'/)
      NCON = INFM(1)
      DO 111  ITR = 1,NCON
      LTR = IR(1,ITR)
      IF(NN.GT.0.AND.NN.NE.LTR) GO TO 111
      PP = TRK(4,LTR)
      IF(ABS(PP).LT.1.) PP = PP*1000.
      WRITE(6,1379) (IR(I,ITR),I=1,3),PP,(RE(I,ITR),I=4,5),(RE(I,ITR),I=
     & 10,11),IR(12,ITR),(RE(I,ITR),I=13,14),TRK(1,LTR),
     & (RE(I,ITR),I=6,9)
1379  FORMAT(' ',3I3,F7.1,F5.1,F7.0,2F6.1,I3,F6.2,2F6.0,1X,4F4.0)
111   CONTINUE
C
      RETURN
1000  WRITE(6,1001)
1001  FORMAT(' ERROR RETURN 1 FORM TOFINT ****')
      RETURN
      END
