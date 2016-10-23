      SUBROUTINE CKTRKO(LBTRCK,LBCELL)
      IMPLICIT INTEGER*2 (H)
#include "cdata.for"
#include "ccycp.for"
#include "cworkpr.for"
#include "cworkeq.for"
      EQUIVALENCE
     ,          (HPCO0 ,HPWRK(17)),(HPCO9 ,HPWRK(18)),(HLDCO ,HPWRK(19))
     ,         ,(ICELL ,IDWRK( 1)),(MHIT  ,IDWRK( 2)),(IRING ,IDWRK( 3))
     ,         ,(IERRCD,IDWRK( 4)),(NTRKEL,IDWRK( 5))
#include "cpatlm.for"
#include "cjdrch.for"
#include "cdsmax.for"
      INTEGER MKLY(16) /1,2,4,8,16,32,64,128,256,512,1024,2048,
     ,                  Z1000,Z2000,Z4000,Z8000/
      INTEGER MKDDCL(3) /Z01,Z02,Z04/, LBDDCL /ZFFFF/
      INTEGER MKBDCL(3) /Z10,Z20,Z40/
      REAL RDLIM(5,2) / -2.0, -1.0, 1.0, 2.0, 8.5,
     ,                  -3.0, -2.0, 2.0, 3.0, 8.5/
      DATA INDLM /-1/
      DIMENSION  LBHT(15)
      LBTRCK = 0
      CALL SETS(LBHT(1),0,60,0)
      JRING0 = 0
      DO 13000 IP = HPCO0,HPCO9,HLDCO
        ILAY   = IWRK(IP   )
        JRING  = IWRK(IP+12)
        LBGOOD = IWRK(IP+ 7)
        DF     = WRK (IP+13)*IWRK(IP+2)
      IF(
     - JRING.NE.JRING0
     -)THEN
          JRING0 = JRING
          INDLM1 = 1
          IF(LAND(MKBDCL(JRING),LBCELL).NE.0) INDLM1 = 2
      IF(
     - INDLM1.NE.INDLM
     -)THEN
            INDLM = INDLM1
            RDLM1 = RDLIM(1,INDLM)
            RDLM2 = RDLIM(2,INDLM)
            RDLM3 = RDLIM(3,INDLM)
            RDLM4 = RDLIM(4,INDLM)
            RDLM5 = RDLIM(5,INDLM)
      ENDIF
      ENDIF
      IF(
     - LBGOOD .NE. 16
     -)THEN
          LBGOOD = 1
          IF(DF.LE. RDLM1 .OR.  DF.GE. RDLM5) LBGOOD = 5
          IF(DF.GE. RDLM1 .AND. DF.LT. RDLM2) LBGOOD = 2
          IF(DF.GE. RDLM3 .AND. DF.LT. RDLM4) LBGOOD = 3
          IF(DF.GE. RDLM4 .AND. DF.LT. RDLM5) LBGOOD = 4
          INDEX = JRING*5 - 5 + LBGOOD
          LBHT(INDEX) = LOR(LBHT(INDEX),MKLY(ILAY+1))
      ENDIF
13000 CONTINUE
13001 CONTINUE
      LMGAP = 3
      MGAP  = 0
      MGAP1 = 0
      MGAPM = 0
      MCON  = 0
      ILAYR = 0
      ILLST = 0
      NHGD  = 0
      LBOUND = 0
      JRING = 0
16000 CONTINUE
      JRING = JRING + 1
        INDEX = JRING*5 - 5
        NHGC  = 0
        NHTRC = 0
        LB1   = LBHT(INDEX + 1)
      ASSIGN 17001 TO IZZZ01
      GOTO 17000
17001 CONTINUE
        LB2   = LBHT(INDEX + 2)
        LB3   = LBHT(INDEX + 3)
        LB4   = LBHT(INDEX + 4)
        IF(LAND(MKDDCL(JRING),LBCELL).NE.0) LB4 = LBDDCL
        LB5   = LBHT(INDEX + 5)
        LBCV  = LOR(  LB3,  LB4)
      IF(
     - LBCV.NE.0
     -)THEN
      ASSIGN 17003 TO IZZZ02
      GOTO 17002
17003 CONTINUE
      ENDIF
        LBGC  = LOR( LBCV,  LB1)
        LBTRC = LOR( LBGC,  LB2)
        LBHIT = LOR(LBTRC,  LB4)
        IL = 0
15000 CONTINUE
      IF(
     - IL.LT.16
     -)THEN
        IL    = IL    + 1
        ILAYR = ILAYR + 1
          MKLY1 = MKLY(IL)
      IF(
     - LAND(LBGC,MKLY1).EQ.0
     -)THEN
      IF(
     - IL.GT.2 .AND. IL.LT.15
     -)THEN
              MGAP  = MGAP  + 1
              MGAP1 = MGAP1+ 1
              MCON  = 0
      ENDIF
      ELSE
            NHGC  = NHGC + 1
            MCON  = MCON + 1
      IF(
     - MCON.GE.2 .OR. MGAP1.EQ.1
     -)THEN
              MGAP  = 0
              MGAPM = 0
              ILLST = ILAYR
              IF(MCON.EQ.4) LBOUND = 0
      ENDIF
            MGAPM = MAX0(MGAPM,MGAP1)
            MGAP1 = 0
      ENDIF
          IF(LAND(LBTRC,MKLY1).NE.0) NHTRC = NHTRC + 1
          IF(LAND(LB1  ,MKLY1).NE.0) NHGD  = NHGD  + 1
      IF(
     - MGAPM.GE.4 .OR. MGAP.GE.6
     -)THEN
            LBACC = 0
            IF(LAND(MKBDCL(JRING),LBCELL).NE.0 .AND. MGAPM.LE.6
     ?         .AND. MGAP.LE.7) LBACC = 1
      IF(
     - LBACC.NE.0
     -)THEN
              LBOUND = 1
              ILLSTB = ILLST
      ELSE
              LBTRCK = ILLST
      GOTO 16001
      ENDIF
      ENDIF
      GOTO 15000
      ENDIF
15001 CONTINUE
      IF(.NOT.(
     - JRING.EQ.3
     -))GOTO 16000
16001 CONTINUE
      IF(LBTRCK.EQ. 0) LBTRCK = ILLST
      IF(LBOUND.NE. 0) LBTRCK = ILLSTB
      IF(NHGD  .LT. 8 .AND. LBTRCK.GT.0) LBTRCK =-LBTRCK
      RETURN
17000 CONTINUE
        LBGD2 = ISHFTL(LB1 ,1)
        MKGD1 = 7
        MKGD2 = 5
        MKGD3 = 2
      DO 13002 IL=1,16
          LBZW  = LAND(LBGD2,MKGD1)
      IF(
     - LBZW.EQ.MKGD2
     -)THEN
            LBGD2 = LOR(MKGD3,LBGD2)
      ENDIF
          MKGD1 = ISHFTL(MKGD1,1)
          MKGD2 = ISHFTL(MKGD2,1)
          MKGD3 = ISHFTL(MKGD3,1)
13002 CONTINUE
13003 CONTINUE
        LBPR = ISHFTR(LBGD2,1)
        LB1  = ISHFTR(LBGD2,1)
      GOTO IZZZ01
17002 CONTINUE
        LBCV2 = ISHFTL(LBCV,1)
        MKCV1 = 7
        MKCV2 = 2
      DO 13004 IL=1,16
          LBZW  = LAND(LBCV2,MKCV1)
      IF(
     - LBZW.EQ.MKCV2
     -)THEN
            LBCV2 = LAND(LCOMPL(MKCV2),LBCV2)
      ENDIF
          MKCV1 = ISHFTL(MKCV1,1)
          MKCV2 = ISHFTL(MKCV2,1)
13004 CONTINUE
13005 CONTINUE
        LBCV = ISHFTR(LBCV2,1)
      GOTO IZZZ02
      END
