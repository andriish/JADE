C   20/12/83 801291924  MEMBER NAME  KLREAD   (JADEGS)      FORTRAN
C   20/12/83 801151310  MEMBER NAME  KLREAD   (S)           FORTRAN
C
C-----------------------------------------------------------------------
      SUBROUTINE KLREAD
C-----------------------------------------------------------------------
C
C
C                                        21. 9. 82      P. STEFFEN
C
C        HANDLES ALL RUN DEPENDENT CONSTANTS
C        ALSO THOSE SET IN KALIBR
C
C        CORRECTION: UPDATE /CALIBR/ FOR MC-DATA IF LBMC(IBANK)=1
C                                        12.11. 82      P. STEFFEN
C
C        HANDLING OF CELL DEPENDENT DRIFT VELOCITY
C                                        30. 8. 83      P. STEFFEN
C
C        HANDLING OF NEW DEDX CONSTANTS
C
C        USE COMMON /CMCCAL/ LBMC(16)  TO INDICATE IF
C        CALIBR. CONST. SHALL BE USED (LBMC(IBANK) = 1)
C                                        28.11. 83      P. STEFFEN
C
C        NEW PERIOD OF RUNS (8712 - 9999)
C        HANDLING OF ZCAL CONSTANTS
C                                        28. 11. 83      P. STEFFEN
C
C        READ CALIBRATION FILES FOR MONTE CARLO EVENTS ONLY IF AT LEAST
C        ONE LBMC IS SET NON-ZERO          15.12.83   J.OLSSON
C        ERROR IN PREVIOUS CHANGE, CAUSED ERROR IN MUON MC. NOW CORRECT.
C                                          20.12.83   J.OLSSON,C.BOWDERY
C
C        ADDED EXTERNAL JADEBD              9.01.84   C.BOWDERY
C
C        NEW PERIOD OF RUNS (15699 - 99999)
C                                          16.05.84   J.SPITZER
C
C        NUMBER OF RUN PERIODS CHANGED
C        FROM 16 TO 24                      4.10.84   J.SPITZER
C
C        LOAD VERTEX CHAMBER CALIBRATION CONSTANTS
C                                          08.10.84   J.HAGEMANN
C
C        CHANGE TO VERTEX CHAMBER CONSTANT DRVELO
C                                          24.10.84   J.HAGEMANN
C
C        CHANGE T0, VD, LORENTZ ANGLE FOR RUNS 16800...
C                                          19.12.85   P. STEFFEN
C        RUN DEPENDENT ABERR(1) (PROPAGATION TIME FOR PULSE ON WIRE)
C                                          27.10.86   J.SPITZER/J.OLSSON
C        ADD BANK IDJS FOR SPITZER CALIBRATION
C        T0 UPDATES FOR FADC RUNS
C                                        17.10. 86      SPITZER/ELSEN
C        NEW VD AND LOR ANGLE AND T0EFFC CALL
C        INSERTED
C                                        11.11. 86      SPITZER/ELSEN
C        NEW T0, VD, AND LOR ANGLE
C                                        20.11. 86      SPITZER/ELSEN
C        PREPARED TO HANDLE VTXC, VTXR
C        AND VTXB CALIBRATION BLOCKS
C                                        15.12. 87      ELSEN
C        COPY VTXC TO COMMON / CVCCAL /
C        BLOCKDATA BDVTXC ADDED
C                                        21.12. 87      J.H./C.K.
C        TAKE OUT BDVTXC
C                                        15.01. 88      ELSEN
C
C        2 FORMAT STATEMENTS IMPROVED
C                                        29.01. 88      C.B.
C
C-----------------------------------------------------------------------
C
      IMPLICIT INTEGER*2 (H)
C                            FORCE LOADING OF JADE BLOCK DATA
      EXTERNAL JADEBD
C
#include "cgraph.for"
#include "cdata.for"
#include "cjvtxc.for"
C
      COMMON/CWORK/HWORK(40)
C
#include "cgeo1.for"
      COMMON /CJTRIG/ PI,TWOPI
C
#include "cjdrch.for"
#include "cdsmax.for"
C
#include "mvccal.for"
C
C
      COMMON /CALIBR/ JPOINT(100),
     1   CMUCA(4185),CLGMA(2880),CTAGS( 192),CJTPL(4608),CJTAB(1344),
     2   CTOFC( 546),CLGST( 500),CDEDX(1841),CSPTG( 193),CRVTX(   6),
     3   CZCAL(11329),CTAGF( 32),CIDJS(12000),CVCALC(1084),
     4   CVCALR(8),CVCALB(4),CVCALF(1008)
         DIMENSION ACALIB(100),HCALIB(100),ICALIB(100)
         EQUIVALENCE(JPOINT(1),ACALIB(1),HCALIB(1),ICALIB(1))
C
C
C                            LG ANALYSIS PARAMETERS
C
      COMMON /CLGPRM/ ITH,MAXCLS,IRLTHD,IRLTH2,IRLTH3, ZVRTX,DZVRTX
     $               ,IDUMM(2),ICALFL
C
      COMMON /CMCCAL/ LBMC(16)
      DIMENSION NAME(18),LENG(18),MAXL(18)
C
      DATA NBANKS /17/
      DATA NAME/'MUCA','LGMA','TAGS','JTPL','JTAB',
     1          'TOFC','LGST','DEDX','SPTG','RVTX',
     2          'ZCAL','TAGF','IDJS','VTXC','VTXR',
     3          'VTXB','VTXF','    ' /
      DATA LENG /2,2,2,2,4,  4,2,2,2,4,  4,2,4,4,4,  4,4,0 /
      DATA MAXL/  4185,  2880,   192, 4608,  1344,
     1             546,   500,  1841,  193,     6,
     2           11329,    32, 12000, 1084,     8,
     3               4,  1008,     0 /
C
C
      DATA LBLGST /0/
      DATA LBCONS /0/
C
C
      DIMENSION ITIME(6),LSTIME(10)
      DIMENSION IBUF(2001),JBUF(2000),IKDAT(1),HBUF(21),HKDAT( 10)
C
C
      EQUIVALENCE(HBUF(1),IBUF(1))
      EQUIVALENCE(HKDAT(1),IKDAT(1))
      EQUIVALENCE(HKDAT(1),HBUF(21))
      EQUIVALENCE(IBUF(2),JBUF(1))
C
      EQUIVALENCE(IBUF(1),LREC)
      EQUIVALENCE(IBUF(2),NAMEBN)
      EQUIVALENCE(IBUF(3),NUMREC)
      EQUIVALENCE(IBUF(4),NTOREC)
      EQUIVALENCE(IBUF(5),KTMEAS)
      EQUIVALENCE(IBUF(6),KTWRIT)
      EQUIVALENCE(IBUF(7),NEXTTM)
      EQUIVALENCE(IBUF(8),IFADDR)
      EQUIVALENCE(IBUF(9),ISPAR1)
      EQUIVALENCE(IBUF(10),ISPAR2)
C
#include "ciouni.for"
C
C                                       RUN DEPENDENT CONSTANTS
C
      DIMENSION IRUNAR(24),TZ0AR (24,3),VDRAR (24,3),
     ,                     ALORAR(24)  ,AMAGAR(24)
      DATA NRUNAR /24/
      DATA IRUNAR /      1497, 1967, 2521, 3730, 6185, 7592, 8712,
     ,            10000,10267,11038,11473,12555,13000,14600,15699,
     ,            16800,20275,24200,99999,    0,    0,    0,    0,    0/
      DATA TZ0AR  /-3.9,  2.4,   .2, -2.5, -6.1, 2.00, 2.20, 2.20,
     ,             3.60, 1.60, 2.20, 2.20, 1.80, 1.20, 1.25, 1.25,
     ,             1.20, 1.20, 3.00,   0.,   0.,   0.,   0.,   0.,
     ,             -3.9,  2.4,   .2, -2.5, -6.1, 3.10, 3.30, 3.30,
     ,             3.60, 1.60, 2.20, 2.20, 1.80, 2.30, 2.35, 2.35,
     ,             2.30, 2.30, 3.90,   0.,   0.,   0.,   0.,   0.,
     ,             -3.0,  2.4,   .2, -2.5, -6.1, 1.60, 1.80, 1.80,
     ,             3.60, 1.60, 3.00, 3.00, 2.50, 2.00, 2.05, 2.05,
     ,             2.00, 2.00, 4.20,   0.,   0.,   0.,   0.,   0./
      DATA VDRAR  /                     5*.3769,.3742,.3768,.3770,
     ,            .3778,.3778,.3758,.3758,.3893,.3767,.3780,.3780,
     ,            .3781,.3781,.3787,   0.,   0.,   0.,   0.,   0.,
     ,                                  5*.3753,.3746,.3783,.3785,
     ,            .3789,.3789,.3768,.3767,.3902,.3775,.3788,.3788,
     ,            .3792,.3792,.3808,   0.,   0.,   0.,   0.,   0.,
     ,                                  5*.3826,.3780,.3817,.3819,
     ,            .3821,.3821,.3805,.3810,.3946,.3814,.3827,.3827,
     ,            .3833,.3833,.3786,   0.,   0.,   0.,   0.,   0./
      DATA ALORAR /          3*18.5, 19.5,21.00,18.41,18.40,18.27,
     ,            18.6 ,18.6 ,19.50,19.40,18.93,18.90,18.90,18.90,
     ,            19.05,19.05,19.54,   0.,   0.,   0.,   0.,   0./
      DATA AMAGAR /                4*-4.50,
     ,                            12*-4.82,8*-4.82/
      DATA VDR01,VDR02,VDR03 /3*99999./,
     ,     DRID0 /99999./, IDRUN0 /99999/
C
C
C                                           EXTERNAL NEEDED TO LOAD
C                                           DL300 SERVICE FUNCTIONS
      EXTERNAL CL2PRC
C
      DATA NHEAD / 9/
      DATA INTERV/0/
      DATA LASTIM/0/, LCONTM /2147483647/
      DATA ICALL/0/ ,ICALO/0/
C
C
C------------------  C O D E  ------------------------------------------
C
 2900 FORMAT(/' FIRST CALL TO KLREAD, EVENT DATE: ',6I5,
     *             ' TZERO:',3F6.2/)
 2901 FORMAT(/' KLREAD WARNING : NO "SPINNING" BLOCK DATA. LG RECAL ',
     *        'NOT POSSIBLE'/)
 2902 FORMAT(' KLREAD ERROR: EVENT TIME =',I12,'FOR EVENT',2I6)
 2903 FORMAT(' KLREAD ERROR CODE',I4,' FORCE NORMAL END OF PROGRAM.')
C
C
C2001 FORMAT(' HCALIB: I0, I9 =',2I8,(/,10(/,1X,10F8.3)))
C2002 FORMAT(' READ0 LUNIT =',I3)
C2003 FORMAT(' READ1 LUNIT =',I3)
 2004 FORMAT(' IB,IP,LB,IFADDR =',10I8)
 2005 FORMAT(' LH,LR,LH,IP(B+1)=',10I8)
C2006 FORMAT('0KALIBR:',10F10.5)
C2007 FORMAT('0KALIBR',/,(1X,12F8.5))
C2008 FORMAT(' HCALIB: I0, I9 =',3I6,(/,10(/,1X,  20I6)))
C2009 FORMAT(' HCALIB: I0, I9 =',3I6,(/,10(/,1X,10F8.3)))
C2011 FORMAT('0CORR :',5X,I10,1X,A4,8I10,(10(/,1X, 10I12),/,3X))
C2012 FORMAT('0CORR :',5X,I10,1X,A4,8I10,(10(/,1X,  20I6),/,3X))
C
C
C---                                    INITIALIZE RETURN LABEL
      KALRET=0
C
      ICALO = ICALO + 1
C                                       GET RUN #
      IPHEAD=IDATA(IBLN('HEAD'))
      IHHEAD=IPHEAD*2
      NRUN=HDATA(IHHEAD+10)
C
C                                      INITIALIZ. AT 1. CALL
      IF(ICALL.NE.0) GOTO 1
C
C CHECK IF MONTE CARLO EVENTS, WITH ALL LBMC SET TO ZERO. IN THIS CASE,
C     SKIP THE FURTHER READING OF THE FILES.
C
      ILBMCS = 0
      DO 1550  ILB = 1, NBANKS
1550  ILBMCS = ILBMCS+LBMC(ILB)
C     IF(ILBMCS.EQ.0.AND.NRUN.LE.100) GO TO 1
C---
C---     FIRST CALL ONLY. SET UP POINTERS TO BANKS WITHIN THE CALIBRA-
C---     TION COMMON. UNIT=1 BYTE FOR CONVENIENCE OF THE MVCL ROUTINE.
C---     ALSO FILL LIST OF TIMES FOR WHICH THE STARTING VALUES FILES
C---     SHOULD BE READ, CHECK THAT THESE TIMES COME IN ASCENDING ORDER,
C---     AND BE SURE THAT FOR EVERY STARTING VALUES FILE THERE IS AN
C---     UPDATES FILE WITH THE SAME STARTING TIME.
C---
         ICALL=1
         CALL SETSL(ACALIB(1),0,400,0)
         ICALIB(1)=100
         DO 2 I=1,NBANKS
            ICALIB(I+1) = ICALIB(I) + MAXL(I)
C           WRITE(6,9501) I, ICALIB(I), MAXL(I), ICALIB(I+1)
C9501       FORMAT(1X,4I10)
    2    CONTINUE
C---
C---     CLEAR ALL BANKS EXCEPT MUCA.
C---
         ISTRT = ICALIB(2) + 1
         LZERO = (ICALIB(NBANKS+1) - ICALIB(2)) * 4
         CALL SETSL(ACALIB(ISTRT),0,LZERO,0)
C
C                                       INITIALIZE LG-CAL. FLAG
         ICALFL = 0
C                                       RETURN IF MC-DATA
C
C
         IF(NCALI.LT.1) GOTO 301
         IF(NCALI.GT.10) GOTO 301
         LAST    = 2000000001
C---
C---     "NCALI" IS THE NUMBER OF PAIRS OF STARTING VALUES AND UPDATES
C---     FILES. "KUNITA" AND "LUNITA" ARE LISTS OF FORTRAN UNIT NUMBERS
C---     FOR THE STARTING AND UPDATE FILES RESPECTIVELY.
C---
         ITP = NCALI
         DO 30 I=1,NCALI
            LUNIT=LUNITA(ITP)
C     PRINT 2002, LUNIT
            READ(LUNIT,ERR=304,END=305) NDUUM,ITM1
            REWIND LUNIT
C                                       CHECK IF ONLY ONE FILE
C                                       DATE(LAST FILE) = 1. DATE ?
            IF(I.EQ.1 .AND. ITM1.LE. 14688000) GOTO 25
            IF(ITM1.GE.LAST) GOTO 307
            LAST=ITM1
            LSTIME(ITP)=ITM1
         ITP = ITP - 1
   30    CONTINUE
         GOTO 1
C                                       ONLY ONE CALIBRATION FILE
   25 CONTINUE
         NCALI = 1
         LSTIME(1) = ITM1
         LUNITA(1) = LUNIT
C
C
C
    1 CONTINUE
      IF(ILBMCS.EQ.0.AND.NRUN.LE.100) GO TO 10
C
C                                       INITIALIZE LABEL FOR NEW JTAB
      LBJTAB = 0
C
C---
C---     NOW BEGIN CODING WHICH IS EXECUTED EVERY CALL. GET THE TIME
C---     AT WHICH THE CURRENT EVENT WAS RECORDED.
C---
      ITIME(1)=HDATA(IHHEAD+3)
      ITIME(2)=HDATA(IHHEAD+4)
      ITIME(3)=HDATA(IHHEAD+5)
      ITIME(4)=HDATA(IHHEAD+6)
      ITIME(5)=HDATA(IHHEAD+7)
      ITIME(6)=HDATA(IHHEAD+8)
C     IF(ICALO.EQ.1) PRINT 8756
C8756 FORMAT(' XV ENLARGED BY .1')
      IEVTIM=KTMCON(ITIME,NOW,LASTIM)
C
C                          CHECK IF EVENT TIME IN RANGE OF CONSTANTS
      IF(IEVTIM.LT.LSTIME(1)) GOTO 9001
C
C                          CHECK IF CURRENT CONSTANTS STILL VALID
      IF(IEVTIM.LT.NEXTTM .AND. IEVTIM.GE.LCONTM) GOTO 10
C
C
   31 CONTINUE
C---
C---  IF TIME HAS MOVED BACKWARDS SINCE THE LAST EVENT, RESET THE
C---  INTERVAL INDEX TO ZERO TO FORCE READING (OR REREADING) OF THE
C---  APPROPRIATE STARTING VALUES FILE.
C---
      IF(IEVTIM.LT.LASTIM) INTERV=0
      LASTIM=IEVTIM
C---
C---  IN WHICH TIME INTERVAL ARE WE, I.E. WHICH STARTING VALUES
C---  FILE IS APPLICABLE?
C---
      NINTER=1
      IF(NCALI.LT.2) GOTO 5
         DO 4 I=2,NCALI
            IF(LSTIME(I).GT.IEVTIM) GOTO 5
            NINTER=I
    4    CONTINUE
    5 CONTINUE
      IF(NINTER.EQ.INTERV) GOTO 6
C                                       REWIND OLD UNIT
         REWIND LUNIT
C---
C---     WE HAVE MOVED INTO A NEW INTERVAL OR MOVED BACKWARD IN TIME
C---     SINCE THE LAST EVENT, OR THIS IS THE FIRST CALL.
C---
         INTERV=NINTER
         LUNIT=LUNITA(NINTER)
C     PRINT 2003, LUNIT
         REWIND LUNIT
         READ(LUNIT,ERR=320,END=321) NDUUM
         IREAD=1
    6 CONTINUE
C---
C---     NOW READ FIRST RECORD OF THE CURRENT UPDATES FILE AND CHECK
C---     WHEN THE FIRST UPDATE IS DUE.
C---
      IF(IREAD.EQ.1) CALL EVREA1(LUNIT,NWORDS,JBUF,IRET)
      IREAD=0
      IBUF(1)=NWORDS
      IF(IRET.EQ.1) GOTO 310
      IF(IRET.EQ.2) GOTO 311
    8 CONTINUE
      IF(IEVTIM.LT.NEXTTM) GOTO 10
C---
C---     IT IS TIME FOR AN UPDATE.
C---
      IREAD=1
      DO 11 I=1,NBANKS
         IBANK=I
         IF(NAMEBN.EQ.NAME(I)) GOTO 12
   11 CONTINUE
C---
C---     CURRENT BANK NAME NOT FOUND IN OUR LIST. THIS IS AN ALLOWABLE
C---     CONDITION.
C---
      GOTO 6
   12 CONTINUE
C****** TEMPORARY CHANGE TO COPE WITH KALWRK0 ERROR
C     IF(IBANK.NE.1 .OR. JBUF(8).NE.7298) GOTO 70
C     NOUT = IBUF(1)
C     IF(LENG(IBANK).EQ.2) NOUT = NOUT * 2
C     IF(LENG(IBANK).EQ.2) PRINT 2012,  (IBUF(I1),I1= 1,10),
C    ,                                         (HBUF(I2),I2=19,NOUT)
C     IF(LENG(IBANK).EQ.4) PRINT 2011,  (IBUF(I1),I1= 1,NOUT)
   70 CONTINUE
C---
C---     MARK EXISTANCE OF LGST-BANK
      IF(IBANK.EQ.7) LBLGST = 1
      IF(IBANK.EQ.7) ICALFL = 1
C---
C---     MARK EXISTANCE OF NEW JTAB-BANK
      IF(IBANK.EQ.5) LBJTAB = 1
C
C
C                              FOR MC-DATA: UPDATE ONLY LABELD BANKS
      IF(NRUN.LE.100 .AND. LBMC(IBANK).EQ.0) GOTO 6
C
C
C                                       KEEP DATE OF CURRENT CONSTANTS
      LCONTM = NEXTTM
C---
C---     CHECK TYPE OF UPDATE, LIST OF VALUES FOR CONTIGUOUS ADDRESSES
C---     OF ADDRESS, NEW VALUE, ADDRESS, NEW VALUE ETC.
C---
      IF(IFADDR.EQ.0) GOTO 18
C---
C---     WE HAVE A LIST OF REPLACEMENT VALUES FOR CONTIGUOUS ADDRESSES.
C---
      IPNTR = ICALIB(IBANK)*4 + (IFADDR-1)*LENG(IBANK)
      LENDH = (LREC-NHEAD) * 4
      IPMAX = IPNTR + LENDH
      IF(IPMAX .GT. ICALIB(IBANK+1)*4) GOTO 315
C     IF( IBANK .GT. 13 )
C    *PRINT 2004, IBANK,ICALIB(IBANK),LENG(IBANK),IFADDR
C     IF( IBANK .GT. 13 )
C    *PRINT 2005, LENDH,LREC,NHEAD,ICALIB(IBANK+1)
      CALL MVCL(ACALIB,IPNTR,HKDAT,0,LENDH)
C IF VTXC THEN COPY TO CVCCAL
      IF (IBANK.EQ.14)
     &  CALL MVCL( VCDATE, 0, ACALIB, IPNTR, 4336 )
      GOTO 7
C
C---
C---     WE HAVE AN ADDRESS, NEW VALUE, ADDRESS NEW VALUE REPLACEMENT.
C---
   18 CONTINUE
C     IF(IBANK.NE.6 .OR. IFADDR.NE.0) GOTO 6
C     I0 = ICALIB(6) + 1
C     I9 = ICALIB(7)
C     PRINT 2001, I0,I9,(ICALIB(I1),I1=I0,I9)
      IF(LENG(IBANK).EQ.4) GOTO 14
        IPNTR = ICALIB(IBANK)*2
        LOOP  = (LREC-NHEAD)*2
        IPMAX = ICALIB(IBANK+1)*2 - IPNTR
C       PRINT 2004, IBANK,IPNTR,LOOP,IPMAX
        DO 15 I=1,LOOP,2
          IP = HKDAT(I)
          IF(IP.LT.0) IP = IP + 65536
          IF(IP.GT.IPMAX) GOTO 316
C         IF(I.LT.20) PRINT 2004, IP,IPNTR,HKDAT(I+1)
          HCALIB(IPNTR+IP) = HKDAT(I+1)
   15   CONTINUE
      GOTO 7
   14 CONTINUE
        IPNTR = ICALIB(IBANK)
        LOOP  = LREC - NHEAD
        IPMAX = ICALIB(IBANK+1) - IPNTR
C       PRINT 2004, IBANK,IPNTR,LOOP,IPMAX
        DO 16 I=1,LOOP,2
          IP = IKDAT(I)
          IF(IP.LT.1 .OR. IP.GT.IPMAX) GOTO 317
C         IF(I.LT.20) PRINT 2004, IP,IPNTR,IKDAT(I+1)
          ICALIB(IPNTR+IP) = IKDAT(I+1)
   16   CONTINUE
C
    7 CONTINUE
C     IF(IBANK.NE.6 .OR. IFADDR.NE.0) GOTO 6
C     I0 = ICALIB(6)  + 1
C     I9 = ICALIB(7)
C     PRINT 2001, I0,I9,(ICALIB(I1),I1=I0,I9)
      GOTO 6
C
C
C     ???????????????????    ERROR RETURNS   ?????????????????????
C
  301 CONTINUE
      KALRET=1
      GOTO 400
  304 CONTINUE
      KALRET=4
      GOTO 400
  305 CONTINUE
      KALRET=5
      GOTO 400
  307 CONTINUE
      KALRET=7
      GOTO 400
  310 CONTINUE
      KALRET=10
      GOTO 400
  311 CONTINUE
C     KALRET=11
C     GOTO 400
C                      *******************  EOF ON CALIBR. FILE
C                      *******************  GENERATE DUMMY RECORD
C                      *******************  WITH YEAR = 2000
         NWORDS  = 9
         JBUF(1) = NAME(2)
         JBUF(2) = 1
         JBUF(3) = 1
         JBUF(4) = 0
         JBUF(5) = NOW
         JBUF(6) = 2000000000
         JBUF(7) = 0
         JBUF(8) = 2000000000
         JBUF(9) = 0
      GOTO 8
  315 CONTINUE
      PRINT 2004, IPMAX,ICALIB(IBANK+1),LENDH,LREC,NHEAD
      PRINT 2004, IBANK,ICALIB(IBANK),LENG(IBANK),IFADDR
      PRINT 2005, LENDH,LREC,NHEAD,ICALIB(IBANK+1)
      KALRET=15
      GOTO 400
  316 CONTINUE
      KALRET=16
      GOTO 400
  317 CONTINUE
      KALRET=17
      GOTO 400
  320 CONTINUE
      KALRET=20
      GOTO 400
  321 CONTINUE
      KALRET=21
  400 CONTINUE
      PRINT 2903, KALRET
      STOP
C
C
C
   10 CONTINUE
C
C
C
      IF(NRUN.GT.100) GOTO 200
C                                       SET RUN DEPENDENT CONSTANTS
  299 CONTINUE
C
C
C     WARNING PRINTOUT IF NO LGST-BANK
      IF(LBLGST.EQ.0) PRINT 2901
      LBLGST = 2
C
C
C     DO 510 IB=1,NBANKS
C     IF(LENG(IB).EQ.4) GOTO 504
C        I0 = ICALIB(IB  ) * 2 + 1
C        I9 = ICALIB(IB+1) * 2
C        PRINT 2008, IB,I0,I9,(HCALIB(I1),I1=I0,I9)
C        GOTO 510
C 504 CONTINUE
C        I0 = ICALIB(IB  )  + 1
C        I9 = ICALIB(IB+1)
C        PRINT 2009, IB,I0,I9,(ICALIB(I1),I1=I0,I9)
C 510 CONTINUE
C
      IF(ICALO.EQ.1) PRINT 2900, (ITIME(I),I=1,6),TZERO
C
      RETURN
C
C
C                                       SET RUN DEPENDENT CONSTANTS
C
  200 CONTINUE
C
C                                       MAGNETIC FIELD
      BKGAUS = .001*HDATA(IHHEAD+30)
      IF(BKGAUS.GT.0.) BKGAUS =-BKGAUS
      MAGON = 1
      IF(ABS(BKGAUS).LT.1.) MAGON = 0
C
C                                       FIND RUN PERIOD
      DO 210 IDRUN=1,NRUNAR
         IF(NRUN.LT.IRUNAR(IDRUN)) GOTO 211
  210 CONTINUE
  211 CONTINUE
C
C                                       CHECK IF NEW CONSTANTS
      IF(IDRUN.EQ.IDRUN0 .AND. LBJTAB.EQ.0) GOTO 270
      IDRUN0 = IDRUN
C
C                                       TIME OFFSET
      TZERO(1) = TZ0AR(IDRUN,1)
      TZERO(2) = TZ0AR(IDRUN,2)
      TZERO(3) = TZ0AR(IDRUN,3)
C
C                                       DRIFT VELOCITY
      RNGTM1   = VDRAR(IDRUN,1)/64.
      RNGTM2   = VDRAR(IDRUN,2)/64.
      RNGTM3   = VDRAR(IDRUN,3)/64.
         VDR01 = RNGTM1
         VDR02 = RNGTM2
         VDR03 = RNGTM3
C
         VD1 = 0.
         VD2 = 0.
         VD3 = 0.
         IPVD = ICALIB(5) + 1248
         DO 240 K=1,24
            IPVD = IPVD + 1
            DRIVEL(K   ,1)=RNGTM1 * (1. + ACALIB(IPVD   ))
            DRIVEL(K   ,2)=RNGTM1 * (1. + ACALIB(IPVD   ))
            DRIVEL(K+24,1)=RNGTM2 * (1. + ACALIB(IPVD+24))
            DRIVEL(K+24,2)=RNGTM2 * (1. + ACALIB(IPVD+24))
            DRIVEL(K+48,1)=RNGTM3 * (1. + ACALIB(IPVD+48))
            DRIVEL(K+48,2)=RNGTM3 * (1. + ACALIB(IPVD+48))
            DRIVEL(K+72,1)=RNGTM3 * (1. + ACALIB(IPVD+72))
            DRIVEL(K+72,2)=RNGTM3 * (1. + ACALIB(IPVD+72))
            VD1 = VD1 + DRIVEL(K   ,1)
            VD2 = VD2 + DRIVEL(K+24,1)
            VD3 = VD3 + DRIVEL(K+48,1) + DRIVEL(K+72,1)
  240    CONTINUE
         VD1 = VD1 / 24.
         VD2 = VD2 / 24.
         VD3 = VD3 / 48.
         TIMDEL(1,1) = VD1
         TIMDEL(2,1) = VD1
         TIMDEL(1,2) = VD2
         TIMDEL(2,2) = VD2
         TIMDEL(1,3) = VD3
         TIMDEL(2,3) = VD3
         VD1 = VD1 * 64.
         VD2 = VD2 * 64.
         VD3 = VD3 * 64.
C     PRINT 2023, VD1,VD2,VD3,VD4,VD5,(VDRAR(IDRUN,I1),I1=1,3)
C2023 FORMAT('0DRIFT VELOCITIES OF RINGS:',3F8.5,3X,2F8.5,6X,3F8.5)
C2024 FORMAT('0LORENTZ ANGLES   OF RINGS:',3F8.5,3X,2F8.5,6X,3F8.5)
C
C
C                                       CHECK IF CHANGE OF LORENTZ ANGLE
  270 CONTINUE
C
C       CORRECT ONLINE T0 CHANGES AT BEGINNING OF '86     J.S. 4/11/86
      IF(NRUN.LT.24200.OR.NRUN.GT.24698) GOTO 7831
        AM7=T0EFFC( NRUN )
        TZERO(1) = TZ0AR(IDRUN,1)-AM7
        TZERO(2) = TZ0AR(IDRUN,2)-AM7
        TZERO(3) = TZ0AR(IDRUN,3)-AM7
7831  CONTINUE
C
C
        TWANG  = ALORAR(IDRUN) * PI/180.
        AKGAUS = AMAGAR(IDRUN)
        DRIDEV = 0.
        IF(MAGON.NE.0) DRIDEV =-TWANG
        DALF = 0.
        IF(ABS((BKGAUS-AKGAUS)/AKGAUS).GE..01)
     ,     DALF = ASIN(SIN(DRIDEV)*BKGAUS/AKGAUS) - DRIDEV
        DRIDEV = DRIDEV + DALF
        IF(DALF.EQ.0. .AND. DRIDEV.EQ.DRID0 .AND. LBJTAB.EQ.0)
     ,     GOTO 295
C          PRINT 2006, BKGAUS,AKGAUS,DRIDEV,TWANG,DRID0,DALF
           DRID0 = DRIDEV
C
           DRISIN = SIN(DRIDEV)
           DRICOS = COS(DRIDEV)
C          PRINT 2006, DRIDEV,DRISIN,DRICOS,TWANG,DALF
C
C          I0 = ICALIB(5)  + 1
C          I9 = 96*14 + I0 - 1
C          PRINT 2010, (ACALIB(I1),I1=I0,I9)
C2010 FORMAT('1JTAB :',(8(/,1X,12F8.5),/))
           IPCAL  = ICALIB(5)
           IPCAL  = IPCAL + 960
           SIGMAG = MAGON
C          RLOR1  = 0.
C          RLOR2  = 0.
C          RLOR3  = 0.
C          RLOR4  = 0.
           DO 280 J=1,96
C             ANGLR1 = (DRIDEV - ACALIB(IPCAL+J)) * SIGMAG
              ANGLR1 = DRIDEV * SIGMAG
C             RLOR3 = RLOR3 + ANGLR1
C             IF(J.LE.24) RLOR1 = RLOR1 + ANGLR1
C             IF(J.LE.48) RLOR2 = RLOR2 + ANGLR1
C             IF(J.GT.48 .AND. AND(J,1).NE.0) RLOR4 = RLOR4 + ANGLR1
              SINLOR=SIN(ANGLR1)
              COSLOR=COS(ANGLR1)
              DRIROT(J,1)=ANGLR1
              SINDRI(J,1)=SINLOR
              COSDRI(J,1)=COSLOR
              DRIROT(J,2)=ANGLR1
              SINDRI(J,2)=SINLOR
              COSDRI(J,2)=COSLOR
  280      CONTINUE
C          RLOR3 = RLOR3 - RLOR2
C          RLOR2 = RLOR2 - RLOR1
C          RLOR5 = RLOR3 - RLOR4
C          RLOR1 = RLOR1 / 24.
C          RLOR2 = RLOR2 / 24.
C          RLOR3 = RLOR3 / 48.
C          RLOR4 = RLOR4 / 24.
C          RLOR5 = RLOR5 / 24.
C          PRINT 2024, RLOR1,RLOR2,RLOR3,RLOR4,RLOR5,DRIDEV
C
C RUN DEPENDENT ABERRATION CONSTANTS FOR INNER DETECTOR HIT CALCULATION
C
C
         ABERR(1) = 0.000217
         IF(NRUN.GE.24200) ABERR(1) = 0.
C
C  SET SOME CONSTANTS WHICH ARE NEVER CHANGED
C
      IF(LBCONS.NE.0) GOTO 295
         LBCONS = 1
         ZAL=2792.9
         FIRSTW(1)=206.50
         FIRSTW(2)=416.50
         FIRSTW(3)=627.83
         FSENSW(1)=211.50
         FSENSW(2)=421.50
         FSENSW(3)=632.83
C
         ABERR(2) = 4.0
         ABERR(3) = 6.8
         ABERR(5) = .048
         ABERR(6) = .000167
         ABERR(7) = 15.0
         ABERR(8) = 1.0
         IF(MAGON.EQ.0) ABERR(2) = 5.
         IF(MAGON.EQ.0) ABERR(3) = 5.
C
C        PRINT 2007,TIMDEL,DRIDEV,DRISIN,DRICOS
C        PRINT 2007,DRIVEL
C        PRINT 2007,DRIROT
C        PRINT 2007,SINDRI,COSDRI
C
C
  295 CONTINUE
C                            SET VERTEX CHAMBER CALIBRATION CONSTANTS
      ALRV = -0.345833
      SNLV = SIN(ALRV)
      CSLV = COS(ALRV)
C
      DO 700 I = 1, 24
        DRILOR(I) = ALRV
        SNLORA(I) = SNLV
        CSLORA(I) = CSLV
        DRVELO(I) = 0.0050
  700 CONTINUE
C
C     RESET LBJTAB
      LBJTAB = 0
      GOTO 299
C
C
C                    STOP ANALYSIS IF EVENT TIME TOO LOW
 9001 CONTINUE
      PRINT 2902, IEVTIM,NRUN,HDATA(IHHEAD+11)
      STOP 77
      END
      BLOCK DATA
C
C
      COMMON /CMCCAL/ LBMC(16)
      DATA LBMC /0,0,0,0,0,  0,0,0,0,0,  0,0,0,0,0,  0 /
C
      END
