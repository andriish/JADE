C   08/12/80 611261026  MEMBER NAME  JETCAL   (JADEGS)      FORTRAN
C   19/08/80 009041235  MEMBER NAME  JETCAL9  (JADESR)      FORTRAN
      SUBROUTINE JETCAL
C---
C--- **************************************************************
C--- ******************* VERSION FOR NOZAKIS NEW JET CONSTANTS ****
C--- **************************************************************
C---     CREATES CALIBRATED JET CHAMBER BANK.
C---
C---                                    WEDNESDAY, MAY 23, 1979
C---                                    L.H. O'NEILL
C---
C---
C---     NEW VERSION FOR FADC-READOUT    P. STEFFEN 12.30 86/02/05
C---
C---     SET AMPLITUDES = 0 IF OVERFLOW  P. STEFFEN 12.30 83/04/21
C---
C---     NEW VERSION WITH A TZERO FOR EACH OF THE THREE RINGS.
C---                                   L.H. O'NEILL 19.30 13.08.79
C---   CHANGED 14.9.80 TO ACCOUNT FOR NOZAKIS NEW CONSTANTS
C---                                                  J.OLSSON
C---
C---   CHANGED 05.11.86   TPED-->IPED  TO ACCOUNT FOR T0 EFFECTS
C---                                      E ELSEN
C---
C---
      IMPLICIT INTEGER*2 (H)
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
C-----------------------------------------------------------------------
C                            MACRO CGRAPH .... GRAPHICS COMMON
C-----------------------------------------------------------------------
C
      LOGICAL DSPDTL,SSTPS,PSTPS,FREEZE
C
      COMMON / CGRAPH / JUSCRN,NDDINN,NDDOUT,IDATSV(11),ICREC,MAXREC,
     +                  LSTCMD,ACMD,LASTVW,ISTANV,
     +                  SXIN,SXAX,SYIN,SYAX,XMIN,XMAX,YMIN,YMAX,
     +                  DSPDTL(30),SSTPS(10),PSTPS(10),FREEZE(30),
     +                  IREADM,LABEL,LSTPS(10),IPSVAR
C
C------- END OF MACRO CGRAPH -------------------------------------------
C
C------------------------------------------
C  MACRO CJDRCH .... JET CHAMBER CONSTANTS.
C------------------------------------------
      COMMON / CJDRCH / RDEC(4),PSIIN(3),RINCR(3),FIRSTW(3),FSENSW(3),
     * RDEPTH,SWDEPL,YSUSPN,TIMDEL(2,3),ZMAX,ZOFFS,ZRESOL,ZNORM,ZAL,
     * ZSCAL,DRIDEV,DRICOS,DRISIN,PEDES,TZERO(3),
     * DRIROT(96,2),SINDRI(96,2),COSDRI(96,2),DRIVEL(96,2),T0FIX(3)
C    PREVIOUS MACRO REPLACED WITH EXPLICIT COMMON STATEMENT, TO ACCOUNT
C    FOR INTRODUCTION OF ARRAY T0FIX.  WILL LATER BE INCLUDED IN MACRO
C CORRESPONDING CHANGE MADE IN SUPERV BLOCK DATA     J.OLSSON 14.9.80
C
C--------- END OF MACRO CJDRCH ------------
      COMMON/CALIBR/JPOINT(16)
      DIMENSION HCALIB(2)
      EQUIVALENCE (HCALIB(1),JPOINT(1))
      DATA ZPHS/2440./
      DATA RAMPS/100./
      DATA ICALL/0/
C---
C---                                    INITIALIZATION
      IF(ICALL.NE.0) GO TO 3
          ICALL=1
          IHEAD=IBLN('HEAD')
          IJETC=IBLN('JETC')
          AC0=0.5*(ZPHS-ZAL)/ZAL
          AC1=0.5*RAMPS*ZPHS/ZAL
    3 CONTINUE
C
C                                       POINTER
      IPHALF=2*JPOINT(4)
      IPHEA2=2*IDATA(IHEAD)
      NRUN=HDATA(IPHEA2+10)
      NEVEN=HDATA(IPHEA2+11)
      IPJET1=IDATA(IJETC)
C---
C---     RENUMBER "JETC 0" TO "JETC 1" AND CREATE "JETC 0".
C---
      NW=IDATA(IPJET1)
      NUMB=IDATA(IPJET1-2)
      CALL BRNM('JETC',NUMB,'JETC',1)
      CALL CCRE(IPJET0,'JETC',0,NW,IERR)
      IF(IERR.NE.0) GO TO 10
      CALL CLOC(IPJET1,'JETC',1)
C---
C---     NOW CALIBRATE "JETC 0".
C---
      IP0=4*IPJET0
      IP1=4*IPJET1
      CALL MVCL(IDATA,IP0,IDATA,IP1,200)
C---
C---     COPY BANK DESCRIPTOR, CELL POINTERS AND LIST OF WIRES WITH
C---     MORE THAN 8 HITS. THE LATTER LIST IS AT THE END OF THE BANK
C---
      NEND=2*(HDATA(2*IPJET0+100)-HDATA(2*IPJET0+99))
      IP2=IP0+4*NW-NEND
      IP3=IP1+4*NW-NEND
      IF(NEND.GT.0) CALL MVCL(IDATA,IP2,IDATA,IP3,NEND)
C---
C---     ADD 100 TO FIRST I*2 BANK DESCRIPTOR WORD FOR CALIBRATED BANK.
C---
      HDATA(2*IPJET0+1)=HDATA(2*IPJET0+1)+200
      NHITS=(HDATA(2*IPJET0+99)-HDATA(2*IPJET0+3))/4
      IF(NHITS.LT.1) RETURN
      J=2*IPJET1+100
      K=2*IPJET0+100
      KMAX=K+HDATA(K-1) - 1
C
C                                        LOOP OVER ALL HITS
    2 CONTINUE
C                                        CHECK IF END OF LOOP
      IF(K.GE.KMAX) GO TO 1
C
        HDATA(K+1) = HDATA(J+1)
        IWIRE      = HDATA(J+1)
        IA1        = HDATA(J+2)
        IA2        = HDATA(J+3)
        IDTIME     = HDATA(J+4)
C
C
        IF(IA1.LT.   0) IA1=0
        IF(IA1.GE.4095) IA1=0
        IA1 = ISHFTL(IA1,3)
        IF(IA2.LT.   0) IA2=0
        IF(IA2.GE.4095) IA2=0
        IA2=ISHFTL(IA2,3)
C
C                                       GET CONSTANTS
        IWIRE = ISHFTR(IWIRE,3)
        IPLOC = IPHALF + IWIRE*6
        ITZERO = HCALIB(IPLOC+1)
C------------------------------------------------------
C DIVIDE CHECK     J.OLSSON   8.9.86
C
        IF(HCALIB(IPLOC+2).NE.0) GO TO 115
        WRITE(6,116) NRUN,NEVEN,IWIRE,(HCALIB(IPLOC+JJ),JJ=1,6)
116    FORMAT(' JETCAL:##### 0 DIVIDE IMMINENT, RUN,EVENT IWIRE,HCALIB',
     $ 9I6)
C
C------------------------------------------------------
115     SF     = AC0 + AC1/HCALIB(IPLOC+2)
        IPEDL1 = HCALIB(IPLOC+3)
        IGAIN1 = HCALIB(IPLOC+4)
        IPEDL2 = HCALIB(IPLOC+5)
        IGAIN2 = HCALIB(IPLOC+6)

C---
C---       THE 512 (2**9) ADDED BELOW CAUSES THE SUBSEQUENT SHIFT RIGHT
C---       BY 10 BITS TO DO A ROUND OFF RATHER THAN A TRUNCATION.
C---
        IF(IA1.GT.0) IA1=IGAIN1*(IA1-IPEDL1)+512
        IF(IA1.LT.0) IA1=0
        IA1=ISHFTR(IA1,10)
        IF(IA2.GT.0) IA2=IGAIN2*(IA2-IPEDL2)+512
        IF(IA2.LT.0) IA2=0
        IA2=ISHFTR(IA2,10)
C---
C---       COMPUTE AMPLITUDE SLEWING CORRECTION AT THIS POINT.
C---
C---       NEW SLEWING CORRECTION INTRODUCED WITH NEW CONSTANTS,
C---       PARABOLIC CORRECTION, DIFFERENT FOR SMALL AND BIG AMPLITUDES;
C---                                                 J.OLSSON  14.9.80
C---       NO SLEWING CORRECTION FOR FADC READOUT    P.STEFFEN 29.1.86
C---
        IAMCOR=0
        IF(NRUN .GE. 24200) GOTO 290
C
C
          IAM=IA1
          IF(IA2.GT.IA1) IAM=IA2
          IF(NRUN.GE.6185) GO TO 203
C ----    ---- NEW   14.9.80
            IF(IAM.GT.2000) GO TO 200
              TAMCOR =-64.*1.494 + 7.872E-03*IAM*8. - 1.157E-05*IAM*IAM
              GO TO 201
200         IF(IAM.GT.4000) GO TO 202
              TAMCOR =-64.*0.8207 + 2.926E-03*IAM*8. - 2.561E-06*IAM*IAM
201           IAMCOR = TAMCOR - 0.5
202         CONTINUE
            GO TO 204
C ----    ---- END NEW   14.9.80
C ----    ---- NEW   25.3.81
203       CONTINUE
            IF(IAM.GT.1800) GO TO 205
              TAMCOR =-64.*1.811 + 1.112E-02*IAM*8. - 2.155E-05*IAM*IAM
              GO TO 206
205         IF(IAM.GT.5600) GO TO 204
              TAMCOR =-64.*0.8430 + 2.340E-03*IAM*8. - 1.623E-06*IAM*IAM
206           IAMCOR = TAMCOR - 0.5
C ----      ---- END NEW   25.3.81
204       CONTINUE
C
C
  290   CONTINUE
C
        AX=SF*(IA2-IA1)
        IF(AX.GT.0.) AX=AX+0.5
        IF(AX.LT.0.) AX=AX-0.5
        IS=AX
        IF(IA1.GT.0) IA1=IA1-IS
        IF(IA1.LT.    0) IA1=0
        IF(IA1.GT.32767) IA1=32767
        IF(IA2.GT.0) IA2=IA2+IS
        IF(IA2.LT.    0) IA2=0
        IF(IA2.GT.32767) IA2=32767
C
C                                         DRIFTTIME
        IF(NRUN .GE. 24200) GOTO 310
          IF(IDTIME.LT.0) IDTIME=0
          IF(IDTIME.GT.255) IDTIME=255
          IDTIME = ISHFTL(IDTIME,6)
  310   CONTINUE
        IDTIME = IDTIME - ITZERO + IAMCOR
        KLOK=1
        IF(IWIRE.GE.384) KLOK=2
        IF(IWIRE.GE.768) KLOK=3
C ----  ---- NEW   14.9.80
        IPED = INT(64.*(TZERO(KLOK) + T0FIX(KLOK)) + 10000.5) - 10000
C ----  ---- END NEW   14.9.80
C ----     TPED CHANGED INTO IPED ABOVE
C
C
        HDATA(K+2) = IA1
        HDATA(K+3) = IA2
        HDATA(K+4) = IDTIME - IPED
C
C
      J = J + 4
      K = K + 4
      GO TO 2
C
C
    1 CONTINUE
      RETURN
C
C                                       ERROR
   10 CONTINUE
      WRITE(JUSCRN,101) IERR
  101 FORMAT(' ERROR RETURN',I4,' FROM CCRE.')
      RETURN
      END
