C   01/11/84            MEMBER NAME  HODRFI   (S)           FORTRAN
C
C-----------------------------------------------------------------------
      SUBROUTINE HODRFI
C-----------------------------------------------------------------------
C
C    AUTHOR:   J. OLSSON        ?     :  DRAW HODOSCOPE HITS
C
C       MOD:   J. OLSSON     07/03/80 :  ?
C
C  LAST MOD:   J. HAGEMANN   10/10/84 :  NOW OWN MEMBER (FROM EVDISP)
C
C-----------------------------------------------------------------------
C
      IMPLICIT INTEGER*2 (H)
C
      LOGICAL TBIT,DSPDTM
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
C-----------------------------------------------------------------------
C                            MACRO CGEO1 .... JADE GEOMETRY
C-----------------------------------------------------------------------
C
      COMMON / CGEO1 / BKGAUS,
     +                 RPIP,DRPIP,XRLPIP,   RBPC,DRBPC,XRLBPC,
     +                 RITNK,DRITNK,XRLTKI, R0ROH,DR0ROH,XR0ROH,
     +                 R1ROH,DR1ROH,XR1ROH, R2ROH,DR2ROH,XR2ROH,
     +                 R3ROH,DR3ROH,XR3ROH, ROTNK,DROTNK,XRLTKO,
     +                 RTOF,DRTOF,XRTOF,    RCOIL,DRCOIL,XRCOIL,
     +                 ZJM,DZJM,XRZJM,ZJP,DZJP,XRZJP,ZTKM,DZTKM,XRZTKM,
     +                 ZTKP,DZTKP,XRZTKP,ZBPPL,ZBPMI,ZTOFPL,ZTOFMI,
     +                 XRJETC,RLG,ZLGPL,ZLGMI,OUTR2,CTLIMP,
     +                 CTLIMM,DELFI,BLXY,BLZ,BLDEP,ZENDPL,ZENDMI,DEPEND,
     +                 XHOL1,XHOL2,YHOL1,YHOL2,BLFI
C
C------------------------- END OF MACRO CGEO1 --------------------------
C
C
      COMMON / CGRAP2 / BCMD, DSPDTM(30)
      COMMON / CWORK1 / R,FI,R1,COSF,X1,Y1,R2,SINF,X2,Y2,ZET,X3,Y3,X4,Y400002400
     +                 ,HMW(132)
C
      COMMON / CJTRIG / PI, TWOPI
      COMMON / SMPTOF / MTOF, TTOF(42)
C
      DIMENSION HELP(2)
C
      EQUIVALENCE (ICAMWD,HELP(1))
C
      DATA HELP /0,0/, PIDEG /57.29578/
*** PMF 17/11/99: add variables needed for emulation of DESYLIB routine 'CORE'  
      CHARACTER cHMW*264
      EQUIVALENCE (cHMW,HMW(1))
*** PMF(end)
C
C-----------------  C O D E  -------------------------------------------
C
      IPJ = IDATA(IBLN('LATC'))
      IF(IPJ.LE.0) RETURN
      IF(IDATA(IPJ).LE.3) RETURN
      IPJ = 2*IPJ
      IPJF = IDATA(IBLN('ATOF'))
      RF = RTOF + 1.7*DRTOF
      IF(LASTVW.EQ.3) RF = RF + DRTOF*1.5
      SH3 = DRTOF*1.3
      IF(LASTVW.EQ.3) SH3 = SH3*1.5
      IHHEA2=2*IDATA(IBLN('HEAD'))
      NRUN=HDATA(IHHEA2+10)
      CALL TOFSMP(NRUN)
      SH2=DRTOF
      call setcol('TOFH') ! PMF 23/11/99: set colour
      DO 31 IH = 1,2
      IF(IH.NE.2) GO TO 41
C** TOF COUNTERS HERE
      R1 = RTOF
      R2 = DRTOF + R1
      DEFI = TWOPI/42.
      J1 = IPJ + 6
      J2 = IPJ + 11
      NBIT=7
      GO TO 42
   41 CONTINUE
C** BEAM PIPE COUNTERS HERE
      R1 = RBPC
      R2 = DRBPC + R1
      DEFI = TWOPI/24.
      J1 = IPJ + 3
      J2 = IPJ + 5
      NBIT=8
   42 CONTINUE
      DEPLC = - DEFI*.5
      DO 32 JK = J1,J2
      HELP(2) = HDATA(JK)
      ICAMWD = ISHFTL(ICAMWD,1)
      DO 32 IBT = 1,NBIT
      ICAMWD = ISHFTR(ICAMWD,1)
      IE = (JK-J1)*NBIT
      IF(.NOT.TBIT(ICAMWD,31)) GO TO 32
      IE = IE + IBT
C---
C---     DRAW TOF COUNTER NUMBERS OUTSIDE COUNTER RING.
C---
      IF(IH.NE.2) GO TO 33
      IF(DSPDTM(3)) GO TO 33
      FI=PI/2.-(IE-1)*DEFI
C     X4=RTOF-2.*SH2
C     IF(IE.GT.22) X4=RTOF-SH2
      X4 = RF + SH3*1.25
      IF(IE.GT.22) X4 = X4 + SH2
      X1=-X4*SIN(FI)
      Y1= X4*COS(FI)
      IF(IE.GT.22) FI=FI+PI
      X1=X1-SH2*COS(FI)
      Y1=Y1-SH2*SIN(FI)
      CALL DNUM(IE,X1,Y1,SH2,FI)
   33 FI = DEPLC + (IE-1)*DEFI
      COSF = COS(FI)
      SINF = SIN(FI)
      X1 = - R1*COSF
      Y1 = R1*SINF
      X4 = - R2*COSF
      Y4 = R2*SINF
      FI = FI + DEFI
      COSF = COS(FI)
      SINF = SIN(FI)
      X2 = - R1*COSF
      Y2 = R1*SINF
      X3 = - R2*COSF
      Y3 = R2*SINF
      CALL CRICRO(0.,0.)
      IF(IH.NE.2.OR.IPJF.LE.0) GO TO 32
      IF(DSPDTM(3)) GO TO 32
C** DISPLAY TOF VALUES ABOVE THE COUNTERS
      WANGLE = .5*PI - (IE-1)*DEFI
      IF(IE.GT.22) WANGLE = WANGLE + PI
      WANGLE = WANGLE*PIDEG
      FI1 = FI - DEFI
      IF(IE.GT.22) FI1 = FI1 + DEFI
      RF1 = RF
      IF(IE.GT.22) RF1 = RF + SH3
      XT = RF1*COS(FI1)
      YT = RF1*SIN(FI1)
      CALL CORE(HMW,5)
      WRITE(cHMW,3258) TTOF(IE) ! PMF 17/11/99: UNIT=10 changed to cHMW
3258  FORMAT(F5.1)
      CALL USRSYM(-XT,YT,SH3,HMW,5,WANGLE)
C     IEC = (IE-1)/4
C     IE1 = IE - IEC*4
C     IPJHF = 2*(IPJF+1) + 91 + IEC*9 + 2*IE1- 2
C     WANGLE = .5*PI - (IE-1)*DEFI
C     IF(IE.GT.22) WANGLE = WANGLE + PI
C     FI = FI - DEFI*.8
C     IF(IE.GT.22) FI = FI + DEFI*.6
C     RADD = 0.
C     IF(IE.GT.22) RADD = SH3
C     X1 = (RF+RADD)*COS(FI)
C     Y1 = (RF+RADD)*SIN(FI)
C     ITDC = HDATA(IPJHF+1) + HDATA(IPJHF+2)
C     CALL DNUM(ITDC,-X1,Y1,SH3,WANGLE)
C     GO TO 32
   32 CONTINUE
   31 CONTINUE
      call setcol(' ') !PMF 23/11/99: reset colour
      RETURN
      END
