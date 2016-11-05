C   01/11/84            MEMBER NAME  PBGRFI   (S)           FORTRAN
C
C-----------------------------------------------------------------------
      SUBROUTINE PBGRFI( SH1, SH2, SH3, DEFIX )
C-----------------------------------------------------------------------
C
C    AUTHOR:   J. OLSSON        ?     :  LG AMPLITUDES  (R-FI-VIEW)
C
C  LAST MOD:   J. HAGEMANN   10/10/84 :  NOW OWN MEMBER (FROM EVDISP)
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
C
      COMMON /CWORK1/ R,FI,XA,COSFI,X1,Y1,YA,SINFI,X2,Y2,ZET,X3,Y3,X4,Y400001900
     +               ,IMW(200)
C
      COMMON /CJTRIG/ PI,TWOPI
C
C-----------------  C O D E  -------------------------------------------
C
C  LEAD GLASS AMPLITUDES
      CALL PBGSUM(2)
      call setcol('ECAL')     ! PMF 23/11/99: set colour
      DO 4 NROW=1,84
      IESUM=IMW(NROW)
      IF(IESUM.LE.0) GO TO 4
C CORNER POINTS, START PNT FOR TEXT
      X1 = - RLG*COS((NROW-1)*DEFIX)
      Y1 = RLG*SIN((NROW-1)*DEFIX)
      X2 = - RLG*COS(NROW*DEFIX)
      Y2 = RLG*SIN(NROW*DEFIX)
      X3 = - OUTR2*COS(NROW*DEFIX)
      Y3 = OUTR2*SIN(NROW*DEFIX)
      X4 = - OUTR2*COS((NROW-1)*DEFIX)
      Y4 = OUTR2*SIN((NROW-1)*DEFIX)
      IF((NROW.GE.22).AND.(NROW.LE.63)) GO TO 19
      XA = X4
      YA = Y4
      FI = ATAN2((Y4-Y1),(X4-X1))
      IF(FI.GT.PI*.5) FI = FI + PI
      IF(FI.LE.-PI*.5 + .001) FI = FI + PI
      GO TO 20
   19 CONTINUE
      XA = X2
      YA = Y2
      FI = ATAN2((Y3-Y2),(X3-X2))
      IF(FI.LT.0.) FI = FI + TWOPI
   20 CONTINUE
      COSFI = COS(FI)
      SINFI = SIN(FI)
C START POINT FOR TEXT
      X0 = XA + SH1*COSFI - SH2*SINFI
      Y0 = YA + SH1*SINFI + SH2*COSFI
      CALL DNUM(IESUM,X0,Y0,SH3,FI)
    4 CONTINUE
      call setcol(' ')        ! PMF 23/11/99: reset colour
      RETURN
      END
