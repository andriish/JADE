C   01/11/84 411011623  MEMBER NAME  TRIG2    (S)           FORTRAN
C
C-----------------------------------------------------------------------
      SUBROUTINE TRIG2
C-----------------------------------------------------------------------
C
C   AUTHOR:     J. OLSSON       ?    :  DISPLAYS "TRIG(2)" RESULTS
C
C   LAST MOD:   J.H. C.B.    1/11/84 :  ROTNK CHANGE UPDATE
C
C-----------------------------------------------------------------------
C
      IMPLICIT INTEGER*2 (H)
C
C                            DISPLAYS "TRIG(2)" RESULTS.
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
C                            MACRO CJDRCH .... JET CHAMBER CONSTANTS.
C-----------------------------------------------------------------------
C
      COMMON / CJDRCH / RDEC(4),PSIIN(3),RINCR(3),FIRSTW(3),FSENSW(3),
     +                  RDEPTH,SWDEPL,YSUSPN,TIMDEL(2,3),ZMAX,ZOFFS,
     +                  ZRESOL,ZNORM,ZAL,ZSCAL,DRIDEV,DRICOS,DRISIN,
     +                  PEDES,TZERO(3),DRIROT(96,2),SINDRI(96,2),
     +                  COSDRI(96,2),DRIVEL(96,2),T0FIX(3),
     +                  ABERR(8), DUMJDC(20)
C
C      BLOCK DATA SET TO MC VALUES, KALIBR WILL SET REAL DATA VALUES
C--->  A CHANGE OF THIS COMMON MUST BE DONE SIMULTANEOUSLY WITH  <----
C--->  A CHANGE OF THE BLOCK DATA                                <----
C
C--------------------------- END OF MACRO CJDRCH -----------------------
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
      COMMON / CJTRIG / PI, TWOPI
      COMMON / CJCELL / NCELL(3), NWIRES(3)
C
      DIMENSION  RAD(5),IPOINT(5),HCHAR(5),HWORD(2)
C
      EQUIVALENCE (IWORD,HWORD(1))
C
      DATA  IPOINT / 2,2,1,3,4 /
      DATA  HCHAR / 'O','O','O','A','F' /
      DATA  ICALL / 0 /
C
C------------------  C O D E  ------------------------------------------
C
      IF( ICALL .NE. 0 ) GO TO 10
      ICALL = 1
      RAD(1) = (FSENSW(2)+FSENSW(1)+(NWIRES(1)-1)*RINCR(1))/2.
      RAD(2) = (FSENSW(3)+FSENSW(2)+(NWIRES(2)-1)*RINCR(2))/2.
      GAP    = FSENSW(3)-FSENSW(2)-(NWIRES(2)-1)*RINCR(2)
      RAD(3) = FSENSW(3)+(NWIRES(3)-1)*RINCR(3)+GAP/2.
C
      SH1      = GAP - 35.0
      ROTNKP   = ROTNK + DROTNK
      RAD(4)   = ROTNKP +     (RTOF - ROTNKP)/4.0
      RAD(5)   = ROTNKP + 2.0*(RTOF - ROTNKP)/3.0
      HWORD(1) = 0
   10 CONTINUE
      IPJ=IDATA(IBLN('TRIG'))
      IF(IPJ.LT.1) RETURN
      IBN=IDATA(IPJ-2)
      IF(IBN.EQ.2) GO TO 1
      IPJ=IDATA(IPJ-1)
      IF(IPJ.LT.1) RETURN
      IBN=IDATA(IPJ-2)
      IF(IBN.NE.2) RETURN
    1 CONTINUE
      IWO=2*IPJ-2
      DO 2 IREG=1,6
      PHI0=(IREG-1)*TWOPI/6.
      IWO=IWO+4
      DO 3 IRAD=1,5
      IRING=IRAD
      IF(IRING.GT.3) IRING=3
      HWORD(2)=HDATA(IWO+IPOINT(IRAD))
      IF(IRAD.EQ.1) IWORD=ISHFTR(IWORD,8)
      R=RAD(IRAD)
      HWRIT=HCHAR(IRAD)
      NN=NCELL(IRING)/3
      DPHI=TWOPI/(6*NN)
      MASK=1
      DO 4 IPHI=1,NN
      PHI=PHI0+IPHI*DPHI
      ITEST=LAND(IWORD,MASK)
      MASK=2*MASK
      IF(ITEST.EQ.0) GO TO 4
      X=-R*COS(PHI)
      Y= R*SIN(PHI)
      PHWT=PI/2.-PHI
      IF(PHWT.LT.0.) PHWT=PHWT+TWOPI
      DX0=-SH1/3.
      DY0=-SH1/2.
      DX=COS(PHWT)*DX0-SIN(PHWT)*DY0
      DY=SIN(PHWT)*DY0+COS(PHWT)*DX0
      X=X+DX
      Y=Y+DY
      ICNT=1
      CALL HEMSYM(X,Y,SH1,HWRIT,ICNT,PHWT)
    4 CONTINUE
    3 CONTINUE
    2 CONTINUE
      RETURN
      END
