C   28/11/79 112032028  MEMBER NAME  MOMGAM   (JADEGS)      FORTRAN
      SUBROUTINE MOMGAM(IP,PX,PY,PZ,PTRANS,PTOT,PHI,THE)
      IMPLICIT INTEGER*2 (H)
C---
C---     GIVEN:  POINTER TO CLUSTER BANK IP
C---     RETURN: THREE COMPONENTS OF THE MOMENTUM
C---          J.OLSSON  16.09.79        LAST CHANGE  06.11.81
C---
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
      COMMON /CJTRIG/ PI,TWOPI
C----------------------------------------
C
      PHI = ATAN2(ADATA(IP+10),ADATA(IP+9))
      IF(PHI.LT.0.) PHI = PHI + TWOPI
      THE = ARCOS(ADATA(IP+11))
C     JPART = IDATA(IP+1)
C     IF(JPART.NE.0) GO TO 2
C****************************** BARREL PHOTONS *****
C     PHI = ADATA(IP+4)
C     THE = ATAN2(ADATA(IP+5),RLG)
C     THE = PI*.5 - THE
C     GO TO 3
C****************************** ENDCAP PHOTONS *****
C2     XP = ADATA(IP+4)
C      YP = ADATA(IP+5)
C      RR = SQRT(XP*XP + YP*YP)
C      THE = ATAN2(RR,ZENDPL)
C      IF(JPART.LT.0) THE = PI - THE
C      PHI = ATAN2(YP,XP)
C      IF(PHI.LT.0.) PHI = PHI + TWOPI
3     CONTINUE
      EGAM = ADATA(IP+2)
      COSTH = COS(THE)
      PZ = EGAM*COSTH
      COSTH = SIN(THE)*EGAM
      PX = COSTH*COS(PHI)
      PY = COSTH*SIN(PHI)
      PTRANS = SQRT(PX*PX + PY*PY)
      PTOT = SQRT(PTRANS*PTRANS + PZ*PZ)
      RETURN
      END
