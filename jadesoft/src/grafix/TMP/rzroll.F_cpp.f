C   18/07/79 212011859  MEMBER NAME  RZROLL   (JADEGS)      FORTRAN
      SUBROUTINE RZROLL
C---
C---   DISPLAY IMPACT POINTS OF CHARGED TRACKS ON THE ROLLED OUT RZVIEW
C---       J.OLSSON  03.11.82    LAST CHANGE 03.11.82
C---
      IMPLICIT INTEGER*2 (H)
      COMMON /CJTRIG/ PI,TWOPI
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
      COMMON /CZGEO/ RZCHI,RZCHA,NZRPSI,NZZ,Z1ZCH,Z2ZCH,ZCHA,ZCHB,ZCHSS,
     $               ZCHDL,ZCHDLL,DLZZ,DLZPHI,DLZW1,DLZW2
      COMMON /CZKON/ ZCVDR,ZCXCH,ZCTZER,ZCAPED,XL1,XL2
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
C
      ADDERZ = 24.*DLZPHI
      ADDRAD = ADDERZ/TWOPI
      FI48 = TWOPI/48.
      COS48 = COS(FI48)
      RINRZ = RZCHI/COS48
      RUTRZ = RZCHA/COS48
      R1I = RZCHI + ZCHA
      R2I = RZCHI + ZCHB
      ADY = 1500. + Z2ZCH
      IPO = IDATA(IBLN('PATR'))
      IF(IPO.GT.0) GO TO 1900
      CALL TRMOUT(80,' NO PATR BANK..^')
      RETURN
1900  ICNT = 0
      LO = IDATA(IPO+1)
      NTR = IDATA(IPO+2)
      LTR = IDATA(IPO+3)
      IPO = IPO + LO - LTR
200   ICNT = ICNT + 1
      IF(ICNT.GT.NTR) GO TO 21
      IPO = IPO + LTR
      ITR = IDATA(IPO + 1)
C** DISPLAY TRACKS
      IFLGX = IDATA(IPO+29)
      CALL RZTRCK(50,IDATA(IPO+18),ADATA(IPO+19),ADATA(IPO+20),
     $ ADATA(IPO+21),ADATA(IPO+22),ADATA(IPO+5),ADATA(IPO+6),
     $ ADATA(IPO+12),ADATA(IPO+13),ADATA(IPO+31),ADATA(IPO+30),
     $ XP,YP,ZP,XP2,YP2,ZP2,IFLGX)
      IF(IFLGX.NE.2) GO TO 200
      FIP = ATAN2(YP,XP)
      IF(FIP.LT.0.) FIP = FIP + TWOPI
      X1 = 0.
      X2 = X1 + FIP*ADDRAD
      Y1 = ADY
      Y2 = Y1 + ZP
201   CALL PLYGON(9,.35*DLZZ,X2,Y2,0)
      SIZE = 60.
      CALL NUMBWR(30,ITR,X2+.45*DLZZ,Y2,SIZE)
      CALL MOVEA(X2,Y2)
      IF(IFLGX.NE.2) GO TO 200
      FIP = ATAN2(YP2,XP2)
      IF(FIP.LT.0.) FIP = FIP + TWOPI
      X1 = 0.
      X2 = X1 + FIP*ADDRAD
      Y1 = ADY
      Y2 = Y1 + ZP2
      CALL DRAWA(X2,Y2)
      GO TO 200
21    CONTINUE
      RETURN
      END
