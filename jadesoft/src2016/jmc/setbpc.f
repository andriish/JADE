C   11/04/78 C8071203   MEMBER NAME  SETBPC   (JS2)         FORTRAN
      SUBROUTINE SETBPC(R)
C
C      THIS ROUTINE SETS THE BEAM PIPE COUNTERS
C      IF A BPC COUNTER IS SET, THE CORRESPONDING POSITION IN THE
C      ARRAY HBPCAR IS SET TO 1, OTHERWISE IT IS 0
C
      COMMON/CGEO1/BKGAUS, RPIP,DRPIP,XRLPIP, RBPC,DRBPC,XRLBPC,
     *             RITNK,DRITNK,XRLTKI, R0ROH,DR0ROH,XR0ROH,
     *             R1ROH,DR1ROH,XR1ROH, R2ROH,DR2ROH,XR2ROH,
     *             R3ROH,DR3ROH,XR3ROH, ROTNK,DROTNK,XRLTKO,
     *             RTOF,DRTOF,XRTOF, RCOIL, DRCOIL, XRCOIL,
     *             ZJM,DZJM,XRZJM, ZJP,DZJP,XRZJP,
     *             ZTKM,DZTKM,XRZTKM, ZTKP,DZTKP,XRZTKP,
     *             ZBPPL,ZBPMI,ZTOFPL,ZTOFMI,
     *             XRJETC,
     *             RLG,ZLGPL,ZLGMI,OUTR2,CTLIMP,CTLIMM,DELFI,
     *             BLXY,BLZ,BLDEP,ZENDPL,ZENDMI,DEPEND,
     *             XHOL1,XHOL2,YHOL1,YHOL2,BLFI
CAV     *             XHOL1,XHOL2,YHOL1,YHOL2
CAV  Same size required
      COMMON/CBPC/HBPCAR(24)
*** PMF 15/10/99      DIMENSION R(1)
      DIMENSION R(*)
*** PMF (end)
      INTEGER*2 HBPCAR
      REAL PI/3.14159/,DFIBPC/0.262/
C
C      CHECK WHETHER BPC COUNTERS ARE HIT AT ALL
C
      IF(R(3).GT.ZBPPL) RETURN
      IF(R(3).LT.ZBPMI) RETURN
C
C      SET BPC COUNTER
C
      PHI=ATAN2(R(2),R(1))
      IF(PHI.LT.0.) PHI=2.*PI+PHI
      NBPC=PHI/DFIBPC+1.5
      IF(NBPC.GT.24) NBPC=1
      HBPCAR(NBPC)=1
      RETURN
      END
