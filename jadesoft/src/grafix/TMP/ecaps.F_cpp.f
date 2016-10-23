C   01/11/84            MEMBER NAME  ECAPS    (S)           FORTRAN
C
C-----------------------------------------------------------------------
      SUBROUTINE ECAPS(NECC)
C-----------------------------------------------------------------------
C
C    AUTHOR:   J. OLSSON        ?     :  LG END CAPS ENERGIES
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
      COMMON /CWORK1/ R,FI,R1,FI1,X1,Y1,R2,FI2,X2,Y2,ZET,X3,Y3,X4,Y4,
     +                HMW(132)
C
      DATA ICALL/0/
C
C-----------------  C O D E  -------------------------------------------
C
      IF(ICALL.EQ.1) GO TO 1000
      ICALL=1
      OFST1=0.
      OFST2=1.-OFST1
      BLSEX = OFST1*BLXY
      SH3 = 0.6667*BLXY
 1000 CONTINUE
      IPJ = IDATA(IBLN('ALGN'))
      IF(IPJ.LE.0) GO TO 4
      NWO = IDATA(IPJ)
      IF(NWO.LE.3) GO TO 4
      IPJ = 2*IPJ + 8
      NWO = IPJ + 2*NWO - 8
C
      DO 12  NI = IPJ,NWO,2
      NO = HDATA(NI-1)
      IF(NO.LT.2687) GO TO 12
      IF(NO.GE.0.AND.NO.LE.2879) GO TO 13
      WRITE(JUSCRN,101) NO
  101 FORMAT('ECAPS FOUND ILLEGAL BLOCK NO:',I10)
      GO TO 12
C
C  GET COORDINATE OF BLOCK
C
13    CONTINUE
C     REDUCE TO NUMBERS 1 - 192
      NO = NO - 2687
C     0 FOR -Z, 1 FOR +Z
      NE = (NO - 1)/96
      IF(NECC.EQ.1.AND.NE.NE.0) GO TO 12
      IF(NECC.EQ.2.AND.NE.NE.1) GO TO 12
C     REDUCE TO 1 - 96
      NO = NO - NE*96
C     GET QUADRANT NUMBER 0 - 3
      NQ = (NO - 1)/24
C     REDUCE TO 1 - 24
      NO = NO - NQ*24
C     COMPUTE CORNER OF BLOCK IN FIRST QUADRANT
      IF(.NOT.((NO.EQ.1).OR.(NO.EQ.5).OR.(NO.EQ.10).OR.(NO.EQ.16)))
     1GO TO 43
      Y0 = BLSEX
      X0 = 3.4*BLXY - BLSEX + (NO/5)*BLXY
   43 CONTINUE
      IF(.NOT.((NO.EQ.2).OR.(NO.EQ.6).OR.(NO.EQ.11).OR.(NO.EQ.17)))
     1GO TO 44
      Y0 = BLSEX + BLXY
      X0 = 3.*BLXY - BLSEX + (NO/5)*BLXY
   44 CONTINUE
      IF(.NOT.((NO.EQ.3).OR.(NO.EQ.7).OR.(NO.EQ.12).OR.(NO.EQ.18)))
     1GO TO 45
      Y0 = BLSEX + 2.*BLXY
      X0 = 2.*BLXY - BLSEX + (NO/5)*BLXY
   45 CONTINUE
      IF(.NOT.((NO.EQ.8).OR.(NO.EQ.13).OR.(NO.EQ.19).OR.(NO.EQ.20)))
     1GO TO 46
      Y0 = BLSEX + BLXY*3.
      K = NO/5
      IF(K.GT.2) K = 7 - K
      X0 = (K+1)*BLXY-BLSEX
   46 CONTINUE
      IF(.NOT.((NO.EQ.4).OR.(NO.EQ.9).OR.(NO.EQ.15).OR.(NO.EQ.24)))
     1GO TO 47
      X0 = BLXY - BLSEX
      K = (NO-1)/5
      IF(K.EQ.4) K = 3
      Y0 = 2.4*BLXY + BLSEX + K*BLXY
   47 CONTINUE
      IF(.NOT.((NO.EQ.14).OR.(NO.EQ.23)))
     1GO TO 48
      X0 = 2.*BLXY - BLSEX
      Y0 = 4.*BLXY + BLSEX + (NO/15)*BLXY
   48 CONTINUE
      IF(.NOT.((NO.EQ.21).OR.(NO.EQ.22)))
     1GO TO 49
      Y0 = 4.*BLXY + BLSEX
      X0 = - BLSEX + 4.*BLXY - ((NO-2)/20)*BLXY
   49 CONTINUE
C     ROTATE TO CORRECT QUADRANT
      IF(NQ.NE.1) GO TO 50
      REM = X0
      X0 = - Y0
      Y0 = REM - OFST2*BLXY
      GO TO 52
   50 CONTINUE
      IF(NQ.NE.2) GO TO 51
      X0 = - X0 + OFST2*BLXY
      Y0 = - Y0 - OFST2*BLXY
      GO TO 52
   51 CONTINUE
      IF(NQ.NE.3) GO TO 52
      REM = Y0
      Y0 = - X0
      X0 = REM + OFST2*BLXY
   52 CONTINUE
      X0 = - X0
 2000 CONTINUE
      X1=X0+10.
      Y1=Y0+10.
      IE = HDATA(NI)
      CALL PHNUMB(IE,SH3,0.)
   12 CONTINUE
C
    4 CONTINUE
      RETURN
      END
