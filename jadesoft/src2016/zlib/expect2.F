C   12/08/87 805102011  MEMBER NAME  EXPECT2  (S)           FORTRAN77
      SUBROUTINE EXPECT(IPART,E,VAR,VERT,NBLOCK,IBLIST,BLCFRC,AVGSUM)
C
C   COPIED FROM F11HEL.ES(EXPECT)     LAST MOD 19/04/88 MZ
C
C
C     CALCULATE EXPECTED FRACTIONS OF ELECTROMAGNETIC SHOWER
C     IN NBLOCK BLOCKS GIVEN IN A BLOCKLIST IBLIST
C
C     I/P : IPART        DETECTOR PART -1,0,+1
C           E            SHOWER ENERGY (DEFAULT 500 MEV)
C           VAR          IMPACT POINTS ON LEAD-GLASS SURFACE
C                        VAR(1),VAR(2) = PHI,Z IN BARREL CASE
C                        VAR(1),VAR(2) =   X,Y IN ENDCAP CASE
C           VERT         EVENT VERTEX (X,Y,Z)
C           NBLOCK       LENGTH OF BLOCKLIST
C           IBLIST       LIST OF BLOCKS FOR WHICH THE FRACTIONS
C                        SHOULD BE CALCULATED
C
C     O/P : BLCFRC       FRACTION FOR EACH REQUESTED BLOCK
C           AVGSUM       ENERGY FRACTION IN BLOCKS CONTAINED IN IBLIST
C
C                                       06/08/82
      EXTERNAL SF
C
#include "cgeo1.for"
C
      COMMON /TRAFO/ STR,CTR,SPR,CPR,CTRCPR,CTRSPR,STRCPR,STRSPR,
     +               JPART,XMOV,YMOV,ZMOV
      COMMON /PARAME/ P(6)
      COMMON / CJTRIG / PI,TWOPI
      DIMENSION VAR(2),VERT(3)
      DIMENSION EL(6),AL(6),ALL(6),BL(6),ED(5),AD(5),ALD(5),BD(5)
      DIMENSION XL(3),XU(3)
      DIMENSION IBLIST(1),BLCFRC(1)
C
C     SHOWER FUNCTION PARAMETERS ACCORDING TO E.LONGO ET. AL.
C
      DATA EL     /100.   ,300.   ,500.   ,700.   ,1000.  ,5000.  /
      DATA AL     /  4.54 ,  7.18 ,  8.24 ,  8.32 ,  8.58 , 10.88 /
      DATA ALL    /  1.   ,  1.45 ,  1.65 ,  1.84 ,  2.03 ,  2.74 /
      DATA BL     /   .515,   .493,   .476,   .470,   .468,   .454/
C
      DATA ED     /100.   ,200.   ,200.   ,300.   ,4000.   /
      DATA AD     /  2.64 ,  1.06 ,   .08 ,   .26 ,   2.3  /
      DATA ALD    /   .45 ,   .2  ,   .19 ,   .19 ,    .71 /
      DATA BD     /  -.022,  -.017,  -.006,  -.002,   -.014/
C
CHANGED 19/04/88 MZ
CCC   CALL RNSET(1)
C
      JPART = IPART
      DO 115 IBL = 1,NBLOCK
      BLCFRC(IBL) = 0.
  115 CONTINUE
      AVGSUM = 0.
C
C     IMPACT POINT ON LG SURFACE ??
C
      IF(ICHECK(VAR(1),VAR(2),IPART).NE.1) RETURN
C
C     TRANSFORMATION PARAMETERS
C
      IF(IPART.EQ.0) GOTO 1
      ZENDCP = IPART*ZENDPL
      DX = VAR(1)-VERT(1)
      DY = VAR(2)-VERT(2)
      DZ = ZENDCP-VERT(3)
      XMOV = VAR(1)
      YMOV = VAR(2)
      ZMOV = ZENDCP
      GOTO 2
    1 DX = RLG*COS(VAR(1))-VERT(1)
      DY = RLG*SIN(VAR(1))-VERT(2)
      DZ = VAR(2)-VERT(3)
      XMOV = RLG*COS(VAR(1))
      YMOV = RLG*SIN(VAR(1))
      ZMOV = VAR(2)
    2 CTR = DZ/SQRT(DX*DX+DY*DY+DZ*DZ)
      STR = SQRT(1.-CTR*CTR)
      PR=ATAN2(DY,DX)
      IF(PR.LT.0.) PR=PR+TWOPI
      SPR = SIN(PR)
      CPR = COS(PR)
      CTRCPR = CTR*CPR
      CTRSPR = CTR*SPR
      STRCPR = STR*CPR
      STRSPR = STR*SPR
C
C     SHOWER PARAMETERS (PLO'I' ARE INTERPOLATED FROM THE LONGO PARAM.)
C
      IF(E.LE.0.) E=500.
      IF(E.LT.5000.) GOTO 3
      PLO1 = AL(6)
      PLO2 = ALL(6)
      PLO3 = BL(6)
      GOTO 9
    3 IF(E.LT.1000.) GOTO 4
      EFAC = (E-EL(5))/ED(5)
      PLO1 = AL(5)+AD(5)*EFAC
      PLO2 = ALL(5)+ALD(5)*EFAC
      PLO3 = BL(5)+BD(5)*EFAC
      GOTO 9
    4 IF(E.LT.700.)  GOTO 5
      EFAC = (E-EL(4))/ED(4)
      PLO1 = AL(4)+AD(4)*EFAC
      PLO2 = ALL(4)+ALD(4)*EFAC
      PLO3 = BL(4)+BD(4)*EFAC
      GOTO 9
    5 IF(E.LT.500.)  GOTO 6
      EFAC = (E-EL(3))/ED(3)
      PLO1 = AL(3)+AD(3)*EFAC
      PLO2 = ALL(3)+ALD(3)*EFAC
      PLO3 = BL(3)+BD(3)*EFAC
      GOTO 9
    6 IF(E.LT.300.)  GOTO 7
      EFAC = (E-EL(2))/ED(2)
      PLO1 = AL(2)+AD(2)*EFAC
      PLO2 = ALL(2)+ALD(2)*EFAC
      PLO3 = BL(2)+BD(2)*EFAC
      GOTO 9
    7 IF(E.LT.100.)  GOTO 8
      EFAC = (E-EL(1))/ED(1)
      PLO1 = AL(1)+AD(1)*EFAC
      PLO2 = ALL(1)+ALD(1)*EFAC
      PLO3 = BL(1)+BD(1)*EFAC
      GOTO 9
    8 PLO1 = AL(1)
      PLO2 = ALL(1)
      PLO3 = BL(1)
C
    9 P(1) = PLO2
      P(2) = PLO1*2.023
      P(3) = PLO3
      P(4) = 2.04604
      P(5) = PLO1*.17
      P(6) = .597942
C
C     NORMALIZE INTEGRAL
C
      P2A1=PLO2+1.
      FNORM = 66678.5*PLO1*GAMMA(P2A1)/PLO3**P2A1
C
C     INTEGRATION
C
C
C     CHECK TOTAL VALUE ON REQUEST
C
      IF(NBLOCK.GT.0) GOTO 113
      XL(1) = RLG
      IF(IPART.NE.0) XL(1) = VAR(1)-500.
      XL(2) = VAR(1)-2.
      IF(IPART.NE.0) XL(2) = VAR(2)-500.
      XL(3) = VAR(2)-500.
      IF(IPART.EQ. 1) XL(3) = ZENDPL
      IF(IPART.EQ.-1) XL(3) = ZENDMI-1000.
      XU(1) = RLG+1000.
      IF(IPART.NE.0) XU(1) = VAR(1)+500.
      XU(2) = VAR(1)+2.
      IF(IPART.NE.0) XU(2) = VAR(2)+500.
      XU(3) = VAR(2)+500.
      IF(IPART.EQ. 1) XU(3) = ZENDPL+1000.
      IF(IPART.EQ.-1) XU(3) = ZENDMI
      CALL VEGAS(SF,.001,1000,9,XL,XU,AVG,SD,C2)
      AVG = AVG/FNORM
      SD = SD/FNORM
      WRITE(6,1133) AVG,SD,C2
 1133 FORMAT(10X,'---> VEGAS INTEGRAL RESULT : ',F12.5,/,
     +       10X,'---> ERROR + CHI**2 VALUE  : ',2F12.5)
      RETURN
C
C     PERFORM INTEGRATION OVER REQUESTED BLOCKS
C
  113 DO 114 IBL = 1,NBLOCK
      IBLOCK = IBLIST(IBL)
      CALL LIMINT(IBLOCK,IPART,IER,XL(1),XL(2),XL(3),XU(1),XU(2),XU(3))
      IF(IER.GT.0) GOTO 114
      CALL VEGAS(SF,.01,200,3,XL,XU,AVG,SD,C2)
      AVG = AVG/FNORM
      SD = SD/FNORM
      AVGSUM = AVGSUM+AVG
      BLCFRC(IBL) = AVG
  114 CONTINUE
C
      AVGDIF = 1.-AVGSUM
C
      RETURN
      END
C
      REAL FUNCTION SF(X)
C
C     FUNCTION TO BE INTEGRATED BY VEGAS
C                                       16/06/82
C
      COMMON /TRAFO/ STR,CTR,SPR,CPR,CTRCPR,CTRSPR,STRCPR,STRSPR,
     +               JPART,XMOV,YMOV,ZMOV
      COMMON /PARAME/ P(6)
      REAL X(3)
      DATA X0 /22.39/
C
      SF = 0.
C
C     TRANSFORM INTO CARTESIAN DETECTOR SYSTEM
C
      IF(JPART.NE.0) GOTO 1
      XDET = X(1)*COS(X(2))
      YDET = X(1)*SIN(X(2))
      ZDET = X(3)
      FJACOB = X(1)
      GOTO 2
    1 XDET = X(1)
      YDET = X(2)
      ZDET = X(3)
      FJACOB = 1.
C
C     LINEAR TRANSFORMATION TO SHOWER START POINT
C
    2 XLTR = XDET-XMOV
      YLTR = YDET-YMOV
      ZLTR = ZDET-ZMOV
C
C     ROTATE INTO SHOWER SYSTEM (FIRST CHECK SIGN OF ZSHW)
C
      ZSHW  =  XLTR * STRCPR   +   YLTR * STRSPR   +   ZLTR * CTR
C
      IF(ZSHW.LE.0.0) RETURN
C
      XSHW  =  XLTR * CTRCPR   +   YLTR * CTRSPR   -   ZLTR * STR
      YSHW  = -XLTR * SPR      +   YLTR * CPR
C
C     SCALE IN RADIATION LENGTHS
C
      RSHS = SQRT(XSHW*XSHW+YSHW*YSHW)/X0
      ZSHS = ZSHW/X0
C
C     COMPRESS SHOWER FUNCTION
C
      P3Z = P(3)*ZSHS
      REX = -1.*P(1)*ALOG(ZSHS)
      EX1 = -1.*(REX+P3Z+P(4)*RSHS)
      EX2 = -1.*(REX+P3Z+P(6)*RSHS)
      IF(EX1.LT.-10..OR.EX2.LT.-10.) RETURN
C
      SF =  P(2)*EXP(EX1) + P(5)*EXP(EX2)
C
C     JACOBIAN
C
      SF = SF*FJACOB
C
      RETURN
      END
C
      SUBROUTINE VEGAS(FXN,BCC,NCALL,ITMX,XL,XU,AVGI,SD,CHI2A)
C
C     SUBROUTINE PERFORMS N-DIMENSIONAL MONTE CARLO INTEGRATION
C     - BY PETER LEPAGE  SEPT 1976/(REV)APR 1978
C
C     COPIED FROM T00VER.AXOLIB.S       02/04/82
C     CHANGED                           20/06/82
C
C     ****                                                          ****
C     ****  THIS IS A MODIFIED VERSION OF THE ORIGINAL VEGAS        ****
C     ****  ROUTINE IN 'T00VER.AXOLIB.S'                            ****
C     ****  THE MAIN CHANGES ARE :                                  ****
C     ****  - ONLY FOR 3-DIMENSIONAL INTEGRATION                    ****
C     ****  - ALL REDUNDANT STATEMENTS REMOVED TO OBTAIN FASTER     ****
C     ****    EXECUTION                                             ****
C     ****  - NO DIVIDE CHECKS FOR INTEGRATION REGIONS WITH 'KINK'  ****
C     ****  - REDUCED NUMBER OF INPUT PARAMETERS                    ****
C     ****                                                          ****
C
      IMPLICIT REAL*8 (D)
C
      DIMENSION D(50,3), DI(50,3), XIN(50), R(50), DX(3), IA(3)
     1 , KG(3), DT(3), XI(50,3)
      DIMENSION XL(3), XU(3)
      DIMENSION QRAN(3), X(3)
      DATA NDMX,ONE,MDS/50, 1., 1 /
      DATA XI /150*1./
      NDO = 1
      DO 1 J = 1,3
    1 XI(1,J) = ONE
      ENTRY VEGAS1( FXN, BCC, NCALL, ITMX, XL, XU, AVGI, SD, CHI2A)
C      -INITIALIZES CUMMULATIVE VARIABLES, BUT NOT GRID
      IT = 0
      SI = 0.
      SI2 = SI
      SWGT = SI
      SCHI = SI
      SCALLS = SI
      ENTRY VEGAS2( FXN, BCC, NCALL, ITMX, XL, XU, AVGI, SD, CHI2A)
C      - NO INITIALIZATION
      ND = NDMX
      NG = 1
      IF( MDS .EQ. 0 )  GO TO 2
      NG = ( NCALL / 2. ) ** .333
      MDS = 1
      IF ( ( 2*NG - NDMX) .LT .0 )  GO TO 2
      MDS = -1
      NPG = NG / NDMX + 1
      ND = NG / NPG
      NG = NPG * ND
    2 K= NG * NG * NG
      NPG = NCALL / K
      IF( NPG .LT. 2)  NPG = 2
      CALLS = NPG*K
      DXG = ONE/NG
      DV2G = DXG * DXG * DXG * DXG * DXG * DXG /( NPG*NPG*( NPG-ONE ) )
      XND = ND
      NDM = ND - 1
      DXG = DXG * XND
      XJAC = ONE
      DO 3 J = 1,3
        DX( J ) = XU( J ) - XL( J )
    3   XJAC = XJAC * DX(J)
      IF( ND .EQ. NDO )  GO TO 8
      RC = NDO / XND
      DO 7 J=1,3
        K = 0
        XN = 0.
        DR = XN
        I = K
    4   K = K+1
        DR = DR + ONE
        XO = XN
        XN = XI(K,J)
    5   IF( RC .GT. DR )  GO TO 4
        I = I+1
        DR = DR-RC
        XIN( I ) = XN - (XN-XO) * DR
        IF( I .LT. NDM )  GO TO 5
        DO 6 I=1,NDM
          XI(I,J)=XIN(I)
    6   CONTINUE
        XI(ND,J)=ONE
    7 CONTINUE
      NDO = ND
      ACC = BCC
    8 CONTINUE
      ENTRY VEGAS3(FXN,BCC,NCALL,ITMX,XL,XU,AVGI,SD,CHI2A)
C      - MAIN INTEGRATION LOOP
    9 IT = IT+1
      ITS = IT
      TI = 0.
      TSI = TI
      DO 10 J=1,3
        KG(J)=1
        DO 10 I=1,ND
          D(I,J)=TI
          DI(I,J)=TI
   10 CONTINUE
   11 FB = 0.
      F2B = FB
      K = 0
   12 K = K+1
      DO 112 J=1,3
  112   QRAN(J)=RN(0)
        WRITE(6,9345)   QRAN
 9345   FORMAT ( 3X,'EXPECT2  3 * RANDOM NUMBER: ',3( F12.8,2X) )
      WGT=XJAC
      DO 15 J=1,3
        XN=(KG(J)-QRAN(J))*DXG+ONE
        IA(J)=XN
        IAJ=IA(J)
        IF(IAJ.GT.1)GO TO 13
        XO=XI(IAJ,J)
        RC=(XN-IA(J))*XO
        GO TO 14
   13   XO=XI(IAJ,J)-XI(IAJ-1,J)
        RC=XI(IAJ-1,J)+(XN-IAJ)*XO
   14   X(J)=XL(J)+RC*DX(J)
        WGT=WGT*XO*XND
   15 CONTINUE
      F=FXN(X)*WGT
      F1=F/CALLS
      W=WGT/CALLS
      F2=F*F
      FB=FB+F
      F2B=F2B+F2
      DO 16 J=1,3
        IAJ=IA(J)
        DI(IAJ,J)=DI(IAJ,J)+F/CALLS
   16   IF(MDS.GE.0)D(IAJ,J)=D(IAJ,J)+F2
      IF(K.LT.NPG)GO TO 12
      F2B=DSQRT(F2B*NPG)
      F2B=(F2B-FB)*(F2B+FB)
      TI=TI+FB
      TSI=TSI+F2B
      IF(MDS.GE.0)GO TO 18
      DO 17 J=1,3
      IAJ=IA(J)
   17 D(IAJ,J)=D(IAJ,J)+F2B
   18 K=3
   19 KG(K)=MOD(KG(K),NG)+1
      IF(KG(K).NE.1)GO TO 11
      K=K-1
      IF(K.GT.0)GO TO 19
      TI=TI/CALLS
      TSI=TSI*DV2G
C
C     CHECK FOR TSI
C
      IF(TSI.GT.0.) GOTO 29
      AVGI = 0.
      SD = 0.
      CHI2A = 0.
      RETURN
C
   29 TI2=TI*TI
      WGT=TI2/TSI
      SI=SI+TI*WGT
      SI2=SI2+TI2
      SWGT=SWGT+WGT
      SCHI=SCHI+TI2*WGT
      SCALLS=SCALLS+CALLS
      AVGI=SI/SWGT
      SD=SWGT*IT/SI2
      CHI2A=0.
      IF(IT.GT.1)CHI2A=SD*(SCHI/SWGT-AVGI*AVGI)/(IT-.999)
      SD=DSQRT(ONE/SD)
      SDAV=SD/AVGI
      IF(ABS(SDAV).LE.ACC.OR.IT.GE.ITMX)NOW=2
      DO 23 J=1,3
      XO=D(1,J)
      XN=D(2,J)
      D(1,J)=(XO+XN)/2.
      DT(J)=D(1,J)
      DO 22 I=2,NDM
      D(I,J)=XO+XN
      XO=XN
      XN=D(I+1,J)
      D(I,J)=(D(I,J)+XN)/3.
   22 DT(J)=DT(J)+D(I,J)
      D(ND,J)=(XN+XO)/2.
   23 DT(J)=DT(J)+D(ND,J)
      DO 28 J=1,3
      RC=0.
      DO 24 I=1,ND
      R(I)=0.
      IF(D(I,J).LE.0)GO TO 24
      XO=DT(J)/D(I,J)
      R(I)=((XO-ONE)/XO/DLOG(XO))
   24 RC=RC+R(I)
      RC=RC/XND
      K=0
      XN=0
      DR=XN
      I=K
   25 K=K+1
      DR=DR+R(K)
      XO=XN
      XN=XI(K,J)
   26 IF(RC.GT.DR)GO TO 25
      I=I+1
      DR=DR-RC
      XIN(I)=XN-(XN-XO)*DR/R(K)
      IF(I.LT.NDM)GO TO 26
      DO 27 I=1,NDM
   27 XI(I,J)=XIN(I)
   28 XI(ND,J)=ONE
      SDAV=SD/AVGI
      IF(IT.LT.ITMX.AND.ACC.LT.ABS(SDAV))GO TO 9
      RETURN
      END
C
      SUBROUTINE LIMINT(IBLOCK,IPART,IER,XL1,XL2,XL3,XU1,XU2,XU3)
C
C     RETURNS INTEGRATION LIMITS USED BY VEGAS FOR INTEGRATION
C     OF THE SHOWER FUNCTION OVER THE BLOCK IBLOCK
C
C     XLI/ XUI  ARE IN CYLINDRICAL COORDINATES OR CARTESIAN COORDINATES
C               FOR BARREL OR ENDCAPS RESPECTIVELY
C
C
C                                       03/06/82
#include "cgeo1.for"
C
      XL1 = 0.
      XL2 = 0.
      XL3 = 0.
      XU1 = 0.
      XU2 = 0.
      XU3 = 0.
      IER = 0
      IB = IBLOCK
C
      IF((IB.GE.0).AND.(IB.LE.2879)) GO TO 13
      IER = 1
      GO TO 2000
C
   13 CONTINUE
      IF(IPART.NE.0) GO TO 12
      IF(IB.LE.2687) GOTO 14
      IER = 1
      GOTO 2000
   14 NFI =  IB/32
      NZET = IB - NFI*32
      PC = NFI*DELFI + DELFI/2.
      ZC = ZLGMI + NZET*BLZ + BLZ/2.
      XL1 = RLG
      XU1 = RLG + BLDEP
      XL2 = PC - DELFI/2.
      XU2 = PC + DELFI/2.
      XL3 = ZC - BLZ/2.
      XU3 = ZC + BLZ/2.
      GO TO 2000
C
   12 CONTINUE
C!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
C     CHECK FOR CONSIST. IPART - BLOCK# HAS TO BE WRITTEN   !!!!!!
C!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      BLXY2 = BLXY/2.
C     REDUCE TO NUMBERS 1 - 192
      IB = IB - 2687
C     0 FOR -Z, 1 FOR +Z
      NE = (IB - 1)/96
C     REDUCE TO 1 - 96
      IB = IB - NE*96
C     GET QUADRANT NUMBER 0 - 3
      NQ = (IB - 1)/24
C     REDUCE TO 1 - 24
      IB = IB - NQ*24
C     CALCULATE CENTER COORDINATES
      IF(.NOT.((IB.EQ.1).OR.(IB.EQ.5).OR.(IB.EQ.10).OR.(IB.EQ.16)))
     1GO TO 43
      YC = BLXY2
      XC = 3.4*BLXY - BLXY2 + (IB/5)*BLXY
   43 CONTINUE
      IF(.NOT.((IB.EQ.2).OR.(IB.EQ.6).OR.(IB.EQ.11).OR.(IB.EQ.17)))
     1GO TO 44
      YC = BLXY2 + BLXY
      XC = 3.*BLXY - BLXY2 + (IB/5)*BLXY
   44 CONTINUE
      IF(.NOT.((IB.EQ.3).OR.(IB.EQ.7).OR.(IB.EQ.12).OR.(IB.EQ.18)))
     1GO TO 45
      YC = BLXY2 + 2.*BLXY
      XC = 2.*BLXY - BLXY2 + (IB/5)*BLXY
   45 CONTINUE
      IF(.NOT.((IB.EQ.8).OR.(IB.EQ.13).OR.(IB.EQ.19).OR.(IB.EQ.20)))
     1GO TO 46
      YC = BLXY2 + BLXY*3.
      K = IB/5
      IF(K.GT.2) K = 7 - K
      XC = (K+1)*BLXY-BLXY2
   46 CONTINUE
      IF(.NOT.((IB.EQ.4).OR.(IB.EQ.9).OR.(IB.EQ.15).OR.(IB.EQ.24)))
     1GO TO 47
      XC = BLXY - BLXY2
      K = (IB-1)/5
      IF(K.EQ.4) K = 3
      YC = 2.4*BLXY + BLXY2 + K*BLXY
   47 CONTINUE
      IF(.NOT.((IB.EQ.14).OR.(IB.EQ.23)))
     1GO TO 48
      XC = 2.*BLXY - BLXY2
      YC = 4.*BLXY + BLXY2 + (IB/15)*BLXY
   48 CONTINUE
      IF(.NOT.((IB.EQ.21).OR.(IB.EQ.22)))
     1GO TO 49
      YC = 4.*BLXY + BLXY2
      XC = - BLXY2 + 4.*BLXY - ((IB-2)/20)*BLXY
   49 CONTINUE
C     ROTATE TO CORRECT QUADRANT
      IF(NQ.NE.1) GO TO 50
      REM = XC
      XC = - YC
      YC = REM
      GO TO 2001
   50 CONTINUE
      IF(NQ.NE.2) GO TO 51
      XC = - XC
      YC = - YC
      GO TO 2001
   51 CONTINUE
      IF(NQ.NE.3) GO TO 2001
      REM = YC
      YC = - XC
      XC = REM
C
 2001 XL1 = XC - BLXY/2.
      XU1 = XC + BLXY/2.
      XL2 = YC - BLXY/2.
      XU2 = YC + BLXY/2.
      XL3 = ZENDPL
      XU3 = ZENDPL+DEPEND
      IF(IPART.EQ.-1) XL3=-XU3
      IF(IPART.EQ.-1) XU3=-XL3
C
 2000 CONTINUE
C
      RETURN
      END
C
      FUNCTION ICHECK(P1,P2,IPART)
C
C     CHECK WETHER IMPACT POINT (P1,P2) IS ON LG-SURFACE
C
C     ICHECK = 1 -->  O.K.
C                                       03/06/82
C
#include "cgeo1.for"
      DIMENSION MAP(6,6)
C
      ICHECK = 0
C
      IF(IPART.EQ.0) GO TO 1
C
      MAP(1,1)=   -1
      MAP(2,1)=   -1
      MAP(3,1)= 2688
      MAP(4,1)= 2692
      MAP(5,1)= 2697
      MAP(6,1)= 2703
      MAP(1,2)=   -1
      MAP(2,2)=   -1
      MAP(3,2)= 2689
      MAP(4,2)= 2693
      MAP(5,2)= 2698
      MAP(6,2)= 2704
      MAP(1,3)= 2691
      MAP(2,3)= 2690
      MAP(3,3)= 2694
      MAP(4,3)= 2699
      MAP(5,3)= 2705
      MAP(6,3)=   -1
      MAP(1,4)= 2696
      MAP(2,4)= 2695
      MAP(3,4)= 2700
      MAP(4,4)= 2707
      MAP(5,4)= 2706
      MAP(6,4)=   -1
      MAP(1,5)= 2702
      MAP(2,5)= 2701
      MAP(3,5)= 2709
      MAP(4,5)= 2708
      MAP(5,5)=   -1
      MAP(6,5)=   -1
      MAP(1,6)= 2711
      MAP(2,6)= 2710
      MAP(3,6)=   -1
      MAP(4,6)=   -1
      MAP(5,6)=   -1
      MAP(6,6)=   -1
      X = P1
      Y = P2
      IF((ABS(X).LT.(2.*BLXY)).AND.(ABS(Y).LT.(2.*BLXY))) GO TO 100
      IF((X.GT.0.).AND.(Y.GE.0.)) IQUAD=1
      IF((X.LE.0.).AND.(Y.GT.0.)) IQUAD=2
      IF((X.LT.0.).AND.(Y.LE.0.)) IQUAD=3
      IF((X.GE.0.).AND.(Y.LT.0.)) IQUAD=4
C
C     ROTATE (X,Y) INTO THE FIRST QUADRANT.
C
      JQUAD=0
    2 CONTINUE
      JQUAD=JQUAD+1
      IF(JQUAD.GE.IQUAD) GO TO 3
      XS = Y
      YS =-X
      X  =XS
      Y  =YS
      GO TO 2
    3 CONTINUE
      IF(Y.LT.BLXY) X=X-50.
      IF(X.LT.BLXY) Y=Y-50.
      IX=1.+X/BLXY
      IY=1.+Y/BLXY
      IF((IX.GT.6).OR.(IY.GT.6)) GO TO 100
      IBLK=MAP(IX,IY)
      IF(IBLK.LT.0) GO TO 100
      ICHECK = 1
      GO TO 100
C
    1 Z = P2
      IZ=(Z-ZLGMI)/BLZ
      IF((IZ.LT.1).OR.(IZ.GT.30)) GO TO 100
      ICHECK = 1
C
  100 RETURN
      END
