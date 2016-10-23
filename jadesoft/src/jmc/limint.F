C   20/01/83 606061623  MEMBER NAME  LIMINT   (S)           FORTRAN
      SUBROUTINE LIMINT(IBLOCK,IPART,IER,XL1,XL2,XL3,XU1,XU2,XU3)
C
C     RETURNS INTEGRATION LIMITS USED BY VEGAS FOR INTEGRATION
C     OF THE SHOWER FUNCTION OVER THE BLOCK IBLOCK
C
C     XLI/ XUI  ARE IN CYLINDRICAL COORDINATES OR CARTESIAN COORDINATES
C               FOR BARREL OR ENDCAPS RESPECTIVELY
C
C
C                                       04/06/82
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
C                                       04/06/82
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
