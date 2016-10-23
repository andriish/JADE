      SUBROUTINE ZVTRCK(ZVTX,IND,IPPATR)
      IMPLICIT INTEGER*2 (H)
#include "cdata.for"
      COMMON /CWORK/ ZTR(120), HIST(100)
      EQUIVALENCE (Z1,ZTR(1)) , (Z2,ZTR(2)) , (Z3,ZTR(3))
      DZBIN  =  60.
      DZBIN2 = 120.
      Z0BIN  =-3000.
      IP = IPPATR
      IND = -3
      IF(IP.LE.0) RETURN
      IND = -2
      IF(IDATA(IP+2).LE.0) RETURN
      IND = -1
      NTR = IDATA(IP+2)
      LDTR = IDATA(IP+3)
      JP0 = IP+IDATA(IP+1)
      JP9 = JP0 + LDTR*NTR - 1
      MTR = 0
      DO 13000 JP1 = JP0,JP9,LDTR
          I0 = JP1+ 1
          I9 = I0 + LDTR
      IF(
     - ADATA(JP1+32).LT.60. .AND. IDATA(JP1+33).GT.12 .AND.
     ?     ABS(ADATA(JP1+25)).LT..00150
     -)THEN
          RSQ = ADATA(JP1+5)**2 + ADATA(JP1+6)**2
      IF(
     - RSQ.LT.250000.
     -)THEN
            MTR = MTR + 1
            ZTR(MTR) = ADATA(JP1+31)
      ENDIF
      ENDIF
13000 CONTINUE
13001 CONTINUE
      IF(MTR.LE.0) RETURN
      IND = 0
      IF(
     - MTR.EQ.1
     -)THEN
        ZVTX = Z1
        RETURN
      ENDIF
      IF(
     - MTR.EQ.2
     -)THEN
      IF(
     - ABS(Z2-Z1).GT.DZBIN2
     -)THEN
          ZVTX = Z1
          IF(ABS(Z1).GT.ABS(Z2)) ZVTX = Z2
          RETURN
      ENDIF
        ZVTX = (Z1+Z2)*.5
        IND = 1
        RETURN
      ENDIF
      IF(
     - MTR.EQ.3
     -)THEN
      IF(
     - Z1.GT.Z2
     -)THEN
          ZW = Z2
          Z2 = Z1
          Z1 = ZW
      ENDIF
      IF(
     - Z2.GT.Z3
     -)THEN
      IF(
     - Z1.GT.Z3
     -)THEN
            ZW = Z3
            Z3 = Z2
            Z2 = Z1
            Z1 = ZW
      ELSE
            ZW = Z3
            Z3 = Z2
            Z2 = ZW
      ENDIF
      ENDIF
      IF(
     - ABS(Z2-Z1).GT.DZBIN2 .AND. ABS(Z3-Z2).GT.DZBIN2
     -)THEN
          ZVTX = Z1
          IF(ABS(Z2).LT.ABS(ZVTX)) ZVTX = Z2
          IF(ABS(Z3).LT.ABS(ZVTX)) ZVTX = Z3
          RETURN
      ENDIF
      IF(
     - ABS(Z3-Z1).LT.DZBIN2
     -)THEN
          ZVTX = (Z1+Z2+Z3) / 3.
          IND = 1
          RETURN
      ENDIF
      IF(
     - ABS(Z2-Z1).LT.DZBIN2 .AND. ABS(Z3-Z2).LT.DZBIN2
     -)THEN
          Z1 = (Z1+Z2) * .5
          Z2 = (Z2+Z3) * .5
          ZVTX = Z1
          IF(ABS(Z2).LT.ABS(ZVTX)) ZVTX = Z2
          IND = 1
          RETURN
      ENDIF
      IF(
     - ABS(Z2-Z1).GT.DZBIN2
     -)THEN
          Z1 = Z2
          Z2 = Z3
      ENDIF
        ZVTX = (Z1+Z2) * .5
        IND = 1
        RETURN
      ENDIF
      CALL SETS(HIST(1),0,200,0)
      DO 13002 I=1,MTR
        IZ = (ZTR(I)-Z0BIN) / DZBIN
      IF(
     - IZ.GT.0 .AND. IZ.LT.99
     -)THEN
          HIST(IZ  ) = HIST(IZ  ) + 1
          HIST(IZ+1) = HIST(IZ+1) + 2
          HIST(IZ+2) = HIST(IZ+2) + 1
      ENDIF
13002 CONTINUE
13003 CONTINUE
      MPEAK0 = 0
      ZVTX = -100000.
      LBPEAK = 0
      DO 13004 IZ=1,99
        MPEAK1 = HIST(IZ) + HIST(IZ+1)
      IF(
     - MPEAK1.GT.3
     -)THEN
      IF(
     - MPEAK1.GT.MPEAK0
     -)THEN
            MZ = IZ
            MPEAK = MPEAK1
            LBPEAK = 1
      ELSE
      IF(
     - MPEAK1.EQ.MPEAK0
     -)THEN
              LBPEAK = 2
      ELSE
      IF(
     - LBPEAK.NE.3
     -)THEN
                ZV0  = MZ*DZBIN + Z0BIN
                DZV0 = DZBIN
                IF(LBPEAK.EQ.2) ZV0  = DZBIN*.5 + ZV0
                IF(LBPEAK.EQ.2) DZV0 = DZBIN*1.5
      IF(
     - ABS(ZV0).LT.ABS(ZVTX)
     -)THEN
                  ZVTX = ZV0
                  DZVT = DZV0
      ENDIF
      ENDIF
              LBPEAK = 3
      ENDIF
      ENDIF
      ELSE
      IF(
     - LBPEAK.GT.0 .AND. LBPEAK.NE.3
     -)THEN
            ZV0  = MZ*DZBIN + Z0BIN
            DZV0 = DZBIN
            IF(LBPEAK.EQ.2) ZV0  = DZBIN*.5 + ZV0
            IF(LBPEAK.EQ.2) DZV0 = DZBIN*1.5
      IF(
     - ABS(ZV0).LT.ABS(ZVTX)
     -)THEN
              ZVTX = ZV0
              DZVT = DZV0
      ENDIF
      ENDIF
          LBPEAK = 0
      ENDIF
        MPEAK0 = MPEAK1
13004 CONTINUE
13005 CONTINUE
      ZVSUM = 0.
      NZVS  = 0
      DO 13006 I=1,MTR
      IF(
     - ABS(ZTR(I)-ZVTX).LE.DZVT
     -)THEN
          ZVSUM = ZVSUM + ZTR(I)
          NZVS  = NZVS + 1
      ENDIF
13006 CONTINUE
13007 CONTINUE
      IF(
     - NZVS.GT.1
     -)THEN
        ZVTX = ZVSUM / NZVS
        IND = 1
      ELSE
        ZVTX = -100000.
      DO 13008 I=1,MTR
          IF(ABS(ZTR(I)).LT.ABS(ZVTX)) ZVTX = ZTR(I)
13008 CONTINUE
13009 CONTINUE
      ENDIF
      RETURN
      END
