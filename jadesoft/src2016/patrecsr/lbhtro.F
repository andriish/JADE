      SUBROUTINE LBHTRO(ITRK,PAR1,PAR2,PAR3,INDPAR)
      IMPLICIT INTEGER*2 (H)
#include "cworkpr.for"
#include "cworkeq.for"
      EQUIVALENCE
     ,          (HPCO0 ,HPWRK(17)),(HPCO9 ,HPWRK(18)),(HLDCO ,HPWRK(19))
     ,         ,(ICELL ,IDWRK( 1)),(MHIT  ,IDWRK( 2)),(IRING ,IDWRK( 3))
     ,         ,(IERRCD,IDWRK( 4)),(NTRKEL,IDWRK( 5))
#include "cpatlm.for"
#include "cjdrch.for"
#include "cdsmax.for"
      DIMENSION HST( 80)
      DATA XCV /152.7/
      CALL SETS(HST(1),0,160,0)
      DCRV =  .000010
      CRV1 = -DCRV*20.
      X0   = FSENSW(IRING) + RINCR(IRING)*7.5
      Y0   = (PAR1*X0 + PAR2)*X0 + PAR3
      S0   = PAR1*X0*2. + PAR2
      CRV0 = (S0*X0 - Y0) / X0**2
      CRV0 = CRV0 + CRV1
      CVP0 = (S0*X0 - Y0) / (X0**2 - XCV**2)
      CVP0 = CVP0 + CRV1
      ZW1  = X0
      ZW2  = Y0 / X0
      ZW3  = XCV**2
      DO 13000 IP = HPCO0,HPCO9,HLDCO
          X   = WRK(IP+3)
          Y   = WRK(IP+4)
      IF(
     - ABS(X-X0) .GT. 25.
     -)THEN
            CV1 = ZW2*X - Y
            CV2 = (ZW1-X)*X
            CRV = CV1 / CV2
            IBN = (CRV-CRV0) / DCRV + 1
            IF(IBN.GT.0 .AND. IBN.LE.40) HST(IBN   ) = HST(IBN   ) + 1
            CVP = CV1 / ((X/ZW1-1.)*ZW3 + CV2)
            IBM = (CVP-CVP0) / DCRV + 1
            IF(IBM.GT.0 .AND. IBM.LE.40) HST(IBM+40) = HST(IBM+40) + 1
      ENDIF
13000 CONTINUE
13001 CONTINUE
      MHST = 0
      IMAX = 0
      DO 13002 I=19,22
        NHST = HST(I-1) + HST(I  ) + HST(I+1)
      IF(
     - NHST.GE.MHST
     -)THEN
          MHST = NHST
          IMAX = I
      ENDIF
13002 CONTINUE
13003 CONTINUE
      MHSTP = 0
      IMAXP = 0
      DO 13004 I=59,62
        NHST = HST(I-1) + HST(I  ) + HST(I+1)
      IF(
     - NHST.GE.MHSTP
     -)THEN
          MHSTP = NHST
          IMAXP = I
      ENDIF
13004 CONTINUE
13005 CONTINUE
      IF(
     - MHSTP.LT.IRING*8 .OR. MHSTP-5.LT.MHST
     -)THEN
        INDPAR = 0
      ELSE
        INDPAR = 1
        MHST = MHSTP
        IMAX = IMAXP - 40
        CALL MVC2(HST(1),0,HST(2),0,80) !PMF 28/06/99 MVC -> MVC2
        CRV0 = CVP0
        PAR1 = CVP0 - CRV1
        PAR3 = PAR1 * ZW3
      ENDIF
      NH0 = HST(IMAX-2)
      NH1 = HST(IMAX-1)
      NH2 = HST(IMAX  )
      NH3 = HST(IMAX+1)
      NH4 = HST(IMAX+2)
      IF(
     - IMAX.EQ.19 .AND.
     ?   NH0.GT.0 .AND. NH0.GT.NH1 .AND. NH4.NE.0
     -)THEN
        IMAX = IMAX + 1
      ENDIF
      IF(
     - IMAX.EQ.22 .AND.
     ?   NH4.GT.0 .AND. NH4.GT.NH3 .AND. NH0.NE.0
     -)THEN
        IMAX = IMAX - 1
      ENDIF
      IM1 = IMAX - 1
      IM3 = IMAX + 1
      IF(NH1.LE.2 .AND. NH3-NH1.GT.2) IM1 = IM1 + 1
      IF(NH3.LE.2 .AND. NH1-NH3.GT.2) IM3 = IM3 - 1
      NHST  = NH1 + NH2 + NH3
      NHTTR = 0
      IF(
     - NHST.GE.3
     -)THEN
        IHIT = 0
        IPHT = HPHT0
      DO 13006 IP = HPCO0,HPCO9,HLDCO
          X   = WRK(IP+3)
          Y   = WRK(IP+4)
      IF(
     - ABS(X-X0) .GT. 25.
     -)THEN
            CV1 = ZW2*X - Y
            CV2 = (ZW1-X)*X
      IF(
     - INDPAR.EQ.0
     -)THEN
              CRV = CV1 / CV2
      ELSE
              CRV = CV1 / ((X/ZW1-1.)*ZW3 + CV2)
      ENDIF
            IBN = (CRV-CRV0) / DCRV + 1
      IF(
     - IBN.GE.IM1 .AND. IBN.LE.IM3
     -)THEN
      IF(
     - IWRK(IP+ 9).EQ.ICELL
     -)THEN
                LBGOOD = 4
      ASSIGN 17001 TO IZZZ01
      GOTO 17000
17001 CONTINUE
      ELSE
                LBGOOD = 0
      ENDIF
      ELSE
              LBGOOD = 8
      ASSIGN 17002 TO IZZZ01
      GOTO 17000
17002 CONTINUE
      ENDIF
      ELSE
            LBGOOD = 1
      ASSIGN 17003 TO IZZZ01
      GOTO 17000
17003 CONTINUE
      ENDIF
          IWRK(IP+7) = LBGOOD
          IF(LBGOOD.EQ.0) IHIT = IHIT + 1
13006 CONTINUE
13007 CONTINUE
        NHTTR = IHIT
      ENDIF
      RETURN
17000 CONTINUE
      IF(
     - ITRK.GT.0
     -)THEN
        JCELL = IWRK(IP+ 9)
      IF(
     - JCELL.EQ.ICELL
     -)THEN
          IPBACK = IWRK(IP+ 1)
15000 CONTINUE
      IF(
     - IPHT.LT.HPHT9
     -)THEN
      IF(
     - IWRK(IPHT+1).EQ.IPBACK
     -)THEN
              IF(IWRK(IPHT+9).EQ.ITRK .OR. IWRK(IPHT+10).EQ.ITRK)
     ?          LBGOOD = 0
      GOTO 15001
      ENDIF
      IF(
     - IWRK(IPHT+1).GT.IPBACK
     -)THEN
      GOTO 15001
      ENDIF
          IPHT = IPHT + HLDHT
      GOTO 15000
      ENDIF
15001 CONTINUE
      ENDIF
      ENDIF
      GOTO IZZZ01
      END
