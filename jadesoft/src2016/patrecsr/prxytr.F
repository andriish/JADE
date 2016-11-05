      SUBROUTINE PRXYTR
      IMPLICIT INTEGER*2 (H)
      LOGICAL TBIT
#include "cjdrch.for"
#include "cworkpr.for"
#include "cworkeq.for"
#include "cdsmax.for"
      IPTR  = HPFREE
      HPTE0 = HPFREE
      NBYTE = HLDTE*HNTR*4
      CALL SETSL(IWRK(IPTR),0,NBYTE,0)
      DO 13000 ITREL=1,HNTR
        JCELL = ITRKAR(ITREL,1)
        IWRK(IPTR   ) = JCELL
        IWRK(IPTR+ 1) = ITREL
        IWRK(IPTR+ 2) = ITRKAR(ITREL, 2)
        IWRK(IPTR+19) = ITRKAR(ITREL, 9)
      IF(
     - JCELL.LE.24
     -)THEN
          JRING = 1
          DXWR=DIRWR1(JCELL  ,1)
          DYWR=DIRWR1(JCELL  ,2)
          TRLOXL=TRMATS(JCELL  ,1)
          TRLOYL=TRMATC(JCELL  ,1)
          TRLOXR=TRMATS(JCELL  ,2)
          TRLOYR=TRMATC(JCELL  ,2)
      ELSE
      IF(
     - JCELL.LE.48
     -)THEN
          JRING = 2
          DXWR=DIRWR1(JCELL-24,1)
          DYWR=DIRWR1(JCELL-24,2)
          TRLOXL=TRMATS(JCELL  ,1)
          TRLOYL=TRMATC(JCELL  ,1)
          TRLOXR=TRMATS(JCELL  ,2)
          TRLOYR=TRMATC(JCELL  ,2)
      ELSE
          JRING = 3
          DXWR=DIRWR3(JCELL-48,1)
          DYWR=DIRWR3(JCELL-48,2)
          TRLOXL=TRMATS(JCELL  ,1)
          TRLOYL=TRMATC(JCELL  ,1)
          TRLOXR=TRMATS(JCELL  ,2)
          TRLOYR=TRMATC(JCELL  ,2)
      ENDIF
      ENDIF
        R0 = FSENSW(JRING)
        DR0= RINCR (JRING)
        DRC = DR0*.5
        ILAYR = ITRKAR(ITREL,3)
        DRSP1 = TRKAR(ITREL,4)
        DRSP2 = DRSP1 + TRKAR(ITREL,5)
      IF(
     - DRSP1.LT.DRC .AND. ABS(DRSP2-DRSP1).GE.DRC
     -)THEN
          DWIR = ITRKAR(ITREL,6)-ILAYR
          CRV  = 0.
          IF(DWIR.GT.0.) CRV = (TRKAR(ITREL,8)-TRKAR(ITREL,5))/DWIR
          SLOPE = CRV*.5 + DRSP2-DRSP1
          DRSP1 = DRSP1 + SLOPE
          DRSP2 = CRV*.5 + SLOPE + DRSP1
          ILAYR = ILAYR + 1
      ENDIF
        R1 = R0 + DR0*ILAYR
        R2 = R1 + DR0
        X1 = R1*DXWR
        X2 = R2*DXWR
        Y1 = R1*DYWR
        Y2 = R2*DYWR
      IF(
     - DRSP1.LT.DRC
     -)THEN
          SNFI = (DRSP2 - DRSP1) / DR0
          CSFI = SQRT(1.-SNFI**2)
          DRSPC = DRSP1/CSFI
          XL  = X1 + DYWR*DRSPC
          YL  = Y1 - DXWR*DRSPC
          DXL =-DXWR*CSFI - DYWR*SNFI
          DYL = DXWR*SNFI - DYWR*CSFI
          XR  = X1 - DYWR*DRSPC
          YR  = Y1 + DXWR*DRSPC
          DXR =-DXWR*CSFI + DYWR*SNFI
          DYR =-DXWR*SNFI - DYWR*CSFI
      ELSE
          YL  = Y1 - TRLOYL*DRSP1
          DYL = Y2 - TRLOYL*DRSP2 - YL
          XL  = X1 - TRLOXL*DRSP1
          DXL = X2 - TRLOXL*DRSP2 - XL
          DISTL = SQRT(DXL**2+DYL**2)
          DXL =-DXL / DISTL
          DYL =-DYL / DISTL
          COSGL = -DXL*TRLOYL + DYL*TRLOXL
          DDRSP = (1./COSGL - 1.) * DRC
          XL =-DDRSP*TRLOXL + XL
          YL =-DDRSP*TRLOYL + YL
          YR  = Y1 + TRLOYR*DRSP1
          DYR = Y2 + TRLOYR*DRSP2 - YR
          XR  = X1 + TRLOXR*DRSP1
          DXR = X2 + TRLOXR*DRSP2 - XR
          DISTR = SQRT(DXR**2+DYR**2)
          DXR =-DXR / DISTR
          DYR =-DYR / DISTR
          COSGR = -DXR*TRLOYR + DYR*TRLOXR
          DDRSP = (1./COSGR - 1.) * DRC
          XR = DDRSP*TRLOXR + XR
          YR = DDRSP*TRLOYR + YR
      ENDIF
        WRK(IPTR+ 3) = XL
        WRK(IPTR+ 4) = YL
        WRK(IPTR+ 5) = DXL
        WRK(IPTR+ 6) = DYL
        WRK(IPTR+ 7) = XR
        WRK(IPTR+ 8) = YR
        WRK(IPTR+ 9) = DXR
        WRK(IPTR+10) = DYR
        ILAYR = ITRKAR(ITREL,6)
        DRSP1 = TRKAR(ITREL,7)
        DRSP2 = DRSP1 - TRKAR(ITREL,8)
      IF(
     - DRSP1.LT.DRC .AND. ABS(DRSP2-DRSP1).GE.DRC
     -)THEN
          DWIR = ILAYR - ITRKAR(ITREL,3)
          CRV  = 0.
          IF(DWIR.GT.0.) CRV = (TRKAR(ITREL,8)-TRKAR(ITREL,5))/DWIR
          SLOPE =-CRV*.5 + DRSP1-DRSP2
          DRSP1 = DRSP1 - SLOPE
          DRSP2 = CRV*.5 - SLOPE + DRSP1
          ILAYR = ILAYR - 1
      ENDIF
        R1 = R0 + DR0*ILAYR
        R2 = R1 - DR0
          X1 = R1*DXWR
          X2 = R2*DXWR
          Y1 = R1*DYWR
          Y2 = R2*DYWR
      IF(
     - DRSP1.LT.DRC
     -)THEN
          SNFI = (DRSP2 - DRSP1) / DR0
          CSFI = SQRT(1.-SNFI**2)
          DRSPC = DRSP1/CSFI
          XL  = X1 + DYWR*DRSPC
          YL  = Y1 - DXWR*DRSPC
          DXL =-DXWR*CSFI + DYWR*SNFI
          DYL =-DXWR*SNFI - DYWR*CSFI
          XR  = X1 - DYWR*DRSPC
          YR  = Y1 + DXWR*DRSPC
          DXR =-DXWR*CSFI - DYWR*SNFI
          DYR = DXWR*SNFI - DYWR*CSFI
      ELSE
          YL  = Y1 - TRLOYL*DRSP1
          DYL = Y2 - TRLOYL*DRSP2 - YL
          XL  = X1 - TRLOXL*DRSP1
          DXL = X2 - TRLOXL*DRSP2 - XL
          DISTL = SQRT(DXL**2+DYL**2)
          DXL = DXL / DISTL
          DYL = DYL / DISTL
          COSGL = -DXL*TRLOYL + DYL*TRLOXL
          DDRSP = (1./COSGL - 1.) * DRC
          XL =-DDRSP*TRLOXL + XL
          YL =-DDRSP*TRLOYL + YL
          YR  = Y1 + TRLOYR*DRSP1
          DYR = Y2 + TRLOYR*DRSP2 - YR
          XR  = X1 + TRLOXR*DRSP1
          DXR = X2 + TRLOXR*DRSP2 - XR
          DISTR = SQRT(DXR**2+DYR**2)
          DXR = DXR / DISTR
          DYR = DYR / DISTR
          COSGR = -DXR*TRLOYR + DYR*TRLOXR
          DDRSP = (1./COSGR - 1.) * DRC
          XR = DDRSP*TRLOXR + XR
          YR = DDRSP*TRLOYR + YR
      ENDIF
      IF(
     - TBIT(ITRKAR(ITREL,9),23)
     -)THEN
          WRK(IPTR+15) = XL
          WRK(IPTR+16) = YL
          WRK(IPTR+17) = DXL
          WRK(IPTR+18) = DYL
          WRK(IPTR+11) = XR
          WRK(IPTR+12) = YR
          WRK(IPTR+13) = DXR
          WRK(IPTR+14) = DYR
      ELSE
          WRK(IPTR+11) = XL
          WRK(IPTR+12) = YL
          WRK(IPTR+13) = DXL
          WRK(IPTR+14) = DYL
          WRK(IPTR+15) = XR
          WRK(IPTR+16) = YR
          WRK(IPTR+17) = DXR
          WRK(IPTR+18) = DYR
      ENDIF
        IPTR = IPTR + HLDTE
13000 CONTINUE
13001 CONTINUE
      HPFREE = IPTR
      HPTE9  = IPTR - 1
      RETURN
      END
