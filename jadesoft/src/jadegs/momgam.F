C   28/11/79 112032028  MEMBER NAME  MOMGAM   (JADEGS)      FORTRAN
      SUBROUTINE MOMGAM(IP,PX,PY,PZ,PTRANS,PTOT,PHI,THE)
      IMPLICIT INTEGER*2 (H)
C---
C---     GIVEN:  POINTER TO CLUSTER BANK IP
C---     RETURN: THREE COMPONENTS OF THE MOMENTUM
C---          J.OLSSON  16.09.79        LAST CHANGE  06.11.81
C---
#include "cdata.for"
#include "cgraph.for"
#include "cgeo1.for"
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
