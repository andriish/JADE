C   30/06/80 007010946  MEMBER NAME  DEDSEL   (JADEGS)      FORTRAN
      SUBROUTINE SPARE
      IMPLICIT INTEGER*2 (H)
#include "cjdrch.for"
#include "cgraph.for"
#include "cdata.for"
      COMMON /CJTRIG/ PI,TWOPI
      DIMENSION LIST(2,8)
      DIMENSION INTR(2,8)
      DATA LIST/ 17,1, 13,2, 13,3, 13,4,  9,3,  9,4, 17,3, 17,4/
      DATA INTR/   539, 30000,
     1            2303, 30000,
     1            2570,  3311,
     1            2570,  3311,
     1            2775, 30000,
     1            2775, 30000,
     1            2783, 30000,
     1            2783, 30000/
C---
      IF(LASTVW.GT.3) GO TO 200
      IHHEAD=2*IDATA(IBLN('HEAD'))
      IF(IHHEAD.LT.1) GO TO 200
      NRUN=HDATA(IHHEAD+10)
      DO 1 IDED=1,8
      LIM1=INTR(1,IDED)
      LIM2=INTR(2,IDED)
      IF(NRUN.LT.LIM1) GO TO 1
      IF(NRUN.GE.LIM2) GO TO 1
      ISEC=LIST(1,IDED)
      ISEL=LIST(2,IDED)
      PHI0=TWOPI*(ISEC-1)/24.
      DPHI=TWOPI/48.
      IF(ISEL.EQ.3) DPHI=TWOPI/96.
      IF(ISEL.EQ.4) DPHI=TWOPI*3./96.
      PHI=PHI0+DPHI
      CSS=COS(PHI)
      SNN=SIN(PHI)
      R0=FSENSW(1)
      IF(ISEL.EQ.2) R0=FSENSW(2)
      IF(ISEL.GE.3) R0=FSENSW(3)
      R=R0+75.
      DS=TWOPI*R/48.
      IF(ISEL.GE.3) DS=TWOPI*R/96.
      X1=R-DS
      Y1= -DS
      X2=R+DS
      Y2=  DS
      X3=R-DS
      Y3=  DS
      X4=R+DS
      Y4= -DS
      X=CSS*X1-SNN*Y1
      Y=SNN*X1+CSS*Y1
      CALL MOVEA(-X,Y)
      X=CSS*X2-SNN*Y2
      Y=SNN*X2+CSS*Y2
      CALL DRAWA(-X,Y)
      X=CSS*X3-SNN*Y3
      Y=SNN*X3+CSS*Y3
      CALL MOVEA(-X,Y)
      X=CSS*X4-SNN*Y4
      Y=SNN*X4+CSS*Y4
      CALL DRAWA(-X,Y)
    1 CONTINUE
  200 CONTINUE
      RETURN
      END
