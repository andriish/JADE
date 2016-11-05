C   09/08/85 508101747  MEMBER NAME  DISPZC   (S)           FORTRAN
C
C-----------------------------------------------------------------------
      SUBROUTINE DISPZC( LVIEW )
C-----------------------------------------------------------------------
C
C    AUTHOR:   C. BOWDERY   9/08/85 :  DISPLAY Z CHAMBER HITS
C
C     DISPLAYS THE HIT INFORMATION FOR THE Z CHAMBER
C
C     LVIEW :  1 = R-PHI VIEW
C              2 = ZX AND ZY VIEWS
C              3 = ROLLED OUT VIEW
C
C-----------------------------------------------------------------------
C
      IMPLICIT INTEGER*2 (H)
C
      LOGICAL DSPDTM
C
#include "cdata.for"
C
      COMMON / CGRAP2 / BCMD,DSPDTM(30),ISTVW,JTVW
C
C------------------  C O D E  ------------------------------------------
C
      IF( .NOT. DSPDTM(11) ) RETURN
C
      IPZETC = IDATA(IBLN('ZETC'))
      IF( IPZETC .LE. 0 )    RETURN
      LENZC  = IDATA(IPZETC)
      IF( LENZC  .LT. 3 )    RETURN
C
      IZHITS = (LENZC-1)/2
      NWO    = IPZETC + LENZC -2
      IPZETC = IPZETC + 1
  10  IF( IPZETC .GT. NWO )  RETURN
C
      NWIR   = HDATA(2*IPZETC + 1)/8
C
C                            GET THE HIT COORDINATES
C
      CALL RZHITS( IPZETC, XZ,YZ, XZ1,YZ1, XZ2,YZ2, ZZ1,ZZ2, IERR )
C
C                            DRAW THE Z CHAMBER FOR VIEW = LVIEW
C
      CALL RZDRAW( NWIR, XZ,YZ, XZ1,YZ1, XZ2,YZ2, ZZ1,ZZ2, LVIEW )
C
      IPZETC = IPZETC + 2
      GO TO 10
C
      END
