C   19/02/84 705211107  MEMBER NAME  RDPATR   (JADEGS)      FORTRAN
C
C-----------------------------------------------------------------------
      SUBROUTINE RDPATR
C-----------------------------------------------------------------------
C
C   AUTHOR:   E. ELSEN     2/05/79 :  INSERT DEVIATIONS IN PATR BANK
C
C   LAST MOD: E. ELSEN    10/04/81 :
C
C    INSERT APPROPRIATE AVERAGE DEVIATIONS FOR FITS IN R-PHI-
C    AND R-Z-PLANE IN PATR, 12
C    SET NTR PROPERLY IN CASES WHERE THERE ARE TOO MANY
C    TRACKS WHICH HAVE NOT BEEN PROPERLY RECORDED IN PATR BANK 12
C
C-----------------------------------------------------------------------
C
      INTEGER*2 HDATA
#include "cmubcs.for"
C
C
      COMMON / CJDRCH / RDEC(4),
     *                  PSIIN(3),
     *                  RINCR(3),
     *                  FIRSTW(3),
     *                  FSENSW(3),
     *                  RDEPTH,
     *                  SWDEPL,
     *                  YSUSPN,
     *                  TIMDEL(6), ZMAX, ZOFFS, ZRESOL, ZNORM,ZAL,ZSCAL,
     *                  DRIDEV,DRICOS,DRISIN
C
      DATA S12 / 3.4641016 /
C
C------------------  C O D E  ------------------------------------------
C
      CALL BLOC( IPJ, 'PATR', 12, *8000 )
      NTR = IDATA(IPJ+2)
      IF( NTR .LE. 0 ) RETURN
C                                           FIX UP FOR LONG PATR BANKS
      NWNTR = (IDATA(IPJ)-IDATA(IPJ+1))/IDATA(IPJ+3)
      IF( NWNTR .LT. NTR ) NTR = NWNTR
      IDATA(IPJ+2) = NTR
C
      SRPHI = TIMDEL(1) / S12
      SRZ = ZRESOL
C
      IP = IPJ + IDATA(IPJ+1)
      LTR = IDATA(IPJ+3)
      IEND = IP + ( NTR - 1 ) * LTR
C
      DO 1000 J = IP, IEND, LTR
      ADATA(J+23) = SRPHI
 1000 ADATA(J+32) = SRZ
C
 8000 RETURN
      END
