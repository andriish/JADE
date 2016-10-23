C   01/11/84            MEMBER NAME  PBGSUM   (S)           FORTRAN
C
C-----------------------------------------------------------------------
      SUBROUTINE PBGSUM(INDEX)
C-----------------------------------------------------------------------
C
C    AUTHOR:   J. OLSSON        ?     :  SUM UP LG ENERGIES
C
C  LAST MOD:   J. HAGEMANN   10/10/84 :  NOW OWN MEMBER (FROM EVDISP)
C
C-----------------------------------------------------------------------
C
      IMPLICIT INTEGER*2 (H)
C
#include "cdata.for"
#include "cgraph.for"
C
      COMMON /CWORK1/ R,FI,XA,COSFI,X1,Y1,YA,SINFI,X2,Y2,ZET,X3,Y3,X4,Y400001800
     +               ,IMW(200)
C
C-----------------  C O D E  -------------------------------------------
C
C SUM UP ALL LEAD GLASS COLUMN AND ROW ARRAYS, INCLUDING END CAPS
C--
C-- IMW 1-84   RFI- ROWS
C-- IMW 85 - 108  CROSS SUMS IN ENDCAPS     DIFFERENT FOR ZX OR ZY VIEWS
C-- IMW 109 - 172   CROSS SUMS IN BARREL      --------- SAME ---------
C--
      DO 1 NI=1,172
1     IMW(NI)=-1
      IF(INDEX.EQ.1.OR.INDEX.EQ.4.OR.INDEX.EQ.8.OR.INDEX.GT.11) RETURN
      IPJ = IDATA(IBLN('ALGN'))
      IF(IPJ.LE.0) RETURN
      NWO=IDATA(IPJ)
      IF(NWO.LE.3) RETURN
      IPJ=2*IPJ+7
      LIM2=IPJ+2*NWO - 7
      DO 2 NI=IPJ,LIM2,2
      NO=HDATA(NI)
      IF(NO.GT.2687) GO TO 3
      NROW=NO/32+1
      IMW(NROW)=IMW(NROW)+HDATA(NI+1)
      NPHI = NO/32
      NROW = NO - NPHI*32 + 109
      NQ = NO/672
      IF(INDEX.LT.8.AND.(NQ.EQ.1.OR.NQ.EQ.2)) NROW = NROW + 32
      IF(INDEX.GT.7.AND.NQ.GT.1) NROW = NROW + 32
      IF(IMW(NROW).EQ.-1) IMW(NROW) = 0
      IMW(NROW) = IMW(NROW) + HDATA(NI+1)
      GO TO 2
 3    IF(INDEX.LT.5) RETURN
      CALL XYBLK(NO,XEB,YEB,DXEB,DYEB)
      IF((NO-2687).LE.96) XEB=-XEB+898.50
      IF((NO-2687).GT.96) XEB=-XEB+6013.02
      YEB=YEB-900.
      IF(INDEX.LE.7) IECIND=(XEB-0.5*DXEB+7.*DXEB)/DXEB
      IF(INDEX.GT.7) IECIND=(YEB+0.5*DYEB+7.*DYEB)/DYEB
      IF(IECIND.LT.1) IECIND=1
      IF(IECIND.GT.12) IECIND=12
      IF((NO-2687).GT.96) IECIND=IECIND+12
      IMW(84+IECIND)=IMW(84+IECIND)+HDATA(NI+1)
    2 CONTINUE
      RETURN
      END
