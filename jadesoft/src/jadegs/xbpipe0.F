C   28/11/85 606091629  MEMBER NAME  XBPIPE0  (JADEGS)      FORTRAN
C
C-----------------------------------------------------------------------
      FUNCTION XBPIPE(DUMMY)
C-----------------------------------------------------------------------
C
C
C     RETURNS THE NUMBER OF RADIATION LENGTHS FROM THE INSIDE OF THE
C     BEAM PIPE TO THE FIRST GAS MOLECULE IN THE JET CHAMBER TANK.
C     BASED ON DATE IN HEAD BANK.
C     IF A VTXC BANK IS PRESENT, NEW GEOMETRY IS FORCED.
C     ARGUMENT DUMMY IS IGNORED.
C
C   RADIATION LENGTH UPDATE, 9.6.1986    J.O.
C
C-----------------------------------------------------------------------
C
      IMPLICIT INTEGER*2 (H)
C
#include "cdata.for"
C
C------------------  C O D E  ------------------------------------------
C
      IH2    = 2*IDATA( IBLN( 'HEAD' ) )
      IVTXC  =   IDATA( IBLN( 'VTXC' ) )
      IF( IVTXC .GT. 0 ) GO TO 10
C
      IMONTH = HDATA(IH2+7)
      IYEAR  = HDATA(IH2+8)
C
C                            OLD GEOMETRY (PRIOR TO MAY 1984)
C
C   FOR VALUE OF RADIATION LENGTH, SEE JCN 87
C
      XBPIPE = 0.2027
C     XBPIPE = 0.16
C
      IF( IYEAR .LT. 1984  .OR.
     +    IYEAR .EQ. 1984  .AND.  IMONTH .LT. 5 ) RETURN
C
C                            NEW GEOMETRY
C
C 10  XBPIPE = 0.1312
  10  XBPIPE = 0.1853
C
      RETURN
      END
