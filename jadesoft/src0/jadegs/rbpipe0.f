C   28/11/85 512182247  MEMBER NAME  RBPIPE0  (S)           FORTRAN
C
C-----------------------------------------------------------------------
      FUNCTION RBPIPE(DUMMY)
C-----------------------------------------------------------------------
C
C
C     RETURNS THE BEAMPIPE RADIUS, BASED ON DATE IN HEAD BANK
C     IF A VTXC BANK IS PRESENT, NEW GEOMETRY IS FORCED.
C     ARGUMENT DUMMY IS IGNORED.
C
C-----------------------------------------------------------------------
C
      IMPLICIT INTEGER*2 (H)
C
#include "cdata.for"
#include "cgeo1.for"
#include "cgeov.for"
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
      RBPIPE = RPIP
C
      IF( IYEAR .LT. 1984  .OR.
     +    IYEAR .EQ. 1984  .AND.  IMONTH .LT. 5 ) RETURN
C
C                            NEW GEOMETRY
C
  10  RBPIPE = RPIPV
C
      RETURN
      END
