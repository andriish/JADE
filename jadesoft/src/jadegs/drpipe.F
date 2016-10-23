C   28/11/85 610021350  MEMBER NAME  DRPIPE   (JADEGS)      FORTRAN
C
C-----------------------------------------------------------------------
      FUNCTION DRPIPE(DUMMY)
C-----------------------------------------------------------------------
C
C
C     RETURNS THE BEAMPIPE THICKNESS(MM), BASED ON DATE IN HEAD BANK
C     MC-DATA: NEW GEOMETRY IS FORCED IF FLAG LVTXC IS TRUE
C     ARGUMENT DUMMY IS IGNORED.
C
C-----------------------------------------------------------------------
C
      IMPLICIT INTEGER*2 (H)
C
      LOGICAL NEWDET,LVTXC,LNHARD
C
#include "cadmin.for"
#include "cdata.for"
#include "cgeo1.for"
#include "cgeov.for"
C
      COMMON / CVCEX  / LVTXC
C
C------------------  C O D E  ------------------------------------------
C
      IH2    = 2*IDATA( IBLN( 'HEAD' ) )
C
      IMONTH = HDATA(IH2+7)
      IYEAR  = HDATA(IH2+8)
C
C                            OLD GEOMETRY (PRIOR TO MAY 1984)
C
      DRPIPE = DRPIP
C                            NEW DETECTOR HARDWARE IN MONTE-CARLO DATA?
C                            THEN LVTXC IS TRUE (FLAG IS CHECKED AND
C                                                SET IN RDMTCO)
C
      LNHARD = (IMONTH .GE. 5  .AND.  IYEAR .EQ. 1984)
     +                          .OR.  IYEAR .GE. 1985
      NEWDET = LVTXC .OR. (IEVTP.EQ.0 .AND. LNHARD)
C
      IF( .NOT. NEWDET ) RETURN
C
C                            NEW GEOMETRY
C
      DRPIPE = DRPIPV
C
      RETURN
      END
