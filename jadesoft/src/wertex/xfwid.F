      FUNCTION XFWID(IPHEAD) 
C-----------------------------------------------------------------------
C
C
C     RETURNS THE NUMBER OF RADIATION LENGTHS FROM THE INSIDE OF THE
C     BEAM PIPE TO THE FIRST GAS MOLECULE IN THE JET CHAMBER TANK.
C     BASED ON DATE IN HEAD BANK.
C     MC-DATA: NEW GEOMETRY IS FORCED IF FLAG LVTXC IS TRUE
C     ARGUMENT DUMMY IS IGNORED.
C
C   RADIATION LENGTH UPDATE, 9.6.1986    J.O.
C   RADIATION LENGTH UPDATE, 11.6.1986    J.O.
C-----------------------------------------------------------------------
C
      IMPLICIT INTEGER*2 (H)
C PMF 03.11.98 
      LOGICAL TBIT
C
      LOGICAL NEWDET,LVTXC,LNHARD
C
#include "cdata.for"
#include "cadmin.for"
C
      COMMON / CVCEX  / LVTXC
C
C------------------  C O D E  ------------------------------------------
C
      IH2    = 2*IPHEAD
C
      IMONTH = HDATA(IH2+7)
      IYEAR  = HDATA(IH2+8)
C
C                            OLD GEOMETRY (PRIOR TO MAY 1984)
C
C
C   FOR VALUE OF RADIATION LENGTH, SEE JCN 87
C
      XFWID = 0.1604
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
      XFWID = 0.1443
C
      IF ((IEVTP.NE.0).AND.(IYEAR.LE.1985)) XFWID = 0.1334
C
      RETURN
      END
