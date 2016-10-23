C   06/02/87            MEMBER NAME  TPTGCY   (S)        M  FORTRAN77
C
C-----------------------------------------------------------------------
      SUBROUTINE TPTGCY( ITRACK, IB, IW, AW, HW )
C-----------------------------------------------------------------------
C
C     Author:  C. Bowdery        6/02/87:  Copy TAGG/2 track to IW array
C
C     Based on routine by Y. Watanabe 29/08/79
C
C     Routine to copy information about one tagging cluster to a work
C     array IW.
C
C-----------------   A R G U M E N T S      ----------------------------
C
C     ITRACK   In        I*4      Current TAGG track/cluster
C     IB       In        I*4      Pointer into TAGG/2 bank
C     IW            Out  I*4      Filled work array
C     AW            Out  R*4      Filled work array equivalenced to IW
C     HW            Out  I*2      Filled work array equivalenced to IW
C
C------------------  D E F I N I T I O N S  ----------------------------

      IMPLICIT  INTEGER*2 (H)

      PARAMETER  ( NW = 40, NW2 = 2*NW )

#include "bosdata.for"

      COMMON / CLGPRM / ITH,MAXCLS,IRLTHD,IRLTH2,IRLTH3, ZVRTX,DZVRTX

      DIMENSION  IW( NW ), AW( NW ), HW( NW2 )

C------------------  C O D E  ------------------------------------------

      HW(1)  = 1
      HW(3)  = 1
      HW(5)  = 0
      HW(13) = ITRACK

C                            HW(17) = 2 or 20      set in TPTGT1  <----

      AW(12) = ZVRTX
      AW(15) = DZVRTX

      AW(23) = 100.0

C                            Charge is not determined.   Set to 100.

      AW(24) = ADATA( IB + 5 )
      AW(25) = ADATA( IB + 6 )

C                            Direction

      IW(26) =  1
      AW(27) = ADATA( IB + 11 )
      AW(28) = ADATA( IB + 12 )
      AW(29) = ADATA( IB + 13 )

C                            Unknown particles

      HW(68) = 0
      AW(35) = 0.14

C                            Check if edge blocks (implemented?)

      HW(77) = 2
      IF( HDATA( IB*2 + 6 ) .GT. 500 ) HW(77) = 1

C                            Energy

      AW(36) = AW(24)
      AW(37) = AW(36)
      AW(38) = AW(25)

      RETURN
      END
