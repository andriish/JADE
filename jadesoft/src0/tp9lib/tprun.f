C   07/02/87            MEMBER NAME  TPRUN    (S)        M  FORTRAN77
C
C-----------------------------------------------------------------------
      SUBROUTINE  TPRUN( IRUN )
C-----------------------------------------------------------------------
C
C     Author:  C. Bowdery        7/02/87:  Get event run number
C
C
C
C     Routine to get event run number from the HEAD bank.
C
C-----------------   A R G U M E N T S      ----------------------------
C
C     IRUN         Out   I*4      Run number of this event
C
C------------------  D E F I N I T I O N S  ----------------------------

      IMPLICIT  INTEGER*2 (H)

#include "bosdata.for"

C------------------  C O D E  ------------------------------------------

C                            Get HEAD bank

      IHEAD = IDATA( IBLN('HEAD') )

C                            Check if bank still exists

      IF( IHEAD .GT. 0 ) THEN

        IRUN = HDATA( 2*IHEAD + 10 )

      ELSE

        CALL TPERRH
        IRUN = 10000

      ENDIF

      RETURN
      END
