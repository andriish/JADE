C   28/09/88 809281622  MEMBER NAME  TPSEFL   (S)        M  FORTRAN77
C
C-----------------------------------------------------------------------
      SUBROUTINE  TPSEFL( IFLAG, ICODE )
C-----------------------------------------------------------------------
C
C     Author:   C. Bowdery      28/09/88:  Store error flag  (TPEV)
C
C
C
C     Routine to store/update one of the error flags in TPEV/1 bank
C
C-----------------   A R G U M E N T S      ----------------------------
C
C     IFLAG     In       I*4      Flag number of error
C     ICODE     In       I*4      Error code
C
C------------------  D E F I N I T I O N S  ----------------------------

      IMPLICIT  INTEGER*2 (H)

#include "bosdata.for"

      INTEGER  IFLAG, ICODE, ITPEV

C------------------  C O D E  ------------------------------------------

C                            Find TPEV/1 bank

      CALL CLOC( ITPEV, 'TPEV', 1 )

      IF( ITPEV .GT. 0 ) THEN

        IF( IFLAG .GT. 0  .AND.  IFLAG .LE. 9 ) THEN

          HDATA(2*ITPEV+110+IFLAG) = HDATA(2*ITPEV+110+IFLAG) + ICODE

        ELSE

C                            Illegal error flag

          CALL TPERRH

        ENDIF

      ELSE

C                            TPEV bank is missing!

        CALL TPERRH

      ENDIF

      RETURN
      END
