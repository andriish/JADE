C   11/08/87 708141527  MEMBER NAME  TPCNTR   (S)        M  FORTRAN77
C
C-----------------------------------------------------------------------
      SUBROUTINE  TPCNTR
C-----------------------------------------------------------------------
C
C     Author:   C. Bowdery      14/08/87:  Count no. of each track type
C
C
C
C     Routine to count no. of each particle type and charge and no.
C     of vertices and store these statistics in the TPEV bank.
C
C------------------  D E F I N I T I O N S  ----------------------------

      IMPLICIT  INTEGER*2 (H)

      INTEGER  IFLAG( 4 )

C                            Array of bank pointers (TPTR, later TPVX)
C                            MAXLIM is maximum no. of  banks handled

      PARAMETER ( MAXLIM = 200 )

      INTEGER  INDA( MAXLIM )

#include "bosdata.for"

C------------------  C O D E  ------------------------------------------

      CALL CLOC( ITPEV, 'TPEV', 1 )
      IF( ITPEV .GT. 0 ) THEN
        ITPEV2 = ITPEV * 2

        DO  10  I = 1,4
          IFLAG(I) = 0
 10     CONTINUE

C                            Process TPTR banks to increment statistics
C                            on the no., type and charge for the event

        CALL BDAR( 'TPTR', NTRACK, INDA, MAXLIM )
        IF( NTRACK .EQ. MAXLIM ) THEN
          CALL TPWARN
        ENDIF

        DO  20  ITRK = 1,NTRACK
          CALL TPCNT1( ITPEV2, INDA( ITRK ), IFLAG )
 20     CONTINUE

C                            Set the flag to indicate seen particles

        HDATA( ITPEV2 + 31 ) = IFLAG(4)*1000 +
     +                         IFLAG(3)*100  +
     +                         IFLAG(2)*10   +
     +                         IFLAG(1)

C                            Collect vertex statistics (total, 0,+/-)

        NCVTX   = 0
        NNVTX   = 0

        CALL BDAR( 'TPVX', NVTX, INDA, MAXLIM )
        IF( NVTX .EQ. MAXLIM ) THEN
          CALL TPWARN
        ENDIF

        DO  30  IVTX = 1,NVTX
          ITPVX  = INDA( IVTX )
          ITPVX2 = ITPVX * 2
          IF( HDATA( ITPVX2 + 21 ) .EQ. 0 ) THEN

C                            Count this neutral vertex if not primary

            IF( IDATA( ITPVX - 2 ) .NE. 1 ) NNVTX = NNVTX + 1

          ELSE

            NCVTX = NCVTX + 1

          ENDIF
  30    CONTINUE

        HDATA( ITPEV2 + 12 ) = NVTX
        HDATA( ITPEV2 + 13 ) = NNVTX
        HDATA( ITPEV2 + 14 ) = NCVTX

      ELSE

        CALL TPERRH

      ENDIF

      RETURN
      END
