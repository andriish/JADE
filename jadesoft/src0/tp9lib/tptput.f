C   09/12/87 807261221  MEMBER NAME  TPTPUT   (S)        M  FORTRAN77
C
C-----------------------------------------------------------------------
      SUBROUTINE  TPTPUT( NP, NMAXP, T, AXIS )
C-----------------------------------------------------------------------
C
C     Author:   C. Bowdery       9/12/87:  Store thrust info (TPEV)
C
C     Last mod: C. Bowdery      26/07/88:  Add extra arguments
C
C     Routine to store thrust and thrust axis in TPEV/1 bank
C
C-----------------   A R G U M E N T S      ----------------------------
C
C     NP        In       I*4      Number of particles
C     NMAXP     In       I*4      Max number used in thrust calc.
C     T         In       R*4      Thrust of event
C     AXIS      In       R*4      Thrust axis (1..3)
C                                   AXIS(1) = x dir. cos. of thrust axis
C                                   AXIS(2) = y dir. cos. of thrust axis
C                                   AXIS(3) = z dir. cos. of thrust axis
C
C------------------  D E F I N I T I O N S  ----------------------------

      IMPLICIT  INTEGER*2 (H)

#include "bosdata.for"

      REAL  T, AXIS(3)

C------------------  C O D E  ------------------------------------------

C                             Find TPEV/1 bank and copy thrust info

      CALL CLOC( ITPEV, 'TPEV', 1 )

      IF( ITPEV .GT. 0 ) THEN

        ADATA( ITPEV + 42 )   = T
        ADATA( ITPEV + 43 )   = AXIS(1)
        ADATA( ITPEV + 44 )   = AXIS(2)
        ADATA( ITPEV + 45 )   = AXIS(3)

C                             Copy number of tracks available and
C                             max number that were accepted.

        HDATA( 2*ITPEV + 81 ) = NP
        HDATA( 2*ITPEV + 82 ) = NMAXP

      ELSE

C                            TPEV bank is missing!

        CALL TPERRH

      ENDIF

      RETURN
      END
