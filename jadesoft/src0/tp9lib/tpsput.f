C   09/12/87 807261203  MEMBER NAME  TPSPUT   (S)        M  FORTRAN77
C
C-----------------------------------------------------------------------
      SUBROUTINE  TPSPUT( NP, SPHITY, AXES )
C-----------------------------------------------------------------------
C
C     Author:   C. Bowdery       9/12/87:  Store sphericity info (TPEV)
C
C     Last mod: C. Bowdery      26/07/88:  NP argument added.
C
C     Routine to store sphericity eigenvalues and vectors in TPEV/1 bank
C
C-----------------   A R G U M E N T S      ----------------------------
C
C     NP        In       I*4      Number of particles used
C     SPHITY    In       R*4      Eigenvalues of sphericity
C                                   SPHITY(1) = smallest eigenvalue
C                                   SPHITY(2) = medium   eigenvalue
C                                   SPHITY(3) = largest  eigenvalue
C     AXES      In       R*4      Eigenvectors of sphericity (1..9)
C                                   AXES(1) = x dir. cos. of minor axis
C                                   AXES(2) = y dir. cos. of minor axis
C                                   AXES(3) = z dir. cos. of minor axis
C                                   AXES(4) = x dir. cos. of major axis
C                                   AXES(7) = x dir. cos. of S axis, etc
C
C------------------  D E F I N I T I O N S  ----------------------------

      IMPLICIT  INTEGER*2 (H)

#include "bosdata.for"

      REAL  SPHITY(3), AXES(9)

      INTEGER  NP

C------------------  C O D E  ------------------------------------------

C                            Find the TPEV/1 bank and copy eigenvalues
C                            and eigenvectors.

      CALL CLOC( ITPEV, 'TPEV', 1 )

      IF( ITPEV .GT. 0 ) THEN

        ADATA( ITPEV + 29 ) = SPHITY(1)
        ADATA( ITPEV + 30 ) = SPHITY(2)
        ADATA( ITPEV + 31 ) = SPHITY(3)

        CALL UCOPY( AXES, ADATA( ITPEV + 32 ), 9 )

C                            Set flag to say charged and neutrals used.
C                            Record number of particles used.

        HDATA( 2*ITPEV + 55 ) = 0
        HDATA( 2*ITPEV + 56 ) = NP

      ELSE

C                            TPEV bank is missing!

        CALL TPERRH

      ENDIF

      RETURN
      END
