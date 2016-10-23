C   26/07/87 901181602  MEMBER NAME  TPTPEV   (S)        M  FORTRAN77
C
C-----------------------------------------------------------------------
      SUBROUTINE  TPTPEV
C-----------------------------------------------------------------------
C
C     Author:   C. Bowdery      26/07/87:  Create bank TPEV/1
C
C     Last mod: C. Bowdery      30/09/88:  Set error flags
C
C     Routine to create and initialise the event summary bank TPEV/1
C
C------------------  D E F I N I T I O N S  ----------------------------

      IMPLICIT  INTEGER*2 (H)

#include "bosdata.for"

      INTEGER  LTPEV, ITPEV, IJETC, IATOF, IALGL, IALGN, IMUEV, IATAG

C                            Define length of the TPEV/1 bank

      PARAMETER  ( LTPEV = 60 )

C------------------  C O D E  ------------------------------------------

C                            Create the TPEV/1 bank

      CALL CCRE( ITPEV, 'TPEV', 1, LTPEV, IER)

      IF( IER .EQ. 0 ) THEN

C                            Version number = 916 = v9.1.6

        IDATA( ITPEV + 1 ) = 916

C                            Today's date and time

        IDATA( ITPEV + 2 ) = IDATTM( 0 )

C                            Set 'no raw data' error flags for
C                            inner detector, TOF counters, LG
C                            muon filter and tagging system.

        IJETC = IDATA( IBLN( 'JETC' ) )
        IF( IJETC .LE. 0 )               HDATA( 2*ITPEV + 111 ) = 10000

        CALL CLOC( IATOF, 'ATOF', 0 )
        IF( IATOF .LE. 0 )               HDATA( 2*ITPEV + 113 ) = 10000

        IALGL = IDATA( IBLN( 'ALGL' ) )
        CALL CLOC( IALGN, 'ALGN', 1 )
        IF( IALGL .LE. 0  .AND.
     +      IALGN .LE. 0        )        HDATA( 2*ITPEV + 115 ) = 10000

        IMUEV = IDATA( IBLN( 'MUEV' ) )
        IF( IMUEV .LE. 0 )               HDATA( 2*ITPEV + 116 ) = 10000

        CALL CLOC( IATAG, 'ATAG', 0 )
        IF( IATAG .LE. 0 )               HDATA( 2*ITPEV + 117 ) = 10000

      ELSE

        CALL TPERRH

      ENDIF

      RETURN
      END
