C   06/11/87 711061909  MEMBER NAME  TPTRFM   (S)        M  FORTRAN77
C
C-----------------------------------------------------------------------
      SUBROUTINE  TPTRFM
C-----------------------------------------------------------------------
C
C     Author:   C. Bowdery       6/11/87:  Add vertex related info.
C
C
C
C     Routine to add vertex related information to the TPTR banks
C     and create TPVX banks. Includes fine momentum determination.
C
C------------------  D E F I N I T I O N S  ----------------------------

      IMPLICIT  INTEGER*2 (H)

#include "bosdata.for"

C------------------  C O D E  ------------------------------------------


C                            Delete existing vertex banks

      CALL BMLT( 1, 'GVTX' )
      CALL BDLM

      IPATR = IDATA( IBLN( 'PATR'))
      IF( IPATR .LE. 0 ) THEN
        CALL TPWARN
      ELSE

C                            Main vertex finding routine

        CALL TPVTXD( IPATR, IERR )
        IF( IERR .NE. 0 ) THEN

C                            Handle the difficult cases, e.g. collinear
C                            2 prongs and coplanar events.

          CALL TPVTX1( IPATR )
          CALL CLOC( ITPVX,'TPVX',1)
          IF( ITPVX .GT. 0 ) THEN

C                            Update the TPVX banks for the above cases

            CALL CLOC( ITPEV, 'TPEV', 1 )
            IF( ITPEV .GT. 0 ) THEN
              CALL TPVTX2( IPATR, ITPEV, ITPVX )
            ELSE
              CALL TPERRH
            ENDIF

          ENDIF
        ENDIF

      ENDIF

      RETURN
      END
