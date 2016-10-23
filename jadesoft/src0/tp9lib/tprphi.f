C   28/07/87 708112125  MEMBER NAME  TPRPHI   (S)        M  FORTRAN77
C
C-----------------------------------------------------------------------
      SUBROUTINE  TPRPHI
C-----------------------------------------------------------------------
C
C     Author:   C. Bowdery      28/07/87:  Do inner det. R-Phi refit
C
C
C     Routine to do R-Phi refit of inner detector tracks  (optional)
C
C------------------  D E F I N I T I O N S  ----------------------------

      IMPLICIT  INTEGER*2 (H)

      LOGICAL  REFITR

C                            Mode is refitting mode (See JCN 94)

      INTEGER  MODE

#include "bosdata.for"

C------------------  C O D E  ------------------------------------------

C                            Get instruction about refitting

      CALL TPRFI1( REFITR, MODE )

C                            If R-Phi refit wanted, do it.

      IF( REFITR ) THEN

        IPATR = IDATA( IBLN( 'PATR' ) )
        CALL EXPATR( IPATR, 64, IER )

        CALL XYRFTV( MODE )

        CALL TPDIAG(10)

      ENDIF

      RETURN

      END
