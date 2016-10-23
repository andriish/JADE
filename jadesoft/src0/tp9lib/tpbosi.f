C   29/09/86 801271428  MEMBER NAME  TPBOSI   (S)        M  FORTRAN77
C
C-----------------------------------------------------------------------
      SUBROUTINE  TPBOSI
C-----------------------------------------------------------------------
C
C     Author:   C. Bowdery      29/09/86:  Initialise BOS for TP
C
C     Last mod: C. Bowdery      27/01/88:  Add second output stream
C
C     Routine to initialise the BOS memory manager.
C
C------------------  D E F I N I T I O N S  ----------------------------
C
      IMPLICIT  INTEGER*2 (H)
C
C                            Include BOS COMMON macro
C
#include "bosdata.for"
C
C                            Include PARAMETER statement defining l.u.n
C
#include "units.for"
C
C------------------  C O D E  ------------------------------------------

C                            Do a page throw

      WRITE(6,1)
   1  FORMAT('1')

C                            Initialise BOS COMMON to length = LENBOS

      CALL BINT( LENBOS, 4000, 500, 0 )

C                            Initialise BOS output for both streams
C                            i.e. accepted and rejected events.

      CALL BWRO( JUNITA, 1558, 2 )
      CALL BWRO( JUNITR, 1558, 2 )

      RETURN
      END
