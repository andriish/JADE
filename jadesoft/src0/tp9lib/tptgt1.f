C   06/02/87 807251153  MEMBER NAME  TPTGT1   (S)        M  FORTRAN77
C
C-----------------------------------------------------------------------
      SUBROUTINE  TPTGT1( IB, ITRACK, LAST, MODVTX, IER )
C-----------------------------------------------------------------------
C
C     Author:  C. Bowdery        6/02/87:  Add 1 tagg track to new TPTR
C              A. Finch
C
C          mod: C. Bowdery      11/01/88:  SETSL--> VZERO
C     Last mod: C. Bowdery      25/07/88:  Add extra argument
C
C     Routine to copy information about one tagging cluster to a new
C     TPTR bank.
C
C-----------------   A R G U M E N T S      ----------------------------
C
C     IB       In        I*4      Pointer into TAGG/2 bank
C     ITRACK   In        I*4      Current cluster/track number
C     LAST     In / Out  I*4      Current last TPTR bank number
C     MODVTX   In        I*4      Vertex info from TPVX/1 bank
C     IER           Out  I*4      Error code from CCRE (0 = ok)
C
C------------------  D E F I N I T I O N S  ----------------------------

      IMPLICIT  INTEGER*2 (H)

      PARAMETER  ( NW = 40, NW2 = 2*NW )

      DIMENSION  IW( NW ), AW( NW ), HW( NW2 )

      EQUIVALENCE  ( IW(1), AW(1) ), ( IW(1), HW(1) )

C------------------  C O D E  ------------------------------------------

C                            Clear the work array IW

             CALL VZERO( IW, NW )

C                            Fill in the work array from TAGG/2

             CALL TPTGCY( ITRACK, IB, IW, AW, HW )

C                            Add in vertex info from TPVX/1

             HW(17) = MODVTX

C                            Create a new TPTR bank

             LAST   = LAST + 1

             CALL CCRE( NPTR, 'TPTR', LAST, NW, IER )

             IF( IER .NE. 0 ) RETURN

             CALL BSTR( NPTR, IW, NW )

             RETURN
             END
