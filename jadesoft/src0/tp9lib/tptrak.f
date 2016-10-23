C   09/02/87 711061719  MEMBER NAME  TPTRAK   (S)        M  FORTRAN77
C
C-----------------------------------------------------------------------
      SUBROUTINE  TPTRAK
C-----------------------------------------------------------------------
C
C     Author:   C. Bowdery      25/03/87:  Process tracks
C
C          mod: C. Bowdery      26/07/87:  Include dE/dx call
C     Last mod: C. Bowdery      12/10/87:  Remove dE/dx and other calls
C
C     Routine to process tracks and create summary banks for them.
C
C------------------  D E F I N I T I O N S  ----------------------------

      IMPLICIT  INTEGER*2 (H)

C------------------  C O D E  ------------------------------------------

C                            Process tracks in the central detectors

      CALL TPCDET

C                            Create summary banks including vertex info

      CALL TPTRTP

C                            Delete unwanted track banks

      CALL TPTRDL

      RETURN
      END
