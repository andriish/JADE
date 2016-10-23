C   05/02/87 801121144  MEMBER NAME  TPTAGS   (S)        M  FORTRAN77
C
C-----------------------------------------------------------------------
      SUBROUTINE  TPTAGS
C-----------------------------------------------------------------------
C
C     Author:   C. Bowdery       5/02/87:  Process tagging system
C               A. Finch
C
C     Last mod: C. Bowdery      12/01/88:  Remove excess C's in col 1
C
C     Routine to perform tagging analysis (if wanted) and place
C     the summary information to the TP banks.
C     Unwanted banks are deleted at the end.
C
C------------------  D E F I N I T I O N S  ----------------------------

      IMPLICIT  INTEGER*2 (H)

C------------------  C O D E  ------------------------------------------

C                            Analyse tagging system (may do nothing)

      CALL TPTAGA

C                            Add tagg information to banks (if wanted)

      CALL TPTAGT

C                            Delete tagging banks as requested

      CALL TPTAGD


      RETURN
      END
