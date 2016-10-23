C   11/01/88 807212001  MEMBER NAME  TPTRI2   (S)        M  FORTRAN77
C
C-----------------------------------------------------------------------
      SUBROUTINE  TPTRI2( VPROC )
C-----------------------------------------------------------------------
C
C     Author:   C. Bowdery       7/06/88:  Get instr. about vertex proc.
C
C
C
C     Routine to provide yes/no answer about need for track
C     post-processing (primary vertex, new direction cosines, decay
C     searches and V0 searches).
C
C-----------------   A R G U M E N T S      ----------------------------
C
C     VPROC     Out      L*4      TRUE if vertex processing step wanted
C
C------------------  D E F I N I T I O N S  ----------------------------

      IMPLICIT  INTEGER*2 (H)

      CHARACTER  TRACK*8, VPROCS*20

      LOGICAL  VPROC, VP, FIRST

      SAVE  VP, FIRST

      DATA  FIRST / .TRUE. /
      DATA  TRACK / 'TRACK' /,  VPROCS / 'VPROCESS' /

C------------------  C O D E  ------------------------------------------

C                            If not first call, VP already SAVEd.

      IF( FIRST ) THEN

C                            Get user request relating to vertex proc.

        CALL TPUOPT( TRACK, VPROCS, VP )

        FIRST = .FALSE.

      ENDIF

C                            Set VPROC equal to VP

      VPROC = VP

C                            Record this in the statistics area

      CALL TPSTAT

      RETURN
      END
