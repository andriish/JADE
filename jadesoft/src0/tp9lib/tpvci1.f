C   15/01/88 807222029  MEMBER NAME  TPVCI1   (S)        M  FORTRAN77
C
C-----------------------------------------------------------------------
      SUBROUTINE  TPVCI1( ANALYS )
C-----------------------------------------------------------------------
C
C     Author:   C. Bowdery      22/07/88:  Get instr. about VTXC anal
C
C
C     Routine to provide yes/no answer about need for vertex chamber
C     analysis.
C
C-----------------   A R G U M E N T S      ----------------------------
C
C     ANALYS    Out      L*4      TRUE if analysis step is to be done
C
C------------------  D E F I N I T I O N S  ----------------------------

      IMPLICIT  INTEGER*2 (H)

      CHARACTER  VTXC*8, CANALY*20

      LOGICAL  ANALYS, ANAL, FIRST

      SAVE  FIRST, ANAL

      DATA  FIRST  / .TRUE. /
      DATA  VTXC   / 'VTXC' /
      DATA  CANALY / 'ANALYSE'   /

C------------------  C O D E  ------------------------------------------

C                            If not first call, ANAL already SAVEd.

      IF( FIRST ) THEN

C                            Get user option relating to VTXC analysis
C                            that is, ANALYSE.

        CALL TPUOPT( VTXC, CANALY, ANAL )

        FIRST = .FALSE.

      ENDIF

      IF( ANAL ) THEN

        ANALYS = .TRUE.


      ELSE

        ANALYS = .FALSE.

      ENDIF

C                            Record this in the statistics area

      CALL TPSTAT

      RETURN
      END
