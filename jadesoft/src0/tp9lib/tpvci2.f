C   15/01/88 807222034  MEMBER NAME  TPVCI2   (S)        M  FORTRAN77
C
C-----------------------------------------------------------------------
      SUBROUTINE  TPVCI2( COMFIT )
C-----------------------------------------------------------------------
C
C     Author:   C. Bowdery      22/07/88:  Get instr. about COMFIT
C
C
C     Routine to provide yes/no answer about need for COMFIT. This can
C     be done even when no vertex chamber information.
C
C-----------------   A R G U M E N T S      ----------------------------
C
C     ANALYS    Out      L*4      TRUE if COMFIT step is to be done
C
C------------------  D E F I N I T I O N S  ----------------------------

      IMPLICIT  INTEGER*2 (H)

      CHARACTER  VTXC*8, CCOMFT*20

      LOGICAL  COMFIT, COMF, FIRST

      SAVE  FIRST, COMF

      DATA  FIRST  / .TRUE. /
      DATA  VTXC   / 'VTXC' /
      DATA  CCOMFT / 'COMFIT'   /

C------------------  C O D E  ------------------------------------------

C                            If not first call, COMF already SAVEd.

      IF( FIRST ) THEN

C                            Get user option relating to COMFIT

        CALL TPUOPT( VTXC, CCOMFT, COMF )

        FIRST = .FALSE.

      ENDIF

      IF( COMF ) THEN

        COMFIT = .TRUE.


      ELSE

        COMFIT = .FALSE.

      ENDIF

C                            Record this in the statistics area

      CALL TPSTAT

      RETURN
      END
