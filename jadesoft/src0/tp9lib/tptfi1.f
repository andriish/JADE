C   12/01/88 801121139  MEMBER NAME  TPTFI1   (S)        M  FORTRAN77
C
C-----------------------------------------------------------------------
      SUBROUTINE  TPTFI1( ANALYS )
C-----------------------------------------------------------------------
C
C     Author:   C. Bowdery      12/01/88:  Get instructions about TOFINT
C
C
C
C
C     Routine to provide yes/no answer about need for TOF analysis.
C
C-----------------   A R G U M E N T S      ----------------------------
C
C     ANALYS    Out      L*4      TRUE if analysis step is to be done
C
C------------------  D E F I N I T I O N S  ----------------------------

      IMPLICIT  INTEGER*2 (H)

      CHARACTER  TOF*8, CIFND*20, CANALY*20

      LOGICAL  ANALYS, ANAL, IFND, FIRST

#include "bosdata.for"

      INTEGER  ITOFR

      SAVE  FIRST, ANAL, IFND

      DATA  FIRST / .TRUE. /
      DATA  TOF   / 'TOF'  /, CIFND  / 'IFNOTDONE' /
      DATA                    CANALY / 'ANALYSE'   /

C------------------  C O D E  ------------------------------------------

C                            If not first call, ANAL/IFND already SAVEd.

      IF( FIRST ) THEN

C                            Get user option relating to TOF analysis
C                            that is, ANALYSE & IFNOTDONE

        CALL TPUOPT( TOF, CANALY, ANAL )
        CALL TPUOPT( TOF, CIFND,  IFND )

        FIRST = .FALSE.

      ENDIF

      IF( ANAL ) THEN

        ANALYS = .TRUE.

        IF( IFND ) THEN

C                            Find the TOFR bank if present. No analysis
C                            is needed if it is as IFNOTDONE was option.

          ITOFR = IDATA( IBLN('TOFR') )

          IF( ITOFR .GT. 0 ) THEN

            ANALYS = .FALSE.

          ENDIF

        ENDIF

      ELSE

        ANALYS = .FALSE.

      ENDIF

C                            Record this in the statistics area

      CALL TPSTAT

      RETURN
      END
