C   11/01/88 801131646  MEMBER NAME  TPTGI1   (S)        M  FORTRAN77
C
C-----------------------------------------------------------------------
      SUBROUTINE  TPTGI1( ANALYS )
C-----------------------------------------------------------------------
C
C     Author:   C. Bowdery      11/01/88:  Get instructions about tagan
C
C     Last mod: C. Bowdery      13/01/88:  NPACLS --> IACLS
C
C     Routine to provide yes/no answer about need for tagging analysis.
C
C-----------------   A R G U M E N T S      ----------------------------
C
C     ANALYS    Out      L*4      TRUE if analysis step is to be done
C
C------------------  D E F I N I T I O N S  ----------------------------

      IMPLICIT  INTEGER*2 (H)

      CHARACTER  TAGG*8, CIFND*20, CANALY*20

      LOGICAL  ANALYS, ANAL, IFND, FIRST

      INTEGER  LISTBK(10), IACLS

      SAVE  FIRST, ANAL, IFND

      DATA  FIRST / .TRUE. /
      DATA  TAGG  / 'TAGG' /, CIFND  / 'IFNOTDONE' /
      DATA                    CANALY / 'ANALYSE'   /

C------------------  C O D E  ------------------------------------------

C                            If not first call, ANAL/IFND already SAVEd.

      IF( FIRST ) THEN

C                            Get user option relating to tagg analysis
C                            that is, ANALYSE & IFNOTDONE

        CALL TPUOPT( TAGG, CANALY, ANAL )
        CALL TPUOPT( TAGG, CIFND,  IFND )

        FIRST = .FALSE.

      ENDIF

      IF( ANAL ) THEN

        ANALYS = .TRUE.

        IF( IFND ) THEN

C                            Find the compulsory tagging banks

          CALL CLOC( IACLS, 'ACLS', 0 )
          CALL BDAR( 'TAGG', MAXNUM, LISTBK, 10 )

C                            No 'ACLS'/0 or less than 3 'TAGG' banks?

          IF( IACLS .GT. 0  .AND.  MAXNUM .EQ. 3 ) THEN

C                            If 'TAGG'/0-2 all present and re-analysis
C                            not requested then do nothing


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
