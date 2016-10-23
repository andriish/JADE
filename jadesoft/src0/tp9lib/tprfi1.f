C   18/01/88 807262112  MEMBER NAME  TPRFI1   (S)        M  FORTRAN77
C
C-----------------------------------------------------------------------
      SUBROUTINE  TPRFI1( REFITR, MODE )
C-----------------------------------------------------------------------
C
C     Author:   C. Bowdery      18/01/88:  Get instr. about R-Phi refit
C
C          mod: C. Bowdery      22/07/88:  Add 32 to MODE for JHTL
C     Last mod: C. Bowdery      26/07/88:  Change to TPINQR and response
C
C     Routine to provide yes/no answer about need for inner detector
C     R-Phi refit and if wanted, the refitting mode. See JCN 94.
C
C-----------------   A R G U M E N T S      ----------------------------
C
C     REFITR    Out      L*4      TRUE if R-FI refit step is to be done
C     MODE      Out      I*4      MODE value needed for s/r XYRFTV
C
C------------------  D E F I N I T I O N S  ----------------------------

      IMPLICIT  INTEGER*2 (H)

      CHARACTER  REFIT*8, CIFNOR*20, CRFI*20
      CHARACTER           COLDP*20, CWEAK*20, CSTRNG*20

      LOGICAL  RFI, IFNOR, FIRST, OLDPAT, WEAK, STRONG

      LOGICAL  REFITR, NOV, WEAKV, STRNGV

      INTEGER  MODE

      SAVE  FIRST, RFI, IFNOR, OLDPAT, WEAK, STRONG

      DATA  FIRST / .TRUE. /
      DATA  REFIT / 'REFIT' /,  CIFNOR  / 'IFNORFI' /
      DATA  COLDP / 'OLDPATR' /, CRFI   / 'RFI'   /
      DATA  CWEAK / 'WEAKVTX' /, CSTRNG / 'STRONGVTX' /

C------------------  C O D E  ------------------------------------------

C                            If not first call, IFNOR etc already SAVEd.

      IF( FIRST ) THEN

C                            Get user option relating to rfi refitting
C                            that is, RFI, IFNORFI, OLDPATR, WEAKVTX
C                            and  STRONGVTX.

        CALL TPUOPT( REFIT, CRFI,   RFI    )
        CALL TPUOPT( REFIT, CIFNOR, IFNOR  )
        CALL TPUOPT( REFIT, COLDP,  OLDPAT )
        CALL TPUOPT( REFIT, CWEAK,  WEAK   )
        CALL TPUOPT( REFIT, CSTRNG, STRONG )

        FIRST = .FALSE.

      ENDIF

C                            If RFI specified then set REFITR and MODE
C                            then handle IFNORFI condition if specified.

      IF( RFI ) THEN

        REFITR = .TRUE.

C                            Specify if new PATR bank to be created

        IF( OLDPAT ) THEN
          MODE = 0
        ELSE
          MODE = 1
        ENDIF

C                            Specify vertex constraint (no,weak,strong)

        IF( WEAK ) THEN
          MODE = MODE + 4
        ELSEIF( .NOT. STRONG ) THEN
          MODE = MODE + 16
        ENDIF

C                            Set the create/update option for JHTL

        MODE = MODE + 32

C                            Was IFNORFI condition specified? If so,
C                            determine whether refit needed.

        IF( IFNOR ) THEN

C                            No/weak/strong vertex constraint Spitzer
C                            fit on latest PATR bank?

          CALL TPINQR( NOV, WEAKV, STRNGV )

C                            If so and if it matches what is needed,
C                            then no new refit required.

          IF( ( WEAKV   .AND.  WEAK   )  .OR.
     +        ( STRNGV  .AND.  STRONG )  .OR.
     +        ( NOV     .AND.  .NOT. ( STRONG  .OR.  WEAK ) ) ) THEN
            REFITR = .FALSE.
            MODE   = 0
          ENDIF
        ENDIF

      ELSE

C                            No refit wanted.

        REFITR = .FALSE.
        MODE   = 0

      ENDIF

C                            Record this in the statistics area

      CALL TPSTAT

      RETURN
      END
