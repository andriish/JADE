C   18/01/88 807262227  MEMBER NAME  TPRFI2   (S)        M  FORTRAN77
C
C-----------------------------------------------------------------------
      SUBROUTINE  TPRFI2( REFITZ, MODE, IOPT )
C-----------------------------------------------------------------------
C
C     Author:   C. Bowdery      18/01/88:  Get instr. about Z-S refit
C
C          mod: C. Bowdery       2/02/88:  COMMONZVTX not COMMONVTX now
C          mod: C. Bowdery      22/07/88:  New PATR only if rphi not new
C     Last mod: C. Bowdery      26/07/88:  Change to TPINQZ and response
C
C     Routine to provide yes/no answer about need for central detector
C     Z-S refit and if wanted, the refitting mode. See JCN 95.
C
C-----------------   A R G U M E N T S      ----------------------------
C
C     REFITZ    Out      L*4      TRUE if Z-S  refit step is to be done
C     MODE      Out      I*4      MODE value needed for s/r ZSRFTV
C     IOPT      Out      I*4      IOPT value needed for s/r ZSRFTV
C
C------------------  D E F I N I T I O N S  ----------------------------

      IMPLICIT  INTEGER*2 (H)

      CHARACTER  REFIT*8, CIFNOZ*20, CZS*20
      CHARACTER  CRFI*20, COMVTX*20, COLDP*20

      LOGICAL  ZS, IFNOZ, FIRST, OLDPAT, COMVER, RFI
      LOGICAL  REFITZ, INDEP, WICOMV

      INTEGER  MODE, IOPT

      SAVE  FIRST, ZS, IFNOZ, OLDPAT, COMVER, RFI

      DATA  FIRST / .TRUE. /
      DATA  REFIT / 'REFIT' /,  CIFNOZ  / 'IFNOZS' /
      DATA  COLDP / 'OLDPATR' /, CZS    / 'ZS'   /
      DATA  CRFI  / 'RFI'   /
      DATA  COMVTX/ 'COMMONZVTX' /

C------------------  C O D E  ------------------------------------------

C                            If not first call, IFNOZ etc already SAVEd.

      IF( FIRST ) THEN

C                            Get user option relating to Z-S refitting
C                            that is, ZS, IFNOZS, OLDPATR, COMMONZVTX
C                            Also get RFI option.

        CALL TPUOPT( REFIT, CZS,    ZS     )
        CALL TPUOPT( REFIT, CIFNOZ, IFNOZ  )
        CALL TPUOPT( REFIT, COLDP,  OLDPAT )
        CALL TPUOPT( REFIT, COMVTX, COMVER )
        CALL TPUOPT( REFIT, CRFI,   RFI    )

        FIRST = .FALSE.

      ENDIF

      IF( ZS ) THEN

        REFITZ = .TRUE.

C                            Don't ask for a new PATR if OLDPATR
C                            selected or if a new one was made for RFI

        IF( OLDPAT  .OR.  RFI ) THEN
          MODE = 0
        ELSE
          MODE = 1
        ENDIF

C                            Set option pertaining to common_z_vertex

        IF( COMVER ) THEN
          IOPT = 2
        ELSE
          IOPT = 1
        ENDIF

C                            Handle conditional fit case

        IF( IFNOZ) THEN

C                            Has the latest PATR bank been z refitted
C                            with or without a common_z_vertex ?
C                            If no z refit, both flags are FALSE.

          CALL TPINQZ( INDEP, WICOMV )

C                            If desired fit already exists, cancel refit

          IF( ( WICOMV  .AND.        COMVER )  .OR.
     +        ( INDEP   .AND.  .NOT. COMVER )       ) THEN
            REFITZ = .FALSE.
            MODE   = 0
            IOPT   = 0
          ENDIF

        ENDIF

      ELSE

        REFITZ = .FALSE.
        MODE   = 0
        IOPT   = 0

      ENDIF

C                            Record this in the statistics area

      CALL TPSTAT

      RETURN
      END
