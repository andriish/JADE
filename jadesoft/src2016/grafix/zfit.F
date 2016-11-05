C   22/04/87 706161856  MEMBER NAME  ZFIT     (S)           FORTRAN77
      SUBROUTINE ZFIT
C-----------------------------------------------------------
C  Version of 14/05/87          Last Mod 05/06/87  E Elsen
C  Display z-s-Fit results for ZSFIT or ZSRFTV depending
C  on selection of parameters.
C-----------------------------------------------------------
      IMPLICIT INTEGER*2 (H)
      COMMON / BCS / IW(1)
#include "czsprm.for"
#include "cgraph.for"
      COMMON / CGRAP2 / BCMD, DSPDTM(30), ISTVW, JTVW
C                                           MODEGA = 1 FOR GA
      COMMON / CGAMOD / MODEGA, GAHEX
C
      INTEGER PLOTW(4)
      LOGICAL LDISPL, LZSPD, LHELP
      CHARACTER*80 CLINE
      CHARACTER*1 ANSWER
      CHARACTER*4 CHELP
C
C
      LZSPD = IW(IBLN('ZSPD')).GT.0
      NPPATR = IW(IBLN('PATR'))
      IF( NPPATR.GT.0 ) THEN
        NTR = IW(NPPATR+2)
      ELSE
        NTR = 0
      ENDIF
C
C                                           ZSFIT( 1 ) HAS TO BE DONE TO
C                                           INIT RUN RANGES PROPERLY.
      MODE = 1
      CALL ZSFIT( MODE ,0,0,0,0,1) ! PMF 03/12/99: four dummy arguments + 6th argument added
C                                           ADDITIONAL PARMS
C                                           ON INPUT?
      IF( ACMD.GT.0. .AND. ACMD .LT. 5. ) THEN
        ANSWER = CHAR(IFIX(ACMD+.5)+ICHAR('0'))
      ELSE
        ANSWER = ' '
      ENDIF
      IF( BCMD.GT.0. ) THEN
        ITR = BCMD + .5
      ELSE
        ITR = 0
      ENDIF
C
   10 NPZSPD = IW(IBLN('ZSPD'))
      LDISPL = .FALSE.
      IF(  ANSWER .EQ. ' ' ) THEN
C                                           SELECT FIT TYPE
C                                           IF NOT YET DONE
        IF( MODEGA .EQ. 1 ) THEN
          CALL CLEAR
          LHELP = .TRUE.
        ELSE
          CALL TRMOUT( 80,
     +       ' Z-S-FIT Package. Enter Selection Number (0-4 or HELP)^')
          LHELP = .FALSE.
        ENDIF
   11   IF( LHELP ) THEN
          CALL TRMOUT(80, ' Z-S Fit Package^')
          CALL TRMOUT(80, ' ---------------^')
          CALL TRMOUT(80, ' Display Selection Level:^')
          CALL TRMOUT(80, '                         ^')
          CALL TRMOUT(80, '   0.Quit...........................^')
          IF( LZSPD ) THEN
            CALL TRMOUT(80, '   1.ZSPD Bank......................^')
            CALL TRMOUT(80,
     *             '   or select fit type (overwriting ZSPD banks)^')
          ELSE
            CALL TRMOUT(80, '   or select fit type ^')
          ENDIF
          CALL TRMOUT(80, '   2.ZSFIT single track (PD ).......^')
          CALL TRMOUT(80, '   3.ZSRFTV single track.(JS * EE)..^')
          CALL TRMOUT(80, '   4.ZSRFTV common Z-Fit.(JS * EE)..^')
        ENDIF
        CALL TRMIN(80,CLINE)
        NB = 1
        DO 12 WHILE( CLINE(NB:NB) .EQ. ' ' .AND. NB.LT.80 )
          NB = NB + 1
   12   CONTINUE
        CHELP = CLINE(NB:MIN0(NB+3,80))
        IF( CHELP .EQ. 'HELP' ) THEN
          LHELP = .TRUE.
          GO TO 11
        ENDIF
        ANSWER = CLINE(NB:NB)
      ENDIF
      IF( ANSWER .EQ. '1' .AND. LZSPD ) THEN
C                                           USE AVAILABLE INPUT
        LDISPL = .TRUE.
      ELSEIF( ANSWER .EQ. '2' ) THEN
        CALL BMLT(1,'ZSPD')
        CALL BDLM
        LZSPD = .FALSE.
        LZSPDF = .TRUE.
        MODE = 0
        CALL ZSFIT( MODE,0,0,0,0,1 )! PMF 03/12/99: four dummy arguments + 6th argument added
        LZSPDF = .FALSE.
        LDISPL = .TRUE.
      ELSEIF( ANSWER .EQ. '3' ) THEN
        CALL BMLT(1,'ZSPD')
        CALL BDLM
        LZSPD = .FALSE.
        IOPT = 1 + 8
        MODE = 0
        CALL ZSRFTV( MODE, IOPT )
        LDISPL = .TRUE.
      ELSEIF( ANSWER .EQ. '4' ) THEN
        CALL BMLT(1,'ZSPD')
        CALL BDLM
        LZSPD = .FALSE.
        IOPT = 2 + 8
        MODE = 0
        CALL ZSRFTV( MODE, IOPT )
        LDISPL = .TRUE.
      ENDIF
C                                           RESET ANSWER
      ANSWER = ' '
C
      IF( LDISPL ) THEN
        CALL SEETW ( PLOTW(1), PLOTW(2), PLOTW(3), PLOTW(4) )
  110   IF( ITR .EQ. 0 ) THEN
          WRITE(CLINE,9101) NTR
 9101     FORMAT(' Enter Track Number (1-',I2,')^')
          CALL TRMOUT(80, CLINE )
          CALL TRMIN(80, CLINE)
          NB = 1
          DO 111 WHILE( CLINE(NB:NB) .EQ. ' ' .AND. NB.LT.80 )
            NB = NB + 1
  111     CONTINUE
          ANSWER = CLINE(NB:NB)
          IF( ANSWER .NE. ' ' ) READ(CLINE,*) ITR
        ENDIF
  120   IF( ITR.NE.0 ) THEN
          ITROLD = ITR
          CALL ZFITDS( ITR )
          IF( ITR .NE. ITROLD ) THEN
            ITR = IABS(ITR)
            GO TO 120
          ENDIF
C         ITR = 0
C         GO TO 110
        ENDIF
        ITR = 0
        CALL TWINDO( PLOTW(1), PLOTW(2), PLOTW(3), PLOTW(4) )
        CALL SETSCL(JTVW )
        LDISPL = .FALSE.
        ANSWER = ' '
        GO TO 10
      ENDIF
      IF( .NOT. LZSPD ) THEN
        CALL BMLT(1,'ZSPD')
        CALL BDLM
      ENDIF
      END
