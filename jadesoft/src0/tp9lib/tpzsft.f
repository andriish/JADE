C   28/07/87 801191014  MEMBER NAME  TPZSFT   (S)        M  FORTRAN77
C
C-----------------------------------------------------------------------
      SUBROUTINE  TPZSFT
C-----------------------------------------------------------------------
C
C     Author:   C. Bowdery      28/07/87:  Do central det. Z-S refit
C
C
C     Routine to do Z-S refit of inner detector tracks  using Z chamber
C     information where available  (optional)
C
C------------------  D E F I N I T I O N S  ----------------------------

      IMPLICIT  INTEGER*2 (H)

      LOGICAL  REFITZ

C                            MODE is refitting mode (See JCN 95)
C                            IOPT is refitting options code

      INTEGER  MODE, IOPT

C------------------  C O D E  ------------------------------------------

C                            Get instruction about refitting

      CALL TPRFI2( REFITZ, MODE, IOPT )

C                            If Z-S refit wanted, do it.

      IF( REFITZ ) THEN

        CALL ZSRFTV( MODE, IOPT )

        CALL TPDIAG()

      ENDIF

      RETURN

      END
