C   29/01/88 802041650  MEMBER NAME  TPSOEA   (S)        M  FORTRAN77
C
C-----------------------------------------------------------------------
      SUBROUTINE  TPSOEA( IPOS, IERROR )
C-----------------------------------------------------------------------
C
C     Author:   C. Bowdery      29/01/88:  Set option error array
C
C
C
C     Routine to set up array of marker positions and error numbers
C     associated with illegal input options.
C
C-----------------   A R G U M E N T S      ----------------------------
C
C     IPOS      In       I*4      Position of error within input string
C     IERROR    In       I*4      Error number
C
C------------------  D E F I N I T I O N S  ----------------------------

      IMPLICIT  INTEGER*2 (H)

      INTEGER  NINREC, IERRM, NINR, NERRM, KPOS

      PARAMETER  ( NINR = 25, NERRM = 10 )

      COMMON / CTPIR2 / NINREC, KPOS, IERRM(NINR,NERRM,2)

C------------------  C O D E  ------------------------------------------

      IF( KPOS .LT. NERRM ) THEN
        KPOS   = KPOS + 1
        IERRM(NINREC,KPOS,1) = IPOS
        IERRM(NINREC,KPOS,2) = IERROR

        IF( KPOS .NE. NERRM ) THEN
          IERRM(NINREC,KPOS+1,1) = 0
        ENDIF
      ENDIF

      RETURN
      END
