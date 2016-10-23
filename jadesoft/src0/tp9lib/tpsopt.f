C   17/01/88 802041825  MEMBER NAME  TPSOPT   (S)        M  FORTRAN77
C
C-----------------------------------------------------------------------
      SUBROUTINE  TPSOPT( CTPOPT, COPT, IERROR )
C-----------------------------------------------------------------------
C
C     Author:  C. Bowdery       17/01/88:  Store TP options
C
C
C
C     Routine to store TP options for option group COPT.
C     Special records (COPT = 'SPECIAL ') are not stored here.
C
C-----------------   A R G U M E N T S      ----------------------------
C
C     CTPOPT    In   CHAR*20      Array of options
C     COPT      In    CHAR*8      Option group name
C                                 = 'SPECIAL ' if special option record
C     IERROR    Out      I*4      Error code variable
C                                  11 :  Duplicate option record
C                                  12 :  Option keyword not in TABLE ?
C
C------------------  D E F I N I T I O N S  ----------------------------

      IMPLICIT  INTEGER*2 (H)

      PARAMETER  ( NOPT = 20 )

      INTEGER  IERROR

#include "tables.for"

      LOGICAL  DONE(NOPTG)

      CHARACTER   COPT*8, CTPOPT( NOPT )*20

      SAVE  DONE

      DATA DONE / NOPTG* .FALSE. /

C------------------  C O D E  ------------------------------------------

      IERROR = 0

      IF( COPT .EQ. 'SPECIAL ' ) RETURN


      DO  30  I = 1,NOG

        IF( COPT .EQ. NTABLE(I) ) THEN

C                            First check that this group has not already
C                            appeared on a previous input record.

          IF( .NOT. DONE(I) ) THEN

            DONE(I) =.TRUE.

            DO  20  J = 1,NOPT
              IF( CTPOPT(J) .EQ. ' ' ) RETURN
              DO  10  K = 1,NOK
                IF( CTPOPT(J) .EQ. OTABLE(K) ) THEN
                  ATABLE(I,K) = .TRUE.
                  GO TO 20
                ENDIF
  10          CONTINUE

C                            Should never get here

              IERROR = 12
              CALL TPSOEA( 81, IERROR )
              CALL TPERRH
              RETURN

  20        CONTINUE
          ELSE

C                            Duplicate option record. Set error array

            IERROR = 11
            CALL TPSOEA( 81, IERROR )

          ENDIF

C                            Record processed so end loop now

          RETURN

        ENDIF

  30  CONTINUE

C                            Should never get here

      IERROR = 12
      CALL TPSOEA( 81, IERROR )
      CALL TPERRH

      RETURN
      END
