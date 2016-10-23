C   11/01/88 801172319  MEMBER NAME  TPUOPT   (S)        M  FORTRAN77
C
C-----------------------------------------------------------------------
      SUBROUTINE  TPUOPT( NAME, OPTION, ANSWER )
C-----------------------------------------------------------------------
C
C     Author:  C. Bowdery       11/01/88:  Provide required option.
C
C
C
C     Routine to provide one option from the answers_table compiled
C     at the start of the program.
C
C-----------------   A R G U M E N T S      ----------------------------
C
C     NAME      In    Char*8      Name of option group
C     OPTION    In    Char*20     Name of option keyword
C     ANSWER    Out      L*4      TRUE if option keyword specified
C
C------------------  D E F I N I T I O N S  ----------------------------

      IMPLICIT  INTEGER*2 (H)

      LOGICAL  ANSWER

      CHARACTER  NAME*8, OPTION*20

#include "tables.for"

C------------------  C O D E  ------------------------------------------

      DO  30  I = 1,NOG

        IF( NAME .EQ. NTABLE(I) ) THEN

          DO  10  J = 1,NOK

            IF( OPTION .EQ. OTABLE(J) ) THEN

              ANSWER = ATABLE(I,J)

C                            Record success in statistics area

              CALL TPSTAT

              RETURN

            ENDIF

  10      CONTINUE

          CALL TPERRH

        ENDIF

  30  CONTINUE

      CALL TPERRH

      RETURN

      END
