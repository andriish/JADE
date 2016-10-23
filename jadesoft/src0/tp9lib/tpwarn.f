C   22/01/88 802012154  MEMBER NAME  TPWARN   (S)        M  FORTRAN77
C
C-----------------------------------------------------------------------
      SUBROUTINE  TPWARN
C-----------------------------------------------------------------------
C
C     Author:   C. Bowdery      22/01/88:  Handle TP warnings
C
C
C
C     Routine to handle the TP warning calls.
C
C------------------  D E F I N I T I O N S  ----------------------------

      IMPLICIT  INTEGER*2 (H)

      PARAMETER  ( NUM = 20 )

      CHARACTER  NAMES(NUM)*8

      INTEGER  MTRACE, K

C------------------  C O D E  ------------------------------------------

      WRITE(6,10)
  10  FORMAT(/'    ------> TPWARN called  ')

      MTRACE = ITRACE( NAMES, NUM )

      WRITE(6,20) (NAMES(K),K=2,MTRACE)
 20   FORMAT(/'            T R A C E B A C K '//20(17X,A8/))

      WRITE(6,30)
 30   FORMAT('     E N D   O F   T R A C E B A C K'//)

      RETURN
      END
