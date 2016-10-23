C   11/01/88 801112204  MEMBER NAME  TPTGI3   (S)        M  FORTRAN77
C
C-----------------------------------------------------------------------
      SUBROUTINE  TPTGI3( DELETE )
C-----------------------------------------------------------------------
C
C     Author:  C. Bowdery       11/01/88:  Get instr on tagg bank delete
C
C
C
C     Routine to provide yes/no answers about deleting tagging banks.
C
C-----------------   A R G U M E N T S      ----------------------------
C
C     DELETE    Out      L*4      TRUE if delete step is to be done
C
C------------------  D E F I N I T I O N S  ----------------------------

      IMPLICIT  INTEGER*2 (H)

      CHARACTER  TAGG*8, DELET*20

      LOGICAL  DELETE, DEL, FIRST

      SAVE  DEL, FIRST

      DATA  FIRST / .TRUE. /
      DATA  TAGG  / 'TAGG' /, DELET / 'DELETE' /

C------------------  C O D E  ------------------------------------------

C                            If not first call, DEL already SAVEd.

      IF( FIRST ) THEN

C                            Get user request relating to tagg delete

        CALL TPUOPT( TAGG, DELET, DEL )

        FIRST = .FALSE.

      ENDIF

C                            Set DELETE equal to DEL

      DELETE = DEL

C                            Record this in the statistics area

      CALL TPSTAT

      RETURN
      END
