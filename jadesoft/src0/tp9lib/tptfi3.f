C   12/01/88 801121108  MEMBER NAME  TPTFI3   (S)        M  FORTRAN77
C
C-----------------------------------------------------------------------
      SUBROUTINE  TPTFI3( DELETE )
C-----------------------------------------------------------------------
C
C     Author:   C. Bowdery      12/01/88:  Get instr. about TOF bank del
C
C
C
C     Routine to provide yes/no answer about deleting TOFR bank.
C
C-----------------   A R G U M E N T S      ----------------------------
C
C     DELETE    Out      L*4      TRUE if delete step is to be done
C
C------------------  D E F I N I T I O N S  ----------------------------

      IMPLICIT  INTEGER*2 (H)

      CHARACTER  TOF*8, DELET*20

      LOGICAL  DELETE, DEL, FIRST

      SAVE  DEL, FIRST

      DATA  FIRST / .TRUE. /
      DATA  TOF   / 'TOF' /, DELET / 'DELETE' /

C------------------  C O D E  ------------------------------------------

C                            If not first call, DEL already SAVEd.

      IF( FIRST ) THEN

C                            Get user request about TOF  'TOFR' delete

        CALL TPUOPT( TOF, DELET, DEL )

        FIRST = .FALSE.

      ENDIF

C                            Set DELETE equal to DEL

      DELETE = DEL

C                            Record this in the statistics area

      CALL TPSTAT

      RETURN
      END
