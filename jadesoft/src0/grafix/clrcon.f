C   15/07/85 507150248  MEMBER NAME  CLRCON   (S)           FORTRAN
C
C-----------------------------------------------------------------------
      SUBROUTINE CLRCON
C-----------------------------------------------------------------------
C
C   AUTHOR:   C. BOWDERY  15/07/85 :  CLEARS COMMAND TERMINAL SCREEN
C
C
C     USES  COMMON / CGAMOD / TO DETERMINE WHETHER GRAPHICS IS
C     RUNNING IN GA MODE OR NOT. IF GA MODE, S/R CLEAR IS CALLED. IF
C     NOT THEN S/R ERASE IS CALLED.
C
C-----------------------------------------------------------------------
C
      IMPLICIT INTEGER*2 (H)
C
      COMMON / CGAMOD / MODEGA, GAHEX
C
C-------------------  C O D E  -----------------------------------------
C
      IF( MODEGA .EQ. 1 ) GO TO 1
        CALL ERASE
        RETURN
C
   1    CALL CLEAR
        RETURN
      END
