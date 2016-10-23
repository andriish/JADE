C   08/07/85 507082013  MEMBER NAME  PROMPT   (S)           FORTRAN
C
C-----------------------------------------------------------------------
      SUBROUTINE PROMPT
C-----------------------------------------------------------------------
C
C        HOLD THE PROGRAM BY PROMPTING THE SCANNER TO RETURN
C
C-----------------------------------------------------------------------
C
      IMPLICIT INTEGER*2 (H)
C
      CALL TRMOUT(80,'Press ENTER to proceed.^')
      CALL TRMIN(1,HDUMM)
      RETURN
      END
