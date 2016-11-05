C   22/04/80 403232136  MEMBER NAME  MUTIM    (JADEMUS)     FORTRAN
C
C-----------------------------------------------------------------------
      SUBROUTINE MUTIM(SECS,*)
C-----------------------------------------------------------------------
C
C        NOVEMBER 1979  HARRISON B. PROSPER
C
C THIS ROUTINE FINDS HOW MUCH TIME IS LEFT FOR PROCESSING.IT CAN BE
C USED TO FORCE AN EXIT ( FROM A LOOP FOR EXAMPLE )
C SECS IS A TIME-OUT LIMIT AFTER WHICH A RETURN 1 WILL OCCUR
C
C------------------  C O D E  ------------------------------------------
C
C                        NTIME(DUM) GIVES THE TIME LEFT IN 1/100 SECS
C
      BIGBEN = NTIME(DUM)
      BIGBEN = BIGBEN/100
C
      IF( BIGBEN .LE. (SECS + 0.001) ) RETURN1
      RETURN
      END
