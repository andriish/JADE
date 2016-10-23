C   22/04/80 004221939  MEMBER NAME  MUTIMA   (JADEMUS)     FORTRAN
C=======================================================================
C THIS ROUTINE CAN BE USED TO TIME PROGRAMS
C=======================================================================
C  APRIL 1980  HARRISON B. PROSPER     J A D E
C=======================================================================
      SUBROUTINE MUTIMA (TIMDIF)
      LOGICAL FIRST /.TRUE./
      LOGICAL*1 C/'C'/,TYPE
C=======================================================================
C-----NTIME(DUM) GIVES THE TIME LEFT IN 1/100 SECS
C-----MODELL(TYPE) GIVES THE MACHINE THE JOB IS RUNNING ON.
C-----IF THE JOB IS RUNNING ON MACHINE C,SCALE UP THE TIME LEFT
C-----BY 1.7,SINCE MACHINE C IS FASTER.
      BIGBEN = NTIME(DUM)
      BIGBEN = BIGBEN/100
C-----ON FIRST CALL TO TIMER GET FIND OUT WHICH MACHINE
C-----THE JOB IS RUNNING ON
      IF(.NOT.FIRST) GOTO 2
      CALL MODELL(TYPE)
      FIRST = .FALSE.
                   SCALE = 1.0
      IF(TYPE.EQ.C)SCALE = 1.7
      TIME0  = BIGBEN
 2    CONTINUE
      TIMDIF = SCALE*( TIME0 - BIGBEN )
      RETURN
      END
