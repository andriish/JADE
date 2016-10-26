C   29/04/85 703130936  MEMBER NAME  KALIBR   (JADEGS)      FORTRAN
C
C-----------------------------------------------------------------------
      SUBROUTINE KALIBR
C-----------------------------------------------------------------------
C
C        HANDLES STANDARD JADE CALIBRATION DATA
C
C   FRIDAY 13.3.1987   CALL TO CNEWID ADDED, RUN VERTICES FOR NEW ID CAL
C                J.OLSSON & J. SPITZER
C-----------------------------------------------------------------------
C
      IMPLICIT INTEGER*2 (H)
C                                       FORCE LOADING OF JADE BLOCK DATA
      EXTERNAL JADEBD
C
C------------------  C O D E  ------------------------------------------
C
      CALL RUNFIX
C
      write(*,*)   'here was kal'
C      CALL KLREAD
      CALL CNEWID(0)
C
      CALL MUREG(0)
C
      RETURN
      END
