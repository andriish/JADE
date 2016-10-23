C   17/05/88 805171202  MEMBER NAME  JOBFIN   (S)           FORTRAN77
      SUBROUTINE JOBFIN( ICODE, IEV, * )
C----------------------------------------------------------
C  VERSION OF 14/10/80    LAST MOD 14/10/80    E.ELSEN
C  PRINT OUT MESSAGES AT END OF JOB AND DURING JOB PROCESSING
C  RETURN IS RETRUN 1
C----------------------------------------------------------
C
      GO TO ( 100, 200, 300, 400 ), ICODE
      RETURN
C
C                                           DATA END
  100 WRITE(6,9101) IEV
 9101 FORMAT(1X,20(1H-),'   DATA END  AFTER ',I7,' EVENTS ',20(1H-) )
      GO TO 8000
C
C                                           TIME OUT
  200 WRITE(6,9102) IEV
 9102 FORMAT(1X,20(1H-),'   TIME OUT  AFTER ',I7,' EVENTS ',20(1H-) )
      GO TO 8000
C
C                                           RAD ERROR
  300 WRITE(6,9103) IEV
 9103 FORMAT(1X,20(1H-),'  READ ERROR AFTER ',I7,' EVENTS ',20(1H-) )
      GO TO 8000
C                                           RAD ERROR
  400 WRITE(6,9104) IEV
 9104 FORMAT(1X,20(1H-),' IRECMX EXCEEDED AFTER ',I7,' EVENTS ',20(1H-))
C
 8000 CONTINUE
      RETURN 1
      END
