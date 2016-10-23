C   03/12/83 312030146  MEMBER NAME  MCVERR   (S)           FORTRAN
C
C-----------------------------------------------------------------------
      SUBROUTINE MCVERR( IERROR,TEXT,VALUE,IVALUE,
     +                   ITEXT2,IEVENT,ITRACK,VALID )
C-----------------------------------------------------------------------
C
C   AUTHOR:   C. BOWDERY   16/11/83 :  PRINTS VALIDATION ERROR MESSAGES
C
C        MOD: C. BOWDERY   22/11/83 :  FOR TEST DISABLING
C   LAST MOD: C. BOWDERY    2/12/83 :  IMPROVEMENTS
C
C   THIS ROUTINE PRINTS THE MESSAGE STORED IN 'TEXT' FOR ERROR CODE
C   'IERROR' UNLESS 20 MESSAGES HAVE BEEN PRINTED OR THE TEST HAS BEEN
C   DISABLED IN COMMON /CVFLAG/. IF THE TEST IS ENABLED, THE ERROR IS
C   RECORDED IN COMMON /CVERR/.
C
C-----------------------------------------------------------------------
C
      IMPLICIT INTEGER*2 (H)
C
      LOGICAL*1 TEXT(1), ENDT, TEXT1(35) , BLANK
C
      LOGICAL VALID , VTEST
C
      COMMON / CVERR  / MESSAG(20)
      COMMON / CVFLAG / VTEST(20)
C
      DATA ENDT / '^' / , BLANK / ' ' /
C
C-------------------  C O D E  -----------------------------------------
C
C                       IS THE VALIDATION FLAG ENABLED FOR THIS ERROR?
C                       IF DISABLED, THEN RETURN WITH VALID = .TRUE.
C
      VALID = .TRUE.
      IF( .NOT. VTEST(IERROR) ) RETURN
      VALID = .FALSE.
C
C                       PRINT NOTHING IF 20 OF THESE ERRORS HAVE ALREADY
C                       BEEN PRINTED.
C
      MESSAG(IERROR) = MESSAG(IERROR) + 1
      IF( MESSAG(IERROR) .GT. 20 )  RETURN
C
C                       TRANSFER 'TEXT' TO 'TEXT1' AFTER CLEARING LATTER
C
      DO  5   N = 1,35
        TEXT1( N ) = BLANK
   5  CONTINUE
C
      DO  10  N = 1,35
        IF( TEXT( N ) .EQ. ENDT ) GO TO 20
          TEXT1( N ) = TEXT( N )
  10  CONTINUE
C
C
C                       SEPARATE THE INTEGER AND REAL ERRORS
C
  20  IF( IERROR .GT. 10 ) GO TO 3
        WRITE(6,2) IERROR,TEXT1,VALUE,ITEXT2,IEVENT,ITRACK
   2    FORMAT(/' *** INPUT DATA ERROR ',I2,' *** ',5X,35A1,' (',G9.3,
     +          A4,')   FOR EVENT ',I6,'  TRACK ',I3,
     +          '    ***')
        RETURN
C
   3  WRITE(6,4) IERROR,TEXT1,IVALUE,ITEXT2,IEVENT,ITRACK
   4  FORMAT(/' *** INPUT DATA ERROR ',I2,' *** ',5X,35A1,' (',I9,
     +          A4,')   FOR EVENT ',I6,'  TRACK ',I3,
     +          '    ***')
      RETURN
      END
