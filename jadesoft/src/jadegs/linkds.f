C   13/03/84 403132043  MEMBER NAME  LINKDS   (JADEGS)      FORTRAN
C
C-----------------------------------------------------------------------
         SUBROUTINE  LINKDS(LUNC,JNAME,HERR,DDN)
C-----------------------------------------------------------------------
C
C   AUTHOR:  C. BOWDERY    ??/??/81 : LINKS A HSM/MSS DATASET
C
C
C
C         THIS SUBROUTINE LINKS AN HSM/MSS DATASET TO A LOGICAL
C         UNIT NUMBER AND HANDLES ERRORS.
C
C         JNAME   -   ARRAY OF 44 CHARS CONTAINING DSNAME
C         LUNC    -   VARIABLE HOLDING LOGICAL UNIT NO.
C         HERR    -   RETURN ERROR VARIABLE
C         DDN     -   DOUBLE PRECISION (REAL*8) VARIABLE
C                     (CONTAINS 'FT..F001' ON RETURN)
C         IF LUNC=0 ON ENTRY, THE VALUE OF LUNC ON EXIT WILL BE
C         DECIDED BY SUBROUTINE GETPDD (FROM R02SCH.TSOIPS.LOAD).
C         IF JNAME HAS LEADING BLANKS, THEY WILL BE REMOVED.
C
C-----------------------------------------------------------------------
C
      IMPLICIT INTEGER*2 (H)
C
      LOGICAL*1 JNAME,BLANK
      REAL*8 DDN,VOLSR
C
      DIMENSION JNAME(44)
C
      DATA BLANK/' '/,VOLSR/'        '/
C
C------------------  C O D E  ------------------------------------------
C
C                  SCAN ARRAY JNAME CONTAINING THE DSNAME TO FIND
C                  LEADING BLANKS AND SHIFT LEFT TO REMOVE THEM.
C
      IF( JNAME(1) .NE. BLANK ) GO TO 4
      DO  5  I = 2,44
        IF( JNAME(I) .NE. BLANK ) GO TO 2
 5    CONTINUE
C                  IF NO NAME GIVEN (ONLY 'ENTER')
C                  THEN RETURN WITH 'NO NAME GIVEN' ERROR (=100)
      HERR = 100
      RETURN
C
C                  SHIFT LEFT TO REMOVE BLANKS BUT KEEP TRAILING BLANKS
C
 2    DO  6  J = 1,44
        JNAME(J) = BLANK
        K = 44 - I + 1
        IF( J .LE. K ) JNAME(J) = JNAME(J+I-1)
 6    CONTINUE
C
C
C                  CALL GETPDD TO ALLOCATE LOGICAL UNIT NUMBER LUNC
C                  TO THE CALIBRATION DATASET. IF LUNC IS ZERO, GETPDD
C                  CHOOSES A VALUE. ON RETURN, HERR SHOULD BE ZERO.
C
 4    CALL GETPDD(JNAME,VOLSR,DDN,LUNC,HERR)
C
C                  IF HERR IS 'DDNAME ALREADY ALLOCATED',FREE THE DDNAME
C
      IF( HERR .NE. 1040 ) RETURN
        REWIND LUNC
        CALL FREEDD(DDN,HERROR)
        IF( HERROR .EQ. 0 ) GO TO 4
        HERR = - HERROR
        RETURN
      END
