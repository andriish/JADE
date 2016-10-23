C   22/05/84 801182011  MEMBER NAME  LOCFLD   (JADEGS)      FORTRAN
C
C-----------------------------------------------------------------------
      SUBROUTINE LOCFLD( CHARS, NFIELD, ISTAFL, IENDFL )
C-----------------------------------------------------------------------
C
C   AUTHOR:   C. BOWDERY  22/05/84 :  LOCATE THE FIELDS IN THE COMMAND
C
C LAST MOD:   C. BOWDERY  16/07/85 :  SEPARATOR = ' ' OR ';'
C
C
C     THIS ROUTINE FINDS THE STARTING AND ENDING POSITIONS OF FIELDS IN
C     THE COMMAND STRING CHARS. THE NO. OF FOUND FIELDS IS PUT IN NFIELD
C     ISTAFL IS AN ARRAY OF STARTING POINTS; IENDFL, ENDING POINT.
C
C-----------------------------------------------------------------------
C
      IMPLICIT INTEGER*2 (H)
C
      LOGICAL  GAP
C
      LOGICAL*1  CHARS(80), CBLANK, CSEMIC
C
      DIMENSION  ISTAFL(80), IENDFL(80)
C
      DATA  CBLANK /' '/, CSEMIC /';'/
C
C-------------------  C O D E  -----------------------------------------
C
      NFIELD = 0
      GAP    = .TRUE.
C
C                            THE FOLLOWING CASES EXIST
C                                  XXXX  YYYY              (1)
C                                  XXXX; YYYY              (2)
C                                  XXXX;YYYY               (3)
C                                  XXXX ; YYYY             (4)
C                                  XXXX ;YYYY              (5)
C
      DO  20  I = 1,80
C
C                            IS THIS A SEMI-COLON ';' ?
C
        IF( CHARS(I) .NE. CSEMIC ) GO TO 8
          IF( GAP ) GO TO 4
C                                                CASES (4) AND (5)
            IENDFL(NFIELD) = I - 1
            NFIELD         = NFIELD + 1
            ISTAFL(NFIELD) = I
            IENDFL(NFIELD) = I
            GAP            = .TRUE.
            GO TO 20
C                                                CASES (2) AND (3)
   4        NFIELD         = NFIELD + 1
            ISTAFL(NFIELD) = I
            IENDFL(NFIELD) = I
            GO TO 20
C                                                CASE  (1)
C
   8    IF( .NOT. GAP  .OR.  CHARS(I) .EQ. CBLANK ) GO TO 10
          GAP            = .FALSE.
          NFIELD         = NFIELD + 1
          ISTAFL(NFIELD) = I
          GO TO 20
C
  10    IF( GAP  .OR.  CHARS(I) .NE. CBLANK ) GO TO 20
          GAP            = .TRUE.
          IENDFL(NFIELD) = I - 1
C
  20  CONTINUE
C
C                                                LAST FIELD IS A CHAR
C
      IF( .NOT. GAP ) IENDFL(NFIELD) = 80
C
      RETURN
      END
