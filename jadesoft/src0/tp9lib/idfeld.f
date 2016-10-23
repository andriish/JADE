C   21/12/87 801171518  MEMBER NAME  IDFELD   (S)        M  FORTRAN77
C
C-----------------------------------------------------------------------
      SUBROUTINE  IDFELD( STRING, LENGTH, ICODE, VALUE, VALFLG, IERROR )
C-----------------------------------------------------------------------
C
C     Author:  C. Bowdery       21/12/87:  Identify string
C
C
C
C     Routine to match input string against dictionary using CMPDCT
C     and to return corresponding ICODE  or, if a number, return VALUE
C     along with VALFLG = .TRUE. .
C
C-----------------   A R G U M E N T S      ----------------------------
C
C     STRING    In   CHAR*(*)     String of characters with no blanks
C     LENGTH    In       I*4      Length of STRING
C     ICODE     Out      I*4      Number code as in dictionary
C     VALUE     Out      R*4      Value of STRING if a number
C     VALFLG    Out      L*4      .TRUE. if string is a number
C     IERROR    Out      i*4      Error code
C                                  0 : no error
C                                  1 : string not in dictionary
C                                      or number was unrecognisable
C                                  2 : programming error; no operation
C
C------------------  D E F I N I T I O N S  ----------------------------

      IMPLICIT  INTEGER*2 (H)

      CHARACTER*(*)  STRING

      INTEGER  ICODE,  LENGTH,  IERROR

      REAL  VALUE

      LOGICAL  IER,  VALFLG

C------------------  C O D E  ------------------------------------------

C                            Is STRING a number?

      CALL STRNUM( STRING, LENGTH, VALUE, IER )

C                            Error return means STRING is not a number
C                            So, is STRING a known word?

      IF( IER ) THEN

        CALL CMPDCT( STRING, LENGTH, ICODE, IERROR )
        VALFLG = .FALSE.

      ELSE

        VALFLG = .TRUE.
        IERROR = 0

      ENDIF
      RETURN
      END
