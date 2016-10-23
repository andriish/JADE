C   09/01/89 901091422  MEMBER NAME  CMPDCT   (S)        M  FORTRAN77
C
C-----------------------------------------------------------------------
      SUBROUTINE  CMPDCT( STRING, LENGTH, ICODE, IERROR )
C-----------------------------------------------------------------------
C
C     Author:  C. Bowdery       21/12/87:  Get ICODE for input STRING
C
C
C
C     Routine to match input string against dictionary held in COMMON
C     and to return corresponding ICODE.
C
C-----------------   A R G U M E N T S      ----------------------------
C
C     STRING    In   CHAR*(*)     String of characters with no blanks
C     LENGTH    In       I*4      Length of STRING
C     ICODE     Out      I*4      Number code as in dictionary
C     IERROR    Out      I*4      Error code
C                                  0 : no error and match found
C                                  1 : no match found
C                                  2 : illegal input; no match possible
C
C------------------  D E F I N I T I O N S  ----------------------------

      IMPLICIT  INTEGER*2 (H)

      CHARACTER*(*)  STRING

      INTEGER  ICODE,  LENGTH,  IERROR

C                            The dictionary words for a given length

      INTEGER  SHORT, LONG

      PARAMETER ( SHORT = 8, LONG = 20 )

      CHARACTER  CDICTS*(SHORT),  CDICTL*(LONG)

      COMMON / CSDICT / CDICTS(1)
      COMMON / CLDICT / CDICTL(1)

C                            The associated code numbers plus
C                            the numbers of words of each length

      COMMON / CNSDCT / NS, ICODES(1)
      COMMON / CNLDCT / NL, ICODEL(1)

C------------------  C O D E  ------------------------------------------

      IF( LENGTH .LT. 1  .OR.  LENGTH .GT. LONG ) THEN
        IERROR = 2
        ICODE  = 0
      ELSE

C                            Examine dictionary entries according to
C                            the length of the input STRING
C                            If 8 or less, use short words dictionary
C                            and try long words only if no match found

        IERROR = 0

        IF( LENGTH .LE. SHORT ) THEN

          DO  10  I = 1,NS
            IF( STRING .EQ. CDICTS(I) ) THEN
              ICODE = ICODES(I)
              RETURN
            ENDIF
  10      CONTINUE

        ENDIF

C                            Try the long words dictionary (even if
C                            this is a short word - just in case)

        DO  20  I = 1,NL
          IF( STRING .EQ. CDICTL(I) ) THEN
            ICODE = ICODEL(I)
            RETURN
          ENDIF
  20    CONTINUE

C                            No match found

        IERROR = 1
        ICODE  = 0

      ENDIF

      RETURN
      END
