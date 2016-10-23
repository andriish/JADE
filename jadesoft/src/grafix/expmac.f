C   18/07/85 508031418  MEMBER NAME  EXPMAC   (S)           FORTRAN
C
C-----------------------------------------------------------------------
      SUBROUTINE EXPMAC( LSTCMD, CHARS, NEXFLD, IERR )
C-----------------------------------------------------------------------
C
C   AUTHOR:   C. BOWDERY  18/07/85 :  EXPAND THE REQUESTED MACRO
C
C LAST MOD:   C. BOWDERY   3/08/85 :  ORIGINAL COMMAND MOVED TO POS 31
C
C
C     THIS ROUTINE EXPANDS A MACRO WHICH IS POINTED TO BY LSTCMD-MACSTA
C     SO THAT ON RETURN, ARRAY CHARS CONTAINS THE COMMAND LINE. THE
C     POINTER IS SAVED ON A STACK TO ALLOW MACRO NESTING TO DEPTH 10.
C     DEPTH = 1 CONTAINS THE POINTER TO THE INVOKING COMMAND LINE SO
C     USEFUL MACRO NESTING OF 9 LEVELS IS ALLOWED.
C     NEXFLD IS THE FIELD NUMBER WHERE THE NEXT COMMAND AFTER THIS MACRO
C     STARTS. IERR > 0 IF ERROR DETECTED.
C
C-----------------------------------------------------------------------
C
      IMPLICIT INTEGER*2 (H)
C
      LOGICAL*1  CHARS(80)
      LOGICAL*1  CDEF
C
      COMMON / CGRMAC / MACNR, MACSTA, MACDEP, MACPNT(2,10), CDEF(80,31)
C
C------------------  C O D E  ------------------------------------------
C
C                            SAVE THE COMMAND LINE THAT INVOKED THE
C                            MACRO UNLESS WE ARE EXPANDING A NESTED
C                            MACRO, I.E.  IS DEPTH > 1
C
      IF( MACDEP .GT. 1 ) GO TO 5
C
C                            SAVE COMMAND LINE AS 'MACRO 31'
C
        DO  2  I = 1,80
          CDEF(I,31) = CHARS(I)
   2    CONTINUE
        MACPNT(1,1) = 31
C
   5  MACPNT(2,MACDEP) = NEXFLD
      MACDEP = MACDEP + 1
      IF( MACDEP .LE. 10 ) GO TO 10
        IERR   = 1
        MACDEP = 0
        WRITE(6,7)
   7    FORMAT(' Error: Macro nesting exceeded 9 levels. Aborted.')
        RETURN
C
  10  IPNT = LSTCMD - MACSTA
      MACPNT(1,MACDEP) = IPNT
C
      DO  20  I = 1,80
        CHARS(I) = CDEF(I,IPNT)
  20  CONTINUE
      IERR   = 0
      RETURN
      END
