C   18/07/85 507221511  MEMBER NAME  RESMAC   (S)           FORTRAN
C
C-----------------------------------------------------------------------
      SUBROUTINE RESMAC( CHARS, NEXFLD )
C-----------------------------------------------------------------------
C
C   AUTHOR:   C. BOWDERY  18/07/85 :  RESTORE THE PREVIOUS MACRO
C
C
C     THIS ROUTINE RESTORES 1 LEVEL OF MACRO NESTING. LEVEL 1 IS THE
C     COMMAND LINE THAT CALLED THE MACRO. CHARS CONTAINS THE RESTORED
C     COMMAND LINE AND NEXFLD IS THE FIELD NUMBER OF THE NEXT COMMAND.
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
      MACDEP = MACDEP - 1
C
      IPNT   = MACPNT(1,MACDEP)
      NEXFLD = MACPNT(2,MACDEP)
C
      DO  20  I = 1,80
        CHARS(I) = CDEF(I,IPNT)
  20  CONTINUE
      RETURN
      END
