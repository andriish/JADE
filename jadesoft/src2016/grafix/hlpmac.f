C   09/07/85 508031514 MEMBER NAME  HLPMAC   (S)           FORTRAN
C
C-----------------------------------------------------------------------
      SUBROUTINE HLPMAC( ICMD , CHARS, LEN )
C-----------------------------------------------------------------------
C
C   AUTHOR:   C. BOWDERY   3/08/85 :  GIVE HELP FOR MACROS
C
C
C     FROM  ICMD, THE COMMAND NUMBER, THE MACRO NUMBER IS DETERMINED
C     AND THE CORRESPONDING MACRO DEFINITION IS FOUND AND DISPLAYED
C     ON THE SCREEN
C
C-----------------------------------------------------------------------
C
      IMPLICIT INTEGER*2 (H)
C
      LOGICAL*1  CDEF
      LOGICAL*1  CHARS(LEN)
C
      COMMON / CGRMAC / MACNR, MACSTA, MACDEP, MACPNT(2,10), CDEF(80,31)
C
C-------------------  C O D E  -----------------------------------------
C
      CALL CLRCON
C
      IMAC = ICMD - MACSTA
      WRITE(6,10) (CHARS(K),K=1,LEN)
  10  FORMAT(/' Macro:        ',8A1)
      WRITE(6,20) (CDEF(K,IMAC),K=1,80)
  20  FORMAT(/' The current definition of this macro is:'//
     +       1X,80A1/)
      RETURN
      END
