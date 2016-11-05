C   03/08/85 508052038 MEMBER NAME  HELPLI   (S)           FORTRAN
C
C-----------------------------------------------------------------------
      SUBROUTINE HELPLI
C-----------------------------------------------------------------------
C
C   AUTHOR:   C. BOWDERY  14/07/85 :  GIVE LIST OF HELP ITEMS
C
C      MOD:   C. BOWDERY   3/08/85 :  MACROS LISTED SEPARATELY
C LAST MOD:   C. BOWDERY   5/08/85 :  IMPROVED FORMAT
C
C-----------------------------------------------------------------------
C
      IMPLICIT INTEGER*2 (H)
C
      LOGICAL*1  CCMD
      LOGICAL*1  CDEF
C
      COMMON / CGRAP4 / IPOS4,CCMD(8,200),HCMD(5,200)
      COMMON / CGRMAC / MACNR, MACSTA, MACDEP, MACPNT(2,10), CDEF(80,31)
C
C-------------------  C O D E  -----------------------------------------
C
      ICOMD = 0
      DO  5  I = 1,IPOS4
        IF( HCMD(1,I) .LE. MACSTA ) ICOMD = ICOMD + 1
   5  CONTINUE
C
C                            LIST COMMANDS
C
      WRITE(6,10) ( ( CCMD(J,K),J=1,8 ),K=1,ICOMD )
  10  FORMAT(30X,'H  E  L  P'/' Commands: ', 200(/5X,8(1X,8A1) ) )
C
C                            LIST MACROS
C
      IF( ICOMD .EQ. IPOS4 ) RETURN
      K1 = ICOMD + 1
      WRITE(6,20) ( ( CCMD(J,K),J=1,8 ),K=K1,IPOS4 )
  20  FORMAT(/' Macros: ', 200(/5X,8(1X,8A1) ) )
C
      RETURN
      END
