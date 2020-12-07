C   25/08/83 308251842  MEMBER NAME  JSPOIN   (S1)          FORTRAN
C
C-----------------------------------------------------------------------
      SUBROUTINE JSPOIN
C-----------------------------------------------------------------------
C
C   AUTHOR:   E. ELSEN     20/09/78 :  SHIFTS WIRE NOS. * SETS UP HPCELL
C
C        MOD  E. ELSEN     02/05/79 :
C   LAST MOD  C. BOWDERY   19/08/83 :  EXPANDED COMMON /CWSET/.
C                                   :  HITAR(16000) I.E. 4000 HITS
C
C        SHIFTS WIRE NUMBERS AND SETS UP CELL POINTERS HPCELL.
C
C
C
C
C
C-----------------------------------------------------------------------
C
      IMPLICIT INTEGER*2 (H)
C
      COMMON / CWSET  / NHALL, ISTART, HPCELL(98), HITAR(16000)
C
C-----------------------  C O D E  -------------------------------------
C
      JH4  = ISTART - 4
      ICL  = 0
      ILOW = 1
      J4   = 1
C
  100 IF( J4 .GT. JH4 ) GO TO 400
         HITAR(J4) = HITAR(J4)*8 - 7
         ICELL     = HITAR(J4)/128 + 1
         IF( ICELL .EQ. ICL ) GO TO 300
C
            DO  200  I = ILOW,ICELL
               HPCELL(I) = J4
  200       CONTINUE
C
            ILOW = ICELL + 1
            ICL  = ICELL
  300    J4 = J4 + 4
         GO TO 100
C
C                  FILL REST OF HPCELL
C
 400  DO  500  I = ILOW,98
         HPCELL(I) = J4
 500  CONTINUE
C
      RETURN
      END
