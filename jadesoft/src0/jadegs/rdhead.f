C   10/05/82 308102131  MEMBER NAME  RDHEAD   (S)           FORTRAN
      SUBROUTINE RDHEAD
C-----------------------------------------------------------
C                             LAST MOD 10/08/83   J.OLSSON
C   SAVE THE DATE IN HEAD WORDS 6-8,. IN HEAD WORDS 96-98
C   THIS IS TO SAVE INFORMATION OF DATE FOR TRACKING OF THE UNSMEARED
C   DATA, WHICH WILL LATER BE OVERWRITTEN BY RDDATE.
C-----------------------------------------------------------
C
      IMPLICIT INTEGER*2 (H)
      COMMON / BCS / IW(1)
      DIMENSION HW(1)
      EQUIVALENCE ( HW(1), IW(1) )
      NPHEAD = IW(IBLN('HEAD'))
      IF( NPHEAD .LE. 0 ) GO TO 8000
C
      HW(NPHEAD*2+96) = HW(NPHEAD*2+6)
      HW(NPHEAD*2+97) = HW(NPHEAD*2+7)
      HW(NPHEAD*2+98) = HW(NPHEAD*2+8)
8000  RETURN
      END
