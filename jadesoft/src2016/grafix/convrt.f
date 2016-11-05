C   24/06/85 606102145  MEMBER NAME  CONVRT   (S)           FORTRAN
C
C-----------------------------------------------------------------------
      INTEGER FUNCTION CONVRT(IWORD)
C-----------------------------------------------------------------------
C
C    AUTHOR:   J. HAGEMANN   17/10/84 :  FUNCTION TO CONVERT VERTEX
C                                        CHAMBER WIRE NUMBERS FROM
C                                        HARDWARE TO SOFTWARE COUNTING
C  LAST MOD:   J. HAGEMANN   21/06/85 :  NOW IN OWN MEMBER
C
C-----------------  C O D E  -------------------------------------------
C
C
      IF( IWORD .GE.   1 .AND. IWORD .LE. 322) CONVRT = IWORD + 14
      IF( IWORD .GE. 323 .AND. IWORD .LE. 336) CONVRT = IWORD - 322
C
      RETURN
      END
