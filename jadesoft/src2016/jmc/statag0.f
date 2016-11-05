C   19/08/80 008211142  MEMBER NAME  STATAG                 FORTRAN
      SUBROUTINE STATAG( LENGTH, ITAG )
C-----------------------------------------------------------
C
C   VERSION OF 10/05/79      LAST MOD    21/08/80       E.ELSEN
C   STORE FWD LEAD GLASS DATA IN ARRAR ITAG.
C   LENGTH IS TOTAL I*4 LENGTH OF ARRAY.
C   EXTRA ENTRY RETURNS LENGTH ONLY.
C-----------------------------------------------------------
C
      IMPLICIT INTEGER*2 (H)
      DIMENSION ITAG(1), HP(4), IHP(2)
      EQUIVALENCE ( HP(1), IHP(1) )
      COMMON / CGGRAW / HGG(192)
      DIMENSION HELP(2)
      EQUIVALENCE ( HELP(1), IHELP )
C
C                                           INITIALISE POINTERS
      HP(1) = 1
      HP(2) = 0
      HP(3) = 0
      HP(4) = 0
C
      NBLIM = 96
      LENGTH = 3
      INX = 1
      DO 100 J = 1, 192
      IF( HGG(J) .LE. 0 ) GO TO 100
      LENGTH = LENGTH + 1
      HELP(1) = J
      HELP(2) = HGG(J)
      ITAG(LENGTH) = IHELP
      IF( J .LT. NBLIM ) GO TO 100
      INX = INX + 1
      HP(INX) = LENGTH*2 - 7
      NBLIM = NBLIM + 96
  100 CONTINUE
      IF( LENGTH .LE. 3 ) GO TO 8000
      HP(3) = LENGTH*2 - 5
      IF( HP(2) .EQ. 0 ) HP(2) = HP(3)
C
C                                           SET DESCRIPTOR AND POINTERS
      ITAG(1) = 0
      ITAG(2) = IHP(1)
      ITAG(3) = IHP(2)
C
C                                           LENGTH OF DATA STORED
C-----------------------------------------------------------
      ENTRY LHATAG( LENGTH )
C-----------------------------------------------------------
C
      LENGTH = 3
      DO 2000 J=1,192
      IF( HGG(J) .LE. 0 ) GO TO 2000
      LENGTH = LENGTH + 1
 2000 CONTINUE
      IF( LENGTH .LE. 3 ) GO TO 8000
      RETURN
C
 8000 LENGTH = 0
      RETURN
      END
