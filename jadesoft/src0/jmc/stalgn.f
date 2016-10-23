C   11/07/79 102121127  MEMBER NAME  STALGN   (S)           FORTRAN
      SUBROUTINE STALGN( LENGTH, ILGL )
C-----------------------------------------------------------
C
C   VERSION OF 10/05/79      LAST MOD    12/02/81       E.ELSEN
C   STORE LEAD GLASS DATA IN ARRAR ILGL.
C   BANK DESCRIPTOR AND BARREL/END CAP POINTERS ARE PUT IN.
C   LENGTH IS TOTAL I*4 LENGTH OF ARRAY.
C   EXTRA ENTRY RETURNS LENGTH ONLY.
C   RANGE OF BLOCK NUMBERS NOW FROM 0 - 2879
C   CHECK ON ENERGY SMEAR FLAG LFLAG INTRODUCED  11/06/80 18:30
C                         AND EXTENDED           12/02/81
C-----------------------------------------------------------
C
      IMPLICIT INTEGER*2 (H)
      DIMENSION ILGL(1), HP(4), IHP(2)
      EQUIVALENCE ( HP(1), IHP(1) )
      COMMON / CLGAMP / LGLONG, HAMPL(3000)
      DIMENSION                 IAMPL(1500), HELP(2)
      EQUIVALENCE ( HAMPL(1), IAMPL(1) )
      EQUIVALENCE ( HELP(1), IHELP )
C
      LOGICAL * 1 LFLAG
      COMMON/CFLAG/LFLAG(10)
C
CCC  CHECK NUMBER OF FIRED BLOCKS
      IF( LGLONG .EQ. 0 ) GO TO 1000
C
CCC  INITIALISE POINTERS
      HP(1) = 1
      HP(2) = 0
      HP(3) = 0
      HP(4) = LGLONG + 1
C
      NBLIM = 2688
      LGL2 = LGLONG / 2
      INX = 1
      DO 200 J = 1, LGL2
      IHELP = IAMPL(J)
   10 IF( HELP(1) .LE. NBLIM ) GO TO 100
      INX = INX + 1
      HP(INX) = J*2 - 1
      NBLIM = NBLIM + 96
      GO TO 10
  100 HELP(1) = HELP(1) - 1
C                                          STORE IN ILGL ARRAY
  200 ILGL(3+J) = IHELP
      IF( HP(3) .EQ. 0 ) HP(3) = HP(4)
      IF( HP(2) .EQ. 0 ) HP(2) = HP(3)
C
CCC SET DESCRIPTOR AND POINTERS
      ILGL(1) = 1
      IF( LFLAG(1) ) ILGL(1) = 2
      IF( LFLAG(3) ) ILGL(1) = ILGL(1)+16
      ILGL(2) = IHP(1)
      ILGL(3) = IHP(2)
C
CCC  EXTRA ENTRY FOR LENGTH OF ARRAY
C-----------------------------------------------------------
      ENTRY LHALGN( LENGTH )
C-----------------------------------------------------------
C
 1000 LENGTH = 0
      IF( LGLONG .EQ. 0 ) RETURN
      LENGTH = LGLONG / 2 + 3
      RETURN
      END
