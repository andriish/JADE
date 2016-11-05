C   19/04/79 111031114  MEMBER NAME  STHEAD0  (S)           FORTRAN
      SUBROUTINE STHEAD( LENGTH, IHEAD )
C-----------------------------------------------------------
C
C  VERSION OF 18/04/79  LAST MOD 03/11/81    E.ELSEN
C  SET FIRST 50 WORDS OF BANK 'HEAD', CONTAINING EVENT AND
C  RUN INFORMATION ( SEE JADE COMPUTER NOTE 23 )
C  EXTRA ENTRY LHHEAD RETURN LENGTH ONLY.
C----------------------------------------------------------
C
      IMPLICIT INTEGER*2 (H)
      COMMON / BCS / IW(1)
      DIMENSION RW(1), HW(1)
      EQUIVALENCE (HW(1),RW(1),IW(1))
      DIMENSION IHEAD(50)
C
      COMMON / CGEO1 / BKGAUS
      DIMENSION HELP(2)
      EQUIVALENCE (IHELP,HELP(1))
      DATA HRCTYP / 1 /, HROPAT / Z61CC /
      DATA ICALL / 0 /
C
      IF( ICALL .GT. 0 ) GO TO 1
      ICALL = 1
      IPVECT = IBLN('VECT')
    1 CONTINUE
      IBEAM = 0
      NR = 0
      NPVECT = IW(IPVECT)
      IF( NPVECT .LE. 0 ) GO TO 2
      NR = IW(NPVECT+3)
      L0 = IW(NPVECT+1)
      IF( L0 .LE. 12 ) GO TO 2
      IBEAM = IW(NPVECT+13)
    2 CONTINUE
C
C
      CALL DATEMC( IHEAD(2) )
C
      HELP(1) = NR
      HELP(2) = HRCTYP
      IHEAD(6) = IHELP
C
      HELP(1) = HROPAT
      HELP(2) = 0
      IHEAD(7) = IHELP
C
      HELP(1) = IBEAM
      HELP(2) = BKGAUS * 1000.
      IHEAD(15) = IHELP
C
C-----------------------------------------------------------
      ENTRY LHHEAD( LENGTH )
C-----------------------------------------------------------
      LENGTH = 100
      RETURN
      END
